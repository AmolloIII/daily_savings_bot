# friday_savings_email.R
# Usage: Rscript friday_savings_email.R --recipient="email@example.com"

args <- commandArgs(trailingOnly = TRUE)
recipient_email <- sub("--recipient=", "", args[grepl("--recipient=", args)])

if(length(recipient_email) == 0) stop("Please provide --recipient argument")

Sys.setenv(
  MY_GMAIL_ACCOUNT = Sys.getenv('MY_GMAIL_ACCOUNT'),
  SMTP_PASSWORD    = Sys.getenv('SMTP_PASSWORD')
)

library(tidyr)
library(lubridate)
library(gt)
library(dplyr)
library(googlesheets4)
library(blastula)
library(glue)

# --- Helper functions ---
format_kes <- function(x) paste0("KES ", format(round(x,2), big.mark = ",", nsmall = 2))

create_email_body <- function(member_name, weekly_table, payout_info, current_week_total, month_total, current_date){
  week_number <- isoweek(current_date)
  formatted_date <- format(current_date, "%A, %B %d, %Y")
  table_html <- gt::as_raw_html(weekly_table)
  
  glue::glue('<html><body>
<h2>Hello {member_name}!</h2>
<p>This Week: {format_kes(current_week_total)}</p>
<p>Month-to-date: {format_kes(month_total)}</p>
<p>Next payout: {payout_info$member} on {ifelse(is.na(payout_info$date),"Not scheduled",format(payout_info$date,"%B %d, %Y"))}</p>
{table_html}
</body></html>')
}

# --- Member info ---
members_info <- data.frame(
  sheet_name = c("Ben Amollo","Shabir Odhiambo","Arthur Mbatia","Stephen Katana","James Nyaga","Collins Korir","Meshack Ngava","Orvile Oreener","George Njoroge","Dan Njenga"),
  display_name = c("Ben Amollo","Shabir","Kanyanjua","Katz","Mutugi Nyaga","Kolo","Ngava","Double O","Njoro","Wakajiado3"),
  email = c("amollozeethird@gmail.com","ayoubshabir@gmail.com","aspkenya@gmail.com","lamiri93@gmail.com","mutugiwanyaga24@gmail.com","korayalakwen@gmail.com","ngavamuumbi@outlook.com","orvillenyandoro@gmail.com","gnjoro9@gmail.com","danielkaroga@gmail.com"),
  order_number = c(10,8,1,9,7,5,2,4,3,6),
  stringsAsFactors = FALSE
)

get_member_data <- function(member_name, member_daily_long) {
  member_daily_long %>%
    filter(Name == member_name) %>%
    head(366) %>%
    mutate(
      date = as.Date(date),
      day_in_month = day(date),
      target = day_in_month * 15,
      week_start = floor_date(date, "week", week_start = 1),
      week_end = week_start + days(6)
    ) %>%
    arrange(date)
}

# --- MAIN EXECUTION ---
cat("Starting email for", recipient_email, "...\n")

# Google Sheets auth
if(file.exists("gs.json")) gs4_auth(path="gs.json") else gs4_auth()
sheet_id <- "1qidIxYD2DAOIZ64ONtbphsJOXdPDXnxVnP1kcJs_Qx0"
daily_dataz <- read_sheet(sheet_id, sheet="Member Contributions") %>%
  filter(!Name %in% c("Total Contributions","Banked"))

member_daily_long <- daily_dataz %>%
  pivot_longer(cols = matches("^\\d{1,2}/\\d{1,2}/\\d{4}$"), names_to="date", values_to="actual") %>%
  mutate(date=dmy(date), actual=replace_na(actual,0)) %>%
  arrange(Name,date)

today <- Sys.Date()
selected_month <- month(today)
current_year <- year(today)
current_week_start <- floor_date(today,"week",week_start=1)

member <- members_info %>% filter(email == recipient_email)
member_data <- get_member_data(member$sheet_name, member_daily_long)

# Filter month, weekly summary
dfz <- member_data %>% filter(year(date)==current_year, month(date)==selected_month)
weekly_data <- dfz %>% group_by(week_start, week_end) %>%
  summarise(Target=sum(target), Actual=sum(actual), .groups="drop") %>%
  arrange(week_start) %>%
  mutate(Week=row_number(),
         Start_Date=format(week_start,"%d %b"),
         End_Date=format(week_end,"%d %b"),
         Week_Status = case_when(
           week_start < current_week_start ~ "past",
           week_start == current_week_start ~ "current",
           TRUE ~ "future"
         ))

weekly_gt <- weekly_data %>%
  select(Week, Start_Date, End_Date, Target, Actual, Week_Status) %>%
  gt() %>%
  fmt_currency(columns=c(Target,Actual), currency="KES", decimals=0) %>%
  cols_hide("Week_Status")

current_week_total <- sum(dfz$actual[dfz$week_start == current_week_start])
month_total <- sum(dfz$actual)

# Next payout
payout_dates <- as.Date(c("2026-02-08","2026-02-15","2026-02-22","2026-03-01","2026-03-08","2026-03-15","2026-03-22","2026-03-29","2026-04-05","2026-04-12"))
payout_df <- members_info %>% arrange(order_number) %>% mutate(Payout_Date=payout_dates[order_number])
next_payout <- payout_df %>% filter(Payout_Date>=today) %>% slice(1)
payout_info <- list(member=next_payout$display_name, date=next_payout$Payout_Date, days_until=as.numeric(next_payout$Payout_Date-today))

# Compose & send
email_body <- create_email_body(member$display_name, weekly_gt, payout_info, current_week_total, month_total, today)
email_msg <- compose_email(body=html(email_body))

my_email_creds <- creds_envvar(user=Sys.getenv('MY_GMAIL_ACCOUNT'), pass_envvar='SMTP_PASSWORD', provider='gmail', use_ssl=TRUE)

cat("Sending email to", recipient_email, "...\n")
smtp_send(email_msg, from=Sys.getenv('MY_GMAIL_ACCOUNT'), to=recipient_email,
          subject=paste("💰 Weekly Savings Update - Week", isoweek(today)), credentials=my_email_creds, port=587)
cat("✅ Email sent to", recipient_email, "\n")

