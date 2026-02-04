# =============================================================================
# FRIDAY SAVINGS EMAIL – SAFE VERSION WITH GT CHARTS AS ATTACHMENTS
# =============================================================================

library(googlesheets4)
library(dplyr)
library(lubridate)
library(gt)
library(webshot2)
library(glue)
library(blastula)
library(htmltools)
library(jsonlite)

# =============================================================================
# CONFIG
# =============================================================================

# Google Sheets Auth
tmpfile <- tempfile(fileext = ".json")

# Write the JSON from environment variable
sa_json <- Sys.getenv("GSHEET_CREDENTIALS")
writeLines(sa_json, tmpfile)

# Now authenticate
googlesheets4::gs4_auth(path = tmpfile)


Sys.setenv(
  MY_GMAIL_ACCOUNT = "benamolo@gmail.com",
  SMTP_PASSWORD   = "mkjz ansr zbkv dzip",
  SHEET_ID        = "1qidIxYD2DAOIZ64ONtbphsJOXdPDXnxVnP1kcJs_Qx0"
)

Sys.setenv(
  MY_GMAIL_ACCOUNT = Sys.getenv("MY_GMAIL_ACCOUNT"),
  SMTP_PASSWORD   = Sys.getenv("SMTP_PASSWORD"),
  SHEET_ID        = Sys.getenv("SHEET_ID")
)

# =============================================================================
# LOAD DATA
# =============================================================================

daily_data <- read_sheet(Sys.getenv("SHEET_ID"), sheet = "Member Contributions") %>%
  filter(!Name %in% c("Total Contributions", "Banked"))

# Convert to long format for plotting
member_long <- daily_data %>%
  pivot_longer(
    cols = matches("^\\d{1,2}/\\d{1,2}/\\d{4}$"),
    names_to = "date",
    values_to = "actual"
  ) %>%
  mutate(
    date = lubridate::dmy(date),
    actual = replace_na(actual, 0)
  )

today <- Sys.Date()
current_week_start <- floor_date(today, "week", week_start = 1)

# Example: assign payout dates per member
payout_dates <- today + weeks(1:10)
members_info <- daily_data %>%
  select(Name) %>%
  mutate(
    display_name = Name,
    Payout_Date = payout_dates[1:n()],
    order_number = row_number()
  )

# =============================================================================
# CREATE GT TABLE + SAVE AS IMAGE
# =============================================================================

save_gt_table_image <- function(member_name, member_data) {
  weekly_summary <- member_data %>%
    group_by(Name) %>%
    summarise(
      TotalSaved = sum(actual, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(TotalSaved))

  gt_tbl <- weekly_summary %>%
    gt() %>%
    tab_header(title = glue("💰 Weekly Savings Summary – {member_name}")) %>%
    fmt_currency(columns = TotalSaved, currency = "KES", decimals = 0) %>%
    cols_label(TotalSaved = "Total Saved")

  # Save as PNG
  img_file <- tempfile(fileext = ".png")
  gtsave(gt_tbl, filename = img_file)
  img_file
}

# =============================================================================
# CREATE EMAIL
# =============================================================================

create_email_body <- function(member_name, current_week_total, month_total, payout_info) {
  glue("
  <div style='font-family:Arial,sans-serif;max-width:800px;margin:auto;'>

    <h2 style='background:#5e60ce;color:#fff;padding:15px;text-align:center;'>
      💰 Weekly Savings Update – {member_name}
    </h2>

    <p><strong>Current Week Total:</strong> KES {current_week_total}</p>
    <p><strong>Month Total:</strong> KES {month_total}</p>
    <p><strong>Next Payout:</strong> {payout_info$member} on {payout_info$date} ({payout_info$days_until} days away)</p>

    <p>See attached chart for weekly breakdown.</p>

    <p style='margin-top:30px;font-size:12px;color:#666;'>Generated on {Sys.Date()}</p>
  </div>
  ")
}

# =============================================================================
# SEND EMAIL
# =============================================================================

send_member_email <- function(member_name, member_email, member_data) {
  # Create table image
  table_img <- save_gt_table_image(member_name, member_data)

  current_week_total <- sum(member_data$actual[member_data$date >= current_week_start], na.rm = TRUE)
  month_total <- sum(member_data$actual[month(member_data$date) == month(today)], na.rm = TRUE)
  payout_info <- list(member = member_name, date = today + 7, days_until = 7)

  email_body <- compose_email(
    body = html(create_email_body(member_name, current_week_total, month_total, payout_info))
  ) %>%
    add_attachment(file = table_img, filename = "weekly_savings.png")

  my_email_creds <- creds_envvar(
    user = Sys.getenv("MY_GMAIL_ACCOUNT"),
    pass_envvar = "SMTP_PASSWORD",
    provider = "gmail"
  )

  smtp_send(
    email = email_body,
    from = Sys.getenv("MY_GMAIL_ACCOUNT"),
    to = "amollozeethird@gmail.com",
    subject = glue("💰 Weekly Savings Update – Week {isoweek(today)}"),
    credentials = my_email_creds
  )
}

# =============================================================================
# LOOP THROUGH MEMBERS
# =============================================================================

for (i in 1:nrow(members_info)) {
  member <- members_info[i, ]
  member_data <- member_long %>% filter(Name == member$Name)
  cat(sprintf("Sending email for %s (%s)...\n", member$display_name, member$Name))
  send_member_email(member$display_name, member$Name, member_data)
  Sys.sleep(2)
}

cat("✅ All emails sent successfully!\n")


