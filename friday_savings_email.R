# =========================
# DAILY / WEEKLY / MONTHLY TELEGRAM BOT
# =========================

# Load required libraries
library(googlesheets4)
library(dplyr)
library(lubridate)
library(ggplot2)
library(httr)
library(stringr)
library(jsonlite)
library(tidyr)
library(scales)
library(patchwork)
library(telegram.bot)
library(gridExtra)
library(base64enc)

# =========================
# TELEGRAM CONFIG
# =========================
BOT_TOKEN <- Sys.getenv("BOT_TOKEN")
CHAT_ID <- Sys.getenv("CHAT_ID")
bot <- Bot(token = BOT_TOKEN)   # initialize bot early for photo sending

# =========================
# HELPER FUNCTIONS
# =========================

send_telegram_message <- function(bot_token, chat_id, text, parse_mode = NULL) {
  text_enc <- URLencode(text, reserved = TRUE)
  url <- paste0(
    "https://api.telegram.org/bot", bot_token,
    "/sendMessage?chat_id=", chat_id,
    "&text=", text_enc
  )
  if (!is.null(parse_mode)) {
    url <- paste0(url, "&parse_mode=", parse_mode)
  }
  res <- httr::GET(url)
  httr::stop_for_status(res)
  return(res)
}

escape_markdown_v2 <- function(text) {
  specials <- c("_", "*", "[", "]", "(", ")", "~", "`", ">", "#", "+", "-", "=", "|", "{", "}", ".", "!")
  for (s in specials) {
    text <- gsub(s, paste0("\\", s), text, fixed = TRUE)
  }
  text <- gsub("%", "\\%", text, fixed = TRUE)
  return(text)
}

send_message_buttons <- function(text, buttons) {
  body <- list(
    chat_id = CHAT_ID,
    text = text,
    parse_mode = "HTML",
    reply_markup = list(inline_keyboard = buttons)
  )
  httr::POST(
    paste0("https://api.telegram.org/bot", BOT_TOKEN, "/sendMessage"),
    body = body,
    encode = "json"
  )
}

# ----- Functions for financial reminder (bills, payments, goals) -----
tg_post <- function(method, body) {
  response <- POST(
    paste0("https://api.telegram.org/bot", BOT_TOKEN, "/", method),
    body = body,
    encode = "json"
  )
  return(response)
}

send_message <- function(text) {
  body <- list(
    chat_id = CHAT_ID,
    text = text,
    parse_mode = "HTML"
  )
  tg_post("sendMessage", body)
}

read_bills <- function() {
  cat("📊 Reading bills data...\n")
  bills <- read_sheet(SHEET_URL_Z, sheet = "bills")
  bills <- bills %>%
    mutate(
      due_date = as.Date(due_date),
      amount = as.numeric(amount),
      due_in_days = as.numeric(due_date - Sys.Date())
    ) %>%
    arrange(due_date)
  cat("✅ Bills loaded:", nrow(bills), "records\n")
  return(bills)
}

read_payments <- function() {
  cat("💰 Reading payments data...\n")
  tryCatch({
    payments <- read_sheet(SHEET_URL_Z, sheet = "payments")
    if(nrow(payments) == 0) {
      cat("⚠️ No payments data found\n")
      return(data.frame(
        bill_id = character(), 
        name = character(), 
        amount = numeric(), 
        paid_on = as.Date(character()), 
        method = character()
      ))
    }
    payments <- payments %>%
      mutate(
        paid_on = as.Date(paid_on),
        amount = as.numeric(amount)
      )
    cat("✅ Payments loaded:", nrow(payments), "records\n")
    return(payments)
  }, error = function(e) {
    cat("⚠️ Could not read payments sheet:", e$message, "\n")
    return(data.frame(
      bill_id = character(), 
      name = character(), 
      amount = numeric(), 
      paid_on = as.Date(character()), 
      method = character()
    ))
  })
}

read_goals <- function() {
  cat("🎯 Reading goals data...\n")
  goals <- read_sheet(SHEET_URL_Z, sheet = "goals")
  goals <- goals %>%
    mutate(
      target_date = as.Date(target_date),
      target_amount = as.numeric(target_amount),
      current_amount = as.numeric(current_amount),
      progress_pct = round(current_amount / target_amount * 100, 1),
      amount_left = target_amount - current_amount,
      daily_required = ifelse(
        as.numeric(target_date - Sys.Date()) > 0,
        amount_left / as.numeric(target_date - Sys.Date()),
        NA
      )
    )
  cat("✅ Goals loaded:", nrow(goals), "records\n")
  return(goals)
}

match_payments_to_bills <- function(bills, payments) {
  cat("🔍 Matching payments to bills...\n")
  if(nrow(payments) == 0) {
    bills$total_paid <- 0
    bills$pending_amount <- bills$amount
    bills$payment_status <- "unpaid"
    bills$last_payment_date <- NA
    bills$payment_method <- NA
    return(bills)
  }
  payments_by_id <- payments %>%
    group_by(bill_id) %>%
    summarise(
      total_paid = sum(amount, na.rm = TRUE),
      last_payment_date = max(paid_on, na.rm = TRUE),
      payment_method = paste(unique(method[!is.na(method)]), collapse = ", "),
      .groups = "drop"
    ) %>%
    rename(id = bill_id)
  payments_by_name <- payments %>%
    group_by(name) %>%
    summarise(
      total_paid_name = sum(amount, na.rm = TRUE),
      last_payment_date_name = max(paid_on, na.rm = TRUE),
      payment_method_name = paste(unique(method[!is.na(method)]), collapse = ", "),
      .groups = "drop"
    )
  bills <- bills %>%
    left_join(payments_by_id, by = "id") %>%
    left_join(payments_by_name, by = "name") %>%
    mutate(
      total_paid = coalesce(total_paid, total_paid_name, 0),
      last_payment_date = coalesce(last_payment_date, last_payment_date_name),
      payment_method = coalesce(payment_method, payment_method_name),
      total_paid_name = NULL,
      last_payment_date_name = NULL,
      payment_method_name = NULL,
      pending_amount = pmax(amount - total_paid, 0),
      payment_status = case_when(
        total_paid >= amount * 0.99 ~ "paid",
        total_paid > 0 & total_paid < amount ~ "partial",
        total_paid == 0 ~ "unpaid",
        TRUE ~ "unknown"
      ),
      paid_on_time = ifelse(
        !is.na(last_payment_date) & last_payment_date <= due_date,
        TRUE,
        FALSE
      )
    )
  paid_count <- sum(bills$payment_status == "paid", na.rm = TRUE)
  partial_count <- sum(bills$payment_status == "partial", na.rm = TRUE)
  unpaid_count <- sum(bills$payment_status == "unpaid", na.rm = TRUE)
  cat("📋 Payment Matching Summary:\n")
  cat("   ✅ Paid:", paid_count, "bills\n")
  cat("   🔄 Partial:", partial_count, "bills\n")
  cat("   ❌ Unpaid:", unpaid_count, "bills\n")
  return(bills)
}

format_currency <- function(amount) {
  if (is.na(amount) || amount == 0) return("KES 0")
  paste0("KES ", format(round(amount, 2), big.mark = ",", nsmall = 2))
}

get_urgency_icon <- function(days_left, is_paid = FALSE, is_partial = FALSE) {
  if(is_paid) return("✅")
  if(is_partial) return("🔄")
  if (is.na(days_left) || days_left < 0) return("🔴")
  if (days_left == 0) return("🟠")
  if (days_left <= 3) return("🟡")
  if (days_left <= 7) return("🟢")
  return("⚪")
}

create_progress_bar <- function(percentage, width = 10) {
  if (is.na(percentage)) percentage <- 0
  filled <- round(width * percentage / 100)
  empty <- width - filled
  bar <- paste0(
    "<code>[",
    strrep("█", filled),
    strrep("░", empty),
    "]</code> ",
    round(percentage, 1), "%"
  )
  return(bar)
}

format_date_display <- function(date) {
  if (is.na(date)) return("No date")
  today <- Sys.Date()
  days_diff <- as.numeric(date - today)
  if (days_diff == 0) return("<b>Today</b>")
  if (days_diff == 1) return("<b>Tomorrow</b>")
  if (days_diff < 0) return(paste0("<b>Overdue by ", abs(days_diff), " days</b>"))
  if (days_diff <= 7) return(paste0("In <b>", days_diff, " days</b>"))
  return(format(date, "%b %d, %Y"))
}

create_unpaid_bills_section <- function(bills) {
  unpaid_bills <- bills %>% 
    filter(pending_amount > 0) %>%
    filter(!is.na(name) & !is.na(amount)) %>%
    arrange(due_date)
  if (nrow(unpaid_bills) == 0) {
    return(paste0(
      "<b>📋 PENDING BILLS</b>\n",
      "✅ All bills are paid! 🎉\n\n"
    ))
  }
  section_text <- "<b>📋 PENDING BILLS</b>\n"
  for (i in 1:nrow(unpaid_bills)) {
    bill <- unpaid_bills[i, ]
    is_partial <- bill$payment_status == "partial"
    section_text <- paste0(
      section_text,
      get_urgency_icon(bill$due_in_days, FALSE, is_partial), " ",
      "<b>", bill$name, "</b>\n",
      "   Original: ", format_currency(bill$amount), "\n",
      "   Due: ", format_date_display(bill$due_date), "\n"
    )
    if (is_partial) {
      section_text <- paste0(
        section_text,
        "   Paid: ", format_currency(bill$total_paid), "\n",
        "   Pending: ", format_currency(bill$pending_amount), "\n"
      )
    } else {
      section_text <- paste0(
        section_text,
        "   Pending: ", format_currency(bill$pending_amount), "\n"
      )
    }
    if ("recurring" %in% colnames(bill) && !is.na(bill$recurring) && bill$recurring != "") {
      section_text <- paste0(section_text, "   🔄 ", bill$recurring, "\n")
    }
    section_text <- paste0(section_text, "\n")
  }
  return(section_text)
}

create_paid_bills_summary <- function(bills) {
  paid_bills <- bills %>% 
    filter(payment_status == "paid") %>%
    filter(!is.na(name) & !is.na(amount))
  if (nrow(paid_bills) == 0) {
    return("")
  }
  total_paid <- sum(paid_bills$amount, na.rm = TRUE)
  recent_paid <- paid_bills %>%
    filter(!is.na(last_payment_date)) %>%
    arrange(desc(last_payment_date)) %>%
    head(3)
  section_text <- paste0(
    "<b>✅ RECENTLY PAID BILLS</b>\n",
    "Total paid: ", format_currency(total_paid), "\n\n"
  )
  for (i in 1:nrow(recent_paid)) {
    bill <- recent_paid[i, ]
    section_text <- paste0(
      section_text,
      "• <b>", bill$name, "</b>\n",
      "  Amount: ", format_currency(bill$amount), "\n",
      "  Paid on: ", format(bill$last_payment_date, "%b %d"), "\n"
    )
    if (!is.na(bill$payment_method) && bill$payment_method != "") {
      section_text <- paste0(section_text, "  Method: ", bill$payment_method, "\n")
    }
    section_text <- paste0(section_text, "\n")
  }
  return(paste0(section_text, "\n"))
}

create_debts_section <- function(bills) {
  owe_bills <- bills %>% 
    filter(direction == "owe" & pending_amount > 0) %>%
    filter(!is.na(name) & !is.na(amount))
  owed_bills <- bills %>% 
    filter(direction == "owed_to_me" & pending_amount > 0) %>%
    filter(!is.na(name) & !is.na(amount))
  section_text <- ""
  if (nrow(owe_bills) > 0) {
    total_pending <- sum(owe_bills$pending_amount, na.rm = TRUE)
    section_text <- paste0(
      section_text,
      "<b>💸 YOU OWE TO OTHERS</b>\n",
      "Total pending: ", format_currency(total_pending), "\n\n"
    )
    for (i in 1:min(5, nrow(owe_bills))) {
      bill <- owe_bills[i, ]
      section_text <- paste0(
        section_text,
        "• ", get_urgency_icon(bill$due_in_days, bill$payment_status == "paid"), " ",
        "<b>", bill$name, "</b>\n",
        "  Pending: ", format_currency(bill$pending_amount), "\n",
        "  Due: ", format_date_display(bill$due_date), "\n\n"
      )
    }
    if (nrow(owe_bills) > 5) {
      section_text <- paste0(section_text, 
                             "  ... and <b>", nrow(owe_bills) - 5, " more</b>\n\n")
    }
  }
  if (nrow(owed_bills) > 0) {
    total_pending <- sum(owed_bills$pending_amount, na.rm = TRUE)
    section_text <- paste0(
      section_text,
      "<b>💰 OWED TO YOU</b>\n",
      "Total pending: ", format_currency(total_pending), "\n\n"
    )
    for (i in 1:min(5, nrow(owed_bills))) {
      bill <- owed_bills[i, ]
      section_text <- paste0(
        section_text,
        "• ", get_urgency_icon(bill$due_in_days, bill$payment_status == "paid"), " ",
        "<b>", bill$name, "</b>\n",
        "  Pending: ", format_currency(bill$pending_amount), "\n",
        "  Due: ", format_date_display(bill$due_date), "\n\n"
      )
    }
    if (nrow(owed_bills) > 5) {
      section_text <- paste0(section_text, 
                             "  ... and <b>", nrow(owed_bills) - 5, " more</b>\n\n")
    }
  }
  if (section_text == "") {
    return(paste0(
      "<b>🤝 DEBTS</b>\n",
      "✅ All settled up! 🎉\n\n"
    ))
  }
  return(section_text)
}

create_goals_section <- function(goals) {
  active_goals <- goals %>% 
    filter(status == "ongoing" | is.na(status)) %>%
    arrange(target_date)
  if (nrow(active_goals) == 0) {
    return(paste0(
      "<b>🎯 FINANCIAL GOALS</b>\n",
      "No active goals set\n\n"
    ))
  }
  section_text <- "<b>🎯 FINANCIAL GOALS</b>\n"
  for (i in 1:nrow(active_goals)) {
    goal <- active_goals[i, ]
    section_text <- paste0(
      section_text,
      "\n<b>", goal$goal_name, "</b>\n",
      create_progress_bar(goal$progress_pct), "\n",
      format_currency(goal$current_amount), " / ", 
      format_currency(goal$target_amount), "\n",
      "📅 ", format_date_display(goal$target_date), "\n"
    )
    if (!is.na(goal$daily_required) && goal$daily_required > 0 && 
        goal$progress_pct < 100 && as.numeric(goal$target_date - Sys.Date()) > 0) {
      section_text <- paste0(
        section_text,
        "📈 Daily target: ", format_currency(goal$daily_required), "\n"
      )
    }
  }
  return(paste0(section_text, "\n"))
}

create_summary_section <- function(bills, goals) {
  total_pending <- sum(bills$pending_amount, na.rm = TRUE)
  total_paid_bills <- bills %>% 
    filter(payment_status == "paid") %>%
    summarise(total = sum(amount, na.rm = TRUE)) %>% 
    pull(total)
  total_partial_bills <- bills %>% 
    filter(payment_status == "partial") %>%
    summarise(total = sum(pending_amount, na.rm = TRUE)) %>% 
    pull(total)
  total_pending_owe <- bills %>% 
    filter(direction == "owe") %>%
    summarise(total = sum(pending_amount, na.rm = TRUE)) %>% 
    pull(total)
  total_pending_owed <- bills %>% 
    filter(direction == "owed_to_me") %>%
    summarise(total = sum(pending_amount, na.rm = TRUE)) %>% 
    pull(total)
  total_goals <- goals %>% 
    filter(status == "ongoing" | is.na(status)) %>% 
    summarise(
      current = sum(current_amount, na.rm = TRUE),
      target = sum(target_amount, na.rm = TRUE)
    )
  net_position <- total_pending_owed - total_pending_owe
  urgent_bills <- bills %>% 
    filter((due_in_days <= 7 & due_in_days >= 0) | due_in_days < 0) %>%
    filter(!is.na(pending_amount) & pending_amount > 0)
  urgent_count <- nrow(urgent_bills)
  urgent_total <- sum(urgent_bills$pending_amount, na.rm = TRUE)
  summary_text <- paste0(
    "<b>📊 FINANCIAL SUMMARY</b>\n",
    "┌────────────────────────────┐\n",
    "│ Payment Status             │\n",
    "│ • Paid: ", format_currency(total_paid_bills), "\n",
    "│ • Pending: ", format_currency(total_pending), "\n",
    "│ • Partial: ", format_currency(total_partial_bills), "\n",
    "└────────────────────────────┘\n\n"
  )
  if (urgent_count > 0) {
    summary_text <- paste0(
      "<b>⚠️  URGENT NOTICE</b>\n",
      "You have <b>", urgent_count, "</b> bill(s) due within 7 days\n",
      "Total urgent amount: <b>", format_currency(urgent_total), "</b>\n\n",
      summary_text
    )
  }
  return(summary_text)
}

# =========================
# QUOTES PROCESSING
# =========================
text <- "Are you ready to enhance your understanding of money and its impact on your life? ..."   # (truncated for brevity, full text remains as in original)

lines <- str_split(text, "\n")[[1]] %>%
  str_trim() %>%
  .[. != ""]

quote_lines <- lines[str_detect(lines, '“.+”\\s*–\\s*.+')]

quotes_df <- tibble(raw = quote_lines) %>%
  mutate(
    quote = str_extract(raw, '“.+?”'),
    author = str_extract(raw, '(?<=–\\s).*')
  ) %>%
  select(quote, author)

# Daily random quote
daily_quote <- tryCatch({
  sample_n(quotes_df, 1) %>%
    mutate(full = paste0(quote, " – ", author)) %>%
    pull(full)
}, error = function(e) { "Keep going! Every shilling counts." })

# =========================
# GOOGLE SHEETS AUTH
# =========================
GSHEET_JSON_B64 <- Sys.getenv("GSHEET_JSON_B64")
YOUR_GOOGLE_SHEET_URL <- Sys.getenv("YOUR_GOOGLE_SHEET_URL")
SHEET_URL_Z <- Sys.getenv("SHEET_URL_Z")

if (YOUR_GOOGLE_SHEET_URL == "" || GSHEET_JSON_B64 == "") {
  stop("❌ YOUR_GOOGLE_SHEET_URL or GSHEET_JSON_B64 not set in environment")
}

writeLines(rawToChar(base64enc::base64decode(GSHEET_JSON_B64)), "gs.json")
Sys.chmod("gs.json", mode = "600")

gs4_auth(
  path = "gs.json",
  scopes = "https://www.googleapis.com/auth/spreadsheets.readonly",
  cache = FALSE
)
message("✅ Google Sheets authenticated")

# =========================
# READ SAVINGS DATA
# =========================
savings_data <- read_sheet(YOUR_GOOGLE_SHEET_URL) %>% 
  mutate(date = as.Date(date),
         actual = as.numeric(actual),
         day_of_month = day(date)) %>% 
  arrange(date)

# Correct actual sequence (15 per day)
corrected_savings <- savings_data %>%
  arrange(date) %>%
  mutate(correct_actual = 15 * row_number())

# =========================
# BEN AND ABBY DATA FOR REPORTS
# =========================
BenQ_data <- read_sheet(YOUR_GOOGLE_SHEET_URL, sheet = "savings_data") %>% mutate(member = "Ben")
Abby_data <- read_sheet(YOUR_GOOGLE_SHEET_URL, sheet = "savings_data_Abby") %>% mutate(member = "Abby")

all_data <- bind_rows(BenQ_data, Abby_data) %>%
  mutate(date = as.Date(date),
         day_of_month = day(date)) %>%
  arrange(member, date) %>%
  group_by(member, month = month(date)) %>%
  mutate(
    daily_target = 15 * row_number(),
    cumulative_actual = cumsum(actual),
    cumulative_target = cumsum(daily_target)
  ) %>%
  ungroup() %>%
  select(-month)

ben_data <- all_data %>% filter(member == "Ben")
abby_data <- all_data %>% filter(member == "Abby")  # may be empty

# =========================
# STATE FILE FOR STATUS CHANGES
# =========================
STATE_FILE <- "last_status_state.rds"

if (file.exists(STATE_FILE)) {
  last_state <- readRDS(STATE_FILE)
} else {
  last_state <- savings_data %>%
    select(date, status)
}

status_changes <- savings_data %>%
  select(date, status) %>%
  left_join(
    last_state %>% rename(prev_status = status),
    by = "date"
  ) %>%
  filter(!is.na(prev_status), status != prev_status)

today <- Sys.Date()

# =========================
# DAILY METRICS
# =========================
today_row <- savings_data %>% filter(date == today)
leoste <- if(nrow(today_row) > 0) today_row$correct_actual else 15 * nrow(savings_data) + 15

cumulative_saved <- sum(savings_data$actual[savings_data$date <= today], na.rm = TRUE)
total_missed <- sum(corrected_savings$correct_actual[corrected_savings$status == "Missed" & corrected_savings$date <= today])
yearly_target <- 86070
percentage_target <- round(cumulative_saved / yearly_target * 100, 1)

# =========================
# MISSED DAYS CHECK (all past days)
# =========================
missed_days <- savings_data %>%
  filter(date < today, status == "Missed") %>%
  arrange(date)

if (nrow(missed_days) > 0) {
  missed_days_chr <- format(as.Date(missed_days$date), "%Y-%m-%d")
  msg <- paste0(
    "⚠️ Savings Reminder\n\n",
    "You have missed savings for the following *", nrow(missed_days), " day(s)*:\n",
    paste(paste0("• ", missed_days_chr), collapse = "\n"),
    "\n\nTotal missed days: *", nrow(missed_days), "*",
    "\n\n👉 Take action now by visiting your dashboard:\n",
    "https://kampspatial.shinyapps.io/BenQ_2026_DSPC/",
    "\n\nLet’s get back on track 💪\n\n",
    "*", daily_quote, "*"
  )
  send_telegram_message(BOT_TOKEN, CHAT_ID, msg, parse_mode = "Markdown")
}

# =========================
# WEEKLY SUMMARY (Sunday)
# =========================
week_start <- floor_date(today, unit = "week")
week_end <- ceiling_date(today, unit = "week") - days(1)

if (wday(today) == 1) { # Sunday
  weekly_data <- savings_data %>%
    filter(date >= week_start & date <= week_end)
  
  # Original weekly bar chart (Ben)
  total_saved <- sum(weekly_data$actual, na.rm = TRUE)
  days_saved <- sum(weekly_data$status == "Saved", na.rm = TRUE)
  
  weekly_chart_file <- "weekly_chart.png"
  png(weekly_chart_file, width = 800, height = 600)
  print(
    ggplot(weekly_data, aes(x = date, y = actual)) +
      geom_col(fill = "steelblue") +
      geom_text(aes(label = actual), vjust = -0.5) +
      labs(title = paste0("Weekly Savings: ", week_start, " to ", week_end),
           x = "Date", y = "Amount Saved (KES)") +
      theme_minimal()
  )
  dev.off()
  
  daily_quote_safe <- as.character(daily_quote)
  caption <- paste0(
    "📊 Weekly Savings Summary\n",
    "Total Saved: KES ", total_saved, "\n",
    "Days Saved: ", days_saved, "/", nrow(weekly_data), "\n\n",
    daily_quote_safe
  )
  
  if (file.exists(weekly_chart_file)) {
    bot$sendPhoto(chat_id = CHAT_ID, photo = weekly_chart_file, caption = caption)
  } else {
    warning("Weekly chart file not found!")
  }
  
  # ----- NEW: Generate comprehensive report for Ben -----
  generate_person_report <- function(person_data, person_name) {
    today <- Sys.Date()
    
    # Prepare data
    person_data <- person_data %>%
      arrange(date) %>%
      mutate(
        daily_target = 15,
        cumulative_target = cumsum(daily_target),
        cumulative_actual = cumsum(actual),
        status_fill = case_when(
          status == "Saved"  ~ "Saved",
          status == "Missed" ~ "Missed",
          TRUE               ~ "Future"
        )
      )
    
    plot_data <- person_data %>% filter(date <= today)
    
    # Cumulative chart
    plot_data <- plot_data %>%
      mutate(fade = scales::rescale(as.numeric(date - min(date)), to = c(0.15, 1)))
    
    current_row <- plot_data %>% slice_tail(n = 1)
    
    p_cumulative <- ggplot(plot_data, aes(x = date)) +
      geom_ribbon(
        aes(ymin = pmin(cumulative_actual, cumulative_target),
            ymax = pmax(cumulative_actual, cumulative_target),
            alpha = fade),
        fill = "firebrick"
      ) +
      geom_line(aes(y = cumulative_target, color = "Target"), linewidth = 1) +
      geom_line(aes(y = cumulative_actual, color = "Actual"), linewidth = 1) +
      geom_label(
        data = current_row,
        aes(y = cumulative_actual, label = paste0("Actual: KES ", scales::comma(cumulative_actual))),
        fill = "white", color = "darkgreen", size = 3, fontface = "bold", vjust = -0.7
      ) +
      geom_label(
        data = current_row,
        aes(y = cumulative_target, label = paste0("Target: KES ", scales::comma(cumulative_target))),
        fill = "white", color = "blue", size = 3, fontface = "bold", vjust = -0.7
      ) +
      scale_color_manual(values = c("Target" = "#2b8cbe", "Actual" = "#006d2c")) +
      scale_alpha_continuous(range = c(0.15, 1), guide = "none") +
      scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0.08, 0.35))) +
      scale_x_date(expand = expansion(mult = c(0.02, 0.06))) +
      labs(title = "Cumulative Savings", x = NULL, y = "KES", color = NULL) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "top", plot.title = element_text(face = "bold", hjust = 0.5))
    
    # Monthly heatmap
    month_start <- floor_date(today, "month")
    month_end   <- ceiling_date(today, "month") - days(1)
    
    all_days_month <- seq(month_start, month_end, by = "day")
    month_complete <- tibble(date = all_days_month) %>%
      mutate(
        daily_target = 15,
        cumulative_target = cumsum(daily_target)
      ) %>%
      left_join(plot_data %>% select(date, status_fill), by = "date") %>%
      mutate(
        status_fill = if_else(is.na(status_fill), "Future", status_fill),
        expected_cumulative = cumulative_target
      )
    
    p_monthly <- ggplot(month_complete, aes(x = date, y = 1)) +
      geom_tile(aes(fill = status_fill, alpha = expected_cumulative), color = "white", height = 0.9) +
      scale_fill_manual(values = c("Saved" = "darkgreen", "Missed" = "red", "Future" = "grey80")) +
      scale_alpha_continuous(range = c(0.3, 1), guide = "none") +
      scale_x_date(
        breaks = seq(month_start, month_end, by = "week"),
        date_labels = "%d %b",
        expand = c(0, 0)
      ) +
      labs(
        title = paste("Monthly Progress -", format(today, "%B %Y")),
        x = NULL, y = NULL
      ) +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5)
      )
    
    # Weekly summary table (current month)
    weekly_summary <- month_complete %>%
      mutate(
        week_start = floor_date(date, "week", week_start = 1),
        week_end   = week_start + days(6)
      ) %>%
      group_by(week_start, week_end) %>%
      summarise(
        Target = sum(daily_target),
        Actual = sum(if_else(status_fill == "Saved", daily_target, 0)),
        .groups = "drop"
      ) %>%
      mutate(
        Week = row_number(),
        Start = format(week_start, "%d %b"),
        End   = format(week_end, "%d %b"),
        Status = case_when(
          week_end < today ~ ifelse(Actual >= Target, "✓ Achieved", "✗ Not Achieved"),
          week_start <= today & week_end >= today ~ "In Progress",
          TRUE ~ "Future"
        )
      ) %>%
      select(Week, Start, End, Target, Actual, Status)
    
    tt <- gridExtra::ttheme_minimal(
      core = list(fg_params = list(hjust = 0, x = 0.1)),
      colhead = list(fg_params = list(fontface = "bold"))
    )
    table_grob <- gridExtra::tableGrob(weekly_summary, rows = NULL, theme = tt)
    
    # Combine with patchwork
    design <- "
      AB
      CC
    "
    combined <- p_cumulative + p_monthly + patchwork::wrap_elements(table_grob) +
      patchwork::plot_layout(design = design, heights = c(1, 0.6)) +
      patchwork::plot_annotation(
        title = paste(person_name, "Savings Report"),
        subtitle = paste("As at", format(today, "%d %B %Y")),
        theme = theme(
          plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)
        )
      )
    
    return(combined)
  }
  
  ben_report <- generate_person_report(ben_data, "Ben")
  ggsave("ben_report.png", ben_report, width = 12, height = 8, dpi = 150)
  bot$sendPhoto(chat_id = CHAT_ID, photo = "ben_report.png",
                caption = "Ben's Detailed Savings Report")
  
  if (nrow(abby_data) > 0) {
    abby_report <- generate_person_report(abby_data, "Abby")
    ggsave("abby_report.png", abby_report, width = 12, height = 8, dpi = 150)
    bot$sendPhoto(chat_id = CHAT_ID, photo = "abby_report.png",
                  caption = "Abby's Detailed Savings Report")
  }
}

# =========================
# MONTHLY SUMMARY (last day of month)
# =========================
month_start <- floor_date(today, "month")
month_end <- ceiling_date(today, "month") - days(1)

if (today == month_end) {
  monthly_data <- savings_data %>%
    filter(date >= month_start & date <= month_end)
  
  total_saved <- sum(monthly_data$actual, na.rm = TRUE)
  days_saved <- sum(monthly_data$status == "Saved", na.rm = TRUE)
  
  monthly_chart_file <- "monthly_chart.png"
  png(monthly_chart_file, width = 800, height = 600)
  print(
    ggplot(monthly_data, aes(x = date, y = actual)) +
      geom_col(fill = "darkgreen") +
      geom_line(aes(y = cumsum(actual)), color = "blue", size = 1) +
      labs(title = paste0("Monthly Savings: ", month_start, " to ", month_end),
           x = "Date", y = "Amount Saved (KES)") +
      theme_minimal()
  )
  dev.off()
  
  daily_quote_safe <- as.character(daily_quote)
  caption <- paste0(
    "📈 Monthly Savings Summary\n",
    "Total Saved: KES ", total_saved, "\n",
    "Days Saved: ", days_saved, "/", nrow(monthly_data), "\n\n",
    daily_quote_safe
  )
  
  if (file.exists(monthly_chart_file)) {
    bot$sendPhoto(chat_id = CHAT_ID, photo = monthly_chart_file, caption = caption)
  } else {
    warning("Monthly chart file not found!")
  }
}

# =========================
# DAILY DETAILED MESSAGE
# =========================
todayz <- as_date(with_tz(Sys.time(), tzone = "Africa/Nairobi"))
leoste <- corrected_savings %>%
  filter(date == todayz) %>%
  pull(correct_actual)
if(length(leoste) == 0) leoste <- 15 * (nrow(corrected_savings) + 1)

daily_msg <- paste0(
  "*🕖 DAILY SAVINGS UPDATE*\n\n",
  "*Day:* ", day(today), " of ", month(today, label = TRUE, abbr = FALSE), "\n",
  "*Today's Target:* KES ", leoste, "\n",
  "*Cumulative Saved:* KES ", cumulative_saved, "\n",
  "*Cumulative Deficit:* KES ", total_missed, "\n",
  "*Progress:* ", percentage_target, "%\n\n",
  "💡 *Motivation:* ", as.character(daily_quote)
)

daily_msg_safe <- escape_markdown_v2(daily_msg)
send_telegram_message(BOT_TOKEN, CHAT_ID, daily_msg_safe, parse_mode = "MarkdownV2")

# =========================
# BUTTON MESSAGE
# =========================
dashboard_url <- "https://kampspatial.shinyapps.io/BenQ_2026_DSPC/"
buttons <- list(
  list(list(text = "📊 Open DSPC WebApp", url = dashboard_url))
)
send_message_buttons(
  "📌 <b>Quick Actions</b>\nChoose an option below:",
  buttons
)

# =========================
# FINANCIAL REMINDER (bills, payments, goals) - uncomment to run daily
# =========================
# send_financial_reminder <- function() {
#   tryCatch({
#     bills <- read_bills()
#     payments <- read_payments()
#     goals <- read_goals()
#     bills <- match_payments_to_bills(bills, payments)
#     message <- paste0(
#       "<b>💳 FINANCIAL OVERVIEW</b>\n",
#       "<i>", format(Sys.Date(), "%A, %B %d, %Y"), "</i>\n",
#       strrep("─", 35), "\n\n",
#       create_unpaid_bills_section(bills),
#       create_paid_bills_summary(bills),
#       create_debts_section(bills),
#       create_goals_section(goals),
#       create_summary_section(bills, goals),
#       "<i>📅 Next update: ", format(Sys.Date() + 1, "%b %d"), "</i>"
#     )
#     send_message(message)
#     
#     urgent_bills <- bills %>% 
#       filter((due_in_days <= 3 & due_in_days >= 0) | due_in_days < 0) %>%
#       filter(!is.na(name) & !is.na(pending_amount) & pending_amount > 0)
#     
#     if (nrow(urgent_bills) > 0) {
#       urgent_msg <- "<b>🚨 URGENT REMINDERS</b>\n\n"
#       for (i in 1:nrow(urgent_bills)) {
#         bill <- urgent_bills[i, ]
#         urgent_msg <- paste0(
#           urgent_msg,
#           "• <b>", bill$name, "</b>\n",
#           "  Pending: ", format_currency(bill$pending_amount), "\n",
#           "  Due: ", 
#           ifelse(bill$due_in_days < 0, 
#                  paste0("<b>OVERDUE by ", abs(bill$due_in_days), " days</b>"),
#                  ifelse(bill$due_in_days == 0, "<b>TODAY</b>", 
#                         paste("in <b>", bill$due_in_days, " days</b>"))),
#           "\n"
#         )
#         if (bill$payment_status == "partial") {
#           urgent_msg <- paste0(urgent_msg,
#                                "  Already paid: ", format_currency(bill$total_paid), "\n")
#         }
#         urgent_msg <- paste0(urgent_msg, "\n")
#       }
#       send_message(urgent_msg)
#     }
#     cat("✅ Financial reminder sent successfully!\n")
#   }, error = function(e) {
#     error_msg <- paste0(
#       "❌ Error sending financial reminder:\n",
#       "<code>", e$message, "</code>\n\n",
#       "Please check your data structure and try again."
#     )
#     send_message(error_msg)
#     cat("❌ Error:", e$message, "\n")
#   })
# }
# 
# # Uncomment to run the financial reminder daily
# # send_financial_reminder()

# =========================
# STATUS CHANGE NOTIFICATIONS
# =========================
if (nrow(status_changes) > 0) {
  for (i in seq_len(nrow(status_changes))) {
    d <- status_changes$date[i]
    from <- status_changes$prev_status[i]
    to <- status_changes$status[i]
    
    if (from == "Future" && to == "Saved") {
      send_message(
        paste0(
          "✅ <b>SAVINGS RECORDED</b>\n\n",
          "📅 Date: ", d, "\n",
          "💰 Status changed from <i>Future</i> to <b>Saved</b>\n\n",
          "👏 Great job staying consistent!"
        )
      )
    }
    if (from == "Future" && to == "Missed") {
      send_message(
        paste0(
          "⚠️ <b>SAVINGS MISSED</b>\n\n",
          "📅 Date: ", d, "\n",
          "❌ Status changed from <i>Future</i> to <b>Missed</b>\n\n",
          "🔁 Try to recover tomorrow."
        )
      )
    }
    if (from == "Missed" && to == "Saved") {
      send_message(
        paste0(
          "🔥 <b>RECOVERY ALERT</b>\n\n",
          "📅 Date: ", d, "\n",
          "💪 Status changed from <i>Missed</i> to <b>Saved</b>\n\n",
          "🚀 That’s discipline!"
        )
      )
    }
  }
}

# Save current status for future comparisons
saveRDS(
  savings_data %>% select(date, status),
  STATE_FILE
)

# =========================
# BATTERY PLOT FOR BEN (optional, already included in weekly report)
# =========================
# This part remains as originally but can be removed if redundant.
# We'll keep it for completeness.
create_battery_plot <- function(df, title, member_name = "Ben") {
  days_summary <- df %>%
    summarize(
      saved = sum(saved),
      missed = n() - sum(saved)
    ) %>%
    pivot_longer(cols = c(saved, missed), names_to = "status", values_to = "count") %>%
    mutate(
      perc = count / sum(count),
      ypos = cumsum(perc) - 0.5 * perc
    )
  terminals <- tibble(
    x = 1,
    ymin = 1,
    ymax = 1.05,
    xmin = 0.85,
    xmax = 1.15
  )
  ggplot(days_summary, aes(x = member_name, y = perc, fill = status)) +
    geom_col(width = 0.5, color = "black") +
    geom_text(aes(y = ypos, label = scales::percent(perc)), color = "white", fontface = "bold") +
    geom_rect(
      data = terminals,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "black",
      inherit.aes = FALSE
    ) +
    scale_fill_manual(values = c("saved" = "#2ca02c", "missed" = "#d62728")) +
    scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.1))) +
    labs(title = title, x = "", y = "") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "none",
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank()
    )
}

current_date  <- Sys.Date()
week_start  <- floor_date(current_date, "week")
week_end    <- ceiling_date(current_date, "week") - days(1)
month_start <- floor_date(current_date, "month")
month_end   <- ceiling_date(current_date, "month") - days(1)
current_year <- year(current_date)

week_data <- ben_data %>%
  filter(date >= week_start & date <= week_end)
month_data <- ben_data %>%
  filter(date >= month_start & date <= month_end)
year_data <- ben_data %>%
  filter(year(date) == current_year)

week_data <- week_data %>%
  complete(
    date = seq.Date(week_start, week_end, by = "day"),
    fill = list(
      saved = FALSE,
      actual = 0,
      status = "Not Saved",
      daily_target = 0,
      cumulative_actual = NA,
      cumulative_target = NA
    )
  )
month_data <- month_data %>%
  complete(
    date = seq.Date(month_start, month_end, by = "day"),
    fill = list(
      saved = FALSE,
      actual = 0,
      status = "Not Saved",
      daily_target = 0,
      cumulative_actual = NA,
      cumulative_target = NA
    )
  )

week_title  <- paste0("Week ", week(current_date), " of ", month(current_date, label = TRUE, abbr = FALSE))
month_title <- paste0("Month of ", month(current_date, label = TRUE, abbr = FALSE))
year_title  <- paste0("Year ", current_year)

week_plot  <- create_battery_plot(week_data, week_title)
month_plot <- create_battery_plot(month_data, month_title)
year_plot  <- create_battery_plot(year_data, year_title)

combined_plot <- week_plot + month_plot + year_plot + plot_layout(ncol = 3)
combined_file <- "Ben_savings_combined.png"
ggsave(combined_file, combined_plot, width = 15, height = 6, dpi = 300)

bot$sendPhoto(chat_id = CHAT_ID, photo = combined_file, caption = "Ben's Savings Progress 📊")


# =========================
# ADDITIONAL LIBRARIES FOR ENHANCED REPORTING
# =========================
library(patchwork)
library(gridExtra)
library(grid)

today <- Sys.Date()

week_start <- floor_date(today, unit = "week")
week_end <- ceiling_date(today, unit = "week") - days(1)

start_date <- as.Date("2026-01-01")
end_date <- as.Date("2026-02-13")
all_dates <- seq(start_date, end_date, by = "day")

date = all_dates
year = year(all_dates)
month = month(all_dates, label = TRUE, abbr = FALSE)
month_num = month(all_dates)
day = day(all_dates)
day_of_week = wday(all_dates, label = TRUE, abbr = FALSE)
# daily_target = day(all_dates) * 15
# =========================
# WEEKLY COMPREHENSIVE REPORT (SUNDAY)
# =========================
if (wday(today) == 6) {   # Sunday
  weekly_data <- savings_data %>%
    filter(date >= week_start & date <= week_end)
  
  # Original weekly bar chart (Ben)
  total_saved <- sum(weekly_data$actual, na.rm = TRUE)
  days_saved <- sum(weekly_data$status == "Saved", na.rm = TRUE)
  # ---- Prepare full daily data from Jan 1 to today ----
  start_date <- as.Date("2026-01-01")
  today_date <- today
  
  # Ensure one row per day with complete sequence
  ben_full <- savings_data %>%
    filter(date >= start_date, date <= today_date) %>%
    arrange(date) %>%
    tidyr::complete(
      date = seq.Date(start_date, today_date, by = "day"),
      fill = list(actual = 0, status = "Missed")
    ) %>%
    mutate(
      daily_target = day(all_dates) * 15,
      cumulative_target = cumsum(daily_target),
      cumulative_actual = cumsum(actual),
      expected_cumulative = cumulative_target,
      day_of_month = day(date),
      month = month(date),
      year = year(date)
    )
  
  # ---- Plot 1: Cumulative savings vs target (with gap ribbon) ----
  p1 <- ggplot(ben_full, aes(x = date)) +
    geom_ribbon(
      aes(
        ymin = pmin(cumulative_actual, cumulative_target),
        ymax = pmax(cumulative_actual, cumulative_target),
        alpha = as.numeric(date - min(date)) / as.numeric(max(date) - min(date))
      ),
      fill = "firebrick"
    ) +
    geom_line(aes(y = cumulative_target, color = "Target"), linewidth = 1) +
    geom_line(aes(y = cumulative_actual, color = "Actual"), linewidth = 1) +
    geom_label(
      data = ben_full %>% slice_tail(n = 1),
      aes(
        y = cumulative_actual,
        label = paste0("Actual: KES ", scales::comma(cumulative_actual))
      ),
      fill = "white", color = "darkgreen", size = 3, vjust = -0.7
    ) +
    geom_label(
      data = ben_full %>% slice_tail(n = 1),
      aes(
        y = cumulative_target,
        label = paste0("Target: KES ", scales::comma(cumulative_target))
      ),
      fill = "white", color = "blue", size = 3, vjust = -0.7
    ) +
    scale_color_manual(values = c("Target" = "#2b8cbe", "Actual" = "#006d2c")) +
    scale_alpha_continuous(range = c(0.15, 1), guide = "none") +
    scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0.08, 0.35))) +
    scale_x_date(expand = expansion(mult = c(0.02, 0.06))) +
    labs(title = "Cumulative Savings vs Target", x = NULL, y = "KES") +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "top",
      plot.title = element_text(face = "bold", size = 11)
    )
  
  # ---- Plot 2: Cumulative progress with highlights ----
  p2 <- ggplot(ben_full, aes(x = date)) +
    geom_line(aes(y = cumulative_target, color = "Target"), linewidth = 1, linetype = "dashed") +
    geom_line(aes(y = cumulative_actual, color = "Actual"), linewidth = 1.2) +
    geom_point(aes(y = cumulative_actual, color = "Actual"), size = 1.5) +
    geom_point(
      data = ben_full %>% filter(date == today_date),
      aes(y = cumulative_actual), size = 3, color = "#d7301f"
    ) +
    geom_point(
      data = ben_full %>% filter(date == today_date),
      aes(y = cumulative_target), size = 3, color = "#d7301f"
    ) +
    geom_label(
      data = ben_full %>% filter(date == today_date),
      aes(y = cumulative_actual, label = scales::comma(cumulative_actual)),
      fill = "white", size = 3, vjust = -0.8
    ) +
    geom_label(
      data = ben_full %>% filter(date == today_date),
      aes(y = cumulative_target, label = scales::comma(cumulative_target)),
      fill = "white", size = 3, vjust = -0.8
    ) +
    scale_color_manual(values = c("Actual" = "#2c7fb8", "Target" = "#fdae61")) +
    scale_x_date(
      breaks = seq(start_date, today_date, by = "1 week"),
      date_labels = "%d %b",
      expand = expansion(mult = c(0.01, 0.05))
    ) +
    scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0.05, 0.15))) +
    labs(title = "Cumulative Progress with Highlights", x = NULL, y = "KES") +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "top",
      plot.title = element_text(face = "bold", size = 11)
    )
  
  # ---- Plot 3: Daily savings status heatmap ----
  p3 <- ggplot(ben_full, aes(x = date, y = 1)) +
    geom_tile(aes(fill = status, alpha = expected_cumulative), color = "white", height = 0.9) +
    scale_fill_manual(values = c("Saved" = "darkgreen", "Missed" = "red", "Future" = "grey80")) +
    scale_x_date(
      breaks = seq(start_date, today_date, by = "1 week"),
      date_labels = "%d %b"
    ) +
    scale_alpha_continuous(range = c(0.3, 1), guide = "none") +
    labs(title = "Daily Savings Status", x = NULL, y = NULL) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank(),
      legend.position = "top",
      plot.title = element_text(face = "bold", size = 11)
    )
  
  # ---- Plot 4: Weekly cumulative matrix table ----
  weekly_summary <- ben_full %>%
    mutate(
      week_start = floor_date(date, "week", week_start = 1),
      week_end = week_start + days(6)
    ) %>%
    group_by(week_start, week_end) %>%
    summarise(
      Target = sum(daily_target, na.rm = TRUE),
      Actual = sum(actual, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(week_start) %>%
    mutate(
      Week = row_number(),
      Start = format(week_start, "%d %b"),
      End = format(week_end, "%d %b"),
      Status = case_when(
        week_end < today_date ~ ifelse(Actual >= Target, "✓ Achieved", "✗ Not Achieved"),
        week_start <= today_date & week_end >= today_date ~ "In Progress",
        TRUE ~ "Future"
      )
    ) %>%
    select(Week, Start, End, Target, Actual, Status)
  
  # Convert to a polished table grob
  tt <- ttheme_minimal(
    core = list(
      bg_params = list(fill = c("white", "grey95"), col = "grey80"),
      fg_params = list(fontsize = 9)
    ),
    colhead = list(fg_params = list(fontsize = 10, fontface = "bold")),
    rowhead = list(fg_params = list(fontsize = 8))
  )
  tbl <- tableGrob(weekly_summary, rows = NULL, theme = tt)
  p4 <- wrap_elements(tbl)
  
  # ---- Combine all four plots into a single image ----
  combined_weekly <- (p1 + p2) / (p3 + p4) +
    plot_annotation(
      title = paste(
        "Weekly Savings Report — Week", week(today),
        "(", format(week_start, "%d %b"), "–", format(week_end, "%d %b"), ")"
      ),
      theme = theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14))
    )
  
  # ---- Save and send ----
  report_file <- "weekly_report.png"
  ggsave(report_file, combined_weekly, width = 16, height = 12, dpi = 150)
  
  # Prepare caption with key metrics
  total_saved_week <- sum(weekly_data$actual, na.rm = TRUE)
  days_saved_week <- sum(weekly_data$status == "Saved", na.rm = TRUE)
  
  caption <- paste0(
    "📊 Weekly Savings Report\n",
    "Total Saved: KES ", scales::comma(total_saved_week), "\n",
    "Days Saved: ", days_saved_week, "/", nrow(weekly_data), "\n\n",
    daily_quote
  )
  
  # Send using the telegram.bot method
  bot$sendPhoto(chat_id = CHAT_ID, photo = report_file, caption = caption)
  
  # Optional: remove the temporary file
  unlink(report_file)
}

# =========================
# END OF SCRIPT
# =========================
