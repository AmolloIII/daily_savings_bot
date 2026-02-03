# friday_savings_email.R - DEBUG VERSION
Sys.setenv(
  MY_GMAIL_ACCOUNT = Sys.getenv('MY_GMAIL_ACCOUNT'),
  SMTP_PASSWORD = Sys.getenv('SMTP_PASSWORD')
)

library(tidyr)
library(lubridate)
library(gt)
library(dplyr)
library(googlesheets4)
library(blastula)
library(glue)

cat("=== DEBUG MODE ===\n")

# Test email credentials first
cat("Testing email credentials...\n")
my_email_creds <- creds_envvar(
  user = Sys.getenv('MY_GMAIL_ACCOUNT'),
  pass_envvar = 'SMTP_PASSWORD',
  provider = 'gmail'
)

# Test with a simple email
test_email <- compose_email(
  body = "Test email from Friday script. If you receive this, credentials are working."
)

tryCatch({
  smtp_send(
    email = test_email,
    from = Sys.getenv("MY_GMAIL_ACCOUNT"),
    to = "amollozeethird@gmail.com",
    subject = "Test Email - Friday Script",
    credentials = my_email_creds
  )
  cat("✅ Test email sent successfully!\n")
}, error = function(e) {
  cat("❌ Test email failed:", e$message, "\n")
})

# Now let's debug the main function
cat("\n=== Testing data loading ===\n")

tryCatch({
  # Authenticate and load data
  gs4_auth(path = "gs.json")
  sheet_id <- "1qidIxYD2DAOIZ64ONtbphsJOXdPDXnxVnP1kcJs_Qx0"
  
  daily_dataz <- read_sheet(sheet_id, sheet = "Member Contributions") 
  daily_dataz <- daily_dataz %>%
    dplyr::filter(!Name %in% c("Total Contributions", "Banked"))
  
  cat("✅ Data loaded successfully. Rows:", nrow(daily_dataz), "\n")
  
  # Test with just one member
  test_member <- "Ben Amollo"
  cat("\n=== Testing with member:", test_member, "===\n")
  
  # Get member data
  member_data <- daily_dataz %>%
    filter(Name == test_member) %>%
    pivot_longer(
      cols = matches("^\\d{1,2}/\\d{1,2}/\\d{4}$"),
      names_to = "date",
      values_to = "actual"
    ) %>%
    mutate(
      date = dmy(date),
      saved = !is.na(actual),
      status = if_else(saved, "Saved", "Not Saved"),
      actual = replace_na(actual, 0),
      date = as.Date(date),
      day_in_month = day(date),
      target = day_in_month * 15,
      week_start = floor_date(date, "week", week_start = 1),
      week_end = week_start + days(6)
    ) %>%
    arrange(date)
  
  cat("✅ Member data processed. Rows:", nrow(member_data), "\n")
  
  # Filter for current month
  today <- Sys.Date()
  current_year <- year(today)
  selected_month <- month(today)
  
  dfz <- member_data %>%
    filter(
      year(date) == current_year,
      month(date) == selected_month
    )
  
  cat("✅ Filtered for current month. Rows:", nrow(dfz), "\n")
  
  if(nrow(dfz) > 0) {
    # Create weekly summary
    weekly_data <- dfz %>%
      group_by(week_start, week_end) %>%
      summarise(
        Target = sum(target, na.rm = TRUE),
        Actual = sum(actual, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(week_start) %>%
      mutate(
        Week = row_number(),
        Start_Date = format(week_start, "%d %b"),
        End_Date = format(week_end, "%d %b")
      )
    
    cat("✅ Weekly data created. Weeks:", nrow(weekly_data), "\n")
    
    # Create gt table
    weekly_gt <- weekly_data %>%
      select(Week, Start_Date, End_Date, Target, Actual) %>%
      gt() %>%
      cols_label(
        Week = "Week #",
        Start_Date = "Start Date",
        End_Date = "End Date",
        Target = "Weekly Target",
        Actual = "Actual Savings"
      ) %>%
      fmt_currency(
        columns = c(Target, Actual),
        currency = "KES",
        decimals = 0
      ) %>%
      cols_align(align = "center", columns = Week) %>%
      opt_table_outline()
    
    cat("✅ GT table created successfully\n")
    
    # Test converting to HTML
    cat("Testing GT table to HTML conversion...\n")
    table_html <- gt::as_raw_html(weekly_gt)
    cat("✅ GT table converted to HTML. Length:", nchar(table_html), "\n")
    
    # Test creating a simple email
    cat("Testing simple email creation...\n")
    
    # Simple function to format KES
    format_kes <- function(x) {
      if(is.numeric(x)) {
        return(paste0("KES ", format(round(x, 2), big.mark = ",", nsmall = 2)))
      }
      return(paste0("KES ", x))
    }
    
    # Calculate totals
    current_week_data <- member_data %>%
      filter(date >= floor_date(today, unit = "week") &
             date <= ceiling_date(today, unit = "week") - days(1))
    
    current_week_total <- sum(current_week_data$actual, na.rm = TRUE)
    month_total <- sum(dfz$actual, na.rm = TRUE)
    
    cat("Current week total:", current_week_total, "\n")
    cat("Month total:", month_total, "\n")
    
    # Create simple HTML
    simple_html <- paste0(
      '<html><body>',
      '<h1>Test Email for ', test_member, '</h1>',
      '<p>Current week total: ', format_kes(current_week_total), '</p>',
      '<p>Month total: ', format_kes(month_total), '</p>',
      table_html,
      '</body></html>'
    )
    
    # Create email
    test_email2 <- compose_email(body = html(simple_html))
    
    # Send test email
    cat("Sending test email...\n")
    smtp_send(
      email = test_email2,
      from = Sys.getenv("MY_GMAIL_ACCOUNT"),
      to = "amollozeethird@gmail.com",
      subject = paste("DEBUG - Weekly Savings Test for", test_member),
      credentials = my_email_creds
    )
    
    cat("✅ Test email with data sent successfully!\n")
    
  } else {
    cat("⚠️ No data for current month\n")
  }
  
}, error = function(e) {
  cat("❌ Error in debug mode:", e$message, "\n")
  cat("Traceback:\n")
  traceback()
})
