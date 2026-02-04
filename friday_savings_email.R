# friday_savings_email.R
# Weekly savings email script - Runs on Fridays at 7 PM Kenya time (16:00 UTC)

# Load required packages
library(tidyr)
library(lubridate)
library(gt)
library(dplyr)
library(googlesheets4)
library(blastula)
library(glue)

cat("📧 Friday Savings Email Script\n")
cat("==============================\n\n")

# Member information mapping
members_info <- data.frame(
  sheet_name = c(
    "Ben Amollo",
    "Shabir Odhiambo", 
    "Arthur Mbatia",
    "Stephen Katana",
    "James Nyaga",
    "Collins Korir",
    "Meshack Ngava",
    "Orvile Oreener",
    "George Njoroge",
    "Dan Njenga"
  ),
  display_name = c(
    "Ben Amollo",
    "Shabir",
    "Kanyanjua",
    "Katz",
    "Mutugi Nyaga",
    "Kolo",
    "Ngava",
    "Double O",
    "Njoro",
    "Wakajiado3"
  ),
  email = c(
    "amollozeethird@gmail.com",
    "ayoubshabir@gmail.com",
    "aspkenya@gmail.com",
    "lamiri93@gmail.com",
    "mutugiwanyaga24@gmail.com",
    "korayalakwen@gmail.com",
    "ngavamuumbi@outlook.com",
    "orvillenyandoro@gmail.com",
    "gnjoro9@gmail.com",
    "danielkaroga@gmail.com"
  ),
  order_number = c(10, 8, 1, 9, 7, 5, 2, 4, 3, 6),
  stringsAsFactors = FALSE
)

# Create payout dates (in order)
payout_dates <- as.Date(
  c(
    "2026-02-08", "2026-02-15", "2026-02-22",
    "2026-03-01", "2026-03-08", "2026-03-15",
    "2026-03-22", "2026-03-29",
    "2026-04-05", "2026-04-12"
  )
)

# Function to format currency
format_kes <- function(amount) {
  if (is.na(amount) || amount == 0) {
    return("KES 0")
  }
  return(paste0("KES ", format(round(amount), big.mark = ",", nsmall = 0)))
}

# Function to create email body
create_email_body <- function(member_name, weekly_table_html, 
                              payout_member, payout_date_str,
                              current_week_total, month_total, 
                              current_date) {
  
  week_number <- isoweek(current_date)
  formatted_date <- format(current_date, "%A, %B %d, %Y")
  
  # Build HTML
  html_content <- paste0('
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <style>
        body { font-family: Arial, sans-serif; line-height: 1.6; color: #333; margin: 0; padding: 20px; background-color: #f4f4f4; }
        .container { max-width: 600px; margin: 0 auto; background-color: white; padding: 20px; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); }
        .header { background-color: #4CAF50; color: white; padding: 20px; text-align: center; border-radius: 5px 5px 0 0; }
        .content { padding: 20px; }
        .stats { display: flex; justify-content: space-between; margin: 20px 0; }
        .stat-box { flex: 1; padding: 15px; margin: 0 10px; border-radius: 5px; text-align: center; }
        .week-stat { background-color: #e8f5e9; border-left: 4px solid #2e7d32; }
        .month-stat { background-color: #e3f2fd; border-left: 4px solid #1565c0; }
        .stat-value { font-size: 24px; font-weight: bold; margin: 10px 0; }
        .payout-box { background-color: #fff3e0; padding: 15px; border-radius: 5px; border-left: 4px solid #e65100; margin: 20px 0; }
        table { width: 100%; border-collapse: collapse; margin: 20px 0; }
        th, td { padding: 10px; text-align: left; border-bottom: 1px solid #ddd; }
        th { background-color: #f2f2f2; }
        .current-week { background-color: #d4edda; }
        .footer { text-align: center; margin-top: 20px; padding-top: 20px; border-top: 1px solid #eee; color: #777; font-size: 12px; }
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>💰 Weekly Savings Update</h1>
            <p>Week ', week_number, ' • ', formatted_date, '</p>
        </div>
        
        <div class="content">
            <h2>Hello ', member_name, '!</h2>
            
            <div class="stats">
                <div class="stat-box week-stat">
                    <h3>This Week\'s Savings</h3>
                    <div class="stat-value">', format_kes(current_week_total), '</div>
                </div>
                
                <div class="stat-box month-stat">
                    <h3>Month-to-Date</h3>
                    <div class="stat-value">', format_kes(month_total), '</div>
                </div>
            </div>
            
            <div class="payout-box">
                <h3>🏆 Upcoming Payout</h3>
                <p><strong>Next Recipient:</strong> ', payout_member, '</p>
                <p><strong>Payout Date:</strong> ', payout_date_str, '</p>
            </div>
            
            <h3>📊 Your Savings Progress</h3>
            ', weekly_table_html, '
            
            <div class="tip-box">
                <p><strong>💡 Tip:</strong> Consistency is key! Try to save daily to meet your weekly targets.</p>
            </div>
        </div>
        
        <div class="footer">
            <p>Savings Group • Automated Weekly Update</p>
            <p>© ', format(Sys.Date(), "%Y"), '</p>
        </div>
    </div>
</body>
</html>')
  
  return(html_content)
}

# Function to test email credentials
test_email_credentials <- function() {
  cat("🔧 Testing email credentials...\n")
  
  tryCatch({
    # Get credentials from environment
    gmail_user <- Sys.getenv("MY_GMAIL_ACCOUNT")
    gmail_pass <- Sys.getenv("SMTP_PASSWORD")
    
    if (gmail_user == "" || gmail_pass == "") {
      cat("❌ Email credentials not found in environment variables\n")
      cat("   MY_GMAIL_ACCOUNT:", ifelse(gmail_user == "", "NOT SET", "SET"), "\n")
      cat("   SMTP_PASSWORD:", ifelse(gmail_pass == "", "NOT SET", "SET (hidden)"), "\n")
      return(FALSE)
    }
    
    cat("✅ Email credentials found in environment\n")
    
    # Create test credentials
    test_creds <- creds_envvar(
        user = Sys.getenv('MY_GMAIL_ACCOUNT'),
        pass_envvar = 'SMTP_PASSWORD',
        provider = 'gmail'
      ))
    
    # Try to create a simple test email
    test_email <- compose_email(
      body = "This is a test email to verify SMTP credentials are working correctly."
    )
    
    cat("📨 Attempting to send test email...\n")
    
    # Send test email
    smtp_send(
      email = test_email,
      from = gmail_user,
      to = gmail_user,  # Send to self
      subject = "Test Email - Friday Savings Script",
      credentials = test_creds
    )
    
    cat("✅ Test email sent successfully!\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Email test failed:", e$message, "\n")
    cat("\n🔧 TROUBLESHOOTING TIPS:\n")
    cat("1. Make sure you're using an APP PASSWORD, not your regular Gmail password\n")
    cat("2. Enable 2-Step Verification on your Google account\n")
    cat("3. Generate an App Password at: https://myaccount.google.com/apppasswords\n")
    cat("4. For GitHub Actions, set secrets in: Settings > Secrets and variables > Actions\n")
    return(FALSE)
  })
}

# Main execution function
run_friday_email <- function(send_emails = FALSE) {
  success_count <- 0
  total_members <- nrow(members_info)
  
  cat("Starting Friday email script...\n")
  cat("Send emails mode:", ifelse(send_emails, "ENABLED", "DISABLED (saving to files)"), "\n\n")
  
  # Test email credentials if sending is enabled
  if (send_emails) {
    if (!test_email_credentials()) {
      cat("\n⚠️ Switching to file save mode due to email credential issues\n")
      send_emails <- FALSE
    }
  }
  
  tryCatch({
    # Create payout dataframe
    payout_df <- members_info %>%
      arrange(order_number) %>%
      mutate(Payout_Date = payout_dates[order_number])
    
    # -------- READ & PREPARE DAILY DATA --------
    cat("Authenticating with Google Sheets...\n")
    
    # Create credentials file from environment variable
    gs4_auth(path = "gs.json")
    
    sheet_id <- "1qidIxYD2DAOIZ64ONtbphsJOXdPDXnxVnP1kcJs_Qx0"
    
    daily_dataz <- read_sheet(sheet_id, sheet = "Member Contributions") 
    daily_dataz <- daily_dataz %>%
      dplyr::filter(!Name %in% c("Total Contributions", "Banked"))
    
    cat("✅ Data loaded successfully\n")
    
    # Set up email credentials if sending
    if (send_emails) {
      email_creds <- creds(
        user = Sys.getenv("MY_GMAIL_ACCOUNT"),
        pass = Sys.getenv("SMTP_PASSWORD"),
        provider = "gmail",
        host = "smtp.gmail.com",
        port = 587,
        use_ssl = TRUE
      )
    }
    
    # Process each member
    for(i in 1:nrow(members_info)) {
      member <- members_info[i, ]
      
      cat(sprintf("\n[%d/%d] Processing %s...\n", i, total_members, member$display_name))
      
      tryCatch({
        # Get member data
        member_data <- daily_dataz %>%
          filter(Name == member$sheet_name) %>%
          pivot_longer(
            cols = matches("^\\d{1,2}/\\d{1,2}/\\d{4}$"),
            names_to = "date",
            values_to = "actual"
          ) %>%
          mutate(
            date = as.Date(dmy(date)),
            actual = replace_na(actual, 0)
          ) %>%
          arrange(date) %>%
          head(366)
        
        if(nrow(member_data) == 0) {
          cat("  ⚠️ No data found\n")
          next
        }
        
        # Get current date info
        today <- Sys.Date()
        selected_month <- month(today)
        current_year <- year(today)
        current_week_start <- floor_date(today, "week", week_start = 1)
        
        # Add calculated columns
        member_data <- member_data %>%
          mutate(
            day_in_month = day(date),
            target = day_in_month * 15,
            week_start = floor_date(date, "week", week_start = 1),
            week_end = week_start + days(6)
          )
        
        # Filter for current month
        monthly_data <- member_data %>%
          filter(
            year(date) == current_year,
            month(date) == selected_month
          )
        
        if(nrow(monthly_data) == 0) {
          cat("  ⚠️ No data for current month\n")
          next
        }
        
        # Create weekly summary
        weekly_data <- monthly_data %>%
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
            End_Date = format(week_end, "%d %b"),
            Week_Status = case_when(
              week_start < current_week_start ~ "past",
              week_start == current_week_start ~ "current",
              week_start > current_week_start ~ "future"
            )
          )
        
        # Create HTML table
        table_html <- paste0('
        <table border="1" cellpadding="5" cellspacing="0" style="border-collapse: collapse; width: 100%;">
        <thead>
        <tr style="background-color: #f2f2f2;">
            <th>Week #</th>
            <th>Start Date</th>
            <th>End Date</th>
            <th>Weekly Target</th>
            <th>Actual Savings</th>
        </tr>
        </thead>
        <tbody>')
        
        for(j in 1:nrow(weekly_data)) {
          week <- weekly_data[j, ]
          row_class <- ifelse(week$Week_Status == "current", "current-week", "")
          table_html <- paste0(table_html, '
        <tr class="', row_class, '">
            <td>', week$Week, '</td>
            <td>', week$Start_Date, '</td>
            <td>', week$End_Date, '</td>
            <td>', format_kes(week$Target), '</td>
            <td>', format_kes(week$Actual), '</td>
        </tr>')
        }
        
        table_html <- paste0(table_html, '
        </tbody>
        </table>')
        
        # Calculate totals
        current_week_data <- member_data %>%
          filter(date >= floor_date(today, unit = "week") &
                 date <= ceiling_date(today, unit = "week") - days(1))
        
        current_week_total <- sum(current_week_data$actual, na.rm = TRUE)
        month_total <- sum(monthly_data$actual, na.rm = TRUE)
        
        # Get payout information
        next_payout <- payout_df %>%
          filter(Payout_Date >= today) %>%
          slice(1)
        
        payout_member <- ifelse(nrow(next_payout) > 0, next_payout$display_name, "Not scheduled")
        payout_date_str <- ifelse(nrow(next_payout) > 0, 
                                 format(next_payout$Payout_Date, "%B %d, %Y"), 
                                 "Not scheduled")
        
        # Create email body
        email_body <- create_email_body(
          member_name = member$display_name,
          weekly_table_html = table_html,
          payout_member = payout_member,
          payout_date_str = payout_date_str,
          current_week_total = current_week_total,
          month_total = month_total,
          current_date = today
        )
        
        if (send_emails) {
          # Send email
          cat("  📨 Sending email to", member$email, "...\n")
          
          email_msg <- compose_email(body = html(email_body))
          
          smtp_send(
            email = email_msg,
            from = Sys.getenv("MY_GMAIL_ACCOUNT"),
            to = member$email,
            subject = paste("💰 Weekly Savings Update - Week", isoweek(today), "|", format(today, "%B %d, %Y")),
            credentials = email_creds
          )
          
          cat("  ✅ Email sent successfully!\n")
          success_count <- success_count + 1
          
          # Add delay to avoid rate limiting
          if(i < total_members) {
            Sys.sleep(2)
          }
          
        } else {
          # Save email to file
          filename <- paste0("email_", gsub(" ", "_", member$display_name), "_", 
                            format(today, "%Y%m%d"), ".html")
          writeLines(email_body, filename)
          
          cat(sprintf("  💾 Email saved to %s\n", filename))
          success_count <- success_count + 1
        }
        
      }, error = function(e) {
        cat(sprintf("  ❌ Error: %s\n", e$message))
      })
    }
    
    if (send_emails) {
      cat(sprintf("\n📊 Summary: %d/%d emails sent successfully\n", success_count, total_members))
    } else {
      cat(sprintf("\n📊 Summary: %d/%d emails saved as HTML files\n", success_count, total_members))
      cat("📁 Check the generated HTML files in your current directory\n")
    }
    
    return(TRUE)
    
  }, error = function(e) {
    cat(sprintf("❌ Critical error: %s\n", e$message))
    return(FALSE)
  })
}

# Check command line arguments
args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 0 && args[1] == "--send") {
  # Run with email sending enabled
  cat("🚀 Running with email sending enabled\n")
  run_friday_email(send_emails = TRUE)
} else {
  # Run in file save mode (default)
  cat("💾 Running in file save mode (use --send to send emails)\n")
  run_friday_email(send_emails = FALSE)
}

