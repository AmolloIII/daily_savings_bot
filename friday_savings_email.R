# friday_savings_email.R
# Weekly savings email script - Runs on Fridays at 7 PM Kenya time (16:00 UTC)

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
send_emails <- "--send" %in% args

cat("Send emails mode:", send_emails, "\n")

# Load required packages
library(tidyr)
library(lubridate)
library(gt)
library(dplyr)
library(googlesheets4)
library(blastula)
library(glue)

# Helper function to format currency
format_kes <- function(x) {
  if (is.numeric(x)) {
    paste0("KES ", format(round(as.numeric(x), 2), big.mark = ",", nsmall = 2))
  } else {
    paste0("KES ", x)
  }
}

#' Create HTML email body with proper structure
create_email_body <- function(member_name, weekly_table, payout_info, 
                              current_week_total, month_total, current_date) {
  
  week_number <- isoweek(current_date)
  formatted_date <- format(current_date, "%A, %B %d, %Y")
  
  # Convert gt table to HTML string
  table_html <- gt::as_raw_html(weekly_table)
  
  # Create the complete HTML email
  html_content <- glue::glue('
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Weekly Savings Update</title>
    <style>
        body {{
            font-family: Arial, sans-serif;
            line-height: 1.6;
            color: #333;
            margin: 0;
            padding: 0;
            background-color: #f4f4f4;
        }}
        .container {{
            max-width: 600px;
            margin: 0 auto;
            background-color: #ffffff;
        }}
        .header {{
            background-color: #4CAF50;
            color: white;
            padding: 25px;
            text-align: center;
            border-radius: 5px 5px 0 0;
        }}
        .header h1 {{
            margin: 0;
            font-size: 24px;
        }}
        .header p {{
            margin: 10px 0 0 0;
            font-size: 16px;
            opacity: 0.9;
        }}
        .content {{
            padding: 25px;
        }}
        .greeting {{
            color: #333;
            border-bottom: 2px solid #4CAF50;
            padding-bottom: 15px;
            margin-bottom: 20px;
            font-size: 20px;
        }}
        .stats-grid {{
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
            gap: 20px;
            margin: 25px 0;
        }}
        .stat-box {{
            padding: 20px;
            border-radius: 8px;
            text-align: center;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }}
        .savings-box {{
            background-color: #e8f5e9;
            border-left: 4px solid #2e7d32;
        }}
        .month-box {{
            background-color: #e3f2fd;
            border-left: 4px solid #1565c0;
        }}
        .stat-box h3 {{
            margin: 0 0 10px 0;
            font-size: 16px;
            color: #333;
        }}
        .stat-value {{
            font-size: 28px;
            font-weight: bold;
            margin: 0;
        }}
        .savings-value {{
            color: #1b5e20;
        }}
        .month-value {{
            color: #0d47a1;
        }}
        .payout-box {{
            background-color: #fff3e0;
            border-left: 4px solid #e65100;
            padding: 20px;
            border-radius: 8px;
            margin: 25px 0;
        }}
        .payout-box h3 {{
            color: #e65100;
            margin: 0 0 15px 0;
            font-size: 18px;
        }}
        .payout-info p {{
            margin: 8px 0;
            font-size: 16px;
            line-height: 1.5;
        }}
        .payout-info strong {{
            display: inline-block;
            min-width: 140px;
            color: #555;
        }}
        .table-section {{
            margin: 30px 0;
            overflow-x: auto;
        }}
        .table-section h3 {{
            color: #333;
            margin-bottom: 15px;
            font-size: 18px;
        }}
        .legend {{
            margin-top: 15px;
            font-size: 13px;
            color: #666;
            text-align: center;
            padding: 10px;
            background-color: #f9f9f9;
            border-radius: 4px;
        }}
        .legend-item {{
            display: inline-block;
            margin: 0 15px;
        }}
        .current-week {{
            color: #2e7d32;
            font-weight: bold;
        }}
        .past-week {{
            color: #dc3545;
        }}
        .future-week {{
            color: #6c757d;
        }}
        .tip-box {{
            background-color: #f5f5f5;
            padding: 20px;
            border-radius: 8px;
            margin-top: 25px;
            font-size: 14px;
            color: #555;
            border-left: 4px solid #6c757d;
        }}
        .tip-box p {{
            margin: 0 0 10px 0;
        }}
        .tip-box p:last-child {{
            margin-bottom: 0;
        }}
        .footer {{
            text-align: center;
            padding: 20px;
            color: #777;
            font-size: 12px;
            border-top: 1px solid #eee;
            background-color: #f9f9f9;
        }}
        @media (max-width: 600px) {{
            .stats-grid {{
                grid-template-columns: 1fr;
            }}
            .content {{
                padding: 15px;
            }}
            .payout-info strong {{
                min-width: 120px;
            }}
            .legend-item {{
                display: block;
                margin: 5px 0;
            }}
        }}
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>💰 Weekly Savings Update</h1>
            <p>Week {week_number} • {formatted_date}</p>
        </div>
        
        <div class="content">
            <h2 class="greeting">Hello {member_name}!</h2>
            
            <div class="stats-grid">
                <div class="stat-box savings-box">
                    <h3>This Week\'s Savings</h3>
                    <p class="stat-value savings-value">{format_kes(current_week_total)}</p>
                </div>
                
                <div class="stat-box month-box">
                    <h3>Month-to-Date</h3>
                    <p class="stat-value month-value">{format_kes(month_total)}</p>
                </div>
            </div>
            
            <div class="payout-box">
                <h3>🏆 Upcoming Payout</h3>
                <div class="payout-info">
                    <p><strong>Next Recipient:</strong> {payout_info$member}</p>
                    <p><strong>Payout Date:</strong> {ifelse(is.na(payout_info$date), "Not scheduled", format(payout_info$date, "%B %d, %Y"))}</p>
                    <p><strong>Days until payout:</strong> {ifelse(is.na(payout_info$days_until), "-", payout_info$days_until)}</p>
                </div>
            </div>
            
            <div class="table-section">
                <h3>📊 Your Savings Progress</h3>
                {table_html}
                
                <div class="legend">
                    <span class="legend-item current-week">● Current week</span>
                    <span class="legend-item past-week">● Past weeks</span>
                    <span class="legend-item future-week">● Upcoming weeks</span>
                </div>
            </div>
            
            <div class="tip-box">
                <p><strong>💡 Tip:</strong> Consistency is key! Try to save daily to meet your weekly targets.</p>
                <p>Have questions? Contact the group administrator.</p>
            </div>
        </div>
        
        <div class="footer">
            <p>Savings Group • Automated Weekly Update</p>
            <p>© {format(Sys.Date(), "%Y")}</p>
        </div>
    </div>
</body>
</html>
')
  
  return(html_content)
}

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

# Function to get member-specific data
get_member_data <- function(member_name, member_daily_long) {
  member_data <- member_daily_long %>% 
    filter(Name == member_name) %>% 
    head(366) %>%
    mutate(
      date = as.Date(date),
      day_in_month = day(date),
      target = day_in_month * 15,
      week_start = floor_date(date, "week", week_start = 1),
      week_end   = week_start + days(6)
    ) %>%
    arrange(date)
  
  return(member_data)
}

# Main execution function
run_friday_email <- function(send_emails = FALSE) {
  tryCatch({
    cat("Starting Friday email script...\n")
    cat("Mode:", ifelse(send_emails, "SENDING EMAILS", "TEST/DRY RUN"), "\n")
    
    # Create payout dates (in order)
    payout_dates <- as.Date(
      c(
        "2026-02-08", "2026-02-15", "2026-02-22",
        "2026-03-01", "2026-03-08", "2026-03-15",
        "2026-03-22", "2026-03-29",
        "2026-04-05", "2026-04-12"
      )
    )
    
    # Create payout dataframe
    payout_df <- members_info %>%
      arrange(order_number) %>%
      mutate(Payout_Date = payout_dates[order_number])
    
    # -------- READ & PREPARE DAILY DATA --------
    cat("Authenticating with Google Sheets...\n")
    
    # Check for Google credentials file
    if (file.exists("gs.json")) {
      cat("Using gs.json for authentication\n")
      gs4_auth(path = "gs.json")
    } else if (file.exists("myrstuff-fe49a7146f1b.json")) {
      cat("Using myrstuff-fe49a7146f1b.json for authentication\n")
      gs4_auth(path = "myrstuff-fe49a7146f1b.json")
    } else {
      cat("No Google credentials found. Using default authentication\n")
      gs4_auth()
    }
    
    sheet_id <- "1qidIxYD2DAOIZ64ONtbphsJOXdPDXnxVnP1kcJs_Qx0"
    
    daily_dataz <- read_sheet(sheet_id, sheet = "Member Contributions") 
    daily_dataz <- daily_dataz %>%
      dplyr::filter(!Name %in% c("Total Contributions", "Banked"))
    
    member_daily_long <- daily_dataz %>%
      filter(!Name %in% c("Total Contributions", "Banked")) %>%
      pivot_longer(
        cols = matches("^\\d{1,2}/\\d{1,2}/\\d{4}$"),
        names_to = "date",
        values_to = "actual"
      ) %>%
      mutate(
        date   = dmy(date),
        saved  = !is.na(actual),
        status = if_else(saved, "Saved", "Not Saved"),
        actual = replace_na(actual, 0)
      ) %>%
      arrange(Name, date)
    
    # Get today's date
    today <- Sys.Date()
    selected_month <- month(today)
    current_year   <- year(today)
    current_week_start <- floor_date(today, "week", week_start = 1)
    
    # Loop through each member and send email
    for(i in 1:nrow(members_info)) {
      member <- members_info[i, ]
      
      cat(sprintf("\nProcessing %s (%s)...\n", member$display_name, member$email))
      
      # Get member-specific data
      member_data <- get_member_data(member$sheet_name, member_daily_long)
      
      if(nrow(member_data) == 0) {
        cat(sprintf("  No data found for %s\n", member$sheet_name))
        next
      }
      
      # Filter data for current month/year
      dfz <- member_data %>%
        filter(
          year(date) == current_year,
          month(date) == selected_month
        )
      
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
          End_Date   = format(week_end, "%d %b"),
          Week_Status = case_when(
            week_start < current_week_start ~ "past",
            week_start == current_week_start ~ "current",
            week_start > current_week_start ~ "future"
          )
        )
      
      # Create gt table
      weekly_gt <- weekly_data %>%
        select(
          Week,
          Start_Date,
          End_Date,
          Target,
          Actual,
          Week_Status
        ) %>%
        gt() %>%
        cols_label(
          Week = "Week #",
          Start_Date = "Start Date",
          End_Date = "End Date",
          Target = "Weekly Target",
          Actual = "Actual Savings",
          Week_Status = "Status"
        ) %>%
        tab_style(
          style = list(
            cell_fill(color = "#d4edda"),
            cell_text(weight = "bold")
          ),
          locations = cells_body(rows = Week_Status == "current")
        ) %>%
        tab_style(
          style = cell_fill(color = "#f8d7da"),
          locations = cells_body(rows = Week_Status == "past")
        ) %>%
        tab_style(
          style = cell_fill(color = "#f2f2f2"),
          locations = cells_body(rows = Week_Status == "future")
        ) %>%
        fmt_currency(
          columns = c(Target, Actual),
          currency = "KES",
          decimals = 0
        ) %>%
        cols_hide(Week_Status) %>%
        cols_align(
          align = "center",
          columns = Week
        ) %>%
        opt_table_outline()
      
      # Get current week data
      current_week_data <- member_data %>%
        filter(date >= floor_date(today, unit = "week") &
                 date <= ceiling_date(today, unit = "week") - days(1))
      
      # Calculate totals
      current_week_total <- sum(current_week_data$actual, na.rm = TRUE)
      month_total <- sum(dfz$actual, na.rm = TRUE)
      
      # Get payout information
      next_payout <- payout_df %>%
        filter(Payout_Date >= today) %>%
        slice(1)
      
      payout_info <- list(
        member = next_payout$display_name,
        date = next_payout$Payout_Date,
        days_until = as.numeric(next_payout$Payout_Date - today)
      )
      
      # Create email body
      cat("  Creating email content...\n")
      email_body <- create_email_body(
        member_name = member$display_name,
        weekly_table = weekly_gt,
        payout_info = payout_info,
        current_week_total = current_week_total,
        month_total = month_total,
        current_date = today
      )
      
      # Create email message
      email_msg <- compose_email(
        body = html(email_body)
      )
      
      # Update the email sending section - REPLACE THIS PART:

if (send_emails) {
  # Set up credentials for SMTP from environment variables
  cat(sprintf("  Setting up email credentials for %s...\n", member$email))
  
  # Create credentials ONCE and reuse them
  if (!exists("my_email_creds")) {
    my_email_creds <- creds_envvar(
      user = Sys.getenv('MY_GMAIL_ACCOUNT'),
      pass_envvar = 'SMTP_PASSWORD',
      provider = 'gmail'
    )
  }
  
  # Send email
  cat(sprintf("  Sending email to %s...\n", member$email))
  
  # Try to send with error handling AND memory cleanup
  tryCatch({
    # Force garbage collection before sending
    gc()
    
    smtp_send(
      email = email_msg,
      from = Sys.getenv("MY_GMAIL_ACCOUNT"),
      to = member$email,
      subject = paste("💰 Weekly Savings Update - Week", isoweek(today), "|", format(today, "%B %d, %Y")),
      credentials = my_email_creds
    )
    cat(sprintf("  ✅ Email sent to %s\n", member$display_name))
  }, error = function(e) {
    cat(sprintf("  ❌ Failed to send email to %s: %s\n", member$display_name, e$message))
  })
}
      
      # Add a small delay between emails to avoid rate limiting
      if(i < nrow(members_info) && send_emails) {
        Sys.sleep(2)
      }
    }
    
    if (send_emails) {
      cat("\n✅ All Friday emails sent successfully!\n")
    } else {
      cat("\n📊 DRY RUN COMPLETE. No emails were sent.\n")
      cat("To send emails, run with --send flag\n")
    }
    
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Error in Friday email script:\n")
    cat(e$message, "\n")
    cat("Traceback:\n")
    print(traceback())
    return(FALSE)
  })
}

# Run the function with command line argument
run_friday_email(send_emails = send_emails)


