# friday_savings_email.R
# Weekly savings email script - Runs on Fridays at 7 PM Kenya time (16:00 UTC)

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
send_emails <- "--send" %in% args

cat("Send emails mode:", send_emails, "\n")

# Set memory and timeout options upfront to prevent crashes
options(timeout = 600)  # 10 minute timeout
options(warn = 1)  # Show warnings immediately

# Load required packages with error handling
cat("Loading required packages...\n")
required_packages <- c(
  "dplyr", "lubridate", "tidyr", "googlesheets4", 
  "gargle", "blastula", "glue", "gt"
)

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("ERROR: Package", pkg, "is not installed\n")
    quit(status = 1)
  }
  cat("  Loaded:", pkg, "\n")
}

# Helper function to format currency
format_kes <- function(x) {
  if (is.numeric(x)) {
    paste0("KES ", format(round(as.numeric(x), 2), big.mark = ",", nsmall = 2))
  } else {
    paste0("KES ", x)
  }
}

# (Keep all your existing functions: create_email_body, members_info, get_member_data)
# ... [PASTE ALL YOUR EXISTING FUNCTIONS HERE] ...

# Main execution function with FIXED email sending
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
    
    # Loop through each member
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
      
      if (send_emails) {
        # Create email message
        email_msg <- blastula::compose_email(
          body = blastula::html(email_body)
        )
        
        cat(sprintf("  Sending email to %s...\n", member$email))
        
        # SAFE EMAIL SENDING FUNCTION - FIXED VERSION
        safe_send_email <- function() {
          tryCatch({
            # Force garbage collection to prevent memory issues
            gc()
            
            # Create fresh credentials for each email (prevents segfault)
            my_email_creds <- blastula::creds_envvar(
              user = Sys.getenv('MY_GMAIL_ACCOUNT'),
              pass_envvar = 'SMTP_PASSWORD',
              provider = 'gmail'
            )
            
            # Send email with minimal options
            blastula::smtp_send(
              email = email_msg,
              from = Sys.getenv("MY_GMAIL_ACCOUNT"),
              to = member$email,
              subject = paste("💰 Weekly Savings Update - Week", 
                            isoweek(today), "|", format(today, "%B %d, %Y")),
              credentials = my_email_creds
            )
            
            return(TRUE)
          }, error = function(e) {
            cat(sprintf("    Error: %s\n", e$message))
            return(FALSE)
          })
        }
        
        # Try to send with retries
        max_retries <- 3
        success <- FALSE
        
        for (retry in 1:max_retries) {
          if (retry > 1) {
            cat(sprintf("    Retry %d of %d...\n", retry - 1, max_retries - 1))
            Sys.sleep(5)  # Wait before retry
          }
          
          success <- safe_send_email()
          if (success) {
            cat(sprintf("  ✅ Email sent to %s\n", member$display_name))
            break
          }
        }
        
        if (!success) {
          cat(sprintf("  ❌ Failed to send email to %s after %d attempts\n", 
                     member$display_name, max_retries))
        }
        
      } else {
        # Dry run
        cat(sprintf("  📧 [DRY RUN] Email would be sent to: %s\n", member$email))
        cat(sprintf("  Subject: Weekly Savings Update - Week %s | %s\n", 
                    isoweek(today), format(today, "%B %d, %Y")))
        cat(sprintf("  This week's total: %s\n", format_kes(current_week_total)))
        cat(sprintf("  Month total: %s\n", format_kes(month_total)))
      }
      
      # Add delay between emails to prevent rate limiting
      if(send_emails && i < nrow(members_info)) {
        Sys.sleep(3)
      }
    }
    
    if (send_emails) {
      cat("\n✅ Email sending process completed!\n")
    } else {
      cat("\n📊 DRY RUN COMPLETE. No emails were sent.\n")
      cat("To send emails, run with --send flag\n")
    }
    
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Error in Friday email script:\n")
    cat("Message:", e$message, "\n")
    cat("Call stack:\n")
    print(sys.calls())
    return(FALSE)
  })
}

# Run the function
run_friday_email(send_emails = send_emails)
