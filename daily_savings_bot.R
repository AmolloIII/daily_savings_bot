# =========================
# DAILY / WEEKLY / MONTHLY TELEGRAM BOT
# =========================

library(googlesheets4)
library(dplyr)
library(lubridate)
library(ggplot2)
library(httr)
library(stringr)
library(httr)
library(jsonlite)
library(tidyr)
library(scales)
library(patchwork)
library(telegram.bot)


text <- "Are you ready to enhance your understanding of money and its impact on your life?

This quotes collection provides nuanced sparks of wisdom about saving, investing, and cultivating a positive financial mindset. Whether you’re looking to reach monetary success with financial goal setting or seeking inspiration for simpler living, these quotes will provide valuable insights to guide you.

Saving Philosophy
“If you’re saving, you’re succeeding.” – Steve Burkholder
“A penny saved is a penny earned.” – Benjamin Franklin
“Try to save something while your salary is small; it’s impossible to save after you begin to earn more.” – Jack Benny
“More people should learn to tell their dollars where to go instead of asking them where they went.” – Roger Babson
“The bitterness of poor quality remains long after the sweetness of low price is forgotten.” – Ben Franklin
“Too many people spend money they haven’t earned to buy things they don’t want to impress people they don’t like.” – Will Rogers
“Money doesn’t buy elegance. You can take an inexpensive sheath, add a pretty scarf, gray shoes and a wonderful bag, and it will always be elegant.” – Carolina Herrera
“Frugality includes all the other virtues.” – Cicero
“If you think nobody cares if you’re alive, try missing a couple of car payments.” – Earl Wilson
Saving Strategies
“Do not save what is left after spending; instead spend what is left after saving.” – Warren Buffett
“Saving must become a priority, not just a thought. Pay yourself first.” – Dave Ramsey
“Keep your eyes on the price when spending; don’t regret later to find that you have nothing for tomorrow.” – Auliq Ice
“He who buys what he does not need steals from himself.” – Swedish Proverb
“Enough is better than too much.” – Dutch Proverb
“The habit of saving is itself an education; it fosters every virtue, teaches self-denial, cultivates the sense of order, trains to forethought, and so broadens the mind.” – T.T. Munger
“Don’t tell me where your priorities are. Show me where you spend your money and I’ll tell you what they are.” – James W. Frick
“Look everywhere you can to cut a little bit from your expenses. It will all add up to a meaningful sum.” – Suze Orman
“Never spend your money before you have it.” – Thomas Jefferson
“A budget is telling your money where to go instead of wondering where it went.” – John C. Maxwell
Financial Mindset
“Many folks think they aren’t good at earning money, when what they don’t know is how to use it.” – Frank A. Clark
“The way to stop financial joyriding is to arrest the chauffeur, not the automobile.” – Woodrow Wilson
“Every time you borrow money, you’re robbing your future self.” – Nathan W. Morris
“Money is only a tool. It will take you wherever you wish, but it will not replace you as the driver.” – Ayn Rand
“Wealth consists not in having great possessions, but in having few wants.” – Epictetus
“If we command our wealth, we shall be rich and free. If our wealth commands us, we are poor indeed.” – Edmund Burke
“The cost of a thing is the amount of what I call life which is required to be exchanged for it, immediately or in the long run.” – Henry David Thoreau
“If you wish to get rich, save what you get. A fool can earn money; but it takes a wise man to save and dispose of it to his own advantage.” – Brigham Young
“A penny here and a dollar there, placed at interest, goes on accumulating, and in this way, the desired result is attained. It requires some training, perhaps, to accomplish this economy, but when once used to it, you will find there is more satisfaction in rational saving than in irrational spending.” – P.T. Barnum
Bonus: How to Make Financial Wellness Your Reality

Brent Hines shares his personal journey from financial success to crisis, emphasizing the importance of mindset and behavior in achieving financial wellness.
In this video, you’ll learn how to live wholeheartedly so you can hit your financial goals!

In the video, Brent Hines teaches that true financial wellness starts with aligning your financial habits with your overall well-being. Here’s how you can take actionable steps:

Address self-limiting money beliefs. Identify and challenge thoughts like “I’ll never get ahead” or “Money is the root of all problems.” Reframe these beliefs to create a healthier mindset about earning and managing money.
Focus on behaviors over financial literacy alone. Knowledge is important, but consistent action drives results. Set small, achievable goals, like saving a percentage of each paycheck or tracking your daily expenses, to build positive financial habits.
Prioritize mindful budgeting. Create a budget that balances essentials, savings, and small personal rewards. This ensures you meet your obligations without feeling deprived, reducing stress and encouraging consistency.
Foster open conversations about money. Break the taboo by discussing financial goals and challenges with trusted individuals. Sharing insights can provide support, accountability, and new perspectives.
Integrate financial habits with overall wellness. Reduce financial stress by aligning spending with values and prioritizing emotional and physical health. For example, consider investing in experiences that bring joy or practicing mindfulness to ease money-related anxiety.
By taking a holistic approach, you can create a healthier relationship with money while improving your overall quality of life.

Making Use of Time
“Time well-spent results in more money to spend, more money to save, and more time to vacation.” – Zig Ziglar
“My favorite things in life don’t cost any money. It’s really clear that the most precious resource we all have is time.” – Steve Jobs
“Everyday is a bank account, and time is our currency. No one is rich, no one is poor, we’ve got 24 hours each.” – Christopher Rice
“Time is money. Wasted time means wasted money means trouble.” –Shirley Temple
“Opportunity is missed by most people because it is dressed in overalls and looks like work.” – Thomas Edison
“Time is more value than money. You can get more money, but you cannot get more time.” – Jim Rohn
“Many people take no care of their money till they come nearly to the end of it, and others do just the same with their time.” – Johann Wolfgang von Goethe
“Time is free, but it’s priceless. You can’t own it, but you can use it. You can’t keep it, but you can spend it. Once you’ve lost it you can never get it back.” – Harvey Mackay
“The trouble is, you think you have time.” – Buddha
“There’s only one thing more precious than our time and that’s who we spend it on.” – Leo Christopher
“The greatest gift you can give someone is your time because when you give your time, you are giving a portion of your life that you will never get back.” – Unknown
“You will never find time for anything. If you want time, you must make it.” – Charles Brixton
“Time is precious. Make sure you spend it with the right people.” – Unknown
“We all make time for what we feel is important in our lives.” – Unknown
“The two most powerful warriors are patience and time.” – Leo Tolstoy
“Time has a wonderful way of showing us what really matters.” – Unknown
“Always make time for things that make you feel happy to be alive.” – Unknown
Investing
“Investing should be more like watching paint dry or watching grass grow. If you want excitement, take $800 and go to Las Vegas.” – Paul Samuelson
“Small amounts saved daily add up to huge investments in the end.” – Margo Vader
“It’s not how much money you make, but how much money you keep, how hard it works for you, and how many generations you keep it for.” – Robert Kiyosaki
“You don’t have to see the whole staircase; just take the first step.” – Martin Luther King, Jr.
“By definition, saving for anything requires us to not get things now so that we can get bigger ones later.” – Jean Chatzky
“The rich invest in time, the poor invest in money.” – Warren Buffett
“Everyone wants a piece of land. It’s the only sure investment, it can never depreciate like a car or a washing machine.” – Russel Sage
“In any investment, you expect to have fun and make money.” – Michael Jordan
“An investor without investment objectives is like a traveler without a destination.” – Unknown
“Sometimes your best investments are the ones you don’t make.” – Donald Trump
“Invest in yourself, you can afford it, trust me.” – Rashon Carraway
“The price of a commodity will never go to zero. When you invest in commodities futures, you are not buying a piece of paper that says you own an intangible of a company that can go bankrupt.” – Jim Rogers
“Wealthy people invest first and spend what’s left and broke people spend first and invest what’s left.” – Unknown
“Earn as much as you can, save as much as you can, invest as much as you can, give as much as you can.” – John Wesley
“90% of all millionaires become so through owning real estate.” – Andrew Carnegie
“Goodness is the only investment that never fails.” – Henry David Thoreau
“If you buy things you do not need, soon you will have to sell things you need.” – Warren Buffet
“Wise spending is part of wise investing. And it’s never too late to start.” – Rhonda Katz
“The wisest rule in investment is: when others are selling, buy. When others are buying, sell.” – Jonathan Sacks
“If you do not know how to care for money, money will stay away from you.” – Robert T. Kiyosaki

Investment Risk
“The biggest risk of all is not taking one.” – Mellody Hobson
“Compound interest is the eighth wonder of the world. He who understands it, earns it. He who doesn’t, pays it.” – Albert Einstein
“I suppose my formula might be: Dream, diversify and never miss an angle.” – Walt Disney
“With a good perspective on history, we can have a better understanding of the past and present, and thus a clear vision of the future.” – Carlos Slim Helu
“Divide your portion to seven, or even to eight, for you do not know what misfortune may occur on the earth.” – King Solomon
“An investment in knowledge pays the best interest.” – Benjamin Franklin
“The stock market is a device for transferring money from the impatient to the patient.” – Warren Buffett
“Opportunity is missed by most people because it is dressed in overalls and looks like work.” – Thomas Edison
“The only limit to our realization of tomorrow will be our doubts of today.” – Franklin D. Roosevelt
“The circulation of confidence is better than the circulation of money.” – James Madison

Financial Success
“Money is usually attracted, not pursued.” – Jim Rohn
“There is no monopoly on becoming a millionaire. If you’re jealous of those with more money, don’t just sit there and complain—do something to make more money yourself.” – Gina Rinehart
“Financial fitness is not a pipe dream or a state of mind. It’s a reality if you are willing to pursue it and embrace it.” – Will Robinson
“At least eighty percent of millionaires are self-made. That is, they started with nothing but ambition and energy, the same way most of us start.” – Brian Tracy
“Money is multiplied in practical value depending on the number of W’s you control in your life: what you do, when you do it, where you do it, and with whom you do it.” – Tim Ferriss
“I’m a great believer in luck, and I find the harder I work the more I have of it.” – Thomas Jefferson
“The key factor that will determine your financial future is not the economy; the key factor is your philosophy.” – Jim Rohn
“Academic qualifications are important and so is financial education. They’re both important and schools are forgetting one of them.” – Robert Kiyosaki
“I believe that through knowledge and discipline, financial peace is possible for all of us.” – Dave Ramsey
“It all comes down to this: if your subconscious ‘financial blueprint’ is not ‘set’ for success, nothing you learn, nothing you know, and nothing you do will make much of a difference.” – T. Harv Eker
“The only way you will ever permanently take control of your financial life is to dig deep and fix the root problem.” – Suze Orman
“Money is better than poverty, if only for financial reasons.” – Woody Allen
“Before you can become a millionaire, you must learn to think like one. You must learn how to motivate yourself to counter fear with courage. Making critical decisions about your career, business, investments and other resources conjures up fear, fear that is part of the process of becoming a financial success.” – Thomas J. Stanley
“You can be a victim or you can be rich, but you can’t be both. Listen up! Every time, and I mean every time, you blame, justify, or complain, you are slitting your financial throat.” – T. Harv Eker
“A big part of financial freedom is having your heart and mind free from worry about the what-ifs of life.” – Suze Orman
Simpler Living
“Financial peace isn’t the acquisition of stuff. It’s learning to live on less than you make, so you can give money back and have money to invest. You can’t win until you do this.” – Dave Ramsey
“It is not the man who has too little, but the man who craves more, that is poor.” – Seneca
“Empty pockets never held anyone back. Only empty heads and empty hearts can do that.” – Norman Vincent Peale
“You must gain control over your money or the lack of it will forever control you.” – Dave Ramsey
“Wealth, after all, is a relative thing since he that has little and wants less is richer than he that has much and wants more.” – Charles Caleb Colton
“Money never made a man happy yet, nor will it. The more a man has, the more he wants. Instead of filling a vacuum, it makes one.” – Benjamin Franklin
“It’s good to have money and the things that money can buy, but it’s good, too, to check up once in a while and make sure that you haven’t lost the things that money can’t buy.” – George Lorimer
“Know what you own, and know why you own it.” – Peter Lynch
“I’m not that lazy, but I don’t need that much money. I lead a fairly simple life.” – Karl Pilkington
“Dogs have no money. Isn’t that amazing? They’re broke their entire lives. But they get through. You know why dogs have no money? … No pockets.” – Jerry Seinfeld

Giving
“Money may not buy happiness, but it can damn well give it!” – Freddie Mercury
“Success is not just making money. Success is happiness. Success is fulfillment; it’s the ability to give.” – Adam Neumann
“Let us not be satisfied with just giving money. Money is not enough, money can be got, but they need your hearts to love them. So, spread your love everywhere you go.” – Mother Teresa
“Making money is a happiness. And that’s a great incentive. Making other people happy is a super-happiness.” – Muhammad Yunus
“For me, money is not my definition of success. Inspiring people is a definition of success.” – Kanye West
“Greed is not a financial issue. It’s a heart issue.” – Andy Stanley
“The trick is to stop thinking of it as ‘your’ money.” – IRS auditor
“We make a living by what we get, but we make a life by what we give.” – Winston Churchill
“No one has ever become poor by giving.” – Anne Frank
“Giving is not just about making a donation. It is about making a difference.” – Kathy Calvin
“Life is a boomerang. What you give, you get.” – Unknown
“We rise by lifting others.” – Robert Ingersoll
“Only by giving are you able to receive more than you already have.” – Jim Rohn
“Do come and good will come to you.” – Unknown
“Giving opens the way for receiving.” – Florence Scovel Shinn
“When you learn, teach. When you get, give.” – Maya Angelou
“It’s not how much we give, but how much love we put into giving.” – Mother Teresa

Starting a New Career
“Formal education will make you a living; self-education will make you a fortune.” – Jim Rohn
“What we really want to do is what we are really meant to do. When we do what we are meant to do, money comes to us, doors open for us, we feel useful, and the work we do feels like play to us.” – Julia Cameron
“If you don’t value your time, neither will others. Stop giving away your time and talents. Value what you know and start charging for it.” – Kim Garst
“Fortune sides with him who dares.” – Virgil
“Never confuse the size of your paycheck with the size of your talent.” – Marlon Brando
“Do what you love and the money will follow.” – Marsha Sinetar
“Find out what you like doing best, and get someone to pay you for doing it.” – Katharine Whitehorn
“The most common way people give up their power is by thinking they don’t have any.” – Alice Walker
“If you don’t feel it, flee from it. Go where you are celebrated, not merely tolerated.” – Paul F. Davis
“It does not matter how slowly you go as long as you do not stop.” – Confucius
“If opportunity doesn’t knock, build a door.” – Milton Berle
“Nothing will work unless you do.” – Maya Angelou
“If you’re going through hell, keep going.” – Winston Churchill
“The future depends on what you do today.” – Mahatma Gandhi
“The only way to do great work is to love what you do. If you haven’t found it yet, keep looking. Don’t settle.” – Steve Jobs
“Start by doing what is necessary, then do what is possible, and suddenly you are doing the impossible.” – Francis of Assisi

Gratitude
“Live like you’ll die tomorrow, work like you don’t need the money, and dance like nobody’s watching.” – Bob Fosse
“I’m not motivated by money or power or fame. In the end, it doesn’t bring much happiness. The only thing that is driving me is self-satisfaction, self-validation.” – William Clay Ford, Jr.
“A treasure is to be valued for its own sake and not for what it will buy.” – Graham Greene
“There are people who have money and people who are rich.” – Coco Chanel
“A wise person should have money in their head, but not in their heart.” – Jonathan Swift
“I don’t want to make money. I just want to be wonderful.” – Marilyn Monroe
“All the money in the world can’t buy you back good health.” – Reba McEntire
“Friends and good manners will carry you where money won’t go.” – Margaret Walker
“For I don’t care too much for money, for money can’t buy me love.” – The Beatles
“Wealth is not his that has it, but his that enjoys it.” – Benjamin Franklin
“Happiness is not in the mere possession of money; it lies in the joy of achievement, in the thrill of creative effort.” – Franklin D. Roosevelt
“The real measure of your wealth is how much you’d be worth if you lost all your money.” – Unknown
“Too many people measure how successful they are by how much money they make or the people that they associate with. In my opinion, true success should be measured by how happy you are.” – Richard Branson
“Money is a terrible master but an excellent servant.” – P.T. Barnum
“The stock market is filled with individuals who know the price of everything, but the value of nothing.” – Philip Fisher
“Not everything that can be counted counts, and not everything that counts can be counted.” – Albert Einstein
“There is a gigantic difference between earning a great deal of money and being rich.” – Marlene Dietrich"

# Split into lines
lines <- str_split(text, "\n")[[1]] %>%
  str_trim() %>%
  .[. != ""]  # remove empty lines

# Filter lines that have quotes in “ ” followed by – author
quote_lines <- lines[str_detect(lines, '“.+”\\s*–\\s*.+')]

# Extract quote and author
quotes_df <- tibble(raw = quote_lines) %>%
  mutate(
    quote = str_extract(raw, '“.+?”'),       # extract the text within quotation marks
    author = str_extract(raw, '(?<=–\\s).*') # extract text after "– "
  ) %>%
  select(quote, author)


# -------------------------
# RANDOM QUOTE
# -------------------------
daily_quote <- tryCatch({
  sample_n(quotes_df, 1) %>%
    mutate(full = paste0(quote, " – ", author)) %>%
    pull(full)
}, error = function(e) { "Keep going! Every shilling counts." })




# -------------------------
# TELEGRAM CONFIG
# -------------------------
BOT_TOKEN <- Sys.getenv("BOT_TOKEN")
CHAT_ID <- Sys.getenv("CHAT_ID")

send_telegram_message <- function(bot_token, chat_id, text, parse_mode = NULL) {
  # URL-encode the text
  text_enc <- URLencode(text, reserved = TRUE)
  
  url <- paste0(
    "https://api.telegram.org/bot", BOT_TOKEN,
    "/sendMessage?chat_id=", CHAT_ID,
    "&text=", text_enc
  )
  
  # Add parse_mode if provided
  if (!is.null(parse_mode)) {
    url <- paste0(url, "&parse_mode=", parse_mode)
  }
  
  res <- httr::GET(url)
  httr::stop_for_status(res)
  return(res)
}


# -------------------------
# GOOGLE SHEETS AUTH
# -------------------------

#if (file.exists("GSHEET_JSON")) {
#  gs4_auth(path = "GSHEET_JSON")
#  gs_connected <- TRUE
#} else {
#  gs_connected <- FALSE
#  warning("⚠️ Google Sheets not connected. Data will not be saved.")
#}

GSHEET_JSON_B64 <- Sys.getenv("GSHEET_JSON_B64")
YOUR_GOOGLE_SHEET_URL <- Sys.getenv("YOUR_GOOGLE_SHEET_URL")
# Validate environment variables
if (YOUR_GOOGLE_SHEET_URL == "" || GSHEET_JSON_B64 == "") {
  stop("❌ YOUR_GOOGLE_SHEET_URL or GSHEET_JSON_B64 not set in environment")
}

# -------------------------------
# 3. Decode Google JSON
# -------------------------------
writeLines(rawToChar(base64enc::base64decode(GSHEET_JSON_B64)), "gs.json")
Sys.chmod("gs.json", mode = "600")

# -------------------------------
# 4. Authenticate Google Sheets
# -------------------------------
gs4_auth(
  path = "gs.json",
  scopes = "https://www.googleapis.com/auth/spreadsheets.readonly",
  cache = FALSE
)
message("✅ Google Sheets authenticated")
# Define Google Sheet ID (replace with your actual sheet ID)
sheet_id <- "SHEET_ID"
# -------------------------
# FIX ACTUALS (15/day increment)
# -------------------------


savings_data <- read_sheet(YOUR_GOOGLE_SHEET_URL) %>% mutate(date = as.Date(date), # ensure date format
                                                             actual = as.numeric(actual),
                                                             day_of_month = day(date)) %>% arrange(date)


savings_data <- savings_data %>%
  mutate(correct_actual = cumsum(rep(15, n())))


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

# -------------------------
# DAILY METRICS
# -------------------------
today_row <- savings_data %>% filter(date == today)
leoste <- today_row$correct_actual

cumulative_saved <- sum(savings_data$correct_actual[savings_data$status == "Saved" & savings_data$date <= today])
total_missed <- sum(savings_data$correct_actual[savings_data$status == "Missed" & savings_data$date <= today])
yearly_target <- 86070
percentage_target <- round(cumulative_saved / yearly_target * 100, 1)

# -------------------------
# LAST TWO DAYS MISSED
# -------------------------
########## Missed savings check (all past days) ##########

today <- Sys.Date()

missed_days <- savings_data %>%
  filter(
    date < today,
    status == "Missed"
  ) %>%
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
  
  
  send_telegram_message(BOT_TOKEN, CHAT_ID, msg)
}



########## Weekly summary (every Sunday) ##########

week_start <- floor_date(today, unit = "week") # Monday
week_end <- ceiling_date(today, unit = "week") - 1 # Sunday

weekly_data <- savings_data %>%
  filter(date >= week_start & date <= week_end)



########## Monthly summary (last day of month) ##########

month_start <- floor_date(today, "month")
month_end <- ceiling_date(today, "month") - 1

monthly_data <- savings_data %>%
  filter(date >= month_start & date <= month_end)


# -------------------------
# DAILY MESSAGE
# -------------------------

########## Daily detailed message at 7:00pm ##########

today_data <- savings_data %>% filter(date == today)
cumulative_saved <- sum(savings_data$actual[savings_data$date <= today], na.rm = TRUE)
percentage_target <- round(cumulative_saved / 86070 * 100, 1)
leoste <- savings_data %>%
  filter(date == Sys.Date()) %>%
  pull(actual)
# Initialize
corrected_savings <- savings_data %>%
  arrange(date) %>%
  mutate(correct_actual = 0)  # placeholder

# Loop through each row to generate the correct sequence
for(i in 1:nrow(corrected_savings)) {
  if(i == 1){
    corrected_savings$correct_actual[i] <- 15  # base value
  } else {
    corrected_savings$correct_actual[i] <- corrected_savings$correct_actual[i-1] + 15
  }
}

# Filter missed rows and sum their corrected actuals
total_missed <- corrected_savings %>%
  filter(status == "Missed") %>%
  summarise(total_missed = sum(correct_actual)) %>%
  pull(total_missed)






# -------------------------
# WEEKLY MESSAGE (Sundays)
# -------------------------
if (wday(today) == 1) { # Sunday
  total_saved <- sum(weekly_data$actual, na.rm = TRUE)
  days_saved <- sum(weekly_data$status == "Saved", na.rm = TRUE)
  
  # Plot weekly savings
  library(ggplot2)
  weekly_chart_file <- "weekly_chart.png"
  png(weekly_chart_file, width = 800, height = 600)
  ggplot(weekly_data, aes(x = date, y = actual)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = actual), vjust = -0.5) +
    labs(title = paste0("Weekly Savings: ", week_start, " to ", week_end),
         x = "Date", y = "Amount Saved (KES)") +
    theme_minimal()
  dev.off()
  
  # Prepare caption
  daily_quote_safe <- as.character(daily_quote)
  caption <- paste0(
    "📊 Weekly Savings Summary\n",
    "Total Saved: KES ", total_saved, "\n",
    "Days Saved: ", days_saved, "/", nrow(weekly_data), "\n\n",
    daily_quote_safe
  )
  caption_safe <- URLencode(as.character(caption))
  
  # Debug print
  cat("Sending weekly chart...\n", "Caption preview:", substr(caption_safe, 1, 100), "\n")
  
  # Send photo safely
  if (file.exists(weekly_chart_file)) {
    send_telegram_photo(BOT_TOKEN, CHAT_ID, weekly_chart_file, caption_safe)
  } else {
    warning("Weekly chart file not found!")
  }
}

# -------------------------
# MONTHLY MESSAGE (Month-end)
# -------------------------
if (today == month_end) {
  total_saved <- sum(monthly_data$actual, na.rm = TRUE)
  days_saved <- sum(monthly_data$status == "Saved", na.rm = TRUE)
  
  monthly_chart_file <- "monthly_chart.png"
  png(monthly_chart_file, width = 800, height = 600)
  ggplot(monthly_data, aes(x = date, y = actual)) +
    geom_col(fill = "darkgreen") +
    geom_line(aes(y = cumsum(actual)), color = "blue", size = 1) +
    labs(title = paste0("Monthly Savings: ", month_start, " to ", month_end),
         x = "Date", y = "Amount Saved (KES)") +
    theme_minimal()
  dev.off()
  
  daily_quote_safe <- as.character(daily_quote)
  caption <- paste0(
    "📈 Monthly Savings Summary\n",
    "Total Saved: KES ", total_saved, "\n",
    "Days Saved: ", days_saved, "/", nrow(monthly_data), "\n\n",
    daily_quote_safe
  )
  caption_safe <- URLencode(as.character(caption))
  
  cat("Sending monthly chart...\n", "Caption preview:", substr(caption_safe, 1, 100), "\n")
  
  if (file.exists(monthly_chart_file)) {
    send_telegram_photo(BOT_TOKEN, CHAT_ID, monthly_chart_file, caption_safe)
  } else {
    warning("Monthly chart file not found!")
  }
}


escape_markdown_v2 <- function(text) {
  # List of characters to escape
  specials <- c("_", "*", "[", "]", "(", ")", "~", "`", ">", "#", "+", "-", "=", "|", "{", "}", ".", "!")
  
  for (s in specials) {
    text <- gsub(s, paste0("\\", s), text, fixed = TRUE)
  }
  
  # Also escape %
  text <- gsub("%", "\\%", text, fixed = TRUE)
  
  return(text)
}


todayz <- as_date(with_tz(Sys.time(), tzone = "Africa/Nairobi"))

leoste <- corrected_savings %>%
  filter(date == todayz) %>%
  .[["correct_actual"]]

# -------------------------
# DAILY MESSAGE
# -------------------------
daily_msg <- paste0(
  "*🕖 DAILY SAVINGS UPDATE*\n\n",
  "*Day:* ", day(today), " of ", month(today, label = TRUE, abbr = FALSE), "\n",
  "*Today's Target:* KES ", leoste, "\n",
  "*Cumulative Saved:* KES ", cumulative_saved, "\n",
  "*Cumulative Deficit:* KES ", total_missed, "\n",
  "*Progress:* ", percentage_target, "%\n\n",
  "💡 *Motivation:* ", as.character(daily_quote)
)

# Ensure message is character and URL-safe
daily_msg_safe <- escape_markdown_v2(daily_msg)

# Debug print
cat("Sending daily message...\n", "Preview:", substr(daily_msg_safe, 1, 100), "\n")

# Send


send_telegram_message(BOT_TOKEN, CHAT_ID, daily_msg_safe, parse_mode = "MarkdownV2")




send_message_buttons <- function(text, buttons) {
  
  body <- list(
    chat_id = CHAT_ID,
    text = text,
    parse_mode = "HTML",
    reply_markup = list(
      inline_keyboard = buttons
    )
  )
  
  httr::POST(
    paste0("https://api.telegram.org/bot", BOT_TOKEN, "/sendMessage"),
    body = body,
    encode = "json"
  )
}



dashboard_url <- "https://kampspatial.shinyapps.io/BenQ_2026_DSPC/"

buttons <- list(
  list(
    list(text = "📊 Open DSPC WebApp", url = dashboard_url)
  )
)

send_message_buttons(
  "📌 <b>Quick Actions</b>\nChoose an option below:",
  buttons
)



############################################
## ENHANCED TELEGRAM FINANCIAL REMINDER
## With exact payment matching and pending amount calculation
############################################
# -------- TELEGRAM ------------------------
SHEET_URL_Z <- Sys.getenv("SHEET_URL_Z")

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

# -------- DATA ----------------------------
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

# -------- PAYMENT MATCHING LOGIC ----------
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
  
  # First, try to match by bill_id (exact match)
  payments_by_id <- payments %>%
    group_by(bill_id) %>%
    summarise(
      total_paid = sum(amount, na.rm = TRUE),
      last_payment_date = max(paid_on, na.rm = TRUE),
      payment_method = paste(unique(method[!is.na(method)]), collapse = ", "),
      .groups = "drop"
    ) %>%
    rename(id = bill_id)
  
  # Second, try to match by name (if no id match)
  payments_by_name <- payments %>%
    group_by(name) %>%
    summarise(
      total_paid_name = sum(amount, na.rm = TRUE),
      last_payment_date_name = max(paid_on, na.rm = TRUE),
      payment_method_name = paste(unique(method[!is.na(method)]), collapse = ", "),
      .groups = "drop"
    )
  
  # Join payments to bills
  bills <- bills %>%
    left_join(payments_by_id, by = "id") %>%
    left_join(payments_by_name, by = "name") %>%
    mutate(
      # Use id-based payment if available, otherwise use name-based
      total_paid = coalesce(total_paid, total_paid_name, 0),
      last_payment_date = coalesce(last_payment_date, last_payment_date_name),
      payment_method = coalesce(payment_method, payment_method_name),
      # Remove temporary columns
      total_paid_name = NULL,
      last_payment_date_name = NULL,
      payment_method_name = NULL,
      # Calculate pending amount
      pending_amount = pmax(amount - total_paid, 0),
      # Determine payment status
      payment_status = case_when(
        total_paid >= amount * 0.99 ~ "paid",  # 99% threshold to account for rounding
        total_paid > 0 & total_paid < amount ~ "partial",
        total_paid == 0 ~ "unpaid",
        TRUE ~ "unknown"
      ),
      # Check if payment was made on time (before or on due date)
      paid_on_time = ifelse(
        !is.na(last_payment_date) & last_payment_date <= due_date,
        TRUE,
        FALSE
      )
    )
  
  # Show matching summary
  paid_count <- sum(bills$payment_status == "paid", na.rm = TRUE)
  partial_count <- sum(bills$payment_status == "partial", na.rm = TRUE)
  unpaid_count <- sum(bills$payment_status == "unpaid", na.rm = TRUE)
  
  cat("📋 Payment Matching Summary:\n")
  cat("   ✅ Paid:", paid_count, "bills\n")
  cat("   🔄 Partial:", partial_count, "bills\n")
  cat("   ❌ Unpaid:", unpaid_count, "bills\n")
  
  return(bills)
}

# -------- FORMATTING HELPERS --------------
format_currency <- function(amount) {
  if (is.na(amount) || amount == 0) return("KES 0")
  paste0("KES ", format(round(amount, 2), big.mark = ",", nsmall = 2))
}

get_urgency_icon <- function(days_left, is_paid = FALSE, is_partial = FALSE) {
  if(is_paid) return("✅")
  if(is_partial) return("🔄")
  if (is.na(days_left) || days_left < 0) return("🔴")  # Overdue
  if (days_left == 0) return("🟠")  # Today
  if (days_left <= 3) return("🟡")  # 1-3 days
  if (days_left <= 7) return("🟢")  # 4-7 days
  return("⚪")  # More than 7 days
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

# -------- MESSAGE SECTIONS -----------------
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
  # Get debts (bills with direction)
  owe_bills <- bills %>% 
    filter(direction == "owe" & pending_amount > 0) %>%
    filter(!is.na(name) & !is.na(amount))
  
  owed_bills <- bills %>% 
    filter(direction == "owed_to_me" & pending_amount > 0) %>%
    filter(!is.na(name) & !is.na(amount))
  
  section_text <- ""
  
  # You owe to others
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
  
  # Owed to you
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
    
    # Add daily required amount if not too close to deadline
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
  # Calculate totals based on pending amounts
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
  
  # Get urgent bills (due within 7 days or overdue)
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
  
  # Add urgent notice if needed
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

# -------- MAIN FUNCTION -------------------
send_financial_reminder <- function() {
  tryCatch({
    # Read all data
    cat("📊 Reading data from Google Sheets...\n")
    bills <- read_bills()
    payments <- read_payments()
    goals <- read_goals()
    
    # Match payments to bills
    bills <- match_payments_to_bills(bills, payments)
    
    # Show summary of what was found
    cat("\n📋 BILLS STATUS SUMMARY:\n")
    cat("   Total bills:", nrow(bills), "\n")
    cat("   Fully paid:", sum(bills$payment_status == "paid", na.rm = TRUE), "\n")
    cat("   Partially paid:", sum(bills$payment_status == "partial", na.rm = TRUE), "\n")
    cat("   Unpaid:", sum(bills$payment_status == "unpaid", na.rm = TRUE), "\n")
    cat("   Total pending amount:", format_currency(sum(bills$pending_amount, na.rm = TRUE)), "\n")
    
    # Build message
    message <- paste0(
      "<b>💳 FINANCIAL OVERVIEW</b>\n",
      "<i>", format(Sys.Date(), "%A, %B %d, %Y"), "</i>\n",
      strrep("─", 35), "\n\n",
      
      create_unpaid_bills_section(bills),
      create_paid_bills_summary(bills),
      create_debts_section(bills),
      create_goals_section(goals),
      create_summary_section(bills, goals),
      
      
      "<i>📅 Next update: ", format(Sys.Date() + 1, "%b %d"), "</i>"
    )
    
    # Send main message
    # cat("📤 Sending message to Telegram...\n")
    # send_message(message)
    
    # Send urgent reminders separately if any
    urgent_bills <- bills %>% 
      filter((due_in_days <= 3 & due_in_days >= 0) | due_in_days < 0) %>%
      filter(!is.na(name) & !is.na(pending_amount) & pending_amount > 0)
    
    if (nrow(urgent_bills) > 0) {
      urgent_msg <- "<b>🚨 URGENT REMINDERS</b>\n\n"
      for (i in 1:nrow(urgent_bills)) {
        bill <- urgent_bills[i, ]
        urgent_msg <- paste0(
          urgent_msg,
          "• <b>", bill$name, "</b>\n",
          "  Pending: ", format_currency(bill$pending_amount), "\n",
          "  Due: ", 
          ifelse(bill$due_in_days < 0, 
                 paste0("<b>OVERDUE by ", abs(bill$due_in_days), " days</b>"),
                 ifelse(bill$due_in_days == 0, "<b>TODAY</b>", 
                        paste("in <b>", bill$due_in_days, " days</b>"))),
          "\n"
        )
        
        if (bill$payment_status == "partial") {
          urgent_msg <- paste0(urgent_msg,
                               "  Already paid: ", format_currency(bill$total_paid), "\n")
        }
        
        urgent_msg <- paste0(urgent_msg, "\n")
      }
     # send_message(urgent_msg)
      cat("⚠️ Sent urgent reminders:", nrow(urgent_bills), "bills\n")
    }
    
    cat("✅ Financial reminder sent successfully!\n")
    
  }, error = function(e) {
    error_msg <- paste0(
      "❌ Error sending financial reminder:\n",
      "<code>", e$message, "</code>\n\n",
      "Please check your data structure and try again."
    )
    send_message(error_msg)
    cat("❌ Error:", e$message, "\n")
  })
}

# -------- DATA CONSISTENCY CHECK ----------
check_data_consistency <- function() {
  cat("🔍 Checking data consistency...\n")
  
  tryCatch({
    bills <- read_bills()
    payments <- read_payments()
    
    # Check for bills without matching payments
    bills_with_payments <- match_payments_to_bills(bills, payments)
    
    unpaid_bills <- bills_with_payments %>%
      filter(payment_status %in% c("unpaid", "partial")) %>%
      select(id, name, amount, total_paid, pending_amount, due_date)
    
    cat("\n📊 UNPAID/UNDERPAID BILLS:\n")
    if(nrow(unpaid_bills) > 0) {
      print(unpaid_bills)
    } else {
      cat("   All bills are fully paid! 🎉\n")
    }
    
    # Check for payments without matching bills
    unmatched_payments <- payments %>%
      filter(!bill_id %in% bills$id)
    
    cat("\n📊 PAYMENTS WITHOUT MATCHING BILLS:\n")
    if(nrow(unmatched_payments) > 0) {
      print(unmatched_payments)
    } else {
      cat("   All payments match a bill ID\n")
    }
    
  }, error = function(e) {
    cat("❌ Error checking consistency:", e$message, "\n")
  })
}

# -------- RUN -----------------------------
# First, check data consistency (optional)
# check_data_consistency()

# Then send the reminder
# send_financial_reminder()


if (nrow(status_changes) > 0) {

  for (i in seq_len(nrow(status_changes))) {

    d <- status_changes$date[i]
    from <- status_changes$prev_status[i]
    to <- status_changes$status[i]

    # ✅ Future → Saved
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

    # ❌ Future → Missed
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

    # 🔄 Missed → Saved (recovery!)
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


saveRDS(
  savings_data %>% select(date, status),
  STATE_FILE
)



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

# -------------------------------
# 2. Filter for Ben only
# -------------------------------
ben_data <- all_data %>% filter(member == "Ben")

current_date <- Sys.Date()
current_week <- floor_date(current_date, "week")
current_month <- floor_date(current_date, "month")
current_year <- year(current_date)

# Filter periods
week_data  <- ben_data %>% filter(date >= current_week & date <= current_date)
month_data <- ben_data %>% filter(date >= current_month & date <= current_date)
year_data  <- ben_data %>% filter(year(date) == current_year)

# -------------------------------
# 3. Function to create battery plot
# -------------------------------
create_battery_plot <- function(df, title) {
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
  
  ggplot(days_summary, aes(x = "Ben", y = perc, fill = status)) +
    geom_col(width = 0.5, color = "black", size = 1, radius = 0.05) +
    geom_text(aes(y = ypos, label = percent(perc)), color = "white", fontface = "bold") +
    geom_rect(
      data = terminals,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "black",
      inherit.aes = FALSE
    ) +
    scale_fill_manual(values = c("saved" = "#2ca02c", "missed" = "#d62728")) +
    scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.1))) +
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

# -------------------------------
# 4. Create individual plots
# -------------------------------
# Week title: "Week X of Month"
week_num <- week(current_date)
week_month <- month(current_date, label = TRUE, abbr = FALSE)
week_title <- paste0("Week ", week_num, " of ", week_month)

week_plot  <- create_battery_plot(week_data, week_title)
month_title <- paste0("Month of ", month(current_date, label = TRUE, abbr = FALSE))
month_plot <- create_battery_plot(month_data, month_title)
year_plot  <- create_battery_plot(year_data, paste0("Year ", current_year))

# -------------------------------
# 5. Combine plots side by side
# -------------------------------
combined_plot <- week_plot + month_plot + year_plot + plot_layout(ncol = 3)

# Save the combined plot
combined_file <- "Ben_savings_combined.png"
ggsave(combined_file, combined_plot, width = 15, height = 6, dpi = 300)

# -------------------------------
# 6. Send via Telegram
# -------------------------------


bot <- Bot(token = BOT_TOKEN)
bot$sendPhoto(chat_id = CHAT_ID, photo = combined_file, caption = "Ben's Savings Progress 📊")























