# =========================
# DAILY / WEEKLY / MONTHLY TELEGRAM BOT
# =========================

library(googlesheets4)
library(dplyr)
library(lubridate)
library(ggplot2)
library(httr)
library(stringr)


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
# TELEGRAM CONFIG
# -------------------------
BOT_TOKEN <- Sys.getenv("BOT_TOKEN")
CHAT_ID <- Sys.getenv("CHAT_ID")

send_telegram_message <- function(text, chat_id = CHAT_ID, bot_token = BOT_TOKEN, photo = NULL) {
  if (!is.null(photo)) {
    httr::POST(
      url = paste0("https://api.telegram.org/bot", bot_token, "/sendPhoto"),
      body = list(chat_id = chat_id, photo = httr::upload_file(photo), caption = text, parse_mode = "Markdown"),
      encode = "multipart"
    )
  } else {
    httr::POST(
      url = paste0("https://api.telegram.org/bot", bot_token, "/sendMessage"),
      body = list(chat_id = chat_id, text = text, parse_mode = "Markdown"),
      encode = "form"
    )
  }
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

# Validate environment variables
if (GSHEET_URL == "" || GSHEET_JSON_B64 == "") {
  stop("❌ GSHEET_URL or GSHEET_JSON_B64 not set in environment")
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

savings_data <- read_sheet(sheet_id) %>% mutate(date = as.Date(date)) %>% arrange(date)


savings_data <- savings_data %>%
  mutate(correct_actual = cumsum(rep(15, n())))

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
last_two_days <- savings_data %>%
  filter(date %in% (today - 1:2) & status == "Missed") %>%
  pull(date) %>%
  format("%Y-%m-%d")
missed_msg <- if(length(last_two_days) > 0) {
  paste0("⚠️ Reminder: You have missed savings for the last two days (", paste(last_two_days, collapse=", "), "). Don't break the streak!\n\n")
} else { "" }

# -------------------------
# RANDOM QUOTE
# -------------------------
daily_quote <- tryCatch({
  sample_n(quotes_df, 1) %>%
    mutate(full = paste0(quote, " – ", author)) %>%
    pull(full)
}, error = function(e) { "Keep going! Every shilling counts." })

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
  missed_msg,
  "💡 *Motivation:* ", daily_quote
)

# -------------------------
# MONTHLY CHART
# -------------------------
monthly_data <- savings_data %>%
  filter(month(date) == month(today) & year(date) == year(today))
plot_file <- tempfile(fileext = ".png")
p <- ggplot(monthly_data, aes(x = day_of_month, y = correct_actual)) +
  geom_line(color = "blue") + geom_point(color = "darkblue") +
  labs(title = paste0("Savings Progress: ", month(today, label = TRUE, abbr = FALSE)),
       x = "Day of Month", y = "Amount Saved (KES)") +
  theme_minimal()
ggsave(plot_file, p, width = 6, height = 4, dpi = 150)

send_telegram_message(daily_msg, photo = plot_file)

# -------------------------
# WEEKLY MESSAGE (Sunday)
# -------------------------
if (wday(today) == 1) {
  week_start <- today - 6
  week_data <- savings_data %>% filter(date >= week_start & date <= today)
  week_saved <- sum(week_data$correct_actual[week_data$status == "Saved"])
  days_saved <- sum(week_data$status == "Saved")
  days_missed <- sum(week_data$status == "Missed")
  
  week_msg <- paste0(
    "*📅 WEEKLY SAVINGS SUMMARY*\n\n",
    "Week: ", format(week_start, "%d %b"), " – ", format(today, "%d %b"), "\n",
    "Total Saved: KES ", week_saved, "\n",
    "Days Saved: ", days_saved, "\n",
    "Days Missed: ", days_missed, "\n",
    "Success? ", if(days_saved >= 5) "✅ Great week!" else "⚠️ Needs improvement"
  )
  
  week_plot_file <- tempfile(fileext = ".png")
  p_week <- ggplot(week_data, aes(x = date, y = correct_actual)) +
    geom_line(color = "darkgreen") + geom_point(color = "green") +
    labs(title = paste0("Weekly Savings: ", format(week_start, "%d %b"), " – ", format(today, "%d %b")),
         x = "Date", y = "Amount Saved (KES)") +
    theme_minimal()
  ggsave(week_plot_file, p_week, width = 6, height = 4, dpi = 150)
  
  send_telegram_message(week_msg, photo = week_plot_file)
}

# -------------------------
# MONTHLY MESSAGE (last day of month)
# -------------------------
if (today == ceiling_date(today, "month") - days(1)) {
  month_data <- savings_data %>% filter(month(date) == month(today) & year(date) == year(today))
  month_saved <- sum(month_data$correct_actual[month_data$status == "Saved"])
  days_saved <- sum(month_data$status == "Saved")
  days_missed <- sum(month_data$status == "Missed")
  
  month_msg <- paste0(
    "*📊 MONTHLY SAVINGS SUMMARY*\n\n",
    "Month: ", month(today, label = TRUE, abbr = FALSE), "\n",
    "Total Saved: KES ", month_saved, "\n",
    "Days Saved: ", days_saved, "\n",
    "Days Missed: ", days_missed, "\n",
    "Success? ", if(days_saved >= 20) "✅ Excellent!" else "⚠️ Could do better"
  )
  
  month_plot_file <- tempfile(fileext = ".png")
  p_month <- ggplot(month_data, aes(x = day_of_month, y = correct_actual)) +
    geom_line(color = "purple") + geom_point(color = "darkpurple") +
    labs(title = paste0("Monthly Savings: ", month(today, label = TRUE, abbr = FALSE)),
         x = "Day", y = "Amount Saved (KES)") +
    theme_minimal()
  ggsave(month_plot_file, p_month, width = 6, height = 4, dpi = 150)
  
  send_telegram_message(month_msg, photo = month_plot_file)
}




