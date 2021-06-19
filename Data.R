## install.packages("pacman") tto automatically load the packages
pacman::p_load(twitteR, dplyr, purrr, tidyr, lubridate, scales, ggplot2, tidytext, sentimentr, quanteda,
               lexicon, stringr, wordcloud2, rtweet, vader, BatchGetSymbols, data.table, gridExtra, topicmodels)
#packages list

library(twitteR) #connect to twitter
library(dplyr) #manipulate table data
library(purrr) #writing neat functions
library(tidyr) #manipulate table data
library(lubridate) #manipulate dates
library(scales) #for plotting
library(ggplot2) #for plotting
library(tidytext) #manipulating text data and natural language processing
library(sentimentr) #natural language processing
library(lexicon) #sentiment dictionaries
library(stringr) #manipulating text data
library(wordcloud2) #create wordclouds
library(rtweet)
library(vader)
library(BatchGetSymbols) #for downloading yahoo financial quotes
library(data.table)
library(gridExtra) 
library(topicmodels)
library(quanteda)

#end of packages list


#load the TwitterAPI logs
consumer_key <- "tN47xSZv2YI1OhzF9fXe7qN0R"
consumer_secret <- "QgblhGdhltdAte2iNEJ0tQJz7Ek5Vw2tJITty1LvQXP9BobGMT"
access_token <- "1370231772-hiIr4H29RZqXmBb1muEEEHgnwNiUxo9F82oQZ4c"
access_secret <- "hA898Nmof4cbdHAsRMT2pcaEB9FYYm8MKJRVymcX0XtAH"

#connecting to twitter via API
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# I can request only 3200 most recent tweets;
# This will likely change when I get approved for Twitter Academic Developer Account
obama_tweets <- userTimeline("BarackObama", n = 3200, maxID = NULL , sinceID =  NULL, includeRts = TRUE, excludeReplies = FALSE)
obama_tweets_df <- tbl_df(map_df(obama_tweets, as.data.frame))

potus44_tweets <- userTimeline("POTUS44", n = 3200, maxID = NULL , sinceID =  NULL, includeRts = TRUE, excludeReplies = FALSE)
potus44_tweets_df <- tbl_df(map_df(potus44_tweets, as.data.frame))

biden_tweets <- userTimeline("JoeBiden", n = 3200, maxID = NULL , sinceID =  NULL, includeRts = TRUE, excludeReplies = FALSE)
biden_tweets_df <- tbl_df(map_df(biden_tweets, as.data.frame))

potus46_tweets <- userTimeline("POTUS", n = 3200, maxID = NULL , sinceID =  NULL, includeRts = TRUE, excludeReplies = FALSE)
potus46_tweets_df <- tbl_df(map_df(potus46_tweets, as.data.frame))

news_tweets <- userTimeline("WSJmarkets", n = 3200, maxID = NULL , sinceID =  NULL, includeRts = TRUE, excludeReplies = FALSE)
news_tweets_df <- tbl_df(map_df(news_tweets, as.data.frame))

setwd("C:/Users/VANESCOO/Documents/aRotterdam School of Management/BM THESIS/R_code")
write.csv(biden_tweets_df, "biden_tweets.csv") #saving last 3200 tweets as of 09/02/21
write.csv(obama_tweets_df, "obama_tweets.csv") #saving last 3200 tweets as of 09/02/21
write.csv(potus44_tweets_df, "potus44_tweets.csv") #saving all 351 tweets as of 09/02/21
write.csv(potus46_tweets_df, "potus46_tweets.csv") #saving all 122 tweets as of 09/02/21
write.csv(news_tweets_df, "news_tweets.csv")
#now loading manually database of all trump tweets downloaded from The Trump Archive
#Encoding UTF-8 must be ensured!
trump_tweets_df <- data.frame(trump_tweets)

#frequency of tweets as time series
obama_tweets_df$created <- dmy_hm(obama_tweets_df$created)
ts_plot(obama_tweets_df, "weeks")
ts_plot(potus44_tweets_df, "weeks")
ts_plot(biden_tweets_df, "weeks")
ts_plot(potus46_tweets_df, "weeks")
ts_plot(news_tweets_df, "weeks")
#time format of initial trump tweets was incompatible with POSIXct
trump_tweets_df$date <- dmy_hm(trump_tweets_df$date)
ts_plot(trump_tweets_df, by = "weeks")

#analysis of Trump Tweets

#clean tweets from retweets to obtain only organic content
trump_no_df <- trump_tweets_df %>% 
  filter(trump_tweets_df$isRetweet %in% c("f"))

#filter tweets from deleted ones
trump_nodel_df <- trump_no_df %>% 
  filter(trump_no_df$isDeleted %in% c("f"))

#filter tweets from quotations
trump_nodelquo_df <- trump_nodel_df %>%
  filter(!str_detect(text, '^"')) %>%
  filter(!str_detect(text, '^“'))

#filter tweets starting with link
trump_nodql_df <- trump_nodelquo_df %>%
  filter(!str_detect(text, '^https://t.co/'))

#filter tweets with ANY links
trump_textonly_df <- trump_nodql_df %>%
  filter(!str_detect(text, 'https://t.co/'))

#delete residual &amp
trump_textonly_df <- trump_textonly_df %>%
  filter(!str_detect(text, 'amp'))

#tweets containing only 'stock and trade' name in it
trump_stock_df <- trump_nodql_df %>%
  filter(str_detect(text, 'stock|trade'))

#stock tweets filtered for RT/DEL/LNKs-------------------------------> for potential further analysis
trump_stockcln_df <- trump_stock_df %>%
  filter(!str_detect(text, 'https://t.co/')) %>%
  filter(!str_detect(text, '^"')) %>%
  filter(!str_detect(text, '^“'))
  
ts_plot(trump_textonly_df, by = "weeks")
ts_plot(trump_stockcln_df, by = "weeks")

#for finding sources of tweets
sources <- trump_textonly_df %>%
  group_by(device) %>%
  count() %>%
  arrange(-n)

trump_srcinstagram <- trump_textonly_df %>%
  filter(str_detect(device, 'Instagram'))

trump_srcandroid <- trump_textonly_df %>%
  filter(str_detect(device, 'Android'))

ts_plot(trump_srcandroid, by = "weeks")

trump_srciphone <- trump_textonly_df %>%
  filter(str_detect(device, 'iPhone'))

ts_plot(trump_srciphone, by = "weeks")

trump_srcwebclient <- trump_textonly_df %>%
  filter(str_detect(device, 'Web Client'))

ts_plot(trump_srcwebclient, by = "weeks")

#selecting the data timeframe relevant for thesis

trump_presidency_df <- trump_textonly_df[trump_textonly_df$date >= "2017-01-20" & trump_textonly_df$date <= "2021-01-08",]
trump_stockcln_df <- trump_stockcln_df[trump_stockcln_df$date >= "2017-01-20" & trump_stockcln_df$date <= "2021-01-08",]

#subsection for Hypothesis 3, Trump before the campaign (denoted by u), trump's campaign (Denoted by w) 
#and period afterwards (hypothesis 4) (Z)
u_base <- trump_textonly_df[trump_textonly_df$date >= "2017-01-20" & trump_textonly_df$date <= "2020-08-18",]
w_base <- trump_textonly_df[trump_textonly_df$date >= "2020-08-19" & trump_textonly_df$date <= "2020-11-07",]
z_base <- trump_textonly_df[trump_textonly_df$date >= "2020-11-08" & trump_textonly_df$date <= "2021-01-08",]


sources <- trump_presidency_df %>%
  group_by(device) %>%
  count() %>%
  arrange(-n)

#checking for duplicates - if trump_presidency_df = trump_presidency2_df then no duplicates found
trump_presidency2_df <- distinct(trump_presidency_df)




#--------------------------------------------------------------- keywords search

trump_words <- trump_presidency_df %>%
  unnest_tokens(word, text, token = "tweets")
# ^ returns 326688 words used in all 56571 tweets relevant for the time-period
# to return only words which convey some personalized style I need to exclude common
# words like 'this' or 'the' or 'about' view stop_words from tidytext package to
# see the whole list of excluded words
# -------------------------------------------> View(stop_words)
trump_words <- trump_words %>%
  anti_join(stop_words, by = "word")
# ^ renders 139531 words

trump_words_count <- trump_words %>%
  group_by(word) %>%
  count()

trump_words <- trump_words_count %>%
  arrange(-n)

View(trump_words)
trump_words_df <- as.data.frame(trump_words)
write.csv(trump_words_df, "trump_words.csv") # saving all words counted for manual
# review and analysis

#-----------------------------------------------end of keywords search

#ANALYSIS OF TRUMP

trump_presidency_df <- trump_presidency_df %>%
  #filter(str_detect(text, paste(c(words_trump$word),collapse = '|'))) #load manually words_trump with UTF-8
  filter(str_detect(text, 'market|stock|economy|unemployment|prices|cut|fed|jobs|highest|rates|all-time|lowest|manufacturing|tax|consumer|regulation|inflation|401k'))
  #filter(str_detect(text, 'market'))
# for alternative word list see oldcode.R



trump_presidency_vader <- vader_df(trump_presidency_df$text, incl_nt = T, neu_set = T, rm_qm = T)

trump_presidency_vader$date <- trump_presidency_df$date

trump_presidency_valid <- trump_presidency_vader[trump_presidency_vader$compound >= 0.5 | trump_presidency_vader$compound <= -0.5,]

ts_plot(trump_presidency_valid, by = "week")

#after16and weekend
#to shift twweets from outside the trading hours (16:00 onwards) I simply add 8 hours so they count
#as next day tweets (ie also having influence over next day trading results)

t_timeshift = as.POSIXlt(trump_presidency_valid$date)
t_timeshift_final = t_timeshift

for (i in seq_along(t_timeshift_final)){
  if (t_timeshift[i]$hour > 17) t_timeshift_final[i]=t_timeshift[i] + 28800
}

trump_presidency_valid$date <- t_timeshift_final

#next I want to move all saturday days by 24hrs effectivelly eliminating saturday tweets

t_satshift_final = as.POSIXlt(t_timeshift_final)
for (i in seq_along(t_timeshift_final)){
  if (weekdays(t_timeshift_final[i]) == "Saturday") t_satshift_final[i]=t_timeshift_final[i] + 115200
}

#lastly I want to move all Sunday days by 24hrs effectively making all Saturday and Sunday Tweets count as Monday Tweets

t_sunshift_final = as.POSIXlt(t_satshift_final)
for (i in seq_along(t_timeshift_final)){
  if (weekdays(t_satshift_final[i]) == "Sunday") t_sunshift_final[i]=t_satshift_final[i] + 115200
}
weekdays(t_sunshift_final[1:200]) #checkup if the process went well

trump_presidency_valid$date <- t_sunshift_final #save up

#get rid of the hours/minutes/seconds as now are irrelevant
trump_presidency_valid$date <- as.Date(as.POSIXct(trump_presidency_valid$date,format='%m/%d/%Y %H:%M:%S %p'))

#final tweets data set with compound sum and compound mean valence scores across all days observed
t_final <- trump_presidency_valid
t_final <- aggregate(trump_presidency_valid["compound"], by=trump_presidency_valid["date"], sum)
t_mean <- aggregate(trump_presidency_valid["compound"], by=trump_presidency_valid["date"], mean)
t_final$mean <- t_mean$compound
t_final$difference <- t_final$compound - t_final$mean

#------------------------------------------------------------------------------------------------end

#download SP500 , user needs to manually fill in last date!! first date is n-1 to calculate the return
market <- BatchGetSymbols(
  tickers = c("^GSPC"),
  first.date = t_final$date[1] - 1,
  last.date = Sys.Date(),
  thresh.bad.data = 0.75,
  bench.ticker = "^GSPC",
  type.return = "arit",
  freq.data = "daily",
  how.to.aggregate = "last",
  do.complete.data = FALSE,
  do.fill.missing.prices = TRUE,
  do.cache = TRUE,
  cache.folder = file.path(tempdir(), "BGS_Cache2"),
  do.parallel = FALSE,
  be.quiet = FALSE
)

market_dt <- market$df.tickers %>% setDT() %>% #convert to data table
  .[order(ticker, ref.date)] #order by ticker and date
market_dt <- market_dt %>% select(7,10)
#--------------
for (i in seq_along(t_final)) {
  t_SPX <- left_join(t_final, market_dt, by = c("date" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

t_SPX$return <- round(t_SPX$return, digits = 4)
t_SPX <- na.omit(t_SPX) #skip N/A values
    
t_SPX$correlation <- round(t_SPX$mean * t_SPX$return, digits = 6)
t_SPX$valid <- t_SPX$correlation > 0 
sum(t_SPX$correlation > 0, na.rm=TRUE)
sum(t_SPX$correlation < 0, na.rm=TRUE)
sum(t_SPX$correlation == 0, na.rm=TRUE)

#OBAMA
#
#
#
#
#
#OBAMA

#clean tweets from retweets to obtain only organic content
obama_no_df <- obama_tweets_df %>% 
  filter(obama_tweets_df$isRetweet %in% c("FALSE"))

#filter tweets from deleted ones
#no deleted tweet in dataset

#filter tweets from quotations
obama_nodelquo_df <- obama_no_df %>%
  filter(!str_detect(text, '^"')) %>%
  filter(!str_detect(text, '^“'))

#filter tweets starting with link
obama_nodql_df <- obama_nodelquo_df %>%
  filter(!str_detect(text, '^https://t.co/'))

#filter tweets with ANY links
obama_textonly_df <- obama_nodql_df %>%
  filter(!str_detect(text, 'https://t.co/'))

#delete residual &amp
obama_textonly_df <- obama_textonly_df %>%
  filter(!str_detect(text, 'amp'))

ts_plot(obama_textonly_df, by = "weeks")

#selecting the data timeframe relevant for thesis

obama_presidency_df <- obama_textonly_df[obama_textonly_df$created >= "2015-05-18" & obama_textonly_df$created <= "2017-01-20",]

#checking for duplicates - if trump_presidency_df = trump_presidency2_df then no duplicates found
obama_presidency_df <- distinct(obama_presidency_df)


#obama
#--------------------------------------------------------------- keywords search

obama_words <- obama_presidency_df %>%
  unnest_tokens(word, text, token = "tweets")
# ^ returns 326688 words used in all 56571 tweets relevant for the time-period
# to return only words which convey some personalized style I need to exclude common
# words like 'this' or 'the' or 'about' view stop_words from tidytext package to
# see the whole list of excluded words
# -------------------------------------------> View(stop_words)
obama_words <- obama_words %>%
  anti_join(stop_words, by = "word")
# ^ renders 139531 words

obama_words_count <- obama_words %>%
  group_by(word) %>%
  count()

obama_words <- obama_words_count %>%
  arrange(-n)

View(obama_words)
obama_words_df <- as.data.frame(obama_words)
write.csv(obama_words_df, "obama_words.csv") # saving all words counted for manual
# review and analysis

#-----------------------------------------------end of keywords search
  

#BIDEN
#
#
#
#
#
#

#clean tweets from retweets to obtain only organic content
biden_no_df <- biden_tweets_df %>% 
  filter(biden_tweets_df$isRetweet %in% c("FALSE"))

#filter tweets from deleted ones
#no deleted tweet in dataset

#filter tweets from quotations
biden_nodelquo_df <- biden_no_df %>%
  filter(!str_detect(text, '^"')) %>%
  filter(!str_detect(text, '^“'))

#filter tweets starting with link
biden_nodql_df <- biden_nodelquo_df %>%
  filter(!str_detect(text, '^https://t.co/'))

#filter tweets with ANY links - not applicable to biden
biden_textonly_df <- biden_nodql_df #%>%
#  filter(!str_detect(text, 'https://t.co/'))

#delete residual &amp
biden_textonly_df <- biden_textonly_df %>%
  filter(!str_detect(text, 'amp'))

ts_plot(biden_textonly_df, by = "weeks")

#selecting the data timeframe relevant for thesis

biden_presidency_df <- biden_textonly_df[biden_textonly_df$created >= "2020-08-19" & biden_textonly_df$created <= "2021-04-28",]

#Hypothesis 5 ; Period from August till results, campaign (C) and period after winning (D)
c_base <- biden_textonly_df[biden_textonly_df$created >= "2020-08-19" & biden_textonly_df$created <= "2020-11-07",]
d_base <- biden_textonly_df[biden_textonly_df$created >= "2020-11-08" & biden_textonly_df$created <= "2021-04-28",]

#checking for duplicates - if trump_presidency_df = trump_presidency2_df then no duplicates found
biden_presidency_df <- distinct(biden_presidency_df)


#biden
#--------------------------------------------------------------- keywords search

biden_words <- biden_presidency_df %>%
  unnest_tokens(word, text, token = "tweets")
# ^ returns 326688 words used in all 56571 tweets relevant for the time-period
# to return only words which convey some personalized style I need to exclude common
# words like 'this' or 'the' or 'about' view stop_words from tidytext package to
# see the whole list of excluded words
# -------------------------------------------> View(stop_words)
biden_words <- biden_words %>%
  anti_join(stop_words, by = "word")
# ^ renders 139531 words

biden_words_count <- biden_words %>%
  group_by(word) %>%
  count()

biden_words <- biden_words_count %>%
  arrange(-n)

View(biden_words)
biden_words_df <- as.data.frame(biden_words)
write.csv(biden_words_df, "biden_words.csv") # saving all words counted for manual
# review and analysis

#-----------------------------------------------end of keywords search

#POTUS44
#
#
#
#
#
#

#clean tweets from retweets to obtain only organic content
p44_no_df <- potus44_tweets_df %>% 
  filter(potus44_tweets_df$isRetweet %in% c("FALSE"))

#filter tweets from deleted ones
#no deleted tweet in dataset

#filter tweets from quotations
p44_nodelquo_df <- p44_no_df %>%
  filter(!str_detect(text, '^"')) %>%
  filter(!str_detect(text, '^“'))

#filter tweets starting with link
p44_nodql_df <- p44_nodelquo_df %>%
  filter(!str_detect(text, '^https://t.co/'))

#filter tweets with ANY links
p44_textonly_df <- p44_nodql_df #%>%
#  filter(!str_detect(text, 'https://t.co/'))

#delete residual &amp
p44_textonly_df <- p44_textonly_df %>%
  filter(!str_detect(text, 'amp'))

ts_plot(p44_textonly_df, by = "weeks")

#selecting the data timeframe relevant for thesis

p44_presidency_df <- p44_textonly_df[p44_textonly_df$created >= "2015-05-18" & p44_textonly_df$created <= "2017-01-20",]

#checking for duplicates - if trump_presidency_df = trump_presidency2_df then no duplicates found
p44_presidency_df <- distinct(p44_presidency_df)


#
#--------------------------------------------------------------- keywords search

p44_words <- p44_presidency_df %>%
  unnest_tokens(word, text, token = "tweets")
# ^ returns 326688 words used in all 56571 tweets relevant for the time-period
# to return only words which convey some personalized style I need to exclude common
# words like 'this' or 'the' or 'about' view stop_words from tidytext package to
# see the whole list of excluded words
# -------------------------------------------> View(stop_words)
p44_words <- p44_words %>%
  anti_join(stop_words, by = "word")
# ^ renders 139531 words

p44_words_count <- p44_words %>%
  group_by(word) %>%
  count()

p44_words <- p44_words_count %>%
  arrange(-n)

View(p44_words)
p44_words_df <- as.data.frame(p44_words)
write.csv(p44_words_df, "p44_words.csv") # saving all words counted for manual
# review and analysis

#-----------------------------------------------end of keywords search

#POTUS46
#BIDENPOTUS
#
#
#
#
#

#clean tweets from retweets to obtain only organic content
p46_no_df <- potus46_tweets_df %>% 
  filter(potus46_tweets_df$isRetweet %in% c("FALSE"))

#filter tweets from deleted ones
#no deleted tweet in dataset

#filter tweets from quotations
p46_nodelquo_df <- p46_no_df %>%
  filter(!str_detect(text, '^"')) %>%
  filter(!str_detect(text, '^“'))

#filter tweets starting with link
p46_nodql_df <- p46_nodelquo_df %>%
  filter(!str_detect(text, '^https://t.co/'))

#filter tweets with ANY links
p46_textonly_df <- p46_nodql_df #%>%
#  filter(!str_detect(text, 'https://t.co/'))

#delete residual &amp
p46_textonly_df <- p46_textonly_df %>%
  filter(!str_detect(text, 'amp'))

ts_plot(p46_textonly_df, by = "weeks")

#selecting the data timeframe relevant for thesis

p46_presidency_df <- p46_textonly_df[p46_textonly_df$created >= "2020-08-19" & p46_textonly_df$created <= "2021-04-28",]

#checking for duplicates - if trump_presidency_df = trump_presidency2_df then no duplicates found
p46_presidency_df <- distinct(p46_presidency_df)


#
#--------------------------------------------------------------- keywords search

p46_words <- p46_presidency_df %>%
  unnest_tokens(word, text, token = "tweets")
# ^ returns 326688 words used in all 56571 tweets relevant for the time-period
# to return only words which convey some personalized style I need to exclude common
# words like 'this' or 'the' or 'about' view stop_words from tidytext package to
# see the whole list of excluded words
# -------------------------------------------> View(stop_words)
p46_words <- p46_words %>%
  anti_join(stop_words, by = "word")
# ^ renders 139531 words

p46_words_count <- p46_words %>%
  group_by(word) %>%
  count()

p46_words <- p46_words_count %>%
  arrange(-n)

View(p46_words)
p46_words_df <- as.data.frame(p46_words)
write.csv(p46_words_df, "p46_words.csv") # saving all words counted for manual
# review and analysis

#-----------------------------------------------end of keywords search

#
#
# Positive TEST
#
#

#clean tweets from retweets to obtain only organic content
news_no_df <- news_tweets_df %>% 
  filter(news_tweets_df$isRetweet %in% c("FALSE"))

#filter tweets from deleted ones
#no deleted tweet in dataset

#filter tweets from quotations
news_nodelquo_df <- news_no_df %>%
  filter(!str_detect(text, '^"')) %>%
  filter(!str_detect(text, '^“'))

#filter tweets starting with link
news_nodql_df <- news_nodelquo_df %>%
  filter(!str_detect(text, '^https://t.co/'))

#filter tweets with ANY links
news_textonly_df <- news_nodql_df #%>%
#  filter(!str_detect(text, 'https://t.co/'))

#delete residual &amp
news_textonly_df <- news_textonly_df %>%
  filter(!str_detect(text, 'amp'))

ts_plot(news_textonly_df, by = "days")


