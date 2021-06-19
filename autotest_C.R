keyword_c = "China"
vaderscore_c = 0.0

#c_base <- biden_textonly_df[biden_textonly_df$created >= "2020-08-19" & biden_textonly_df$created <= "2020-11-07",]
#c_base <- biden_textonly_df[biden_textonly_df$created >= "2020-11-08" & biden_textonly_df$created <= "2021-04-28",]
c_base <- news_textonly_df #For analysis of news

#c_base <- c_base %>%
  #filter(str_detect(text, paste(c(words_trump$word),collapse = '|'))) #load manually words_trump with UTF-8
  #filter(str_detect(text, 'China|jobs|trade|Russia|USA|dollars|economy|tax|Korea|bill|market|tariff|companies|billion|Iran|stock|cut|business|economic|million|prices|unemployment|Ukraine|deals|fed'))
  #filter(str_detect(text, keyword_c))
  #filter(str_detect(text, 'market|stock|economy|unemployment|prices|cut|fed|jobs|highest|rates|all-time|cases|lowest|rate|best|faster|manufacturing|dow|tax|prescription|wealth|consumer|regulation|deaths|plants|reserve|numbers|inflation|401k|recorded'))
  
c_vader <- vader_df(c_base$text, incl_nt = T, neu_set = T, rm_qm = T)
c_vader$date <- c_base$created

c_valid <- c_vader[c_vader$compound >= vaderscore_c | c_vader$compound <= -vaderscore_c,]

ts_plot(c_valid, by = "week")

#after16and weekend
#to shift twweets from outside the trading hours (16:00 onwards) I simply add 8 hours so they count
#as next day tweets (ie also having influence over next day trading results)

c_timeshift = as.POSIXlt(c_valid$date)
c_timeshift_final = c_timeshift

for (i in seq_along(c_timeshift_final)){
  if (c_timeshift[i]$hour > 17) c_timeshift_final[i]=c_timeshift[i] + 28800
}

c_valid$date <- c_timeshift_final

#next I want to move all saturday days by 24hrs effectivelly eliminating saturday tweets

c_satshift_final = as.POSIXlt(c_timeshift_final)
for (i in seq_along(c_timeshift_final)){
  if (weekdays(c_timeshift_final[i]) == "Saturday") c_satshift_final[i]=c_timeshift_final[i] + 115200
}

#lastly I want to move all Sunday days by 24hrs effectively making all Saturday and Sunday Tweets count as Monday Tweets

c_sunshift_final = as.POSIXlt(c_satshift_final)
for (i in seq_along(c_timeshift_final)){
  if (weekdays(c_satshift_final[i]) == "Sunday") c_sunshift_final[i]=c_satshift_final[i] + 115200
}
weekdays(c_sunshift_final[1:200]) #checkup if the process went well

c_valid$date <- c_sunshift_final #save up

#get rid of the hours/minutes/seconds as now are irrelevant
c_valid$date <- as.Date(as.POSIXct(c_valid$date,format='%m/%d/%Y %H:%M:%S %p'))

#final tweets data set with compound sum and compound mean valence scores across all days observed
c_final <- c_valid
c_final <- aggregate(c_valid["compound"], by=c_valid["date"], sum)
c_mean <- aggregate(c_valid["compound"], by=c_valid["date"], mean)
c_final$mean <- c_mean$compound
c_final$difference <- c_final$compound - c_final$mean

#download SP500
SPX <- BatchGetSymbols(
  tickers = c("^GSPC"),
  first.date = c_final$date[1] - 1,
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

SPX_dt <- SPX$df.tickers %>% setDT() %>% #convert to data table
  .[order(ticker, ref.date)] #order by ticker and date
SPX_dt <- SPX_dt %>% select(7,10)
#--------------
for (i in seq_along(c_final)) {
  c_SPX <- left_join(c_final, SPX_dt, by = c("date" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

c_SPX$return <- round(c_SPX$return, digits = 4)
c_SPX <- na.omit(c_SPX) #skip N/A values

c_SPX$correlation <- round(c_SPX$mean * c_SPX$return, digits = 6)
c_SPX$valid <- c_SPX$correlation > 0 

#download Nasdaq
NSQ <- BatchGetSymbols(
  tickers = c("^IXIC"),
  first.date = c_final$date[1] - 1,
  last.date = Sys.Date(),
  thresh.bad.data = 0.75,
  bench.ticker = "^IXIC",
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

NSQ_dt <- NSQ$df.tickers %>% setDT() %>% #convert to data table
  .[order(ticker, ref.date)] #order by ticker and date
NSQ_dt <- NSQ_dt %>% select(7,10)
#--------------
for (i in seq_along(c_final)) {
  c_NSQ <- left_join(c_final, NSQ_dt, by = c("date" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

c_NSQ$return <- round(c_NSQ$return, digits = 4)
c_NSQ <- na.omit(c_NSQ) #skip N/A values

c_NSQ$correlation <- round(c_NSQ$mean * c_NSQ$return, digits = 6)
c_NSQ$valid <- c_NSQ$correlation > 0 

#download DOW
DOW <- BatchGetSymbols(
  tickers = c("^DJI"),
  first.date = c_final$date[1] - 1,
  last.date = Sys.Date(),
  thresh.bad.data = 0.75,
  bench.ticker = "^DJI",
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

DOW_dt <- DOW$df.tickers %>% setDT() %>% #convert to data table
  .[order(ticker, ref.date)] #order by ticker and date
DOW_dt <- DOW_dt %>% select(7,10)
#--------------
for (i in seq_along(c_final)) {
  c_DOW <- left_join(c_final, DOW_dt, by = c("date" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

c_DOW$return <- round(c_DOW$return, digits = 4)
c_DOW <- na.omit(c_DOW) #skip N/A values

c_DOW$correlation <- round(c_DOW$mean * c_DOW$return, digits = 6)
c_DOW$valid <- c_DOW$correlation > 0 

print("SPX")
sum(c_SPX$correlation > 0, na.rm=TRUE)
sum(c_SPX$correlation < 0, na.rm=TRUE)
sum(c_SPX$correlation == 0, na.rm=TRUE)
print("NSQ")
sum(c_NSQ$correlation > 0, na.rm=TRUE)
sum(c_NSQ$correlation < 0, na.rm=TRUE)
sum(c_NSQ$correlation == 0, na.rm=TRUE)
print("DOW")
sum(c_DOW$correlation > 0, na.rm=TRUE)
sum(c_DOW$correlation < 0, na.rm=TRUE)
sum(c_DOW$correlation == 0, na.rm=TRUE)


