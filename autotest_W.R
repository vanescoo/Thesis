keyword_w = "business"
vaderscore_w = 0.9

w_base <- trump_textonly_df[trump_textonly_df$date >= "2020-08-19" & trump_textonly_df$date <= "2020-11-07",]

w_base <- w_base %>%
  #filter(str_detect(text, paste(c(words_trump$word),collapse = '|'))) #load manually words_trump with UTF-8
  #filter(str_detect(text, 'China|jobs|trade|Russia|USA|dollars|economy|tax|Korea|bill|market|tariff|companies|billion|Iran|stock|cut|business|economic|million|prices|unemployment|Ukraine|deals|fed'))
  #filter(str_detect(text, keyword_w))
  #filter(str_detect(text, 'market|stock|economy|unemployment|prices|cut|fed|jobs|highest|rates|all-time|cases|lowest|rate|best|faster|manufacturing|dow|tax|prescription|wealth|consumer|regulation|deaths|plants|reserve|numbers|inflation|401k|recorded'))

w_vader <- vader_df(w_base$text, incl_nt = T, neu_set = T, rm_qm = T)
w_vader$date <- w_base$date

w_valid <- w_vader[w_vader$compound >= vaderscore_w | w_vader$compound <= -vaderscore_w,]

ts_plot(w_valid, by = "week")

#after16and weekend
#to shift twweets from outside the trading hours (16:00 onwards) I simply add 8 hours so they count
#as next day tweets (ie also having influence over next day trading results)

w_timeshift = as.POSIXlt(w_valid$date)
w_timeshift_final = w_timeshift

for (i in seq_along(w_timeshift_final)){
  if (w_timeshift[i]$hour > 17) w_timeshift_final[i]=w_timeshift[i] + 28800
}

w_valid$date <- w_timeshift_final

#next I want to move all saturday days by 24hrs effectivelly eliminating saturday tweets

w_satshift_final = as.POSIXlt(w_timeshift_final)
for (i in seq_along(w_timeshift_final)){
  if (weekdays(w_timeshift_final[i]) == "Saturday") w_satshift_final[i]=w_timeshift_final[i] + 115200
}

#lastly I want to move all Sunday days by 24hrs effectively making all Saturday and Sunday Tweets count as Monday Tweets

w_sunshift_final = as.POSIXlt(w_satshift_final)
for (i in seq_along(w_timeshift_final)){
  if (weekdays(w_satshift_final[i]) == "Sunday") w_sunshift_final[i]=w_satshift_final[i] + 115200
}
weekdays(w_sunshift_final[1:200]) #checkup if the process went well

w_valid$date <- w_sunshift_final #save up

#get rid of the hours/minutes/seconds as now are irrelevant
w_valid$date <- as.Date(as.POSIXct(w_valid$date,format='%m/%d/%Y %H:%M:%S %p'))

#final tweets data set with compound sum and compound mean valence scores across all days observed
w_final <- w_valid
w_final <- aggregate(w_valid["compound"], by=w_valid["date"], sum)
w_mean <- aggregate(w_valid["compound"], by=w_valid["date"], mean)
w_final$mean <- w_mean$compound
w_final$difference <- w_final$compound - w_final$mean

#download SP500
SPX <- BatchGetSymbols(
  tickers = c("^GSPC"),
  first.date = w_final$date[1] - 1,
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
for (i in seq_along(w_final)) {
  w_SPX <- left_join(w_final, SPX_dt, by = c("date" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

w_SPX$return <- round(w_SPX$return, digits = 4)
w_SPX <- na.omit(w_SPX) #skip N/A values

w_SPX$correlation <- round(w_SPX$mean * w_SPX$return, digits = 6)
w_SPX$valid <- w_SPX$correlation > 0 

#download Nasdaq
NSQ <- BatchGetSymbols(
  tickers = c("^IXIC"),
  first.date = w_final$date[1] - 1,
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
for (i in seq_along(w_final)) {
  w_NSQ <- left_join(w_final, NSQ_dt, by = c("date" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

w_NSQ$return <- round(w_NSQ$return, digits = 4)
w_NSQ <- na.omit(w_NSQ) #skip N/A values

w_NSQ$correlation <- round(w_NSQ$mean * w_NSQ$return, digits = 6)
w_NSQ$valid <- w_NSQ$correlation > 0 

#download DOW
DOW <- BatchGetSymbols(
  tickers = c("^DJI"),
  first.date = w_final$date[1] - 1,
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
for (i in seq_along(w_final)) {
  w_DOW <- left_join(w_final, DOW_dt, by = c("date" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

w_DOW$return <- round(w_DOW$return, digits = 4)
w_DOW <- na.omit(w_DOW) #skip N/A values

w_DOW$correlation <- round(w_DOW$mean * w_DOW$return, digits = 6)
w_DOW$valid <- w_DOW$correlation > 0 

print("SPX")
sum(w_SPX$correlation > 0, na.rm=TRUE)
sum(w_SPX$correlation < 0, na.rm=TRUE)
sum(w_SPX$correlation == 0, na.rm=TRUE)
print("NSQ")
sum(w_NSQ$correlation > 0, na.rm=TRUE)
sum(w_NSQ$correlation < 0, na.rm=TRUE)
sum(w_NSQ$correlation == 0, na.rm=TRUE)
print("DOW")
sum(w_DOW$correlation > 0, na.rm=TRUE)
sum(w_DOW$correlation < 0, na.rm=TRUE)
sum(w_DOW$correlation == 0, na.rm=TRUE)


