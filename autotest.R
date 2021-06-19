keyword = "deals"
vaderscore = 0

trump_presidency_df <- trump_textonly_df[trump_textonly_df$date >= "2017-01-20" & trump_textonly_df$date <= "2021-01-08",]

trump_presidency_df <- trump_presidency_df %>%
  #filter(str_detect(text, paste(c(words_trump$word),collapse = '|'))) #load manually words_trump with UTF-8
  #filter(str_detect(text, 'China|jobs|trade|Russia|USA|dollars|economy|tax|Korea|bill|market|tariff|companies|billion|Iran|stock|cut|business|economic|million|prices|unemployment|Ukraine|deals|fed'))
  filter(str_detect(text, keyword))
  #filter(str_detect(text, 'market|stock|economy|unemployment|prices|cut|fed|jobs|highest|rates|all-time|cases|lowest|rate|best|faster|manufacturing|dow|tax|prescription|wealth|consumer|regulation|deaths|plants|reserve|numbers|inflation|401k|recorded'))

trump_presidency_vader <- vader_df(trump_presidency_df$text, incl_nt = T, neu_set = T, rm_qm = T)
trump_presidency_vader$date <- trump_presidency_df$date

trump_presidency_valid <- trump_presidency_vader[trump_presidency_vader$compound >= vaderscore | trump_presidency_vader$compound <= -vaderscore,]

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

#download SP500
SPX <- BatchGetSymbols(
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

SPX_dt <- SPX$df.tickers %>% setDT() %>% #convert to data table
  .[order(ticker, ref.date)] #order by ticker and date
SPX_dt <- SPX_dt %>% select(7,10)
#--------------
for (i in seq_along(t_final)) {
  t_SPX <- left_join(t_final, SPX_dt, by = c("date" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

t_SPX$return <- round(t_SPX$return, digits = 4)
t_SPX <- na.omit(t_SPX) #skip N/A values

t_SPX$correlation <- round(t_SPX$mean * t_SPX$return, digits = 6)
t_SPX$valid <- t_SPX$correlation > 0 

#download Nasdaq
NSQ <- BatchGetSymbols(
  tickers = c("^IXIC"),
  first.date = t_final$date[1] - 1,
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
for (i in seq_along(t_final)) {
  t_NSQ <- left_join(t_final, NSQ_dt, by = c("date" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

t_NSQ$return <- round(t_NSQ$return, digits = 4)
t_NSQ <- na.omit(t_NSQ) #skip N/A values

t_NSQ$correlation <- round(t_NSQ$mean * t_NSQ$return, digits = 6)
t_NSQ$valid <- t_NSQ$correlation > 0 

#download DOW
DOW <- BatchGetSymbols(
  tickers = c("^DJI"),
  first.date = t_final$date[1] - 1,
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
for (i in seq_along(t_final)) {
  t_DOW <- left_join(t_final, DOW_dt, by = c("date" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

t_DOW$return <- round(t_DOW$return, digits = 4)
t_DOW <- na.omit(t_DOW) #skip N/A values

t_DOW$correlation <- round(t_DOW$mean * t_DOW$return, digits = 6)
t_DOW$valid <- t_DOW$correlation > 0 

print("SPX")
sum(t_SPX$correlation > 0, na.rm=TRUE)
sum(t_SPX$correlation < 0, na.rm=TRUE)
sum(t_SPX$correlation == 0, na.rm=TRUE)
print("NSQ")
sum(t_NSQ$correlation > 0, na.rm=TRUE)
sum(t_NSQ$correlation < 0, na.rm=TRUE)
sum(t_NSQ$correlation == 0, na.rm=TRUE)
print("DOW")
sum(t_DOW$correlation > 0, na.rm=TRUE)
sum(t_DOW$correlation < 0, na.rm=TRUE)
sum(t_DOW$correlation == 0, na.rm=TRUE)


