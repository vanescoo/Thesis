keyword_u = "business"
vaderscore_u = 0

u_base <- trump_textonly_df[trump_textonly_df$date >= "2017-01-20" & trump_textonly_df$date <= "2020-08-18",]

u_base <- u_base %>%
  #ilter(str_detect(text, paste(c(words_trump$word),collapse = '|'))) #load manually words_trump with UTF-8
  #filter(str_detect(text, 'China|jobs|trade|Russia|USA|dollars|economy|tax|Korea|bill|market|tariff|companies|billion|Iran|stock|cut|business|economic|million|prices|unemployment|Ukraine|deals|fed'))
  #filter(str_detect(text, keyword_u))
  #filter(str_detect(text, 'market|stock|economy|unemployment|prices|cut|fed|jobs|highest|rates|all-time|cases|lowest|rate|best|faster|manufacturing|dow|tax|prescription|wealth|consumer|regulation|deaths|plants|reserve|numbers|inflation|401k|recorded'))

u_vader <- vader_df(u_base$text, incl_nt = T, neu_set = T, rm_qm = T)
u_vader$date <- u_base$date

u_valid <- u_vader[u_vader$compound >= vaderscore_u | u_vader$compound <= -vaderscore_u,]

ts_plot(u_valid, by = "week")

#after16and weekend
#to shift twweets from outside the trading hours (16:00 onwards) I simply add 8 hours so they count
#as next day tweets (ie also having influence over next day trading results)

u_timeshift = as.POSIXlt(u_valid$date)
u_timeshift_final = u_timeshift

for (i in seq_along(u_timeshift_final)){
  if (u_timeshift[i]$hour > 17) u_timeshift_final[i]=u_timeshift[i] + 28800
}

u_valid$date <- u_timeshift_final

#next I want to move all saturday days by 24hrs effectivelly eliminating saturday tweets

u_satshift_final = as.POSIXlt(u_timeshift_final)
for (i in seq_along(u_timeshift_final)){
  if (weekdays(u_timeshift_final[i]) == "Saturday") u_satshift_final[i]=u_timeshift_final[i] + 115200
}

#lastly I want to move all Sunday days by 24hrs effectively making all Saturday and Sunday Tweets count as Monday Tweets

u_sunshift_final = as.POSIXlt(u_satshift_final)
for (i in seq_along(u_timeshift_final)){
  if (weekdays(u_satshift_final[i]) == "Sunday") u_sunshift_final[i]=u_satshift_final[i] + 115200
}
weekdays(u_sunshift_final[1:200]) #checkup if the process went well

u_valid$date <- u_sunshift_final #save up

#get rid of the hours/minutes/seconds as now are irrelevant
u_valid$date <- as.Date(as.POSIXct(u_valid$date,format='%m/%d/%Y %H:%M:%S %p'))

#final tweets data set with compound sum and compound mean valence scores across all days observed
u_final <- u_valid
u_final <- aggregate(u_valid["compound"], by=u_valid["date"], sum)
u_mean <- aggregate(u_valid["compound"], by=u_valid["date"], mean)
u_final$mean <- u_mean$compound
u_final$difference <- u_final$compound - u_final$mean

#download SP500
SPX <- BatchGetSymbols(
  tickers = c("^GSPC"),
  first.date = u_final$date[1] - 1,
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
for (i in seq_along(u_final)) {
  u_SPX <- left_join(u_final, SPX_dt, by = c("date" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

u_SPX$return <- round(u_SPX$return, digits = 4)
u_SPX <- na.omit(u_SPX) #skip N/A values

u_SPX$correlation <- round(u_SPX$mean * u_SPX$return, digits = 6)
u_SPX$valid <- u_SPX$correlation > 0 

#download Nasdaq
NSQ <- BatchGetSymbols(
  tickers = c("^IXIC"),
  first.date = u_final$date[1] - 1,
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
for (i in seq_along(u_final)) {
  u_NSQ <- left_join(u_final, NSQ_dt, by = c("date" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

u_NSQ$return <- round(u_NSQ$return, digits = 4)
u_NSQ <- na.omit(u_NSQ) #skip N/A values

u_NSQ$correlation <- round(u_NSQ$mean * u_NSQ$return, digits = 6)
u_NSQ$valid <- u_NSQ$correlation > 0 

#download DOW
DOW <- BatchGetSymbols(
  tickers = c("^DJI"),
  first.date = u_final$date[1] - 1,
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
for (i in seq_along(u_final)) {
  u_DOW <- left_join(u_final, DOW_dt, by = c("date" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

u_DOW$return <- round(u_DOW$return, digits = 4)
u_DOW <- na.omit(u_DOW) #skip N/A values

u_DOW$correlation <- round(u_DOW$mean * u_DOW$return, digits = 6)
u_DOW$valid <- u_DOW$correlation > 0 

print("SPX")
sum(u_SPX$correlation > 0, na.rm=TRUE)
sum(u_SPX$correlation < 0, na.rm=TRUE)
sum(u_SPX$correlation == 0, na.rm=TRUE)
print("NSQ")
sum(u_NSQ$correlation > 0, na.rm=TRUE)
sum(u_NSQ$correlation < 0, na.rm=TRUE)
sum(u_NSQ$correlation == 0, na.rm=TRUE)
print("DOW")
sum(u_DOW$correlation > 0, na.rm=TRUE)
sum(u_DOW$correlation < 0, na.rm=TRUE)
sum(u_DOW$correlation == 0, na.rm=TRUE)


