keyword_bi = "deal"
vaderscore_bi = 0.3

biden_presidency_df <- biden_textonly_df[biden_textonly_df$created >= "2020-08-19" & biden_textonly_df$created <= "2021-04-28",]

#biden_presidency_df <- biden_presidency_df %>%
  #filter(str_detect(text, paste(c(words_trump$word),collapse = '|'))) #load manually words_trump with UTF-8
  #filter(str_detect(text, 'China|jobs|trade|Russia|USA|dollars|economy|tax|Korea|bill|market|tariff|companies|billion|Iran|stock|cut|business|economic|million|prices|unemployment|Ukraine|deal|fed'))
  #filter(str_detect(text, keyword_bi))
  #filter(str_detect(text, 'market|stock|economy|unemployment|prices|cut|fed|jobs|highest|rates|all-time|cases|lowest|rate|best|faster|manufacturing|dow|tax|prescription|wealth|consumer|regulation|deaths|plants|reserve|numbers|inflation|401k|recorded'))

biden_presidency_vader <- vader_df(biden_presidency_df$text, incl_nt = T, neu_set = T, rm_qm = T)
biden_presidency_vader$created <- biden_presidency_df$created

biden_presidency_valid <- biden_presidency_vader[biden_presidency_vader$compound >= vaderscore_bi | biden_presidency_vader$compound <= -vaderscore_bi,]

ts_plot(biden_presidency_valid, by = "week")

#after16and weekend
#to shift twweets from outside the trading hours (16:00 onwards) I simply add 8 hours so they count
#as next day tweets (ie also having influence over next day trading results)

b_timeshift = as.POSIXlt(biden_presidency_valid$created)
b_timeshift_final = b_timeshift

for (i in seq_along(b_timeshift_final)){
  if (b_timeshift[i]$hour > 17) b_timeshift_final[i]=b_timeshift[i] + 28800
}

biden_presidency_valid$date <- b_timeshift_final

#next I want to move all saturday days by 24hrs effectivelly eliminating saturday tweets

b_satshift_final = as.POSIXlt(b_timeshift_final)
for (i in seq_along(b_timeshift_final)){
  if (weekdays(b_timeshift_final[i]) == "Saturday") b_satshift_final[i]=b_timeshift_final[i] + 115200
}

#lastly I want to move all Sunday days by 24hrs effectively making all Saturday and Sunday Tweets count as Monday Tweets

b_sunshift_final = as.POSIXlt(b_satshift_final)
for (i in seq_along(b_timeshift_final)){
  if (weekdays(b_satshift_final[i]) == "Sunday") b_sunshift_final[i]=b_satshift_final[i] + 115200
}
weekdays(b_sunshift_final[1:200]) #checkup if the process went well

biden_presidency_valid$created <- b_sunshift_final #save up

#get rid of the hours/minutes/seconds as now are irrelevant
biden_presidency_valid$created <- as.Date(as.POSIXct(biden_presidency_valid$created,format='%m/%d/%Y %H:%M:%S %p'))

#final tweets data set with compound sum and compound mean valence scores across all days observed
b_final <- biden_presidency_valid
b_final <- aggregate(biden_presidency_valid["compound"], by=biden_presidency_valid["created"], sum)
b_mean <- aggregate(biden_presidency_valid["compound"], by=biden_presidency_valid["created"], mean)
b_final$mean <- b_mean$compound
b_final$difference <- b_final$compound - b_final$mean

#download SP500
SPX <- BatchGetSymbols(
  tickers = c("^GSPC"),
  first.date = b_final$created[1] - 1,
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
for (i in seq_along(b_final)) {
  b_SPX <- left_join(b_final, SPX_dt, by = c("created" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

b_SPX$return <- round(b_SPX$return, digits = 4)
b_SPX <- na.omit(b_SPX) #skip N/A values

b_SPX$correlation <- round(b_SPX$mean * b_SPX$return, digits = 6)
b_SPX$valid <- b_SPX$correlation > 0 

#download Nasdaq
NSQ <- BatchGetSymbols(
  tickers = c("^IXIC"),
  first.date = b_final$created[1] - 1,
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
for (i in seq_along(b_final)) {
  b_NSQ <- left_join(b_final, NSQ_dt, by = c("created" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

b_NSQ$return <- round(b_NSQ$return, digits = 4)
b_NSQ <- na.omit(b_NSQ) #skip N/A values

b_NSQ$correlation <- round(b_NSQ$mean * b_NSQ$return, digits = 6)
b_NSQ$valid <- b_NSQ$correlation > 0 

#download DOW
DOW <- BatchGetSymbols(
  tickers = c("^DJI"),
  first.date = b_final$created[1] - 1,
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
for (i in seq_along(b_final)) {
  b_DOW <- left_join(b_final, DOW_dt, by = c("created" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

b_DOW$return <- round(b_DOW$return, digits = 4)
b_DOW <- na.omit(b_DOW) #skip N/A values

b_DOW$correlation <- round(b_DOW$mean * b_DOW$return, digits = 6)
b_DOW$valid <- b_DOW$correlation > 0 

print("SPX")
sum(b_SPX$correlation > 0, na.rm=TRUE)
sum(b_SPX$correlation < 0, na.rm=TRUE)
sum(b_SPX$correlation == 0, na.rm=TRUE)
print("NSQ")
sum(b_NSQ$correlation > 0, na.rm=TRUE)
sum(b_NSQ$correlation < 0, na.rm=TRUE)
sum(b_NSQ$correlation == 0, na.rm=TRUE)
print("DOW")
sum(b_DOW$correlation > 0, na.rm=TRUE)
sum(b_DOW$correlation < 0, na.rm=TRUE)
sum(b_DOW$correlation == 0, na.rm=TRUE)


