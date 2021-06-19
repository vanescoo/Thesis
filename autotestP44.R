keyword_p44 = "fed"
vaderscore_p44 = 0.9

p44_presidency_df <- p44_textonly_df[p44_textonly_df$created >= "2015-05-18" & p44_textonly_df$created <= "2017-01-20",]

p44_presidency_df <- p44_presidency_df  #%>%
  #filter(str_detect(text, paste(c(words_trump$word),collapse = '|'))) #load manually words_trump with UTF-8
  #filter(str_detect(text, 'China|jobs|trade|Russia|USA|dollars|economy|tax|Korea|bill|market|tariff|companies|billion|Iran|stock|cut|business|economic|million|prices|unemployment|Ukraine|deal|fed'))
  #filter(str_detect(text, keyword_p44))
  #filter(str_detect(text, 'market|stock|economy|unemployment|prices|cut|fed|jobs|highest|rates|all-time|cases|lowest|rate|best|faster|manufacturing|dow|tax|prescription|wealth|consumer|regulation|deaths|plants|reserve|numbers|inflation|401k|recorded'))

p44_presidency_vader <- vader_df(p44_presidency_df$text, incl_nt = T, neu_set = T, rm_qm = T)
p44_presidency_vader$created <- p44_presidency_df$created

p44_presidency_valid <- p44_presidency_vader[p44_presidency_vader$compound >= vaderscore_p44 | p44_presidency_vader$compound <= -vaderscore_p44,]

ts_plot(p44_presidency_valid, by = "week")

#after16and weekend
#to shift twweets from outside the trading hours (16:00 onwards) I simply add 8 hours so they count
#as next day tweets (ie also having influence over next day trading results)

p44_timeshift = as.POSIXlt(p44_presidency_valid$created)
p44_timeshift_final = p44_timeshift

for (i in seq_along(p44_timeshift_final)){
  if (p44_timeshift[i]$hour > 17) p44_timeshift_final[i]=p44_timeshift[i] + 28800
}

p44_presidency_valid$date <- p44_timeshift_final

#next I want to move all saturday days by 24hrs effectivelly eliminating saturday tweets

p44_satshift_final = as.POSIXlt(p44_timeshift_final)
for (i in seq_along(p44_timeshift_final)){
  if (weekdays(p44_timeshift_final[i]) == "Saturday") p44_satshift_final[i]=p44_timeshift_final[i] + 115200
}

#lastly I want to move all Sunday days by 24hrs effectively making all Saturday and Sunday Tweets count as Monday Tweets

p44_sunshift_final = as.POSIXlt(p44_satshift_final)
for (i in seq_along(p44_timeshift_final)){
  if (weekdays(p44_satshift_final[i]) == "Sunday") p44_sunshift_final[i]=p44_satshift_final[i] + 115200
}
weekdays(p44_sunshift_final[1:200]) #checkup if the process went well

p44_presidency_valid$created <- p44_sunshift_final #save up

#get rid of the hours/minutes/seconds as now are irrelevant
p44_presidency_valid$created <- as.Date(as.POSIXct(p44_presidency_valid$created,format='%m/%d/%Y %H:%M:%S %p'))

#final tweets data set with compound sum and compound mean valence scores across all days observed
p44_final <- p44_presidency_valid
p44_final <- aggregate(p44_presidency_valid["compound"], by=p44_presidency_valid["created"], sum)
p44_mean <- aggregate(p44_presidency_valid["compound"], by=p44_presidency_valid["created"], mean)
p44_final$mean <- p44_mean$compound
p44_final$difference <- p44_final$compound - p44_final$mean

#download SP500
SPX <- BatchGetSymbols(
  tickers = c("^GSPC"),
  first.date = p44_final$created[1] - 1,
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
for (i in seq_along(p44_final)) {
  p44_SPX <- left_join(p44_final, SPX_dt, by = c("created" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

p44_SPX$return <- round(p44_SPX$return, digits = 4)
p44_SPX <- na.omit(p44_SPX) #skip N/A values

p44_SPX$correlation <- round(p44_SPX$mean * p44_SPX$return, digits = 6)
p44_SPX$valid <- p44_SPX$correlation > 0 

#download Nasdaq
NSQ <- BatchGetSymbols(
  tickers = c("^IXIC"),
  first.date = p44_final$created[1] - 1,
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
for (i in seq_along(p44_final)) {
  p44_NSQ <- left_join(p44_final, NSQ_dt, by = c("created" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

p44_NSQ$return <- round(p44_NSQ$return, digits = 4)
p44_NSQ <- na.omit(p44_NSQ) #skip N/A values

p44_NSQ$correlation <- round(p44_NSQ$mean * p44_NSQ$return, digits = 6)
p44_NSQ$valid <- p44_NSQ$correlation > 0 

#download DOW
DOW <- BatchGetSymbols(
  tickers = c("^DJI"),
  first.date = p44_final$created[1] - 1,
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
for (i in seq_along(p44_final)) {
  p44_DOW <- left_join(p44_final, DOW_dt, by = c("created" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

p44_DOW$return <- round(p44_DOW$return, digits = 4)
p44_DOW <- na.omit(p44_DOW) #skip N/A values

p44_DOW$correlation <- round(p44_DOW$mean * p44_DOW$return, digits = 6)
p44_DOW$valid <- p44_DOW$correlation > 0 

print("SPX")
sum(p44_SPX$correlation > 0, na.rm=TRUE)
sum(p44_SPX$correlation < 0, na.rm=TRUE)
sum(p44_SPX$correlation == 0, na.rm=TRUE)
print("NSQ")
sum(p44_NSQ$correlation > 0, na.rm=TRUE)
sum(p44_NSQ$correlation < 0, na.rm=TRUE)
sum(p44_NSQ$correlation == 0, na.rm=TRUE)
print("DOW")
sum(p44_DOW$correlation > 0, na.rm=TRUE)
sum(p44_DOW$correlation < 0, na.rm=TRUE)
sum(p44_DOW$correlation == 0, na.rm=TRUE)


