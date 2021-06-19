keyword_ob = "fed"
vaderscore_ob = 0.9

obama_presidency_df <- obama_textonly_df[obama_textonly_df$created >= "2015-05-18" & obama_textonly_df$created <= "2017-01-20",]

obama_presidency_df <- obama_presidency_df %>%
  #filter(str_detect(text, paste(c(words_trump$word),collapse = '|'))) #load manually words_trump with UTF-8
  #filter(str_detect(text, 'China|jobs|trade|Russia|USA|dollars|economy|tax|Korea|bill|market|tariff|companies|billion|Iran|stock|cut|business|economic|million|prices|unemployment|Ukraine|deal|fed'))
  #filter(str_detect(text, keyword_ob))
  filter(str_detect(text, 'market|stock|economy|unemployment|prices|cut|fed|jobs|highest|rates|all-time|cases|lowest|rate|best|faster|manufacturing|dow|tax|prescription|wealth|consumer|regulation|deaths|plants|reserve|numbers|inflation|401k|recorded'))

obama_presidency_vader <- vader_df(obama_presidency_df$text, incl_nt = T, neu_set = T, rm_qm = T)
obama_presidency_vader$created <- obama_presidency_df$created

obama_presidency_valid <- obama_presidency_vader[obama_presidency_vader$compound >= vaderscore_ob | obama_presidency_vader$compound <= -vaderscore_ob,]

ts_plot(obama_presidency_valid, by = "week")

#after16and weekend
#to shift twweets from outside the trading hours (16:00 onwards) I simply add 8 hours so they count
#as next day tweets (ie also having influence over next day trading results)

o_timeshift = as.POSIXlt(obama_presidency_valid$created)
o_timeshift_final = o_timeshift

for (i in seq_along(o_timeshift_final)){
  if (o_timeshift[i]$hour > 17) o_timeshift_final[i]=o_timeshift[i] + 28800
}

obama_presidency_valid$date <- o_timeshift_final

#next I want to move all saturday days by 24hrs effectivelly eliminating saturday tweets

o_satshift_final = as.POSIXlt(o_timeshift_final)
for (i in seq_along(o_timeshift_final)){
  if (weekdays(o_timeshift_final[i]) == "Saturday") o_satshift_final[i]=o_timeshift_final[i] + 115200
}

#lastly I want to move all Sunday days by 24hrs effectively making all Saturday and Sunday Tweets count as Monday Tweets

o_sunshift_final = as.POSIXlt(o_satshift_final)
for (i in seq_along(o_timeshift_final)){
  if (weekdays(o_satshift_final[i]) == "Sunday") o_sunshift_final[i]=o_satshift_final[i] + 115200
}
weekdays(o_sunshift_final[1:200]) #checkup if the process went well

obama_presidency_valid$created <- o_sunshift_final #save up

#get rid of the hours/minutes/seconds as now are irrelevant
obama_presidency_valid$created <- as.Date(as.POSIXct(obama_presidency_valid$created,format='%m/%d/%Y %H:%M:%S %p'))

#final tweets data set with compound sum and compound mean valence scores across all days observed
o_final <- obama_presidency_valid
o_final <- aggregate(obama_presidency_valid["compound"], by=obama_presidency_valid["created"], sum)
o_mean <- aggregate(obama_presidency_valid["compound"], by=obama_presidency_valid["created"], mean)
o_final$mean <- o_mean$compound
o_final$difference <- o_final$compound - o_final$mean

#download SP500
SPX <- BatchGetSymbols(
  tickers = c("^GSPC"),
  first.date = o_final$created[1] - 1,
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
for (i in seq_along(o_final)) {
  o_SPX <- left_join(o_final, SPX_dt, by = c("created" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

o_SPX$return <- round(o_SPX$return, digits = 4)
o_SPX <- na.omit(o_SPX) #skip N/A values

o_SPX$correlation <- round(o_SPX$mean * o_SPX$return, digits = 6)
o_SPX$valid <- o_SPX$correlation > 0 

#download Nasdaq
NSQ <- BatchGetSymbols(
  tickers = c("^IXIC"),
  first.date = o_final$created[1] - 1,
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
for (i in seq_along(o_final)) {
  o_NSQ <- left_join(o_final, NSQ_dt, by = c("created" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

o_NSQ$return <- round(o_NSQ$return, digits = 4)
o_NSQ <- na.omit(o_NSQ) #skip N/A values

o_NSQ$correlation <- round(o_NSQ$mean * o_NSQ$return, digits = 6)
o_NSQ$valid <- o_NSQ$correlation > 0 

#download DOW
DOW <- BatchGetSymbols(
  tickers = c("^DJI"),
  first.date = o_final$created[1] - 1,
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
for (i in seq_along(o_final)) {
  o_DOW <- left_join(o_final, DOW_dt, by = c("created" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

o_DOW$return <- round(o_DOW$return, digits = 4)
o_DOW <- na.omit(o_DOW) #skip N/A values

o_DOW$correlation <- round(o_DOW$mean * o_DOW$return, digits = 6)
o_DOW$valid <- o_DOW$correlation > 0 

print("SPX")
sum(o_SPX$correlation > 0, na.rm=TRUE)
sum(o_SPX$correlation < 0, na.rm=TRUE)
sum(o_SPX$correlation == 0, na.rm=TRUE)
print("NSQ")
sum(o_NSQ$correlation > 0, na.rm=TRUE)
sum(o_NSQ$correlation < 0, na.rm=TRUE)
sum(o_NSQ$correlation == 0, na.rm=TRUE)
print("DOW")
sum(o_DOW$correlation > 0, na.rm=TRUE)
sum(o_DOW$correlation < 0, na.rm=TRUE)
sum(o_DOW$correlation == 0, na.rm=TRUE)


