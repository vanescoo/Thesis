keyword_p46 = "million"
vaderscore_p46 = 0.3

p46_presidency_df <- p46_textonly_df[p46_textonly_df$created >= "2020-08-19" & p46_textonly_df$created <= "2021-04-28",]

p46_presidency_df <- p46_presidency_df %>%
  #filter(str_detect(text, paste(c(words_trump$word),collapse = '|'))) #load manually words_trump with UTF-8
  #filter(str_detect(text, 'China|jobs|trade|Russia|USA|dollars|economy|tax|Korea|bill|market|tariff|companies|billion|Iran|stock|cut|business|economic|million|prices|unemployment|Ukraine|deal|fed'))
  filter(str_detect(text, keyword_p46))
  #filter(str_detect(text, 'market|stock|economy|unemployment|prices|cut|fed|jobs|highest|rates|all-time|cases|lowest|rate|best|faster|manufacturing|dow|tax|prescription|wealth|consumer|regulation|deaths|plants|reserve|numbers|inflation|401k|recorded'))

p46_presidency_vader <- vader_df(p46_presidency_df$text, incl_nt = T, neu_set = T, rm_qm = T)
p46_presidency_vader$created <- p46_presidency_df$created

p46_presidency_valid <- p46_presidency_vader[p46_presidency_vader$compound >= vaderscore_p46 | p46_presidency_vader$compound <= -vaderscore_p46,]

ts_plot(p46_presidency_valid, by = "week")

#after16and weekend
#to shift twweets from outside the trading hours (16:00 onwards) I simply add 8 hours so they count
#as next day tweets (ie also having influence over next day trading results)

p46_timeshift = as.POSIXlt(p46_presidency_valid$created)
p46_timeshift_final = p46_timeshift

for (i in seq_along(p46_timeshift_final)){
  if (p46_timeshift[i]$hour > 17) p46_timeshift_final[i]=p46_timeshift[i] + 28800
}

p46_presidency_valid$date <- p46_timeshift_final

#next I want to move all saturday days by 24hrs effectivelly eliminating saturday tweets

p46_satshift_final = as.POSIXlt(p46_timeshift_final)
for (i in seq_along(p46_timeshift_final)){
  if (weekdays(p46_timeshift_final[i]) == "Saturday") p46_satshift_final[i]=p46_timeshift_final[i] + 115200
}

#lastly I want to move all Sunday days by 24hrs effectively making all Saturday and Sunday Tweets count as Monday Tweets

p46_sunshift_final = as.POSIXlt(p46_satshift_final)
for (i in seq_along(p46_timeshift_final)){
  if (weekdays(p46_satshift_final[i]) == "Sunday") p46_sunshift_final[i]=p46_satshift_final[i] + 115200
}
weekdays(p46_sunshift_final[1:200]) #checkup if the process went well

p46_presidency_valid$created <- p46_sunshift_final #save up

#get rid of the hours/minutes/seconds as now are irrelevant
p46_presidency_valid$created <- as.Date(as.POSIXct(p46_presidency_valid$created,format='%m/%d/%Y %H:%M:%S %p'))

#final tweets data set with compound sum and compound mean valence scores across all days observed
p46_final <- p46_presidency_valid
p46_final <- aggregate(p46_presidency_valid["compound"], by=p46_presidency_valid["created"], sum)
p46_mean <- aggregate(p46_presidency_valid["compound"], by=p46_presidency_valid["created"], mean)
p46_final$mean <- p46_mean$compound
p46_final$difference <- p46_final$compound - p46_final$mean

#download SP500
SPX <- BatchGetSymbols(
  tickers = c("^GSPC"),
  first.date = p46_final$created[1] - 1,
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
for (i in seq_along(p46_final)) {
  p46_SPX <- left_join(p46_final, SPX_dt, by = c("created" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

p46_SPX$return <- round(p46_SPX$return, digits = 4)
p46_SPX <- na.omit(p46_SPX) #skip N/A values

p46_SPX$correlation <- round(p46_SPX$mean * p46_SPX$return, digits = 6)
p46_SPX$valid <- p46_SPX$correlation > 0 

#download Nasdaq
NSQ <- BatchGetSymbols(
  tickers = c("^IXIC"),
  first.date = p46_final$created[1] - 1,
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
for (i in seq_along(p46_final)) {
  p46_NSQ <- left_join(p46_final, NSQ_dt, by = c("created" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

p46_NSQ$return <- round(p46_NSQ$return, digits = 4)
p46_NSQ <- na.omit(p46_NSQ) #skip N/A values

p46_NSQ$correlation <- round(p46_NSQ$mean * p46_NSQ$return, digits = 6)
p46_NSQ$valid <- p46_NSQ$correlation > 0 

#download DOW
DOW <- BatchGetSymbols(
  tickers = c("^DJI"),
  first.date = p46_final$created[1] - 1,
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
for (i in seq_along(p46_final)) {
  p46_DOW <- left_join(p46_final, DOW_dt, by = c("created" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

p46_DOW$return <- round(p46_DOW$return, digits = 4)
p46_DOW <- na.omit(p46_DOW) #skip N/A values

p46_DOW$correlation <- round(p46_DOW$mean * p46_DOW$return, digits = 6)
p46_DOW$valid <- p46_DOW$correlation > 0 

print("SPX")
sum(p46_SPX$correlation > 0, na.rm=TRUE)
sum(p46_SPX$correlation < 0, na.rm=TRUE)
sum(p46_SPX$correlation == 0, na.rm=TRUE)
print("NSQ")
sum(p46_NSQ$correlation > 0, na.rm=TRUE)
sum(p46_NSQ$correlation < 0, na.rm=TRUE)
sum(p46_NSQ$correlation == 0, na.rm=TRUE)
print("DOW")
sum(p46_DOW$correlation > 0, na.rm=TRUE)
sum(p46_DOW$correlation < 0, na.rm=TRUE)
sum(p46_DOW$correlation == 0, na.rm=TRUE)


