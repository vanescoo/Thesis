keyword_z = "China"
vaderscore_z = 0.9

z_base <- trump_textonly_df[trump_textonly_df$date >= "2020-11-08" & trump_textonly_df$date <= "2021-01-08",]

z_base <- z_base %>%
  #filter(str_detect(text, paste(c(words_trump$word),collapse = '|'))) #load manually words_trump with UTF-8
  #filter(str_detect(text, 'China|jobs|trade|Russia|USA|dollars|economy|tax|Korea|bill|market|tariff|companies|billion|Iran|stock|cut|business|economic|million|prices|unemployment|Ukraine|deals|fed'))
  #filter(str_detect(text, keyword_z))
  #filter(str_detect(text, 'market|stock|economy|unemployment|prices|cut|fed|jobs|highest|rates|all-time|cases|lowest|rate|best|faster|manufacturing|dow|tax|prescription|wealth|consumer|regulation|deaths|plants|reserve|numbers|inflation|401k|recorded'))

z_vader <- vader_df(z_base$text, incl_nt = T, neu_set = T, rm_qm = T)
z_vader$date <- z_base$date

z_valid <- z_vader[z_vader$compound >= vaderscore_z | z_vader$compound <= -vaderscore_z,]

ts_plot(z_valid, by = "week")

#after16and weekend
#to shift twweets from outside the trading hours (16:00 onwards) I simply add 8 hours so they count
#as next day tweets (ie also having influence over next day trading results)

z_timeshift = as.POSIXlt(z_valid$date)
z_timeshift_final = z_timeshift

for (i in seq_along(z_timeshift_final)){
  if (z_timeshift[i]$hour > 17) z_timeshift_final[i]=z_timeshift[i] + 28800
}

z_valid$date <- z_timeshift_final

#next I want to move all saturday days by 24hrs effectivelly eliminating saturday tweets

z_satshift_final = as.POSIXlt(z_timeshift_final)
for (i in seq_along(z_timeshift_final)){
  if (weekdays(z_timeshift_final[i]) == "Saturday") z_satshift_final[i]=z_timeshift_final[i] + 115200
}

#lastly I want to move all Sunday days by 24hrs effectively making all Saturday and Sunday Tweets count as Monday Tweets

z_sunshift_final = as.POSIXlt(z_satshift_final)
for (i in seq_along(z_timeshift_final)){
  if (weekdays(z_satshift_final[i]) == "Sunday") z_sunshift_final[i]=z_satshift_final[i] + 115200
}
weekdays(z_sunshift_final[1:200]) #checkup if the process went well

z_valid$date <- z_sunshift_final #save up

#get rid of the hours/minutes/seconds as now are irrelevant
z_valid$date <- as.Date(as.POSIXct(z_valid$date,format='%m/%d/%Y %H:%M:%S %p'))

#final tweets data set with compound sum and compound mean valence scores across all days observed
z_final <- z_valid
z_final <- aggregate(z_valid["compound"], by=z_valid["date"], sum)
z_mean <- aggregate(z_valid["compound"], by=z_valid["date"], mean)
z_final$mean <- z_mean$compound
z_final$difference <- z_final$compound - z_final$mean

#download SP500
SPX <- BatchGetSymbols(
  tickers = c("^GSPC"),
  first.date = z_final$date[1] - 1,
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
for (i in seq_along(z_final)) {
  z_SPX <- left_join(z_final, SPX_dt, by = c("date" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

z_SPX$return <- round(z_SPX$return, digits = 4)
z_SPX <- na.omit(z_SPX) #skip N/A values

z_SPX$correlation <- round(z_SPX$mean * z_SPX$return, digits = 6)
z_SPX$valid <- z_SPX$correlation > 0 

#download Nasdaq
NSQ <- BatchGetSymbols(
  tickers = c("^IXIC"),
  first.date = z_final$date[1] - 1,
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
for (i in seq_along(z_final)) {
  z_NSQ <- left_join(z_final, NSQ_dt, by = c("date" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

z_NSQ$return <- round(z_NSQ$return, digits = 4)
z_NSQ <- na.omit(z_NSQ) #skip N/A values

z_NSQ$correlation <- round(z_NSQ$mean * z_NSQ$return, digits = 6)
z_NSQ$valid <- z_NSQ$correlation > 0 

#download DOW
DOW <- BatchGetSymbols(
  tickers = c("^DJI"),
  first.date = z_final$date[1] - 1,
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
for (i in seq_along(z_final)) {
  z_DOW <- left_join(z_final, DOW_dt, by = c("date" = "ref.date")) %>% 
    rename(return = ret.closing.prices)  }

z_DOW$return <- round(z_DOW$return, digits = 4)
z_DOW <- na.omit(z_DOW) #skip N/A values

z_DOW$correlation <- round(z_DOW$mean * z_DOW$return, digits = 6)
z_DOW$valid <- z_DOW$correlation > 0 

print("SPX")
sum(z_SPX$correlation > 0, na.rm=TRUE)
sum(z_SPX$correlation < 0, na.rm=TRUE)
sum(z_SPX$correlation == 0, na.rm=TRUE)
print("NSQ")
sum(z_NSQ$correlation > 0, na.rm=TRUE)
sum(z_NSQ$correlation < 0, na.rm=TRUE)
sum(z_NSQ$correlation == 0, na.rm=TRUE)
print("DOW")
sum(z_DOW$correlation > 0, na.rm=TRUE)
sum(z_DOW$correlation < 0, na.rm=TRUE)
sum(z_DOW$correlation == 0, na.rm=TRUE)


