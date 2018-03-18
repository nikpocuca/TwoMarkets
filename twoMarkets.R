library(quantmod)

getAndParse <- function(string,src){
  getSymbols(string,source = src)
  newStock <- get(string)
  colnames(newStock) <- substring(text = colnames(get(string)), first = (nchar(string) +2))
  return(as.data.frame(newStock))
}
# use getAndParse from now on instead of getSymbols
newTWM <- getAndParse("TWMJF","google")

# function for telling whether your dates are on weekends or weekdays. 
# returns false if on weekend, true if weekday. 

checkDOW <- function(stringOYear,stringOfHoliday){
  date2Check <- paste(stringOYear,stringOfHoliday,sep = "-")
  day <- as.POSIXlt(as.Date(date2Check))$wday
  print(day)
  check <- day %in% c(1,2,3,4,5)
  return(check)
}

sampleDates <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")
canadaDay <- "07-03"
civic <- "08-03"

checkDOW(sampleDates,canadaDay)
checkDOW(sampleDates,civic)

# next function to code is one that extracts previous day date. 



plot(TWMJF)
newTWM <- TWMJF
colnames(TWMJF) <- colnames(TWMJF)
colnames(newTWM) <- substring(text = colnames(TWMJF), first = 7)
times <- index(newTWM)




getSymbols("WEED.TO",source = "google")

canadaDay <- "2017-07-03"  # canadian side closed, american open.
civicHoliday <- "2015-08-03" #canadian closed, american open. 

getSymbols("ABX",source = "google")
getSymbols("ABX.TO",source = "yahoo")



as.POSIXlt("2018-03-17")$wday


