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

#American Dates 
civicHoliday <- "2015-08-03" 
americanStringDates <- c("2015-08-03")

#Canadian Dates  
canadaDay <- "2017-07-03"  # canadian side closed, american open.
canadianStringDates <- c("2017-07-03")

#Stock Strings
stkString <- c("CanopyGrowth,WEED.TO,TWMJF","Berrick Gold Group,ABX.TO,ABX" ) 

# MasterList Function 
createMasterList <- function(canadianDates,americanDates,StockStrings) {
  # Generate Stock Name and Ticker Info List.
  #need to figure this part out 
  TickerListInfo <- parseStkString(stockStrings = StockStrings)
  MasterDataList <- list(tickerMap = TickerListInfo)
  for (i in 1:length(TickerListInfo$CmpnyName)) {
  MasterDataList[[TickerListInfo[i,1]]] <- genStockObj(TickerListInfo[i,1],TickerListInfo[i,2],TickerListInfo[i,3])
  }
  # now to add in the function that extracts relevant data points. 
  
  return(MasterDataList)
}

#Parsing stock string 
parseStkString <- function(stockStrings) {
  StkStringFrame <- data.frame(1,2,3)
  colnames(StkStringFrame) <- c("CmpnyName","CanadianTicker","AmericanTicker")
  for (stockString in stockStrings) {
  unlistedString <- unlist(strsplit(stockString,","))
  StkStringFrame <- rbind(StkStringFrame,unlistedString)
  }
  StkStringFrame <- StkStringFrame[-1,]
  return(StkStringFrame)
}

#Generate list object for stock 
genStockObj <- function(cName,cStkTicker,aStkTicker){
  return(list(
    Company = cName,
    CanData = getAndParse(cStkTicker,"google"),
    AmerData = getAndParse(aStkTicker,"google")
  ))
}

#genTest 
genStockObj("CanopyGrowth","WEED.TO","TWMJF")

test <- createMasterList(canadianDates = canadianStringDates,
                 americanDates = americanStringDates,
                 StockStrings = stkString)



