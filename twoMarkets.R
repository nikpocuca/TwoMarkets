library(quantmod)

getAndParse <- function(string,src){
  getSymbols(string,source = src)
  newStock <- get(string)
  colnames(newStock) <- substring(text = colnames(get(string)), first = (nchar(string) +2))
  newData <- as.data.frame(newStock)
  Dates <- rownames(newData)
  return(cbind(newData,Dates))
}
# use getAndParse from now on instead of getSymbols
newTWM <- getAndParse("TWMJF","google")

# function for telling whether your dates are on weekends or weekdays. 
# returns false if on weekend, true if weekday. 

checkDOW <- function(stringOYear,stringOfHoliday){
  date2Check <- paste(stringOYear,stringOfHoliday,sep = "-")
  day <- as.POSIXlt(as.Date(date2Check))$wday
  check <- day %in% c(1,2,3,4,5)
  return(list(day,check))
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


# Master list creation
masterList <- createMasterList(canadianDates = canadianStringDates,
                 americanDates = americanStringDates,
                 StockStrings = stkString)


# Now to build extraction of dates and weeks. 


extractDays <- function(n,day,year,type,stockData){
  # n is for number of days forward and backword from the day of interest. 
  dowCheck <- checkDOW(year, day)
  if (dowCheck[[2]][1] == TRUE) {
    if (type == "CAD") {
    primary <- unique(stockData$CanData)
    dates <- rownames(primary)
    rowIndex <- match(paste(year,day,sep = "-"),dates)
    lower <- rowIndex - n
    upper <- rowIndex + n
    primary <- primary[lower:upper,]
    secondary <- unique(stockData$AmerData)
    dates <- rownames(secondary)
    rowIndex <- match(paste(year,day,sep = "-"),dates)
    secondary <- secondary[lower:upper,]
    return(list(primary = primary, secondary = secondary))
    }
    else{ 
      print("Hit")
      primary <- unique(stockData$AmerData)
      dates <- rownames(primary)
      rowIndex <- match(paste(year,day,sep = "-"),dates)
      lower <- rowIndex - n
      upper <- rowIndex + n
      primary <- primary[lower:upper,]
      secondary <- unique(stockData$CanData)
      dates <- rownames(secondary)
      rowIndex <- match(paste(year,day,sep = "-"),dates)
      secondary <- secondary[lower:upper,]
      return(list(primary = primary, secondary = secondary))
    }
  }
  else 
    return("Holiday falls on a weekend.")
}

testDay <- civic
testYear <- "2016"

# Depending on what you put as type you can either get the american data as primary, or canadian data as secondary. 
# Type should be what side your primiary target should be, Ie. The stock should be open the day you targeted. 
test <- extractDays(10,testDay,testYear,"AMER",masterList$`Berrick Gold Group`)










