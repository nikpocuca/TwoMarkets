library(quantmod)

getAndParse <- function(string,src){
  getSymbols(string,source = src)
  newStock <- get(string)
  colnames(newStock) <- substring(text = colnames(get(string)), first = (nchar(string) +2))
  return(as.data.frame(newStock))
}
# use getAndParse from now on instead of getSymbols
newTWM <- getAndParse("TWMJF","google")


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


