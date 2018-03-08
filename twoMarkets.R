library(quantmod)
getSymbols("TWMJF",source ="google")
plot(TWMJF)
newTWM <- TWMJF
colnames(TWMJF) <- colnames(TWMJF)
colnames(newTWM) <- substring(text = colnames(TWMJF), first = 7)
times <- index(newTWM)
getSymbols("WEED.TO",source = "google")




getSymbols("ABX",source = "google")
getSymbols("ABX.TO",source = "yahoo")