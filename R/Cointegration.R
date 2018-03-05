list.cointegration <- function(datalist,data,testfun){
  index <- 0
  for(i in 1:length(datalist)){
    results <- rep(0,choose(length(datalist),2))
    sec1 <- datalist[i]
    for(j in 1:(length(datalist)-i)){
      index <- index + 1
      sec2 <- datalist[i+j]
      results[index] <- testfun()
    }
  }
}
