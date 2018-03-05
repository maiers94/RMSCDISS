#'@export
list.testrun <- function(datalist,data,testfun,decrease=FALSE,...){
  results <- rep(0,length(datalist))
  for(i in 1:length(datalist[[1]])){
    sec1 <- data[,datalist[[1]][i]]
    sec2 <- data[,datalist[[2]][i]]
    results[i] <- testfun(sec1,sec2,...)
    print((i/length(datalist[[1]])))
  }
  res <- data.frame(
    Sec1 = datalist[[1]],
    Sec2 = datalist[[2]],
    val = results
  )
  ressort <- res[with(res, order(val,decreasing = decrease)),]
  return(ressort)
}


#'@export
euclidean <- function(sec1,sec2){
  sumsq <- 0
  for(i in 1:length(sec1)){
    diffsq <- (sec1[i]-sec2[i])**2
    sumsq <- sumsq + diffsq
  }
  return(sumsq)
}

#'@export
#'@import tseries
adf <- function(sec1,sec2,...){
  mod <- lm(sec1~sec2)
  xm <- adf.test(mod$residuals)
  metric <- as.numeric(xm$statistic)

  return(metric)
}



#'@export
#'@import urca

johansen <- function(sec1,sec2){

  return(metric)
}
