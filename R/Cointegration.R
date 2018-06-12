# DATA: 2000-2010  <->  a[1:2871,]
# DATA: 2000-2010  <->  a[2872:4696,]



#'@export
#'@import progress
findpairs <- function(datalist,data,testfun,silent=FALSE,...){
  decrease <- FALSE
  if(identical(testfun,johansen)){
    decrease <- TRUE
  }
  results <- rep(0,length(datalist[[1]]))
  #john <- rep(0,length(datalist[[1]]))
  #cvaln <- rep(0,length(datalist[[1]]))
  if(silent==FALSE){
    pb <- progress_bar$new(total = length(datalist[[1]]))
    pb$tick()
  }

  for(i in 1:length(datalist[[1]])){
    sec1 <- data[,datalist[[1]][i]]
    sec2 <- data[,datalist[[2]][i]]
    results[i] <- testfun(sec1,sec2,...)#[[1]]
    #john[i] <- testfun(sec1,sec2,...)[[2]]
    #cvaln[i] <- testfun(sec1,sec2,...)[[3]]
    if(silent==FALSE){
      pb$tick()
    }
  }
  res <- data.frame(
    Sec1 = datalist[[1]],
    Sec2 = datalist[[2]],
    val = results#,
    #joh = john,
    #cval = cvaln
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
#'@import stats
adf <- function(sec1,sec2,...){
  mod <- lm(sec1~sec2)
  xm <- adf.test(mod$residuals)
  metric <- as.numeric(xm$statistic)

  return(metric)
}



#'@export
#'@import urca

johansen <- function(sec1,sec2,...){
  mod <- data.frame(sec1, sec2)
  xm <- ca.jo(mod, type="trace", K=2, ecdet="none", spec="longrun",...)
  metric <- as.numeric(xm@teststat[length(xm@teststat)]-xm@cval[dim(xm@cval)[1],dim(xm@cval)[2]])
  #metric2 <- as.numeric(xm@teststat[length(xm@teststat)])
  #metric3 <- as.numeric(xm@cval[dim(xm@cval)[1],dim(xm@cval)[2]])
  #return(list(metric,metric2,metric3))
  return(metric)
}
