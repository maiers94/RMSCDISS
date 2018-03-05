#'@export
list.cointegration <- function(datalist,data,testfun){

  results <- rep(0,length(datalist))
  for(i in 1:length(datalist[[1]])){
    sec1 <- data[,datalist[[1]][i]]
    sec2 <- data[,datalist[[2]][i]]
    results[i] <- testfun(sec1,sec2)
  }
  return(list(datalist[[1]],datalist[[2]],results))
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


johansen <- function(sec1,sec2){

  return(metric)
}
