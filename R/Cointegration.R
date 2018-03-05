#'@export
list.testrun <- function(datalist,data,testfun,min=TRUE,top=1){

  results <- rep(0,length(datalist))
  for(i in 1:length(datalist[[1]])){
    sec1 <- data[,datalist[[1]][i]]
    sec2 <- data[,datalist[[2]][i]]
    results[i] <- testfun(sec1,sec2)
  }
  res <- data.frame(
    Sec1 = datalist[[1]],
    Sec2 = datalist[[2]],
    val = results
  )
  ressort <- res[with(res, order(val)),]
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

johansen <- function(sec1,sec2){

  return(metric)
}
