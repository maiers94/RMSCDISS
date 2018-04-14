# Importing Algorithm
#
#   a <- readxl::read_excel("FTSE100.xlsx")
#   for(i in 2:dim(a)[2]){
#     nam <- paste("STOCK", i-1, sep = "_")
#     assign(nam,a[2:4697,i])
#   }
#

####### Backtesting - Campbell, Hardy
#70/30 split
#Variance of Returns - Sharpe Ratio
#Drawdown - max fall acceptable
#Sortino Ratio / CALMAR ratio (included here)
#manipulation proof performance measure
#significance tests (?)
#47top

#'@export

pickup <- function(data="data/FTSE100.Rdata"){
  load(data)
  return(NULL)
}

#'@export
price2ret <- function(data,sort=FALSE){
  it <- ncol(data)
  output <- matrix(nrow=nrow(data),ncol=it)
  for(i in 1:it){
    if(sort==FALSE){
      base <- as.numeric(data[1,i])
    }
    else{
      base<-1
    }
    output[,i] <- as.numeric(as.vector(data[,i]))/base
  }
  return(output)
}



#'@export

listgen.allc <- function(b){
  v1 <- vector(length=choose(ncol(b),2))
  v2 <- vector(length=choose(ncol(b),2))
  nos <- ncol(b)
  index <- 0
  for(i in 1:(nos-1)){
    for(j in (1+i):nos){
      index <- index + 1
      v1[index] <- i
      v2[index] <- j
    }
  }
  l <- list(v1,v2)
  return(l)
}
