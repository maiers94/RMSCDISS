#'@export
trade.pairs <- function(data,testfun,datalist="default",top=10,tradestart=2872,normalise=TRUE,...){
  #first get metric and find pairs
  #find pairs
  data <- price2ret(data,sort=TRUE)
  ndata <- price2ret(data)
  lastday <- length(data[,1])
  if(datalist == "default"){
    datalist <- listgen.allc(data)
  }
  if(normalise==TRUE){
    pairslist <- findpairs(datalist,ndata[1:(tradestart-1),],testfun,...)
  }
  else{
    pairslist <- findpairs(datalist,data[1:(tradestart-1),],testfun,...)
  }


  #Find standard deviations for the top pairs:
  std <- vector(length=top)
  for(i in 1:top){
    sec1 <- ndata[1:(tradestart-1),pairslist[i,1]]
    sec2 <- ndata[1:(tradestart-1),pairslist[i,2]]
    diff <- sec1-sec2
    plot(diff)
    std[i] <- 2*sd(diff)
  }
  print(std)

  #Trade securities for remainder of period:
  #create position matrix
  pos <- matrix(ncol=top,nrow=(lastday-tradestart+2))
  pos[1,] <- rep(0,top)
  for(i in tradestart:lastday){
    #for each day
    daily <- ndata[i,]
    prevdailypos <- pos[(i-tradestart+1),]
    dailypos <- rep(NA,length(prevdailypos))
    for(k in 1:top){
      #for each security on the day
      dev <- std[k]
      pairdiff <- daily[pairslist[k,1]]-daily[pairslist[k,2]]

      if(prevdailypos[k] != 0){
        #if poition was open
        prevpairdiff <- prevdaily[pairslist[k,1]]-prevdaily[pairslist[k,2]]
        if(sign(pairdiff) == sign(prevpairdiff)){
          #keep position open
          dailypos[k] <- prevdailypos[k]
        }
        else{
          #close position
          dailypos[k] <- 0
        }
      }
      else if(prevdailypos[k]==0){
        #if position was closed
        if(abs(pairdiff) <= dev){
          #keep position closed
          dailypos[k] <- 0
        }
        else{
          #open position
          #-1 - if sec2 > sec1
          #1 - if sec 1 > sec 2
          dailypos[k] <- sign(pairdiff)
        }
      }
    }
    prevdaily <- daily
    pos[(i-tradestart+2),] <- dailypos
  }
  #ensure all positions are closed on the last day
  pos[(lastday-tradestart+2),] <- rep(0,top)
  return(pos)
}
