#'@export
trade.pairs <- function(data,testfun,scale=1,datalist="default",top=10,tradestart=2872,normalise=TRUE,silent=FALSE,...){
  #first get metric and find pairs
  #find pairs
  data <- price2ret(data,sort=TRUE)
  ndata <- price2ret(data)
  lastday <- length(data[,1])
  if(datalist == "default"){
    datalist <- listgen.allc(data)
  }
  if(normalise==TRUE){
    pairslist <- findpairs(datalist,ndata[1:(tradestart-1),],testfun,silent=silent,...)
  }
  else{
    pairslist <- findpairs(datalist,data[1:(tradestart-1),],testfun,silent=silent,...)
  }


  #Find standard deviations for the top pairs:
  std <- vector(length=top)
  for(i in 1:top){
    sec1 <- ndata[1:(tradestart-1),pairslist[i,1]]
    sec2 <- ndata[1:(tradestart-1),pairslist[i,2]]
    diff <- sec1-sec2

    std[i] <- scale*sd(diff)
  }

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
  returns <- returncalc(data[tradestart:lastday,],pairslist[1:top,],pos)

  return(list(pos,returns))
}

#'@export
output.pairs <- function(data,testfun,scale=1,datalist="default",top=10,tradestart=2872,normalise=TRUE,silent=FALSE,...){
  #find pairs
  data <- price2ret(data,sort=TRUE)
  ndata <- price2ret(data)
  lastday <- length(data[,1])
  if(datalist == "default"){
    datalist <- listgen.allc(data)
  }
  if(normalise==TRUE){
    pairslist <- findpairs(datalist,ndata[1:(tradestart-1),],testfun,silent=silent,...)
  }
  else{
    pairslist <- findpairs(datalist,data[1:(tradestart-1),],testfun,silent=silent,...)
  }
  return(pairslist)
}

returncalc <- function(data,datalist,pos){
  traderet <- matrix(data = 0, nrow=nrow(data), ncol = nrow(datalist))
  prevpos <- pos[1,]
  #vector of zeros, to store open positions
  open <- prevpos
  for(i in 1:length(data[,1])){
    curpos <- pos[i+1,]
    for(k in 1:length(curpos)){
      if(curpos[k] != 0 && open[k] == 0){
        #open new position if pos changes and currently none open
        open[k] <- sign(curpos[k])*i #save direction and day of trade
      }

      if(curpos[k] == 0 && open[k] != 0){
        #close position if need be and compute returns
        opdate <- abs(open[k])
        dir <- sign(open[k])
        if(dir == 1){
          #since sec1 > sec2, short sec 1
          shortsec <- data[,datalist[k,1]]
          longsec <- data[,datalist[k,2]]
        }
        else if(dir == -1){
          shortsec <- data[,datalist[k,2]]
          longsec <- data[,datalist[k,1]]
        }
        shortlogret <- log(shortsec[opdate]) - log(shortsec[k])
        longlogret <- log(longsec[k]) - log(longsec[opdate])
        traderet[i,k] <- longlogret + shortlogret
        open[k] <- 0
      }
    }

    prevpos <- curpos
  }
  return(traderet)
}


#'@export
vary.param <- function(j,data,testfun,reps=50,jump=1/25){
  pb <- progress_bar$new(total = reps)
  posi <- trade.pairs(data,testfun,scale=jump,silent=TRUE)
  plot(1,compound.returns(posi,j),xlim=c(1,reps),ylim=c(-1,5),pch=16,xlab=NA, ylab=NA)
  lines(x=c(0,reps),y=c(0,0),col="red")
  not <- vector(length = reps)
  not[1] <- sum(posi[[2]][,j] !=0)
  pb$tick()

  for(i in 2:reps){
    k <- i*jump
    posi <- trade.pairs(data,testfun,scale=k,silent=TRUE)
    points(i,compound.returns(posi,j),pch=16)
    not[i] <- sum(posi[[2]][,j] !=0)
    pb$tick()
  }
  par(new = T)
  plot(x = seq(1,reps), y = not, type = "l", axes=F, xlab=NA, ylab=NA)
  axis(side = 4)

  return(NULL)
}



#'@export
compound.returns <- function(mat,sec){
  returns <- mat [[2]][,sec]
  returns <- returns[returns != 0]
  compound <- 1
  for(i in 1:length(returns)){
    compound <- compound*(1+returns[i])
  }
  return(compound - 1)
}




compound.returns.old <- function(mat,sec){
  trades <- mat[[1]][,sec]
  returns <- mat [[2]][,sec]
  returns <- returns[returns != 0]
  times <- vector(length=sum(returns!=0))
  positions <- vector(length=sum(returns!=0))
  count <- 0
  index <- 1
  for(i in 2:length(trades)){
    if(trades[i] == 0 && trades[i-1] != 0){
      times[index] <- count
      positions[index] <- trades[i-1]
      index <- index + 1
      count <- 0
    }
    else{
      count <- count + 1
    }
  }
  compound <- 1
  for(i in 1:length(times)){
    if(positions[i] != 0){
      compound <- compound*(1+returns[i])
    }
  }
  return(compound - 1)
}
