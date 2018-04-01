#'@export
trade.pairs <- function(data,testfun,scale=1,datalist="default",top=10,tradestart=3393,normalise=TRUE,silent=FALSE,min = 1,vary.param=FALSE,...){
  #first get metric and find pairs
  #find pairs
  data <- price2ret(data,sort=TRUE)
  ndata <- price2ret(data)
  lastday <- length(data[,1])
  if(datalist[[1]][1] == "default"){
    datalist <- listgen.allc(data)
  }
  if(vary.param==FALSE){
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
  }
  else{
    if(normalise==TRUE){
      pairslist <- findpairs(datalist,ndata,testfun,silent=silent,...)
    }
    else{
      pairslist <- findpairs(datalist,data,testfun,silent=silent,...)
    }
    #Find standard deviations for the top pairs:
    std <- vector(length=top)
    for(i in 1:top){
      sec1 <- ndata[,pairslist[i,1]]
      sec2 <- ndata[,pairslist[i,2]]
      diff <- sec1-sec2

      std[i] <- scale*sd(diff)
    }
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
    for(k in min:top){
      #for each security on the day
      dev <- std[k]
      pairdiff <- daily[pairslist[k,1]]-daily[pairslist[k,2]]

      if(prevdailypos[k] != 0){
        #if position was open
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

  returns <- returncalc(data[tradestart:lastday,],pairslist[min:top,],pos,min)
  #print(returns)
  return(list(pos,returns))
}

#'@export
output.pairs <- function(data,testfun,datalist="default",top=10,tradestart=3393,normalise=TRUE,silent=FALSE,...){
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
  return(pairslist[1:top,])
}

returncalc <- function(data,datalist,pos,min){
  traderet <- matrix(data = 0, nrow=nrow(data), ncol = nrow(datalist)+min-1)
  prevpos <- pos[1,]

  #vector of zeros, to store open positions
  open <- prevpos
  for(i in 1:length(data[,1])){
    curpos <- pos[i+1,]
    for(k in min:length(curpos)){

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
          shortsec <- data[,datalist[k-min+1,1]]
          longsec <- data[,datalist[k-min+1,2]]
        }
        else if(dir == -1){
          shortsec <- data[,datalist[k-min+1,2]]
          longsec <- data[,datalist[k-min+1,1]]
        }
        shortlogret <- log(shortsec[opdate]) - log(shortsec[i])

        longlogret <- log(longsec[i]) - log(longsec[opdate])

        traderet[i,k] <- longlogret + shortlogret

        open[k] <- 0
      }
    }

    prevpos <- curpos
  }
  return(traderet)
}


#'@export
vary.param <- function(j,data,testfun=euclidian,reps=50,jump=1/25,start=1){
  pb <- progress_bar$new(total = reps)
  param <- vector(length=reps)
  posi <- trade.pairs(data,testfun,scale=(jump+start),silent=TRUE,top = j,min = j,tradestart=1)
  y <- compound.returns(posi,j)
  param[1]<-y
  plot(1,y,xlim=c(1,reps),ylim=c(-1,2),pch=16,xlab=NA, ylab=NA)
  lines(x=c(0,reps),y=c(0,0),col="red")
  not <- vector(length = reps)
  not[1] <- sum(posi[[2]][,j] !=0)
  pb$tick()

  for(i in 2:reps){
    k <- (i*jump)+start
    posi <- trade.pairs(data,testfun,scale=k,silent=TRUE,top = j,min = j,tradestart=1)
    y <- compound.returns(posi,j)
    param[i] <- y
    points(i,y,pch=16)
    not[i] <- sum(posi[[2]][,j] !=0)
    pb$tick()
  }
  par(new = T)
  plot(x = seq(1,reps), y = not, type = "l", axes=F, xlab=NA, ylab=NA)
  axis(side = 4)

  return(param)
}



#'@export
compound.returns <- function(mat,sec){
  returns <- mat[[2]][,sec]
  returns <- returns[returns != 0]
  if(sum(returns) == 0){
    return(0)
  }
  compound <- 1
  for(i in 1:length(returns)){
    compound <- compound*(1+returns[i])
  }

  return(compound - 1)
}

#'@export
summary.returns <- function(mat,interest){
  #average daily returns on open positions
  n <- ncol(mat[[1]])
  rets <- vector(length=n)
  for(i in 1:n){
    rets[i] <- compound.returns.interest(mat,i,interest)

  }
  avg <- sum(rets)/n
  print("AVERAGE RETURN:")
  print(avg)
  #sd of retruns
  print("STANDARD DEVIATION:")
  print(sd(rets))
  print("SHARPE RATIO:")
  print((avg-0.005)/sd(rets))
  hist(rets)
  return(rets)
}



#'@export
compare.lists <- function(list1,list2){
  matches <- list()
  counter <- 0
  for(i in 1:length(list1[,1])){
    for(j in 1:length(list2[,1])){
      if(list1[i,1]==list2[j,1] && list1[i,2]==list2[j,2]){
        counter <- counter + 1
        matches[[counter]] <- c(list1[i,1],list1[i,2])
      }
    }
  }
  matchesv <- matrix(nrow=length(matches),ncol=2)
  for(i in 1:nrow(matchesv)){
    matchesv[i,] <- matches[[i]]
  }
  matchesl <- list(matchesv[,1],matchesv[,2])
  return(matchesl)
}

#'@export
compound.returns.interest <- function(mat,sec,int){
  n <- length(mat[[2]][,1])
  k <- sec
  rets <- 1
  curpos <- 0
  for(i in 1:n){
    if(curpos != mat[[1]][(i+1),k]){
      if(mat[[1]][(i+1),k] == 0){
        traderet <- mat[[2]][(i),k]
        rets <- rets*(1 + traderet)

      }
      curpos <- mat[[1]][(i+1),k]

    }
    else if(curpos == 0){
      rets <- rets*((int[i]/100) + 1) ^ (1/250)


    }


  }

  return(rets - 1)
}

#'@export
optimise.param <- function(tops=25,data,reps,start,jump){
  param <- vector(length=tops)
  for(i in 1:tops){
    param[i] <- max(vary.param(i,data[1:3392,],testfun=euclidian,reps = reps, start= start,jump=jump))
  }
  return(param)
}


#####################################################################

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
