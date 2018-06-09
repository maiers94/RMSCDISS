#'@export
trade.pairs <- function(data,testfun,scale=1,datalist="default",top=10,tradestart=3393,normalise=TRUE,silent=FALSE,min = 1,vary.param=FALSE,...){
  #setup

  if(length(scale)==1){
    scale <- rep(scale,top)
  }

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

      std[i] <- scale[i]*sd(diff)
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

      std[i] <- scale[i]*sd(diff)
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
        if(sign(pairdiff) == prevdailypos[k]){
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
          dailypos[k] <- prevdailypos[k]
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
vary.param <- function(j,data,testfun=euclidean,reps=50,jump=1/25,start=1,pre=TRUE){
  pb <- progress_bar$new(total = reps)
  param <- vector(length=reps)
  if(pre==TRUE){
    posi <- trade.pairs(data,testfun,scale=(jump+start),silent=TRUE,top = j,min = j,tradestart=1,vary.param = TRUE)
  }
  else{
    posi <- trade.pairs(data,testfun,scale=(jump+start),silent=TRUE,top = j,min = j)
  }
  y <- compound.returns(posi,j)
  param[1]<-y
  plot(1,y,xlim=c(1,reps),ylim=c(-1,6),pch=16,xlab=NA, ylab=NA)
  lines(x=c(0,reps),y=c(0,0),col="red")
  not <- vector(length = reps)
  not[1] <- sum(posi[[2]][,j] !=0)
  pb$tick()

  for(i in 2:reps){
    k <- (i*jump)+start
    if(pre==TRUE){
      posi <- trade.pairs(data,testfun,scale=k,silent=TRUE,top = j,min = j,tradestart=1,vary.param = TRUE)
    }
    else{
      posi <- trade.pairs(data,testfun,scale=k,silent=TRUE,top = j,min = j)
    }
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
#'@import tseries
#'@import graphics
summarise <- function(mat,interest,tc = 0, tradedays = 261){
  #average daily returns on open positions
  n <- ncol(mat[[1]])
  rets <- vector(length=n)
  theta <- vector(length=n)
  rho <- 2
  for(i in 1:n){
    temp <- compound.returns.interest(mat,i,interest,tc,tradedays)
    rets[i] <- temp[[1]]

    #######Manipulation Proof Measure########
    k <- length(temp[[2]])
    s <- vector(length=k)
    for(j in 1:k){
      s[j] <- (1+temp[[2]][j])/((1+interest[j]/100) ^ (1/tradedays))
      s[j] <- s[j]^(1-rho)
      #if(is.na(s[j])==T){
      #  print(j)
      #  print(i)
      #  print(temp[[2]][j])
      #  print(interest[j])
      #}
    }
    sp <- log(sum(s)/k)
    theta[i] <- (1/((1-rho)*(1/tradedays))) * sp
  }

  theta <- mean(theta)





  #################################
  avg <- sum(rets)/n
  print("AVERAGE RETURN: (%)")
  print(round(avg*100,4))
  #sd of retruns
  print("STANDARD DEVIATION: (%)")
  print(round(sd(rets)*100,4))
  print("SHARPE RATIO:")
  print(round((avg)/sd(rets),4))
  print("MAX DRAWDOWN: (%)")
  print(round(maxdrawdown(rets)$maxdrawdown*100,4))
  print("CALMAR(whole period):")
  print(round(avg/maxdrawdown(rets)$maxdrawdown,4))
  #print("SORTINO:")
  #print((avg)/sd(rets[rets<0]))
  print("AVG. MPPF (annualised): (%)")
  print(round(theta*100,4))
  print("#################")
  print("No of Trades:")
  print(sum(mat[[2]]!=0))
  print("#################")
  print("GATEV RETURNS (portfolio value weighterd average,compounded):")
  print(round(gatevreturns(mat)*100,4))
  #hist(rets)
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
compound.returns.interest <- function(mat,sec,int,tc,tradedays = 261){
  tc <- tc / 10000
  n <- length(mat[[2]][,1])
  k <- sec
  rets <- 1
  curpos <- 0
  cont <- vector(length = n)
  lag <- 0
  for(i in 1:n){
    ############# - for total returns - #############
    if(curpos != mat[[1]][(i+1),k]){
      if(mat[[1]][(i+1),k] == 0){
        traderet <- mat[[2]][(i),k]
        rets <- rets*(1 + traderet - 2*tc)


      }
      curpos <- mat[[1]][(i+1),k]
    }
    else if(curpos == 0){
      rets <- (rets*(int[i]/(tradedays*100)+1))

    }
    ############# - for cont. returns - ################
    if(mat[[1]][(i+1),k] == 0){
      cont[i] <- (int[i]/(tradedays*100))
      if(lag != 0){
        cont[(i-lag):(i-1)] <- rep((((mat[[2]][i,k] - 2*tc + 1) ^ (1/lag)) - 1), lag)

        lag <- 0
      }
    }
    else if(mat[[1]][(i+1),k] != 0){
      lag <- lag + 1
    }

  }

  return(list((rets - 1),cont))
}

#'@export
gatevreturns <- function(mat){
  #tc <- tc / 10000
  n <- length(mat[[2]][,1])
  k <- length(mat[[2]][1,])
  w <- rep(1,k)
  rp <- vector(length = n)
  rp[1] <- sum(mat[[2]][1,])/k
  for(t in 2:n){
    rpcur <- rep(0,k)
    for(i in 1:k){
      w[i] <- w[i]*(1 + mat[[2]][(t-1),i])
      rpcur[i] <- w[i]*mat[[2]][t,i]
    }
    rp[t] <- sum(rpcur)/sum(w)
  }
  rp <- rp + 1
  rp <- prod(rp) - 1
  return(rp)
}

#'@export
optimise.param <- function(tops=25,data,reps,start,jump){
  param <- vector(length=tops)
  for(i in 1:tops){
    temp <- vary.param(i,data[1:3392,],testfun=euclidean,reps = reps, start= start,jump=jump)
    param[i] <- (which.max(temp)*jump)+start
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
