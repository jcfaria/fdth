plot.fdt.multiple <- function (x,
                               type=c('fh', 'fp', 
                                      'rfh', 'rfp', 'rfph', 'rfpp',
                                      'd', 'cdh', 'cdp', 
                                      'cfh', 'cfp', 'cfph', 'cfpp'),
                               v=FALSE,
                               v.round=2,
                               v.pos=3,
                               xlab='Class limits',
                               xlas=0,
                               ylab=NULL,
                               col='gray',
                               xlim=NULL,
                               ylim=NULL,
                               main=NULL,
                               main.vars=TRUE,
                               x.round=2,
                               grouped=FALSE,
                               args.legend=NULL,...)
{

  typearg <- match.arg(type)
  if(!grouped){  
    is.whole.number <- function (x,
                                 tol=.Machine$double.eps^0.5)
      abs(x - round(x)) < tol

  old.mf  <- par("mfrow")
  old.oma <- par("oma")
  old.mar <- par("mar")

  on.exit(par(mfrow=old.mf,
              oma=old.oma,
              mar=old.mar))

  mf <- old.mf

  if (length(mf) == 0)
    mf <- c(1, 1)

  if ((n <- length(x)) > 1 & max(mf) == 1)
    mf <- if   (n <= 2)  c(2, 1)
      else  if (n <= 4)  c(2, 2)
      else  if (n <= 6)  c(2, 3)
      else  if (n <= 9)  c(3, 3)
      else  if (n <= 12) c(3, 4)
      else  if (n <= 16) c(4, 4)
      else               c(4, 5)

      par(mfrow=mf)
      nplot.device <- prod(mf)

      if (!is.null(main))
        main <- rep(main, length(x))
      else if (main.vars)
        main <- names(x)[names(x)!='call']

      xx <- x[names(x)!='call']
      i <- 0

      repeat {
        if ((i != 0) & is.whole.number(i/nplot.device)) {
          dev.new()
          par(mfrow=mf)
        }

        i <- i + 1

        plot.fdt.default(xx[[i]],
                         type=type,
                         v=v,
                         v.round=v.round,
                         v.pos=v.pos,
                         xlab=xlab,
                         xlas=xlas,
                         ylab=ylab,
                         col=col,
                         xlim=xlim,
                         ylim=ylim,
                         main=main[i],
                         x.round=x.round, ...)

        if (i == length(xx))
          break
      } 
  } else if(typearg == 'fp' | typearg == 'rfp' | typearg == 'rfpp' | typearg == 'cdp' | typearg == 'cfp' | typearg == 'cfpp'){
    DFagain <- eval(getCall(x)$x)
    byagain <- eval(getCall(x)$by)
    varis <- sapply(DFagain,is.numeric)
    varisnum <- names(varis)[varis]
    namesx <- names(x)[names(x)!='call']
    namesvar <- gsub('([\\w\\W]+)\\.\\b',
                     '',
                     namesx,
                     perl=TRUE)

    posvaris <- list()
    tabsc <- list()
    aux1 <- list()
    aux2 <- list()
    auxk <- list()
    for(i in 1:length(varisnum)){
      posvaris[[i]] <- which(namesvar==varisnum[i])
      tabsc[[i]] <- x[posvaris[[i]]]
      aux1[[i]] <- lapply(tabsc[[i]],function(x)x$breaks)
      aux2[[i]] <- do.call('rbind',aux1[[i]])
      auxk[[i]] <- lapply(tabsc[[i]],function(x)x$table[,5])
    }
    liis <- lapply(aux2,function(x)min(x[,1]))
    lsss <- lapply(aux2,function(x)max(x[,2])) 
    auxk1 <- lapply(auxk,unlist)
    k <- lapply(auxk1,function(x)nclass.Sturges(1:max(x)))#pegando o fator com maior observação, pois o número de classes será maior e irá englobar os demais fatores caso tenha uma amostra menor!
    hh <- mapply(function(x,y,z) (y - x)/z,
                 liis,
                 lsss,
                 k)

    ## O PRÓXIMO PASSO É FAZER UMA TABELA PARA CADA FATOR E PARA CADA VARIÁVEL
    factors <- levels(DFagain[[byagain]])
    subgroups <- list()
    for(i in 1:length(factors)){
      subgroups[[i]] <- subset(DFagain,
                               eval(parse(text=byagain))%in%factors[i])
    }

    tabsfactors <- rep(list(rep(list(NA),length(varisnum))),length(factors))
    for(i in 1:length(factors)){
      for(j in 1:length(varisnum)){
        ifelse(length(varisnum)==1,
               groups <- subgroups[[i]][,((ncol(DFagain)-length(varisnum))+1):ncol(DFagain)],
               groups <- subgroups[[i]][,((ncol(DFagain)-length(varisnum))+1):ncol(DFagain)][,j]) 
        tabsfactors[[i]][[j]] <- fdt(groups,
                                     start=liis[[j]],
                                     end=lsss[[j]],
                                     h=hh[j])
      }
    }

    mids <- mapply(function(x,y,z) seq(x+z/2, y-z/2, z),
                   liis,
                   lsss,
                   as.list(hh))

    typearg <- match.arg(type)

    old.mf  <- par("mfrow")
    old.oma <- par("oma")
    old.mar <- par("mar")

    on.exit(par(mfrow=old.mf,
                oma=old.oma,
                mar=old.mar))

    mf <- old.mf

    if (length(mf) == 0){
      mf <- c(1, 1)
    }

    if((n <- length(varisnum)) > 1 & max(mf) == 1){
      mf <- if(n <= 2){
        c(2, 1)
      } else if(n <= 4){
        c(2, 2)
      } else if(n <= 6){
        c(2, 3)
      } else if(n <= 9){
        c(3, 3)
      } else if(n <= 12){
        c(3, 4)
      } else if(n <= 16){
        c(4, 4)
      } else {
        c(4, 5)
      }
    }

    par(mfrow=mf)
    nplot.device <- prod(mf)
    if(!is.null(main)){
      main <- rep(main, length(x))
    } else if(main.vars){
      main <- varisnum
    }

    if(is.null(xlim)){
      xlim <- cbind(unlist(liis),unlist(lsss))
    }

    if(is.null(ylab)){
      ylab <- 'Frequency'
    }

    if(length(col)==1){
      col = c(col,colors()[152:253][2:length(varisnum)])
    }

    switch(typearg,         
           # f (absolute frequency) - polygon
           fp = {
             onlyfreq <- list()
             for(i in 1:length(factors)){
               onlyfreq[[i]] <- lapply(tabsfactors[[i]],
                                       function(x)with(x,table[,2]))
             }

             if(is.null(ylim)){
               ylim1 <- lapply(onlyfreq,function(x)do.call('cbind',x))
               ylim2 <- do.call('rbind',ylim1)
               ylim3 <- apply(ylim2,2,max)*1.2 
               ylim4 <- rbind(rep(0,length(varisnum)),ylim3)
               ylim <- t(ylim4)
             }

             par(mfrow = mf)
             for(i in 1:length(varisnum)){
               plot(mids[,i],
                    onlyfreq[[1]][[i]], 
                    type = 'b',
                    xaxt = 'n',
                    bty = 'n',
                    xlim = xlim[i,], 
                    ylim = ylim[i,],
                    xlab = xlab,
                    ylab = ylab,
                    col = col[1],
                    main = main[i])
               axis(1,at=round(mids[,i],x.round))
               j <- 2
               while(j <= length(factors)){
                 points(mids[,i],
                        onlyfreq[[j]][[i]],
                        type = 'b',
                        col = col[j])
                 j <- j+1
               }
             }
           },
           # rfp (relative frequency percentual) - polygon
           rfp = {
             onlyfreq <- list()
             for(i in 1:length(factors)){
               onlyfreq[[i]] <- lapply(tabsfactors[[i]],
                                       function(x)with(x,table[,3]))
             }

             if(is.null(ylim)){
               ylim1 <- lapply(onlyfreq,function(x)do.call('cbind',x))
               ylim2 <- do.call('rbind',ylim1)
               ylim3 <- apply(ylim2,2,max)*1.2 
               ylim4 <- rbind(rep(0,length(varisnum)),ylim3)
               ylim <- t(ylim4)
             }

             par(mfrow = mf)
             for(i in 1:length(varisnum)){
               plot(mids[,i],
                    onlyfreq[[1]][[i]], 
                    type = 'b',
                    xaxt = 'n',
                    bty = 'n',
                    xlim = xlim[i,], 
                    ylim = ylim[i,],
                    xlab = xlab,
                    ylab = ylab,
                    col = col[1],
                    main = main[i])
               axis(1,at=round(mids[,i],x.round)) 
               j <- 2
               while(j <= length(factors)){
                 points(mids[,i],
                        onlyfreq[[j]][[i]],
                        type = 'b',
                        col = col[j])
                 j <- j+1
               }
             }
           },
           # rfpp (relative frequency percentual percentual) - polygon
           rfpp = {
             onlyfreq <- list()
             for(i in 1:length(factors)){
               onlyfreq[[i]] <- lapply(tabsfactors[[i]],
                                       function(x)with(x,table[,4]))
             }

             if(is.null(ylim)){
               ylim1 <- lapply(onlyfreq,function(x)do.call('cbind',x))
               ylim2 <- do.call('rbind',ylim1)
               ylim3 <- apply(ylim2,2,max)*1.2 
               ylim4 <- rbind(rep(0,length(varisnum)),ylim3)
               ylim <- t(ylim4)
             }

             par(mfrow = mf)
             for(i in 1:length(varisnum)){
               plot(mids[,i],
                    onlyfreq[[1]][[i]], 
                    type = 'b',
                    xaxt = 'n',
                    bty = 'n',
                    xlim = xlim[i,], 
                    ylim = ylim[i,],
                    xlab = xlab,
                    ylab = ylab,
                    col = col[1],
                    main = main[i])
               axis(1,at=round(mids[,i],x.round))  
               j <- 2
               while(j <= length(factors)){
                 points(mids[,i],
                        onlyfreq[[j]][[i]],
                        type = 'b',
                        col = col[j])
                 j <- j+1
               }
             }
           },         
           # cdp (cumulative density) - polygon
           cdp = {
             onlyfreq <- list()
             onlyden <- list()
             for(i in 1:length(factors)){
               onlyfreq[[i]] <- lapply(tabsfactors[[i]],
                                       function(x)with(x,table[,3]))
               onlyden[[i]] <- mapply(function(y,z)cumsum(y/z),
                                      onlyfreq[[i]],
                                      as.list(hh))
             }

             if(is.null(ylim)){
               ylim2 <- do.call('rbind',onlyden)
               ylim3 <- apply(ylim2,2,max)*1.2 
               ylim4 <- rbind(rep(0,length(varisnum)),ylim3)
               ylim <- t(ylim4)
             }

             par(mfrow = mf)
             for(i in 1:length(varisnum)){
               plot(mids[,i],
                    onlyden[[1]][,i], 
                    type = 'b',
                    xaxt = 'n',
                    bty = 'n',
                    xlim = xlim[i,], 
                    ylim = ylim[i,],
                    xlab = xlab,
                    ylab = ylab,
                    col = col[1],
                    main = main[i])
               axis(1,at=round(mids[,i],x.round))   
               j <- 2
               while(j <= length(factors)){
                 points(mids[,i],
                        onlyden[[j]][,i],
                        type = 'b',
                        col = col[j])
                 j <- j+1
               }
             }
           },          
           # cfp (cumulative frequency) - polygon
           cfp = {
             onlyfreq <- list()
             for(i in 1:length(factors)){
               onlyfreq[[i]] <- lapply(tabsfactors[[i]],
                                       function(x)with(x,table[,5]))
             }

             if(is.null(ylim)){
               ylim1 <- lapply(onlyfreq,function(x)do.call('cbind',x))
               ylim2 <- lapply(ylim1,function(x)apply(x,2,max))
               ylim3 <- do.call('rbind',ylim2)
               ylim4 <- apply(ylim3,2,max)*1.2 
               ylim5 <- rbind(rep(0,length(varisnum)),ylim4)
               ylim <- t(ylim5)
             }

             par(mfrow = mf)
             for(i in 1:length(varisnum)){
               plot(mids[,i],
                    onlyfreq[[1]][[i]], 
                    type = 'b',
                    xaxt = 'n',
                    bty = 'n',
                    xlim = xlim[i,], 
                    ylim = ylim[i,],
                    xlab = xlab,
                    ylab = ylab,
                    col = col[1],
                    main = main[i])
               axis(1,at=round(mids[,i],x.round))    
               j <- 2
               while(j <= length(factors)){
                 points(mids[,i],
                        onlyfreq[[j]][[i]],
                        type = 'b',
                        col = col[j])
                 j <- j+1
               }
             }
           },          
           # cfpp (cumulative frequency%) - polygon
           cfpp = {
             onlyfreq <- list()
             for(i in 1:length(factors)){
               onlyfreq[[i]] <- lapply(tabsfactors[[i]],
                                       function(x)with(x,table[,6]))
             }

             if(is.null(ylim)){
               ylim <- matrix(rep(c(0,1.2*100),length(varisnum)),
                              byrow=TRUE,
                              nrow=length(varisnum))
             }

             par(mfrow = mf)
             for(i in 1:length(varisnum)){
               plot(mids[,i],
                    onlyfreq[[1]][[i]], 
                    type = 'b',
                    xaxt = 'n',
                    bty = 'n',
                    xlim = xlim[i,], 
                    ylim = ylim[i,],
                    xlab = xlab,
                    ylab = ylab,
                    col = col[1],
                    main = main[i])
               axis(1,at=round(mids[,i],x.round))     
               j <- 2
               while(j <= length(factors)){
                 points(mids[,i],
                        onlyfreq[[j]][[i]],
                        type = 'b',
                        col = col[j])
                 j <- j+1
               }
             }
           })
    if(is.null(args.legend)){

      args.2Kl <- list(x        = 'right',
                       legend   = factors,
                       col = col,
                       lty = rep(1,length(factors)),
                       xpd = T)

      do.call('legend',
              args.2Kl)

    } else {

      args.2Kl <- list(x        = 'right',
                       legend   = factors,
                       col = col,
                       lty = rep(1,length(factors)),
                       xpd=T)

      args.2Kl[names(args.legend)] <- args.legend     

      do.call('legend',
              args.2Kl) 

    }  
  } else {
    warning("Set an of the options to the type argument: fp, rfp, rfpp, cdp, cfp or cfpp!")
  }
}

