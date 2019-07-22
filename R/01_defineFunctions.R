
### A.2: define fundamental functions ##############################################

# create 'not in' function:
'%!in%' <- function(x,y)!('%in%'(x,y))

# function for selecting the number of factors (Ahn and Horenstein 2013, Econometrica)
ahnhorenstein <- function(data = "", K.factors = NULL, extractFactors=FALSE) {
  n <- ncol(data) #dimension
  p <- nrow(data) #sample size
  
  if (extractFactors == FALSE) {
    values <- extract(data, K = ncol(data))$eval
    
    values = pmax(values, 0) #values
  } else {
    values <- data
  }
  
  ratio = c()  
  K.factors <- if (is.null(K.factors)) {
    for(i in 1:(floor(min(n,p)/2))){
      ratio = append(ratio, values[i+1]/values[i])
    }
    ratio = ratio[is.finite(ratio)]
    K.factors = which.min(ratio)} else {
      for (i in 1:K.factors) {
        ratio = append(ratio, values[i+1]/values[i])
      }
      ratio = ratio[is.finite(ratio)]
      K.factors = which.min(ratio)
    }
  
  return(K.factors)
  cat("Number of factors with eigenvalue-ratio test (Ahn and Hohrenstein, 2013, Econometrica): ", K.factors, "\n")
}

# lag data:
lagdata <-function(y, lags, intercept = FALSE){
  T <- nrow(y)
  K <- ncol(y)
  obs <- T-lags
  x  <- embed(y, dimension = lags + 1)[,-(1:K)]
  if(intercept == TRUE){
    x <- cbind(1, x)
  }
  yi <- y[(lags+1):T,]
  return(list(y = yi, x = x, obs = obs, T = T, K = K));
}

# exctract factors (Stock and Watson 2002, JASA):
extract <- function(x, K, compMarR2 = T){
  f_call <- match.call()
  n <- ncol(x)
  x <- as.matrix(x)
  x.x <- t(x)%*%x
  
  evectors <- eigen(x.x)$vectors
  evalues <- eigen(x.x)$values
  
  varexplained <- evalues/sum(evalues)*100
  cumvarexplained <- cumsum(varexplained)
  
  ret.evectors <- sqrt(n)*evectors[, 1:K] #loadings
  fac <- x%*%ret.evectors/n #factors
  error = x - (fac %*% t(ret.evectors)) #error
  
  if (compMarR2 == T) {
    print("Compute marginal R-squared: TRUE")
    
    dataReg <- as.data.frame(cbind(fac, x))
    fhat <- c(paste0("fhat", seq(1, K, by = 1)))
    colnames(dataReg) <- c(fhat, colnames(x))
    dataFhat <- matrix(NA, nrow = K, ncol = ncol(dataReg))
    colnames(dataFhat) <- colnames(dataReg)
    
    pbar <- progress_bar$new(format = " :spin estimation process [:bar] :percent estimated: :eta",
                             total = ncol(dataFhat),
                             clear = FALSE,
                             width = 60)
    
    for (i in 1:ncol(dataFhat)) {
      pbar$tick()
      for (j in 1:K) {
        linmod.fhat <- lm(data = dataReg,
                          dataReg[, which(colnames(dataReg) == fhat[j])] ~ dataReg[, i])
        dataFhat[j,i] <- summary(linmod.fhat)$r.squared*100
      }
    }
  } else {
    dataFhat <- NULL
  }
  
  return(list(lam = ret.evectors,
              fac = fac,
              err = error,
              cumvar = cumvarexplained,
              eval = evalues,
              marR2 = dataFhat,
              K = K,
              f_call = f_call))
}

transData <- function(x, standardize = T){
  
  data <- x
  for (i in seq(2, ncol(data), by = 1)) {
    if (tcodeuse[1,i] == 1) { #no transformation
      cat("tcode: 1 -->", colnames(data[i]), "\n")
      next()
    } else if (tcodeuse[1,i] == 2) { #first-difference
      cat("tcode: 2 -->", colnames(data[i]), "\n")
      data[,i] <- c(NA, diff(data[,i], lag = 1, differences = 1))
    } else if (tcodeuse[1,i] == 3) { #second-difference
      cat("tcode: 3 -->", colnames(data[i]), "\n")
      data[,i] <- c(NA, diff(data[,i], lag = 1, differences = 2))
    } else if (tcodeuse[1,i] == 4) { #log-transform
      cat("tcode: 4 -->", colnames(data[i]), "\n")
      data[,i] <- log10(data[,i])
    } else if (tcodeuse[1,i] == 5) { #first log-differences
      cat("tcode: 5 -->", colnames(data[i]), "\n")
      data[,i] <- c(NA, diff(log10(data[,i]), lag = 1, differences = 1))
    } else if (tcodeuse[1,i] == 6) { #second log-differences
      cat("tcode: 6 -->", colnames(data[i]), "\n")
      data[,i] <- c(NA, NA, diff(log10(data[,i]), lag = 1, differences = 2))
    } else if (tcodeuse[1,i] == 7) { #(x_t / x_t-1 - 1.0)
      cat("tcode: 7 -->", colnames(data[i]), "\n")
      data[,i] <- (c(diff(data[,i], lag = 1, differences = 1), NA) / lag(data[,i], lag = 1)) - 1
      data[,i] <- c(NA, data[-length(data[,i]), i])
    }
  }
  
  if (standardize == T) {
    print("standardize data: TRUE")
    # standardize stationary data:
    for (i in seq(2, ncol(data), by = 1)) {
      data[,i] <- as.numeric(scale(data[,i], center = T, scale = T))
      # added as.numeric, otherwise output of scale() is a matrix.
    }
  } else if (standardize == F) {
    print("standardize data: FALSE")
  }
  return(data)
}

estFac3d <- function(data = ..., from = forecastStart, to = forecastEnd, K = 20, standardize = T) { #start function
  
  R <- array(NA, dim = c(length(seq.Date(from = as.Date(from), to = as.Date(to), by = "month")), ncol(data)-1, K))
  
  if (standardize == T) { #start if I
    dateTime2 <- c(seq.Date(from = as.Date(from), to = as.Date(to), by = "month"))
    for (t in dateTime2) { #start for-loop I
      print(as.Date(t))
      ndata <- data[which(data[, "sasdate"] <= t),]
      ndata <- scale(ndata[, which(colnames(data) != "sasdate")], center = T, scale = T)
      fac <- extract(as.matrix(ndata), K = K)
      marR2 <- fac$marR2[, -(1:K)]
      
      tt <- which(dateTime2 == t)
      for (kk in 1:K) { #start for-loop I.1
        R[tt, 1:ncol(ndata), kk] <- marR2[kk,]
      } #end for-loop I.1
      
    } #end for-loop I
    return(R)
  } # end if I
} #end function

plotFac3d <- function(data = factors3d[,,], K = 1){
  plot_ly(z = ~data[,,K],
          colorbar = list( title = ""),
          colors = grey(level = c(0.2, 0.5, 0.8), alpha = 1), linetypes = 5) %>% 
    add_surface() %>%
    layout(title = paste("Marginal R-squares for Factor", K),
           scene = list(
             xaxis = list(title = paste0("Variables (1 - ", ncol(data[,,K]), ")"),
                          gridcolor = "rgb(255, 255, 255)",
                          zerolinecolor = "rgb(255, 255, 255)",
                          showbackground = TRUE,
                          backgroundcolor = "rgb(240, 240, 240)",
                          autorange = "reversed",
                          showticklabels = F),
             yaxis = list(title = paste0("Time (",
                                         substr(forecastStart,1,4), "/M", substr(forecastStart, 9,10), " - ",
                                         substr(forecastEnd,1,4), "/M", substr(forecastEnd, 9,10), ")"),
                          gridcolor = "rgb(255, 255, 255)",
                          zerolinecolor = "rgb(255, 255, 255)",
                          showbackground = TRUE,
                          backgroundcolor = "rgb(230, 230, 230)",
                          showticklabels = F),
             zaxis = list(title = "R-squared",
                          range = c(0,100),
                          gridcolor = "rgb(255, 255, 255)",
                          zerolinecolor = "rgb(255, 255, 255)",
                          showbackground = TRUE,
                          backgroundcolor = "rgb(220, 220, 220)")
           )
    ) 
}
# date to split = 2000,
# horizon = 2002
splitTrainTestData <- function(.data, 
                               dateTimeTravel,
                               forecastHorizon=60,
                               verbose=TRUE,
                               .yvar=yvar) {
  # Returns:
  #   A list with 4 data.frames. x.train, y.train, x.test, y.test. 
  #
  # Args:
  #   dateTimeTravel
  #       cut the data with information at time t,
  #   forecastHorizon
  #       how long should be the test horizon (in months) looking back from
  #       time t (dateTimeTravel)
  #   .yvar
  #       which variable to be modelled 
  
  # make sure dateTimetravel is in date format
  if (!is.Date(dateTimeTravel)) {
    dateTimeTravel <- as.Date(dateTimeTravel)
  }
  
  # first cut data up to "time travel" period. 
  .data <- .data %>% 
    filter(sasdate <= dateTimeTravel)
  
  # split remaining into 2 sections. Begin til split; split til dateTimeTravel
  splitDate <- dateTimeTravel %m-% months(forecastHorizon)
  startDate <- head(.data$sasdate, 1)
  endDate <- tail(.data$sasdate, 1)
  
  ## split train und test (read %>% as in "then")
  x.train <- .data %>% 
    filter(sasdate <= splitDate) %>% select(-.yvar)
  y.train <- .data %>% 
    filter(sasdate <= splitDate) %>% select(yvar) %>% pull()  # pull: extract
  x.test <- .data %>%                                          # as numeric 
    filter(sasdate > splitDate) %>% select(-.yvar)
  y.test <- .data %>% 
    filter(sasdate > splitDate) %>% select(.yvar) %>% pull()  # pull: extract
                                                              # as numeric 
  if (nrow(x.train) != length(y.train)) {
    warning("check dimensions of training and test")
  } 
  if (isTRUE(verbose)) {
    message(paste0("Splitting Dates...\n",
                   "Start: ", startDate,
                   " || Split: ", splitDate,
                   " || End: ", endDate, 
                   " || Horizon: ", forecastHorizon, "Ms"))
  }
  return(list(x.train = x.train,
              x.test = x.test, 
              y.train = y.train,
              y.test = y.test, 
              metaSplits = c(startDate = startDate,
                             splitDate = splitDate,
                             endDate   = endDate)
              )
         )
}

alphaSearchFun <- function(splittedData, alphaSearchMin = 0.1, 
                           alphaSearchMax = 0.9, alphaSearchBy = 0.01,
                           lambdaseq = NULL, verbose=FALSE, alphaFix=NULL) {
  
  ## alpha search
  if (verbose) {
    message(paste("Alpha Search: from ", alphaSearchMin, "to ",
                  alphaSearchMax , "by ", alphaSearchBy))  
  }
  
  list.of.fits <- list()
  results <- data.frame()
  
  if (!is.null(alphaFix)) {
    alphaSeq <- alphaFix
  } else {
    alphaSeq <- seq(alphaSearchMin, alphaSearchMax, by = alphaSearchBy)
  }
  for(i in alphaSeq) {
    fit.name <- paste0(("alpha"), i)
    list.of.fits[[fit.name]] <- glmnet(data.matrix(splittedData$x.train[,-1]),
                                       splittedData$y.train,
                                       alpha = i,
                                       lambda = lambdaseq,
                                       family = "gaussian")
    
    # predicted <- predict(list.of.fits[[fit.name]],
    #                      s = list.of.fits[[fit.name]]$lambda,
    #                      newx = as.matrix(splittedData$x.test[,-1]))
    
    # mse <- mean((as.numeric(splittedData$y.test) - predicted)^2)
    temp <- data.frame(alpha = i,  fit.name = fit.name)
    results <- rbind(results, temp)
  }
  list.of.fits$results <- results
  return(list.of.fits)
}

EN_SelectVars <- function(rawdata=NULL,
                          genNrLags=12, 
                          dateTimeTravel = NULL,
                          forecastHorizon = 60,
                          ..yvar = yvar,
                          lambdaseq = 10^seq(0, -7, length.out = 100),
                          alphaSearchMin = 0.4,
                          alphaSearchMax = .6, 
                          alphaSearchBy = 0.01,
                          alphaFix = NULL,
                          maxVars = 60,
                          returnOnlyDfr = NULL,
                          verbose=T) {
  call <- sys.call()
  whichData <- call$data
  # gen lagged data
  data <- genLaggedData(rawdata, genNrLags)
  
  # split data
  splittedData <- splitTrainTestData(.data = data, dateTimeTravel,
                                     forecastHorizon=forecastHorizon,
                                     verbose=verbose, .yvar=..yvar)
  
  if (!is.null(alphaFix)) {
    alphaSeq <- alphaFix
  } else {
    alphaSeq <- seq(alphaSearchMin, alphaSearchMax, by = alphaSearchBy)
  }
  list.of.fits <- alphaSearchFun(splittedData, alphaSearchMin, alphaSearchMax, 
                                 alphaSearchBy, lambdaseq, verbose, alphaFix)
  
  # grid search alha and lambda
  l <- list()
  for (i in alphaSeq) {
    fit.name <- paste0(("alpha"), i)
    testPred <- predict(list.of.fits[[fit.name]], newx = as.matrix(splittedData$x.test[,-1]))
    sqerrors <- ((splittedData$y.test) - testPred)^2
    mse <- colMeans(sqerrors)
    mse <- c(mse, rep(NA, 100 - length(mse))) #just to make all same dimension
    #assign(paste0("predtest_mse", i), mse) 
    l[[fit.name]] <- mse
  }
  
  # transform to list to data frame
  asDF <- data.frame(l)
  # get location of min MSE among all alphas and lambdas
  loc<-which(asDF == min(asDF, na.rm = TRUE), arr.ind = TRUE)
  # extract name of best fit
  bestfitname <- colnames(asDF[loc[2]])
  # extract best fit of list of fits
  bestfit <- list.of.fits[[bestfitname]]
  # get MSE of best fit, lambda and alpha
  bestMSE <- asDF[loc]
  bestLambda <- bestfit$lambda[loc[1]]
  bestAlpha <- list.of.fits$results$alpha[loc[2]]
  
  # extract the Coefs based on the above selected best lambda
  coefsOfBestLambda <- bestfit$beta[, bestfit$lambda == bestLambda]
  nrOfNonNullCoefs <- sum(abs(coefsOfBestLambda) > 0)
  
  # get colnames of all variables with non-zero coeficients estimates
  resVarsSortedAbs <- sort(abs(coefsOfBestLambda[abs(coefsOfBestLambda) > 0 ]),
                           decreasing = T)
  resVarsNames <- names(resVarsSortedAbs)
  resVarsNamesClean <- gsub("\\..*$","", resVarsNames)
  resVarsNamesUniq <- unique(resVarsNamesClean)
  numberOfUniqVars <- length(resVarsNamesUniq)
  # generate vector of NON duplicatse (true,false)
  noDups <- !duplicated(resVarsNamesClean)
  resVarsSortedAbsUniq <- resVarsSortedAbs[noDups]
  names(resVarsSortedAbsUniq) <- gsub("\\..*$","", names(resVarsSortedAbsUniq))
  
  if (verbose) {
    message("Resulting in...")
    message(paste("alpha: ", bestAlpha, "\tmse: ", bestMSE, "\tlambda: ", bestLambda))
    message("Nr of selected Vars: ",length(resVarsNames),
            "\tNr of unique Vars: ", numberOfUniqVars)
    message(rep("=", 80))
    
  }
  if (length(resVarsNamesUniq) > maxVars) {
    # if more than maxVars, restrict to only the 
    #resVarsSortedAbs  <- head(resVarsSortedAbs, maxVars)
    resVarsNames      <- head(resVarsNames, maxVars)
    resVarsNamesClean <- head(resVarsNamesClean, maxVars)
    resVarsNamesUniq  <- head(resVarsNamesUniq, maxVars)
    resVarsSortedAbsUniq <- head(resVarsSortedAbsUniq, maxVars)
  }
  
  outData <- data[, names(resVarsSortedAbs)]
  # if only interested in final DF with relevant vars ... 
  if (isTRUE(returnOnlyDfr)) {
    return(outData)
  }
  # ... otherwise return extra info as well in a list
  meta <- list(whichData = whichData,
               call = call, 
               splitDates = splittedData$metaSplits,
               dimOutData = dim(outData),
               resVarsSortedAbs = resVarsSortedAbs,
               resVarsSortedAbsUniq = resVarsSortedAbsUniq,
               resVarsNames = resVarsNames,
               resVarsNamesUniq = resVarsNamesUniq,
               resVarsNamesClean = resVarsNamesClean,
               numberOfUniqVars = numberOfUniqVars,
               maxVars = maxVars,
               coefsOfBestLambda = coefsOfBestLambda,  # coefsOfBestLambda inclusive 0 valued coefs!
               nrOfNonNullCoefs = nrOfNonNullCoefs,
               bestMSE = bestMSE, 
               bestLambda = bestLambda,
               bestAlpha = bestAlpha)
  
  out <- list(outData = outData, meta = meta, bestfit = bestfit)
  class(out) <- "EN_Select"
  return(out)
}


# create generic barplot function for EN_SElect object
barplot.EN_Select <- function(x, unique=FALSE, ...) {
  leg = paste0(
    "start date\t", "split date\t\t", " end date\n", 
    x$meta$splitDates[1],"\t", x$meta$splitDates[2], "\t",x$meta$splitDates[3],
    "\n# of selected vars:  ", x$meta$nrOfNonNullCoefs, 
    "\n# of unique vars:  ", x$meta$numberOfUniqVars,
    "\n max vars:  ", x$meta$maxVars,
    "\n data:  ", x$meta$whichData,
    "\nlambda:  ", format(x$meta$bestLambda, digits=3),
    "\nmse:  ", format(x$meta$bestMSE, digits=3),
    "\nalpha:  ", format(x$meta$bestAlpha, digits=3)
  )
  
  varsToPlotClean <- gsub("\\..*$","", x$meta$resVarsNames)
  groups <- metaDataFrame[6, ][varsToPlotClean]
  xtoPlot <- x$meta$resVarsSortedAbs
  xAxis <- paste0(names(xtoPlot), " (", groups, ")")
  color = as.character(metaDataFrame[metaDataFrame$sasdate == "color", ][varsToPlotClean])
  
  #uniqueVarstoPlot <- x$meta$resVarsNamesUniq %in% names(xtoPlot) ### PROBLEM!!!
  
  
  if (isTRUE(unique)) {
    warning("unique is in beta still!!! Check for repeated vars!!!")
    xtoPlotUniq <- x$meta$resVarsSortedAbsUniq
    noDups <- !duplicated(x$meta$resVarsNamesClean)
    colorUniq = color[noDups]
    
    varsToPlotCleanUniq <- gsub("\\..*$","", names(x$meta$resVarsSortedAbsUniq))
    groupsUniq <- metaDataFrame[6, ][varsToPlotCleanUniq]
    xAxisUniq <- paste0(names(x$meta$coefsOfBestLambda[x$meta$resVarsNamesUniq]), " (", groupsUniq, ")")
    
    if (any(duplicated(names(xtoPlotUniq)))) {
      stop("duplicated in unique, there is something wrong.")
    }
    
    main = "Selected Variables (Unique)"
    invisible(barplot(main = main, xtoPlotUniq, names.arg = xAxisUniq,
                      col = colorUniq, las=2, cex.axis = .60, cex.names = .6,
                      cex.main= .7, ...))
    return(mtext(leg, adj = 1, side=4, cex = .7, line=-1.6, las=2))
    #return(mtext(leg, adj = 1, side=4, cex = .7, las=1, padj = -.7, las=2, line=-1.6))
  }
  
  main = "Selected Variables (Duplicates)"
  par(mar=c(8, 4, 4, 2) + 0.1 )
  invisible(barplot(xtoPlot, main = main, names.arg = xAxis, col = color, las=2,
                    cex.axis = .60, cex.names = .6, cex.main= .7, ...))
  return(mtext(leg, adj = 1, side=4, cex = .7, line=-1.6, las=2))
}

genLaggedData <- function(data, lags=12) {
  tmplist <- as.matrix(data[,-1]) %>% lagdata(lags = lags)
  x <- as.data.frame(tmplist$x)
  y <- as.data.frame(tmplist$y)
  varnames <- colnames(tmplist$y)
  egrid <- expand.grid(varnames, paste0(".", 1:lags)) 
  expVarNames <- do.call(paste0, as.list(egrid))
  x <- as.data.frame(x)
  names(x) <- expVarNames
  dates <- data$sasdate[-(1:lags)]
  x$sasdate <- dates
  y$sasdate <- dates
  
  dfMerge <- merge(y, x)
}

## Sanity checks!
sanityCheckLagData <- function(ldata) {
  # basically this functions checks if the "var" in t is equal
  # to "var.lag", accounting for the shift in the data 
  # The variable and obs is chosen randomly
  i <- sample(1:125, 1)
  rowNbr <- sample(1:(nrow(ldata)-24),1)
  p <- sample(1:12, 1)
  toCheck <- names(ldata[,-1])[i]
  
  c1 <- ldata %>% select(toCheck) %>% 
    filter(row_number() == rowNbr) %>% as.numeric()
  c2 <- ldata %>% select( paste0(toCheck,".", p) ) %>% 
    filter(row_number() == rowNbr+p) %>% as.numeric()
  
  assertthat::are_equal(c1, c2)
}



plot.EN_Results <- function(in_data=NULL, type=c("spaghetti", "heatmap") , 
                            cellw = 4, cellh = 4, ...) {
  dataToPlot <- in_data
  rownames(dataToPlot) <- dataToPlot$vars
  dataToPlot <- dataToPlot[,-1]
  dataToPlot <- replace(dataToPlot, is.na(dataToPlot), 0)
  if (type=="spaghetti") {
    # rank by column (2)
    dfRankToPlot <- apply(-dataToPlot, MARGIN = 2, rank, ties.method = "max" )
    dfRankToPlot <- t(floor(dfRankToPlot))
    dfRank_ts <- ts(dfRankToPlot, start = as.yearmon(rownames(dfRankToPlot)[1]) ,
                    frequency = 12)
    
    par(mar=c(1,1,0.2,0.2)*2.2)
    plot.ts(dfRank_ts,
            plot.type = "single",
            col = misc$colorsCode[misc$blockAsNum],
            xlab = "", ylab = "",
            lwd = 2,
            xaxt = "n", cex.lab=0.75, tck=-.01, ...
    )
    ix <- seq(1, nrow(dfRank_ts), by=12) #every 60 days
    labs <- year(forecastMadeOnSeq)
    axis(side = 1, at = labs, labels = labs,  cex.axis = .9, tck=-.01)
    abline(h = 60, lwd=1.5)
    nUniq_ts <- ts(attr(in_data, "nUniq"),
                start = as.yearmon(as.character(forecastStart)), freq=12 )
    lines(nUniq_ts, col = "black", lwd = 3, xpd=TRUE)
    points(nUniq_ts, col = "black", ylim=c(0,120), pch = 16)
    legend("topleft", legend = misc$tableNames,  ###before:legend = paste("G:", unique(misc$blockAsNum))
           col = misc$colorsCode, lty=1, cex=.7, horiz = F, lwd=5, 
           inset=c(2,2)/70, ncol=2, bg="white")
  }
  if (type=="heatmap") {
    # dfHeatToPlot <- apply(dfRank, MARGIN = 2, rank, ties.method = "max")
    # dfHeatToPlot <- replace(in_data, is.na(in_data), 0)
    ix <- which(names(metaDataFrame) == yvar)
    anotRow <- data.frame(block=metaDataFrame["block",-c(1, ix)] %>% as.numeric())
    
    if (yvar %in% rownames(dataToPlot)) {
      ixx <- which(rownames(dataToPlot) == yvar)
      dataToPlot <- dataToPlot[-ixx, ]
    }
    rownames(anotRow) <- rownames(dataToPlot)
    anotcolors <- list(block=c(misc$colorsCode))
    dfHeatToPlot <- (dataToPlot)^(1/14)
    
    suppressWarnings(
      pheatmap::pheatmap(dfHeatToPlot, 
                         cluster_cols = F,
                         cluster_rows = F,
                         show_rownames = T, 
                         show_colnames = T,
                         fontsize = 5,
                         legend = F, annotation_legend = F,
                         border_color = "white",
                         annotation_row = anotRow,
                         annotation_colors = anotcolors,
                         # gaps_row = which(!is.na(misc$tableNames2)==T)[-1]-1,
                         gaps_row = c(17,  48,  58,  66,  80, 101, 120),
                         labels_col = as.yearmon(colnames(dfHeatToPlot)[seq(1, ncol(dfHeatToPlot),by=3)]), 
                         #color = c("white", gray.colors(20, rev=T)),
                         color = c("white", RColorBrewer::brewer.pal(9, "Blues")),
                         # color = colorRampPalette(c("white", "white", "lightblue", "blue", "darkblue"))(30),
                         # filename = "fig/selectvars_prelim.pdf",
                         # main=datapath, 
                         # scale = "column"
                         cellwidth = cellw, cellheight = cellh, 
                         ...)
    )
  }
}



doTheLoopEN <- function(inputData, lags=12, .alphaFix=NULL, ...) {
  
  dfRes <- data.frame(matrix(NA, nrow = ncol(inputData)-2, ncol = 0))
  dfRes$vars <- names(inputData)[-c(1,which(names(inputData)==yvar))]
  nUniq <- nTot <- c()
  for (i in seq_along(forecastMadeOnSeq)) {
    dateTimeTravel <- forecastMadeOnSeq[i]
    
    res <- EN_SelectVars(rawdata = odata, genNrLags = lags, alphaFix = .alphaFix,
                         dateTimeTravel = dateTimeTravel, ...)
    
    dateAsChar <- as.character(dateTimeTravel)
    aa<-data.frame(vars=names(res$meta$resVarsSortedAbsUniq), 
                   res$meta$resVarsSortedAbsUniq)
    names(aa)[2] <- as.character(dateTimeTravel)
    dfRes <- plyr::join(dfRes, aa)

    nUniq[i] <- res$meta$numberOfUniqVars
    nTot[i] <- res$meta$nrOfNonNullCoefs
    ## if last month, keep 
    if (i == length(forecastMadeOnSeq)) {
      attr(dfRes, "call") <- res$meta$call
      attr(dfRes, "nUniq") <- nUniq
      attr(dfRes, "nTot") <- nTot
    }
  }
  # datapath <- paste0("./data/dfRes_",
  #                    attr(dfRes, "call")$rawdata, "_",
  #                    sys.call(), 
  #                    as.yearmon(head(forecastMadeOnSeq, 1)), "s_",
  #                    as.yearmon(tail(forecastMadeOnSeq, 1)),  "f_",
  #                    ".RDS")
  # attr(dfRes, "datapath") <- datapath
  # message("saving ", datapath)
  # saveRDS(dfRes, datapath)
  
  class(dfRes) <- c("EN_Results", "data.frame") 
  attr(dfRes, "callLoop") <- sys.call()
  
  return(dfRes)
}

fcTest <- function(data = ...,
                   fcstart = forecastStart,
                   fcend = forecastEnd,
                   by = "month",
                   fchorizon.steps = c(1,2,3,6,12,24),
                   center.x = F,
                   center.y = F,
                   center.xy = F,
                   scale.x = F,
                   scale.y = F,
                   scale.xy = F) { #start function
  
  dateSeqAll <- seq.Date(from = fcstart, to = fcend, by = by)
  fchorizon <- max(fchorizon.steps)
  if (max(fchorizon.steps) > fchorizon) {
    stop(print("max stepsize for forecast exceeds data implied forecast horizon"))}
  
  newColNam <- c()
  for (i in 1:length(fchorizon.steps)) {
    newColNam[i] <- paste0("fchor", fchorizon.steps[i]) 
  }
  
  AR.1 <- matrix(NA, nrow = 1, ncol = length(fchorizon.steps) + 1)
  colnames(AR.1) <- c("sasdate", newColNam)
  
  VAR.1 <- matrix(NA, nrow = 1, ncol = length(fchorizon.steps) + 1)
  colnames(VAR.1) <- c("sasdate", newColNam)
  
  ARX.1 <- matrix(NA, nrow = 1, ncol = length(fchorizon.steps) + 1)
  colnames(ARX.1) <- c("sasdate", newColNam)
  
  for (t in dateSeqAll) { #start for-loop I
    
    ### 1: set data ############################################################
    t <- as.Date(t) #, format = "%Y-%m-%d", origin = "1970-01-01")
    cat("fc made on:", as.character(as.Date(t)), "-",
        "date+1:", as.character(as.Date(t %m+% months(1))), "-",
        paste0("date+", fchorizon, ":"), as.character(as.Date(t %m+% months(fchorizon))), "\n")
    
    #print("P1: set data")
    dateSeq.train <- as.Date(c(seq.Date(from = customTimeStart, to = t, by = by)))
    dateSeq.test  <- as.Date(c(seq.Date(from = as.Date(t %m+% months(1)), to = as.Date(t %m+% months(fchorizon)), by = by)))
    dateSeqSteps.test <- dateSeq.test[fchorizon.steps]
    
    #print("1")
    # ndata = "new data": 
    x.ndata  <- data[, which(colnames(data) %!in% yvars), drop = F]
    y.ndata  <- data[, which(colnames(data) %in% c("sasdate", yvars)), drop = F]
    xy.ndata <- data
    
    #print("2")
    # ndata.train = in-sample data ("time travel"): data <= t
    x.ndata.train  <- data.frame(as.character(dateSeq.train), scale(x.ndata[which(x.ndata[,1] <= as.Date(t)),][,-1], center = center.x, scale = scale.x))
    y.ndata.train  <- data.frame(as.character(dateSeq.train), scale(y.ndata[which(y.ndata[,1] <= as.Date(t)),][,-1], center = center.y, scale = scale.y))
    xy.ndata.train <- data.frame(as.character(dateSeq.train), scale(xy.ndata[which(xy.ndata[,1] <= as.Date(t)),][,-1], center = center.xy, scale = scale.xy))
    
    #print("2.1")
    colnames(x.ndata.train)  <- c("sasdate", colnames(x.ndata[,-1]))
    colnames(y.ndata.train)  <- c("sasdate", as.character(yvars))
    colnames(xy.ndata.train) <- c("sasdate", colnames(xy.ndata[,-1]))
    
    #print("3")
    # ndata.test = pseudo-out-of-sample data ("hold-back -true- realizations!"), data > t
    x.ndata.test   <- data.frame(as.character(dateSeq.test), scale(x.ndata[which(x.ndata[,1] %in% dateSeq.test), ][,-1], center = center.x, scale = scale.x))
    y.ndata.test   <- data.frame(as.character(dateSeq.test), scale(y.ndata[which(y.ndata[,1] %in% dateSeq.test), ][,-1], center = center.y, scale = scale.y))
    xy.ndata.test  <- data.frame(as.character(dateSeq.test), scale(xy.ndata[which(xy.ndata[,1] %in% dateSeq.test), ][,-1], center = center.xy, scale = scale.xy))
    
    #print("3.1")
    colnames(x.ndata.test)  <- c("sasdate", colnames(x.ndata[,-1]))
    colnames(y.ndata.test)  <- c("sasdate", as.character(yvars))
    colnames(xy.ndata.test) <- c("sasdate", colnames(xy.ndata[,-1]))
    
    ### 2: forecast exercise (AR, VAR) #########################################
    ### 2.1: AR ################################################################
    
    fcdata.train <- y.ndata.train
    fcdata.test  <- y.ndata.test[,]
    
    fc.ar1 <- ar.mle(x = fcdata.train[,-1], aic = T)
    fc.ar1$x <- fcdata.train[,-1]
    
    fc.ar1 <- data.frame(NA, t(sqrt((forecast(fc.ar1, h = fchorizon)$mean[c(fchorizon.steps)] - fcdata.test[c(fchorizon.steps),-1])^2)))
    
    colnames(fc.ar1) <- c("sasdate", newColNam)
    fc.ar1[1, "sasdate"] <- as.character(as.Date(t))
    
    AR.1 <- rbind(AR.1, fc.ar1)
    
    ### 2.2: VAR ###############################################################
    fcdata.train <- data.frame(x.ndata.train[, c("sasdate", "INDPRO", "FEDFUNDS")], y.ndata.train[,-1, drop = F])
    fcdata.test  <- xy.ndata.test[, c("sasdate", yvars)]
    
    fc.var1 <- vars::VAR(y = as.matrix(fcdata.train[,-1]), ic = "BIC",type = "const")
    #fc.var1$x <- fcdata.train[,-1]
    #print(predict(fc.var1, n.ahead = fchorizon))
    #fc.var1 <- predict(fc.var1, h = fchorizon)
    
    fc.var1 <- data.frame(NA, t(sqrt((predict(fc.var1, n.ahead = fchorizon)$fcst$CPIAUCSL[,"fcst"][c(fchorizon.steps)] - fcdata.test[c(fchorizon.steps),-1])^2)))
    colnames(fc.var1) <- c("sasdate", newColNam)
    fc.var1[1, "sasdate"] <- as.character(as.Date(t))
    VAR.1 <- rbind(VAR.1, fc.var1)
    
    #print("collect fc for steps")
    #fc.ar1 <- fc.ar1$mean[fchorizon.steps]
    
    ### 2.3: SW-DI (Stock and Watson diffusion index) ##########################
    fcdata.train <- data.frame(xy.ndata.train)
    fcdata.test  <- data.frame(xy.ndata.test[, c("sasdate", yvars)])
    K <- 15
    
    y.fcdata.train <- y.ndata.train[,-1, drop = F]
    y.fcdata.test <- y.ndata.test[,-1, drop = F]
    x.fcdata.train <- x.ndata.train[,-1, drop = F]
    
    n.fhat <- extract(x = scale(fcdata.train[, which(colnames(fcdata.train) %!in% c("sasdate", yvars))], center = T, scale = T),
                      K = K, compMarR2 = F)$fac[,1:K]
    colnames(n.fhat) <- paste0("fhat", rep(1:K))
    
    y.fcdata.train <- as.matrix(y.fcdata.train)
    n.fhat <- as.matrix(n.fhat)
    
    fc.fhat <- vars::VAR(y = as.matrix(n.fhat), ic = "BIC", type = "const")
    fc.fhat <- predict(fc.fhat, n.ahead = fchorizon)$fcst
    
    R <- matrix(NA, nrow = fchorizon, ncol = K)
    for (i in 1:K) {
      f <- fc.fhat[[paste0("fhat", i)]][, 1]
      R[,i] <- f
    }
    fc.fhat <- R
    
    arx <- stats::arima(y.fcdata.train, order = c(4,0,0), xreg = n.fhat)
    
    fc.arx <- predict(arx, n.ahead = fchorizon, newxreg = fc.fhat)
    fc.arx <- data.frame(NA, t(sqrt((fc.arx$pred[c(fchorizon.steps)] - y.fcdata.train[c(fchorizon.steps)])^2)))
    colnames(fc.arx) <- c("sasdate", newColNam)
    fc.arx[1, "sasdate"] <- as.character(as.Date(t))
    ARX.1 <- rbind(ARX.1, fc.arx)
    
    
    #fc.arx <- MARX::forecast.marx(y = fcdata.train[, yvars, drop = F],
    #X = as.matrix(n.fhat[,1:K]),
    #p_C = 4, p_NC = 0,
    #h = fchorizon)
    
    #SW.1 <- SWfore(y = y.fcdata.train,
    #x = x.fcdata.train[, which(colnames(fcdata.train) %!in% c("sasdate", yvars))],
    #orig = ,
    #m = K)
    
    
  } #end for-loop I
  
  return(list(dateSeq = list(train = dateSeq.train,
                             test = dateSeq.test,
                             steps.test = dateSeqSteps.test),
              data = list(x.ndata.train = x.ndata.train,
                          y.ndata.train = y.ndata.train,
                          xy.ndata.train = xy.ndata.train,
                          x.ndata.test = x.ndata.test,
                          y.ndata.test = y.ndata.test,
                          xy.ndata.test = xy.ndata.test,
                          fcdata = fcdata.train,
                          #fc.var1 = fc.var1,
                          AR.1 = AR.1,
                          VAR.1 = VAR.1,
                          ARX.1 = ARX.1)))
  
} #end functions


fcSW <- function(ydata = ..., xdata = ..., k = 3,
                 fchorizon.steps = c(1,2,3,6,12,24), 
                 fcstart = forecastStart, fcend = forecastEnd) { #start function
  
  fchorizon <- max(fchorizon.steps)
  originStart <- which(ydata[, "sasdate"] == fcstart)
  originEnd <- which(ydata[, "sasdate"] == fcend)
  
  newColNam <- paste0("fchor", rep(c(fchorizon.steps)))
  
  y.fcndata <- ydata[, -which(colnames(ydata) == "sasdate")]
  x.fcndata <- xdata[, -which(colnames(xdata) == "sasdate")]
  
  # initiate result matrices:
  SW.1 <- matrix(NA, nrow = 1, ncol = length(fchorizon.steps) + 1)
  colnames(SW.1) <- c("sasdate", newColNam)
  
  for (t in originStart:originEnd) { #start for-loop (t)
    date <- ydata[t, "sasdate"]
    print(as.Date(date))
    
    fc.SW <- SWfore(y = y.fcndata, x = x.fcndata, orig = t, m = k)$yhat[1:fchorizon][c(fchorizon.steps)]
    
    #plot.ts(fc.SW)
    #lines(y.fcndata[t+1:(t+fchorizon+1)][fchorizon.steps], col = "red")
    
    fc.SW <- data.frame(NA, t(sqrt((fc.SW - y.fcndata[t+1:(t+fchorizon+1)][fchorizon.steps])^2)))
    
    #print("here1")
    colnames(fc.SW) <- c("sasdate", newColNam)
    #print("here2")
    fc.SW[1,"sasdate"] <- as.character(as.Date(date))
    
    #print("here3")
    SW.1 <- rbind(SW.1, fc.SW)
    
  } #end for-loop (t)
  
  return(list(SW.1 = SW.1))
  
} #end function


