library("plotmo")
### do the loops ---------------------------------------------------------------
lagsVec <- c(12, 18, 24, 6)
lambdasVec <- list(
  NULL , expression(10^seq(0, -7, length.out = 100))
)
fc_horizVec <- c(60, 12, 24, 48)
alphasFixList <- list(.3, .5, .7)
for (lags in lagsVec) {
  for (fc_horiz in fc_horizVec) {
    for (lambdasExp in lambdasVec) {
      for (alpha in alphasFixList) {
        message(paste(lags, fc_horiz, lambdasExp, alpha))
        xx <- doTheLoopEN(odata, lags = lags, lambdaseq=eval(lambdasExp),
                          forecastHorizon = fc_horiz, .alphaFix = alpha)
        
        message("plotting")
        plot(xx, type="spaghetti", main = attr(xx, "callLoop"))
        plot(xx, type="heatmap", main = attr(xx, "callLoop"))
        
        message("saving")
        saveRDS(xx, paste("./data/bb_", "lg", lags, "lmb", lambdasExp,
                           "hzn", fc_horiz, "a",  alpha, sep = "_"))
        message(rep("=", 80));message(rep("=", 80));message(rep("=", 80));
      }
    }
  }
}

for (i in lambdasVec) {
  print(i)
}

# lambdaseq=10^seq(0, -7, length.out = 100)
### do the loops one by one  ---------------------------------------------------
res_060606a <- doTheLoopEN(odata, lags = 24, alphaSearchMin=0.96, 
                           alphaSearchMax=1, alphaSearchBy=.005,
                           forecastHorizon=60)
plot(res_060606a, type="spaghetti")

res_060606b <- doTheLoopEN(odata, lags = 12, alphaSearchMin=0.96, 
                           alphaSearchMax=1, alphaSearchBy=.001,
                           forecastHorizon=18)
plot(res_060606b, type="spaghetti")
saveRDS(res_060606b, "./data/cc_alpha_096-1_fein_12lags_18fchoriz.RDS")



saveRDS(res_060606a, "./data/cc_alpha_096-1_fein.RDS")


xxx<-EN_SelectVars(rawdata = odata, genNrLags = 12, alphaSearchMin=.95, 
                   alphaSearchMax=1, alphaSearchBy=.005, lambdaseq = NULL,
                   forecastHorizon=60, dateTimeTravel = forecastMadeOnSeq[160])

plot(xxx$bestfit, c("norm", "lambda", "dev")[2])
abline(v=log(xxx$meta$bestLambda))

kplot(res_060606, type="spaghetti")
plot(res_060606, type="heatmap")
saveRDS(res_060606, "./data/cc_alpha_0-1_free.RDS")


### do the loops one by one  ---------------------------------------------------

resbench <- doTheLoopEN(odata, 12)
plot(resbench, type="spaghetti", main = attr(resbench, "call"))
plot(resbench, type="heatmap", main = attr(resbench, "call"))
saveRDS(resbench, "./data/aa_resbench.RDS")

res24 <- doTheLoopEN(odata, 24)
class(res24) <- c("EN_Results", "data.frame")
plot(res24, type="spaghetti", main = attr(res24, "call"))
plot(res24, type="heatmap", main = attr(res24, "call"))
saveRDS(res24, "./data/aa_res24.RDS")

res18 <- doTheLoopEN(odata, 18)
class(res18) <- c("EN_Results", "data.frame")
plot(res18, type="spaghetti", main = attr(res18, "call"))
plot(res18, type="heatmap", main = attr(res18, "call"))
saveRDS(res18, "./data/aa_res18.RDS")

res_a_0.03 <- doTheLoopEN(odata, 12, .alphaFix = .03)
class(res_a_0.03) <- c("EN_Results", "data.frame")
plot(res_a_0.03, type="spaghetti", main = attr(res_a_0.03, "call"))
plot(res_a_0.03, type="heatmap", main = attr(res_a_0.03, "call"))
saveRDS(res_a_0.03, "./data/aa_res_a_0.03.RDS")

res_a_0.03_l24 <- doTheLoopEN(odata, 24, .alphaFix = .03)
class(res_a_0.03_l24) <- c("EN_Results", "data.frame")
plot(res_a_0.03_l24, type="spaghetti", main = attr(res_a_0.03_l24, "call"))
plot(res_a_0.03_l24, type="heatmap", main = attr(res_a_0.03_l24, "call"))
saveRDS(res_a_0.03_l24, "./data/aa_res_a_0.03_l24.RDS")

res_a_0.05 <- doTheLoopEN(odata, 12, .alphaFix = .05)
class(res_a_0.05) <- c("EN_Results", "data.frame")
plot(res_a_0.05, type="spaghetti", main = attr(res_a_0.05, "call"))
plot(res_a_0.05, type="heatmap", main = attr(res_a_0.05, "call"))
saveRDS(res_a_0.05, "./data/aa_res_a_0.05.RDS")

res_a_0.05_l24 <- doTheLoopEN(odata, 24, .alphaFix = .05)
class(res_a_0.05_l24) <- c("EN_Results", "data.frame")
plot(res_a_0.05_l24, type="spaghetti", main = attr(res_a_0.05_l24, "call"))
plot(res_a_0.05_l24, type="heatmap", main = attr(res_a_0.05_l24, "call"))
saveRDS(res_a_0.05_l24, "./data/aa_res_a_0.05_l24.RDS")

res_a_0.1 <- doTheLoopEN(odata, 12, .alphaFix = .1)
class(res_a_0.1) <- c("EN_Results", "data.frame")
plot(res_a_0.1, type="spaghetti", main = attr(res_a_0.1, "call"))
plot(res_a_0.1, type="heatmap", main = attr(res_a_0.1, "call"))
saveRDS(res_a_0.1, "./data/aa_res_a_0.1.RDS")

res_a_0.1_l24 <- doTheLoopEN(odata, 24, .alphaFix = .1)
class(res_a_0.1_l24) <- c("EN_Results", "data.frame")
plot(res_a_0.1_l24, type="spaghetti", main = attr(res_a_0.1_l24, "call"))
plot(res_a_0.1_l24, type="heatmap", main = attr(res_a_0.1_l24, "call"))
saveRDS(res_a_0.1_l24, "./data/aa_res_a_0.1_l24.RDS")

res_a_0.2 <- doTheLoopEN(odata, 12, .alphaFix = .2)
class(res_a_0.2) <- c("EN_Results", "data.frame")
plot(res_a_0.2, type="spaghetti", main = attr(res_a_0.2, "call"))
plot(res_a_0.2, type="heatmap", main = attr(res_a_0.2, "call"))
saveRDS(res_a_0.2, "./data/aa_res_a_0.2.RDS")

res_a_0.2_l24 <- doTheLoopEN(odata, 24, .alphaFix = .2)
class(res_a_0.2_l24) <- c("EN_Results", "data.frame")
plot(res_a_0.2_l24, type="spaghetti", main = attr(res_a_0.2_l24, "call"))
plot(res_a_0.2_l24, type="heatmap", main = attr(res_a_0.2_l24, "call"))
saveRDS(res_a_0.2_l24, "./data/aa_res_a_0.2_l24.RDS")






### D. Elastic Net (Simple  Examples) ------------------------------------------
lodata24 <- genLaggedData(odata, 24)
lodata12 <- genLaggedData(odata, 12)
lodata13 <- genLaggedData(odata, 18)

for (i in seq(1, length(forecastMadeOnSeq), by=11)) {
  date <- forecastMadeOnSeq[i]
  fchoriz <- 6
  par(mar=c(1,1,1,1)*2.2)
  
  alphaSeq <- (c(0, ((10^(seq(1, 3, length.out = 6)) -1)/(10^3)/2 ))) %>% 
    round(digits = 5)
  alphaSeq
  alphaFix <- .03
  for (alpha in alphaFix) {
    print(alpha)
    lambdaseq <- exp(seq(1, -10, length.out = 100))
    ab <- EN_SelectVars(rawdata = odata, 
                        genNrLags = 12, 
                        dateTimeTravel = date,
                        forecastHorizon = fchoriz,
                        lambdaseq = lambdaseq,
                        ..yvar = yvar,
                        alphaSearchMin =  alpha,
                        alphaSearchMax = alpha,
                        alphaSearchBy = .0000001,
                        verbose = T)
    # barplot(ab, unique = F, ylim = c(0, 1))
    coll <- coef(ab$bestfit)
    
    #plot(ab$bestfit, xvar = c("norm", "lambda", "dev")[2], 
    #     label = T)
    sortix<- ab$bestfit$beta
    g <- as.character(metaDataFrame["color", -(1:2)])
    g <- rep(g, 13)
    plot(ab$bestfit, xvar = c("norm", "lambda", "dev")[2], 
         label = T)
    
    abline(v=log(ab$meta$bestLambda), col="red", lwd=2, lty=2)
    abline(v = log (alpha+.00001), col="black", lwd=1, lty=3)
    legend("topleft", cex = 1, bty = "n",
           legend = paste(
             "data:", ab$meta$whichData,
             "\ndate:", date, 
             "\nn_uniqs:", ab$meta$numberOfUniqVars, 
             "\nn_all:", ab$meta$nrOfNonNullCoefs,
             "\nalpha:", alpha, 
             "\nlog-lambda:", round(log(ab$meta$bestLambda), 3))
           )
    Sys.sleep(1)
  }
}


### E: Elastic Net (Looping) ---------------------------------------------------

dfRes <- data.frame(matrix(NA, nrow = ncol(data)-1, ncol = 0))
dfRes$vars <- names(data)[-1]
nUniq <- nTot <- c()
for (i in seq_along(forecastMadeOnSeq)) {
  dateTimeTravel <- forecastMadeOnSeq[i]
  
  res <- EN_SelectVars(rawdata = odata, alphaFix = .alphaFix,
                       dateTimeTravel = dateTimeTravel)
  
  dateAsChar <- as.character(dateTimeTravel)
  aa<-data.frame(vars=names(res$meta$resVarsSortedAbsUniq), 
                 res$meta$resVarsSortedAbsUniq)
  names(aa)[2] <- as.character(dateTimeTravel)
  dfRes <- plyr::join(dfRes, aa)
  if (keepListOfMetas) {
    listOfMetas[[dateAsChar]] <- res$meta
  }
  nUniq[i] <- res$meta$numberOfUniqVars
  nTot[i] <- res$meta$nrOfNonNullCoefs
  ## if last month, keep 
  if (i == length(forecastMadeOnSeq)) {
    attr(dfRes, "call") <- res$meta$call
    attr(dfRes, "nUniq") <- nUniq
    attr(dfRes, "nTot") <- nTot
  }
}
datapath <- paste0("./data/dfRes_",
                   attr(dfRes, "call")$rawdata, "_",
                   attr(dfRes, "call")$genNrLags, "l_",
                   attr(dfRes, "call")$alphaSearchMin, "f_",
                   attr(dfRes, "call")$alphaSearchMax, "t_",
                   as.yearmon(head(forecastMadeOnSeq, 1)), "s_",
                   as.yearmon(tail(forecastMadeOnSeq, 1)),  "f_",
                   eval(attr(dfRes, "call")$forecastHorizon),  "fch_",
                   ".RDS")
attr(dfRes, "datapath") <- datapath
message("saving ", datapath)
saveRDS(dfRes, datapath)


# F.1: Plots (rank) ------------------------------------------------------------
if (FALSE) {
  datapath <- "./data/dfRes_lodata13_1971_2011_2016_60.RDS"
  # listofObjs <- list.files("./data/", 
  #                          full.names = T)[stringr::str_starts(list.files("./data/"),
  #                                                              "dfRes")]
  # datapath <- listofObjs[i]
  dfRank <- readRDS(datapath)
  #dfRank <- dfRes
  rownames(dfRank) <- dfRank$vars
  dfRank <- dfRank[,-1]
  dfRank <- replace(dfRank, is.na(dfRank), 0)
  
  # rank by column (2)
  dfRanked <- apply(-dfRank, MARGIN = 2, rank, ties.method = "max" )
  
  dfRanked <- t(floor(dfRanked))
  dfRank_ts <- ts(dfRanked, start = as.yearmon(rownames(dfRanked)[1]) ,
                  frequency = 12)
  
  if (is.character("PLOT HEATMAP?")) {
    # pdf("./fig/fig_en_spagheti.pdf", width = 8, height = 5); 
    par(mar=c(1,1,0.2,0.2)*2.2)
    plot.ts(dfRank_ts,
            plot.type = "single",
            col = misc$colorsCode[misc$blockAsNum],
            xlab = "", ylab = "",
            lwd = 2,
            xaxt = "n", cex.lab=0.75, tck=-.01
    )
    ix <- seq(1, nrow(dfRank_ts), by=12) #every 60 days
    labs <- year(forecastMadeOnSeq)
    axis(side = 1, at = labs, labels = labs,  cex.axis = .9, tck=-.01)
    abline(h = 60, lwd=1.5)
    nUniq <- ts(nUniq, start = as.yearmon(as.character(forecastStart)), freq=12 )
    lines(nUniq, col = "black", lwd = 3, xpd=TRUE)
    points(nUniq, col = "black", ylim=c(0,120), pch = 16)
    legend("topleft", legend = paste("G:", unique(misc$blockAsNum)),
           col = misc$colorsCode,lty=1, cex=.66, horiz = F, lwd=3, 
           inset=c(2,2)/70, ncol=2, bg="white")
    # try(dev.off(), silent = T);try(dev.off(), silent = T)
  }
  #rm(dfRank)
  
  # F.2: Plots (heatmap) ---------------------------------------------------------
  dfHeat <- readRDS(datapath)
  rNames <- dfHeat$vars
  # rownames(dfHeat)
  # colnames(dfHeat)
  dfHeat <- replace(dfHeat, is.na(dfHeat), 0)
  
  dfHeat <- dfHeat %>% select(-"vars")
  dfHeat_Bin <- dfHeat %>% mutate_all(function(x) { ifelse(x>0, 1, 0) }) 
  rownames(dfHeat) <- rNames
  
  pheatmap::pheatmap(dfHeat, 
                     cluster_cols = F,
                     cluster_rows = F,
                     show_rownames = T, 
                     show_colnames = T,
                     fontsize = 5,
                     legend = F,
                     border_color = "white",
                     gaps_row = which(!is.na(misc$tableNames2)==T)[-1],
                     labels_col = as.yearmon(colnames(dfHeat)[seq(1, ncol(dfHeat),by=3)]), 
                     #color = c("white", gray.colors(20, rev=T)),
                     color = c("white", RColorBrewer::brewer.pal(9, "YlGnBu")),
                     # color = colorRampPalette(c("white", "white", "lightblue", "blue", "darkblue"))(30),
                     # filename = "fig/selectvars_prelim.pdf",
                     # main=datapath, 
                     # scale = "column"
                     #cellwidth = 5, cellheight = 4
  )
}
# F.3: Plots (selectedvars) ---------------------------------------------------------
if (FALSE) {
  dfHeat <- readRDS(datapath)
  rNames <- dfHeat$vars
  rownames(dfHeat)
  colnames(dfHeat)
  dfHeat <- replace(dfHeat, is.na(dfHeat), 0)
  # dfHeat <- dfHeat %>% mutate_all(function(x) { ifelse(x>0, 1, 0) }) 
  dfHeat <- dfHeat %>% select(-"vars")
  rownames(dfHeat) <- rNames
  
  dfSel <- dfHeat
  
  crit <- 0.1
  ylim <- c(-3,3)
  
  # check less relevant data
  sdata %>% select(rownames(dfSel[rowMeans(dfSel)<=(crit),]), yvar) %>%
    boxplot(las=2, ylim=ylim, main=datapath)
  
  # check more relevant data
  sdata %>% select(rownames(dfSel[rowMeans(dfSel)>=(1-crit),]), yvar) %>%
    boxplot(las=2, ylim=ylim, cex.axis=0.8, main=datapath)
  
  odataM <- odata %>% reshape2::melt(id.vars = "sasdate",
                                     measure.vars = names(odata)[-1])
  dataM <- data %>% reshape2::melt(id.vars = "sasdate",
                                   measure.vars = names(odata)[-1])
  sdataM <- sdata %>% reshape2::melt(id.vars = "sasdate",
                                     measure.vars = names(sdata)[-1])
  
  
  #ordering <- rownames(dfSel)[order(rowMeans(dfSel), decreasing = T)]
  crit <- 0.5
  
  sdataM %>% 
    filter(variable %in% rownames(dfSel[rowMeans(dfSel)<=(crit),]) | 
             variable == yvar) %>% 
    
    ggplot(aes(x = value, y=variable)) +
    scale_x_continuous(limits=ylim) + theme_ridges() +
    geom_density_ridges(scale=2) +
    ylab("") + xlab("") +
    ggtitle(datapath, 
            paste0("variable present ", (crit)*100, "% of time or less"))
  
  
  crit <- 0.99
  sdataM %>% 
    filter(variable %in% rownames(dfSel[rowMeans(dfSel)>=(1-crit),]) |
             variable == yvar) %>% 
    ggplot(aes(x = value, y = variable)) +
    scale_x_continuous(limits=ylim) + theme_ridges() +
    geom_density_ridges(scale=2) +
    ylab("") + xlab("") + 
    ggtitle(datapath,
            paste0("variable present ", (1-crit)*100, "% of time or more"))
}  
  ## test ab heir (simple training example) --------------------------------------
if (F) {
  splittedData <- splitTrainTestData(odata, dateTimeTravel = forecastStart %m+% months(12*3))
  xTrain <- ts(splittedData$x.train[,-1],
               start = as.yearmon(customTimeStart), frequency = 12)
  xTest <- ts(splittedData$x.test[,-1],
              start = as.yearmon(forecastStart %m+% months(1)), frequency = 12)
  yTrain <- ts(splittedData$y.train, 
               start = as.yearmon(customTimeStart), frequency = 12)
  yTest <- ts(splittedData$y.test,
              start = as.yearmon(forecastStart %m+% months(1)), frequency = 12)
  
  a<-.0002
  lseq <- 10^seq(-3,-13, length.out = 100)
  fit.cv <- cv.glmnet(x = xTrain, y = as.matrix(yTrain),
                      #lambda = lseq,
                      alpha = a)
  
  fit <- glmnet(x = xTrain, y = as.matrix(yTrain),
                alpha=a, 
                #lambda = lseq
                )
  
  plot(fit, xvar=c("norm", "lambda", "dev")[2], label=T)
  
  abline(v=log(fit.cv$lambda.min))
  abline(v=log(fit.cv$lambda.1se))
  
  # 
  # predd <- predict(fit.cv, newx = xTest)
  # fit.cv %>% plot(-1)
  # predd1 <- ts(predd, start = as.yearmon(forecastStart %m+% months(1)), frequency = 12)
  # plot.ts(cbind(predd1, yTest), plot.type = "s", col=c("blue", "red"))
  # plot.ts(predd1 - yTest, plot.type = "s")
  # abline(h=0)
}


