### E: forecasting (under construction) ##############################################

# test <- EN_SelectVars(odata[,-1], dateTimeTravel = forecastStart)

#test <- seq.Date(from = forecastStart, to = forecastEnd, by = "month")
#class(test)

#fcTest <- function(data = ...,
#                   fcstart = forecastStart,
#                   fcend = forecastEnd,
#                   by = "month",
#                   fchorizon = 24,
#                   center = T,
#                   scale = T, 
#                   verbose=T) { #start function
#  
#  for (t in seq.Date(from = fcstart, to = fcend, by = by)) { #start for-loop I
#    print(as.Date(t))
#   
#    dateSeq.train <- as.Date(c(seq.Date(from = customTimeStart, to = as.Date(t), by = by)))
#    dateSeq.test  <- as.Date(c(seq.Date(from = as.Date(t)+1, to = as.Date(t)+fchorizon, by = by)))
#    
#    ndata.train <- cbind(as.Date(dateSeq.train), scale(data[which(data[,1] <= as.Date(t)),][,-1], center = center, scale = scale))
#    ndata.test  <- cbind(as.Date(dateSeq.test), scale(data[which(data[,1] > as.Date(t)), ][,-1], center = center, scale = scale))
#    
#    ## data needs to contain yvar. NOT lagged! 
#    EN_results_names <- EN_SelectVars(rawdata = data, dateTimeTravel = t, ..yvar = yvar,
 #                                     forecastHorizon = 60, genNrLags = 12, verbose = T)$meta$resVarsNamesUniq
 #   # NOTE: the object that stores the unique selecter vars is resVarsNamesUniq 
 #   # and not: resVarsNamesClean (this contains lagged vars too)
 #   
#    # NOTE: do you want to have the sasdata there as well?
#    # NOTE: add select(-sasdate) in the selecter term, if not needed
#    en_data <- data %>% filter(sasdate <= t) %>% select(sasdate, EN_results_names)
#    
#    if (verbose) {
#      message("dim en_data: ", dim(en_data)[1]," by ",  dim(en_data)[2])
#    }
#    
#    
#    # ...
#    
#  } #end for-loop I
##  
##  
##  return(list(train = ndata.train,
##              test = ndata.test))
##  
##} #end function

# debugonce(fcTest)

yvars <- yvar
splittedData <- splitTrainTestData(odata, dateTimeTravel = "2019-01-01",
                                  verbose = F, .yvar = yvar,forecastHorizon = 0)

benchmark <-fcTest(data = odata)
fac_forecastSW <- fcSW(ydata = as.data.frame(cbind(sasdate=splittedData$x.train[,1], CPIAUCSL=splittedData$y.train)), 
                       xdata = splittedData$x.train)

fac_forecastSW
benchmark
