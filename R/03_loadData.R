### B: load data: ##############################################################
# select sheet:
sheetSelect <- "data"

# read csv:
rawdata <- read_xlsx("./data/fred_data.xlsx", sheet = sheetSelect)

data <- rawdata[, -1] # delete first column from import, which has no data!

# drop variables:
dropvars <- matrix(0, nrow = 1, ncol = ncol(data))
colnames(dropvars) <- colnames(data)

for (i in seq(2, ncol(data), by = 1)) { #find variables that are not usable!
  if (data[which(data[,1] == "usage"),i] == 0) {
    cat("drop variable (because timedate) -->", colnames(data[i]), "\n")
    
    dropvars[1, colnames(data[i])] <- 1
  }
}

dropvars <- t(dropvars[, !(colSums(dropvars == 0))])
dropvars <- c(colnames(dropvars))
data <- data[, -(which(colnames(data) %in% dropvars))]

varlabels   <- data[1,]
usedate     <- data[2,]
useavail    <- data[3,]
usage       <- data[4,]
slow        <- data[5,]
block       <- data[6,]
tcode       <- data[7,]
tcodebbe    <- data[8,]

# save metadata in a object to be able to open from other scripts
# generate misc object with plotting arguments
metaDataFrame <- data[1:8, ]

data        <- data[-(1:8),]

data <- as.data.frame(sapply(data, as.numeric))
data[, 1] <- seq.Date(from = as.Date(as.Date(as.numeric(data[1, "sasdate"]), origin = "1899-12-30")),
                      length.out = nrow(data), #or: to = as.Date(as.numeric(data[nrow(data), "sasdate"]), origin = "1899-12-30")
                      by = "month")

# drop variables that are not available:
dropvars <- matrix(0, nrow = 1, ncol = ncol(data))
colnames(dropvars) <- colnames(data)

# transform data to be approx. stationary and standardize data (data, odata, sdata):
tcodeuse <- tcodebbe

# check data!
odata <- transData(data, standardize = F) #odata: original stationary data
sdata <- transData(data, standardize = T) #sdata: standardized stationary data

# split sample (datetime, only split after transform due to NA!):
data <- data[c(seq(which(data[, "sasdate"] == customTimeStart),
                   which(data[, "sasdate"] == customTimeEnd), by = 1)), ]

odata <- odata[c(seq(which(odata[, "sasdate"] == customTimeStart),
                     which(odata[, "sasdate"] == customTimeEnd), by = 1)), ]

sdata <- sdata[c(seq(which(sdata[, "sasdate"] == customTimeStart),
                     which(sdata[, "sasdate"] == customTimeEnd), by = 1)), ]


### B: 2 - Generate lagged data ################################################

# TODO: gen lag data before transformation ?

l_data <- genLaggedData(data)
lodata <- genLaggedData(odata)
lsdata <- genLaggedData(sdata)

checks <- data.frame()
for (i in seq(10)) {
  checks[i,1] <- sanityCheckLagData(l_data)
  checks[i,2] <- sanityCheckLagData(lodata)
  checks[i,3] <- sanityCheckLagData(lsdata)
}

if (all(unlist(checks))) {
  message("All sanity checks ran fine")
  rm(checks)
} else {
  warning("Some sanity check went wrong")
}



### C: Generate misc metada lagged data ########################################

misc <- list()
rownames(metaDataFrame) <- metaDataFrame$sasdate

misc$numBlocks <- length(unique(
  as.numeric(filter(metaDataFrame, sasdate=="block")[-1])))

misc$blockAsNum <- as.numeric(
  filter(metaDataFrame, sasdate=="block")[-1])
  
misc$colorsCode <- RColorBrewer::brewer.pal(misc$numBlocks, "Paired")
misc$colorsCodeRainbow <- rainbow(8)
misc$colorsCodeAccent <- RColorBrewer::brewer.pal(8, "Accent")

metaDataFrame["color",] <- c("color", misc$colorsCode[misc$blockAsNum])
metaDataFrame["colorRainbow",] <- c("colorRainbow", misc$colorsCodeRainbow[misc$blockAsNum])

misc$tableNames <- c("1: Real", 
                     "2: Labor",
                     "3: Housing", 
                     "4: Consumption",
                     "5: Money", 
                     "6: Rates", 
                     "7: Prices",
                     "8: Stock")
misc$blockAsTab <- table(misc$blockAsNum)
misc$tableNames2 <- c("1: Real", rep(NA, misc$blockAsTab[1] -1), 
                      "2: Labor", rep(NA, misc$blockAsTab[2] -1), 
                      "3: Housing",  rep(NA, misc$blockAsTab[3] -1), 
                      "4: Consmp",  rep(NA, misc$blockAsTab[4] -1), 
                      "5: Money",  rep(NA, misc$blockAsTab[5] -1), 
                      "6: Rates",  rep(NA, misc$blockAsTab[6] -1), 
                      "7: Prices", rep(NA, misc$blockAsTab[7] -1), 
                      "8: Stock", rep(NA, misc$blockAsTab[8] -1))

saveRDS(misc, "./data/misc.RDS")

