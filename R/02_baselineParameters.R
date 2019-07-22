### A.3: select parameters: ####################################################

# select transformation codes to be used:
tcodeselec <- "tcodebbe" # 'tcodebbe' for Bernanke, Boivon and Eliasz (2005, QJE) and 'tcode' for McCracken and Ng (2014, JBES)

# select sample size (SW: 1970 - 1998):
customTimeStart <- as.Date("1970-01-01")
customTimeEnd <- as.Date("2018-05-01")

forecastStart <- as.Date("2001-12-01") # time travel: first forecast: on dec 2001 for jan 2002
forecastEnd <- as.Date("2016-05-01") #end of dataset!
forecastMadeOnSeq <- seq.Date(from = forecastStart, to = forecastEnd, by = "month") # month in which forecas is made


# select variable to be forecasted:
yvar <- "CPIAUCSL" # or: CPIULFSL (alternative)

