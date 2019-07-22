### I: load libraries: #########################################################

libs <- c("readxl", "dplyr", "plyr", "zoo", "svMisc", "vars", "Jmisc",
          "dlm", "ggplot2", "ggfortify", "progress", "xts", "FarmSelect",
          "POET", "nFactors", "devtools", "copulaedas", "forecast", "astsa",
          "glmnet", "plotly", "lubridate", "caret",  "MTS", "vars", "dynlm")

# invert order of libs, to avoid masking functions of more important 
# libraries (here: plotly's select was messing around with dplyr's) 
libs <- libs[length(libs):1]

if (install == T) {
  new.pkgs <- c()
  for (i in 1:length(libs)) {
    if (libs[i] %in% installed.packages() == F) {
      new.pkgs[i] <- libs[i]
      if (is.null(new.pkgs) == T) {
        cat("no packages needed to be installed", "\n")
      } else if (!is.null(new.pkgs) == T){
        cat("some packages need to be installed:", length(new.pkgs), "\n")
      }
    }
  }
  install.packages(new.pkgs, dependencies = T)
  if (load.pkg == T) {
    for (i in 1:length(libs)) {
      library(libs[i]) 
    }
  }
} else if (install == F) {
  if (load.pkg == T) {
    for (i in 1:length(libs)) {
      sapply(libs, library, character.only = T) 
    }
  }
}

### A.1: set working directory: ################################################

# save standard par() parameters:
oldParSettings <- par()


if (!dir.exists("fig")) {
  message("create ./fig dir")
  dir.create("./fig", )
} else {
  message("./fig already there!")
}

