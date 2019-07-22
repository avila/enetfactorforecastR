
rm(list = ls())
install <- F
load.pkg <- T
runVisualiseFactors <- T
runVisualiseElasticNet <- T
runForecasting <- T
run_loops <- T # inside visualizeElasticNet

## Preprocessing ---------------------------------------------------------------

source("./R/00_preprocessing.R")

### A.2: define fundamental functions ------------------------------------------
source("./R/01_defineFunctions.R")

### A.3: select baseline parameters: -------------------------------------------
source("./R/02_baselineParameters.R")

### B: load data: --------------------------------------------------------------
source("./R/03_loadData.R")

## Visualisazion  --------------------------------------------------------------

### C: extract and visualize factors -------------------------------------------
if (runVisualiseFactors) { source("./R/04_visualiseFactors.R") }

### D: train and visualize Elastic Net -----------------------------------------
if (runVisualiseElasticNet) { source("./R/05_visualiseElasticNet.R") }

## Forecasting  ----------------------------------------------------------------
if (runForecasting) { source("./R/06_forecasting.R") }


