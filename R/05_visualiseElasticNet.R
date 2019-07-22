saveplotaspdf <- function(object, type, wid, hei, 
                          file_loc=paste0("./fig/fig_robs_", match.call()$object, "_",
                                          match.call()$type, ".pdf"), ...) {
  try(dev.off())
  pdf(file = file_loc, width = wid, height = hei)
  plot(object, type, ...)
  dev.off()
  invisible(plot(object, type))
}

wid <- 11
hei <- 7
widhm <- 13
heihm <- 8

## Benchmark -------------------------------------------------------------------
if (run_loops) {
  resbench <- doTheLoopEN(odata, 12)
  saveRDS(resbench, "./data/aa_resbench.RDS")
} else {
  resbench <- readRDS("./data/aa_resbench.RDS")
}
saveplotaspdf(resbench, wid = wid, hei = hei, type = "spaghetti")
saveplotaspdf(resbench,type="heatmap", wid = widhm, hei = heihm, cellw = 5, cellh = 4)

## Benchmark + 18 lags ---------------------------------------------------------

if (run_loops) {
  resbench18lags_p6alpha_60fch <- doTheLoopEN(odata, 18)
  saveRDS(resbench18lags_p6alpha_60fch, "./data/resbench18lags_p6alpha_60fch.RDS")
} else {
  resbench18 <- readRDS("./data/resbench18lags_p6alpha_60fch.RDS")
}
saveplotaspdf(resbench18lags_p6alpha_60fch, wid = wid, hei = hei, type = "spaghetti")
saveplotaspdf(resbench18lags_p6alpha_60fch,type="heatmap", wid = widhm, hei = heihm, cellw = 5, cellh = 4)

## Benchmark + 24 fch ----------------------------------------------------------
if (run_loops) {
  resbench_fch_24 <- doTheLoopEN(odata, 12, forecastHorizon=24)
  saveRDS(resbench, "./data/resbench_fch_24.RDS")
} else {
  resbench <- readRDS("./data/resbench_fch_24.RDS")
}
saveplotaspdf(resbench_fch_24, wid = wid, hei = hei, type = "spaghetti")
saveplotaspdf(resbench_fch_24,type="heatmap", wid = widhm, hei = heihm, cellw = 5, cellh = 4)

