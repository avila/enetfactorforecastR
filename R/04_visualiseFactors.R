### C: extract and visualize factors ###########################################
# extract factors (full sample, for exact dates check above!):
factors <- extract(sdata[, which(colnames(sdata) %!in% c("sasdate", yvar))], K = 15)
saveRDS(factors, paste0("./data/factors_", Sys.Date() ,".RDS"))
# bar plot 
if (!exists("factors")) {
  factors <- readRDS(paste0("./data/factors_", Sys.Date()-1 ,".RDS"))
}

if (T) {
  pdf("./fig/fig_fac_2d_all.pdf", width = 8, height = 20)
  par(mfrow=c(5,1), mar=c(.5,1.3,0.2,0.2)*2)
  cexx<-1.2
  
  for (i in 1:4) {
    barplot(factors$marR2[i, -(1:factors$K)],
            #main = paste("Marginal R-squares for Factor", i),
            xlab = NULL,
            #names.arg = misc$tableNames2[-which(colnames(sdata)==yvar)],
            names.arg = NA,
            col = misc$colorsCode[misc$blockAsNum][-which(colnames(sdata)==yvar)],
            ylim = c(0, 80),
            cex.axis = cexx,
            cex.names = cexx,
            las = 2,
            border=misc$colorsCode[misc$blockAsNum][-which(colnames(sdata)==yvar)])
    legend("topright", paste(" Factor", i, "    "), 
           inset=c(2,2)/70, cex=1.5)
    box()
  }
  # par(mar=c(1,1,1,1)*2)
  
  i <- 5
  barplot(factors$marR2[i, -(1:factors$K)],
          #main = paste("Marginal R-squares for Factor", i),
          names.arg = misc$tableNames2[-which(colnames(sdata)==yvar)],
          col = misc$colorsCode[misc$blockAsNum][-which(colnames(sdata)==yvar)],
          ylim = c(0, 80),
          cex.axis = cexx,
          cex.names = cexx,
          las = 2,
          border=misc$colorsCode[misc$blockAsNum][-which(colnames(sdata)==yvar)])
  legend("topright", paste("Factor", i, "    "), inset=c(2,2)/70, cex=1.5)
  legend("topleft", legend = misc$tableNames,  ###before:legend = paste("G:", unique(misc$blockAsNum))
         col = misc$colorsCode, lty=1, cex=cexx, horiz = F, lwd=5, 
         inset=c(2,2)/70, ncol=2, bg="white")
  box()
  dev.off()
}

# plot factor stability over time (use odata as data is standardized new at each point in time!):
factors3d <- estFac3d(data = odata[, which(colnames(odata) != yvar)], K = 5) #estimate factors recursively for each point in time

for (i in c(1,2,5)) {
  # height of facets
  pdf(paste0("fig/fig_fac_3d_",i,".pdf"), width = 8, height = 6)
  par(mar=c(1,1,1,1)*2)
  col.pal<-colorRampPalette(c("darkblue", "white"))
  colors<-col.pal(333)
  z<- factors3d[,,i]
  z.facet.center <- (z[-1, -1] + z[-1, -ncol(z)] + z[-nrow(z), -1] + z[-nrow(z), -ncol(z)])/4
  # Range of the facet center on a 100-scale (number of colors)
  z.facet.range<-cut(z.facet.center, 333)
  p <- persp(z = factors3d[,,i], 
             #main = paste("Marginal R-squares for Factor", i),
             zlab = "R-squared",
             xlab = paste0("Time (",
                           substr(forecastStart,1,4), "/M", substr(forecastStart, 6,7), " - ",
                           substr(forecastEnd,1,4), "/M", substr(forecastEnd, 6,7), ")"),
             ylab = paste0("Variables (1 - ", ncol(factors3d[,,i]), ")"),
             theta = 50, phi = 20, expand = 0.5, zlim = c(0,100),
             col = colors[z.facet.range], border=NA
             )
  # legend("topleft", paste("factor", i))
  dev.off()
} #3dplot of factors over time (open in new window to see anything due to grid size!)

plotObj <- plotFac3d(data = factors3d[,,], K = 5) #plotly interactive 3dplot (here K chooses the single factor to be plotted!)
saveRDS(plotObj, file = paste0("./data/plot3D_", Sys.Date(), ".RDS")) # save plot object to be able to
# check it without running code again
plotObj




