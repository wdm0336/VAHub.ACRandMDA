# Eli Lilly and Company (required)-   GSS
# CODE NAME (required)              : <source code location>
# PROJECT NAME (required)           : Animation Plot Function Tool
# DESCRIPTION (required)            : Create a function myplot that makes frames for IXE animation utilizing the animator tool
# SPECIFICATIONS(required)          : <specification file location>
# VALIDATION TYPE (required)        : Peer Review
# INDEPENDENT REPLICATION (required): N/A, validated using Peer Review type
# ORIGINAL CODE (required)          : N/A, this is the original code
# COMPONENT CODE MODULES            : None
# SOFTWARE/VERSION# (required)      : R version 3.2.0; dplyr_0.4.1; ggplot2_1.0.1; grid_3.2.0 
# INFRASTRUCTURE                    : Platform: x86_64-unknown-linux-gnu (64-bit)
#                                   : Running under: Red Hat Enterprise Linux Server release 6.6 (Santiago)
# DATA INPUT                        : <data file location>
# OUTPUT                            : One frame
# SPECIAL INSTRUCTIONS              : No special instructions
# -------------------------------------------------------------------------------------------------------------------------------         
#   -------------------------------------------------------------------------------------------------------------------------------
#   DOCUMENTATION AND REVISION HISTORY SECTION (required):
#   
#   Author &
#   Ver# Validator        Code History Description
# ---- ----------------     -----------------------------------------------------------------------------------------------------
# 1.0   William Martersteck   (Author)       Original version of the code
#       <validator>
# --------------------------------------------------------------------------------------------------------------------------------


myPlot <- function(data){
  
  ndays = 3
  MaxPlotsAcross = 4

  # plot.graph takes in the data associated with one compound and creates the
  # plots. These graphs are created without the frequency rectangle. They are
  # added later.
  plot.graph <- function(df) {
   
    library(beanplot)

    beanplot(df$comp1 ~ df$mda, side="both",
             what = c(0,1,0,0),
             col = list("light blue", "light green"),
             ylim = c(0, 150),
             axes = FALSE,
             frame.plot = FALSE,
             show.names = FALSE,
             log = '')

  }
  
  par(xpd = NA)

  numcol = min(ndays, MaxPlotsAcross)
  numrow = ceiling(ndays / MaxPlotsAcross)

  layout(matrix(c(1:(numcol*numrow*3)), ncol = numcol*3, nrow = numrow, byrow = TRUE),
         heights = rep(1, numrow),
         widths = rep(c(1,3,3), numcol))
  
  for (d in 1:ndays) {
    par(mar = c(1, 3, 1, 0))
    plot(x = 0, y = 0,
         axes = FALSE,
         xlim = c(0,0),
         ylim = c(0, 150),
         col = "white",
         xlab = "",
         ylab = "")
    axis(2)
    par(mar = c(1, 1, 1, 1))
    plot.graph(df = data[data$trt == 0 & data$day == d,])
    plot.graph(df = data[data$trt == 1 & data$day == d,])
  }
  
  for (d in 0:(ndays - 1)) {
    row = numrow - (d %/% numcol) - 1
    col = d %% numcol
    
    rect(grconvertX(col / numcol, from='ndc'), grconvertY(row / numrow, from='ndc'),
         grconvertX((col+1) / numcol, from='ndc'), grconvertY((row+1) / numrow, from='ndc'))
    
    text(x = grconvertX((col / numcol) + (.5/numcol), from='ndc'),
         y = grconvertY((row / numrow) + (.9/numrow), from='ndc'),
         "Title")
  }
}


