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


myPlot <- function(dat){

  components = c("GADVASR5", "HAQTS", "LEITS", "PASIPBPS", "PASITS", "PVLS16", "SJC66", "TJC68")

  MaxPlotsAcross = 2
  numComponents = length(components)

  library(beanplot)
  library(grid)
  library(gridBase)
  library(dplyr)
  
  thresh = data.frame(
    GADVASR5 = 20,
    HAQTS = .5,
    LEITS = 1,
    PASIPBPS = 3,
    PASITS = 1,
    PVLS16 = 15,
    SJC66 = 1,
    TJC68 = 1
  )
  
  ymaxs = data.frame(
    GADVASR5 = 100,
    HAQTS = 3,
    LEITS = 6,
    PASIPBPS = 90,
    PASITS = 60,
    PVLS16 = 100,
    SJC66 = 70,
    TJC68 = 70
  )
  
  dat = dat %>% mutate(GADVASR5.ach = (!is.na(GADVASR5) & (GADVASR5 <= thresh$GADVASR5)))
  dat = dat %>% mutate(HAQTS.ach = (!is.na(HAQTS) & (HAQTS <= thresh$HAQTS)))
  dat = dat %>% mutate(LEITS.ach = (!is.na(LEITS) & (LEITS <= thresh$LEITS)))
  dat = dat %>% mutate(PASIPBPS.ach = (!is.na(PASIPBPS) & (PASIPBPS <= thresh$PASIPBPS)))
  dat = dat %>% mutate(PASITS.ach = (!is.na(PASITS) & (PASITS <= thresh$PASITS)))
  dat = dat %>% mutate(PVLS16.ach = (!is.na(PVLS16) & (PVLS16 <= thresh$PVLS16)))
  dat = dat %>% mutate(SJC66.ach = (!is.na(SJC66) & (SJC66 <= thresh$SJC66)))
  dat = dat %>% mutate(TJC68.ach = (!is.na(TJC68) & (TJC68 <= thresh$TJC68)))
  dat = dat %>% mutate(PASI.ach = PASIPBPS.ach | PASITS.ach)
  
  d <- dat %>% select(GADVASR5.ach, HAQTS.ach, LEITS.ach, PASI.ach, PVLS16.ach, SJC66.ach, TJC68.ach)
  dat = dat %>% mutate(MDA.ach = rowSums(d) >= 5)
  
  dat$trtp <- factor(dat$trtp)

  browser()
  # plot.graph takes in the dat associated with one compound and creates the
  # plots. These graphs are created without the frequency rectangle. They are
  # added later.
  plot.graph <- function(value, mda.achieved, ymax) {
    beanplot(value ~ mda.achieved, side="both",
             what = c(0,1,0,0),
             col = list("light blue", "light green"),
             ylim = c(0, ymax),
             frame.plot = FALSE,
             show.names = FALSE,
             axes = TRUE,
             bw="nrd0",
             log = '')
  }
  
  plotComponentByTrt <- function(dat, component, treatments) {
    
    pushViewport(vp = viewport(layout = grid.layout(
      nrow = 3,
      ncol = length(treatments),
      heights = unit(c(1, 1), c("lines", "null", "lines"))
    )))
    
    grid.rect(gp=gpar(fill = NULL))
    
    {
      grid.text(component,
                vp = viewport(layout.pos.col = 1:length(treatments), layout.pos.row = 1))
      
      for (trt in 1:length(treatments)) {
        pushViewport(vp = viewport(layout.pos.col = trt, layout.pos.row = 2))
        {
          par(new = TRUE, fig = gridFIG())
          par(mar = c(5, 3, 1, 1))
          # par(mar = c(1,2,1,1))
          plot.graph(value = dat[dat$trtp == treatments[trt], component],
                     mda.achieved = dat[dat$trtp == treatments[trt], "MDA.ach"],
                     ymax = ymaxs[component])
          axis(2)
        }
        popViewport(1)
        grid.text(treatments[trt], vp = viewport(layout.pos.col = trt, layout.pos.row = 3))
        
        
      }
    }
    popViewport(1)
  }
  
  numcol = min(numComponents, MaxPlotsAcross)
  numrow = ceiling(numComponents / MaxPlotsAcross)
  
  pushViewport(vp = viewport(layout = grid.layout(nrow = numrow, ncol = numcol)))
  
  for (comp in 1:numComponents) {
    
    pushViewport(vp = viewport(layout.pos.col = ((comp - 1) %% MaxPlotsAcross) + 1,
                               layout.pos.row = ((comp - 1) %/% MaxPlotsAcross) + 1))
    
    plotComponentByTrt(dat = dat, component = components[comp], treatments = levels(dat$trtp))
    
    popViewport(1)
  }
  
  popViewport(1)
}


