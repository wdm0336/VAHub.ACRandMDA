library("sas7bdat")
library("ggplot2")
library("grid")

analy=read.sas7bdat("datafiles/mda_visual_aval.sas7bdat")
alias=data.frame(ADaM=c("GADVASR5","HAQTS","LEITS","PASIPBPS","PASITS","PVLS16","SJC66","TJC68"),
                alias=c("PatGA","HAQ-DI","LEI","BSA","PASI","Pain","SJC66","TJC68"))

# Theoretical range of each component
minmax=data.frame(GADVASR5=c(0,100),HAQTS=c(0,3),LEITS=c(0,6),PASIPBPS=c(0,100), PASITS=c(0,72),PVLS16=c(0,100),SJC66=c(0,66),TJC68=c(0,68) )

visitID=unique( analy$AVISITN )
test.study="RHAP"  # Actually, we don't need to split this dataset into 2 stidues. That's the purpose of using integrated database]
TrtID=as.character( unique( analy$TRT01P ) )
smooth.npoint=201  # Put 201 points between the theoretical min and max

## 
## smooth.array holds the density values. It is an array of 5 dimensions. The 
## dimensions are 1) the treatment, 2) either total plot, MDA only or nonMDA, 3)
## smooth.npoint total density values, 4) visit number, and 5) the test
## component
## 

smooth.array=array(NA,
                   dim = c(length(TrtID),
                           3,
                           smooth.npoint,
                           length(visitID),
                           length(alias$ADaM)),
                   dimnames = list(TrtID,
                                   c("Overall","MDA","nonMDA"),
                                   c(),
                                   paste0("V", visitID),
                                   alias$ADaM) )

for (comp in alias$ADaM) {
  cat(paste0("Calculating densities on component <", comp, ">.\n"))
  for (v in 1:length(visitID)) {
    
    # There is no data in period 2 for the LEITS component.
    if ((v == 2) & (comp == "LEITS")) {
      next
    }
  
    for (t in 1:length(TrtID)) {
      temp.component = analy[analy$AVISITN == visitID[v] &
                               analy$TRT01P == TrtID[t], c(comp, "MDAPASI")]
      
      #---- Without spliting into 2 groups: MDA+ and MDA- ----------#
      temp.component_obsAVAL = temp.component[!is.na(temp.component[, comp]), comp]
      
      # Estimate the dwnsity function using density(). Will return the estimated density at 201 pre-selected points
      temp.density_obsAVAL = density(
        temp.component_obsAVAL,
        kernel = "gaussian",
        from = minmax[1, comp],
        to = minmax[2, comp],
        n = smooth.npoint
      )
      
      smooth.array[t, "Overall", , paste0("V", visitID[v]), comp] = temp.density_obsAVAL$y * length(temp.component_obsAVAL)
      
      #---- Spliting into 2 groups: MDA+ and MDA- ----------#
      temp.component_obsPair = temp.component[complete.cases(temp.component), ]
      temp.component_obsPair = split(temp.component_obsPair[, comp], temp.component_obsPair[, "MDAPASI"])
      
      # in case there is only a few patient met (or did not meet) MDA at some visit, some treatment. By few, let's say 4.
      if (length(temp.component_obsPair$"0") > 5) {
        temp.density_obsPair0 = density(
          temp.component_obsPair$"0",
          kernel = "gaussian",
          from = minmax[1, comp],
          to = minmax[2, comp],
          n = smooth.npoint
        )
        
        smooth.array[t, "nonMDA", , v, comp] = temp.density_obsPair0$y * length(temp.component_obsPair$"0")
      } else {
        smooth.array[t, "nonMDA", , v, comp] = 0
      }
      if (length(temp.component_obsPair$"1") > 5) {
        temp.density_obsPair1 = density(
          temp.component_obsPair$"1",
          kernel = "gaussian",
          from = minmax[1, comp],
          to = minmax[2, comp],
          n = smooth.npoint
        )
        smooth.array[t, "MDA", , v, comp] = temp.density_obsPair1$y * length(temp.component_obsPair$"1")
      } else {
        smooth.array[t, "MDA", , v, comp] = 0
      }
    
    }
  }
}

#======== Static Plot =========#

DayID = 0:(24 * 7)
WeekID = c(0, 1, 2, 4, 8, 12, 16, 20, 24)

# for (comp in alias$ADaM) {
#   cat(paste0("Working on component <", comp, ">.\n"))
#   pdf(sprintf("reports/component_reports/density%s.pdf", comp),
#       height = 8,
#       width = 20)
# 
#   pushViewport(vp = viewport(layout = grid.layout(
#     nrow = 3,
#     ncol = 2,
#     widths = unit(c(1, 1), c("lines", "null")),
#     heights = unit(c(1, 1, 1), c("lines", "null", "lines"))
#   )))
# 
#   ### Print the title
# 
#   grid.text(paste("Component:", comp),
#             vp = viewport(layout.pos.col = 1:2,
#                           layout.pos.row = 1))
# 
#   ### print the treatments to the left in the first column
# 
#   pushViewport(vp = viewport(layout.pos.col = 1, layout.pos.row = 2))
#   pushViewport(vp = viewport(layout = grid.layout(nrow = length(TrtID), ncol = 1)))
# 
#   for (t in 1:length(TrtID)) {
#     grid.text(TrtID[t],
#               vp = viewport(layout.pos.col = 1,
#                             layout.pos.row = t),
#               rot = 90)
#   }
#   popViewport(2)
# 
#   ### print the Period numbers at the bottom of the last row
# 
#   pushViewport(vp = viewport(layout.pos.col = 2, layout.pos.row = 3))
#   pushViewport(vp = viewport(layout = grid.layout(ncol = length(visitID), nrow = 1)))
# 
#   for (v in 1:length(visitID)) {
#     grid.text(paste("Week", WeekID[v]),
#               vp = viewport(layout.pos.col = v,
#                             layout.pos.row = 1))
#   }
# 
#   popViewport(2)
# 
# 
#   ### Print the plots in the main plot area.
# 
#   pushViewport(vp = viewport(layout.pos.col = 2, layout.pos.row = 2))
#   pushViewport(vp = viewport(layout = grid.layout(nrow = length(TrtID), ncol = length(visitID))))
# 
#   ymax = max(smooth.array[, "Overall", , , comp], na.rm = TRUE)
# 
#   for (v in  1:length(visitID)) {
#     if ((v == 2) & (comp == "LEITS")) {
#       next
#     }
# 
#     for (t in 1:length(TrtID)) {
# 
#       dat <-
#         data.frame(
#           x = seq(minmax[1, comp], minmax[2, comp], l = smooth.npoint),
#           nonMDA = smooth.array[t, "nonMDA", , paste0("V", visitID[v]), comp] * -1,
#           MDA    = smooth.array[t, "MDA", , paste0("V", visitID[v]), comp]
#         )
# 
#       gg <- ggplot(data = dat, aes(x = x))
#       gg <-
#         gg + geom_area(
#           aes(y = nonMDA),
#           color = "black",
#           fill = "blue",
#           alpha = .5
#         )
#       gg <-
#         gg + geom_area(aes(y = MDA),
#                        color = "black",
#                        fill = "orange",
#                        alpha = .5)
#       gg <- gg + ylim(-ymax, ymax)
#       gg <- gg + coord_flip()
#       gg <- gg + theme(
#         axis.title = element_blank(),
#         panel.grid = element_blank(),
#         panel.background = element_blank()
#       )
# 
#       print(gg, vp = viewport(layout.pos.col = v,
#                               layout.pos.row = t))
# 
#     }
#   }
#   popViewport(3)
# 
#   dev.off()
# }

#======== Animation =========#
#--- create pseudo dataset ---#

Pseudo.smooth.array = array(0,
                            dim = c(length(TrtID),
                                    3,
                                    smooth.npoint,
                                    length(DayID),
                                    length(alias$ADaM)),
                            dimnames = list(TrtID,
                                            c("Overall","MDA","nonMDA"),
                                            c(),
                                            paste0("Day",DayID),
                                            alias$ADaM)
                            )


for (comp in alias$ADaM) {
  
  if (comp == "LEITS") {
    avail_weeks = c(0, 2, 4, 8, 12, 16, 20, 24)
  } else {
    avail_weeks = c(0, 1, 2, 4, 8, 12, 16, 20, 24)
  }
  
  for (d in 1:length(DayID)) {
    
    # pseudo Day match scheduled Visit Day
    if (DayID[d] %in% (avail_weeks * 7))  {
      Pseudo.smooth.array[, , , d, comp] = smooth.array[, , , which(WeekID %in% (DayID[d] / 7)), comp]
    }
    
    # pseudo Day value is a weighted avg between 2 scheduled visit
    else {
      ul_bounds = (avail_weeks * 7)[(which.max(DayID[d] < (avail_weeks * 7)) - 1):which.max(DayID[d] < (avail_weeks * 7))]
      day_diff  = diff(ul_bounds)
      Pseudo.smooth.array[, , , d, comp] =
        (abs(DayID[d] - ul_bounds[2]) / day_diff) * smooth.array[, , , which((ul_bounds[1] / 7) == WeekID), comp] +
        (abs(DayID[d] - ul_bounds[1]) / day_diff) * smooth.array[, , , which((ul_bounds[2] / 7) == WeekID), comp]
    }
    
  }
}

#---- Generate Multiple plots. One for each day ----#

numComponents <- length(alias$ADaM)
MaxPlotsAcross <- 2

for (d in 1:length(DayID)) {
  
  cat(paste0("Working on day <", d, ">.\n"))
  
  jpeg(
    sprintf("%s/reports/composite_reports/Day%04d.jpeg", getwd(), DayID[d]),
    width = 1024,
    height = 500,
    quality = 100
  )

  pushViewport(vp = viewport(layout = grid.layout(
    nrow = 2,
    ncol = 1,
    heights = unit(c(1, 1), c("lines", "null"))
  )))
  {
    grid.text(paste("Week", (DayID[d] %/% 7) + 1),
              vp = viewport(layout.pos.col = 1, layout.pos.row = 1))
    
    pushViewport(vp = viewport(layout.pos.col = 1,
                               layout.pos.row = 2))
    {
      numcol = min(numComponents, MaxPlotsAcross)
      numrow = ceiling(numComponents / MaxPlotsAcross)
      
      pushViewport(vp = viewport(layout = grid.layout(nrow = numrow, ncol = numcol)))
      
      for (comp in 1:numComponents) {
        
        ymax = max(Pseudo.smooth.array[, "Overall", , , alias$ADaM[comp]], na.rm = TRUE)
        
        pushViewport(vp = viewport(
          layout.pos.col = ((comp - 1) %% MaxPlotsAcross) + 1,
          layout.pos.row = ((comp - 1) %/% MaxPlotsAcross) + 1
        ))
        {
          pushViewport(vp = viewport(layout = grid.layout(
            nrow = 3,
            ncol = length(TrtID),
            heights = unit(c(1, 1), c("lines", "null", "lines"))
          )))
          
          grid.rect(gp = gpar(fill = NULL))
          
          {
            grid.text(alias$ADaM[comp],
                      vp = viewport(
                        layout.pos.col = 1:length(TrtID),
                        layout.pos.row = 1
                      ))
            
            for (trt in 1:length(TrtID)) {
              
              c <- alias$ADaM[comp]
              dat <-
                data.frame(
                  x = seq(minmax[1, c], minmax[2, c], l = smooth.npoint),
                  nonMDA = Pseudo.smooth.array[t, "nonMDA", , paste0("Day", DayID[d]), c] * -1,
                  MDA    = Pseudo.smooth.array[t, "MDA", , paste0("Day", DayID[d]), c]
                )
              
              gg <- ggplot(data = dat, aes(x = x))
              gg <-
                gg + geom_area(
                  aes(y = nonMDA),
                  color = "black",
                  fill = "blue",
                  alpha = .5
                )
              gg <-
                gg + geom_area(aes(y = MDA),
                               color = "black",
                               fill = "orange",
                               alpha = .5)
              gg <- gg + ylim(-ymax, ymax)
              gg <- gg + coord_flip()
              gg <- gg + theme(
                axis.title = element_blank(),
                panel.grid = element_blank(),
                panel.background = element_blank()
              )
              
              print(gg, vp = viewport(
                layout.pos.col = trt,
                layout.pos.row = 2
              ))
              
              grid.text(TrtID[trt],
                        vp = viewport(
                          layout.pos.col = trt,
                          layout.pos.row = 3
                        ))
            }
          }
          popViewport(1)
        }
        popViewport(1)
      }
      popViewport(1)
    }
    popViewport(1)
  }
  popViewport(1)
  
  dev.off()
}

