# Eli Lilly and Company (required)-   GSS
# CODE NAME (required)              : <source code location>
# PROJECT NAME (required)           : IXE efficacy animation 2.2
# DESCRIPTION (required)            : Script performs error checks on the data to ensure it is
#                                     in the format and content the visualization function requires.
# SPECIFICATIONS(required)          : <spectification file location>
# VALIDATION TYPE (required)        : Peer Review
# INDEPENDENT REPLICATION (required): N/A, validated using Peer Review type
# ORIGINAL CODE (required)          : N/A, this is the original code
# COMPONENT CODE MODULES            : None
# SOFTWARE/VERSION# (required)      : R version 3.2.0; dplyr_0.4.1
# INFRASTRUCTURE                    : Platform: x86_64-unknown-linux-gnu (64-bit)
#                                   : Running under: Red Hat Enterprise Linux Server release 6.6 (Santiago)
# DATA INPUT                        : <data file location>
# OUTPUT                            : report of Data errors displayed on the console.
# SPECIAL INSTRUCTIONS              : No special instructions
# -------------------------------------------------------------------------------------------------------------------------------         
#   -------------------------------------------------------------------------------------------------------------------------------
#   DOCUMENTATION AND REVISION HISTORY SECTION (required):
#   
#   Author &
#   Ver# Validator        Code History Description
# ---- ----------------     -----------------------------------------------------------------------------------------------------
# 1.0   William Martersteck  (Author)       Original version of the code
#       Anastasia Alexeeva   (Validator)
# --------------------------------------------------------------------------------------------------------------------------------

data_file = "datafiles/testdata.csv"

df <- read.csv(data_file)

# When this file is sourced, a function called testPlot() is created that can be
# called from the console. This function will source and call the plot function
# with a random day from the data. It is used in debugging. If you change the
# plot function, you can save it and then call testPlot() and it will run the
# newly saved plot function. You can pass in a number and it will use that as
# the time if you would like.
testPlot <- function(timenum = -1) {
  require(grid)
  
  source("plotMDA.R")
  
  if (timenum == -1) {
    timenum = sample(df$day, size = 1)
  }

  if (!(timenum %in% df$day)) {
    print(paste0("Given time number (", timenum,
                ") not in data. Available times are :"))
    print(paste(unique(df$day)))
    stop("Invalid time number")
  }
  grid.newpage()
  myPlot(data = df)
}

