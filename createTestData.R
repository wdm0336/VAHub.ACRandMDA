library(dplyr)

numSubjects = 1000
numDays = 10
numComponents = 4


df <- expand.grid(usubjid = 1:numSubjects, day = 1:numDays, component = 1:numComponents)

df$trt <- ifelse(df$usubjid <= numSubjects/2, 0, 1)

calc_comp_val <- function(trt, day) {
  mean <- ifelse(trt == 0, 20, 20 + (day-1)*10)
  sd = 3
  
  rnorm(1, mean, sd)
}

df$value <- mapply(calc_comp_val, df$trt, df$day)

mda <- data.frame(mda = sample(0:1, numSubjects, replace = TRUE), usubjid = 1:numSubjects)
df <- inner_join(df, mda, by = "usubjid")


write.csv(df, "datafiles/testdata.csv", row.names = FALSE)
