library(dplyr)

numSubjects = 1000

numDays = 10

usubjid = 1:numSubjects

df <- expand.grid(usubjid, 1:numDays)

colnames(df) <- c("usubjid", "day")

df$trt <- ifelse(df$usubjid <= numSubjects/2, 0, 1)

calc_comp <- function(trt, day) {
  mean <- ifelse(trt == 0, 20, 20 + (day-1)*10)
  sd = 3
  
  rnorm(1, mean, sd)
}

df$comp1 <- mapply(calc_comp, df$trt, df$day)
mda <- data.frame(mda = sample(0:1, numSubjects, replace = TRUE), usubjid = 1:numSubjects)
df <- inner_join(df, mda, by = "usubjid")


write.csv(df, "datafiles/testdata.csv", row.names = FALSE)
