# SRS -vanilla estimation
library("readxl")
N <- 5996
consumption <- read_excel("consumptionUBC.xlsx")

# clean data
consumption$`Expenses(month)` <- as.numeric(consumption$`Expenses(month)`)
consumption <- na.omit(consumption)
y.sample <- consumption$`Expenses(month)`
n <- length(y.sample)

# vanilla estimate
ybar.srs <- mean(y.sample)
se.srs <- sqrt((1 - n / N) * var(y.sample) / n)
srs.vanilla <- c(ybar.srs, se.srs)
CI.srs.vanilla <- ybar.srs + 1.96 * c(-se.srs, se.srs) 

# result
srs.vanilla
CI.srs.vanilla

