# SRS -ratio estimation & regression estimation
library("readxl")
N <- 5996
consumption <- read_excel("consumptionUBC.xlsx")

# clean data
consumption$`Cost(month)` <- as.numeric(consumption$`Cost(month)`)
consumption$`Housing(month)` <- as.numeric(consumption$`Housing(month)` )
consumption$`Diet(month)` <- as.numeric(consumption$`Diet(month)`)
consumption$`Hobby(month)` <- as.numeric(consumption$`Hobby(month)`)
consumption$`dailyNecessities(month)` <- as.numeric(
  consumption$`dailyNecessities(month)`)
consumption$`LearningMaterials(term)` <- as.numeric(
  consumption$`LearningMaterials(term)`)
consumption <- na.omit(consumption)
y.sample <- consumption$`Cost(month)`
n <- length(y.sample)

# choose auxiliary variable
cor(consumption$`Housing(month)`, y.sample) # 0.1739406 
cor(consumption$`Diet(month)`, y.sample) # 0.9502347 strong
cor(consumption$`Hobby(month)`, y.sample) # 0.437956
cor(consumption$`dailyNecessities(month)`, y.sample) # 0.6286485
cor(consumption$`LearningMaterials(term)` / 3, y.sample) # 0.4711414
x.sample <- consumption$`Diet(month)`
plot(x.sample, y.sample)
xbar.pop <- 5541 / 6 # cost on diet is $5541 per year (2 terms & 6 months)

# ratio estimation
ybar.srs <- mean(y.sample)
xbar.srs <- mean(x.sample)
ybar.ratio <- (ybar.srs / xbar.srs) * xbar.pop      # ratio estimation
e.ratio <- y.sample - (ybar.srs / xbar.srs) * x.sample
s.e.sq <- 1 / (n-1) * sum(e.ratio^2)
se.ratio <- sqrt((1 - n/N) * (s.e.sq / n))
srs.ratio <- c(ybar.ratio, se.ratio)
CI.srs.ratio <- ybar.ratio + 1.96 * c(-se.ratio, se.ratio) 

# regression estimation
lm.regression <- lm(y.sample~x.sample)
summary(lm.regression)
ybar.regression <- 2.9457 * xbar.pop + 67.0118
e.regression <- resid(lm.regression)
se.regression <- sqrt((1 - n/N)*(var(e.regression) / n))
srs.regression <- c(ybar.regression, se.regression)
CI.srs.regression <- ybar.regression + 1.96 * c(-se.regression, se.regression) 

# result
srs.ratio
CI.srs.ratio
srs.regression
CI.srs.regression
 
