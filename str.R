# Stratified Sample with Proportional Allocation
# male chinese students:female Chinese students = 45:55 in UBC
# hence we sampled 23 male and 27 female
library("readxl")
N <- 5996
N.male <- 2698
N.female <- 3298
w.male <- 0.45
w.female <- 0.55
consumption <- read_excel("consumptionUBCstr.xlsx")
row.indices.male <- which(consumption$Gender == 'male')
row.indices.female <- which(consumption$Gender == 'female')
male.sample <- consumption[row.indices.male, ]
female.sample <- consumption[row.indices.female, ]

# clean data
male.sample$`Expenses(month)` = as.numeric(male.sample$`Expenses(month)`)
female.sample$`Expenses(month)` = as.numeric(female.sample$`Expenses(month)`)
male.sample <- na.omit(male.sample)
female.sample <- na.omit(female.sample)
y.male.sample <- male.sample$`Expenses(month)`
y.female.sample <- female.sample$`Expenses(month)`
n.male <- length(y.male.sample)
n.female <- length(y.female.sample)

# estimation
ybar.male <- mean(y.male.sample)
ybar.female <- mean(y.female.sample)
ybar.str <- w.male*ybar.male + w.female*ybar.female
se.male <- sqrt((1 -n.male / N.male) * var(y.male.sample) / n.male) 
se.female <- sqrt((1 -n.female / N.female) * var(y.female.sample) / n.female)
se.str <-sqrt((N.male / N)^2 * se.male^2 + (N.female / N)^2 * se.female^2)
str <- c(ybar.str, se.str)
CI.str <- ybar.str + 1.96 * c(-se.str, se.str)
str
CI.str
