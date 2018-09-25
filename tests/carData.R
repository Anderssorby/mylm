install.packages("car")
library(car)
library(mylm)
data(SLID, package = "carData")
SLID <- SLID[complete.cases(SLID), ]
reg <- mylm(wages ~ education + age , SLID)
reg2 <- lm(wages ~ education + age, SLID)

print(reg2)
print(reg)


summary(reg)
summary(reg2)

matrix(c(reg$beta,sqrt(diag(reg$covar)),reg$statistics,reg$pvalues),ncol = 4)
