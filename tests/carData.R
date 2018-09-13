#install.packages("car")
library(car)
library(mylm)
data(SLID, package = "carData")
SLID <- SLID[complete.cases(SLID), ]
reg <- mylm(wages ~ sex + language + age + education, SLID)
reg2 <- lm(wages ~ sex + language + age + education, SLID)

print(reg)
summary(reg)

plot(reg)
