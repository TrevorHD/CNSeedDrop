##### Load packages and initialise data -------------------------------------------------------------------

# Load tidyverse
library(tidyverse)
library(leaps)
library(car)
library(corrplot)

# Initialise data
data <- read.csv("https://raw.githubusercontent.com/TrevorHD/CNSeedDrop/master/SeedDropData.csv")

# Treating pappus as cone-shaped, calculate vertex angle, base area, volume, and height
# Also calculate terminal velocity given drop time and fixed drop height
data %>% mutate(SeedAngle = acos(1 - ((SeedWidth^2)/(2*(SeedLength^2)))),
                SeedArea = pi*((SeedWidth/2)^2),
                SeedHeight = sqrt(SeedLength^2 - (SeedWidth/2)^2),
                SeedVol = (pi/3)*((SeedWidth/2)^2)*SeedHeight,
                TV = 1.25/DT.Avg) -> data

# Create new data set that excludes instances where pappus length, width, or DT are NA
data.new <- data[complete.cases(data[, 23:25]), ]

# Remove any rows where the quantities defined above are NaN
data.new <- data.new[-which(is.nan(data.new[, 36])), ]





##### Initial check for multicollinearity  ----------------------------------------------------------------

# Are there correlations between terms? If so, multicollinearity may be a problem.
pairs(data.new[, c("TV", "SeedLength", "SeedWidth", "SeedAngle", "SeedArea", "SeedHeight", "SeedVol")])
cor(data.new[, c("TV", "SeedLength", "SeedWidth", "SeedAngle", "SeedArea", "SeedHeight", "SeedVol")])
corrplot(cor(data.new[, c("TV", "SeedLength", "SeedWidth", "SeedAngle", "SeedArea", "SeedHeight", "SeedVol")]),
         method = "square", type = "upper")

# Looks like TV may be influenced by SeedWidth, SeedAngle, SeedArea, and SeedVol
# There will likely be a high degree of multicollinearity as well

# Generate full model with all predictors
mod.full <- lm(TV ~ SeedWidth + SeedLength + SeedArea + SeedAngle + SeedHeight + 
                    SeedVol, data = data.new)
summary(mod.full)

# VIF is overwhelmingly high
vif(mod.full)





##### Variable selection ----------------------------------------------------------------------------------

# Backward elimination, no AIC or BIC; remove "worst" predictor
mod.1 <- update(mod.full, . ~ . - SeedAngle)
summary(mod.1)
mod.1 <- update(mod.1, . ~ . - SeedArea)
summary(mod.1)

# Backward elimination with AIC produces the exact same result
step(mod.full, direction = "backward", data = data.new)

# Step in both directions with AIC produces the exact same result
step(mod.full, direction = "both", data = data.new)

# Forward selection with AIC
mod.null <- lm(TV ~ 1, data = data.new)
step(mod.null, direction = "forward", scope = list(lower = mod.null, upper = mod.full), data = data.new)
mod.2 <- lm(TV ~ SeedWidth + SeedArea, data = data.new)
summary(mod.2)

# All predictors in model 1 have a high VIF; same for model 2, but not as bad
vif(mod.1)
vif(mod.2)

# Model 2 has an R^2 that's only slightly lower than that of model 1
# Overall, model 2 seems like the best choice because of lower VIF
# We should trade a large decrease in VIF for only a small decrease in R^2

# VIF is still high, but the SeedArea term is significant when added; partial F-test confirms this
mod.3 <- lm(TV ~ SeedWidth, data = data.new)
anova(mod.2, mod.3)

# If we want to keep only two variables, SeedWidth and SeedArea are indeed the ones we want
summary(regsubsets(TV ~ SeedWidth + SeedLength + SeedArea + SeedAngle + SeedHeight + 
                        SeedVol, data = data.new))

# However, SeedWidth and SeedArea contribute to high VIF, so we will want to remove one
# Given that SeedArea ~ Seedwidth^2, then we can just express it as such and not worry about VIF
mod.4 <- lm(TV ~ SeedWidth + I(SeedWidth^2), data = data.new)

# The model with just the linear term might not be enough since its R^2 is significantly lower
summary(mod.3)
summary(mod.4)

# Also, the fit of the model with only linear SeedWidth isn't as good:
plot(TV ~ SeedWidth, data = data.new)
testdata <- data.frame(SeedWidth = seq(min(data.new$SeedWidth), max(data.new$SeedWidth), length.out = 300))
testdata$TV = predict(mod.4, newdata = testdata)
with(testdata, lines(x = SeedWidth, y = TV, col = "chocolate1", lwd = 2))
abline(mod.3, col = "deepskyblue", lwd = 2)
remove(testdata)

# Thus, model 3 (SeedWidth and SeedWidth^2) will be the best choice





##### Model diagnostics -----------------------------------------------------------------------------------

# Model 3 seems to be a decent fit
plot(TV ~ SeedWidth, data = data.new)
testdata <- data.frame(SeedWidth = seq(min(data.new$SeedWidth), max(data.new$SeedWidth), length.out = 300))
testdata$TV = predict(mod.4, newdata = newdat)
with(testdata, lines(x = SeedWidth, y = TV, col = "red"))

# No discernable pattern in standardised residuals
plot(rstandard(mod.4) ~ SeedWidth, data = data.new, ylab = "Standardised Residuals")
abline(h = 0, col = "red")
abline(h = c(-2, 2), col = "red", lty = 3)

# Other diagnostic plots seem fine
# Only a few outliers and bad leverage points, given the large amount of data
# Residuals are mostly normally distributed, with only a bit of departure at the tails
plot(mod.4)

# Curved patterns in standardised residuals for model 4
# We made the right choice by adding the quadratic term
plot(rstandard(mod.3) ~ SeedWidth, data = data.new, ylab = "Standardised Residuals")
abline(h = 0, col = "red")
abline(h = c(-2, 2), col = "red", lty = 3)

# More diagnostics for model 4
plot(mod.3)
