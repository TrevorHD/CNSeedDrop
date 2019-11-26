##### Load packages and initialise data -------------------------------------------------------------------

# Load tidyverse
library(tidyverse)
library(leaps)
library(car)

# Initialise data
data <- read.csv("https://raw.githubusercontent.com/TrevorHD/CNSeedDrop/master/SeedDropData.csv")

# Treating pappus as cone-shaped, calculate vertex angle, base area, volume, and height
# Also calculate terminal velocity given drop time and fixed drop height
data %>% mutate(SeedAngle = acos(1 - ((SeedWidth^2)/(2*(SeedLength^2)))),
                SeedArea = pi*((SeedWidth/2)^2),
                SeedHeight = sqrt(SeedLength^2 + (SeedWidth/2)^2),
                SeedVol = (pi/3)*((SeedWidth/2)^2)*SeedHeight,
                TV = 1.25/DT.Avg) -> data

# Create new data set that excludes instances where pappus length, width, or DT are NA
data.new <- data[complete.cases(data[ , 23:25]), ]





##### Initial check for multicollinearity  ----------------------------------------------------------------

# Are there correlations between terms? If so, multicollinearity may be a problem.
pairs(data.new[, c("TV", "SeedLength", "SeedWidth", "SeedAngle", "SeedArea", "SeedHeight", "SeedVol")])
cor(data.new[, c("TV", "SeedLength", "SeedWidth", "SeedAngle", "SeedArea", "SeedHeight", "SeedVol")])

# Looks like TV may be influenced by SeedWidth, SeedAngle, SeedArea, and SeedVol
# There will likely be a high degree of multicollinearity
# Note: NAs in correlation matrix because SeedAngle has some values that are NaN

# Generate full model with all predictors
mod.full <- lm(TV ~ SeedWidth + SeedLength + SeedWidth:SeedLength + SeedArea + SeedAngle +
                 SeedHeight + SeedVol, data = data.new)
summary(mod.full)

# VIF is overwhelmingly high
vif(mod.full)





##### Variable selection ----------------------------------------------------------------------------------

# Backward elimination, no AIC or BIC; remove "worst" predictor
mod.1 <- update(mod.full, . ~ . - SeedVol)
summary(mod.1)
mod.1 <- update(mod.1, . ~ . - SeedLength)
summary(mod.1)
mod.1 <- update(mod.1, . ~ . - SeedHeight)
summary(mod.1)

# Backward elimination with AIC
step(mod.full, direction = "backward", data = data.new)
mod.2 <- lm(TV ~ SeedWidth + SeedLength + SeedArea + SeedAngle + SeedWidth:SeedLength, 
                 data = data.new)
summary(mod.2)
mod.2 <- update(mod.2, . ~ . - SeedLength)
summary(mod.2)

# Forward selection with AIC
mod.null <- lm(TV ~ 1, data = data.new)
mod.3 <- lm(TV ~ SeedWidth + SeedLength + SeedWidth:SeedLength + SeedArea + SeedAngle +
                 SeedHeight + SeedVol, data = data.new)
step(mod.null, direction = "forward", scope = list(lower = mod.null, upper = mod.3), data = data.new)
mod.3 <- lm(TV ~ SeedWidth + SeedArea, data = data.new)
summary(mod.3)

# Models 1 and 2 are the same, but all predictors have incredibly high VIF
vif(mod.1)
vif(mod.2)

# Model 3 also has high VIF; makes sense several predictors are calculated using other predictors
vif(mod.3)

# mod.3 has an R^2 that's only slightly lower than the other two models
# Overall, mod.3 seems like the best choice because of lower overall VIF
# VIF is still high, but the SeedArea term is significant when added; partial F-test confirms this
mod.4 <- lm(TV ~ SeedWidth, data = data.new)
anova(mod.4, mod.3)

# Thus, the model with just the linear term might not be enough
# The R^2 is significantly lower
summary(mod.3)
summary(mod.4)

# Also, the fit of the model with only SeedWidth isn't as good:
plot(TV ~ SeedWidth, data = data.new)
newdat <- data.frame(SeedWidth = seq(min(data.new$SeedWidth), max(data.new$SeedWidth), length.out = 300))
newdat$pred = predict(mod.5, newdata = newdat)
with(newdat, lines(x = SeedWidth, y = pred, col = "red"))
abline(mod.4, col = "blue")

# If we want to keep only two variables, SeedWidth and SeedArea are the ones we want
summary(regsubsets(TV ~ SeedWidth + SeedLength + SeedWidth:SeedLength + SeedArea + SeedAngle +
                        SeedHeight + SeedVol, data = data.new))

# However, SeedWidth and SeedArea contribute to high VIF, so we will want to remove one
# Given that SeedArea ~ Seedwidth^2, then we can just express it as such and not worry about VIF
mod.5 <- lm(TV ~ SeedWidth + I(SeedWidth^2), data = data.new)

# Is this model the same as the one with SeedArea?
# Almost, and ratio of SE/estimate is almost identical for all predictors
summary(mod.5)

# Thus, mod.5 (SeedWidth and SeedWidth^2) will be the best choice





##### Model diagnostics -----------------------------------------------------------------------------------

# Model seems to be a decent fit
plot(TV ~ SeedWidth, data = data.new)
newdat <- data.frame(SeedWidth = seq(min(data.new$SeedWidth), max(data.new$SeedWidth), length.out = 300))
newdat$pred = predict(mod.5, newdata = newdat)
with(newdat, lines(x = SeedWidth, y = pred, col = "red"))

# No discernable pattern in standardised residuals
plot(rstandard(mod.5) ~ SeedWidth, data = data.new)
abline(h = 0, col = "red")
abline(h = c(-2, 2), col = "red", lty = 3)

# Other diagnostic plots seem fine
# Only a few outliers and bad leverage points, given the large amount of data
# Residuals are mostly normally distributed, with only a bit of departure at the tails
plot(mod.5)
