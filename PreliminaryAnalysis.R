##### Load packages and initialise data -------------------------------------------------------------------

# Load tidyverse
library(tidyverse)

#Initialise data
data <- read.csv("C:\\Users/Trevor Drees/Downloads/SeedDropData.csv")

# Add pappus conical volume and maximum cross-sectional area to data
data %>% mutate(SeedVol = (pi/3)*((SeedWidth/2)^2)*SeedLength,
                SeedCSA = pi*((SeedWidth/2)^2)) -> data

# Create new data set that excludes instances where pappus length, width, or DT are NA
data.new <- data[complete.cases(data[ , 23:25]), ]





##### Preliminary analyses --------------------------------------------------------------------------------

# Distribution of average DT
boxplot(data$DT.Avg)
hist(data$DT.Avg)

# Warming and mowing vs. average DT
boxplot(data$DT.Avg ~ data$Mow)
boxplot(data$DT.Avg ~ data$Warming)





##### Univariate analyses ---------------------------------------------------------------------------------

# Average DT ~ pappus length
mod.L <- lm(DT.Avg ~ SeedLength, data = data.new)
plot(DT.Avg ~ SeedLength, data = data.new)
abline(mod.L, col = "red")
summary(mod.L)
plot(rstandard(mod.L) ~ SeedLength, data = data.new)
abline(h = 0, col = "red")
abline(h = c(-2, 2), col = "red", lty = 3)

# Average DT ~ pappus width
mod.W <- lm(DT.Avg ~ SeedWidth, data = data.new)
plot(DT.Avg ~ SeedWidth, data = data.new)
abline(mod.W, col = "red")
summary(mod.W)
plot(rstandard(mod.W) ~ SeedWidth, data = data.new)
abline(h = 0, col = "red")
abline(h = c(-2, 2), col = "red", lty = 3)

# Log-transformed average DT ~ pappus width
mod.W <- lm(log(DT.Avg) ~ SeedWidth, data = data.new)
plot(log(DT.Avg) ~ SeedWidth, data = data.new)
abline(mod.W, col = "red")
summary(mod.W)
plot(rstandard(mod.W) ~ SeedWidth, data = data.new)
abline(h = 0, col = "red")
abline(h = c(-2, 2), col = "red", lty = 3)

# Square-root average DT ~ pappus width
mod.W <- lm(DT.Avg ~ sqrt(SeedWidth), data = data.new)
plot(DT.Avg ~ sqrt(SeedWidth), data = data.new)
abline(mod.W, col = "red")
summary(mod.W)
plot(rstandard(mod.W) ~ SeedWidth, data = data.new)
abline(h = 0, col = "red")
abline(h = c(-2, 2), col = "red", lty = 3)

# Average DT ~ pappus width only (w/ quadratic term)
mod.W2 <- lm(DT.Avg ~ SeedWidth + I(SeedWidth^2), data = data.new)
plot(DT.Avg ~ SeedWidth, data = data.new)
curve(predict(mod.W2, newdata = data.frame(SeedWidth = x)), add = T, col = "red")
summary(mod.W2)
anova(mod.W, mod.W2)
plot(rstandard(mod.W2) ~ SeedWidth, data = data.new)
abline(h = 0, col = "red")
abline(h = c(-2, 2), col = "red", lty = 3)

# Average DT ~ pappus conical volume
mod.V <- lm(DT.Avg ~ SeedVol, data = data.new)
plot(DT.Avg ~ SeedVol, data = data.new)
abline(mod.V, col = "red")
summary(mod.V)
plot(rstandard(mod.V) ~ SeedVol, data = data.new)
abline(h = 0, col = "red")
abline(h = c(-2, 2), col = "red", lty = 3)

# Average DT ~ pappus max circular CSA
mod.C <- lm(DT.Avg ~ SeedCSA, data = data.new)
plot(DT.Avg ~ SeedCSA, data = data.new)
abline(mod.C, col = "red")
summary(mod.C)
plot(rstandard(mod.C) ~ SeedCSA, data = data.new)
abline(h = 0, col = "red")
abline(h = c(-2, 2), col = "red", lty = 3)





##### Multivariate analyses -------------------------------------------------------------------------------

# Average DT ~ pappus width and length
summary(lm(DT.Avg ~ SeedLength + SeedWidth, data = data))
summary(lm(DT.Avg ~ SeedWidth + SeedLength, data = data))
summary(lm(DT.Avg ~ SeedWidth + SeedLength + SeedWidth:SeedLength, data = data))

# Average DT ~ warming and mowing treatments
mod.WM <- lm(DT.Avg ~ Mow + Warming + Mow:Warming, data = data)
summary(mod.WM)
anova(mod.WM)

# Average DT ~ warming and early mowing treatment
data.new.2 <- subset(data.new, Mow != "L")
mod.WM2 <- lm(DT.Avg ~ Mow + Warming, data = data.new.2)
summary(mod.WM2)
anova(mod.WM2)
