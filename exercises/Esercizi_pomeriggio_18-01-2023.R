data("PlantGrowth")
dim(PlantGrowth)
library(dplyr)
library(psych)
library(car)
library(multcomp)

# The R dataset contains results from an experiment to compare yields 
# (as measured by dried weight of plants) obtained under a control and
# two different treatment conditions. It is a data frame of 30 cases on 2 variables.

# The response variable is given by the column weight.

################################################################################
#1. compute summary statistics using the function describe within psych package
#2. check normality with Shapiro test
#3. check homogeneity of variances according to group factor with the Levene test
#4. Plot the distribution using plot or histogram

PlantGrowth
weight <- PlantGrowth$weight
summary(weight)     # Basic statistics
sd(weight)          # Standard Deviation function
mean(weight)          # Mean function
range(weight)       # Range function
describe(weight, skew = TRUE, ranges = TRUE, quant = c(0.1,0.99))

# The kurtosis index indicates that the distribution of the response variable data 
# is not skewed.
# The sample mean is 5.07, and the median is 5.15. The fact that these values are close 
# to one another indicates a balanced distribution of the data, centered around the mean.

plot(density(weight), col=1, main="Weight") #guardare come cambiare scala per il plot
curve(dnorm(x, mean = mean(weight), sd = sd(milk)), add = T, col=3)
hist(milk, breaks = 10, freq = FALSE)
curve(dnorm(x, mean = mean(weight), sd = sd(weight)), add = T, col=3)

shapiro.test(weight)   # Shapiro - Wilk normality test

# The Wilk-Shapiro test has as null hypothesis the assertion that data is normally distributed
# since we get p-value = 0.8915 > 0.05 we accept the null hypothesis. i.e. weight values are
# normally distributed in the population of plants

car::leveneTest(weight ~ as.factor(group), data=PlantGrowth) #Levene’s test

# The Levene test has as null hypothesis the assertion that the variances are homogeneous
# across the two factors.
# Since we get p-value = 0.3412 > 0.05 we accept the null hypothesis. i.e. variance in the response variable does not depend on the
# values of the group factor.

# Q-Q Plot (quantile-quantile plot)

qqnorm(weight)
qqline(weight)

# The quantile-quantile plot shows that observations are concentrated between -2 and +2 standard
# deviations from the mean, revealing no outliers in the dataset. Moreover observations 
# fit nicely a straight line that correlates values of observations as a function of their 
# computed quantiles.

################################################################################
#5. Run a linear model for your response variable and group as factor

lin_mod <- lm(weight ~ group, data=PlantGrowth)
summary(lin_mod)

################################################################################
#6. Run one-way ANOVA (type 1)

type_I_anova <- aov(weight ~ group, data=PlantGrowth)
summary(type_I_anova)
type_I_anova

################################################################################
#7. Run one-way ANOVA  (type 3)

type_III_anova <- Anova(lm(weight ~ group, data=PlantGrowth), type=3)
summary(type_III_anova)
type_III_anova

################################################################################
#8. Run the Tukey test for multiple comparison

tukey <- TukeyHSD(type_I_anova, "group")
tukey

################################################################################
#9. Plot the results of the Tukey-test

plot(tukey)   # Plot for tukey test
group <- PlantGrowth$group
aggregate(weight, by=list(group), FUN = mean) # Means by group
par(mar=c(5,4,6,2)) # Change parameters for the plot margins
tuk <- glht(type_I_anova, linfct=mcp(group="Tukey")) # Fit the general Linear Hypotheses
plot(cld(tuk, level=0.01), col="lightgrey") # Plot the mean differences
