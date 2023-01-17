# Reads data directly from WEB 
filedir<-"../data/latte-12-02_en.txt"  #I cloned the entire github repository so I work with files locally
cows<-read.table(file = filedir,stringsAsFactors = FALSE,header = TRUE, sep = "\t")


######################################
#  Descriptive Statistics

milk<-cows$milk      # Select the variable 
summary(milk)        # Basic statistics
sd(milk)             # Standard Deviation function
range(milk)          # Range function
install.packages("psych")
library(psych)
describe(milk,skew = TRUE,ranges = TRUE,quant = c(0.1,0.99))
######################################
# Normality Tests
shapiro.test(milk)   # Shapiro - Wilk normality test
# a p-value = 0.04779  can be regarded as indicating a normal distribution
car::leveneTest(milk ~ as.factor(breed), data=cows) #Levene’s test
#skew below 0.48 is ok (if it is >= 0.5 then the sample could be not from a normal distribution)
#the null hypothesis of the Levene test is that variances are homogeneous
# we accept the null hypothesis because Pr(>F) = 0.5878 we have only 1 degree of freedom because the breeds are 2: "BrownSwiss" and "HolsteinFriesian"

#######################################################
#Plots
# Density plot with normal distribution curve 
plot(density(milk), col=4, main="Milk")
curve(dnorm(x, mean = mean(milk), sd = sd(milk)), add = T, col=3)
#adds to the plot the graph of a normal distribution with mu = mean(milk) and sigma = sd(milk)

# Histogram  with normal distribution curve
hist(milk, breaks = 10, freq = FALSE)
curve(dnorm(x, mean = mean(milk), sd = sd(milk)), add = T, col=3)

# Q-Q Plot (quantile-quantile plot)
qqnorm(cows$milk)
qqline(cows$milk)

# The assumptions fo running the ANOVA are verified

