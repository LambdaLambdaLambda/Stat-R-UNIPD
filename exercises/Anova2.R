#https://github.com/LambdaLambdaLambda/Stat-R-UNIPD/blob/master/scripts/anova2.md
#Complete Randomized Block Design
#18 gennaio 2023, mattina

rm(list = ls())
# Create Dataframe
trt=c("T1","T1","T1","T1","T1","T2","T2","T2","T2","T2","T3","T3","T3","T3","T3")
litter=c("1","2","3","4","5","1","2","3","4","5","1","2","3","4","5")
y=c(7.86,8.00,7.93,7.62,7.81,7.76,7.73,7.74,7.43,7.44,7.46,7.68,7.51,7.21,7.42)
#y is the response variable

rcbd<-data.frame(trt,
                 litter,
                 y,
                 stringsAsFactors = TRUE)

fm<-aov(y ~ trt + litter, data = rcbd) # Fit the ANOVA
summary(fm)                            # ANOVA Table

tm<-lm(y ~  trt + litter, data = rcbd) # Fit the linear model
summary(tm)                            # Linear Model Summary

# Before doing the ANOVA we should perform the normality tests (Shapiro, Levene)
# the results give Multiple R-squared:  0.9484,	Adjusted R-squared:  0.9097 
#That indicates that there is a significance but we still do not know which factors give the largest
# significance
# The p-value: 9.763e-05 of the F-statistic tells us that there is a high significance of at least one factor
# in the response variable

library(multcomp)

TukeyHSD(fm,"trt")         # Tukey test for the treatment factor

# This gives   
# diff  lwr         upr     p adj
#T2-T1 -0.224 -0.3447362 -0.10326382 0.0018520
#T3-T1 -0.388 -0.5087362 -0.26726382 0.0000418
#T3-T2 -0.164 -0.2847362 -0.04326382 0.0115281

#since all [lwr, upr] intervals do not include 0, every difference is significant (even though
# with different significance values)

plot(TukeyHSD(fm,"trt"))   # Plot for tukey test
aggregate(rcbd$y, by=list(rcbd$trt), FUN = mean) # Means by group

par(mar=c(5,4,6,2)) # Change parameters for the plot margins
tuk <- glht(fm, linfct=mcp(trt="Tukey")) # Fit the general Linear Hypotheses
plot(cld(tuk, level=0.01),col="lightgrey") # Plot the mean differences

# with a level of significance of 0.01 the difference T3-T2 is not significant enough anymore

#There are three kinds of ANOVA:
# Type I gives more importance to the leftmost column corresponding to a factor
# the function aov performs ANOVA type I
# Type III gives the same importance to every column corresponding to a factor
# the function car performs ANOVA type III

library(car)
fm3 <- Anova(lm(y ~ trt + litter, data=rcbd), type=3)
summary(fm3) 
Anova(lm(y ~ trt + litter, data=rcbd), type=3)




