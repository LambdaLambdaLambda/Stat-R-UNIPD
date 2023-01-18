#https://github.com/LambdaLambdaLambda/Stat-R-UNIPD/blob/master/scripts/anova5.md

rm(list = ls())

gain<-data.frame(trt=c("A","A","A","A","A",
                       "B","B","B","B","B",
                       "C","C","C","C","C"),
                 in_weight=c(350,400,360,350,340,
                             390,340,410,430,390,
                             400,320,330,390,420),
                 gain=c(970,1000,980,980,970,
                        990, 950,980,990,980,
                        990,940,930,1000,1000),
                 stringsAsFactors = TRUE)
gain

contrasts(gain$trt)<-contr.SAS

#gain as a function of in_weight and trt
tm<-lm(gain ~ in_weight + trt, data=gain) # Fit the linear model
summary(tm) 
fm<-aov(gain ~ in_weight + trt, data=gain) # Fit the ANOVA type I
summary(fm) #ANOVA table

#the covariate takes always one degree of freedom
#F-statistic: 19.34 on 3 and 11 DF,  p-value: 0.0001078
# there are 2 factors 

car::Anova(tm,type="III") #Anova table with SS III

TukeyHSD(fm,"trt")         # Tukey test for multiple comparisons
plot(TukeyHSD(fm,"trt"))   # Plot for tukey test
aggregate(gain$gain, by=list(gain$trt), FUN = mean) # Means by group
lsmeans::lsmeans(tm,"trt")          #LSM for treatment

library(multcomp)
par(mar=c(4,4,6,2)) # Change parameters for the plot margins
tuk <- multcomp::glht(fm, linfct=multcomp::mcp(trt="Tukey")) # Fit the general Linear Hypotheses
plot(multcomp::cld(tuk, level=0.05),col="lightgrey") # Plot the mean differences



# B-A -18.273292 -34.89314 -1.653444 0.0315687

# Only the difference between A and B is significant because the p-value is < 0.05
# and the interval [lwr, upr] does not contain 0 inside the 95% confidence interval
