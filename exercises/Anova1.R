#https://github.com/LambdaLambdaLambda/Stat-R-UNIPD/blob/master/scripts/anova1.md
# Example 1, One Way ANOVA
# Create Dataframe
foods<-data.frame(milk=c(25.40,26.31,24.10,23.74,25.10,
                         23.40,21.80,23.50,22.75,21.60,
                         20.00,22.20,19.75,20.60,20.40),
                  food=c("Food1","Food1","Food1","Food1","Food1",
                         "Food2","Food2","Food2","Food2","Food2",
                         "Food3","Food3","Food3","Food3","Food3"),
                  stringsAsFactors = TRUE)

# ANOVA test
tm<-lm(milk ~ food, data = foods)  # fit a linear model to the independent variable food 
summary(tm)                         # ANOVA table

fm<-aov(milk ~ food, data = foods)  # fit the ANOVA (there are different types of ANOVA) 
summary(fm)                         # 

#Mean Sq is equal to Sum Sq divided by Df  (degrees of freedom)
# The number Multiple R-squared:  0.8101
# is obtained as 0.8101 = (47.16)/(47.16+11.05)
#F-statistic:  25.6 on 2 and 12 DF,  p-value: 4.684e-05
# the observations are 14 and the groups are 2
# The test reveals that the diet is significant (but we still don't know which diet gives highest
# milk production difference)

# we see that the median is 0.01 (almost 0) and the 1st and 3rd quartiles are almost centered
# around the median


library(multcomp)
par(mar=c(5,4,6,2))           # Change parameters for the plot margins
tuk <- glht(fm, linfct=mcp(food="Tukey")) # Fit the general Linear Hypotheses
plot(cld(tuk, level=0.05),col="lightgrey")   # Plot the mean differences



# Multiple comparisons

TukeyHSD(fm)                  # Tukey test for multiple comparisons on the results of the ANOVA
plot(TukeyHSD(fm))            # Plot for tukey test
aggregate(foods$milk, by=list(foods$food), FUN = mean) # Means by group

