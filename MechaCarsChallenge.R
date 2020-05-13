install.packages("tidyverse")
library(tidyverse)

MechaCar_mpg <- read.csv(file='MechaCar_mpg.csv', check.names=F, stringsAsFactors = F)
head(MechaCar_mpg)

# Rename columns of the dataset
MechaCar_mpg <- MechaCar_mpg %>% 
  rename(
    vlength = "vehicle length", 
    vweight = "vehicle weight", 
    spangle = "spoiler angle", 
    grclearance = "ground clearance")
head(MechaCar_mpg)

# Generate multiple linear regression model
lm(formula = mpg ~ vlength + vweight + spangle + grclearance + AWD, data = MechaCar_mpg)
# mpg = 6.27*vlength + 0*vweight + 0.07*spangle + 3.55*grclearance - 3.41*AWD - 104
# From this formula we can already see that vehicle length and spoiler angle have
# no (or almost no) impact on the mpg of the vehicle so we can rewrite our model:
# mpg = 6.27*vlength + 3.55*grclearance - 3.41*AWD - 104

# Generate summary statistics
summary(lm(mpg ~ vlength + vweight + spangle + grclearance + AWD,MechaCar_mpg))
# According to the summary output, the r-squared value of the multiple linear 
# regression model is 0.71, which means that roughly 71% of all mpg predictions
# will be correct while using this regression model.
# In addition, the p-value of our linear regression is 5.35e-11, which is much smaller 
# than our assumed significance level of 0.05. Therefore, we can state that there is
# sufficient evidence to reject our null hypothesis.

# Reading the Suspension Coil dataset
susp_coil <- read.csv(file='Suspension_Coil.csv', check.names=F, stringsAsFactors = F)

# Visualizing PSI distribution using density plot
ggplot(susp_coil,aes(x=PSI)) + geom_density()
# The distribution looks normal

shapiro.test(susp_coil$PSI)
# p-value is a lot less than 0.05 so we can state that the distribution of 
# the sample PSI set is normal.

summary(susp_coil$PSI) # mean = 1498.78 and median = 1500
var(susp_coil$PSI) # variance = 62.29
sd(susp_coil$PSI) # standard deviation = 7.89

# Suspension Coil T-Test
t.test(x=susp_coil$PSI,mu=1500)
