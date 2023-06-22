#Lead Concentration project 
# Question 0 : Data Reading 
lead =get(load("C:/Users/LENOVO/Desktop/BMD 407/Project/lead.RData"))
ls(lead)
#-----------------------------------------------------------------------------------------------------

#Question 1: Descriptive Statistics 
#preprocessing 
#in sex coloumn : Assuming the male = 1, female =2 
#in Exposed column : Assuming yes is 1 and no is 0 
lead$Sex <- factor(lead$Sex , labels = c('Male','Female'))
lead$Area <- factor(lead$Area, labels = c('El paso','Texas', 'USA'))
lead$Lead_type <- factor(lead$Lead_type, labels = c('Ld72','Ld73'))
lead$Exposed <- factor(lead$Exposed, labels = c('NO','YES'))

#1.1 : summary
summary(lead) 
mean(lead$Age)
mean(lead$Iqf)
mean(lead$Ld72)
mean(lead$Ld73)
mean(lead$Totyrs)
mean(lead$MAXFWT, na.rm = TRUE)

median(lead$Age)
median(lead$Iqf)
median(lead$Ld72)
median(lead$Ld73)
median(lead$Totyrs)
median(lead$MAXFWT, na.rm = TRUE)

min(lead$Age)
min(lead$Iqf)
min(lead$Ld72)
min(lead$Ld73)
min(lead$Totyrs)
min(lead$MAXFWT, na.rm = TRUE)


max(lead$Age)
max(lead$Iqf)
max(lead$Ld72)
max(lead$Ld73)
max(lead$Totyrs)
max(lead$MAXFWT, na.rm = TRUE)

quantile(lead$Age, na.rm = TRUE, c(0.25,0.75))
quantile(lead$Iqf, na.rm = TRUE, c(0.25,0.75))
quantile(lead$Ld72, na.rm = TRUE, c(0.25,0.75))
quantile(lead$Ld73, na.rm = TRUE, c(0.25,0.75))
quantile(lead$Totyrs, na.rm = TRUE, c(0.25,0.75))
quantile(lead$MAXFWT, na.rm = TRUE, c(0.25,0.75))

#1.2: Frequency table for the categorical data 
table(lead$Area)
table(lead$Sex)
table(lead$Lead_type)
table(lead$Exposed)


#1.3 : correlation coefficient 
#Default : pearson
cor(lead$MAXFWT, lead$Ld72, use = "complete.obs")  
cor(lead$MAXFWT, lead$Ld73, use = "complete.obs")  
#spearman method 
cor(lead$MAXFWT,lead$Ld72, use="complete.obs",method="spearman")
cor(lead$MAXFWT,lead$Ld73, use="complete.obs",method="spearman")
#kendall method
cor(lead$MAXFWT,lead$Ld72, use="complete.obs",method="kendall")
cor(lead$MAXFWT,lead$Ld73, use="complete.obs",method="kendall")

#---------------------------------------------------------------------------------------------------------

#Question 2: Graphics 
#2.1 : Bar chart for categorical variable "Sex" 
#par(mfrow= c(1,1))
barplot(table(lead$Sex), xlab="Sex",ylab="Frequency", col = c('sandybrown','lightpink'))

#2.2 :Bar chart for with mean MAXWT in  males and females 
barplot(tapply(lead$MAXFWT,list(Sex=lead$Sex),mean,na.rm = TRUE),xlab="Sex",ylab="Mean MAXFWT", col = c('darkcyan','pink'))

#2.3 : Histogram for the Age and MAXFWT
hist(lead$Age,xlab="Age",main="Distribution of Age")
hist(lead$MAXFWT,xlab="MAXFWT",main="Distribution of MAXFWT")

#2.4 : scatter plot for the ld72 and MAXFWT 
plot(lead$Ld72,lead$MAXFWT,main = "scatter plot",xlab="Ld72",ylab="MAXFWT")
points(lead$Ld72[lead$Sex=="Male"],lead$MAXFWT[lead$Sex=="Male"], xlab="Ld72",ylab="MAXFWT", col= "red")
points(lead$Ld72[lead$Sex=="Female"],lead$MAXFWT[lead$Sex=="Female"], xlab="Ld72",ylab="MAXFWT", col= "Blue")

#add the Regression lines for each gender 
abline(lm(lead$Ld72[lead$Sex=="Male"]~lead$MAXFWT[lead$Sex=="Male"]), col = 'yellow')
abline(lm(lead$Ld72[lead$Sex=="Female"]~lead$MAXFWT[lead$Sex=="Female"]),col = 'green')

#2.5 : Boxplot for Age and Ld72, Ld73
boxplot(Age~Lead_type, data = lead, main = "Boxplot of Age and lead type")

#----------------------------------------------------------------------------------------------------------------------------

#Question 3: Outlier Detection 
#identifing the Outlier 
numeric_col=c()
for (i in names(lead)){
  if(class(lead[,i])=="integer" || class(lead[,i])=="numeric")
    numeric_col <- c(numeric_col,i)
  }

boxplot(lead[numeric_col[2:7]],main="Boxplot (outliers detection)")
#What do you think ??
#lqf , ld72 and MAXFWT have outliers 

#-----------------------------------------------------------------------------------------------------

#Question 4: Testing Normality / homoscedasticity 
#4.1 : checking normality using two methods
#method 1: Histogram
par(mfrow= c(3,2))
hist(lead$Age,xlab = "Age", main = "Distribution of Age") #not normal 
hist(lead$Iqf, xlab = "Iqf",main = "Distribution of Iqf") #normal 
hist(lead$Ld72, xlab = "Ld72",main = "Distribution of Ld72")  #not normal 
hist(lead$Ld73,xlab = "Ld73",main = "Distribution of Ld73")   #not normal 
hist(lead$Totyrs,xlab = "Totyrs",,main = "Distribution of Totyrs")  #not normal 
hist(lead$MAXFWT, xlab = "MAXWFT",main = "Distribution of MAXWFT")  #not normal 

#Method 2: shapiro test 
# Null Hypothesis : normally distributed 
# Alternative Hypothesis : not normally distributed 
shapiro.test(lead$Age)
# p value = 0.0004677 is less than the alpha value 0.05 which means we have enough evidence to reject the null hypothesis, not normally distributed
shapiro.test(lead$Iqf)
# p value = 0.07334 is larger than the alpha value 0.05 which means we don't have enough evidence to reject the null hypothesis , normally distributed 
shapiro.test(lead$Ld72)
# p value = 3.188e-08 is less than the alpha value 0.05 which means we have enough evidence to reject the null hypothesis , not normally distributed 
shapiro.test(lead$Ld73)
# p value = 3.515e-05 is less than the alpha value 0.05 which means we have enough evidence to reject the null hypothesis , not normally distributed 
shapiro.test(lead$Totyrs)
# p value = 0.0002998 is less than the alpha value 0.05 which means we have enough evidence to reject the null hypothesis , not normally distributed 
shapiro.test(lead$MAXFWT)
# p value = 0.0005636 is less than the alpha value 0.05 which means we have enough evidence to reject the null hypothesis , not normally distributed 




#4.2 Checking for homoscedasticity 
#method 1: Boxplot
par(mfrow= c(3,2))
boxplot(Age~Sex, data = lead)
boxplot(Iqf~Sex, data = lead)
boxplot(Ld72~Sex, data = lead)
boxplot(Ld73~Sex, data = lead)
boxplot(Totyrs~Sex, data = lead)
boxplot(MAXFWT~Sex, data = lead)



#method 2: we will use levene test instead of var test for the colomns which are not normally distributed
#null hypothesis : the variance among the groups are equal 
#alternative hypothesis : the variance among the groups are not equal 
library(car)
leveneTest(Age~Sex, data = lead)
# pr = 0.389 is larger than 0.05 which means we don't have enough evidence to reject the null hypothesis , equal variance 
leveneTest(Ld72~Sex, data = lead)
# pr = 0.8542 is larger than 0.05 which means we don't have enough evidence to reject the null hypothesis , equal variance 
leveneTest(Ld73~Sex, data = lead)
# pr = 0.9177 is larger than 0.05 which means we don't have enough evidence to reject the null hypothesis , equal variance 
leveneTest(Totyrs~Sex, data = lead)
# pr = 0.3524 is larger than 0.05 which means we don't have enough evidence to reject the null hypothesis , equal variance 
leveneTest(MAXFWT~Sex, data = lead)
# pr = 0.1778 is larger than 0.05 which means we don't have enough evidence to reject the null hypothesis , equal variance 


# bartlett test for the normally distributed coloumn 
bartlett.test(Iqf~Sex, data = lead)
# p value = 0.9301 is larger than 0.05 which means we don't have enough evidence to reject the null hypothesis , equal variance 


#conclusion : all the numerical coloumns are not normally distributed except for "IQF", but all of them are equal in variance.



#---------------------------------------------------------------------------------------------------------------------------------------------------------------

#Question 5: Statistical Inferance 
#5.1: calculate the 90%, 95%, 99% CI
means <- tapply(lead$MAXFWT,list(Sex=lead$Sex),mean,na.rm=T)
sd <- tapply(lead$MAXFWT,list(Sex=lead$Sex),sd,na.rm=T)
x = means
s = sd
n = 117

#90% CI 
error1 = qnorm(0.951)*s/sqrt(n)
lowerinterval1 = x - error1
upperinterval1 = x + error1
lowerinterval1
upperinterval1

#95% CI 
error2 <- qnorm(0.975)*s/sqrt(n)
lowerinterval2 <- x - error2
upperinterval2 <- x + error2
lowerinterval2
upperinterval2
#99% CI
error3 <- qnorm(0.9995)*s/sqrt(n)
lowerinterval3 <- x - error3
upperinterval3<- x + error3
lowerinterval3
upperinterval3

#the higher the confidence interval "99%", the wider the range gets 
#for example: in 90% CI: Male ranges from 48.43001 to 52.92999 and in 99% CI : Male ranges from 46.20549 to 55.15451 
# so the confidence interval of 90 is narrow comparing to the 99. 



#---------------------------------------------------------------------------------------------------------------------------------------------------------------

#Question 6: hypothesis testing:


# NOTES:

# [ 1 ]
# ( MAXFWT ) when is higher is better  
# MAXFWT different between male vs female 
# assumptions 
# Assume normality and homoscedasticity 
# biological question: gender type don't affect the finger-wrist tapping score(MAXFWT)
# statistical  question: value of (MAXFWT) is equal in both female and male
# null hypothesis (H0) :  MAXFWT is equal for male and female
# alternative hypothesis (HA) : MAXFWT  different between male and female
# data handling:
# convert NA to zero in MAXFWT column 
#lead =get(load("lead.RData"))
lead[is.na(lead)]=0
#install.packages("magrittr") # package installations are only needed the first time you use it
#install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
# put MAXFWT male's data in new variable  and the MAXFWT female's data in new variable 
#male_MAXFWT <- lead$MAXFWT[lead$Sex == "Male"]
male_MAXFWT <- lead %>% filter(Sex == "Male") %>% pull(MAXFWT)
male_MAXFWT

female_MAXFWT <- lead %>% filter(Sex == "Female") %>% pull(MAXFWT)
female_MAXFWT
# view mean
mean(male_MAXFWT)
mean(female_MAXFWT)
# [ two sample t-test ] we will make the variance = true 
#t.test(lead$MAXFWT ~ lead$Sex, var.equal = T, paired = F)
t.test(female_MAXFWT, male_MAXFWT, var.equal = T, paired = F )

# [ 2 ]
# Assess whether the previous test assumptions have been meet for the test.
# p.value = 0.2453, which is greater than the alpha = 0.05 (0.05 < 0.2453), 
# deviation from the null hypothesis is not statistically significant, and the null hypothesis is not rejected.
# whcih state that "MAXFWT is equal for male and female"



#########


# [ 3 ]
# (2 groups) MAXFWT is “lower” in [[ Ld72 > 40 ]] compared to [[  Ld72 =< 40 ]]
# we will use Two-sample Welch t-test and make the variance = False 
# create two variable for [[ Ld72 > 40 ]] and [[  Ld72 =< 40 ]]
# Assumptions
# Assume normality and homoscedasticity 
# biological question:  children who have high value of Ld72 suffer from low score of finger-wrist tapping(variable name: MAXFWT)
# statistical  question: when the value of ld72 is more than 40 the child will have lower value of MAXFWT compared to childern with 
# null hypothesis: state that MAXWT is “equal” in the group receiving Ld72 > 40 and  Ld72 =< 40
# alternative hypothesis: MAXWT is “lower” in the group receiving Ld72 > 40  compared to the control Ld72 =< 40.

# data manipulation
#lead =get(load("lead.RData"))
#lead[is.na(lead)]=0




#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Ld72_greater_than_40 <- lead$Ld72[lead$Ld72 > 40]
#Ld72_greater_than_40
#Ld72_smaller_or_equal_than_40 <- lead$Ld72[lead$Ld72 <= 40]
#Ld72_smaller_or_equal_than_40
# view data 
#Ld72_greater_than_40
#Ld72_smaller_or_equal_than_40
# mean of the data 
#mean(Ld72_smaller_or_equal_than_40)
#mean(Ld72_greater_than_40)
# [ two sample Welch t-test ]
# Two-sample Welch t-test and make the variance = false
#t.test(Ld72_greater_than_40, Ld72_smaller_or_equal_than_40, alternative = "two.sided", var.equal = FALSE )
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
Ld72_greater_than_40 <- lead %>% filter( Ld72 > 40 ) %>% pull(MAXFWT)
Ld72_greater_than_40
# view data 
Ld72_smaller_or_equal_than_40 <- lead %>% filter(Ld72 <= 40 ) %>% pull(MAXFWT)
Ld72_smaller_or_equal_than_40
# mean of the data 
mean(Ld72_smaller_or_equal_than_40)
mean(Ld72_greater_than_40)
# [ two sample Welch t-test ]
# Two-sample Welch t-test and make the variance = false
t.test(Ld72_greater_than_40, Ld72_smaller_or_equal_than_40, alternative = "two.sided", var.equal = FALSE )









# [ 4 ]
# Assess the previous test assumption
# p.value = 9.916e-09 = 0.000000009916
# p-value = 0.000000009916 < alpha = 0.05
# so we have enough evidence to reject the null hypothesis 
# And accept the alternative hypothesis which that "MAXWT is “lower” in the group receiving Ld72 > 40  compared to the control Ld72 =< 40."
# so the assumption of the question is true


#########

# [ 5 ]
# •	We hypothesis that MAXWT is different between the different Lead types with the different genders  
# (i.e. 4 groups male_leadtype1, male_leadtype2, female_leadtype1, female_leadtype2). 
# Can you perform comparison between the different groups, after assessing the assumptions and performing post-hoc testing 
# (assuming normality and homoscedasticity).

# [ type of test used ]
# to know which test will be used we need to first specify the number of independent variables
# we have one independent variable (MAXFWT) that will compare its value to each group
# one-way ANOVA will be used, because we have only one independent variable ( MAXFWT )

# [ Assumptions ]
# we will assume normally and homoscedasticity in the data as mentioned in the hypothesis question
# which means that The variable (MAXFWT) is normally distributed in each (group) population
# The variances are equal among all statistical populations from which the (groups) were sampled

# [ Data manipulation ]
# convert any NA value to zero
#lead =get(load("lead.RData"))
#lead[is.na(lead)]=0
#install.packages("magrittr") # package installations are only needed the first time you use it
#install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
# filter and put data in new variables
male_lead_72 <- lead %>% filter(Sex == "Male" , Lead_type == "Ld72") %>% pull(MAXFWT)
male_lead_72

female_lead_72 <- lead %>% filter(Sex == "Female" , Lead_type == "Ld72") %>% pull(MAXFWT)
female_lead_72

male_lead_73 <- lead %>% filter(Sex == "Male" , Lead_type == "Ld73") %>% pull(MAXFWT)
male_lead_73

female_lead_73 <- lead %>% filter(Sex == "Female" , Lead_type == "Ld73") %>% pull(MAXFWT)
female_lead_73
# make the length of all columns equal in length by adding NA 
length(female_lead_72) <- length(male_lead_72)
length(male_lead_73) <- length(male_lead_72)
length(female_lead_73) <- length(male_lead_72)
# convert any NA value to zero
female_lead_72[is.na(female_lead_72)]=0
male_lead_73[is.na(male_lead_73)]=0
female_lead_73[is.na(female_lead_73)]=0
# put data in data frame format so the gather() can accept it 
total_groups <- data.frame(male_lead_72, female_lead_72, male_lead_73, female_lead_73)
total_groups
# convert wide data frame shape to long data frame shape 
library(tidyr)
groups_compare <- gather(total_groups, key="group", value="amount",male_lead_72, female_lead_72, male_lead_73, female_lead_73)
groups_compare


# biological question : does the lead type and the gender type don't affect the finger-wrist tapping score (MAXFWT)
# statistical question : does the change in lead and gender types and values don't affect the MAXFW value ?
# Null hypothesis : the (MAXFWT) have the same value between the different Lead types with the different genders 
# Alternative hypothesis : the (MAXFWT) is different between the different Lead types with the different genders 

# [ one-way ANOVA test ]
anova_model <- aov(amount ~ group, data = groups_compare)
summary(anova_model)
# p.value = 4e-14 = 0.00000000000004
# P.value < 0.05, so we have enough evidence to reject the Null hypothesis which state that " the (MAXFWT) have the same value between the different Lead types with the different genders "
# this means that we have enough evidence to accept the alternative hypothesis which represent the hypothesis of the question, state that " the (MAXFWT) is different between the different
# Lead types with the different genders " this statement is true, but before state that its true we need to check with post-hoc test and see which group comparison have p.value less than the alpha 
 

# [ post-hoc testing ]
# Tukey’s Test will be used to make every possible pairwise comparison
# Tukey's test help us identify which group have different (MAXFWT), by comparing the p.value of each comparison to the alpha 
TukeyHSD(anova_model, conf.level=.95) 
plot(TukeyHSD(anova_model, conf.level=.95))
# we can see that all of the groups p.value is smaller than the alpha except one so we can  that the ANOVA test is 
# correct and the alternative hypothesis is true



