#Sample Data Analysis

rm(list=ls(all=TRUE))
cat('\014')
options(digits=8)
sink("D:/R_WD/output.txt")

#Set up wrokspace
setwd("D:/R_wD")

#First, save original xls files into txt file in Excel. Then, Import data into R and call it mydata
mydata<- read.table("SampleData.txt",sep="\t", header=TRUE)

#Get a general idea about the data structure
names(mydata) #column names
dim(mydata)   #data dimension (615 obs and 13 variables)
summary(mydata) # get summary statistics. There are some missing values.
str(mydata)   #data structure 

#Get frequenct tables for TeacherID and studentClassID
table(mydata$studentTeacherID)
table(mydata$studentClassID)

# Create a new dataset without missing data and name it as CompleteData
sum(complete.cases(mydata))   #count the number of complete cases (479 obs)
CompleteData<-na.omit(mydata)
str(CompleteData)


# Selete data based on variable values studentOptOut=No (473obs and 13 variables)
newdata <- CompleteData[ which(CompleteData$studentOptOut=="NO"),]
str(newdata)

# Check whether there is any duplicated cases
duplicated(newdata)


# Cleaning Categorical Data
# 1) studentPattern: change 5 factor levels to 4 
table(newdata$studentPattern)
newdata$studentPattern[which(newdata$studentPattern=="p12bp\n")] <- "p12bp"
newdata$studentPattern <- newdata$studentPattern[,drop=TRUE] 

table(newdata$studentPattern)  # make sure there are 4 levels

# Create a new variable studentGroup with two factor levels: Treatment and Control
# Students are classified into two groups
newdata$studentGroup <- factor(rep(NA, length(newdata$studentPattern) ), levels=c("Treatment", "Control") ) 
newdata$studentGroup[ newdata$studentPattern %in% c("p12bp","p12pb")] <- "Treatment"
newdata$studentGroup[ is.na(newdata$studentGroup) ]             <- "Control"


attach(newdata)

#Frequency table and probability table for studentGroup
studentGroup.table<- table(studentGroup)
studentGroup.table
prop.table(studentGroup.table)


# Cleaning Numeric Data

summary(newdata) # get summary statistics. No missing values, no studentOptOut=YES,no extreme values. 
                 # Mean and median for continuous variables are quite similar. Data looks normal and symmetric.

# Subsetting test score data and name it as scores
scores <- cbind(studentPreScore,studentPreAbilityEstimate,studentBenchScore,studentPostScore,studentPostAbilityEstimate,studentGroup)

# Get descriptive statistics of numeric score data by studentGroup
library(psych)
describeBy(scores,studentGroup, digit=8)

# Subsetting data by studentGroup

Control<-subset(newdata, studentGroup=="Control")
Treatment<-subset(newdata, studentGroup=="Treatment")


#studentPreScore
#Explortary data analysis:histograms, QQ plots, boxplots, and independent sample t-test

Control.studentPrescore<-Control[,7]
Treatment.studentPreScore<-Treatment[,7]

hist(Control.studentPrescore,prob=TRUE);lines(density(Control.studentPrescore)) 
qqnorm(Control.studentPrescore);qqline(Control.studentPrescore, col = 2)
tapply(studentPreScore,studentGroup,mean)
boxplot(studentPreScore~studentGroup)
t.test(Control.studentPrescore,Treatment.studentPreScore,var.equal=T)


#studentPreAbilityEstimate 
#Explortary data analysis:histograms, QQ plots, boxplots, and independent sample t-test

Control.studentPreAbility<-Control[,8]
Treatment.studentPreAbility<-Treatment[,8]

hist(Control.studentPreAbility,prob=TRUE);lines(density(Control.studentPreAbility))
qqnorm(Control.studentPreAbility);qqline(Control.studentPreAbility, col = 2)
tapply(studentPreAbilityEstimate,studentGroup,mean)
boxplot(studentPreAbilityEstimate~studentGroup)
t.test(Control.studentPreAbility,Treatment.studentPreAbility,var.equal=T)

#studentBenchScore
#Explortary data analysis:histograms, QQ plots,boxplots, and independent sample t-test

Control.studentBenchScore<-Control[,10]
Treatment.studentBenchScore<-Treatment[,10]

hist(Control.studentBenchScore,prob=TRUE);lines(density(Control.studentBenchScore))
qqnorm(Control.studentBenchScore);qqline(Control.studentBenchScore, col = 2)
tapply(studentBenchScore,studentGroup,mean)
boxplot(studentBenchScore~studentGroup)
t.test(Control.studentBenchScore,Treatment.studentBenchScore,var.equal=T)

#studentPostScore
#Explortary data analysis:histograms, QQ plots, boxplots, and independent sample t-test

Control.studentPostscore<-Control[,12]
Treatment.studentPostScore<-Treatment[,12]

hist(Control.studentPostscore,prob=TRUE);lines(density(Control.studentPostscore))
qqnorm(Control.studentPostscore);qqline(Control.studentPostscore, col = 2)
tapply(studentPostScore,studentGroup,mean)
boxplot(studentPostScore~studentGroup)
t.test(Control.studentPostscore,Treatment.studentPostScore,var.equal=T)


#studentPostAbilityEstimate 
#Explortary data analysis:histograms, QQ plots, boxplots, and independent sample t-test

Control.studentPostAbility<-Control[,13]
Treatment.studentPostAbility<-Treatment[,13]

hist(Control.studentPostAbility,prob=TRUE);lines(density(Control.studentPostAbility))
qqnorm(Control.studentPostAbility);qqline(Control.studentPostAbility, col = 2)
tapply(studentPostAbilityEstimate,studentGroup,mean)
boxplot(studentPostAbilityEstimate~studentGroup)
t.test(Control.studentPostAbility,Treatment.studentPostAbility,var.equal=T)



#Research question 1

#Test difference scores betweem pre and post scores for different student groups: single sample T-test
difference1<- Control.studentPostscore-Control.studentPrescore
difference2<- Treatment.studentPostScore-Treatment.studentPreScore
difference3<- Control.studentPostAbility-Control.studentPreAbility
difference4<- Treatment.studentPostAbility-Treatment.studentPreAbility

t.test(difference1)
t.test(difference2)
t.test(difference3)
t.test(difference4)

t.test(difference1,difference2,var.equal=T)
t.test(difference3,difference4,var.equal=T)


#ANCOVA on the post-treatment scores, with pre-treatment score as a covariate and treatment as an independent variable. 
#Intuitively, the idea is that a test of the differences between both groups 
#is really what you are after and including pre-test scores 
#as a covariate can increase power compared to a simple t-test or ANOVA.


fit = aov( studentPostScore ~ studentGroup+studentPreScore, data=newdata )
summary( fit, type = "III" )

fit2=aov( studentPostAbilityEstimate ~ studentGroup+studentPreAbilityEstimate, data=newdata )
summary( fit2, type = "III" )


#Research question 2
#Correlation between studentBenchScore and studentPostScore/studentPostAbilityEstimate

data1<-as.matrix(cbind(studentBenchScore,studentPostScore,studentPostAbilityEstimate))
library(Hmisc)
rcorr(data1,type="pearson")

# Compare studentbenchscore by group: two-sample t-test
tapply(studentBenchScore,studentGroup,mean)
t.test(studentBenchScore~studentGroup,var.equal=T)

#simple linear regression: studentPostScore as DV, studentBenchScore as IV
plot(studentBenchScore ~ studentPostScore,col=c("red","blue"))
abline(lsfit(studentPostScore,studentBenchScore))

model1<-lm(studentPostScore ~ studentBenchScore)
summary(model1)

#simple linear regression: studentPostAbilityEstimate as DV, studentBenchScore as IV
plot(studentBenchScore ~ studentPostAbilityEstimate,col=c("orange","black"))
abline(lsfit(studentPostAbilityEstimate,studentBenchScore))

model2<-lm(studentPostAbilityEstimate ~ studentBenchScore)
summary(model2)

detach(newdata)
sink()

