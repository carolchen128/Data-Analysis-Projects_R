####          High School Math Data Analysis R code         ####
####           Carol Cong Chen   Oct 21th, 2015             ####
################################################################

################################################################
#Question: Do the data suggest that the performance of        ##
#treatment students is better than the comparison students?   ##
#Was the intervention more effective for certain students?    ##
#If so, who?                                                  ## 


# Data Merge and Clean Task# 

# Data Preparation:1) convert all data files into csv format; #
#               or 2) use R package xlsx to read in xlsx file #
#                     or package qdap to read in docx file.  #

# remove all elements for a clean start
rm(list=ls(all=TRUE))
cat('\014')

# InstallPackage
library(psych)

#Set up wrokspace
setwd("D:/R_wD/WestEd_RA")


#Read in data 
DistrictA<-read.table("DistrictA.csv",sep=",", header=TRUE)
DistrictB<-read.table("DistrictB.csv",sep=",", header=TRUE)
DistrictC<-read.table("DistrictC.csv",sep=",", header=TRUE)

#Create a new disctric variable
DistrictA$dist<-"A"
DistrictB$dist<-"B"
DistrictC$dist<-"C"

#Get a general idea about the data structure
dim(DistrictA)
str(DistrictA)
dim(DistrictB)
str(DistrictB)
dim(DistrictC)
str(DistrictC)

# Change column names to make them consistent
colnames(DistrictA) <- c("tc","id_s","gen","grade","eth","frl","math_name15","math_score15","math_band15","dist")

DistrictB<-DistrictB[-c(7:9)] # remove unused variable and data
colnames(DistrictB) <- c("id_s","tc","gen","grade","eth","frl","math_name15","math_score15","math_band15","dist")

DistrictC<-DistrictC[c(1:5,9)]  # remove unused variable and data (District C had missing variables)
colnames(DistrictC) <- c("tc","id_s","gen","grade","eth","dist")

# Merge three dataset into one data file 
mydata <- merge(DistrictA,DistrictB,all=TRUE)
mydata <- merge(mydata,DistrictC,all=TRUE)

write.csv(mydata,"MathData_all.csv")

mydata<-read.table("MathData_all.csv",sep=",", header=TRUE)
mydata<-mydata[-c(1)]

# get data structure of mydata
names(mydata) #column names
dim(mydata)   #data dimension (119 obs and 10 variables)
summary(mydata) # get summary statistics. There are some missing values.
str(mydata)   #data structure 

# Cleaning Data step by step by variable name

# 1.tc(treatment status): change type int to factor: (treatment=1, comparison=0)

table(mydata$tc)  # 1:55 & 0:64
mydata$tc[which(mydata$tc==2)] <- 0

# 2.id_s(student ID)
mydata$id_s<-as.character(mydata$id_s)

# Check duplicate case
duplicated(mydata) # no dulicate data
duplicated(mydata$id_s)  # no duplicate student ID

# 3. gen(gender): (Female=1, Male=0)
table(mydata$gen) # 1:62 & 0:57
mydata$gen<-as.numeric(ifelse(mydata$gen == "M", 0, ifelse(mydata$gen == "F", 1,9999)))

# 4. grade: change type int to factor: (grade10=0, grade11=1)

table(mydata$grade) #   10:90    11:25   9999:2
grade.error<- mydata[which(mydata$grade==1),]
write.csv(grade.error,"grade.error.csv")

mydata$grade<-as.numeric(ifelse(mydata$grade == "10", 0, ifelse(mydata$grade == "11", 1,9999)))
mydata$grade[is.na(mydata$grade)] <- 9999

# 5. eth:(race ethnicity): Hispanic or Latino =1, White=2, other=0 

table(mydata$eth)
mydata$eth<-as.numeric(ifelse(mydata$eth == "Hispanic", 1, ifelse(mydata$eth == "Latino", 1,
                       ifelse(mydata$eth == "White", 2,0))))

# 6. dist(district): categorical variable with 3 levels: A,B,C
table(mydata$dist)

#A  B  c 
#49 59 11


# 7.frl(qualified for free or reduced lunch ):qualified=1, not qualified=0
table(mydata$frl)
mydata$frl<-as.numeric(ifelse(mydata$frl == "0", 0, ifelse(mydata$frl == "1", 1,9999)))
mydata$frl[is.na(mydata$frl)] <- 9999
# 0  1 
#65 36

# 8.math_name15(name of math standardized test_2015) 
table(mydata$math_name15)  #SBAC: 93   9999:15
mydata$math_name15<-ifelse(mydata$math_name15== "SBAC", "SBAC", 9999)

# 9.math_score15(math standardized test scale score_2015)
describe(mydata$math_score15)
#vars n  mean     sd median trimmed   mad min  max range  skew kurtosis    se
#  1 101 2424 504.24   2510 2517.27 91.92   0 2758  2758 -4.37    18.17 50.17

hist(mydata$math_score15) 

score_outlier<-mydata[which(mydata$math_score15<50),]
write.csv(score_outlier,"score_outlier.csv")

mydata$math_score15[is.na(mydata$math_score15)]<-9999


# 10.math_band15(math standardized test band_2015)
table(mydata$math_band15)
mydata$math_band15<-as.factor(mydata$math_band15)

#0  1  2  3  4 
#4 31 36 18 12 

write.csv(mydata,"MathData_clean.csv")


# Data Analysis Task# 

# Recode all 9999 in the data as NA
mydata[mydata == 9999]<-NA
summary(mydata)
str(mydata)
# 1: Meanscore by treatemtn group
GroupMean<-describeBy(mydata$math_score15,group=mydata$tc,mat=TRUE,digits=2)
write.csv(GroupMean,"GroupMean.csv")


# 2: Frequency table of gender, grade, race_ethnicity, qualified for free and reduced lunch, 
#    and 2015 math standardized test band by treatment status

table(mydata$tc, mydata$gen)
table(mydata$tc, mydata$grade)
table(mydata$tc, mydata$eth)
table(mydata$tc, mydata$frl)
table(mydata$tc, mydata$math_band15)

# 3: Frequency table of gender, grade, race_ethnicity, qualified for free and reduced lunch, 
#    and 2015 math standardized test band by district

table(mydata$dist, mydata$gen)
table(mydata$dist, mydata$grade)
table(mydata$dist, mydata$eth)
table(mydata$dist, mydata$frl)
table(mydata$dist, mydata$math_band15)

# 4-1 Independent sample T-test: mean math score by treatment status
t.test(mydata$math_score15 ~ mydata$tc)

# effect size
library(compute.es)
es<-mes(2492.561,2377.150,68.26,649.83,41,60,level = 95,dig = 2)
capture.output(es, file = "effectsize.txt")

# 4-2 One-way ANOVA
fit = aov( mydata$math_score15 ~ mydata$tc, data=mydata )
summary( fit, type = "III" )
