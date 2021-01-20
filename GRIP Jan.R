#AYUSHI FRANCIS
#Graduate Rotational Internship Program-The Sparks Foundation
#Data Science & Business Analytics Tasks
#Task 3- Exploratory Data Analysis - Retail

#To do:
#???Perform 'Exploratory Data Analysis' on data set 'SampleSuperstore'
#??? As a business manager, try to find out the weak areas where you can 
#  work to make more profit.
#??? What all business problems you can derive by exploring the data?
#???Data set : https://bit.ly/3i4rbWl


rm()
getwd()
setwd("C:/Users/AYUSHI FRANCIS/Downloads")
library(dlookr)

#Load Data
read.csv("SampleSuperstore.csv")
x<-read.csv("SampleSuperstore.csv")


#Data info
str(x)
head(x)
summary(x)
str(x)
nrow(x)
ncol(x)

#Convert character to categorical variable
x[sapply(x, is.character)] <- lapply(x[sapply(x, is.character)], as.factor)
summary(x)

#Check Duplicate values and NA values
is.na(x)
duplicated(x)
sum(duplicated(x))

#Less duplicate values so we drop it
library(dplyr)
unique(x)
x<-x %>% distinct()
duplicated(x)
sum(duplicated(x))

#Graphical Representation and interpretation
describe(x)
normality(x)
plot_normality(x)
##Interpretation:
#Shapiro-Wilk normality test is performed. 
#When the number of observations is greater than 5000,
#it is tested after extracting 5000 samples by random simple sampling.

correlate(x)
plot_correlate(x)
##Interpretation:
#We can see from the values and plot that there exist no such strong correlation 
#between the numerical variables in the data. But there is 0.479 and 0.201
#positive correlation between Sales and Profit and Sales and Quantity resp.

#Defining the target variable
(target<- target_by(x, Profit))

#Target variable is numeric and predictors are numeric
library(hexbin)
ps<- relate(target, Sales)
ps
summary(ps)
plot(ps)

pq<- relate(target, Quantity)
pq
summary(pq)
plot(pq)

pd<- relate(target, Discount)
pd
summary(pd)
plot(pd)

##Interpretation:
#1.It is seen from the pvalue and the plot that 
#the p-value is smaller than 0.05 and the scatter plot of the observations converges
#on the red diagonal line so there exits a relationship between Sales and Profit.

#2.It is seen from the pvalue and the plot that 
#the p-value is greater than 0.05 and the scatter plot of the observations does not
#converges on the red diagonal line so there does not exits a relationship between 
#Quantity and Profit.

#3.It is seen from the pvalue and the plot that 
#the p-value is greater than 0.05 and the scatter plot of the observations does not
#converges on the red diagonal line so there does not exits a relationship between 
#discount and Profit.

#Target variable is numeric and predictors are categorical variables
psh<- relate(target, Ship_Mode)
psh
summary(psh)
plot(psh)

pse<- relate(target, Segment)
pse
summary(pse)
plot(pse)

pst<- relate(target, State)
pst
summary(pst)
plot(pst)

pci<- relate(target, City)
pci
summary(pci)
plot(pci)

pr<- relate(target, Region)
pr
summary(pr)
plot(pr)

pcat<- relate(target, Category)
pcat
summary(pcat)
plot(pcat)

psc<- relate(target, Sub_Category)
psc
summary(psc)
plot(psc)

##Interpretation:
#1.It is seen from the pvalue and the plot that the p-value is greater than 0.05 
#i.e 0.9288, so we reject the null hypothesis that there is no significance between 
#the variables and hence exits a relationship between Shipmode and Profit.

#2.It is seen from the pvalue and the plot that the p-value is greater than 0.05 
#i.e 0.4045, so we reject the null hypothesis that there is no significance between 
#the variables and hence exits a relationship between Segment and Profit.

#3.It is seen from the pvalue and the plot that the p-value is greater than 0.05 
#i.e 0.022, so we fail to reject the null hypothesis that there is no significance between 
#the variables and hence no exits relationship between State and Profit.

#4.It is seen from the pvalue and the plot that the p-value is greater than 0.05 
#i.e 0.2635, so we reject the null hypothesis that there is no significance between 
#the variables and hence exits a relationship between City and Profit.

#5.It is seen from the pvalue and the plot that the p-value is greater than 0.05 
#i.e 0.04842, so we fail to reject the null hypothesis that there is no significance between 
#the variables and hence exits no relationship between Region and Profit.

#6.It is seen from the pvalue and the plot that the p-value is greater than 0.05 
#i.e 0.2, so reject the null hypothesis that there is no significance between 
#the variables and hence exits a relationship between Category and Profit.

#7.It is seen from the pvalue and the plot that the p-value is greater than 0.05 
#i.e 0.02, so we reject the null hypothesis that there is no significance between 
#the variables and hence exits a relationship between Sub Category and Profit.

##CONCLUSION AND RESULT
#It is concluded that there exists a relationship between Sales, Shipmode, Segment,City, 
#Category and Sub Category in terms of profit. When we see the profit in each segment, 
#Home Office has the maximum profit and Consumer has the lowest Profit, whereas sales of 
#each Segment is almost same.In Category we can see that Office Supplies has the highest 
#value whereas Furniture and Technology has almost same values. We can see that Technology 
#category provides the maximum sales and Profit. 

#SUGGESTION
#Retailer should try to reduce the products related to Home Supplies or try to find right
#marketing strategy to increase Home Supplies sales.