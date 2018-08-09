print("welcome the future data scientist- vaishali sagar")

#reading the file
dataset<- read.csv("Salary_data.csv")
View( dataset)

#splitting the data
library(caTools)
set.seed(123)
split<- sample.split(dataset$Salary,
                     SplitRatio = 2/3)
split
train<- subset(dataset,split==TRUE)
test<- subset(dataset,split==FALSE)

# feature scaling
# train<- scale(train)
# test<- scale(test)

              ##LINEAR REGRESSION##

regressor<- lm(Salary ~ YearsExperience,
               data= train)
summary(regressor)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      25873.5     3028.7   8.543 9.50e-08 ***
#   YearsExperience   9269.3      538.8  17.203 1.28e-12 ***
##### YearsExperience is highly significant alsoo  the p value should be less than 5% and here
              ##it is 1.28 * 10^(-16)

predict<- predict(regressor,
                  newdata=test)
predict

####visualising####training set
library(ggplot2)
ggplot()+
  geom_point(aes(x= train$YearsExperience,y= train$Salary),
             colour="blue")+
  geom_line(aes(x= train$YearsExperience, y= predict(regressor, newdata= train)))+
  ggtitle("Salary vs Experience")+
  xlab("years of experience(training set)")+
  ylab("salary(in Dollars)")

##visualising test set#####

ggplot()+
  geom_point(aes(x= test$YearsExperience, y= test$Salary), colour="black")+
  geom_line(aes(x=train$YearsExperience, y= predict(regressor, newdata=train)), colour= "red")+
  ggtitle("years of experience(test set)")+
  ylab("salary(in Dollars)")


#######################################################################################################

                      #PRACTISE#
print("welcome the fututure data scientist- vaishali sagar")

sal<- read.csv("Salary_data.csv")
View(sal)

split<- sample.split(sal$Salary, SplitRatio = 2/3)
split
train<- subset(sal, split== TRUE)
test<- subset(sal, split==FALSE)

#fitting the model for train set
regressor<- lm(formula = Salary~YearsExperience,
               data= train)
summary(regressor)

#predicitng the value for test set
predict<- predict(regressor, newdata= test)

##visusalising the model for train set
library(ggplot2)
ggplot()+
  geom_point(aes(train$YearsExperience, train$Salary),
             colour= c("purple"))+
  geom_line(aes(train$YearsExperience, y=predict(regressor, newdata= train)))

#visualising for test set
library(ggplot2)
ggplot()+
  geom_point(aes(test$YearsExperience, test$Salary),
             colour=c("sky blue"))+
  geom_line(aes(train$YearsExperience, y=predict( regressor, newdata= train)))
