# ========================================================================================================
# Title:        Using Machine Learning to Improve Companies' Employee Retention Rate
# Team:         Seminar Group 7 Team 2
# Members:      Aries Tan Zhi Lin, Chia Kwang, Chong Sze Kit, Darryl Ang Wei Zhuang, Lek Jie Kai
# Date:         31/10/2021
# Topics:       Logistic Regression & CART
# Data Source:  HR_Employee_Data.csv
#=========================================================================================================

## IMPORT Libraries ##
library(data.table)
library(dplyr)
library(nnet)
library(caTools)
library(corrplot)
library(ggcorrplot)
library(car)
library(ggplot2)
library(cowplot)
library(GGally)
library("MASS")
library(e1071)
library(caret)
library(rpart)
library(rpart.plot)

## SET Working Directory & IMPORT the CSV File ##
setwd('C:/Users/chong/Documents/2021/NTU/Y3S1/BC2406/AY21 Team Assignment and Project/AY21 Team Assignment and Project/Project')
hr_data.dt <- fread("HR_Employee_Data.csv", stringsAsFactors=TRUE,  na.strings=c("NA","missing","N/A",-99,"","m","M","na","."))
View(hr_data.dt)

##============================ DATA CLEANING =================================##

## CHECK for NA Values ##
colSums(is.na(hr_data.dt))
#No NA values in the dataset

#------------------------------- Emp_Id Column --------------------------------#

## CHECK Data Type ##
class(hr_data.dt$Emp_Id) #factor
#No need to change data type since 'Emp_Id' will not be used in our analysis

## CHECK for & REMOVE Duplicates (based on Emp_Id) ##
#To ensure that there are no duplicated rows 
#(i.e. the same employee having more than 1 record in the dataset)
nrow(hr_data.dt) #14999 rows (before removing duplicates)
distinct(hr_data.dt,Emp_Id,.keep_all=TRUE)
nrow(hr_data.dt) #14999 rows (after removing duplicates)
#Number of rows before and after removing duplicates remain the same at 14999
#Therefore, there are no duplicated records based on Emp_Id in the dataset

#--------------------------- satisfaction_level Column ------------------------#

## CHECK Data Type ##
class(hr_data.dt$satisfaction_level) #factor
#Need to change from factor to numeric data type

## REMOVE Percentage Sign ##
hr_data.dt$satisfaction_level=gsub('.{1}$','',hr_data.dt$satisfaction_level)

## CONVERT to Numeric Data Type & DIVIDE by 100 ##
hr_data.dt$satisfaction_level=as.numeric(as.character(hr_data.dt$satisfaction_level))/100

# CHECK Data Type ##
class(hr_data.dt$satisfaction_level) #numeric
#Data type changed successfully from factor to numeric

#--------------------------- last_evaluation Column ---------------------------#

## CHECK Data Type ##
class(hr_data.dt$last_evaluation) #factor
#Need to change from factor to numeric data type

## REMOVE Percentage Sign ##
hr_data.dt$last_evaluation=gsub('.{1}$','',hr_data.dt$last_evaluation)

## CONVERT to Numeric Data Type & DIVIDE by 100 ##
hr_data.dt$last_evaluation=as.numeric(as.character(hr_data.dt$last_evaluation))/100

## CHECK Data Type ##
class(hr_data.dt$last_evaluation) #numeric
#Data type changed successfully from factor to numeric

#--------------------------- number_project Column ----------------------------#

## CHECK Data Type ##
class(hr_data.dt$number_project) #integer
#Need to change from integer to numeric data type 
#To ensure consistency with other columns in the dataset

## CONVERT to Numeric Data Type ##
hr_data.dt$number_project=as.numeric(hr_data.dt$number_project)

## CHECK Data Type ##
class(hr_data.dt$number_project) #numeric
#Data type changed successfully from integer to numeric

#----------------------- average_monthly_hours Column -------------------------#

## CHECK Data Type ##
class(hr_data.dt$average_montly_hours) #integer
#Need to change from integer to numeric data type 
#To ensure consistency with other columns in the dataset

## CONVERT to Numeric Data Type ##
hr_data.dt$average_montly_hours=as.numeric(hr_data.dt$average_montly_hours)

## CHECK Data Type ##
class(hr_data.dt$average_montly_hours) #numeric
#Data type changed successfully from integer to numeric

#----------------------- time_spend_company Column ----------------------------#

## CHECK Data Type ##
class(hr_data.dt$time_spend_company) #integer
#Need to change from integer to numeric data type 
#To ensure consistency with other columns in the dataset

## CONVERT to Numeric Data Type ##
hr_data.dt$time_spend_company=as.numeric(hr_data.dt$time_spend_company)

## CHECK Data Type ##
class(hr_data.dt$time_spend_company) #numeric
#Data type changed successfully from integer to numeric

#--------------------------- Work_accident Column -----------------------------#

## CHECK Data Type ##
class(hr_data.dt$Work_accident) #integer
#Need to change from integer to factor data type

## RENAME Categories ##
hr_data.dt$Work_accident=ifelse(hr_data.dt$Work_accident==1,"Involved","Not Involved")

## CONVERT to Factor Data Type ##
hr_data.dt$Work_accident=factor(hr_data.dt$Work_accident)

## CHECK Data Type ##
class(hr_data.dt$Work_accident) #factor
#Data type changed successfully from integer to factor

## IDENTIFY Baseline ##
levels(hr_data.dt$Work_accident) #Involved

#--------------------------------- left Column --------------------------------#

## CHECK Data Type ##
class(hr_data.dt$left) #integer
#Need to change from integer to factor data type

## RENAME Categories ##
hr_data.dt$left=ifelse(hr_data.dt$left==1,"Resigned","Current employee")

## CONVERT to Factor Data Type ##
hr_data.dt$left=factor(hr_data.dt$left)

## CHECK Data Type ##
class(hr_data.dt$left) #factor
#Data type changed successfully from integer to factor

## IDENTIFY Baseline ##
levels(hr_data.dt$left) #Current employee

#--------------------- promotion_last_5years Column ---------------------------#

## CHECK Data Type ##
class(hr_data.dt$promotion_last_5years) #integer
#Need to change from integer to factor data type

## RENAME Categories ##
hr_data.dt$promotion_last_5years=ifelse(hr_data.dt$promotion_last_5years==1,"Promoted","Not Promoted")

## CONVERT to Factor Data Type ##
hr_data.dt$promotion_last_5years=factor(hr_data.dt$promotion_last_5years)

## CHECK Data Type ##
class(hr_data.dt$promotion_last_5years) #factor
#Data type changed successfully from integer to factor

## IDENTIFY Baseline ##
levels(hr_data.dt$promotion_last_5years) #Not Promoted

#------------------------------- Department Column ----------------------------#

## CHECK Data Type ##
class(hr_data.dt$Department) #factor
#No need to change data type since factor is the desired data type for our analysis

## DROP Unnecessary Categories ##
hr_data.dt$Department=droplevels.factor(hr_data.dt$Department)
#None of the categories dropped, all 10 categories are used

## IDENTIFY Baseline ##
levels(hr_data.dt$Department) #IT

#------------------------------- salary Column --------------------------------#

## CHECK Data Type ##
class(hr_data.dt$salary) #factor
#No need to change data type since factor is the desired data type for our analysis

## IDENTIFY Baseline ##
levels(hr_data.dt$salary) #high

#---------------------------------- Outliers ----------------------------------#

## CHECK for Outliers in satisfaction_level Column ##
hist(hr_data.dt$satisfaction_level)
min(hr_data.dt$satisfaction_level) #0.09
max(hr_data.dt$satisfaction_level) #1
ggplot(hr_data.dt, aes(satisfaction_level))+geom_boxplot()
#No outliers identified

## CHECK for Outliers in last_evaluation Column ##
hist(hr_data.dt$last_evaluation)
min(hr_data.dt$last_evaluation) #0.36
max(hr_data.dt$last_evaluation) #1
ggplot(hr_data.dt, aes(last_evaluation))+geom_boxplot()
#No outliers identified

## CHECK for Outliers in number_project Column ##
hist(hr_data.dt$number_project)
min(hr_data.dt$number_project) #2
max(hr_data.dt$number_project) #7
ggplot(hr_data.dt, aes(number_project))+geom_boxplot()
#No outliers identified

## CHECK for Outliers in average_montly_hours Column ##
hist(hr_data.dt$average_montly_hours)
min(hr_data.dt$average_montly_hours) #96
max(hr_data.dt$average_montly_hours) #310
ggplot(hr_data.dt, aes(average_montly_hours))+geom_boxplot()
#No outliers identified

## CHECK for Outliers in time_spend_company Column ##
hist(hr_data.dt$time_spend_company)
min(hr_data.dt$time_spend_company) #2
max(hr_data.dt$time_spend_company) #10
hr_data.dt[time_spend_company>=6] #1,282 employees
ggplot(hr_data.dt, aes(time_spend_company))+geom_boxplot()
#There seems to be outliers but we do not consider them to be outliers
#Some people will take more time to travel to and fro the company depending on where they stay
#For more information, you may refer to the report (Data Cleaning of Dataset)

#Overall, there is no obvious outlier that should be removed from the dataset

##===================== Data Exploration & Visualisation =====================##

## PLOT Correlation Matrix for Numeric Variables ##
p<-hr_data.dt[,2:6]
p.mat<-cor_pmat(p)
corr<-round(cor(p),1)
ggcorrplot(corr, hc.order=TRUE, type="lower", outline.col="white",
           ggtheme=ggplot2::theme_gray, colors=c("#6D9EC1", "white", "#E46726"), 
           lab=TRUE, p.mat=p.mat, insig="blank", title="Correlation Matrix")

## PLOT Bar Graph of Salary ##
ggplot(data=hr_data.dt, aes(x=salary, fill=left))+
  geom_bar(position="dodge")+ 
  labs(title="Bar Graph of Salary")+
  guides(fill ="none")+
  facet_grid(rows=vars(left))+ 
  geom_text(stat='count',aes(label=after_stat(count)), vjust=0.5)

## PLOT Bar Graph of Salary by Departments ##
ggplot(data=hr_data.dt, aes(x=salary, fill=left))+
  geom_bar(position="dodge")+ 
  labs(title="Bar Graph of Salary by Department")+
  guides(fill="none")+
  facet_grid(rows=vars(left), cols=vars(Department))+
  geom_text(stat="count", aes(label=after_stat(count)), size=2.5, vjust=0.5, position=position_dodge(0.9))+
  theme(axis.text.x=element_text(size=8, angle=45, hjust=1, vjust=1))

## PLOT Bar Graph of Work Accidents by Departments ##
ggplot(data=hr_data.dt[Work_accident=="Involved"], aes(x=Department, fill=left))+
  geom_bar(position="stack")+
  labs(title="Bar Graph of Work Accidents by Department")+
  guides(fill="none")+
  facet_grid(rows=vars(left))+
  geom_text(stat='count',aes(label=after_stat(count)), vjust=0.5)

## PLOT Box Plot for Satisfaction Level ##
ggplot(hr_data.dt, aes(hr_data.dt$Department,hr_data.dt$satisfaction_level, fill=left))+
  geom_boxplot()+
  labs(title="Box Plot for Satisfaction Level across Departments", x="Department", y="satisfaction_level")+
  guides(fill="none")+
  facet_grid(rows=vars(left))

## Plot Density Plot for Time Since Last Evaluation ##
ggplot(data=hr_data.dt, aes(x=last_evaluation, fill=left))+ 
  geom_density(kernel="gaussian")+
  labs(title="Density Plot for Time Since Last Evaluation")+
  guides(fill="none")+
  facet_grid(rows=vars(left))

## PLOT Density Plot for Average Monthly Working Hours ##
ggplot(data=hr_data.dt, aes(x=average_montly_hours, fill=left))+ 
  geom_density(kernel="gaussian")+ 
  labs(title="Density Plot for Average Monthly Working Hours")+ 
  guides(fill="none")+ 
  facet_grid(rows = vars(left))
  
## PLOT Density Plot for Time Spent Commuting ##
ggplot(data=hr_data.dt, aes(x=time_spend_company, fill=left))+ 
  geom_density(kernel="gaussian")+ 
  labs(title="Density Plot for Time Spent Commuting to Work")+ 
  guides(fill="none")+ 
  facet_grid(rows=vars(left))

## PLOT Bar Graph of Promotion by Departments ##
ggplot(data=hr_data.dt, aes(x=promotion_last_5years, fill=left))+
  geom_bar(position="dodge")+
  guides(fill="none")+ 
  facet_grid(rows=vars(left), cols=vars(Department))+
  labs(title="Bar Graph of Promotion in Last 5 Years by Departments")+
  geom_text(stat="count", aes(label=after_stat(count)), size=3, vjust=0.5, position=position_dodge(0.9))+
  theme(axis.text.x=element_text(size=10, angle=30, hjust=1, vjust=1))

## PLOT Bar Graph of Number of Projects ##
ggplot(data=hr_data.dt, aes(x=number_project, fill=left))+ 
  geom_bar(position="dodge")+ 
  labs(title="Bar Graph of Number of Projects")+ 
  guides(fill="none")+ 
  facet_grid(rows=vars(left))+ 
  geom_text(stat='count',aes(label = after_stat(count)), vjust=0.3)

#All insights derived from data exploration and visualisation can be found in the report (Data Exploration of Dataset)

##============= Preparation of Data for Machine Learning Models ==============##

## DROP Emp_ID Column ##
#Emp_ID not required in subsequent analysis, so we have decided to drop the Column
new_hr_data.dt=hr_data.dt[,Emp_Id:=NULL]
View(new_hr_data.dt)

## SPLIT New Dataset into Train & Test Sets ##
set.seed(2021)
train=sample.split(Y=new_hr_data.dt$left,SplitRatio = 0.7)
trainset=subset(new_hr_data.dt,train==T)
testset=subset(new_hr_data.dt,train==F)

## CHECK Distribution of Train & Test Sets ##
summary(trainset$left) 
#Current employee:8000
#Resigned:2500
summary(testset$left)
#Current employee:3428
#Resigned:1071

##============================ Approach 1 Code ===============================## 
#(1) Use logistic regression (univariate) to sieve out significant variables
#(2) Use logistic regression as the machine learning model to predict 'left' variable 

## PERFORM Logistic Regression on satisfaction_level ##
w1_reg_satisfaction=glm(left~satisfaction_level,family=binomial,data=trainset)
summary(w1_reg_satisfaction)
OR.CI_satisfaction=exp(confint(w1_reg_satisfaction))
OR.CI_satisfaction
#95% CI excludes 1 and as seen in summary(w1_reg_satisfaction)
#satisfaction_level is statistically significant

## PERFORM Logistic Regression on last_evaluation ##
w1_reg_evaluation=glm(left~last_evaluation,family=binomial,data=trainset)
summary(w1_reg_evaluation)
OR.CI_evaluation=exp(confint(w1_reg_evaluation))
OR.CI_evaluation
#95% CI includes 1 and as seen in summary(w1_reg_evaluation)
#last_evaluation is not statistically significant

## PERFORM Logistic Regression on number_project ##
w1_reg_project=glm(left~number_project,family=binomial,data=trainset)
summary(w1_reg_project)
OR.CI_project=exp(confint(w1_reg_project))
OR.CI_project
#95% CI excludes 1 and as seen in summary(w1_reg_project)
#number_project is statistically significant

## PERFORM Logistic Regression on average_montly_hours ##
w1_reg_hours=glm(left~average_montly_hours,family=binomial,data=trainset)
summary(w1_reg_hours)
OR.CI_hours=exp(confint(w1_reg_hours))
OR.CI_hours
#95% CI excludes 1 and as seen in summary(w1_reg_hours)
#average_montly_hours is statistically significant

## PERFORM Logistic Regression on time_spend_company ##
w1_reg_time=glm(left~time_spend_company,family=binomial,data=trainset)
summary(w1_reg_time)
OR.CI_time=exp(confint(w1_reg_time))
OR.CI_time
#95% CI excludes 1 and as seen in summary(w1_reg_time)
#time_spend_company is statistically significant

## PERFORM Logistic Regression on Work_accident ##
w1_reg_accident=glm(left~Work_accident,family=binomial,data=trainset)
summary(w1_reg_accident)
OR.CI_accident=exp(confint(w1_reg_accident))
OR.CI_accident
#Baseline:Involved
#95% CI excludes 1 and as seen in summary(w1_reg_accident)
#Work_accident is statistically significant

## PERFORM Logistic Regression on promotion_last_5years ##
w1_reg_promotion=glm(left~promotion_last_5years,family=binomial,data=trainset)
summary(w1_reg_promotion)
OR.CI_promotion=exp(confint(w1_reg_promotion))
OR.CI_promotion
#Baseline:Not Promoted
#95% CI excludes 1 and as seen in summary(w1_reg_promotion)
#promotion_last_5years is statistically significant

## PERFORM Logistic Regression on Department ##
w1_reg_department=glm(left~Department,family=binomial,data=trainset)
summary(w1_reg_department)
OR.CI_department=exp(confint(w1_reg_department))
OR.CI_department
#Baseline:IT
#Some departments have their 95% CI including 1 while others have their 95% CI excluding 1
#Decided to conclude that Department as a whole is not statistically significant

## PERFORM Logistic Regression on salary ##
w1_reg_salary=glm(left~salary,family=binomial,data=trainset)
summary(w1_reg_salary)
OR.CI_salary=exp(confint(w1_reg_salary))
OR.CI_salary
#Baseline:high
#95% CI excludes 1 and as seen in summary(w1_reg_salary)
#salary is statistically significant

## DEVELOP Predictive Logistic Regression Model ##
#Only statistically significant variables from the univariate analysis are used
#last_evaluation and Department are not used
w1_reg_predict=glm(left~satisfaction_level+number_project+average_montly_hours+
                  time_spend_company+Work_accident+promotion_last_5years+salary
                  ,family=binomial,data=trainset)

## CHECK for Multi-collinearity Issue ##
vif(w1_reg_predict)
#No multi-collinearity issue identified since all VIF<2

## DEVELOP Confusion Matrix based on Train Set ##
w1_threshold=0.5
w1_prob_train=predict(w1_reg_predict,type="response")
w1_reg_predict_train=ifelse(w1_prob_train>w1_threshold,"Resigned","Current employee")
w1_cm_train=table(train.actual=trainset$left,predicted=w1_reg_predict_train,deparse.level=2)
w1_cm_train
round(prop.table(w1_cm_train),3)

## ESTABLISH Accuracy of Model based on Train Set ##
mean(trainset$left==w1_reg_predict_train)
#0.7919048 (79.19%)

## DEVELOP Confusion Matrix based on Test Set ##
w1_prob_test=predict(w1_reg_predict,newdata=testset,type="response")
w1_reg_predict_test=ifelse(w1_prob_test>w1_threshold,"Resigned","Current employee")
w1_cm_test=table(test.actual=testset$left,predicted=w1_reg_predict_test,deparse.level=2)
w1_cm_test
round(prop.table(w1_cm_test),3)

## ESTABLISH Accuracy of Model based on Test Set ##
mean(testset$left==w1_reg_predict_test)
#0.788842 (78.88%)

## CREATE Comparison Table for True Positive/True Negative/False Positive/False Negative ##
w1_comparison_table=data.frame('Trainset'=1:6,'Testset'=1:6)
rownames(w1_comparison_table)=c('Accuracy','TP Rate','FN Rate','TN Rate','FP Rate','Precision Rate')

## ASSIGN Values from Confusion Matrix to Variables ##
TPtrain1=w1_cm_train[1,1]
TNtrain1=w1_cm_train[2,2]
FPtrain1=w1_cm_train[1,2]
FNtrain1=w1_cm_train[2,1]

TPtest1=w1_cm_test[1,1]
TNtest1=w1_cm_test[2,2]
FPtest1=w1_cm_test[1,2]
FNtest1=w1_cm_test[2,1]

## STORE Values in Comparison Table ##
w1_comparison_table[1,1] <- mean(trainset$left==w1_reg_predict_train) 
w1_comparison_table[1,2] <- mean(testset$left==w1_reg_predict_test) 
w1_comparison_table[2,1] <- TPtrain1/(TPtrain1+FNtrain1)
w1_comparison_table[2,2] <- TPtest1/(TPtest1+FNtest1)
w1_comparison_table[3,1] <- FNtrain1/(TPtrain1+FNtrain1)
w1_comparison_table[3,2] <- FNtest1/(TPtest1+FNtest1)
w1_comparison_table[4,1] <- TNtrain1/(TNtrain1+FPtrain1)
w1_comparison_table[4,2] <- TNtest1/(TNtest1+FPtest1)
w1_comparison_table[5,1] <- FPtrain1/(TNtrain1+FPtrain1)
w1_comparison_table[5,2] <- FPtest1/(TNtest1+FPtest1)
w1_comparison_table[6,1] <- TPtrain1/(TPtrain1+FPtrain1)
w1_comparison_table[6,2] <- TPtest1/(TPtest1+FPtest1)

## VIEW Comparison Table ##
View(w1_comparison_table)

##============================ Approach 2 Code ===============================##
#(1) Use logistic regression (multivariate) to sieve out significant variables
#(2) Use logistic regression as the machine learning model to predict 'left' variable

## PERFORM Multivariate Logistic Regression ##
w2_reg_full=glm(left~.,family=binomial,data=trainset) 
summary(w2_reg_full)
#All variables except Department are statistically significant
#Some departments are statistically significant while others are not statistically significant 

## CHECK for Multi-collinearity Issue ##
vif(w2_reg_full) 
#No multi-collinearity issue identified since all VIF<2

## PERFORM Backward Elimination ##
#To check if there is any least contributive dependent variable to drop
w2_backwards=step(w2_reg_full)
summary(w2_backwards)

## PERFORM Forward Elimination ##
w2_reg_nothing=glm(left~1,family=binomial,data=trainset)
summary(w2_reg_nothing)
w2_forwards=step(w2_reg_nothing, scope=list(lower=formula(w2_reg_nothing),upper=formula(w2_reg_full)),direction="forward")

## IDENTIFY Variables Dropped/Added based on Stepwise Formula ##
formula(w2_backwards)
formula(w2_forwards)
formula(w2_reg_full)
#On comparison, no variable was dropped during backward and forward eliminations
#Therefore, all variables are statistically significant
#w2_reg_full will be used as the model to predict the dependent variable 'left'

## DEVELOP Confusion Matrix based on Train Set ##
w2_threshold=0.5
w2_prob_train=predict(w2_reg_full,type="response")
w2_reg_predict_train=ifelse(w2_prob_train>w2_threshold,"Resigned","Current employee")
w2_cm_train=table(train.actual=trainset$left,predicted=w2_reg_predict_train,deparse.level=2)
w2_cm_train
round(prop.table(w2_cm_train),3)

## ESTABLISH Accuracy of Model based on Train Set ##
mean(trainset$left==w2_reg_predict_train) 
#0.7898095 (78.98%)

## DEVELOP Confusion Matrix based on Test Set ##
w2_prob_test=predict(w2_reg_full,newdata=testset,type="response")
w2_reg_predict_test=ifelse(w2_prob_test>w2_threshold,"Resigned","Current employee")
w2_cm_test=table(test.actual=testset$left,predicted=w2_reg_predict_test,deparse.level=2)
w2_cm_test
round(prop.table(w2_cm_test),3)

## ESTABLISH Accuracy of Model based on Test Set ##
mean(testset$left==w2_reg_predict_test) 
#0.7870638 (78.71%)

## CREATE Comparison Table for True Positive/True Negative/False Positive/False Negative ##
w2_comparison_table=data.frame('Trainset'=1:6,'Testset'=1:6)
rownames(w2_comparison_table)=c('Accuracy','TP Rate','FN Rate','TN Rate','FP Rate','Precision Rate')

## ASSIGN Values from Confusion Matrix to Variables ##
TPtrain2=w2_cm_train[1,1]
TNtrain2=w2_cm_train[2,2]
FPtrain2=w2_cm_train[1,2]
FNtrain2=w2_cm_train[2,1]

TPtest2=w2_cm_test[1,1]
TNtest2=w2_cm_test[2,2]
FPtest2=w2_cm_test[1,2]
FNtest2=w2_cm_test[2,1]

## STORE Values in Comparison Table ##
w2_comparison_table[1,1] <- mean(trainset$left==w2_reg_predict_train) 
w2_comparison_table[1,2] <- mean(testset$left==w2_reg_predict_test) 
w2_comparison_table[2,1] <- TPtrain2/(TPtrain2+FNtrain2)
w2_comparison_table[2,2] <- TPtest2/(TPtest2+FNtest2)
w2_comparison_table[3,1] <- FNtrain2/(TPtrain2+FNtrain2)
w2_comparison_table[3,2] <- FNtest2/(TPtest2+FNtest2)
w2_comparison_table[4,1] <- TNtrain2/(TNtrain2+FPtrain2)
w2_comparison_table[4,2] <- TNtest2/(TNtest2+FPtest2)
w2_comparison_table[5,1] <- FPtrain2/(TNtrain2+FPtrain2)
w2_comparison_table[5,2] <- FPtest2/(TNtest2+FPtest2)
w2_comparison_table[6,1] <- TPtrain2/(TPtrain2+FPtrain2)
w2_comparison_table[6,2] <- TPtest2/(TPtest2+FPtest2)

## VIEW Comparison Table ##
View(w2_comparison_table)

##============================ Approach 3 Code ===============================## 
#(1) Use CART to sieve out significant variables
#(2) Use CART as the machine learning model to predict 'left' variable

## DEVELOP CART Model ##
set.seed(2021)
w3_CART=rpart(left ~.,data=trainset,method="class",control=rpart.control(minsplit=20,cp=0))
#Given the size of our dataset, default minsplit of 20 is used
#Changed cp to 0 to ensure that tree will be grown to the max

## PLOT & PRINT Maximal Tree ##
rpart.plot(w3_CART,nn=T,main="Maximal Tree")
print(w3_CART)

## PLOT & PRINT Pruning Sequence and 10-fold CV Errors ##
plotcp(w3_CART)
printcp(w3_CART)

## DETERMINE CVerror Cap ##
w3_CVerror.cap=w3_CART$cptable[which.min(w3_CART$cptable[,"xerror"]),"xerror"]+
                w3_CART$cptable[which.min(w3_CART$cptable[,"xerror"]),"xstd"]
w3_CVerror.cap #0.1017131

## FIND Optimal CP Region ##
i <- 1; j<- 4
while(w3_CART$cptable[i,j]>w3_CVerror.cap){
  i <- i+1
}
w3_cp.opt=ifelse(i>1,sqrt(w3_CART$cptable[i,1]*w3_CART$cptable[i-1,1]),1)
w3_cp.opt #0.002529822

## PRUNE Maximal Tree ##
w3_CART_pruned=prune(w3_CART,cp=w3_cp.opt)
plotcp(w3_CART_pruned)
printcp(w3_CART_pruned)

## PLOT & PRINT Optimal Tree ##
rpart.plot(w3_CART_pruned,nn=T,main="Optimal Tree")
print(w3_CART_pruned)

## DETERMINE Variable Importance ##
w3_CART_pruned$variable.importance
round(prop.table(w3_CART_pruned$variable.importance)*100)

## DEVELOP Confusion Matrix based on Train Set ##
w3_CART_predict_train=predict(w3_CART_pruned,type="class")
w3_cm_train=table(observed=trainset$left,predicted=w3_CART_predict_train)
w3_cm_train
round(prop.table(w3_cm_train),3)

## ESTABLISH Accuracy of Model based on Train Set ##
mean(trainset$left==w3_CART_predict_train)
#0.9772381 (97.72%)

## DEVELOP Confusion Matrix based on Test Set ##
w3_CART_predict_test=predict(w3_CART_pruned,newdata=testset,type="class")
w3_cm_test=table(observed=testset$left,predicted=w3_CART_predict_test)
w3_cm_test
round(prop.table(w3_cm_test),3)

## ESTABLISH Accuracy of Model based on Test Set ##
mean(testset$left==w3_CART_predict_test)
#0.9773283 (97.73%)

## CREATE Comparison Table for True Positive/True Negative/False Positive/False Negative ##
w3_comparison_table=data.frame('Trainset'=1:6,'Testset'=1:6)
rownames(w3_comparison_table)=c('Accuracy','TP Rate','FN Rate','TN Rate','FP Rate','Precision Rate')

## ASSIGN Values from Confusion Matrix to Variables ##
TPtrain3=w3_cm_train[1,1]
TNtrain3=w3_cm_train[2,2]
FPtrain3=w3_cm_train[1,2]
FNtrain3=w3_cm_train[2,1]

TPtest3=w3_cm_test[1,1]
TNtest3=w3_cm_test[2,2]
FPtest3=w3_cm_test[1,2]
FNtest3=w3_cm_test[2,1]

## STORE Values in Comparison Table ##
w3_comparison_table[1,1] <- mean(trainset$left==w3_CART_predict_train)
w3_comparison_table[1,2] <- mean(testset$left==w3_CART_predict_test)
w3_comparison_table[2,1] <- TPtrain3/(TPtrain3+FNtrain3)
w3_comparison_table[2,2] <- TPtest3/(TPtest3+FNtest3)
w3_comparison_table[3,1] <- FNtrain3/(TPtrain3+FNtrain3)
w3_comparison_table[3,2] <- FNtest3/(TPtest3+FNtest3)
w3_comparison_table[4,1] <- TNtrain3/(TNtrain3+FPtrain3)
w3_comparison_table[4,2] <- TNtest3/(TNtest3+FPtest3)
w3_comparison_table[5,1] <- FPtrain3/(TNtrain3+FPtrain3)
w3_comparison_table[5,2] <- FPtest3/(TNtest3+FPtest3)
w3_comparison_table[6,1] <- TPtrain3/(TPtrain3+FPtrain3)
w3_comparison_table[6,2] <- TPtest3/(TPtest3+FPtest3)

## VIEW Comparison Table ##
View(w3_comparison_table)

##============================ Approach 4 Code ===============================## 
#(1) Use logistic regression (univariate) to sieve out significant variables
#(2) Use CART as the machine learning model to predict 'left' variable

#Refer to "Way 1 Code" for univariate logistic regression to sieve out significant variables
#last_evaluation and Department determined to be not statistically significant and will not be used

## DEVELOP CART Model ##
set.seed(2021)
w4_CART=rpart(left~satisfaction_level+number_project+average_montly_hours+ 
              time_spend_company+Work_accident+promotion_last_5years+salary, 
              data=trainset,method="class",control=rpart.control(minsplit=20,cp=0))
#Given the size of our dataset, default minsplit of 20 is used
#Changed cp to 0 to ensure that tree will be grown to the max

## PLOT & PRINT Maximal Tree ##
rpart.plot(w4_CART,nn=T,main="Maximal Tree")
print(w4_CART)

## PLOT & PRINT Pruning Sequence and 10-fold CV Errors ##
plotcp(w4_CART)
printcp(w4_CART)

## DETERMINE CVerror Cap ##
w4_CVerror.cap=w4_CART$cptable[which.min(w4_CART$cptable[,"xerror"]),"xerror"]+
                w4_CART$cptable[which.min(w4_CART$cptable[,"xerror"]),"xstd"]
w4_CVerror.cap #0.1041875

## FIND Optimal CP Region ##
i <- 1; j<- 4
while(w4_CART$cptable[i,j]>w4_CVerror.cap){
  i <- i+1
}
w4_cp.opt=ifelse(i>1,sqrt(w4_CART$cptable[i,1]*w4_CART$cptable[i-1,1]),1)
w4_cp.opt #0.003174902

## PRUNE Maximal Tree ##
w4_CART_pruned=prune(w4_CART,cp=w4_cp.opt)
plotcp(w4_CART_pruned)
printcp(w4_CART_pruned)

## PLOT & PRINT Optimal Tree ##
rpart.plot(w4_CART_pruned,nn=T,main="Optimal Tree")
print(w4_CART_pruned)

## DEVELOP Confusion Matrix based on Train Set ##
w4_CART_predict_train=predict(w4_CART_pruned,type="class")
w4_cm_train=table(observed=trainset$left,predicted=w4_CART_predict_train)
w4_cm_train
round(prop.table(w4_cm_train),3)

## ESTABLISH Accuracy of Model based on Train Set ##
mean(trainset$left==w4_CART_predict_train)
#0.9758095 (97.58%)

## DEVELOP Confusion Matrix based on Test Set ##
w4_CART_predict_test=predict(w4_CART_pruned,newdata=testset,type="class")
w4_cm_test=table(observed=testset$left,predicted=w4_CART_predict_test)
w4_cm_test
round(prop.table(w4_cm_test),3)

## ESTABLISH Accuracy of Model based on Test Set ##
mean(testset$left==w4_CART_predict_test)
#0.9735497 (97.35%)

## CREATE Comparison Table for True Positive/True Negative/False Positive/False Negative ##
w4_comparison_table=data.frame('Trainset'=1:6,'Testset'=1:6)
rownames(w4_comparison_table)=c('Accuracy','TP Rate','FN Rate','TN Rate','FP Rate','Precision Rate')

## ASSIGN Values from Confusion Matrix to Variables ##
TPtrain4=w4_cm_train[1,1]
TNtrain4=w4_cm_train[2,2]
FPtrain4=w4_cm_train[1,2]
FNtrain4=w4_cm_train[2,1]

TPtest4=w4_cm_test[1,1]
TNtest4=w4_cm_test[2,2]
FPtest4=w4_cm_test[1,2]
FNtest4=w4_cm_test[2,1]

## STORE Values in Comparison Table ##
w4_comparison_table[1,1] <- mean(trainset$left==w4_CART_predict_train)
w4_comparison_table[1,2] <- mean(testset$left==w4_CART_predict_test)
w4_comparison_table[2,1] <- TPtrain4/(TPtrain4+FNtrain4)
w4_comparison_table[2,2] <- TPtest4/(TPtest4+FNtest4)
w4_comparison_table[3,1] <- FNtrain4/(TPtrain4+FNtrain4)
w4_comparison_table[3,2] <- FNtest4/(TPtest4+FNtest4)
w4_comparison_table[4,1] <- TNtrain4/(TNtrain4+FPtrain4)
w4_comparison_table[4,2] <- TNtest4/(TNtest4+FPtest4)
w4_comparison_table[5,1] <- FPtrain4/(TNtrain4+FPtrain4)
w4_comparison_table[5,2] <- FPtest4/(TNtest4+FPtest4)
w4_comparison_table[6,1] <- TPtrain4/(TPtrain4+FPtrain4)
w4_comparison_table[6,2] <- TPtest4/(TPtest4+FPtest4)

## VIEW Comparison Table ##
View(w4_comparison_table)

##============================ Approach 5 Code ===============================## 
#(1) Use CART to sieve out significant variables
#(2) Use CART as the machine learning model to predict 'left' variable

## IDENTIFY Statistically Significant Variables ##
#Refer to "Way 3 Code" for use of CART to sieve out significant variables
w3_CART_pruned$variable.importance
round(prop.table(w3_CART_pruned$variable.importance)*100)
print(w3_CART_pruned)
#satisfaction_level, average_montly_hours, number_project, last_evaluation & time_spend_company are significant
#Other variables deemed to be not statistically significant

## DEVELOP Predictive Logistic Regression Model ##
#Only statistically significant variables are used
w5_reg_predict=glm(left~satisfaction_level+number_project+average_montly_hours +
                  last_evaluation+time_spend_company,family=binomial,data=trainset)
summary(w5_reg_predict)

## CHECK for Multi-collinearity Issue ##
vif(w5_reg_predict)
#No multi-collinearity issue identified since all VIF<2

## DEVELOP Confusion Matrix based on Train Set ##
w5_threshold=0.5
w5_prob_train=predict(w5_reg_predict,type="response")
w5_reg_predict_train=ifelse(w5_prob_train>w5_threshold,"Resigned","Current employee")
w5_cm_train=table(train.actual=trainset$left,predicted=w5_reg_predict_train,deparse.level=2)
w5_cm_train
round(prop.table(w5_cm_train),3)

## ESTABLISH Accuracy of Model based on Train Set ##
mean(trainset$left==w5_reg_predict_train)
#0.7678095 (76.78%)

## DEVELOP Confusion Matrix based on Test Set ##
w5_prob_test=predict(w5_reg_predict,newdata=testset,type="response")
w5_reg_predict_test=ifelse(w5_prob_test>w5_threshold,"Resigned","Current employee")
w5_cm_test=table(test.actual=testset$left,predicted=w5_reg_predict_test,deparse.level=2)
w5_cm_test
round(prop.table(w5_cm_test),3)

## ESTABLISH Accuracy of Model based on Test Set ##
mean(testset$left==w5_reg_predict_test)
#0.7603912 (76.04%)

## CREATE Comparison Table for True Positive/True Negative/False Positive/False Negative ##
w5_comparison_table=data.frame('Trainset'=1:6,'Testset'=1:6)
rownames(w5_comparison_table)=c('Accuracy','TP Rate','FN Rate','TN Rate','FP Rate','Precision Rate')

## ASSIGN Values from Confusion Matrix to Variables ##
TPtrain5=w5_cm_train[1,1]
TNtrain5=w5_cm_train[2,2]
FPtrain5=w5_cm_train[1,2]
FNtrain5=w5_cm_train[2,1]

TPtest5=w5_cm_test[1,1]
TNtest5=w5_cm_test[2,2]
FPtest5=w5_cm_test[1,2]
FNtest5=w5_cm_test[2,1]

## STORE Values in Comparison Table ##
w5_comparison_table[1,1] <- mean(trainset$left==w5_reg_predict_train)
w5_comparison_table[1,2] <- mean(testset$left==w5_reg_predict_test)
w5_comparison_table[2,1] <- TPtrain5/(TPtrain5+FNtrain5)
w5_comparison_table[2,2] <- TPtest5/(TPtest5+FNtest5)
w5_comparison_table[3,1] <- FNtrain5/(TPtrain5+FNtrain5)
w5_comparison_table[3,2] <- FNtest5/(TPtest5+FNtest5)
w5_comparison_table[4,1] <- TNtrain5/(TNtrain5+FPtrain5)
w5_comparison_table[4,2] <- TNtest5/(TNtest5+FPtest5)
w5_comparison_table[5,1] <- FPtrain5/(TNtrain5+FPtrain5)
w5_comparison_table[5,2] <- FPtest5/(TNtest5+FPtest5)
w5_comparison_table[6,1] <- TPtrain5/(TPtrain5+FPtrain5)
w5_comparison_table[6,2] <- TPtest5/(TPtest5+FPtest5)

## VIEW Comparison Table ##
View(w5_comparison_table)

#===========================End Of Team Project Code============================