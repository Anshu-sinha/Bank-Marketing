
#clearing the global enviornment
rm(list = ls())

#setting the directory and reading the data
setwd("F:/BA-R programming/STAT case study/3. BANK MARKETING CASE STUDY - CLASSIFICATION")
Mydata_train <- read.csv("Train.csv")

#Understanding the data
str(Mydata_train)
summary(Mydata_train)
names(Mydata_train)
View(Mydata_train)
nrow(Mydata_train)
ncol(Mydata_train)
dim(Mydata_train)

#understanding the distribution of data
library(psych)
summary <- describe(Mydata_train)
View(summary)
write.csv(summary,"summary.csv")

str(Mydata_train)

#creating the function for audit report on Numerical variabls

mystat_num <- function(x){
  n= length(x)
  nmiss=sum(is.na(x))
  nmiss_pct=mean(is.na(x))
  sum=sum(x,na.rm = T)
  mean=mean(x,na.rm = T)
  median=median(x,na.rm = T)
  std_dev=sd(x,na.rm = T)
  Cv= sd(x,na.rm = T)/mean(x,na.rm = T)
  variance= var(x,na.rm = T)
  range=max(x,na.rm = T)-min(x,na.rm = T)
  pctl = quantile(x,p=c(0,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1),na.rm = T)
  return(c(N=n, Nmiss=nmiss, Nmiss_perc=nmiss_pct, Sum=sum, Mean=mean, Median=median, Std_dev=std_dev,
           CV=Cv, Variance=variance, range=range, pctl=pctl))
}

#user defined function for categorical variable

mystat_char <- function(x){
  var_type=class(x)
  n=length(x)
  nmiss=sum(is.na(x))
  freq=list(table(x))
  prop= list(prop.table(table(x)))
  return(c(var_type=var_type, N=n, Nmiss=nmiss,Freq= freq, Proportion=prop))
}

names(Mydata_train)


#separating the variables into numerical and categorical
num_var <- names(Mydata_train)[sapply(Mydata_train,FUN = is.numeric)]
char_var <- names(Mydata_train)[sapply(Mydata_train,FUN = is.factor)]


#getting the audit report of all the numerical variables
summary_stat_num <- t(apply(Mydata_train[num_var],2,FUN = mystat_num))
View(summary_stat_num)

#getting the audit report of all the categorical variables
summary_stat_char <- t(apply(Mydata_train[char_var],2,FUN = mystat_char))
View(summary_stat_char)


#Outlier treatmet
outlier_treat <- function(x){
  UC1=quantile(x,p=0.99,na.rm = T)
  LC1=quantile(x,p=0.01,na.rm = T)
  
  x=ifelse(x>UC1,UC1,x)
  x=ifelse(x<LC1,LC1,x)
  return(x)
}

mydata_num <- data.frame(apply(Mydata_train[num_var],2,FUN = outlier_treat))
View(mydata_num)

#number of missing in each variable
sapply(Mydata_train,function(x) sum(is.na(x)))

#missing value treatment for numerical variable
miss_treat_num <- function(x){
  x[is.na(x)]=median(x,na.rm = T)  #replace missing value with mean
  return(x)
}

mydata_num <- data.frame(apply(mydata_num,2,FUN = miss_treat_num))


#missing value treatment for categorical variable
miss_treat_Char <- function(x){
  x[is.na(x)] = x[which.max(prop.table(table(table(x))))]   #replacing the missings with mode
  return(x)
}

mydata_char <- data.frame(apply(Mydata_train[char_var],2,FUN = miss_treat_Char))

#Number of missing in each variable
sapply(mydata_num,function(x) sum(is.na(x)))
sapply(mydata_char,function(x) sum(is.na(x)))


#corelation
cor_mat <- cor(mydata_num)  #Warning message:In cor(mydata_num) : the standard deviation is zero
write.csv(cor_mat,"cor_mat.csv")

names(mydata_num)

fit <- aov(Disbursed~Interest_Rate, data = mydata_num)
summary(fit)

#creating the function to check all the significant variables.
aov_cal <- function(x){
  fit <- aov(x~Disbursed, data = mydata_num)
  return(list(summary(fit)))
}
#checking the significance of the x variables
apply(mydata_num,2,FUN = aov_cal)


#Here we will join the two variables tables i.e mydata_num and mydata_char, so that we can get the treated data 

Mydata_train1 <- cbind(mydata_char,mydata_num)

#Removing the not required data
Mydata_train1$Var1 <- NULL
Mydata_train1$Var2 <- NULL
Mydata_train1$Var5<- NULL
Mydata_train1$Var4 <- NULL
Mydata_train1$Source <- NULL

#converting categorical variable into Dummy variable
names(mydata_char)

##1st is Gender
Mydata_train1$Gender <- factor(Mydata_train1$Gender)
library("caret")
dv1 <- dummyVars(~Gender,data = Mydata_train1)
dummy_Gender <- data.frame(predict(dv1,Mydata_train1))[-1]
View(dummy_Gender)

##2nd is salary_account
#we are not including this variable becase it is creating a dummy variable of 57 variable 
#Mydata_train$Salary_Account <- factor(Mydata_train$Salary_Account)
#dv2 <- dummyVars(~Salary_Account,data = Mydata_train)
#dummy_Salary_account <- data.frame(predict(dv2,Mydata_train))[-1]
#View(dummy_Salary_account)

##3rd is mobile verified
Mydata_train1$Mobile_Verified <- factor(Mydata_train1$Mobile_Verified)
dv3 <- dummyVars(~Mobile_Verified,data = Mydata_train1)
dummy_Mobile_verified <- data.frame(predict(dv3,Mydata_train1))[-1]
View(dummy_Mobile_verified)

##4th is Filled_FOrm
Mydata_train1$Filled_Form <- factor(Mydata_train1$Filled_Form)
dv4 <- dummyVars(~Filled_Form,data = Mydata_train1)
dummy_Filled_form <- data.frame(predict(dv4,Mydata_train1))[-1]
View(dummy_Filled_form)

##5th is Device_type
Mydata_train1$Device_Type <- factor(Mydata_train1$Device_Type)
dv5 <- dummyVars(~Device_Type,data = Mydata_train1)
dummy_Device_type <- data.frame(predict(dv5,Mydata_train1))[-1]
View(dummy_Device_type)


#Joining all the dummy variables with which we will create a newdata set.

Mydata_train2 <- cbind(Mydata_train1,dummy_Device_type,dummy_Filled_form,
                       dummy_Gender,dummy_Mobile_verified)

Mydata_train2$Device_Type <- NULL
Mydata_train2$Filled_Form <- NULL
Mydata_train2$Gender <- NULL
Mydata_train2$Mobile_Verified <- NULL



#splitting data into training and testing datsets
train_ind <- sample(1:nrow(Mydata_train2),size = floor(0.7*nrow(Mydata_train1)))

testing <- Mydata_train2[-train_ind,]
training <- Mydata_train2[train_ind,]


#Building models for training datasets
names(training)

#Finally checking the significance for the whole data set which prepared for glm
aov_cal1 <- function(x){
  fit <- aov(Disbursed~x,data = training)
  return(list(summary(fit)))
}

apply(training,2,FUN = aov_cal1)

#these are significant x variables
# DOB**, Monthly_Income***, #Loan_Tenure_Applied*, Existing_EMI***, #Loan_Amount_Submitted*,
# Loan_Tenure_Submitted***, Interest_Rate***, Processing_Fee***, EMI_Loan_Submitted***, LoggedIn***, 
# #Filled_Form.Y*, Gender.Male***, #Mobile_Verified.Y*

# fit <- glm(Disbursed ~ DOB + Monthly_Income + Loan_Tenure_Applied + Existing_EMI + 
#              Loan_Amount_Submitted + Loan_Tenure_Submitted + Interest_Rate + Processing_Fee + 
#              EMI_Loan_Submitted + LoggedIn + Filled_Form.Y + Gender.Male + Mobile_Verified.Y,
#            data = training,family = binomial(logit))

fit <- glm(Disbursed ~  Monthly_Income  + Existing_EMI + Loan_Tenure_Submitted +Filled_Form.Y+
             Loan_Amount_Submitted + Interest_Rate + Processing_Fee + EMI_Loan_Submitted + 
             LoggedIn + Gender.Male, data = training,family = binomial(logit))
####Don't apply the glm on whole dataset untill you find all x variables significant####
summary(fit)
str(Mydata_train2)

#now prediction is done
p <- predict(fit,type = "response")
summary(p)

library(InformationValue)

train1 <- cbind(training,prob=predict(fit,type = "response"))
Concordance(train1$Disbursed,train1$prob)
somersD(train1$Disbursed,train1$prob)
table(train1$Disbursed)
AUROC(train1$Disbursed,train1$prob)

cut1 <- optimalCutoff(train1$Disbursed,train1$prob, optimiseFor = "Both", returnDiagnostics = T)
# $optimalCutoff
# [1] 0.3794517

ROCTable<-data.frame(cut1$sensitivityTable)
View(ROCTable)

train1$pred_Y <- ifelse(train1$prob>0.3794517,1,0)

#to see whether the predicted value placed correctly or not
confusionMatrix(train1$Disbursed,train1$pred_Y)

confusionMatrix(train1$Disbursed,train1$prob, threshold = 0.25)

sum(train1$Disbursed)
plotROC(train1$Disbursed,train1$prob, Show.labels = F)

sum(is.na()
    
    
# table(Mydata_train2$Disbursed)
 
 
#read,dob <- age, remove unwanted colmn, udfunction cat and num, separate them,missing with mean or median,
#outlier on categ,dummy wll cobine data,sample val,
