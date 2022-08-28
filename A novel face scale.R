
#-------------------------------------------------------------------------------
# Title: qVIEWS
# ------------------------------------------------------------------------------

# Author: Jeronimo Moreno Cuesta
# Date: 4.5.2022

# Read data 
data <- read.table("data_final.txt",header=T,sep="\t")
head(data)
data <-data[1:398,-c(22,23)]
names(data)
fix(data)

# Prepare data
str(data)
Rdate1 <- strptime(as.character(data$sreen_date),"%d/%m/%Y")
data <- data.frame(data,Rdate1)
data$group <- as.factor(data$group)
data$sex <- as.factor(data$sex)
data$ethnicity <- as.factor(data$ethnicity)
data$consent <- as.factor(data$consent)
data$type_of_consent <- as.factor(data$type_of_consent)
data$air_oxygen <- as.factor(data$air_oxygen)
data$avpu <- as.factor(data$avpu)
data$itu_admission <- as.factor(data$itu_admission)
data$time_after_review <- as.factor(data$time_after_review)
data$outcome <- as.factor(data$outcome)
summary(data)
data$air_oxygen[data$air_oxygen=="1%"] <- 1
summary(data)
data_consent <- data[data$consent=="1",]

# Inter-rater variability 
class(data_consent$assessor1)
class(data_consent$assessor2)
db_agree <- as.data.frame (cbind(data_consent$assessor1,data_consent$assessor2))
library(irr)
library(psych)
cohen.kappa(db_agree)# we use weighted estimates for nominal scales such as assessor

# Demography high-risk patients
data_consent_highrisk<- data_consent[data_consent$Total >= 5,]
summary(data_consent_highrisk)
sd(data_consent_highrisk$age)
sd(data_consent_highrisk$Total)
sd(data_consent_highrisk$rr,na.rm=T)
sd(data_consent_highrisk$sat,na.rm=T)
sd(data_consent_highrisk$sbp,na.rm=T)
sd(data_consent_highrisk$hr,na.rm=T)
sd(data_consent_highrisk$temp,na.rm=T)
sd(data_consent_highrisk$assessor1,na.rm=T)

air_oxygen_factor <- as.factor(data_consent_highrisk$air_oxygen)
summary(air_oxygen_factor)
avpu_numeric <- as.numeric(data_consent_highrisk$avpu)
summary(avpu_numeric)
sd(avpu_numeric,na.rm=T)

# Demography low-risk patients
data_consent_lowrisk<- data_consent[data_consent$Total <= 4,]
summary(data_consent_lowrisk)
plot(data_consent_lowrisk$sat)# There are outliers / errors
which(data_consent_lowrisk$sat < 70)
data_consent_lowrisk_sat_corrected <- data_consent_lowrisk[-c(36,181),]
plot(data_consent_lowrisk_sat_corrected$sat)# We checked rows 36 and 86 removed
summary(data_consent_lowrisk_sat_corrected$sat)
sd(data_consent_lowrisk_sat_corrected$sat,na.rm=T)
sd(data_consent_lowrisk$age)
sd(data_consent_lowrisk$Total)
sd(data_consent_lowrisk$rr,na.rm=T)
sd(data_consent_lowrisk$sbp,na.rm=T)
sd(data_consent_lowrisk$hr,na.rm=T)
sd(data_consent_lowrisk$temp,na.rm=T)
sd(data_consent_lowrisk$assessor1,na.rm=T)

air_oxygen_factor_lr <- as.factor(data_consent_lowrisk$air_oxygen)
summary(air_oxygen_factor_lr)
avpu_numeric_lr <- as.numeric(data_consent_lowrisk$avpu)
summary(avpu_numeric_lr)
sd(avpu_numeric_lr,na.rm=T)

# Comparison demography high-risk and low-risk: FIRST ASSESSING NORMALITY OF VARIABLES
##Age
qqnorm(data_consent_highrisk$age,pch=16,col="red")
qqline(data_consent_highrisk$age,lty=2,col="blue") 
shapiro.test(data_consent_highrisk$age)# Age not normal, then we do Wilcoxon
wilcox.test(data_consent_highrisk$age,data_consent_lowrisk$age,paired=FALSE)
#For table 1 then we use median and IQR 
summary(data_consent_highrisk$age)
summary(data_consent_lowrisk$age)


##Sex: you have to create a group to compare vectors of different length
names(data_consent)
group_corrected <- ifelse(data_consent$Total >= 5,1,0)# 1 = high-risk, 0=low-risk) 
t1 <- table(group_corrected,data_consent$sex)
chisq.test(t1)

##Ethnicity
t2 <- table(group_corrected,data_consent$ethnicity);t2
chisq.test(t2)

##NEWS2 score
qqnorm(data_consent_highrisk$Total,pch=16,col="red")
qqline(data_consent_highrisk$Total,lty=2,col="blue") 
shapiro.test(data_consent_highrisk$Total)
wilcox.test(data_consent_highrisk$Total,data_consent_lowrisk$Total,paired=FALSE) 
summary(data_consent_highrisk$Total)
summary(data_consent_lowrisk$Total)

## Respiratory rate
qqnorm(data_consent_highrisk$rr,pch=16,col="red")
qqline(data_consent_highrisk$rr,lty=2,col="blue")
shapiro.test(data_consent_highrisk$rr)
wilcox.test(data_consent_highrisk$rr,data_consent_lowrisk$rr,paired=FALSE) 
summary(data_consent_highrisk$rr)
summary(data_consent_lowrisk$rr)

## Oxygen saturation
qqnorm(data_consent_highrisk$sat,pch=16,col="red")
qqline(data_consent_highrisk$sat,lty=2,col="blue")
shapiro.test(data_consent_highrisk$sat)
wilcox.test(data_consent_highrisk$sat,data_consent_lowrisk$sat,paired=FALSE) 
summary(data_consent_highrisk$sat)
summary(data_consent_lowrisk$sat)

##Oxygen administration. There is a misterious level called"1%" that has not value!
levels(data_consent$air_oxygen)[levels(data_consent$air_oxygen)== '1%'] <- NA # to remove it
t3 <- table(group_corrected,data_consent$air_oxygen);t3
chisq.test(t3)


## Systolic blood pressure
qqnorm(data_consent_highrisk$sbp,pch=16,col="red")
qqline(data_consent_highrisk$sbp,lty=2,col="blue")
shapiro.test(data_consent_highrisk$sbp)# It is normal, hence t-test
var.test(data_consent_highrisk$sbp,data_consent_lowrisk$sbp)# variance are different
t.test(data_consent_highrisk$sbp,data_consent_lowrisk$sbp,paired=FALSE,var.equal=FALSE,
conf.level =0.95,alternative="two.sided")

##Heart rate
qqnorm(data_consent_highrisk$hr,pch=16,col="red")
qqline(data_consent_highrisk$hr,lty=2,col="blue")
shapiro.test(data_consent_highrisk$hr)
var.test(data_consent_highrisk$hr,data_consent_lowrisk$hr)# Var are different
t.test(data_consent_highrisk$hr,data_consent_lowrisk$hr,paired=FALSE,var.equal=FALSE,
conf.level =0.95,alternative="two.sided")

##AVPU
avpu_numeric_highrisk <- as.numeric(data_consent_highrisk$avpu)
avpu_numeric_lowrisk <- as.numeric(data_consent_lowrisk$avpu)
qqnorm(avpu_numeric_highrisk,pch=16,col="red")
qqline(avpu_numeric_highrisk,lty=2,col="blue")
shapiro.test(avpu_numeric_highrisk)
wilcox.test(avpu_numeric_highrisk,avpu_numeric_lowrisk,paired=FALSE)
summary(avpu_numeric_highrisk)
summary(avpu_numeric_lowrisk)

##Temperature
qqnorm(data_consent_highrisk$temp,pch=16,col="red")
qqline(data_consent_highrisk$temp,lty=2,col="blue")
shapiro.test(data_consent_highrisk$temp)
wilcox.test(data_consent_highrisk$temp,data_consent_lowrisk$temp,paired=FALSE)
summary(data_consent_highrisk$temp)
summary(data_consent_lowrisk$temp)

#qVIEWS
qqnorm(data_consent_highrisk$assessor1,pch=16,col="red")
qqline(data_consent_highrisk$assessor1,lty=2,col="blue")
shapiro.test(data_consent_highrisk$assessor1)
wilcox.test(data_consent_highrisk$assessor1,data_consent_lowrisk$assessor1,paired=FALSE)
summary(data_consent_highrisk$assessor1)
summary(data_consent_lowrisk$assessor1)

## Preparing for Rigde regression
#Removing NA and selecting variables
data_consent <- data[data$consent=="1",]
names(data_consent)
newdata <- data_consent[,c(4:6,10:17,19)]
names(newdata)
newdata <- na.omit(newdata)

#Looking for correlation between NEWS and qVIEWS all numerical variables
names(data_consent)
str(data_consent)
data_consent$avpu <- as.numeric(data_consent$avpu)
names(data_consent)
data_consent_numerical <- na.omit(data_consent)
XVars <- data_consent_numerical[,c(9,10,11,13,14,15,16,17)] # X numeric variables
round(cor(XVars), 2) # Correlation 
## # to check if cor between Total News and qVIEWS is significant
cor.test(data_consent_numerical$Total,data_consent_numerical$assessor1)

#RIDGE REGRESSION FOR NEWS variables alone
news_data1 <- newdata[,c(4:10,12)]
names(news_data1)

##Splitting the sample (internal validation)

x1 <- model.matrix(news_data1$itu_admission~.,news_data1)[, -1]
y1 <- news_data1$itu_admission
set.seed(1)
train1 <- sample(1:nrow(x1),nrow(x1)/2)
test1 <- (-train1)
y.test1 <-y1[test1]
library(glmnet)
grid <- 10^seq(10,-2,length=100)

## Calculating best lambda
set.seed(1)
cv.out1 <- cv.glmnet(x1[train1,],y1[train1],alpha=0,family=binomial,type.measure="class")
plot(cv.out1)
bestlam1 <- cv.out1$lambda.min
bestlam1

##Doing a ridge regression 
out1 <- glmnet(x1[train1,],y1[train1],alpha=0,lamda=grid,family=binomial)
ridge.coeff1 <- predict(out1,s = bestlam1,newx = x1[test1 , ],type="coefficients")
ridge.coeff1 

## Doing AUC for NEWS
ridge.coeff1 <- predict(out1,s = bestlam1,newx = x1[test1 , ],type="response")
library(pROC)
roccurve1 <- roc(y1[test1]~ ridge.coeff1)
plot(roccurve1)
auc(roccurve1)
ci.auc(roccurve1)


#RIDGE REGRESSION FOR NEWS variables + qVIEWS 
news_data2 <- newdata[,c(4:12)]# Adding assessor1 which is qVIEWS
names(news_data2)

##Splitting the sample (internal validation)

x2 <- model.matrix(news_data2$itu_admission~.,news_data2)[, -1]
y2 <- news_data2$itu_admission
set.seed(1)
train2 <- sample(1:nrow(x2),nrow(x2)/2)
test2 <- (-train2)
y.test2 <-y2[test2]
library(glmnet)
grid <- 10^seq(10,-2,length=100)

## Calculating best lambda
set.seed(1)
cv.out2 <- cv.glmnet(x2[train2,],y2[train2],alpha=0,family=binomial,type.measure="class")
plot(cv.out2)
bestlam2 <- cv.out2$lambda.min
bestlam2

##Doing a ridge regression 
out2 <- glmnet(x2[train2,],y2[train2],alpha=0,lamda=grid,family=binomial)
ridge.coeff2 <- predict(out2,s = bestlam2,newx = x2[test2 , ],type="coefficients")
ridge.coeff2 


## Doing AUC for NEWS+qVIEWS
ridge.coeff2 <- predict(out2,s = bestlam2,newx = x2[test2, ],type="response")
library(pROC)
roccurve2 <- roc(y2[test2]~ ridge.coeff2)
plot(roccurve2)
auc(roccurve2)
ci.auc(roccurve2)

#GRAPH THAT COMBINES BOTH AUROC
plot.roc(y2[test2]~ ridge.coeff2,percentage=T,col="red",smooth=T)# for NEWS+qVIEWS
grid(NULL,NULL,col = "lightgray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
lines.roc(y1[test1]~ ridge.coeff1,percentage=T,col="blue",smooth=T)# For NEWS only
legend(locator(1),c("AUC (NEWS + qVIEWS): 56.2%","AUC (NEWS): 56.6%"),
lty=c(1,1),col=c("red","blue"))

## RANDOM FOREST
data_consent <- data[data$consent=="1",]
names(data_consent)
names(newdata)
newdata <- data_consent[,c(10:17,19)]

newdata <- na.omit(newdata)
names(newdata)


library(randomForest)

# Separate train and test sample
set.seed(1)
library(caTools)
sample <- sample.split(newdata$itu_admission,SplitRatio = 0.5)
train  <- subset(newdata,sample == TRUE)
testdata <- subset(newdata, sample == FALSE)
names(train)
names(testdata)
dim(train)
dim(testdata)

# Random Forest
set.seed (1)
library(randomForest)
rf.newdata <- randomForest(newdata$itu_admission~.,data= newdata,subset=unlist(train),
mtry =3,importance = TRUE)
importance(rf.newdata)
varImpPlot(rf.newdata)

# A better way to present the importance value as barplot
var.1 <- c("RR","SAT","AIR/OX","SBP","HR","AVPU",
"TEMP","qVIEWS")
Gini <- c(97.3,160.2,22.3,81.0,92.0,11.9,185.6,36.9)
Gini_per <- (Gini*100)/185.6
dt1 <- data.frame(var.1,Gini_per)
library(ggplot2)
library(reshape2)
library(tidyverse)
dt2 <- melt(dt1,id.vars='var.1')

col_grid <- rgb(235, 235, 235, 100, maxColorValue = 255)

ggplot(dt2, aes(fct_reorder(var.1,value),value, fill=variable))+
labs(y= "Variable Importance", x = "Variable Name")+
theme_bw()+
theme(panel.grid = element_line(color = col_grid))+
theme(legend.position="none")+
theme(panel.border = element_blank(), axis.line = element_line())+
geom_bar(stat='identity',position='dodge')


pred1 <- predict(rf.newdata,newdata=testdata,type="class")
table(pred1,testdata$itu_admission)


(40+68)/158 # Correct predictions for around 68% of the locations in the test data set

#Sensitivity, specificity of random forest prediction versus itu_admission in test sample
mx <- matrix(c(68,44,6,40),byrow=T,nrow=2)# Order is modified to allow itu_admission (1) as "1"
mx
library(caret)
confusionMatrix(mx)

#------------------------------------------------------------------------------------------------------------
#End of data analysis and end of script