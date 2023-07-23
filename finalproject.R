#importing libraries
library(Amelia)
library(plotrix)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(Hmisc)
library(corrplot)
library(caTools) 
library(caret)
library(MLmetrics)
#To build Decision Tree model
library(rpart)
#TO build Random Forest
library(randomForest)
#For building SVM
library(e1071)

#Initialize the global environment
rm(list=ls(all=TRUE))

#Setting the working directory. The folder where data resides
setwd("E:/Kaviya/Project/CreditCardData")

#importing Credit Card Data into R
cdata<-read.csv('UCI_Credit_Card.csv')

#Display the dimension of the dataset
dim(cdata)

#Display the structure of R objects
str(cdata)


#Data Conversion
cdata$Defaulter<-as.factor(cdata$Defaulter)
str(cdata)



#DATA PREPROCESSING
#No of Rows
nrow(cdata)
#Identify incomplete rows of a dataset
nrow(cdata[!complete.cases(cdata),])
#Drawing a map of the missingness in a dataset using image function
missmap(cdata,main="Missing vs Observed Value")

#checking duplication in dataframe
any(duplicated(cdata))


#FEATURE ENGINEERING
#Varaible 'MARRIAGE' has 3 categories: 1=married,2=single,3=others but it contains a category '0' will joined to '3'
unique(cdata$MARRIAGE)
cdata["MARRIAGE"][cdata["MARRIAGE"]==0]<-3
unique(cdata$MARRIAGE)
#Varaible 'EDUCATION' has 3 categories with similar info: 0,5,6 grouped into class'4'
unique(cdata$EDUCATION)
cdata["EDUCATION"][cdata["EDUCATION"]==0 | cdata["EDUCATION"]==5 | cdata["EDUCATION"]==6 ]<-4
unique(cdata$EDUCATION)
#removing unwanted attributes
str(cdata)

#removing id attribute
cdata<-cdata[,-1]
dim(cdata)
str(cdata)
#View Summary
summary(cdata)


#converting Defaulter column into table
(table<-table(cdata$Defaulter))
#Rounding values to 100% 
(pent=round(table/sum(table)*100))
#plotting Pie chart 
label <-paste(pent,"%",sep="")
pie3D(table,height=0.12,labels=label,main="Percentage of Credit Card Clients",radius = 1,explode = 0, labelcex=1.5,col=c("#45b3e0","#eb87ce"),shade=0.5)
par(xpd=TRUE)
legend(1,0.7,legend=c("Defaulter","Non Defaulter"),cex=0.9,yjust=0.6, xjust = -0.05,fill = c("#45b3e0","#eb87ce"))  
#This graph shows the high prevalence in the data, only 22% of the clients have a default payment next month and the other 78% do not.


#splitting defaulter and non defaulter value by genderwise 
male<-subset(cdata,SEX=="1",select=c(SEX,Defaulter))
cmalendf<-nrow(subset(male,Defaulter=="0",select=c(Defaulter)))
cmaledf<-nrow(subset(male,Defaulter=="1",select=c(Defaulter)))
female<-subset(cdata,SEX=="2",select=c(SEX,Defaulter))
cfemalendf<-nrow(subset(female,Defaulter=="0",select=c(Defaulter)))
cfemaledf<-nrow(subset(female,Defaulter=="1",select=c(Defaulter)))
#storing them as dataframe
data1<-data.frame(Gender=c("Female","Female","Male","Male"),Defaulter=c("Yes","No"),Count=c(cfemaledf,cfemalendf,cmaledf,cmalendf))
(pmaledf<-paste(round(cmaledf/sum(cmaledf+cmalendf)*100),"%",sep=""))
(pmalendf<-paste(round(cmalendf/sum(cmaledf+cmalendf)*100),"%",sep=""))
(pfemaledf<-paste(round(cfemaledf/sum(cfemaledf+cfemalendf)*100),"%",sep=""))
(pfemalendf<-paste(round(cfemalendf/sum(cfemaledf+cfemalendf)*100),"%",sep=""))
data1
#plotting stacked bar plot
p<-ggplot(data1,aes(y=Count,x=Gender,fill=Defaulter))+geom_bar(stat="identity",width=0.3)+labs(title="GENDER WISE DEFAULTER PERCENT",size=9)+geom_text(aes(label=c(pfemaledf,pfemalendf,pmaledf,pmalendf)),size=5,position=position_stack(vjust=0.5))
p+theme(plot.title = element_text(face="bold",hjust=0.5))
#From the  charts, we can see that there are more females than male in the data set. However, the rate of default is higher among male.


#Marritial status based distribution
married<-subset(cdata,MARRIAGE=="1",select=c(MARRIAGE,Defaulter))
cmarrieddf<-nrow(subset(married,Defaulter=="1",select=c(Defaulter)))
cmarriedndf<-nrow(subset(married,Defaulter=="0",select=c(Defaulter)))
unmarried<-subset(cdata,MARRIAGE=="2",select=c(MARRIAGE,Defaulter))
cunmarrieddf<-nrow(subset(unmarried,Defaulter=="1",select=c(Defaulter)))
cunmarriedndf<-nrow(subset(unmarried,Defaulter=="0",select=c(Defaulter)))
mothers<-subset(cdata,MARRIAGE=="3",select=c(MARRIAGE,Defaulter))
cmothersdf<-nrow(subset(mothers,Defaulter=="1",select=c(Defaulter)))
cmothersndf<-nrow(subset(mothers,Defaulter=="0",select=c(Defaulter)))
data2<-data.frame(MARITAL_STATUS=c("MARRIED","MARRIED","SINGLE","SINGLE","OTHERS","OTHERS"),Defaulter=c("Yes","No","Yes","No","Yes","No"),COUNT=c(cmarrieddf,cmarriedndf,cunmarrieddf,cunmarriedndf,cmothersdf,cmothersndf))
data2
(pmarrieddf<-paste(round(cmarrieddf/sum(cmarrieddf+cmarriedndf)*100),"%",sep=""))
(pmarriedndf<-paste(round(cmarriedndf/sum(cmarrieddf+cmarriedndf)*100),"%",sep=""))
(punmarrieddf<-paste(round(cunmarrieddf/sum(cunmarrieddf+cunmarriedndf)*100),"%",sep=""))
(punmarriedndf<-paste(round(cunmarriedndf/sum(cunmarrieddf+cunmarriedndf)*100),"%",sep=""))
(pmothersdf<-paste(round(cmothersdf/sum(cmothersdf+cmothersndf)*100),"%",sep=""))
(pmothersndf<-paste(round(cmothersndf/sum(cmothersdf+cmothersndf)*100),"%",sep=""))
p<-ggplot(data2,aes(y=COUNT,x=MARITAL_STATUS,fill=Defaulter))+geom_bar(stat="identity",width=0.4)+labs(title="MARITAL STATUS WISE DEFAULTER PERCENT",size=9)+geom_text(aes(label=c(pmarrieddf,pmarriedndf,punmarrieddf,punmarriedndf,NA,NA)),size=5,position=position_stack(vjust=0.5))
p+theme(plot.title = element_text(face="bold",hjust=0.5))
#Married peoples are more likely to pay their default payments compared to singles and others 

#EDUCATION WISE DEFAULTER AND NON-DEFAULTER VALUES
gradschool<-subset(cdata,EDUCATION=="1",select=c(EDUCATION,Defaulter))
cgradschdf<-nrow(subset(gradschool,Defaulter=="1",select=c(Defaulter)))
cgradschndf<-nrow(subset(gradschool,Defaulter=="0",select=c(Defaulter)))
university<-subset(cdata,EDUCATION=="2",select=c(EDUCATION,Defaulter))
cunivdf<-nrow(subset(university,Defaulter=="1",select=c(Defaulter)))
cunivndf<-nrow(subset(university,Defaulter=="0",select=c(Defaulter)))
highschool<-subset(cdata,EDUCATION=="3",select=c(EDUCATION,Defaulter))
chighschdf<-nrow(subset(highschool,Defaulter=="1",select=c(Defaulter)))
chighschndf<-nrow(subset(highschool,Defaulter=="0",select=c(Defaulter)))
others<-subset(cdata,EDUCATION=="4",select=c(EDUCATION,Defaulter))
cothersdf<-nrow(subset(others,Defaulter=="1",select=c(Defaulter)))
cothersndf<-nrow(subset(others,Defaulter=="0",select=c(Defaulter)))
(pgradschdf<-paste(round(cgradschdf/sum(cgradschdf+cgradschndf)*100),"%",sep=""))
(pgradschndf<-paste(round(cgradschndf/sum(cgradschdf+cgradschndf)*100),"%",sep=""))
(punivdf<-paste(round(cunivdf/sum(cunivdf+cunivndf)*100),"%",sep=""))
(punivndf<-paste(round(cunivndf/sum(cunivdf+cunivndf)*100),"%",sep=""))
(phighschdf<-paste(round(chighschdf/sum(chighschdf+chighschndf)*100),"%",sep=""))
(phighschndf<-paste(round(chighschndf/sum(chighschdf+chighschndf)*100),"%",sep=""))
(pothersdf<-paste(round(cothersdf/sum(cothersdf+cothersndf)*100),"%",sep=""))
(pothersndf<-paste(round(cothersndf/sum(cothersdf+cothersndf)*100),"%",sep=""))
data3<-data.frame(EDUCATION=c("UNIVERSITY","UNIVERSITY","GRADUATE SCHOOL","GRADUATE SCHOOL","HIGH SCHOOL","HIGH SCHOOL","OTHERS","OTHERS"),Defaulter=c("Yes","No"),COUNT=c(cunivdf,cunivndf,cgradschdf,cgradschndf,chighschdf,chighschndf,cothersdf,cothersndf))
data3
p<-ggplot(data3,aes(y=COUNT,x=EDUCATION,fill=Defaulter))+geom_bar(stat="identity",width=0.6)+labs(title="EDUCATION WISE DEFAULTER PERCENT",size=9)+geom_text(aes(label=c(punivdf,punivndf,pgradschdf,pgradschndf,phighschdf,phighschndf,NA,NA)),size=4,position=position_stack(vjust=0.5))
p+theme(plot.title = element_text(face="bold",hjust=0.5))
#Compared to graduate school,high school and university clients are more likely to be defaulters next mnth

dev.off()
#Age in relation with defaulters and non defaulters
hist(cdata$AGE,col="skyblue",border="skyblue",freq = 2,main="Distribution of client's age",breaks=10,xlab="Age of the client",ylab="Frequency")
#as per the observation,more no of clients lies btw 25 to 35 years of age
ggplot(cdata,aes(x=AGE,fill=Defaulter))+geom_bar(width=0.8)+labs(x='AGE')


#Limit balance with defaulters count
dflim<-subset(cdata,Defaulter=="1",select=c(LIMIT_BAL))
limdf<-as.data.frame(table(dflim))
hist(cdata$LIMIT_BAL,freq=2,main = "Frequency of credit limit",border="grey",xlab="credit limit(in dollars)")
#ten thousand to ten lakh limit
barplot(limdf$Freq,names.arg=limdf$LIMIT_BAL,col="grey",ylim=c(0,900),xlab="Limit Balance(in dollars)",ylab="Defaulters",main="Frequency of defaulters in the limit balance")
#clients having ten thousand to 1 lakh eleven thousand limit balance has more no of defaulters 


#plotting bill statement for gender
b1<- ggplot(cdata, aes(x=factor(SEX), y=BILL_AMT1)) + stat_summary(fun="mean", geom="bar", width=0.5, fill="brown")+stat_summary(aes(label=round(after_stat(y),2)),fun=mean,geom="text",colour="white",fontface="bold",vjust=1.1,size=4.5)+labs(y ="Mean Bill Amount_SEP (in $)", x = "Gender (1=male, 2=female)")
b2<- ggplot(cdata, aes(x=factor(SEX), y=BILL_AMT2)) + stat_summary(fun="mean", geom="bar", width=0.5, fill="brown")+stat_summary(aes(label=round(after_stat(y),2)),fun=mean,geom="text",colour="white",fontface="bold",vjust=1.1,size=4.5)+labs(y ="Mean Bill Amount_AUG (in $)", x = "Gender (1=male, 2=female)")
b3<- ggplot(cdata, aes(x=factor(SEX), y=BILL_AMT3)) + stat_summary(fun="mean", geom="bar", width=0.5, fill="brown")+stat_summary(aes(label=round(after_stat(y),2)),fun=mean,geom="text",colour="white",fontface="bold",vjust=1.1,size=4.5)+labs(y ="Mean Bill Amount_JULY (in $)", x = "Gender (1=male, 2=female)")
b4<- ggplot(cdata, aes(x=factor(SEX), y=BILL_AMT4)) + stat_summary(fun="mean", geom="bar", width=0.5, fill="brown")+stat_summary(aes(label=round(after_stat(y),2)),fun=mean,geom="text",colour="white",fontface="bold",vjust=1.1,size=4.5)+labs(y ="Mean Bill Amount_JUNE (in $)", x = "Gender (1=male, 2=female)")
b5<- ggplot(cdata, aes(x=factor(SEX), y=BILL_AMT5)) + stat_summary(fun="mean", geom="bar", width=0.5, fill="brown")+stat_summary(aes(label=round(after_stat(y),2)),fun=mean,geom="text",colour="white",fontface="bold",vjust=1.1,size=4.5)+labs(y ="Mean Bill Amount_MAY (in $)", x = "Gender (1=male, 2=female)")
b6<- ggplot(cdata, aes(x=factor(SEX), y=BILL_AMT6)) + stat_summary(fun="mean", geom="bar", width=0.5, fill="brown")+stat_summary(aes(label=round(after_stat(y),2)),fun=mean,geom="text",colour="white",fontface="bold",vjust=1.1,size=4.5)+labs(y ="Mean Bill Amount_APRIL (in $)", x = "Gender (1=male, 2=female)")
grid.arrange(b1, b2,b3,b4,b5,b6,top=textGrob("Usage of Bill Amount by Gender Wise"))
## It seems Average Male spends more than Female.



#Repayment status 
#pay_0
sepclients<-select(cdata,Defaulter,PAY_0)
sepclients$PAY_0<-as.factor(sepclients$PAY_0)
sepclients<-data.frame(table(sepclients))
sepclients
p1<-ggplot(sepclients,aes(x=PAY_0,y=Freq,fill=Defaulter))+geom_bar(stat = "identity",position ="dodge")+labs(y ="COUNT", x = "SEPT_PAY")
#pay_2
augclients<-select(cdata,Defaulter,PAY_2)
augclients$PAY_2<-as.factor(augclients$PAY_2)
augclients<-data.frame(table(augclients))
augclients
p2<-ggplot(augclients,aes(x=PAY_2,y=Freq,fill=Defaulter))+geom_bar(stat = "identity",position ="dodge")+labs(y ="COUNT", x = "AUG_PAY")
#pay_3
julclients<-select(cdata,Defaulter,PAY_3)
julclients$PAY_3<-as.factor(julclients$PAY_3)
julclients<-data.frame(table(julclients))
julclients
p3<-ggplot(julclients,aes(x=PAY_3,y=Freq,fill=Defaulter))+geom_bar(stat = "identity",position ="dodge")+labs(y ="COUNT", x = "JULY_PAY")
#pay_4
junclients<-select(cdata,Defaulter,PAY_4)
junclients$PAY_4<-as.factor(junclients$PAY_4)
junclients<-data.frame(table(junclients))
junclients
p4<-ggplot(junclients,aes(x=PAY_4,y=Freq,fill=Defaulter))+geom_bar(stat = "identity",position ="dodge")+labs(y ="COUNT", x = "JUNE_PAY")
#pay_5
mayclients<-select(cdata,Defaulter,PAY_5)
mayclients$PAY_5<-as.factor(mayclients$PAY_5)
mayclients<-data.frame(table(mayclients))
mayclients
p5<-ggplot(mayclients,aes(x=PAY_5,y=Freq,fill=Defaulter))+geom_bar(stat = "identity",position ="dodge")+labs(y ="COUNT", x = "MAY_PAY")
#pay_6
aprclients<-select(cdata,Defaulter,PAY_6)
aprclients$PAY_6<-as.factor(aprclients$PAY_6)
aprclients<-data.frame(table(aprclients))
aprclients
p6<-ggplot(aprclients,aes(x=PAY_6,y=Freq,fill=Defaulter))+geom_bar(stat = "identity",position ="dodge")+labs(y ="COUNT", x = "APRIL_PAY")
grid.arrange(p1,p2,p3,p4,p5,p6,top=textGrob("Payment status of Defaulters and Non-Defaulters"))
#compared to april , may remaining months has higher defaulters after 2 months of payment

#Correlation for past payment amount
col<-colorRampPalette(c("#BB4444","#EE9988","#FFFFFF","#77AADD","#4477AA"))
res <-rcorr(as.matrix(cdata))
(res$r)
a<-cor(res$r,method="kendall")
corrplot(a,method="color",col=col(200),order = "hclust")




#MODEL BUILDING
ind=sample.split(Y=cdata$Defaulter,SplitRatio=0.7)
train<-cdata[ind,]
test<-cdata[!ind,]
dim(train)
dim(test)

#LOGISTIC REGRESSION
lr<-glm(Defaulter~.,data=train,family = "binomial")
summary(lr)
predicted<-predict(lr,train,type="response")
fitted.results<-ifelse(predicted>0.5,1,0)
misClasificError<-mean(fitted.results!=train$Defaulter)
misClasificError
print(paste('Training Accuracy',1-misClasificError))
confusionMatrix(data=as.factor(fitted.results),reference=as.factor(train$Defaulter))

predicted2<-predict(lr,test,type="response")
fitted.results2<-ifelse(predicted2>0.5,1,0)
misClasificError2<-mean(fitted.results2!=test$Defaulter)
misClasificError2
print(paste('Testing Accuracy',1-misClasificError2))
confusionMatrix(data=as.factor(fitted.results2),reference=as.factor(test$Defaulter))



#Decision tree
#Loading rpart library for decision tree
library(rpart.plot)
# Fit a decision tree model with all predictor variable
dtree<-rpart(Defaulter~.,data=train,method="class",cp=.01)
rpart.plot(dtree)
# Make predictions on train data 
predict_train<-predict(dtree,train,type = "class")
#Confusion matrix for train data
confusionMatrix(train$Defaulter,predict_train)
# Make predictions on test data
predict_test<-predict(dtree,test,type = "class")
#Confusion matrix for test data
confusionMatrix(test$Defaulter,predict_test)





#Random Forest
#Loading randomForest library for Random Forest
# Fit a Random Forest model with all predictor variable
rf1<-randomForest(Defaulter~.,data=train,mtry=2)
# Make predictions on train data
rf_train_pred<-predict(rf1,train,type="class")
#Confusion matrix for train data
confusionMatrix(as.factor(rf_train_pred),as.factor(train$Defaulter))
# Make predictions on test data
rf_test_pred<-predict(rf1,test,type="class")
#Confusion matrix for test data
confusionMatrix(as.factor(rf_test_pred),as.factor(test$Defaulter))






#Support Vector Machine
#Loading e1071 library for Support Vector Machine
# Fit a SVM model with all predictor variable
svm1<-svm(Defaulter~.,data=train,type='C-classification',kernel='linear')
# Make predictions on train data
svm_train_pred = predict(svm1, train,type="decision")
#Confusion matrix for train data
confusionMatrix(as.factor(svm_train_pred),as.factor(train$Defaulter))
# Make predictions on test data
svm_test_pred = predict(svm1,test,type="decision")
#Confusion matrix for test data
confusionMatrix(as.factor(svm_test_pred),as.factor(test$Defaulter))


#ML Metrics for LR
f1_score_logreg<-F1_Score(test$Defaulter,fitted.results2)
f1_score_logreg
Precision_logreg<-Precision(test$Defaulter,fitted.results2)
Precision_logreg
recall_logreg<-Recall(test$Defaulter,fitted.results2)
recall_logreg

#MLmetrics for decision tree
f1_score_dtree<-F1_Score(test$Defaulter,predict_test)
f1_score_dtree
Precision_dtree<-Precision(test$Defaulter,predict_test)
Precision_dtree
recall_dtree<-Recall(test$Defaulter,predict_test)
recall_dtree

 
#MLmetrics for RF
f1_score_randf<-F1_Score(test$Defaulter,rf_test_pred)
f1_score_randf
Precision_randf<-Precision(test$Defaulter,rf_test_pred)
Precision_randf
recall_randf<-Recall(test$Defaulter,rf_test_pred)
recall_randf

#MLmetrics for SVM
f1_score_svm<-F1_Score(test$Defaulter,svm_test_pred)
f1_score_svm
Precision_svm<-Precision(test$Defaulter,svm_test_pred)
Precision_svm
recall_svm<-Recall(test$Defaulter,svm_test_pred)
recall_svm




