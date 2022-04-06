library(base)
library(utils)
library(stats)
library(rpart)
library(rpart.plot)
library(caTools)

#importing dataset from github
#https://raw.githubusercontent.com/JaganMurugesan/Analysis-of-Income-and-Expenditure/main/Income_Expenditure.csv
df<-read.csv("https://raw.githubusercontent.com/JaganMurugesan/Analysis-of-Income-and-Expenditure/main/Income_Expenditure.csv")
str(df)
summary(df)

#Removing 1st column and renaming columns
df<-df[,-1]
names(df)<-c("Income","Rent","Appliances_count","Vehicles_count","Vehicle_exp","Pets_exp","Electric_bill","Water_bill",
             "Gas_bill","Internet_exp","Medical_exp","Food_exp","Education_exp","Workers_exp","Miscellaneous_exp")
str(df)

#Analyzing all coulumns
for(x in colnames(df)){
  print(x)
  print(table(df[,x]))
}

#Function to analyse 2 columns in dataset
analyse<-function(A,B){
x<-table(A,B)
rs<-rowSums(x)
for(i in rownames(x)){
  x[i,]<-(x[i,]/rs[i])*100
}
print(x)
}
#analyse(df$Income,df$Rent)
for(j in colnames(df)){
  print(paste("Income vs ",j))
  analyse(df$Income,df[,j])
}

#Splitting train and test data
split<-sample.split(df[,1],SplitRatio=0.67)
train_data<-subset(df,split=="TRUE")
test_data<-subset(df,split=="FALSE")

#Decision tree model
dt<-rpart(Income~.,data=train_data,method="class",minsplit=1)
dt
rpart.plot(dt)

#Predicting test data
prediction=predict(dt,newdata=test_data,type="class")
prediction
#plot(test_data$Income,pred)
Observed_data<-test_data$Income
Predicted_data<-prediction
mean(Observed_data==Predicted_data)

#Confusion matrix
cm<-table(test_data$Income,prediction)
cm

#TP,FN,FP,TN for Multi classifier
multi_class_rates<-function(confusion_matrix){
  true_positives<-diag(confusion_matrix)
  false_positives<-colSums(confusion_matrix)-true_positives
  false_negatives<-rowSums(confusion_matrix)-true_positives
  true_negatives<-sum(confusion_matrix)-true_positives-false_positives-false_negatives
  return(data.frame(true_positives,false_positives,true_negatives,false_negatives,
                    row.names = names(true_positives)))
}
mcr<-multi_class_rates(cm)
mcr

#Precision
precision<-mcr$true_positives/(mcr$true_positives+mcr$false_positives)

#Recall
recall<-mcr$true_positives/(mcr$true_positives+mcr$false_negatives)

#F1 Measure
f1_measure<-(2*precision*recall)/(precision+recall)

#Accuracy
accuracy<-(mcr$true_positives+mcr$true_negatives)/(mcr$true_positives+mcr$false_positives+mcr$true_negatives+mcr$false_negatives)
accuracy

#Create a Data frame for the result
result<-data.frame(precision=precision,recall=recall,f1_measure=f1_measure,accuracy=accuracy,
                   row.names=row.names(mcr))
result
