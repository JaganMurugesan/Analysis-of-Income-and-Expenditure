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

#income_list creation
income_list<-data.frame(c("< 40k","40k to 80k","80k to 1.25L","1.25L to 2L","> 2L"),c(1,2,3,4,5))
names(income_list)<-c("income","value")

#Splitting train and test data
split<-sample.split(df[,1],SplitRatio=0.67)
train_data<-subset(df,split=="TRUE")
test_data<-subset(df,split=="FALSE")

temp<-data.frame(df[c("26","97","284","333","540"),])
test_data<-rbind(test_data,temp)

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

#finding wrongly classified data (predicted income is greater than original)
for(i in rownames(test_data)){
  x<-test_data[i,]$Income
  y<-prediction[i]
  if(income_list[income_list$income==y,]$value>income_list[income_list$income==x,]$value)
    print(i)
}

p1<-predict(dt,newdata=test_data[c("26","97","284","333","540"),],type="class")
p1
print(test_data[c("26","97","284","333","540"),]$Income)
#printing suggestions 
for(i in c("26","97","284","333","540")){
  x<-test_data[i,]$Income
  y<-prediction[i]
  test_val<-income_list[income_list$income==x,]$value
  pred_val<-income_list[income_list$income==y,]$value
  if(pred_val>test_val){
    print(i)
    if(test_val==1) # < 40k
      n="24"
    else if(test_val==2) # 40k to 80k
      n="294"
    else if(test_val==3) #80k to 1.25L
      n="87"
    else if(test_val==4) #1.25L to 2L
      n="225"
    else if(test_val==5) #> 2L
      n="127"
    print(paste("Income group: ",test_data[i,1]))
    for(j in colnames(test_data)){
      if(test_data[n,j]!=test_data[i,j]){
        print(j)
        print(paste("Original value: ",test_data[i,j]))
        print(paste("Required value: ",test_data[n,j]))
      }
    }
  }
}


## Regression model buliding
#Changing income column to integer values
new_df<-df
new_df$newIncome=0;
for(i in rownames(new_df)){
  if(new_df[i,]$Income=="< 40k"){
    new_df[i,]$newIncome=20000}
  else if(new_df[i,]$Income=="40k to 80k"){
    new_df[i,]$newIncome=60000}
  else if(new_df[i,]$Income=="80k to 1.25L"){
    new_df[i,]$newIncome=100000}
  else if(new_df[i,]$Income=="1.25L to 2L"){
    new_df[i,]$newIncome=160000}
  else if(new_df[i,]$Income=="> 2L"){
    new_df[i,]$newIncome=250000}
}

new_df<-new_df[,-1]

#Splitting train and test data
split<-sample.split(new_df[,1],SplitRatio=0.67)
train_data1<-subset(new_df,split=="TRUE")
test_data1<-subset(new_df,split=="FALSE")

#decision tree regression model
dtr<-rpart(newIncome~.,data=train_data1,method="anova",minsplit=5)
dtr
rpart.plot(dtr)

predict_income=predict(dtr,newdata=test_data1)
predict_income

#mean(test_data1$newIncome==predict_income)

obs_data<-test_data1$newIncome
pred_data<-predict_income
plot(obs_data,pred_data)

###
#Regression Model Evaluation

#Mean Absolute Error(MAE)
diff<-obs_data-pred_data
mae<-mean(abs(diff))
mae/100

#Mean Squared Error(MSE)
mse<-mean(diff^2)
mse/100

#Root Mean Square Error(RMSE)
rmse<-sqrt(mse)
rmse/100

#R-Squared (Coefficient of Determination)
R2<-1-( sum((diff)^2) / sum((obs_data-mean(obs_data))^2) )
R2

