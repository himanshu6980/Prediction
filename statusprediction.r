data=read.csv(file = "TRAIN_DATA.csv",header = TRUE,sep = ",")
 
 library(caTools)
# splitting data set into train and testing
split = sample.split(data, SplitRatio = 0.8)
training = subset(data, split == TRUE)
testing= subset(data, split == FALSE)
# Logistic regression model
model=glm(RESULT~.,training,family = "binomial")
 
# predcition for test data
pred1=predict(model,testing,type="response")
 print(pred1)
 # confusion matrix
pred1=ifelse(pred1>0.5,1,0)
tab1=table(Predicted = pred1,Actual=testing$RESULT)
print(tab1)
 accuracy = 1-sum(diag(tab1))/sum(tab1)
 print(accuracy)
 
 # ROCR curve for model
 library(ROCR)
 library(gplots)
 pred=prediction(pred1,testing$RESULT)
 pref=performance(pred,"tpr","fpr")
 plot(pref)

