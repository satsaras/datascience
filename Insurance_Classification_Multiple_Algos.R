df_insur<-read.csv('E:/kaggle_1/insurance.csv',stringsAsFactors = F)
shh<-suppressMessages
shh(library(tidyverse))
shh(library(caret))
shh(library(Amelia))   
shh(library(GGally))
df_insur<-as.tibble(df_insur)
str(df_insur)
install.packages('xgboost')
levels(df_insur$sex)<-c('F','M')
df_insur %>% 
  select_if(is.numeric) %>% 
  map_dbl(~max(.x))
df_insur<-df_insur %>% 
  mutate(Agegroup=as.factor(findInterval(age,c(18,35,50,80))))
levels(df_insur$Agegroup)<-c("Youth","Mid Aged","Old")
levels(df_insur$smoker)<-c("N","Y")
levels(df_insur$region)<-c("NE","NW","SE","SW")
df_insur<-df_insur%>%filter(charges>=30000)
train<-createDataPartition(df_insur$Agegroup,p=0.8,list=F)
validate<-df_insur[-train,]
train<-df_insur[train,]
control<-trainControl(method = 'cv',number=10)
metric<-'RMSE'
set.seed(233)
fit.knn<-train(charges~.,data=train,method="knn",trControl=control,metric=metric) 
set.seed(233)
fit.svm<-train(charges~.,data=train,method="svmRadial",trControl=control,metric=metric) 
set.seed(233)
fit.gbm<-train(charges~.,data=train,method="gbm",trControl=control,metric=metric,
               verbose=F) 
set.seed(233)
fit.xgb<-train(charges~.,data=train,method="xgbTree",trControl=control,metric=metric,
               verbose=F) 
set.seed(233) 
fit.rf<-train(charges~.,data=train,method="xgbTree",trControl=control,metric=metric,
              verbose=F) 
dotplot(results,main="Model Training Results")
results<-resamples(list(knn=fit.knn,svm=fit.svm,xgb=fit.xgb,gbm=fit.gbm,rf=fit.rf))
getTrainPerf(fit.gbm)
getTrainPerf(fit.xgb)
plot(varImp(fit.rf),main="Model Feature Importance-Random Forest")




