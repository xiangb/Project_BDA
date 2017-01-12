##############################################
####### DATA LOADING AND PROCESSING ##########
##############################################

# Read data 
dataset1 <- read.csv('C:/Users/rauna/OneDrive/Documents/GitHub/Project_BDA/data/adult.data',sep=',',header = F)
dataset2 <- read.csv('C:/Users/rauna/OneDrive/Documents/GitHub/Project_BDA/data/adult.test',sep=',',header = F)
dataset1
#concat dataset1 and dataset2 without the first line (not used) of dataset2
dataset <- rbind(dataset1,dataset2[-1,])
#remove dataset1 and dataset2
rm(dataset1,dataset2)
#columns names 
colnames(dataset) <- c('age','workclass','fnlwgt','education','educationnum','mstatus','occupation','relationship','race','sex','capitalgain','capitalloss','hoursperweek','nativecountry','target')

# remove columns fnlwgt and educationnum

dataset$fnlwgt <- NULL
dataset$educationnum <- NULL

# transform target value to 0 if <=50K 1 if >50K
dataset$target <- as.character(dataset$target)
dataset[(dataset$target==' <=50K'|dataset$target==' <=50K.'),'target'] = 0
dataset[(dataset$target==' >50K'|dataset$target==' >50K.'),'target'] = 1 

# format target 
dataset$target <- as.factor(dataset$target)

#remove every row where there is a missing value, represented by '?' in the dataset
dataset = dataset[(dataset$workclass!=' ?'&dataset$occupation!=' ?'&dataset$nativecountry!=' ?'),]

# group nativecountry into set of countries :
dataset$nativecountry = as.character(dataset$nativecountry)
dataset$nativecountry[dataset$nativecountry==" Cambodia"] = "SEAsia"
dataset$nativecountry[dataset$nativecountry==" Canada"] = "Canada"    
dataset$nativecountry[dataset$nativecountry==" China"] = "China"       
dataset$nativecountry[dataset$nativecountry==" Columbia"] = "LatAmerica"    
dataset$nativecountry[dataset$nativecountry==" Cuba"] = "LatAmerica"        
dataset$nativecountry[dataset$nativecountry==" Dominican-Republic"] = "LatAmerica"
dataset$nativecountry[dataset$nativecountry==" Ecuador"] = "LatAmerica"     
dataset$nativecountry[dataset$nativecountry==" El-Salvador"] = "LatAmerica" 
dataset$nativecountry[dataset$nativecountry==" England"] = "WEurope"
dataset$nativecountry[dataset$nativecountry==" France"] = "WEurope"
dataset$nativecountry[dataset$nativecountry==" Germany"] = "WEurope"
dataset$nativecountry[dataset$nativecountry==" Greece"] = "EEurope"
dataset$nativecountry[dataset$nativecountry==" Guatemala"] = "LatAmerica"
dataset$nativecountry[dataset$nativecountry==" Haiti"] = "LatAmerica"
dataset$nativecountry[dataset$nativecountry==" Holand-Netherlands"] = "WEurope"
dataset$nativecountry[dataset$nativecountry==" Honduras"] = "LatAmerica"
dataset$nativecountry[dataset$nativecountry==" Hong"] = "China"
dataset$nativecountry[dataset$nativecountry==" Hungary"] = "EEurope"
dataset$nativecountry[dataset$nativecountry==" India"] = "India"
dataset$nativecountry[dataset$nativecountry==" Iran"] = "Iran"
dataset$nativecountry[dataset$nativecountry==" Ireland"] = "WEurope"
dataset$nativecountry[dataset$nativecountry==" Italy"] = "WEurope"
dataset$nativecountry[dataset$nativecountry==" Jamaica"] = "LatAmerica"
dataset$nativecountry[dataset$nativecountry==" Japan"] = "Japan"
dataset$nativecountry[dataset$nativecountry==" Laos"] = "SEAsia"
dataset$nativecountry[dataset$nativecountry==" Mexico"] = "Mexico"
dataset$nativecountry[dataset$nativecountry==" Nicaragua"] = "LatAmerica"
dataset$nativecountry[dataset$nativecountry==" Outlying-US(Guam-USVI-etc)"] = "LatAmerica"
dataset$nativecountry[dataset$nativecountry==" Peru"] = "LatAmerica"
dataset$nativecountry[dataset$nativecountry==" Philippines"] = "SEAsia"
dataset$nativecountry[dataset$nativecountry==" Poland"] = "EEurope"
dataset$nativecountry[dataset$nativecountry==" Portugal"] = "WEurope"
dataset$nativecountry[dataset$nativecountry==" Puerto-Rico"] = "LatAmerica"
dataset$nativecountry[dataset$nativecountry==" Scotland"] = "WEurope"
dataset$nativecountry[dataset$nativecountry==" South"] = "EEurope"
dataset$nativecountry[dataset$nativecountry==" Taiwan"] = "China"
dataset$nativecountry[dataset$nativecountry==" Thailand"] = "SEAsia"
dataset$nativecountry[dataset$nativecountry==" Trinadad&Tobago"] = "LatAmerica"
dataset$nativecountry[dataset$nativecountry==" United-States"] = "US"
dataset$nativecountry[dataset$nativecountry==" Vietnam"] = "SEAsia"
dataset$nativecountry[dataset$nativecountry==" Yugoslavia"] = "EEurope"

# format data
dataset$age <- as.numeric(dataset$age)
dataset$workclass <- as.factor(as.character(dataset$workclass)) # to avoid ? in levels
dataset$education <- as.factor(dataset$education)
dataset$mstatus <- as.factor(dataset$mstatus)
dataset$occupation <- as.factor(as.character(dataset$occupation)) # to avoid ? in levels
dataset$relationship <- as.factor(dataset$relationship)
dataset$race <- as.factor(dataset$race)
dataset$sex <- as.factor(dataset$sex)
dataset$nativecountry <- as.factor(dataset$nativecountry)

# scale capitalgain capitalloss and hoursperweek

dataset$capitalgain <- (dataset$capitalgain - mean(dataset$capitalgain))/sd(dataset$capitalgain)
dataset$capitalloss <- (dataset$capitalloss - mean(dataset$capitalloss))/sd(dataset$capitalloss)
dataset$hoursperweek <- (dataset$hoursperweek - mean(dataset$hoursperweek))/sd(dataset$hoursperweek)



##############################################
#######       MODELS EVALUATION     ##########
##############################################

library(randomForest)
library(caTools)

############################
### Logistic regression ####
############################

# repeat 30 times sampling process and save accuracy 
log_accuracy = c()
for (i in c(1:30)){
  
spl <- sample.split(dataset$target, SplitRatio = 0.7)
train <- subset(dataset, spl == TRUE)
test <- subset(dataset, spl == FALSE)

logReg <- glm(target~ ., data = train, family = binomial)

logReg_predict <- predict(logReg, newdata = test, type = "response")
pred = ifelse(logReg_predict>0.5,1,0)
conf_mat <- table(test$target, pred)
# save accuracy
log_accuracy <- c(log_accuracy,(conf_mat[1,1]+conf_mat[2,2])/sum(conf_mat)) 

}

###############################
### END Logistic regression####
###############################



#using CART

library(rpart)
library(rpart.plot)
CARTmodel <- rpart(target ~ ., data = train, method = "class")
prp(CARTmodel)

CARTpredict <- predict(CARTmodel, newdata = test, type = "class")
table(test$target, CARTpredict)

ROCRpred <- prediction(as.numeric(CARTpredict),test$target)
cartacuracy<-confusionMatrix(CARTpredict,test$target)$overall[1]
cartacuracy
as.numeric(performance(ROCRpred, "auc")@y.values)
perf <- performance(ROCRpred, "tpr", "fpr")
plot(perf)

#using randomforest

library(randomForest)

trainSmall <- train[sample(nrow(train), 2000),]
set.seed(1)
RFmodel <- randomForest(target ~., data = trainSmall)

RFmodel_predict <- predict(RFmodel, newdata = test)
table(test$target, RFmodel_predict)
rfacuracy<-confusionMatrix(RFmodel_predict,test$target)$overall[1]
rfacuracy


#using caret
library(caret)

library(e1071)
numFolds <- trainControl(method = "cv", number = 10 )
cartGrid <- expand.grid(.cp = seq(0.002, 0.1, 0.002))
train(target ~., data = train, method = "rpart",trControl = numFolds, tuneGrid = cartGrid)

CARTmodel_final <- rpart(target ~ ., data = train, method = "class",cp=0.002)
prediction <- predict(CARTmodel_final, newdata = test, type = "class")
table(test$target, prediction)

ROCRpred <- prediction(as.numeric(prediction),test$target)
cacuracy<-confusionMatrix(prediction,test$target)$overall[1]
cacuracy

as.numeric(performance(ROCRpred, "auc")@y.values)
perf <- performance(ROCRpred, "tpr", "fpr")
plot(perf)
prp(CARTmodel_final)

## Decision Tree

treeFit<- rpart(target~.,data=train,method = 'class')
print(treeFit)
rpart.plot(treeFit, box.col=c("yellow", "green"))
dtprediction<- predict(treeFit,newdata=test[-13],type = 'class')
dtacu<-confusionMatrix(dtprediction,test$target)$overall[1]
dtacu



## Performace Comparison


Accuracy<-data.frame(Model=c('Decision Tree','Caret','Logistic Regression','Random Forest','CART'),Accuracy=c(dtacu,cacuracy,lracuracy,rfacuracy,cartacuracy))

gg<-ggplot(Accuracy,aes(x=Model,y=Accuracy,fill=Model))+geom_bar(stat = 'identity')+theme_bw()+ggtitle('Accuracies of Models')
gg







