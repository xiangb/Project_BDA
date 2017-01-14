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

# remove columns fnlwgt

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

#models

library(randomForest)
library(caTools)

#logistic

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
log_accuracy
a<-mean(log_accuracy)
a
warnings()


#using randomforest

set.seed(1)
library(randomForest)

rf_accuracy = c()
for (i in c(1:30)){
  
  spl <- sample.split(dataset$target, SplitRatio = 0.7)
  train <- subset(dataset, spl == TRUE)
  test <- subset(dataset, spl == FALSE)
  
  RFmodel <- randomForest(target~ ., data = train,ntree = 500, maxnodes = 10)
  
  RFmodel_predict<- predict(RFmodel, newdata = test, type = "response")
  
  conf_mat <- table(test$target, RFmodel_predict)
  # save accuracy
  rf_accuracy <- c(rf_accuracy,(conf_mat[1,1]+conf_mat[2,2])/sum(conf_mat)) 
  
}
rf_accuracy
b<-mean(rf_accuracy)
b

#using KNN
"the dataincome ks in numeric form and has been used for knn,lda and step function"
library(class)
colnames(dataset)

dataincome=dataset

str(dataincome)
dataincome$workclass=as.numeric(dataincome$workclass)
dataincome$education=as.numeric(dataincome$education)
dataincome$mstatus=as.numeric(dataincome$mstatus)
dataincome$occupation=as.numeric(dataincome$occupation)
dataincome$relationship=as.numeric(dataincome$relationship)
dataincome$race=as.numeric(dataincome$race)
dataincome$sex=as.numeric(dataincome$sex)
dataincome$nativecountry=as.numeric(dataincome$nativecountry)
dataincome$target=as.numeric(dataincome$target)


spl <- sample.split(dataincome$target, SplitRatio = 0.7)
train1<- subset(dataincome, spl == TRUE)

test1<-subset(dataincome, spl == FALSE)


knn_accuracy = c()
for (i in c(1:30)){
  
  spl <- sample.split(dataincome$target, SplitRatio = 0.7)
  train <- subset(dataincome, spl == TRUE)
  test <- subset(dataincome, spl == FALSE)
  
  knnmodel <- knn(train=train,test=test,cl=train[,13],k=10)
  
  #knn_predict <- predict(knnmodel, newdata = test)
 
  conf_mat <- table(test$target,knnmodel)
  # save accuracy
  knn_accuracy <- c(knn_accuracy,(conf_mat[1,1]+conf_mat[2,2])/sum(conf_mat)) 
  
}

knn_accuracy
c<-mean(knn_accuracy)
c
# k=10: 0.8816551 , k=15 : 0.8784633 ,k=20: 0.8749201 ,k=5: 0.8872893

#lda 

library(MASS)
library(caret)
library(ISLR)

tmp <- cor(dataincome)
tmp[upper.tri(tmp)] <- 0
diag(tmp) <- 0
dataincome1<-dataincome[,!apply(tmp,2,function(x) any(x > 0.99))]
library(class) 
lda_accuracy = c()
for (i in c(1:30)){
spl <- sample.split(dataincome1$target, SplitRatio = 0.7)
train <- subset(dataincome1, spl == TRUE)
test <- subset(dataincome1, spl == FALSE)

lda.fit <- lda(target~., data=train)
lda.fit
lda.pred <- predict(lda.fit, test)
names(lda.pred)

conf_mat <- table(lda.pred$class, test$target)
# save accuracy
lda_accuracy <- c(lda_accuracy,(conf_mat[1,1]+conf_mat[2,2])/sum(conf_mat)) 
}

lda_accuracy
d<-mean(lda_accuracy)
d
#lda_accuracy=0.7804683

#step function
datastep=dataincome
colnames(datastep)
library(leaps)
leaps=regsubsets(target~age+workclass+education+mstatus+occupation+relationship+race+sex+capitalgain+capitalloss+hoursperweek+nativecountry,
                   data=datastep, nbest=10,really.big=T)
plot(leaps, scale="adjr2")
plot(leaps, scale="bic")


null=lm(target~1, data=datastep)
null

full=lm(target~., data=datastep)
full

step(null, scope=list(lower=null, upper=full), direction="forward")

# gave: Call:
"lm(formula = target ~ relationship + capitalgain + age + hoursperweek + 
     capitalloss + mstatus + sex + education + occupation + race + 
     workclass, data = datastep)
just try running once more, as per step, nativecountry is useless."
#









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



## Performace Comparison


Accuracy<-data.frame(Model=c('Decision Tree','Caret','Logistic Regression','Random Forest','CART'),Accuracy=c(dtacu,cacuracy,lracuracy,rfacuracy,cartacuracy))

gg<-ggplot(Accuracy,aes(x=Model,y=Accuracy,fill=Model))+geom_bar(stat = 'identity')+theme_bw()+ggtitle('Accuracies of Models')
gg







