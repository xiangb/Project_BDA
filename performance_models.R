##############################################
####### DATA LOADING AND PROCESSING ##########
##############################################

# Read data 
dataset1 <- read.csv('data/adult.data',sep=',',header = F)
dataset2 <- read.csv('data/adult.test',sep=',',header = F)

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
dataset$mstatus <- as.factor(as.character(dataset$mstatus))
dataset$occupation <- as.factor(as.character(dataset$occupation)) # to avoid ? in levels
dataset$relationship <- as.factor(as.character(dataset$relationship))
dataset$race <- as.factor(as.character(dataset$race))
dataset$sex <- as.factor(as.character(dataset$sex))
dataset$nativecountry <- as.factor(as.character(dataset$nativecountry))
dataset$education <- as.factor(as.character(dataset$education))

# scale capitalgain capitalloss and hoursperweek

dataset$capitalgain <- (dataset$capitalgain - mean(dataset$capitalgain))/sd(dataset$capitalgain)
dataset$capitalloss <- (dataset$capitalloss - mean(dataset$capitalloss))/sd(dataset$capitalloss)
dataset$hoursperweek <- (dataset$hoursperweek - mean(dataset$hoursperweek))/sd(dataset$hoursperweek)



####################################
######## Models Evaluation #########
####################################

library(randomForest)
library(ipred)
library(caTools)
library(class)
library(dummies)


#logistic

log_accuracy = c()
for (i in c(1:20)){
  
  spl <- sample.split(dataset$target, SplitRatio = 0.7)
  train <- subset(dataset, spl == TRUE)
  test <- subset(dataset, spl == FALSE)
  
  logReg <- glm(target~ ., data = train, family = binomial)
  
  logReg_predict <- predict(logReg, test, type = "response")
  pred = ifelse(logReg_predict>0.5,2,1)
  conf_mat <- table(test$target, pred)
  # save accuracy
  log_accuracy <- c(log_accuracy,(conf_mat[1,1]+conf_mat[2,2])/sum(conf_mat)) 
  
}
log_accuracy




#randomforest



rf_accuracy = c()
for (i in c(1:20)){
  
  spl <- sample.split(dataset$target, SplitRatio = 0.7)
  train <- subset(dataset, spl == TRUE)
  test <- subset(dataset, spl == FALSE)
  
  RFmodel <- randomForest(target~ ., data = train,ntree = 250)
  
  RFmodel_predict<- predict(RFmodel, newdata = test, type = "response")
  
  conf_mat <- table(test$target, RFmodel_predict)
  # save accuracy
  rf_accuracy <- c(rf_accuracy,(conf_mat[1,1]+conf_mat[2,2])/sum(conf_mat)) 
  
}
rf_accuracy


#Bagging
bag_accuracy = c()
for (i in c(1:20)){
  
  spl <- sample.split(dataset$target, SplitRatio = 0.7)
  train <- subset(dataset, spl == TRUE)
  test <- subset(dataset, spl == FALSE)
  
  bagmodel <- bagging(target~ ., data = train,ntree = 200)
  
  bagmodel_predict<- predict(bagmodel, newdata = test, type = "class")
  
  conf_mat <- table(test$target, bagmodel_predict)
  # save accuracy
  bag_accuracy <- c(bag_accuracy,(conf_mat[1,1]+conf_mat[2,2])/sum(conf_mat)) 
  
}
bag_accuracy

# Comparison of Logistic Regression, Random Forest and Bagging

boxplot(log_accuracy,rf_accuracy,bag_accuracy,main='Accuracy boxplot',names = c("LogReg","RF","Bagging"))




#KNN

#We transform categorical variables to dummy variables

dummies = c()
for (names in c('sex','race','education','workclass','mstatus','occupation','relationship','nativecountry'))
{
  dummies = cbind(dummies,dummy(dataset[,names],sep='_'))
  
}

dataincome <- data.frame(age=dataset$age,capitalgain=dataset$capitalgain,capitalloss=dataset$capitalloss,dataset$hoursperweek,dummies,target=dataset$target)
dataincome$target <- as.factor(dataincome$target)

# PCA function to reduce number of variables 
pca_reduc <- function(nbPC,dataincome)
{
covmat = cov(scale(dataincome[,-73]))
eigen = eigen(covmat)
varexp = cumsum(eigen$values)/sum(eigen$values)
#scree plot
barplot(varexp,names.arg = c(1:72),xlab='number of PC',ylab='Ratio of explained variance',main='Scree plot')
#projection on first nbPC principal components
proj = scale(dataincome[,-73])%*%eigen$vectors[,1:nbPC]
data1 = data.frame(cbind(proj,dataincome$target))
colnames(data1) = c(paste('PC',seq(1,nbPC),sep=''),'target')
data1[data1$target==1,'target']=0
data1[data1$target==2,'target']=1
data1$target <- as.factor(data1$target)
return(data1)
}

knn_func <- function(K,iter,data1)
{
knn_accuracy = c()
for (i in c(1:iter)){
  
  spl <- sample.split(data1$target, SplitRatio = 0.7)
  train <- subset(data1, spl == TRUE)
  test <- subset(data1, spl == FALSE)
  
  knnmodel <- knn(train=train,test=test,cl = train$target,k=K)
  conf_mat <- table(test$target,knnmodel)
  # save accuracy
  knn_accuracy <- c(knn_accuracy,(conf_mat[1,1]+conf_mat[2,2])/sum(conf_mat)) 
  
}
return(knn_accuracy)
}
# Screeplot for PCA
covmat = cov(scale(dataincome[,-73]))
eigen = eigen(covmat)
varexp = cumsum(eigen$values)/sum(eigen$values)
#scree plot
barplot(varexp,names.arg = c(1:72),xlab='number of PC',ylab='Ratio of explained variance',main='Scree plot')


# best number of PCA ?

pca_accuracy = c()
for (nbPC in c(5,10,15,20,30))
{
  pca_accuracy <- cbind(pca_accuracy,knn_func(3,5,pca_reduc(nbPC = nbPC,dataincome)))
  
}

boxplot(pca_accuracy,main='Accuracy boxplot for K = 3 and varying number of kept PC', names=c('PC=5','PC=10','PC=15','PC=20','PC=30'))


# We keep 5 PC and then look at performances depending on the number of nearest neighbours

data1 <- pca_reduc(5,dataincome)

knn_accuracy_3 <- knn_func(3,20,data1)
knn_accuracy_5 <- knn_func(5,20,data1)
knn_accuracy_7 <- knn_func(7,20,data1)
knn_accuracy_10 <- knn_func(10,20,data1)

boxplot(knn_accuracy_3,knn_accuracy_5,knn_accuracy_7,knn_accuracy_10,main='Accuracy boxplot for different number K of neighbours with 5 PC',names = c("K=3","K=5","K=7","K=10"))










