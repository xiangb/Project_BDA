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
# format data
dataset$age <- as.numeric(dataset$age)
dataset$workclass <- as.factor(dataset$workclass)
dataset$education <- as.factor(dataset$education)
dataset$mstatus <- as.factor(dataset$mstatus)
dataset$occupation <- as.factor(dataset$occupation)
dataset$relationship <- as.factor(dataset$relationship)
dataset$race <- as.factor(dataset$race)
dataset$sex <- as.factor(dataset$sex)
dataset$nativecountry <- as.factor(dataset$nativecountry)

# transform target value to 0 if <=50K 1 if >50K
dataset$target <- as.character(dataset$target)
dataset[(dataset$target==' <=50K'|dataset$target==' <=50K.'),'target'] = 0
dataset[(dataset$target==' >50K'|dataset$target==' >50K.'),'target'] = 1 

# format target 
dataset$target <- as.factor(dataset$target)

#remove every row where there is a missing value, represented by '?' in the dataset
dataset = dataset[(dataset$workclass!=' ?'&dataset$occupation!=' ?'&dataset$nativecountry!=' ?'),]

