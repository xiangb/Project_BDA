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

