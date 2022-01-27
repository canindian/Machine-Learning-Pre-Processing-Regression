# Data Pre-processing Steps 
# Neel Patel 

# - 1 - Importing Dataset 

dataset = read.csv("Data.csv")
#dataset = dataset[,2:3]

# View Dataset 
View(dataset)


# - 2 - Taking care of missing Data

#Taking care of Age in Data.CSV file 
dataset$Age = ifelse(is.na(dataset$Age),
                     ave(dataset$Age, FUN = function(x) mean(x,na.rm=TRUE)),
                     dataset$Age) 

#Taking care of Salary in Data.CSV file 
dataset$Salary = ifelse(is.na(dataset$Salary),
                     ave(dataset$Salary, FUN = function(x) mean(x,na.rm=TRUE)),
                     dataset$Salary) 


# - 3 - ENCODING CATEGORICAL DATA 

#Switching Country names(France, Spain, Germany) into numeric labels(1,2,3) in the Data.CSV 
dataset$Country = factor(dataset$Country,
                         levels = c('France', 'Spain', 'Germany'),
                         labels = c(1,2,3))
                         
#Switching Purchase (Yes, No)  into numeric labels(1,2) in the Data.CSV 
dataset$Purchased = factor(dataset$Purchased,
                         levels = c('No', 'Yes'),
                         labels = c(0,1))


# - 4 - Splitting the dataset into Training Set and Test Set 

#install package - caTools
install.packages('caTools')
library(caTools)

#Splitting the dataset into Training Set and Test Set 
set.seed(123)
split = sample.split(dataset$Purchased,SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# - 5 - Feature Scaling 

# training_set[,2:3] = scale(training_set[,2:3])
# test_set[,2:3] = scale(test_set[,2:3])