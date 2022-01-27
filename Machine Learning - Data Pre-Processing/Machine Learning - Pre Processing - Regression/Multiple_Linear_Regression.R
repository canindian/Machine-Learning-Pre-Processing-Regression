# Multiple Linear Regression 
# Neel Patel 



#  - Importing Data set -

dataset = read.csv("Data/50_Startups.csv")
#dataset = dataset[,2:3]

# View Dataset 
View(dataset)



#  - ENCODING CATEGORICAL DATA -

#Switching State names(New York, California, Florida) into numeric labels(1,2,3) in the Data.CSV 
dataset$State = factor(dataset$State,
                         levels = c('New York', 'California', 'Florida'),
                         labels = c(1,2,3))

View(dataset)



#install.packages('caTools')
library(caTools)



# - Splitting the dataset into -  Training Set and Test Set - 
set.seed(123)
split = sample.split(dataset$Profit,SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)



# - Fitting Multiple Linear Regression to the Training Set -
regressor = lm(formula = Profit ~ ., 
               data = training_set)
summary(regressor)



# - Predicting the Test set Results -
y_pred = predict(regressor,newdata = test_set)



# - Building the optimal model using Backward Elimination -

# Building the optimal model using Backward Elimination 
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State, 
               data = dataset)

# Observe for highest P value and statistical significance of the summary (*** - indicates significance)
summary(regressor)

# remove state (remove the predictor - state)
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend, 
               data = dataset)

# Observe for highest P value and statistical significance of the summary (p(P-value) > SL (significant level))
summary(regressor)

# remove Administration (remove the predictor - Administration)
regressor = lm(formula = Profit ~ R.D.Spend  + Marketing.Spend, 
               data = dataset)

# Observe for highest P value and statistical significance of the summary (p(P-value) > SL (significant level))
summary(regressor)

# remove Marketing.Spend (remove the predictor - Marketing Spend)
regressor = lm(formula = Profit ~ R.D.Spend , 
               data = dataset)

# Observe for highest P value and statistical significance of the summary (p(P-value) > SL (significant level))
summary(regressor)
# R.D.Spend is most statistical significant to Profit 


# if you are also interested in an automatic implementation of Backward Elimination in R, here it is:
# 
# backwardElimination <- function(x, sl) {
#   numVars = length(x)
#   for (i in c(1:numVars)){
#     regressor = lm(formula = Profit ~ ., data = x)
#     maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
#     if (maxVar > sl){
#       j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
#       x = x[, -j]
#     }