#Simple Linear Regression 
#Neel Patel 

# - Assumption of a Linear Regression -
# 1. Linearity 
# 2. Homoscedasticity 
# 3. Multivariate normality
# 4. Independence of errors
# 5. Lack of Multicollinearity
# - proceed if these assumption are true -


# - 1 - Importing Dataset -

dataset = read.csv("Salary_Data.csv")
#dataset = dataset[,2:3]

# View Dataset 

View(dataset)


#install package - caTools -

#install.packages('caTools')
library(caTools)

#Splitting the dataset into -  Training Set and Test Set - 

set.seed(123)
split = sample.split(dataset$Salary,SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


#  - Feature Scaling -


# training_set[,2:3] = scale(training_set[,2:3])
# test_set[,2:3] = scale(test_set[,2:3])

#  - Fitting Simple Linear Regression to the Training set -
regressor = lm(formula = Salary ~ YearsExperience,
               data = training_set)

# - 5%(0.05) P value is good indicator of highly significant -


# - Predicting the Test set results - 

y_pred = predict(regressor, newdata = test_set)


# - install.packages('ggplot2') -
library(ggplot2)


# - Visualizing the Training set results - 
ggplot() + 
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             color ='red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            color = 'blue') +
  ggtitle('Salary vs Experience (Training set)') + 
  xlab('Years of Experience') +
  ylab('Salary')
#ggsave('Salary vs Experience(Training Set).png')


# - Visualizing the Test set results - 
ggplot() + 
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             color ='red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            color = 'blue') +
  ggtitle('Salary vs Experience (Test set)') + 
  xlab('Years of Experience') +
  ylab('Salary')
#ggsave('Salary vs Experience(Test Set).png')
