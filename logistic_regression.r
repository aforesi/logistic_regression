library(MASS)
library(corrplot)
library(car)

data("biopsy")
str(biopsy)
summary(biopsy.v2)

#get rid of the biopsy id
biopsy$ID = NULL

#rename the variables
names(biopsy) = c("thick", "u.size", "u.shape", "adhsn", "s.size", "nucl", "chrom", "n.nuc", "mit", "class")

#remove missing data, there are only 16 missing values representing only 2% of the data set
biopsy.v2 = na.omit(biopsy)

#create a correlation matrix
correlationmatrix = cor(biopsy.v2[, 1:9])
corrplot.mixed(correlationmatrix)


#split the data for train and test

set.seed(123) #random number generator
ind = sample(2, nrow(biopsy.v2), replace=TRUE, prob=c(0.7, 0.3))
train = biopsy.v2[ind==1,] #the training data set
test = biopsy.v2[ind==2,] #the test data set
str(test) #confirm it worked

#we are now going to use the glm function to fit the data to a linear model

full.fit = glm(class~., family=binomial, data=train)
summary(full.fit)

#log odds

exp(coef(full.fit))

#check for collinearity amongst the variables using the VIF statistic. Values
#should be less than 5
vif(full.fit)

#create a confusion matrix for our tain data

train$probs = predict(full.fit, type="response")
train$probs[1:5]
contrasts(train$class)

#define that a probability greater than 0.5 should result in malignant

train$predict = rep("benign", 474)
train$predict[train$probs>0.5]="malignant"
table(train$predict, train$class)

#mean error in our train prediction
mean(train$predict==train$class)

#create a confusion matrix for our test data

test$prob = predict(full.fit, newdata=test, type="response")

#create predictions for the test data

test$predict = rep("benign", 209)
test$predict[test$prob>0.5]="malignant"
table(test$predict, test$class)

#mean error in our test prediction
mean(test$predict==test$class)


