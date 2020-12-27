# UMGC DATA 630
# SUMMER 2020
# Assignment 4
# Neural network method on Breast Cancer dataset
# Written by Vanessa Fotso


install.packages("neuralnet")  # to be run only one time unless package already install
library("neuralnet")

#Read the column data file. Make sure the file is in your current library

data<-read.csv(file="wdbc.csv", head=TRUE, sep=",")

#Preview the first 6 rows
head(data)
# preview the structure
str(data)
#Run the summary command 
summary(data)

# dropping id column 
data <- data[, -c(1)]

#transform diagnosis to Binary digits
data[, 1] <- as.numeric(data[, 1]== "M")

summary(data)
prop.table(table(data$diagnosis))
#scale the inputs variable
data[2:31]<-scale(data[2:31])

#correlation

library(PerformanceAnalytics)
chart.Correlation(data[, c(2:11)], histogram = TRUE, col="grey10", pch=1, main="Breast Cancer Mean")

#make sure that the result is reproducible
set.seed(12345)
#split the data into a training and test set
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.75, 0.25))
train.data <- data[ind == 1, ]
test.data <- data[ind == 2, ]
#Build the model. If you receive a warning, rerun the command.
nn<-neuralnet(formula = diagnosis~., data = train.data, hidden=4, err.fct="ce", linear.output = FALSE)
#names command displays the available neural network properties
names(nn)

#Run the commands to display the network properties
nn$call                  # the command we ran to generate the model
nn$response[1:10]        # actual values of the dependent variable for first 10 records
nn$covariate [1:12,]     # input variables that were used to build the model for first 12 records
nn$model.list            # list dependent and independent variables in the model
nn$net.result[[1]][1:10] # display the first 10 predicted probabilities
nn$weights               # network weights after the last method iteration
nn$startweights          # weights on the first method iteration
nn$result.matrix         # number of trainings steps, the error, and the weights 
plot(nn)                 # plot the network
#Model evaluation; Round the predicted probabilities
mypredict<-compute(nn, nn$covariate)$net.result
mypredict<-apply(mypredict, c(1), round)
mypredict [1:10]
# confusion matrix for the training set
table(mypredict, train.data$diagnosis, dnn =c("Predicted", "Actual"))
mean(mypredict==train.data$diagnosis)

# confusion matrix for the test set
testPred <- compute(nn, test.data[, 0:31])$net.result
testPred<-apply(testPred, c(1), round)
table(testPred, test.data$diagnosis, dnn =c("Predicted", "Actual"))
mean(testPred==test.data$diagnosis)

# secon cross validation (repeating the fitting steps). 
# build the model using hidden = 2 in the neuralnet function and keeping all other parameter the same
nn<-neuralnet(formula = diagnosis~., data = train.data, hidden=2, err.fct="ce", linear.output = FALSE)

# plot the network
plot(nn)      

#Model evaluation; Round the predicted probabilities
mypredict<-compute(nn, nn$covariate)$net.result
mypredict<-apply(mypredict, c(1), round)
mypredict [1:10]
# confusion matrix for the training set
table(mypredict, train.data$diagnosis, dnn =c("Predicted", "Actual"))

# confusion matrix for the test set
testPred <- compute(nn, test.data[, 0:31])$net.result
testPred<-apply(testPred, c(1), round)
table(testPred, test.data$diagnosis, dnn =c("Predicted", "Actual"))
# End Script