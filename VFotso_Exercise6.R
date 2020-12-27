# UMGC DATA 630
# SUMMER 2020
# EXERCISE 6
# Neural network method on Vertebral Column data
# Written by Vanessa Fotso


install.packages("neuralnet")  # to be run only one time unless package already install
library("neuralnet")

#Read the column data file. Make sure the file is in your current library

column<-read.csv(file="column.csv", head=TRUE, sep=",")

#Preview the first 6 rows
head(column)
# preview the structure
str(column)
#Run the summary command 
summary(column)

#scale the inputs variable
column[1:6]<-scale(column[1:6])

#make sure that the result is reproducible
set.seed(12345)
#split the data into a training and test set
ind <- sample(2, nrow(column), replace = TRUE, prob = c(0.7, 0.3))
train.data <- column[ind == 1, ]
test.data <- column[ind == 2, ]
#Build the model. If you receive a warning, rerun the command.
nn<-neuralnet(formula = class~pelvic_incidence + pelvic_tilt + lumbar_lordosis + sacral_slope + pelvic_radius + degree_spondylolisthesis, data = train.data, hidden=2, err.fct="ce", linear.output = FALSE)
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
table(mypredict, train.data$class, dnn =c("Predicted", "Actual"))
mean(mypredict==train.data$class)

# confusion matrix for the test set
testPred <- compute(nn, test.data[, 0:6])$net.result
testPred<-apply(testPred, c(1), round)
table(testPred, test.data$class, dnn =c("Predicted", "Actual"))
mean(testPred==test.data$class)

# secon cross validation (repeating the fitting steps). 
# build the model using hidden = 4 in the neuralnet function and keeping all other parameter the same
nn<-neuralnet(formula = class~pelvic_incidence + pelvic_tilt + lumbar_lordosis + sacral_slope + pelvic_radius + degree_spondylolisthesis, data = train.data, hidden=4, err.fct="ce", linear.output = FALSE)

# plot the network
plot(nn)      

#Model evaluation; Round the predicted probabilities
mypredict<-compute(nn, nn$covariate)$net.result
mypredict<-apply(mypredict, c(1), round)
mypredict [1:10]
# confusion matrix for the training set
table(mypredict, train.data$class, dnn =c("Predicted", "Actual"))

# confusion matrix for the test set
testPred <- compute(nn, test.data[, 0:6])$net.result
testPred<-apply(testPred, c(1), round)
table(testPred, test.data$class, dnn =c("Predicted", "Actual"))
# End Script