############################ SVM Handwritten Digit Recognizier #################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#  4.1 Linear kernel
#  4.2 RBF Kernel
# 5 Hyperparameter tuning and cross validation

#####################################################################################
#Loading Neccessary libraries

library(kernlab)
library(readr)
library(caret)
library(ggplot2)

#####################################################################################

# 1. Business Understanding: 

#The objective is to identify each of a classify the handwritten digits based on the pixel values given as features
#####################################################################################

# 2. Data Understanding: 
# Train
# Number of Instances: 60,000
# Number of Attributes: 785 

# Test
# Number of Instances: 20,000
# Number of Attributes: 17 
# Reading the Data Set

# Loading Training and Testing DataSets using read.csv utlity
mnist_train <- read.csv("mnist_train.csv",header = F, stringsAsFactors = F)
mnist_test <- read.csv("mnist_test.csv",header=F,stringsAsFactors = F)


# Checking the structure of data frame
str(mnist_train)
str(mnist_test)

# Checking if any na values are present 

sum(is.na(mnist_train))
sum(is.na(mnist_test))

# Checking if any duplicates are present
sum(duplicated(mnist_train))
sum(duplicated(mnist_test))

# Checking the number of columns and Rows in Data Frame
ncol(mnist_train)
nrow(mnist_train)

ncol(mnist_test)
nrow(mnist_test)

# Checking the column names of mnist_train,mnist_test data frame
colnames(mnist_train)
colnames(mnist_test)

# Checking the Summary of mnist_train,mnist_test Data Frame
summary(mnist_train)
summary(mnist_test)

# Checking the class of mnist_train,mnist_test Data frame
class(mnist_train)
class(mnist_test)

# Checking the dimension of mnist_train,mnist_test
dim(mnist_train)
dim(mnist_test)

#Making our target class to factor

mnist_train$V1<-factor(mnist_train$V1)

mnist_test$V1<-factor(mnist_test$V1)

#  Checking if colnames of train and test are same
colnames_mnist_train<-colnames(mnist_train)
length(colnames_mnist_train)
colnames_mnist_test<-colnames(mnist_test)
length(colnames_mnist_test)
setdiff(colnames_mnist_train,colnames_mnist_test)
setdiff(colnames_mnist_test,colnames_mnist_train)
#####################################################################################
# User Defined Functions

udf_confusion <- function(model_name)
{
  Eval_Poly<- predict(model_name, test)
  a<-confusionMatrix(Eval_Poly,test$V1)
  return(a)
}


#####################################################################################
# EDA
# bar plot between Digits vs Count for train
ggplot(mnist_train,aes(x=mnist_train$V1)) + geom_bar() + 
  xlab("Digits(1-10)") + ylab("Count")  +
  ggtitle("Count Vs Digits") 

# bar plot between Digits vs Count for test
ggplot(mnist_test,aes(x=mnist_test$V1)) + geom_bar() + 
  xlab("Digits(1-10)") + ylab("Count")  +
  ggtitle("Count Vs Digits") 

#####################################################################################
# Split the mnist_train into 15 percent only for training

set.seed(1)
train.indices = sample(1:nrow(mnist_train), 0.15*nrow(mnist_train))
train = mnist_train[train.indices, ]
test=mnist_test

# Each row is a handwritten digit converted into a matrix and then into a 1D vector. each column represents the pixel info. so cannot delete any column. 

#Understanding Dimensions

dim(train)

#Structure of the dataset

str(train)

#printing first few rows

head(train)

#Exploring the data

summary(train)

#checking missing value
#  No missing value
sapply(train, function(x) sum(is.na(x)))
#####################################################################################
#Constructing Model

#Using Linear Kernel
Model_linear <- ksvm(V1~ ., data = train, scaled = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, test)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test$V1)
# Accuracy : 0.9166 
#####################################################################################
#Using Poly Kernel
Model_poly <- ksvm(V1~ ., data = train, scaled = FALSE, kernel = "polydot")
print(Model_poly)

Eval_poly<- predict(Model_poly, test)
#confusion matrix - poly Kernel
# User Defined Function for predicting and finding out confusion matrix
Poly_kernel_confusion_matrix <- udf_confusion(Model_poly)
print(Poly_kernel_confusion_matrix)
# Accuracy : 0.9166 
#####################################################################################
#Using RBF Kernel
Model_RBF <- ksvm(V1~ ., data = train, scaled = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, test)
print(Model_RBF)
#confusion matrix - RBF Kernel

RBF_kernel_confusion_matrix <- udf_confusion(Model_RBF)
print(RBF_kernel_confusion_matrix)
confusionMatrix(Eval_RBF,test$V1)
# Accuracy= 0.9585

############   Hyperparameter tuning and Cross Validation #####################
# Linear Hyper Parameters
trainControl <- trainControl(method="cv", number=5)
# Number - Number of folds 
# Method - cross validation
metric <- "Accuracy"
set.seed(7)
# making a grid of C values. 
grid_linear <- expand.grid(C=seq(1, 5, by=1))

# Performing 5-fold cross validation
fit.svm_linear <- train(V1~., data=train, method="svmLinear", metric=metric, 
                 tuneGrid=grid_linear, trControl=trainControl)

# Printing cross validation result
print(fit.svm_linear)

# Plotting "fit.svm" results
plot(fit.svm_linear)
#  It is same for all hyper parameters accuracy got is 0.91
#####################################################################################
# Radial Hyperparameter
#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 5 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=5)

# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.
metric <- "Accuracy"
#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
set.seed(7)
grid <- expand.grid(.sigma=c(0.0000001,0.0000002,0.0000003,0.0000004,0.0000005,0.0000006), .C=c(1,2,3,4,5) )
#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.
fit.svm <- train(V1~., data=train, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)
print(fit.svm)
plot(fit.svm)

Model_RBF_hyper <- ksvm(V1~ ., data = train, scaled = FALSE, kernel = "rbfdot",.sigma=0.0000004, .C=5)
Eval_RBF<- predict(Model_RBF_hyper, test)
confusionMatrix(Eval_RBF,test$V1)
# User Defined Function for predicting and finding out confusion matrix
udf_confusion(Model_RBF_hyper)
# Accuracy 0.9586 (Best)
#####################################################################################
#  poly Hyper parameters

trainControl <- trainControl(method="cv", number=5)

# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.
metric <- "Accuracy"
#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
set.seed(7)

grid <- expand.grid(.degree=c(1,2,3), .C=c(1,5),.scale=1)
fit.svm_poly <- train(V1~., data=train, method="svmPoly", metric=metric, 
                      tuneGrid=grid, trControl=trainControl)
print(fit.svm_poly)
plot(fit.svm_poly)

Model_poly_hyper <- ksvm(V1~ ., data = train, scaled = FALSE, kernel = "rbfdot",.degree=2, .C=5)
Eval_Poly<- predict(Model_poly_hyper, test)
confusionMatrix(Eval_Poly,test$V1)

# User Defined Function for predicting and finding out confusion matrix
udf_confusion(Model_poly_hyper)
# Accuracy 0.9586

#####################################################################################
#  SO Overall best model is Radial with following hyper parameters got test accuracy as 0.9586
Model_RBF_hyper <- ksvm(V1~ ., data = train, scaled = FALSE, kernel = "rbfdot",.sigma=0.0000004, .C=5)
Eval_RBF<- predict(Model_RBF_hyper, test)
confusionMatrix(Eval_RBF,test$V1)
# accuracy 0.9586
sensitivity_of_test=(0.9878  + 0.9894  + 0.9496 +  0.9535  + 0.9603 +  0.9372 +  0.9718  + 0.9397  + 0.9466 + 0.9455)/10
#  0.95814

specificity_of_test=(0.9962 +  0.9971 +  0.9942 +  0.9957  + 0.9956 +  0.9948 +  0.9961 +  0.9960 +  0.9947 + 0.9455) /10
# specificity 0.9905

Model_RBF_hyper_kernel_confusion_matrix <- udf_confusion(Model_RBF_hyper)
print(RBF_kernel_confusion_matrix)

# User Defined Function for predicting and finding out confusion matrix
udf_confusion(Model_RBF_hyper)