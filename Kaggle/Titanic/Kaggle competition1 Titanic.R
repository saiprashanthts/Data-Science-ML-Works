# Importig the Required Packages
library(dplyr)

# Loading Train Data Set

train <- read.csv('train.csv',header = T,stringsAsFactors = F,na.strings = '')
#  Checking the Head of the data set

head(train)
#  Checking the Structure of Data Set

str(train)

# Checking the Dimensions of Train Data Set
dim(train)
# 891  12

# Checking the summary of train data set
summary(train)

# Finding the total number of NA'sin Data Set
sum(is.na(train))
# Totally 177 NA values is present in Data Set

# Finding the total Number of Duplicated Rows in Data Set
sum(duplicated(train))
#  Total Number of Duplicates found is around 0 So No Duplicates Found

#  Finding  which columns has NA
sapply(train, function (x) sum(is.na(x)))
#  Out of 12 columns only age,Cabin,Embarked column has NA values it has around 177 NA values
# age 177/891 totally 20% of the values are NA so let us consider to impute the values to these NA's
# Cabin 687/891 totally 77% of the values are NA so let us remove that column
# Embarked 2 /891 totally are NA so let us impute it

#  Dropping Cabin column

train <- train[,-which(names(train) %in% c('Cabin'))]

# imputing emarked with mode (Highest frequency)

table(train$Embarked)
names(which.max(summary(factor(train$Embarked))))
names(sort(table(train$Embarked),decreasing = T)[1])
train[which(is.na(train$Embarked)),'Embarked']=names(which.max(summary(factor(train$Embarked))))
table(train$Embarked)

# By imputing the missing values by median 
train$Age
train[which(is.na(train$Age)),'Age'] <- median(train$Age,na.rm = T)

#  Finding  which columns has NA
sapply(train, function (x) sum(is.na(x)))

target<-as.data.frame(train$Survived)

# Finding the class or data type of columns of train data Set
sapply(train, function (x) class(x))
lapply(train, function (x) class(x))

# Dropping the Name column from train data set as it doesn't have much to do
train <- train[,-which(names(train) %in% c('Name','Ticket'))]

#  Now checking dimension of train
dim(train)
# 891  10

# Separting the columns into numric and character columns
numeric_train <-select_if(train,is.numeric)
numeric_train <- numeric_train[,-which(names(numeric_train) %in%  c('Survived'))]
character_train <- select_if(train,is.character)

# Checking only if character and numeric columns are present
unique(sapply(train, function (x) class(x)))
# "integer"   "character" "numeric" 
ncol(numeric_train) + ncol(character_train) == ncol(train)
# TRUE

# Checking for outliers
boxplot(numeric_train)
boxplot.stats(train$Fare)$out
quantile(numeric_train$Fare)
mean(numeric_train$Fare)
sd(numeric_train$Fare)
IQR(numeric_train$Fare)

#  Outlier Method to find if the value is greater than IQR + 3 *SD and less than IQR-3 *SD
outliers <- function (data)
{
  lower_limit <- IQR(data,na.rm = T) - 3*sd(data,na.rm =T)
  upper_limit <- IQR(data,na.rm = T) + 3*sd(data,na.rm = T)
  result <- which(data < lower_limit | data > upper_limit)
  
  # to find how many Such values are present
  #print(length(result))  
  
  # to find percentage of such values present
   print((length(result)/length(data))*100) 
  
  # to print the indices of such values present
  print(result) 
  #print(data[result])
  #data[result] <- median(data,na.rm = T)
}

lapply(numeric_train, outliers)
# 
# $PassengerId
# integer(0)
# 
# $Survived
# integer(0)
# 
# $Pclass
# integer(0)
# 
# $Age
# [1]   7  12  16  34  55  95  97 117 125 153 171 175 196 233 250 253 269 276 281 318 327 367 439 457 468
# [26] 484 488 493 494 497 514 546 556 571 572 583 588 626 627 631 648 660 673 685 695 746 773 775 830 852
# [51] 880
# 
# $SibSp
# [1]  60  72 160 181 202 325 387 481 684 793 847 864
# 
# $Parch
# [1]  14  26  87 168 361 438 439 568 611 639 679 737 775 859 886
# 
# $Fare
# [1]  28  89 119 259 300 312 342 378 381 439 528 558 680 690 701 717 731 738 743 780


# # Outlier Treatment

outliers_treatment <- function(data)
{
  min_v <- IQR(data,na.rm = T) - 3 * sd(data,na.rm = T)
  max_v <- IQR(data,na.rm = T) + 3 * sd(data,na.rm = T)
  result <- data < min_v | data > max_v
  #data[data < IQR(data,na.rm = T) - 3 * sd(data,na.rm = T) | data > IQR(data,na.rm = T) + 3 * sd(data,na.rm = T)] <- median(data,na.rm = T)
  #data[data < min_v | data > max_v] <- median(data,na.rm = T)
  data[result] <- median(data,na.rm = T)
  #print(length(result))
  data
  
}
numeric_train[] <- lapply(numeric_train,outliers_treatment )

# Graphical Analysis
library(ggplot2)

ggplot(character_train,aes(x=character_train$Sex)) + geom_bar()
#  Data Has more number of male compared to female

ggplot(character_train,aes(x=character_train$Embarked)) + geom_bar()
# Data has more number of embarked

ggplot(numeric_train,aes(x=factor(numeric_train$Pclass))) + geom_histogram(stat = "count")
# Data has more number of pclass 3

ggplot(numeric_train,aes(x=factor(numeric_train$SibSp))) + geom_histogram(stat = "count")
# Data has more number of sibsp 0

ggplot(numeric_train,aes(x=factor(numeric_train$Parch))) + geom_histogram(stat = "count")
# Data has more number of Parch 0

# Renaming column name of target
names(target) <-c('Survived')

train_raw = cbind(numeric_train,character_train,target)

View(train_raw)

character_train <- data.frame(lapply(character_train,function(x) factor(x)))
model.matrix(~,a)