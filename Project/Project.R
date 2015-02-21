library(caret)
library(randomForest)

#setwd("D:/Userfiles/dfernandezcanon/Desktop/ML")
setwd("/Users/dfernandezcanon/Documents/Development/R/Project/08-Practica_Machine_Learning/Project")

#getting the data
train.url <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test.url <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(train.url, destfile = "./data/pml-training.csv", method = "curl")
download.file(test.url, destfile = "./data/pml-testing.csv", method = "curl")


#creating data frame for the data
training.data <- read.table("./data/pml-training.csv",sep=",",na.strings = c("NA",""),header=TRUE)
testing.data <- read.table("./data/pml-testing.csv",sep=",",na.strings = c("NA",""),header=TRUE)

#cleaning data
#deleting columns that contains only NA
training.data <- training.data[, colSums(is.na(training.data)) == 0]
testing.data <- testing.data[, colSums(is.na(testing.data)) == 0]

#deleting columns that not contribute to our study
training.remove <- grepl("^X|timestamp|window|user", names(training.data))
training.data <- training.data[, !training.remove]
testing.remove <- grepl("^X|timestamp|window|user", names(testing.data))
testing.data <- testing.data[, !testing.remove]

#making the project reproducible
set.seed(647)

#particioning the training data set into two data sets, one for training and another one for testing
inTrain <- createDataPartition(training.data$classe, p=0.70, list=FALSE)
training <- training.data[inTrain,]
validation <- training.data[-inTrain,]

#using random forest prediction
random.forest <- randomForest(classe ~ ., data=training, method="rf")
random.forest

#predicting new values using the remaining data from training and comparing the results
pred <- predict(random.forest, validation)
confusionMatrix(validation$classe, pred)

validation$predRight <- pred==validation$classe
table(pred,validation$classe)

#predicting cases using testing data

#answers <- predict(random.forest, testing.data)
#answers

#pml_write_files = function(x){
#  n = length(x)
#  for(i in 1:n){
#    filename = paste0("problem_id_",i,".txt")
#    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
#  }
#}

#pml_write_files(answers)
