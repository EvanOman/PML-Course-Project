setwd("C:/cygwin/home/Oman/PML")
library(caret)
allData <- read.csv("./pml-training.csv")

# Username and timestamps are probabaly not good indicators
allData <- subset(allData, select = -c(user_name,raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp))


inTrain <- createDataPartition(y=allData$classe,p=.7,list=FALSE)
training <- allData[inTrain,]
testing <- allData[-inTrain,]

# Fitting the model
modFit <- train(classe ~ ., method="gbm", data=training, verbose=FALSE)
print(modFit)

# Make predictions
preds <- predict(modFit, testing)

# Print accuracy

# Plot the results
# pdf("boostingPlot.pdf")
# qplot(predict(modFit,testing),wage,data=testing)
# dev.off()