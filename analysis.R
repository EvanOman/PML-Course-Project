# setwd("C:/cygwin/home/Oman/PML")
# library(caret)
# allData <- read.csv("./pml-training.csv", na.strings=c("","NA"))
# 
# # Username, row nand timestamps are probabaly not good indicators
# allData <- subset(allData, select = -c(user_name,raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp,num_window,new_window,X))
# 
# for (i in names(allData))
# {
#   if (i != "classe")
#   {
#     allData[[i]] <- as.numeric(allData[[i]])  
#   }
# 
# }
# # Gets the number of rows
# nObs <- nrow(allData)
# 
# # We will now remove all predictors for which there are more than 25% NA's
# allData <- allData[,colSums(is.na(allData))/nObs < 0.25]
# 
# # Exploratory analysis:
# library(reshape2)
# library(ggplot2)
# d <- melt(data.frame(scale(allData[,1:(ncol(allData)-1)])))
# pdf("hist.pdf", height=20,width=20)
# ggplot(d,aes(x = value)) + 
#   facet_wrap(~variable,scales = "free_x") + 
#   geom_histogram() +
#   ggtitle("Scaled Histograms of All Predictors")
# dev.off()
# 
# inTrain <- createDataPartition(y=allData$classe,p=.7,list=FALSE)
# training <- allData[inTrain,]
# testing <- allData[-inTrain,]
# 
# #Fitting the model
# myControl <- trainControl(method="cv", 5)
# #modFit <- train(classe ~ ., method="rf", data=training, verbose=FALSE, trControl = myControl, ntree=250)
# #print(modFit)
# 
# # Make predictions
# preds <- predict(modFit, testing)
# 
# # Print accuracy
# confusionMatrix(preds,testing$classe)
# 
# # Save off our R model
# saveRDS(modFit, "mySavedModel001.rds")
# # modFit <- readRDS("mySavedModel001.rds")
# 
# # Plot the results
# # pdf("boostingPlot.pdf")
# # qplot(predict(modFit,testing),wage,data=testing)
# # dev.off()






## FROME OTHER FILE
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(reshape2)
library(ggplot2)
library(doMC)
registerDoMC(cores = 8)

allData <- read.csv("./pml-training.csv", na.strings=c("","NA"))
subTest <- read.csv("./pml-testing.csv")

allData.cleaned <- subset(allData, select = -c(user_name,raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp,num_window,new_window,X))

for (i in names(allData.cleaned[,1:(ncol(allData.cleaned)-1)]))
{
	allData.cleaned[[i]] <- as.numeric(allData.cleaned[[i]])
}

# Gets the number of rows
nObs <- nrow(allData.cleaned)

# We will now remove all predictors for which there are more than 25% NA's
allData.cleaned <- allData.cleaned[,colSums(is.na(allData.cleaned))/nObs < 0.25]


inTrain <- createDataPartition(y=allData.cleaned$classe,p=.7,list=FALSE)
training <- allData.cleaned[inTrain,]
testing <- allData.cleaned[-inTrain,]



myControl <- trainControl(method="cv", 5)
modFit <- train(classe ~ ., method="rf", data=training, verbose=FALSE, trControl = myControl, ntree=250)


# Make predictions
preds <- predict(modFit, testing)





# Print accuracy
confusionMatrix(preds,testing$classe)



# First we clean the submission test set
myPredictors <- names(allData.cleaned)
myPredictors <- myPredictors[myPredictors!= "classe"]
subTest <- subTest[,myPredictors]
# Make predictions on submission test set
predsSub <- predict(modFit, subTest)

predsSub



d <- melt(data.frame(scale(allData[,1:(ncol(allData)-1)])))
ggplot(d,aes(x = value)) + 
	facet_wrap(~variable,scales = "free_x", ncol = 4) + 
	geom_histogram() +
	ggtitle("Scaled Histograms of All Predictors")

pml_write_files = function(x){
	n = length(x)
	for(i in 1:n){
		filename = paste0("problem_id_",i,".txt")
		write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
	}
}
