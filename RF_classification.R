# RF Classification
library(raster)
library(randomForest)
library(rgdal)
library(rgeos)

set.seed(1)
setwd("~/RDemo/capstone/Sentinel2data")
img <- brick("masked.tif")
names(img) # will change the name of the bands
names(img) <- c("b1", "b2", "b3", "b4", "b5", "b6", "b7")

load(file="smp.rda")
head(smp)
colSums(is.na(smp)) # verify if there are NA values
table(smp$cl) # identify the number of available training samples per class
# we can see that we currently have an imbalanced data set (one class contains significantly more samples than the other)
# This often leads to the problem that classifiers favor and overclass those strongly-represented classes in 
# the classification. 

# In this case, we will perform undersampling procedure (down-sizing the non-events by removing observations at random until the dataset is balanced)

smp.size <- rep(min(summary(smp$cl)), nlevels(smp$cl)) # down-sampling
smp.size

# The complete training takes place via just one function call of tuneRF(); this fc searches for the best param
# setting for mtry - the no of vars available for each tree node. The no of trees to grow 250-1000 trees are 
# usually sufficient, the more the better, but many trees will increase calculation time. We need to specify
# training samples as x (i.e. all columns of our smp dataframe except the last one) and the corresponding class
# labels as y (the last column of smp dataframe called cl):

RF_model <- tuneRF(x = smp[-ncol(smp)], # specify training samples
                   y = smp$cl,           # specify the class label
                   sampsize = smp.size,  # define how many samples it should draw per class
                   strata = smp$cl,      # defines the column which should use for this stratified sampling
                   ntree = 300,          # trees to grow
                   importance = TRUE,    # allows the subsequent assessment of the variable importance
                   doBest = TRUE         # get the RF with the optimal mtry
)
# using tuneRF we automatically get a plot about the OOB errors depending of different mtry settings 
# the optimal mtry (the number of features randomly selected at each split) here is 7


RF_model # get confusion matrix based on the training data
# our Out-of-Bag Error (Misclassification Rate) is 3.28%

varImpPlot(RF_model) # plot the variable importance to identify which are the most important features for
# the classification - here b1, b7 and b5

# MeanDecreaseAccuracy: we permute all threshold values of a specific feature (band5) and see how the 
# purity in a Leaf Node changes. In detail: we count the number of correctly classified OOB samples once 
# the tree is grown. After that, we randomly permutate the values of a specific feature for the OOB samples 
# and classify the OOB samples again. The difference of correctly classified samples between permuted 
# and original OOB data, averaged over all trees, is the MeanDecreaseAccuracy.

# MeanDecreaseGini: each time a split is done, both sub-nodes become more pure and the Gini values decrease. 
# The sum of all Gini decreases for each individual feature (band5) over all trees is the MeanDecreaseAccuracy.

# plot the relationship between OOB Error and the number of trees used. We can color the lines by passing a 
# vector of hex-colors whose length equals the number of classes plus one (the first color is the average OOB line, 
# which is also plotted automatically)
plot(RF_model, col = c("#e9eba4","#D95F02","#14614a","#78c91c","#e0d7d1","#801630","#de4e71","#2eabf0"))
# We can see a decrease in the error with increasing number of trees, where the urban classes have the highest OOB error values

#### manual tuning of model
oob_error <- double(7) # similar to rep(); create double precision vectors
test_error <- double(7)
img2 <- stack(img)
imgdf <- as.data.frame(img2)
head(imgdf)
imgdf <- na.omit(imgdf)
head(imgdf)
dim(imgdf)
for (mtry in 1:7){
  fit_RF <- randomForest(smp$cl ~., smp[-ncol(smp)], ntree=300, mtry=mtry)
  oob_error[mtry] <- fit_RF$err.rate[300] # error of all trees fitted
  pred <- predict(fit_RF, imgdf)
  test_error[mtry] <- with(imgdf, mean(smp$cl == pred))
  cat(mtry, " ") # printing the output to the console
}

mean(fit_RF$err.rate)
test_error
oob_error
matplot(1:mtry, cbind(oob_error, test_error), pch=19, col=c("red","blue"), type="b",
        ylab="Error", xlab="Number of Predictors Considered at each Split")
legend("center", legend = c("Out of Bag Error", "Test Error"), pch=19, col=c("red","blue"))
fit_RF
fit_RF <- randomForest(smp$cl ~., smp[-ncol(smp)], ntree=200, mtry=4)
fit_RF

plot(fit_RF)

save(fit_RF, file = "RF_model.RData")
# load("RF_model.RData")

# predict image data with Random Forest model
result <- predict(img,
                  fit_RF,
                  filename = "RF_classification2.tif",
                  overwrite = TRUE
)
#levels(as.factor(result$RF_classification2))

# plot the resulted classification map
plot(result, 
     main = "Co Dublin RF Classification Map",
     axes = FALSE, 
     box = FALSE,
     col = c("#e9eba4", # bareland
             "#D95F02", # cropland
             "#14614a", # forest
             "#78c91c", # green_area
             "#e0d7d1", # industrial
             "#801630", # residential_high
             "#de4e71", # residential_low
             "#2eabf0"  # water
     )
)
# 
# ##################################################################
# # another way of fitting random forest and decision tree using random sampling method
# library(caret)
# ctrl <- trainControl(method = "repeatedcv",                        
#                      number = 10, repeats = 10,       # use 10-fold CV repeated 10 times                 
#                      selectionFunction = "best",                        
#                      savePredictions = TRUE,                        
#                      classProbs = TRUE,                        
#                      summaryFunction = multiClassSummary)
# grid_rf <- expand.grid(mtry = c(2, 4, 7))
# RNGversion(vstr = "3.6.1"); set.seed(1) #  set the random generators for reproducibility
# m_rf <- train(smp$cl ~ ., data = smp[-ncol(smp)], 
#               method = "rf",                 
#               metric = "ROC", 
#               trControl = ctrl,                 
#               tuneGrid = grid_rf
#               )
# #warnings()
# #install.packages("MLmetrics", dependencies = TRUE)
# # compare the best forest to the best boosted decision tree among trees with 10 , 25 , 50 , and 100  iterations 
# grid2 <- expand.grid(model = "tree",                           
#                         trials = c(10, 25, 50, 100),                           
#                         winnow = FALSE
#                         ) 
# RNGversion(vstr = "3.6.1"); set.seed(1)
# # boosted decision tree model:
# m2 <- train(smp$cl ~ ., data = smp[-ncol(smp)],                               
#                method = "C5.0",           
#                metric = "ROC", 
#                trControl = ctrl,                  
#                tuneGrid = grid2
#                )
# # compare the two approaches side by side. 
# # For the random forest model, the results are:
# m_rf
# # For the boosted model, the results are:
# m2
# 
# library(pROC)
# ROC_m_rf <- roc(m_rf$pred$obs, m_rf$pred$yes)
# ROC_m2 <- roc(m2$pred$obs, m2$pred$yes)
# plot(ROC_m_rf, col = "red", legacy.axes = TRUE, 
#      main = "ROC curves comparing a RF to a boosted DT")
# plot(ROC_m2, col = "blue", add = TRUE)
# 
# # however, until they are used for prediction, we don't know which model is preferred