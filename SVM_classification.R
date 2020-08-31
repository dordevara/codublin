# SVM Classification
library(raster)
library(e1071)
# set.seed(1)
setwd("~/RDemo/capstone/Sentinel2data")
img <- brick("masked.tif")
names(img)
names(img) <- c("b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8")
load(file="smp.rda")

# shuffle samples and undersample
summary(smp$cl)
head(smp)
smp <- smp[sample(nrow(smp)),]
head(smp)
smp.maxsamplesize <- min(summary(smp$cl))
smp.maxsamplesize
smp <- smp[ave(1:(nrow(smp)), smp$cl, FUN = seq) <= smp.maxsamplesize, ]
summary(smp$cl)

# Classifications using an SVM require two parameters: one gamma γ and one cost C value
# These hyperparameters significantly determine the performance of the model. Finding the best 
# hyparameters is not trivial and the best combination can not be determined in advance. 
# Thus, we try to find the best combination by trial and error. Therefore, we create two 
# vectors comprising all values that should be tried out:

gammas = 2^(-8:5)
gammas

costs = 2^(-5:8)
costs

# So we have 14 different values for γ and 14 different values for C. Thus, the whole training 
# process is done for 196 (14 * 14) models, often referred to as gridsearch. Conversely, this means 
# that the more parameters we check, the longer the training process takes.
# We start the training with the tune() function. We need to specify the training samples as 
# train.x, i.e., all columns of our smp dataframe except the last one, 
# and the corresponding class labels as train.y, i.e. the last column of our smp dataframe:

fit_svm <- tune(svm,
              train.x = smp[-ncol(smp)],     # data less classes (input variables)
              train.y = smp$cl,              # classes (output variable)
              type = "C-classification",     # perform a classification task
              kernel = "radial",      # we set the kernel used in training and predicting to a RBF kernel
              scale = TRUE,           # initiate the z-transformation of our data
              ranges = list(gamma = gammas, cost = costs), # takes a named list of parameter vectors(gammas and costs) spanning the sampling range
              tunecontrol = tune.control(cross = 5) # set k for the k-fold cross validation on the training data, which is necessary to assess the model performance
)
fit_svm #  the error of the best model is displayed: 0.058% error rate and the best parameters are :
#  gamma = 4  and cost = 4
  
plot(fit_svm) 
# The plot appears very homogeneous, as the performance varies only very slightly and is already 
# at a very high level. It can happen that the highest accuracies are found on the edge of the 
# grid search. Then the training with other ranges for γ and C should be done again.
# We can extract the best model out of our fit_svm to use for image prediction  
svm_model <- fit_svm$best.model 
# the model trained on the complete training data using the best parameter combination
svm_model 
  
save(svm_model, file = "svmmodel.RData")
#load("svmmodel.RData")  

result <- predict(img,
                  svm_model,
                  filename = "classification_svm.tif",
                  overwrite = TRUE
)

plot(result, 
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

