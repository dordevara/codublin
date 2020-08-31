# RF Classification
library(raster)
library(randomForest)

setwd("~/RDemo/capstone/Sentinel2data")
img <- brick("subset_0_of_S2A_MSIL2A_20200415T114351_N0214_R123_T29UPV_20200415T142447_resampled.tif")
plotRGB(img, r=4, g=3, b=2, stretch="lin") # natural color

shp <- shapefile("training_data.shp")
names(img) <- c("b1","b2","b3","b4","b5","b6","b7","b8","b9","b10","b11","b12","b13","b14")

load(file="smp.rda") # load extracted samples data
smp$cl <- as.factor( shp$classes[ match(smp$ID, seq(nrow(shp)) ) ] )
smp <- smp[-1] # delete the ID column as it is not needed
head(smp)
colSums(is.na(smp))
summary(smp$cl) # identify the number of available training samples per class
str(smp)
# This often leads to the problem that classifiers favor and overclass strongly-represented classes in 
# the classification. However, the Random Forest Algorithm, as an ensemble classifier, provides an ideal 
# solution to compensate for this imbalance. For each decision tree, we draw a bootstrap sample from the 
# minority class (class with the fewest samples). Then, we randomly draw the same number of cases, with 
# replacement, from all other classes. This technique is called down-sampling.

# In our example this is the class forest with 2486 samples. With the rep() function we form a vector where 
# the length corresponds to the number of target classes. We will use this vector to tell the classifier how 
# many samples it should randomly draw per class for each decision tree during training:

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
                  ntree = 250,          # trees to grow
                  importance = TRUE,    # allows the subsequent assessment of the variable importance
                  doBest = TRUE         # get the RF with the optimal mtry
)
# using tuneRF we automatically get a plot about the OOB errors depending of different mtry settings 

RF_model # get confusion matrix based on the training data

varImpPlot(RF_model) # plot the variable importance to identify which are the most important features for
# the classification - here b9, b1, b12, b10

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
plot(RF_model, col = c("#000000", "#fbf793", "#006601", "#bfe578", "#d00000", "#fa6700", "#6569ff"))
# We can see a decrease in the error with increasing number of trees, where the urban classes have the highest OOB error values

save(RF_model, file = "RF_model.RData")
load("RF_model.RData")

# predict image data with Random Forest model
result <- predict(img,
                  RF_model,
                  filename = "RF_classification.tif",
                  overwrite = TRUE
)

# plot the resulted classification map
plot(result, 
    # main = "Co Dublin Classification Map",
     axes = FALSE, 
     box = FALSE,
     col = c("#fbf793", # baresoil
             "#006601", # forest
             "#bfe578", # green_area
             "#d00000", # urban_high
             "#fa6700", # urban_low
             "#6569ff"  # water
     )
)




