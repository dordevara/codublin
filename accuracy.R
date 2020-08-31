# Accuracy Statistics 

# using the validation_RF shapefile processed in QGIS

library(raster)
library(plyr)

setwd("~/RDemo/capstone/Sentinel2data")
img.classified <- raster("RF_classification2.tif")
shp.train <- shapefile("C://Users/Roxana/Documents/RDemo/capstone/QGISprocessed/training_data2.shp")
shp.valid <- shapefile("validation_RF")
table(shp.valid$validclass)

# We will generate two factor vectors, which we then compare in our confusion matrix:
# 1. reference : class labels assigned manually in QGIS
# 2. predicted : class labels that resulted in the automatic RF (or SVM) classification
reference <- as.factor(shp.valid$validclass)
reference # there are NAs, and we will ignore them later on

predicted <- as.factor(extract(img.classified, shp.valid))
predicted

# Once we prepared both factor vectors, we can utilize the table function in an elegant way to build 
# a contingency table of the counts at each combination of factor levels. We can additionally name 
# the vectors so that they are also displayed in the table accordingly:

accmat <- table("pred" = predicted, "ref" = reference)
accmat

# The numbers reflect the number of validation pixels. All pixels that have a NA value in either 
# reference or predicted were ignored here. This output already visualize if and where there are 
# misclassifications in our map: all pixels located on the diagonale are correctly classified, 
# all pixels off the diagonal are not.

UA <- diag(accmat) / rowSums(accmat) * 100
UA # user’s accuracies
PA <- diag(accmat) / colSums(accmat) * 100
PA # producer’s accuracies
OA <- sum(diag(accmat)) / sum(accmat) * 100
OA # overall accuracy

# create the confusion matrix
accmat.ext <- addmargins(accmat)
accmat.ext <- rbind(accmat.ext, "Users" = c(PA, NA))
accmat.ext <- cbind(accmat.ext, "Producers" = c(UA, NA, OA))
colnames(accmat.ext) <- c(levels(as.factor(shp.train$classes)), "Sum", "PA")
rownames(accmat.ext) <- c(levels(as.factor(shp.train$classes)), "Sum", "UA")
accmat.ext <- round(accmat.ext, digits = 1)
dimnames(accmat.ext) <- list("Prediction" = colnames(accmat.ext),
                             "Reference" = rownames(accmat.ext))
class(accmat.ext) <- "table"
accmat.ext

# Significance Test
# Furthermore, we can check if the result is purely coincidental, i.e., whether a random classification 
# of the classes could have led to an identical result. We can use a binomial test for this. 
# We only need two values for this test: x = total number of correctly classified validation points, 
# and n = the total number of validation points in our confusion matrix:

sign <- binom.test(x = sum(diag(accmat)),
                   n = sum(accmat),
                   alternative = c("two.sided"),
                   conf.level = 0.95
)
pvalue <- sign$p.value
pvalue
CI95 <- sign$conf.int[1:2]
CI95 # confidence interval at alpha=0.05

## Conclusion: The p-value is lower than 0.05, so the classification resulted map is somewhat significant.
# If the classification were repeated under the same conditions, it can be assumed that the OA is 95% 
# in the range of 56.92% to 79.45%. There is room for more model tuning, resampling, 
# to look for further improove model performance.