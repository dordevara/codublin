# Random Forest model validation
# create samples in R
library(raster)
library(rgdal)
setwd("~/RDemo/capstone/Sentinel2data")
img.classified <- raster("RF_classification2.tif")

# We will use equalized stratified random sampling

# create 50 test samples per class
samplesperclass <- 50
smp.test <- sampleStratified(x = img.classified, 
                             size = samplesperclass, 
                             na.rm = TRUE, # exclude all NA values
                             sp = TRUE)
smp.test$RF_classification2
smp.test <- smp.test[sample(nrow(smp.test)), ] # mix the samples
smp.test$RF_classification2

# In addition, we can delete all variables in our dataframe smp.test and append a consecutive ID 
# variable called ID, which will then be displayed to us in QGIS:

smp.test <- smp.test[, -c(1, 2)]
smp.test$ID <- 1:nrow(smp.test)
smp.test

# to visualize the distribution of our validation points, we can plot the SpatialPointDataFrame 
# smp.test on top of our classification map in one plot:

plot(img.classified, 
     axes = FALSE, 
     box = FALSE,
     col = c("#e9eba4","#D95F02","#14614a","#78c91c","#e0d7d1","#801630","#de4e71","#2eabf0")
)
points(smp.test) 

# save the SpatialPointDataFrame smp.test as a shapefile to hard drive

# shapefile(smp.test,
#           filename = "validation_RF.shp",
#           overwrite = TRUE
# )




