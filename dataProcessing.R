# Cloud Masking for Sentinel-2 images
# If there are clouds or cloud shadows on the Sentinel-2 scene, they can be mask out using the quality scene classification band of the scene

#install.packages("raster")
#install.packages("rgdal")
library(raster)
library(rgdal)

setwd("~/RDemo/capstone/resampledData")
# Open and plot the image
sen2 <- stack("subset_0_of_S2B_MSIL2A_20200328T113319_N0214_R080_T29UPV_20200328T131505_resampled.tif")
setwd("~/RDemo/capstone")
plot(sen2)
plotRGB(sen2, 3,2,1, stretch="lin")

# Image quality band and the classes we want to delete for our mask (Values): 2 (cloud shadows), 6 (unclassified), 
# 7 (cloud medium probability), 8 (cloud high probability), 9 (thin cirrus) and 10 (snow or ice). 
# For other scenes, you have to adjust the classes if necessary.


# Seperate spectral bands and classification (band 1 to 11 is the multispectral 
# Sentinel-2 scene, band 12 is the quality classification band)

sen2_bands <- sen2[[-12]] # multispectral Sentinel-2 scene

class(sen2)
dim(sen2)

sen2_mask <- sen2[[12]]  # the quality classification band

# Which pixels do we want to mask?
plot(sen2_mask)

sen2_mask_combi <- sen2_mask
sen2_mask_combi[sen2_mask == 2 |sen2_mask == 6 |sen2_mask == 7 | sen2_mask == 8 |sen2_mask == 9 |sen2_mask == 10 ] <- NA
plot(sen2_mask_combi) # Mask without the classes: 2,6,7,8,9,10

#writeRaster(sen2_mask_combi, "sen2_mask.tif")

# apply mask
sen2_bands_masked <- raster::mask(sen2_bands,sen2_mask_combi)
plotRGB(sen2_bands_masked, 3,2,1, stretch="lin")
#writeRaster(sen2_bands_masked, "sen2_masked.tif")


#sen2_bands_masked_a <- sen2_bands 
#sen2_bands_masked_a [sen2_mask == 2 |sen2_mask == 6 |sen2_mask == 7 | sen2_mask == 8 |sen2_mask == 9 |sen2_mask == 10 ] <- NA 
#writeRaster(sen2_bands_masked_a, "sen2_masked_alternativ.tif")
#plot(sen2_bands_masked_a)




