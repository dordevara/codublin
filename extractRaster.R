## Subset and crop the initial Sentinel 2 image to area of interest
# start with clean environment
rm(list = ls())
graphics.off()

library(raster)
library(rgdal)
#library(rgeos) # dealing with vector data
library(RColorBrewer)

setwd("~/RDemo/capstone/Sentinel2data")
# read in the Sentinel 2 image that was resampled previously in SNAP
img <- stack("subset_0_of_S2A_MSIL2A_20200415T114351_N0214_R123_T29UPV_20200415T142447_resampled.tif")
# read in the administrative areas shapefile
ireland_shp <- readOGR("C://Users/Roxana/Documents/RDemo/capstone/webdata/aministrativeAreas", "0b3cbe50-ea67-467b-adb6-f3d9d3aaa43a2020329-1-f7bsff.bn1f")
ireland_shp

compareCRS(ireland_shp, img) # check if the projections of the two datasets are identical
# they don't have same projection, so we will use spTransform() fc to fix that
plot(ireland_shp, col=brewer.pal(n = 8, name = 'Paired'))
nlayers(img)
res(img)
crs(img)
crs(ireland_shp)
head(ireland_shp)
ireland_shp <- spTransform(ireland_shp, CRS("+init=EPSG:32629"))
compareCRS(ireland_shp, img)

plotRGB(img, r=4, g=3, b=2, stretch="lin")
plot(ireland_shp, col=brewer.pal(n = 8, name = 'Set2'), add=TRUE)

table(ireland_shp$COUNTY) # how many areas per county
codublin <- which(ireland_shp$COUNTY == 'DUBLIN') # selecting rows for Co. Dublin areas
codublin
codublin <- ireland_shp[codublin, ] # change it to a SpatialPolygonsDataFrame
codublin
plot(codublin)
head(codublin)

#buff <- gBuffer(codublin, width = 100, byid = TRUE)

codublin_shp <- crop(ireland_shp, codublin)
codublin_shp
plot(codublin_shp)
head(codublin_shp) # we spot other counties other than Dublin (like Meath and Leixlip)
vect <- which(codublin_shp$COUNTY != 'DUBLIN') # making sure they are not included
codublin_shp <- codublin_shp[-vect,]

img_subset <- crop(img, codublin_shp) # subset raster according to shapefile
plotRGB(img_subset, r=4, g=3, b=2, stretch="lin")
plot(codublin_shp, border='white', add=TRUE)

masked <- mask(img_subset, codublin_shp) # mask to co dublin area, this takes 5 mins
plotRGB(masked, r=4, g=3, b=2, stretch="lin")
nlayers(masked)
compareCRS(ireland_shp, masked)
masked

# save the resulting object to the hard drive
save(masked, file="masked.rda")
# and load it from the hard disk if necessary
# load(file="masked.rda")

#Explore and visualise
head(masked)
hasValues(masked)
names(masked)

plotRGB(masked, r=3, g=3, b=3, stretch="lin")

# examine the underlying data distribution in more detail. 
# we can look at a histogram of a specific band with the function hist()
extent(masked[[3]])
band3 <- masked[[3]]
hist(band3,
     breaks = 200,
     xlim = c(0, 4000),
     ylim = c(0, 12000),
     xlab = "band 3 reflectance value [DN * 0.01]",
     ylab = "frequency",
     main = "histogram Sentinel2 band 3 (green)"
)

# save area of interest on hard drive 

masked <- stack(masked)
masked <- masked[[1:7]] # keep only first 7 bands
names <- c("band1","band2","band3","band4","band5","band6","band7")
bandnames <- paste0(names, format=".tif") # change band names
names(masked) <- bandnames
hasValues(masked)
writeRaster(masked, "masked.tif", "GTiff", overwrite = T)
