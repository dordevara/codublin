# here I am going to work with the raster file masked.tif created from the Sentinel 2 product, 
# which was subset and cropped to the size of the Co Dublin administrative area according to 
# the shapefile, and with training_data2.shp file created in QGIS

# .libPaths("C:/Users/Roxana/anaconda3/envs/r_env/lib/R/library")
# .libPaths()

library(raster)
library(plyr)
set.seed(1)

setwd("~/RDemo/capstone/Sentinel2data")
img <- brick("masked.tif")
shp <- shapefile("C://Users/Roxana/Documents/RDemo/capstone/QGISprocessed/training_data2.shp")
shp
compareCRS(shp, img) # check if the projections of the two datasets are identical
plotRGB(img, r=4, g=3, b=2, stretch="lin")
plot(shp, col="red", add=TRUE)

# give classes meaningful names
x <- as.factor(shp$classes)
levels(x)
x <- revalue(x, c("b"="bareland", "c"="cropland", "f"="forest", "g"="green_area",
             "ind"="industrial", "rh"="residential_high","rl"="residential_low", "w"="water"))
levels(x)
for (i in 1:length(unique(x))) {
  cat(paste0(i, " ", levels(x)[i]), sep="\n")
}

# convert the classes column into factor data type as classifiers can only work 
# with integers instead of character strings like water or forest
for (i in 1:length(unique(shp$classes))) {
  cat(paste0(i, " ", levels(as.factor(shp$classes))[i]), sep="\n")
}

names(img) # will change the name of the bands
names(img) <- c("b1", "b2", "b3", "b4", "b5", "b6", "b7")

shp$classes[1:8]
levels(as.factor(shp$classes))[1:8]

# extract samples with class labels from initial dataset and put them all together in a dataframe
# the next process may take some long time, depending on the spatial resolution of raster data
smp <- extract(img, shp, df = TRUE) # this is important to ensure we work with unbiased data

# The data frame has as many rows as pixels could be extracted and as many columns as input features are 
# given (in this example the spectral channels). In addition, smp also provides a column named ID, which 
# holds the IDs of the former polygon for each pixel (each polygon is automatically assigned an ID). 
# Furthermore, we also know which polygon, i.e., each ID, belongs to which class. Because of this, 
# we can establish a relationship between the deposited ID of each pixel and the class using the match() 
# function. We use this to add another column to our data query describing each class. Then we delete 
# the ID column because we do not need it anymore:

head(smp)

smp$cl <- as.factor( shp$classes[ match(smp$ID, seq(nrow(shp)) ) ] )
smp <- smp[-1] # delete the ID column as it is not needed
dim(smp)
head(smp)
# save the resulting object to the hard drive
save(smp, file="smp.rda")
# and load it from the hard disk if necessary
# load(file="smp.rda")
# this way, the extract function does not have to be repeated again and again...

summary(smp$cl)
table(shp$classes)

# spectral profiles plot
# The differentiation of our classes per bands
sp <- aggregate( . ~ cl, data = smp, FUN = mean, na.rm = TRUE )
# plot empty plot of a defined size
plot(0,
     ylim = c(min(sp[2:ncol(sp)]), max(sp[2:ncol(sp)])), 
     xlim = c(1, ncol(smp)-1), 
     type = 'n', 
     xlab = "Sentinel-2 bands", 
     ylab = "reflectance [% * 100]"
)
# define colors for class representation, one color per class
mycolors <- c("#e9eba4","#D95F02","#14614a","#78c91c","#e0d7d1","#801630","#de4e71","#2eabf0")
# draw one line for each class
for (i in 1:nrow(sp)){
  lines(as.numeric(sp[i, -1]), 
        lwd = 4, 
        col = mycolors[i]
  )
}
# add a grid
grid()
# add a legend
sp$cl <- revalue(sp$cl, c("b"="bareland", "c"="cropland", "f"="forest", "g"="green_area",
                  "ind"="industrial", "rh"="residential_high","rl"="residential_low", "w"="water"))
legend(as.character(sp$cl),
       x = "topleft",
       col = mycolors,
       lwd = 5,
       bty = "n"
)