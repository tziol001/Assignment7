Assignment 7
========================================================
**Name:** Nikoula, Latifah & Nikos
**Date**: 13 January 2015

# Description
This document explains the methodology to create a model object using lm() using Landsat band reflectance values as predictors for tree cover (VCF). Using the resulting model object, it predicts VCF values for the Gewata area. A comparison between predicted tree cover raster with the original VCF raster is also executed. Finally, the RMSE between the predicted and the actual tree cover values is calculated as well as the RMSE separately for each of the classes (cropland, forest, wetland). 

# Pre-process
At the beginning of this assignment the appropriate libraries for the analysis,the functions and the required LandSat bands as well as the VCF Tree Cover product are loaded by using the script:

```r
rm(list=ls()) # clear the workspace

getwd()# make sure the data directory
```

```
## [1] "C:/Git/Assignment7"
```

```r
# Required packages and functions
lib <- c("raster","sp","rgdal", "knitr")
sapply(lib, function(...) library(..., character.only = TRUE))
```

```
## Loading required package: sp
## rgdal: version: 0.9-1, (SVN revision 518)
## Geospatial Data Abstraction Library extensions to R successfully loaded
## Loaded GDAL runtime: GDAL 1.11.1, released 2014/09/24
## Path to GDAL shared files: C:/Users/nikos/Documents/R/win-library/3.1/rgdal/gdal
## GDAL does not use iconv for recoding strings.
## Loaded PROJ.4 runtime: Rel. 4.8.0, 6 March 2012, [PJ_VERSION: 480]
## Path to PROJ.4 shared files: C:/Users/nikos/Documents/R/win-library/3.1/rgdal/proj
```

```
## $raster
##  [1] "raster"    "sp"        "knitr"     "stats"     "graphics" 
##  [6] "grDevices" "utils"     "datasets"  "methods"   "base"     
## 
## $sp
##  [1] "raster"    "sp"        "knitr"     "stats"     "graphics" 
##  [6] "grDevices" "utils"     "datasets"  "methods"   "base"     
## 
## $rgdal
##  [1] "rgdal"     "raster"    "sp"        "knitr"     "stats"    
##  [6] "graphics"  "grDevices" "utils"     "datasets"  "methods"  
## [11] "base"     
## 
## $knitr
##  [1] "rgdal"     "raster"    "sp"        "knitr"     "stats"    
##  [6] "graphics"  "grDevices" "utils"     "datasets"  "methods"  
## [11] "base"
```

```r
source("R/rmse.R")

# load the Landsat bands
list<-list.files ('data/', pattern = glob2rx('*.rda'), full.names = TRUE)
lapply(list,load,.GlobalEnv)
```

```
## [[1]]
## [1] "GewataB1"
## 
## [[2]]
## [1] "GewataB2"
## 
## [[3]]
## [1] "GewataB3"
## 
## [[4]]
## [1] "GewataB4"
## 
## [[5]]
## [1] "GewataB5"
## 
## [[6]]
## [1] "GewataB7"
## 
## [[7]]
## [1] "trainingPoly"
## 
## [[8]]
## [1] "vcfGewata"
```

Prior the analysis a rasterbrick is created including the Landsat bands and the original reflectance values should be rescalled to their original scale, between 0 and 1.

```r
landsat <- brick(GewataB1,GewataB2, GewataB3, GewataB4, GewataB5, GewataB7)
landsat <- calc(landsat, fun=function(x) x / 10000)
```

Before importing the vcfGewata rasterLayer a quality checked is required in order find possinle outlier vlaues which are caused by flags for water, cloud or cloud shadow pixels.

```r
hist(vcfGewata)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Histogram shows that,in the vcfGewata rasterLayer there are some values much greater than 100. However, the max value for tree cover is 100 (*source:*http://glcf.umd.edu/data/landsatTreecover), hence, these values should be removed prior add the vcf raster layer to the raster brick of Landsat bands.

```r
vcfGewata[vcfGewata > 100] <- NA
landsatvcf <-  addLayer(landsat,vcfGewata)
names(landsatvcf) <- c("band1", "band2", "band3", "band4",  "band5", "band7", "vcf")
```

The below scatterplot matrix shows the relationships between raster layers. Concerning the VCF indicates that is has a strong relationship with band 3 and 7.

```r
pairs(landsatvcf)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

As a final step prior the statisical analysis the values should be extracted into a data frame for convenience in calculations and demonstration of results.

```r
valuetable <- getValues(landsatvcf)
valuetable <- na.omit(valuetable)
valuetable <- as.data.frame(valuetable)
```

# Statistical analysis with lm model
Creating linear regression model with dependent variable the vcf and as independent variables the landsat bands. In this exercise only the relationship of band 3 as independent variable was investigated.

```r
model <- lm(vcf ~ band3, valuetable)
a<-summary(model) # show results
coef(model) # extract the model's coefficients
```

```
## (Intercept)       band3 
##    101.4772  -1039.6323
```

Summary of the model indicates that band 3 is a crucial factor to predict vcf.

The developed model is used in order to predict the vcf as follows:

```r
#predict vdf values based on the developed regression model
vcf.predict.raster <- predict(landsatvcf, model)
names(vcf.predict.raster) <- "vcf"
```

In this step a data frame with the predicted values is created in order to be used later for calculating the RMSE.

```r
vcf.predict <- predict(model, valuetable)
vcf.predict.df<-as.data.frame(vcf.predict)
hist(vcf.predict)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

```r
summary(vcf.predict.df)
```

```
##   vcf.predict     
##  Min.   :-196.38  
##  1st Qu.:  41.28  
##  Median :  63.32  
##  Mean   :  54.88  
##  3rd Qu.:  69.66  
##  Max.   :  92.64
```

# Plot the predicted tree cover raster and compare with the original VCF raster.

```r
p1=spplot(landsatvcf$vcf, zcol = 'vcf',col.regions = colorRampPalette(c("lightblue","green", "yellow","red"))(100), main= "Original VCF tree cover")

p2=spplot(vcf.predict.raster, zcol = 'vcf',zlim= c(0, 100),col.regions = colorRampPalette(c("lightblue","green", "yellow","red"))(100), main= "Predicted VCF tree cover")

print(p1, position = c(0,0,.5,1),more=T)
print(p2, position =  c(.5,0,1,1),more = T)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

# Compute the root mean squared error (RMSE) between the original and the predicted values

A function called rmse is developed by the authors in order to calculate the RMSE, easily. The function takes as inputs rasters. 

```r
rmse<-rmse(valuetable$vcf,vcf.predict)
print(paste("The rmse is equal to",rmse ))
```

```
## [1] "The rmse is equal to 10.3074748402283"
```

# Calculate the RMSE separately for each classes

Finally, a brick that contains the original and the predicted values is created and by Using the training polygons (3 classes) the mean values which are assigned to each class are "extracted" using the zonal function.

```r
a<-brick(landsatvcf$vcf,vcf.predict.raster)
classes <- rasterize(trainingPoly, vcf.predict.raster, field='Class')
zonal<-zonal(a,classes, mean)
zonal.df<-as.data.frame(zonal)
```
The rmse function is also used here:

```r
source("R/rmse.R")
rmse_1 <- rmse(zonal.df[1,2], zonal.df[1,3])
rmse_2 <- rmse(zonal.df[2,2], zonal.df[2,3])
rmse_3 <- rmse(zonal.df[3,2], zonal.df[3,3])
```

print(paste("The rmse is equal to",rmse_1,",",rmse_2,",",rmse_3,",for forest crop and water,repsectively."))
