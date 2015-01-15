# Name: Nikoula, Latifah & Nikos
# Date: 13 January 2015 

rm(list=ls()) # clear the workspace

getwd()# make sure the data directory

# Required packages and functions
lib <- c("raster","sp","rgdal", "knitr")
sapply(lib, function(...) require(..., character.only = TRUE))

# Load functions:
source("R/rmse.R")

# load the bands
list<-list.files ('data/', pattern = glob2rx('*.rda'), full.names = TRUE)
lapply(list,load,.GlobalEnv)

#Prior the analysis a rasterbrick is created including the Landsat bands ans the values of each band are rescalled from 0 to 1.
landsat <- brick(GewataB1,GewataB2, GewataB3, GewataB4, GewataB5, GewataB7)
landsat <- calc(landsat, fun=function(x) x / 10000)

# the max value for tree cover is 100 (http://glcf.umd.edu/data/landsatTreecover/), remove the value above 100
vcfGewata[vcfGewata > 100] <- NA
landsatvcf <-  addLayer(landsat,vcfGewata)
names(landsatvcf) <- c("band1", "band2", "band3", "band4",  "band5", "band7", "vcf")
landsatvcf

# extract the values into a data frame
valuetable <- getValues(landsatvcf)
valuetable <- na.omit(valuetable)
valuetable <- as.data.frame(valuetable)

# Creating linear regression model with dependent variable the vcf and as independent variables the landsat bands
model <- lm(vcf ~ band3, valuetable)
a<-summary(model) # show results
a$r.squared
coef(model) # extract the model's coefficients

##layout(matrix(1:4,2,2)) 
##plot(model)

# predict vdf values based on the developed regression model
vcf.predict.raster <- predict(landsatvcf, model)
names(vcf.predict.raster) <- "vcf"

#make the data frame
vcf.predict <- predict(model, valuetable)
vcf.predict.df<-as.data.frame(vcf.predict)
hist(vcf.predict)
summary(vcf.predict.df)

# plot the predicted tree cover raster and compare with the original VCF raster.
p1=spplot(landsatvcf$vcf, zcol = 'vcf',col.regions = colorRampPalette(c("lightblue","green", "yellow","red"))(100), main= "Original VCF tree cover")

p2=spplot(vcf.predict.raster, zcol = 'vcf',zlim= c(0, 100),col.regions = colorRampPalette(c("lightblue","green", "yellow","red"))(100), main= "Predicted VCF tree cover")

print(p1, position = c(0,0,.5,1),more=T)
print(p2, position =  c(.5,0,1,1),more = T)

# Compute the root mean squared error (data frames)
rmse<-rmse(valuetable$vcf,vcf.predict)
print(paste("The rmse is equal to",rmse ))

# calculate the RMSE separately for each of the classes and compare

#make it with zonal
a<-brick(landsatvcf$vcf,vcf.predict.raster)
classes <- rasterize(trainingPoly, vcf.predict.raster, field='Class')
zonal<-zonal(a,classes, mean)
zonal.df<-as.data.frame(zonal)

source("R/rmse.R")
rmse_1 <- rmse(zonal.df[1,2], zonal.df[1,3])
rmse_2 <- rmse(zonal.df[2,2], zonal.df[2,3])
rmse_3 <- rmse(zonal.df[3,2], zonal.df[3,3])

print(paste("The rmse is equal to",rmse_1,",",rmse_2,",",rmse_3,",for forest crop and water,repsectively."))
