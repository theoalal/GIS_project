#_________________________________________________________________________
#_________________________________________________________________________
####     Handling, Mapping and Analysing Spatial Data in R -course    ####
#_________________________________________________________________________
#          Module 3. Raster Spatial Analysis and Machine Learning        #
#_________________________________________________________________________

# Made by Pablo P?rez Chaves
# Created on 20/10/2022
# Updated on 08/11/2022

#### Packages ####
library(sp)
library(sf)
library(raster)
library(terra)
library(rgeos)
library(rgdal)
library(caret)
library(CAST)
install.packages("geodata")
library(geodata)
library(RColorBrewer)

#_________________________________________________________________________
####                    Part 1. Basics of raster                      ####
#_________________________________________________________________________

# Defining working directory
setwd("C:\\Users\\tyalal\\Downloads\\R-files\\R-GIS_Module3_data")

# Creating Raster Objects
# There are currently two packages for analyzing raster data in R:
#  (1) raster package
#  (2) terra package

# the terra package is designed to replace an old favourite raster.
# Terra package info states "can do more, is simpler to use, and it is faster."
# Excellent documentation online: https://rspatial.org/terra/index.html
# Compatibility with other packages can be an issue, but conversion back to raster is easy

r_raster <- raster() #basic function for creating/reading a raster file in raster package
r_raster #What is the object class?

r_terra <- rast() #basic function for creating/reading a raster file in terra package
r_terra #What is the object class?

# If you are already a user of raster package it is very easy to learn the syntaxt of terra
# Equivalence of functions/syntax here:
# https://www.rdocumentation.org/packages/terra/versions/0.2-9/topics/terra-package

# basic structure/functions for a raster file
ncol(r_terra) # number of columns 
nrow(r_terra) # number of rows
ncell(r_terra) # number of cells or "pixels"
dim(r_terra) # access to raster dimensions (nrow, ncol and ncell)
hasValues(r_terra) #checking if the raster has values
vals <- 1:ncell(r_terra) # creating values
r_terra <- setValues(r_terra, vals) #assigning values
hasValues(r_terra) #checking if the raster has values
plot(r_terra) #plotting the raster
res(r_terra) # spatial resolution (minimun pixel unit)
ext(r_terra) # will give the raster extent (xmin, xmax, ymin, ymax)
crs(r_terra) # access and asign projection
global(r_terra, mean) #summary statistics of the raster (basic functions: mean, median, sd, etc)
hist(r_terra) #histogram of the raster values

#_________________________________________________________________________
####        Part 2. Merging, masking, cropping raster files           ####
#_________________________________________________________________________
#    Using Digital Elevation Models (DEM) of Peruvian Central Amazonia
#_________________________________________________________________________

# Creating Coordinate Reference System for the study area
CRS84 <- CRS("+proj=longlat +datum=WGS84 +no_defs") # WGS84 CRS

# Read sampling units location as a shapefile
# Location of forest inventory plots
plots<-vect("plots_2016.shp")
plot(plots)

# Read study area as a shapefile
# Ucayali region in Peruvian Central Amazonia
study_area<-vect("study_area.shp")
plot(study_area)
plot(plots, pch=21, add=T)

# Create extent from study area
study_extent<-ext(study_area)
study_extent
#Read raster files
dem_1<-rast("ASTGTM2_S11W073_dem.tif") # digital elevation models
dem_2<-rast("ASTGTM2_S11W074_dem.tif")

plot(dem_1, legend=F)
plot(dem_2, legend=F, add=T)
merge_1<-merge(dem_1, dem_2) #using merge function. only 2 objects can be merged at a time
plot(merge_1)

# List and merge several files
dem <-list.files(pattern="_dem.tif", full.names = TRUE) 
# if the files are in another directory set it: list.files("C:/", pattern=" ", full.names = TRUE)
length(dem) #number of files in the list
dem[1] #calling the first element of the dem list
rast(dem[1]) #calling the first element of the dem list as a raster

dem.list <- list()
for (i in 1:length(dem)) dem.list[[i]] <- rast(dem[i]) #make a loop to merge all dem files in one
dem_merge <- do.call(merge, dem.list)

plot(dem_merge)
plot(study_area, add=T) #plot on top our study area

#cropping our study area
dem_crop<-crop(dem_merge, study_extent) #using the function crop
plot(dem_crop)
plot(study_area, add=T)

#masking our study area
dem_mask<-mask(dem_crop, study_area)
plot(dem_mask)
plot(plots, add=T)

dem_mask #checking min and max values

#using the function hist (histogram) 
#See the distribution and frequency of raster values
hist(dem_mask)  
                
dem2<-dem_mask #creating a new raster based on our dem
dem2 #check min and max values
dem2[dem2 > 2500 ] <- NA #all elevation values above 5000 changed as NA
dem2 #check again min and max values

mfrow=c(1,2)
par(mfcol=mfrow, mar=c(3,2,2,1), mgp=c(1.6,0.6,0))
plot(dem_mask,legend=F, add=T)
plot(dem2,legend=F, add=T)
x11()

dev.off() #clear and close current graphical device

#_________________________________________________________________________
####            Part 3. Raster algebra and operations                 ####
#_________________________________________________________________________
#         Using Landsat images from Peruvian Southern Amazonia
#_________________________________________________________________________
#       Calculating Normalized Difference Vegetation Index (NDVI)
#_________________________________________________________________________

#Reading a Landsat satellite image
#Tahuamanu province, Madre de Dios region, Southern Peruvian Amazonia
landsat<-rast("landsat_SA.tif") #open raster file using the raster function
names(landsat)<-c("band1","band2", "band3","band4","band5","band7") #change names like a simple vector
plot(landsat)

#Landsat reflectance values:
 # band 1 (blue, 0.45-0.51) --> Bathymetric mapping, distinguishing soil from vegetation,
 # band 2 (green, 0.53-0.59) --> Emphasizes peak vegetation, which is useful for assessing plant vigor
 # band 3 (red, 0.64-0.67) --> Discriminates vegetation slopes
 # band 4 (near-infrared, 0.85-0.88) --> Emphasizes biomass content and shorelines
 # band 5 (short-wave infrared, 1.57-1.65) --> Discriminates moisture content of soil and vegetation; penetrates thin clouds
 # band 7 (short-wave infrared, 2.1-2.29 ) --> Hydrothermally altered rocks associated with mineral deposits

#Normalized Difference Vegetation Index (NDVI)
 # For vegetation, there is a peak of reflectance between the red (R) and near-infrared (NIR) spectrum
 # NDVI = (NIR - R)/(NIR + R) --> for landsat: NDVI = (B4 - B3)/(B4 + B3)
 # Zero means no vegetation and close to +1 indicates high green vegetation density

#Now let's calculate the NDVI for our study area
landsat$band4 # selecting just band 4 individual raster
plot(landsat$band4)
landsat$band3 # selecting just band 3 individual raster
plot(landsat$band3)
ndvi<-(landsat$band4-landsat$band3)/(landsat$band4+landsat$band3)
ndvi
plot(ndvi)
plot(ndvi, xlim=c(-69.6,-69.4),ylim=c(-11.5, -11.3))

#Export raster files using the function writeRaster
#Let's explore the function
?writeRaster
writeRaster(ndvi, filename="ndvi.tif",format="GTiff") #export the new created file (NDVI)

# We can add more layers to our raster brick file!
landsat$NDVI<-ndvi

#Now let's plot it again
plot(landsat)

#we can keep on adding layers
#Remember that the files need to have the same extent and resolution
#let's add now an elevation raster (DEM)
elevation<-rast("dem_SA.tif")
plot(elevation, col=terrain.colors(10, alpha=1))
# other color palettes: rainbow, heat.colors, terrain.colors, topo.colors,cm.colors
#let's add the elevation file
landsat$DEM<-elevation
plot(landsat)

#Calculating a slope variable based on the elevation and automatically adding it to the lansat file
landsat$slope<-terra::terrain(landsat$DEM, v="slope", neighbors=8, unit="degrees")
plot(landsat$slope)
#Now let's plot it once again
plot(landsat)

#_________________________________________________________________________
####               Part 4. Extracting values from points               ####
#_________________________________________________________________________
#                          terra::extract function
#_________________________________________________________________________

# Read plot location as a shapefile
# Sampling points in the study area
plots<-vect("plots.shp")
plot(plots)

plot(landsat$DEM)
plot(plots, add=T)

# Extracting the elevation values from point data
points_dem<-terra::extract(landsat$DEM, plots, method='simple', df=TRUE)
points_dem
# Instead of doing it one by one for every raster of our multi-layered
# it is possible to extract them all automatically
points_pred<-terra::extract(landsat, plots, method='simple')
extract(landsat,plots,buffer=100,fun=mean)
# Exporting the results as a table 
write.csv(points_pred, file="points_pred.csv")

#_________________________________________________________________________
####                Part 5. Extracting values from polygons           ####
#_________________________________________________________________________
#                         terra::extract function
#_________________________________________________________________________

# Read a shapefile of our study areas in Mozambique
areas_MOZ<-vect("areas_MOZ.shp")
plot(areas_MOZ)

# Read raster files of temperature in Momzabique
pp_cur<-rast("MOZ_pp_current.tif") #current prcipitation
temp_cur<-rast("MOZ_tx_current.tif") # current temperatures

climate <- c(pp_cur,temp_cur,temp_cur, ftem_temp)


#Plot the study areas with the rainfall raster
plot(pp_cur)
plot(areas_MOZ, add=T)

#Plot the study areas with the temperature raster
plot(temp_cur)
plot(areas_MOZ, add=T)

# Extracting basic statistic from polygon data
# Maximum temperature
avg_temp<-terra::extract(temp_cur, areas_MOZ, fun=mean, df=TRUE) #mean temperature
min_temp<-terra::extract(temp_cur, areas_MOZ, fun=min, df=TRUE) #minimum temperature
max_temp<-terra::extract(temp_cur, areas_MOZ, fun=max, df=TRUE) #maximum temperature
sd_tem <- terra::extract(temp_cur, areas_MOZ, fun=sd)
med_tem <- terra::extract(temp_cur, areas_MOZ, fun=median)
#### Individual exercise 1 ####
# Calculate the min, max and mean future temperature and rainfall of the two areas
# Calculate the min, max and mean current temperature and rainfall of the two areas
# Calculate the difference between current and future climate

fpp_temp <- rast("MOZ_pp_future.tif") #future precipitation
ftem_temp <- rast("MOZ_tx_future.tif") #future temperature

# plotting the study areas
plot(fpp_temp)
plot(ftem_temp, add=T)

# Calculate the min, max and mean future temperature and rainfall of the two areas
min_temp<-terra::extract(fpp_temp, areas_MOZ, fun=min, df=TRUE)
min_temp
max_temp<-terra::extract(fpp_temp, areas_MOZ, fun=max, df=TRUE)
max_temp

climate <- c(pp_cur,temp_cur,temp_cur, ftem_temp)
plot(climate)
#
#.
#.
#.
#.


#_________________________________________________________________________
####            Part 6. Climate change analysis in Finland            ####
#_________________________________________________________________________
#                          Download CMIP6 data
#_________________________________________________________________________


# Set working directory
path<-"C:/Users/tyalal/Downloads/R-files/R-GIS_Module3_data"

# Download a shapefile of administrative limits of Finland
AOI<-getData('GADM', country='FIN', level=0) # levels are the administrative classes of a country
plot(AOI)
AOI2<-vect(AOI) #getData function works for sp package, we transform it to terra/sf
plot(AOI2)

# Defining all the climate models
#----------------------------------------------------------------------#
# There are diferent available climate models in the CMIP6
models<-c("BCC-CSM2-MR", "CNRM-CM6-1", "CNRM-ESM2-1", "CanESM5", "IPSL-CM6A-LR",
          "MIROC-ES2L", "MIROC6", "MRI-ESM2-0") #GFDL-ESM4 not available for ssp 245, 585

# Climate data is downloaded for 3 different time periods: 
# c("2021-2040","2041-2060","2061-2080")

# Defining the shared socio-economic  pathways (SSPs): 
# c("126","245","370","585")

# There are different climate variables: 
#  - Minimum temperature (tmin)
#  - Maximum temperature (tmax)
#  - Average temperature (tvag)
#  - Precipitation (prec)
#  - Bioclim (bio)

# There are different spatial resolutions options: 10, 5 and 2.5 degrees

# Making a double loop to download climate models
#---------------------------------------------------------------------------------#
# In the loop one climate variable (tmax) is downloaded
# The climate data is cropped to the extent of Finland
# Raster files are exported

for(i in c(1:length(models))) {
  model<-models[i]
  
    tmax<- cmip6_world(model=model, ssp="585", time="2061-2080", var="tmax", res=10, path=path)
    tmax<- terra::crop(tmax, ext(AOI2))
    writeRaster(tmax, filename=paste("FIN_tmax_10_ssp585_year70_",model,".tif",sep=""),overwrite=T)
    
}


#----------------------------------------------#
#### PART 6A. Preparing climate projections ####
#----------------------------------------------#

# Listing the future temperature in the directory
r_tmax_ssp585 <-list.files(pattern=glob2rx("FIN_tmax_10*ssp585*tif"), full.names = TRUE) 
r_tmax_ssp585 # there are 8 files with climate future projections
            # each file has 12 layers (january until december)
rast(r_tmax_ssp585[1])
plot(rast(r_tmax_ssp585[1]))

# Making a spatial predictions of climate projections for 2070 in Finland
#----------------------------------------------------------------------#
# Climate data (tmax and prec) for each SSP585

tmax_all_ssp585<-list()

for(i in c(1:length(models))) {
  ssp585_tmax<-max(stack(r_tmax_ssp585[i]))
  tmax_all_ssp585[[i]]<-ssp585_tmax

}

# Climate change spatial statistics #
#----------------------------------#
# Calculating statistics of the 8 models for tmax SSP585

r_tmax_all_ssp585<-stack(calc(stack(tmax_all_ssp585),quantile, na.rm=T), calc(stack(tmax_all_ssp585),sd))
writeRaster(r_tmax_all_ssp585, filename = "r_tmax_all_ssp585_2.5.tif",overwrite=TRUE)
plot(r_tmax_all_ssp585)

#--------------------------------------------------------#
#### PART 6B. Analysing climate change spatially      ####
#--------------------------------------------------------#
# Current climate conditions (tmax)
c_tmax<-worldclim_global(var="tmax", res=10, path=path)
c_tmax<- terra::crop(c_tmax, ext(AOI2))
max_tmax<-max(c_tmax)
plot(c_tmax)


# Current vs future climate (SSP585)

    mfcol=c(1,3)
    png(file="Tmax_FIN_585_2070.png", width = 12, height = 4, units = 'in', res = 250)
    par(mfrow=mfcol, mar=c(3,3,3,5), mgp=c(2,1,0))
    
    plot(raster(max_tmax), col=brewer.pal(n = 8, name = "YlOrRd"), main="Current Maximum Temperature (?C)")
    plot(AOI2, add=T, lwd=0.75)

    plot(r_tmax_all_ssp585$X50., col=brewer.pal(n = 8, name = "YlOrRd"), main="Maximum Temperature (?C) in 2070 (SSP585)")
    plot(AOI2, add=T, lwd=0.75)

    plot(r_tmax_all_ssp585$X50.-raster(max_tmax), col=brewer.pal(n = 8, name = "BuPu"), main="Variation in Maximum Temperature (?C)")
    plot(AOI2, add=T, lwd=0.75)

  dev.off()


plot(r_tmax_all_ssp585$layer)

#_________________________________________________________________________
####        Part 7. Machine learning for spatial prediction           ####
#_________________________________________________________________________
#                         Mapping biodiversity patterns
#_________________________________________________________________________

# Read in the data
nmds<-read.csv("nmds.csv")
nmds #explore the data

# Read the predictors
predictors<-brick("rs_predictors.tif")
names(predictors)<-c("band1","band2", "band3","band4","band5","band6","band7","ndvi","dem","slope","rain","temp")
plot(predictors)

# Convert data.frame as spatial data.frame
nmds_sp<-nmds
coordinates(nmds_sp) <- cbind(nmds$X , nmds$Y)
crs(nmds_sp)<-CRS("+init=epsg:32719")
plot(predictors$dem)
plot(nmds_sp, add=T)


#--------------------------#
#Random Forest regression
#--------------------------#

set.seed(100)

#Create a 10-fold cross-validation set
ctrl<-trainControl(method="cv",number=10,savePredictions=TRUE)
rs_var<-c("band1","band2", "band3","band4","band5","band6","band7","ndvi","dem","slope")  #define the remote sensing variables to use (names of the columns in the envitable)    

#model each ordination axis using all variables
#random cross-validation
nmds1_rf<-train(nmds[,rs_var],nmds[,"nmds1"],method="rf",metric="Rsquared",
                trControl=ctrl,importance=TRUE,ntree=100)
nmds2_rf<-train(nmds[,rs_var],nmds[,"nmds2"],method="rf",metric="Rsquared",
                trControl=ctrl,importance=TRUE,ntree=100)
nmds3_rf<-train(nmds[,rs_var],nmds[,"nmds3"],method="rf",metric="Rsquared",
                trControl=ctrl,importance=TRUE,ntree=100)

#variable importance of the models
plot(varImp(nmds1_rf))
plot(varImp(nmds2_rf))
plot(varImp(nmds3_rf))

#spatial predictions of ordination axes
r.nmds1<-predict(predictors, nmds1_rf)
r.nmds2<-predict(predictors, nmds2_rf)
r.nmds3<-predict(predictors, nmds3_rf)
plot(r.nmds1)
plot(r.nmds2)
nmds_sp<-stack(r.nmds1,r.nmds2,r.nmds3) # combine all three objects into one object
writeRaster(nmds_sp, filename = "nmds_sp.tif")

#plot predictions
plot(nmds_sp)
plotRGB(nmds_sp, 1,2,3, scale=3.586525, stretch="lin") # Beta diversity maps

#area of applicability (AOA)
nmds1_aoa<-aoa(predictors, nmds1_rf)
plot(nmds1_aoa)
plot(nmds1_aoa$AOA)

#forward feature selection (ffs)
nmds1_rf_ffs<-ffs(nmds[,rs_var],nmds[,"nmds1"],method="rf",metric="Rsquared",
                trControl=ctrl,importance=TRUE,ntree=100)

nmds1_rf_ffs$selectedvars #the order of the best variables selected
plot_ffs(nmds1_rf_ffs)
plot_ffs(nmds1_rf_ffs, plotType="selected")


#### Individual exercise 2 ####
# 1. make a random forest regression model of the vegetation cover (in the nmds csv it is the column COV)
# 2. use the same predictors as in the previous example ("band1","band2", "band3","band4","band5","band6","band7","ndvi","dem","slope")
# 3. what's the model performance? (report both R2 and RMSE)
# 4. what are the variables that contribute most of the model?
# 5. perform a variable selection using the feature forward selection (ffs) method
# 6. what are the selected variables after ffs
# 7. calculate the area of applicability (AOA) for the vegetation cover prediction 2 times:
#    - using the initial random forest model (rf)
#    - using the forward feature selection random forest (rf_ffs)
# 8. visualize both AOA predictions and respond, which AOA covers more area?

#.
#.
#.
#.

