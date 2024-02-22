options(error=NULL)

#---------------------#
# Module 1 (script 1) #
#---------------------#

## Part 1: importing a spatial dataset (freely downloaded from a geodatabase) into R
## Part 2: exploring the spatial dataset
## Part 3: exporting a subset of the spatial dataset

# Libraries
library(sf)
library(rgdal)

#--------------------------------------#
#### Part 1: Importing spatial data ####
#--------------------------------------#
## Finland's wilderness reserves
# Data source: https://www.syke.fi/en-US/Open_information/Spatial_datasets/Downloadable_spatial_dataset
# Important points about this dataset:
  # Vector format (not raster format)
  # Coordinate Reference System (CRS): "EPSG:3067"

## Setting the working directory
setwd("")

# Looking at the function "st_read()"
?st_read
help(st_read) # another way of opening the help window

# Importing the file
Wilderness <- st_read("LsAlueEramaa.shp")

#--------------------------------#
#### Part 2: Data exploration ####
#--------------------------------#
## An overview of the dataset
Wilderness
class(Wilderness)
str(Wilderness)
summary(Wilderness)

## The geometries (sfc)
st_geometry(Wilderness)
plot(st_geometry(Wilderness)) # plotting only the geometries (not the variables)

# Note that the geometries can be dropped if needed, resulting in a normal data frame
Wilderness_NoGeometry <- st_drop_geometry(Wilderness)
Wilderness_NoGeometry
class(Wilderness_NoGeometry)

# Adding text to a plot with the function "text()"
Wild_geom <- st_geometry(Wilderness)
Wild_centroid <- st_centroid(Wild_geom)
class(Wild_centroid)

Wild_coord <- st_coordinates(Wild_centroid)
Wild_coord
class(Wild_coord)

x_coord <- Wild_coord[,1]
y_coord <- Wild_coord[,2]

?text
text(x=x_coord, y=y_coord, labels=1:12, font=2)

## Looking at specific observations/rows (sf)
Wilderness
Wilderness[1,] # Checking the first feature or observation
Wilderness[1:3,1] # Checking the first 3 features or observations of the first variable

## What is the Coordinate Reference System (CRS)?
st_crs(Wilderness)

# All available CRSs
CRSs <- make_EPSG() # EPSG
CRSs

## Retrieving the coordinates of the first geometry
Wild_geom_1 <- st_geometry(Wilderness)[[1]]
st_coordinates(Wild_geom_1)
head(st_coordinates(Wild_geom_1))

# The first geometry is composed of how many polygons?
sapply(Wild_geom_1, length)

## Retrieving the bounding box (delimitation) of the first geometry
st_bbox(st_geometry(Wilderness)[[1]])

## Looking closer at the variables
Wilderness
colnames(Wilderness)
Wilderness$Nimi

# Focusing on a single wilderness reserve
Wilderness_1 <- Wilderness[Wilderness$Nimi=="Hammastunturin erämaa",]
plot(st_geometry(Wilderness_1), main="Hammastunturi wilderness") # plotting only the geometry

#----------------------------------------------#
#### Part 3: Exporting a subset of the data ####
#----------------------------------------------#
## Choosing the directory (folder) where to export the file
setwd("")
?st_write
st_write(obj = Wilderness_1, dsn = "Hammastunturi.gpkg")
#st_write(Wilderness_1, "Hammastunturi.gpkg") # same as above, but without argument specification

#----------------------#
# Final considerations #
#----------------------#
## Functions (from library sf) used in this script
?st_read
?st_geometry
?st_drop_geometry
?st_centroid
?st_coordinates
?st_cast
?st_crs
?st_bbox
?st_write
