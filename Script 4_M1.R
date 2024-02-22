#---------------------#
# Module 1 (script 4) #
#---------------------#

# Libraries
library(sf)
library(raster)
library(readxl)
library(rgdal)
library(units)

options(error = NULL)

# Part 1: Geometric operations with a logical output (TRUE or FALSE)
# Part 2: Geometric operations with a geometric output
# Part 3: Geometric measurements
# Part 4: Miscellaneous operations

#-----------------------------------------------------#
# An overview of the functions for sf and sfc objects # 
#-----------------------------------------------------#
methods(class = "sf")
methods(class = "sfc")

## Part 1: Geometric operations with a logical output (TRUE or FALSE)
# Commonly used functions: st_intersects(), st_contains()

## Part 2: Geometric operations without a logical output
# Commonly used functions: st_boundary(), st_buffer(), st_centroid(), st_convex_hull(),
# st_polygonize(), st_segmentize(), st_crop(), st_difference(), st_intersection(), st_join(), st_union()

## Part 3: Geometric measurements
# Commonly used functions: st_area() and st_distance()

## Part 4: Miscellaneous operations
# Commonly used functions: st_cast(), st_coordinates(), st_as_sf(), st_crs(), st_transform(),
# st_bbox(), st_drop_geometry(), st_make_grid(), st_sample()

#--------#
# Part 2 #
#--------#
# Creating a POINT geometry with 2 points
b <- st_sfc(st_point(c(0, 1)), st_point(c(1, 1)))
b

## st_buffer: adds a buffer around simple feature geometries
# When CRS is specified, the coordinates need to be planar (i.e., not represented as longitude/latitude)
b <- st_buffer(b, dist = 1)
plot(b, main="st_buffer")

## Intersection-related operations
## st_difference and st_sym_difference
bDifference.1 <- st_difference(b[1],b[2])
bDifference.2 <- st_difference(b[2],b[1])
bDifference.3 <- st_sym_difference(b[1],b[2])

## st_union: combines several geometries into a single geometry
bUnioned <- st_union(b)
bUnioned
plot(bUnioned, main = "st_union") # internal boundaries are lost

## st_intersection: returns the geometry of the intersected area
bIntersected <- st_intersection(b[1], b[2])
bIntersected
plot(bIntersected, col = "lightgrey", main="st_intersection")

# Clipping one section of the ?land Islands with st_intersection()
# Data aren't lost with the operation
Aland <- raster::getData("GADM", country = "Åland", level = 0)
Aland <- st_as_sf(Aland)
plot(st_geometry(Aland))

MyPoints <- locator(2)
M.1 <- matrix(c(MyPoints$x[1],MyPoints$y[1],MyPoints$x[2],MyPoints$y[1],
                MyPoints$x[2],MyPoints$y[2],MyPoints$x[1],MyPoints$y[2],
                MyPoints$x[1],MyPoints$y[1]), ncol = 2, byrow = TRUE)
Pol.1 <- st_sfc(st_polygon(list(M.1)), crs = st_crs(Aland))

plot(Pol.1, border = "red", add = TRUE)

# Clipping
Inter.1 <- st_intersection(Aland, Pol.1)
plot(st_geometry(Inter.1))

par(mfrow=c(3,3))
plot(b[1], col = "grey", main = "x")
plot(b[2], add = TRUE)
plot(b[2], col = "grey", main = "y")
plot(b[1], add = TRUE)
plot(bUnioned, col = "grey", main = "st_union")
plot(b[1], main = "st_intersection(x,y)")
plot(b[2], add = TRUE)
plot(bIntersected, col = "grey", add = TRUE)
plot(b[1], main = "st_difference(x,y)")
plot(b[2], add = TRUE)
plot(bDifference.1, col = "grey", add = TRUE)
plot(b[1], main = "st_difference(y,x)")
plot(b[2], add = TRUE)
plot(bDifference.2, col = "grey", add = TRUE)
plot(b[1], main = "st_sym_difference(x,y)")
plot(b[2], add = TRUE)
plot(bDifference.3, col = "grey", add = TRUE)
par(mfrow=c(1,1))

#--------#
# Part 3 #
#--------#
## Plotting nest locations onto the ?land Islands
setwd("")
MyData <- read_xlsx("DatasetPractice.xlsx")
MySpatialNests <- st_as_sf(MyData, coords = c("Longitude", "Latitude")) # the order matters: longitude always comes before latitude
MySpatialNests

## Setting the Coordinate Reference System (CRS)
st_crs(MySpatialNests) <- 4326 # a widely used longitude/latitude CRS (this is the CRS I used to create the point coordinates)

plot(st_geometry(Aland))
plot(st_geometry(MySpatialNests), pch = 3, cex = .8, lwd = 1.5, col = "red", add = TRUE)

# Firstly, to do the operations mentioned above, we need to convert the longitude/latitude CRS to a planar CRS
## A list of all CRSs available in R
CRSs <- make_EPSG()
CRSs[which(CRSs$code==3067),] # this is a commonly used planar CRS in Finland

MySpatialNests <- st_transform(MySpatialNests, 3067)
Aland <- st_transform(Aland, 3067)

all.equal(st_crs(MySpatialNests), st_crs(Aland))

## Calculating the distance between nests: st_distance()
MyNestGeometry <- st_geometry(MySpatialNests)
DistanceMatrix <- st_distance(MyNestGeometry)
DistanceMatrix <- data.frame(DistanceMatrix)
colnames(DistanceMatrix) <- paste0("Nest", c(1:nrow(MySpatialNests)))
rownames(DistanceMatrix) <- paste0("Nest", c(1:nrow(MySpatialNests)))

MaxDistances <- apply(DistanceMatrix, 1, max) # maximum distance between a focal nest and the others
MaxDistances # results are in metres

# Changing the unit
set_units(st_distance(MyNestGeometry), km)

## Calculating the area of each polygon (i.e., each island): st_area()
# Check the units
st_crs(Aland) # the unit is metre
st_geometry(Aland)
PAland <- st_cast(st_geometry(Aland), "POLYGON") # Converting MULTIPOLYGON to POLYGON
PAland

# Changing the unit
PolygonsAreas <- st_area(PAland) # results are in square metres
PolygonsAreas <- set_units(PolygonsAreas, km^2) # converting the units
set_units(PolygonsAreas, ha)

summary(PolygonsAreas)

# Subsetting the data based on the area
Index.1 <- which(as.numeric(PolygonsAreas) >=5) # identifying islands equal to or greater than 5 km2
Index.2 <- which(as.numeric(PolygonsAreas) <5) # identifying islands smaller than 5 km2

par(mfrow=c(1,2))
plot(PAland[Index.1], main = expression(paste("Islands equal to or greater than 5 km" ^ "2")))
plot(PAland[Index.2], main = expression(paste("Islands smaller than 5 km" ^ "2")))
par(mfrow=c(1,1))

## st_sample: samples points on or within geometries; the default method is random sampling
## st_convex_hull: returns the minimum convex polygon
MyRandomPoints <- st_sample(bUnioned, 20)
MyRandomPoints
plot(bUnioned, lty = 2, main = "st_sample & st_convex_hull")
plot(MyRandomPoints, pch = 3, add = TRUE)

MyPointsUnioned <- st_union(MyRandomPoints)
MyPointsHulled <- st_convex_hull(MyPointsUnioned)
plot(MyPointsHulled, add = TRUE)

#------------------------------------#
# Working a built-in spatial dataset #
#------------------------------------#
NorthCaroline <- st_read(system.file("shape/nc.shp", package="sf"))
NorthCaroline

# Transforming a longitude/latitude CRS into a planar CRS (planar coordinates are convenient or necessary for various operations)
# st_transform()
st_crs(NorthCaroline)
NorthCaroline <- st_transform(NorthCaroline, 32119)
st_crs(NorthCaroline)

plot(st_geometry(NorthCaroline))

# st_intersects(): which geometries are intersected?
par(mfrow=c(2,1))
plot(st_geometry(NorthCaroline[1:3,]), border = "red")
MyCentroids <- st_coordinates(st_centroid(NorthCaroline[1:3,]$geometry))
text(x = MyCentroids[,1], y = MyCentroids[,2], labels = c(1,2,3))

plot(st_geometry(NorthCaroline[1:2,]), border = "blue")
text(x = MyCentroids[1:2,1], y = MyCentroids[1:2,2], labels = c(1,2))
dev.off()

MyIntersect <- st_intersects(NorthCaroline[1:3,], NorthCaroline[1:2,], sparse = FALSE)
MyIntersect

# st_buffer()
MyBuffer.1 <- st_buffer(NorthCaroline$geometry[[1]], dist = 1000) # a buffer of 1000 m
plot(NorthCaroline$geometry[[1]])
plot(MyBuffer.1, border = "red", add = TRUE)

MyBuffer.2 <- st_buffer(NorthCaroline$geometry, dist = -4000) # a negative buffer of 4000 m
plot(NorthCaroline$geometry)
plot(MyBuffer.2, border = "red", add = TRUE)

# st_union(): combining two or more geometries into one
UnionNorthCaroline <- st_union(NorthCaroline)
plot(UnionNorthCaroline)

# st_intersection(): extracting a geometry from intersecting geometries
M.1 <- matrix(c(666427.7, 635624.7, 668628.0, 732434.3, 776438.7, 666427.7,
                260875.7, 223471.9, 190468.7, 170666.7, 230072.6, 260875.7), ncol = 2)
StudyArea <- st_sfc(st_polygon(list(M.1)))
st_crs(StudyArea) <- st_crs(NorthCaroline)

par(mfrow=c(1,2))
plot(NorthCaroline$geometry)
plot(StudyArea, lty = 2, lwd = 1.2, add = TRUE)

MyIntersection <- st_intersection(StudyArea, NorthCaroline)
plot(MyIntersection)
dev.off()