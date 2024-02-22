options(error=NULL)

#---------------------#
# Module 1 (script 3) #
#---------------------#

# Simple features and the seven geometry types
# Creating the geometry types from scratch: POINT, LINESTRING, POLYGON, MULTIPOINT, MULTILINESTRING, MULTIPOLYGON and GEOMETRYCOLLECTION
# Adding attributes to a newly created geometry

## Remember that three classes define simple features
# Formal definitions
# 1) sfg (simple feature geometry): "the feature geometry of an individual simple feature"
# 2) sfc (simple feature list-column): "the list-column with the geometries for each feature (record)"
# 3) sf (simple feature): "the table (data.frame) with feature attributes and feature geometries"

# Steps: sfg => sfc => sf

# Libraries
library(sf)

## Geometry primitives ##
# 1) POINT: a single point (a zero-dimensional geometry)
P.1 <- st_point(c(5,10))
P.1
plot(P.1)

# 2) LINESTRING: a sequence of points connected by non-intersecting segments (a one-dimensional geometry)
MyMatrix.1 <- matrix(1:10, ncol = 2)
LS.1 <- st_linestring(MyMatrix.1)
LS.1
plot(LS.1)

# 3) POLYGON: a sequence of points that form a closed, non-intersecting ring (a two-dimensional geometry)
MyMatrix.2 <- matrix(c(0,0,10,0,10,10,0,10,0,0), ncol = 2, byrow = TRUE)
Pol.1 <- st_polygon(list(MyMatrix.2))
Pol.1
plot(Pol.1)

MyMatrix.3 <- matrix(c(2,2,4,2,4,4,2,4,2,2), ncol = 2, byrow = TRUE)
MyMatrix.4 <- matrix(c(6,6,8,6,8,8,6,8,6,6), ncol = 2, byrow = TRUE)
Pol.2 <- st_polygon(list(MyMatrix.3, MyMatrix.4))
Pol.2
plot(Pol.2)

## Multipart geometries ##
# 4) MULTIPOINT: a set of points
MyMatrix.1
MP.1 <- st_multipoint(MyMatrix.1)
MP.1
plot(MP.1)

# 5) MULTILINESTRING: a set of line strings
MyMatrix.5 <- matrix(c(MyMatrix.1[,1], MyMatrix.1[,2]+1), ncol = 2)
MyMatrix.6 <- matrix(c(MyMatrix.1[,1], MyMatrix.1[,2]+2), ncol = 2)

LS.2 <- st_linestring(MyMatrix.5)
LS.3 <- st_linestring(MyMatrix.6)

MLS.1 <- st_multilinestring(list(LS.1, LS.2, LS.3))
MLS.1
plot(MLS.1)

# 6) MULTIPOLYGON: a set of polygons
MPol.1 <- st_multipolygon(list(Pol.1, Pol.2))
MPol.1
str(MPol.1) # Pol.1 has 1 polygon; Pol.2 has 2 polygons
plot(MPol.1)

# 7) GEOMETRYCOLLECTION: a set of geometries of any of the above types
GC.1 <- st_geometrycollection(list(P.1, LS.1, Pol.1, MP.1, MLS.1, MPol.1))
str(GC.1)
length(GC.1)

NamesGeom <- c("POINT", "LINESTRING", "POLYGON","MULTIPOINT","MULTILINESTRING","MULTIPOLYGON")
par(mfrow=c(2,3))
for(i in 1:length(GC.1)) plot(GC.1[[i]], main = NamesGeom[i], cex.main = 1.5)
par(mfrow=c(1,1))

## Adding attributes to a newly created geometry
# Steps: sfg (with "function st_sfc()") => sfc (with function st_sf()") => sf
# CRS can be set within either st_sfc() or st_sf()
class(P.1)
geometry <- st_sfc(P.1)
DF.1 <- st_sf(V = 1, geometry) # adding attributes
DF.1

class(Pol.1)
geometry <- st_sfc(Pol.1)
DF.2 <- st_sf(V = 10, geometry) # adding attributes
DF.2

class(MPol.1)
geometry <- st_sfc(MPol.1)
DF.3 <- st_sf(V = 100, geometry) # adding attributes to a single MULTIPOLYGON object
DF.3

class(MPol.1)
geometry <- st_cast(st_sfc(MPol.1), "POLYGON")
DF.4 <- st_sf(V = c(100, 200), geometry) # adding attributes to a MULTIPOLYGON object divided into its POLYGON components
DF.4

