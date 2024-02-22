Sys.setlocale("LC_ALL", "sv_SE.UTF-8") ## Recognising Swedish/Finnish accents
options(error=NULL)

#---------------------#
# Module 1 (script 2) #
#---------------------#

## Part 1: importing a text file into R
## Part 2: exploring the spatial dataset
## Part 3: exporting the spatial dataset with spatial operations

# Library
library(sf)
library(geodata)

#--------------------------------------#
#### Part 1: Importing spatial data ####
#--------------------------------------#
## Wind farms in Finland
# Data source: https://www.ethawind.com/en/finnish-wind-farms/
# Important points about this dataset:
  # Text file with coordinate (numerical) information 
  # Coordinate Reference System (CRS): "EPSG:4326"

## Setting the working directory
setwd("")

## Reading in the data
# txt. file
?read.table
WFarmData_1 <- read.table("windparkdata.txt", header = TRUE, sep = "\t", fill = TRUE)


#--------------------------------#
#### Part 2: Data exploration ####
#--------------------------------#
## An overview of the dataset
summary(WFarmData_1)
str(WFarmData_1)

# Any duplicated rows?
anyDuplicated.data.frame(WFarmData_1)

# Let's pretend there is duplicated information
WFarmDataDuplicated <- WFarmData_1
WFarmDataDuplicated[352,] <- WFarmDataDuplicated[351,]

anyDuplicated.data.frame(WFarmDataDuplicated)
WFarmDataDuplicated <- WFarmDataDuplicated[-anyDuplicated.data.frame(WFarmDataDuplicated),]

all.equal(WFarmData_1, WFarmDataDuplicated)

## "Cleaning" the dataset
# Let's focus on a few variables
WFarmData_2 <- WFarmData_1[,c("phase","year","turbines_max","latitude","longitude","onshore")]
CompleteInfo <- complete.cases(WFarmData_2[,c("phase","year","turbines_max","latitude","longitude","onshore")])
WFarmData_3 <- WFarmData_2[CompleteInfo,]

# How many wind farms are onshore and offshore?
WFarmData_3$onshore
table(WFarmData_3$onshore)

# Let's focus only on onshore wind farms
WFarmData_4 <- WFarmData_3[WFarmData_3$onshore==1,]

## How many wind farms exist in different development phases?
# 1) Identified Project / Pre-Screening
# 2) Land Use Plan Process Started
# 3) EIA Process Ongoing
# 4) EIA done
# 5) Land Use Plan Proposal
# 6) STR Process Ongoing
# 7) Land Use Plan or STR Done
# 8) Fully Permitted
# 9) Under Construction
# 10) In Production

unique(WFarmData_4$phase)
WFarmData_4 <- WFarmData_4[WFarmData_4$phase!="",] # removing incorrect observations
table(WFarmData_4$phase)

# Subsetting the data: focusing on a few development phases
Index <- WFarmData_4$phase %in% c("Land Use Plan or STR Done","Fully Permitted","Under Construction","In Production")
WFarmData_5 <- WFarmData_4[Index,]

## Frequency of number of turbines
hist(WFarmData_5$turbines_max, main = "", xlab = "Number of turbines")
boxplot(WFarmData_5[,c("turbines_max")], ylab="Number of turbines")

## Frequency of number of turbines per development phase
Un_phases <- unique(WFarmData_5$phase)
Un_phases

boxplot(WFarmData_5$turbines_max ~ WFarmData_5$phase,
        xlab="Development phase",
        ylab="No. of turbines")


#----------------------------------#
#### Part 3: Spatial operations ####
#----------------------------------#
## Coverting numeric coordinates into spatial information
?st_as_sf

colnames(WFarmData_5)
WindFarms_sf <- st_as_sf(WFarmData_5, coords = c("longitude", "latitude")) # the order matters: longitude always comes before latitude
WindFarms_sf

## Setting the Coordinate Reference System (CRS)
st_crs(WindFarms_sf) <- 4326 # a widely used longitude/latitude CRS (this is the CRS I used to create the point coordinates)
st_crs(WindFarms_sf)
WindFarms_sf

## Plotting the spatial data
plot(st_geometry(WindFarms_sf), main="Existing and planned wind farms")

## Plotting wind farms over Finnish regions
# Importing a shapefile of Finland
setwd("")
Finland <- st_read("maakunnat2021_1000000_eimeri.shp")

# Wind farms over Finland
plot(st_geometry(Finland))
plot(st_geometry(WindFarms_sf), add=TRUE) # why are wind farms missing?

# Making sure the CRSs are the same
WindFarms_sf <- st_transform(WindFarms_sf, st_crs(Finland)) # transforming the CRS

plot(st_geometry(WindFarms_sf), add=TRUE)

## How many wind farms per region?
# First of all, the regions need to be joined
Finland

# Joining regions with "st_join()": Uusimaa as an example
Uusimaa <- Finland[Finland$Maaku_ni1=="Uusimaa",]
Uusimaa
plot(st_geometry(Uusimaa))

Uusimaa_union <- st_union(Uusimaa)
Uusimaa_union

# Using the function "aggregate()"
Finland$Maaku_ni1
?aggregate
NFinland <- aggregate(Finland[,5], list(Finland$Maaku_ni1), st_union)

# Where do the points (wind farms) fall?
?st_intersects
MyInter <- st_intersects(NFinland, WindFarms_sf)
MyInter

# Computing the number of wind farms per region
class(MyInter)
View(MyInter)

MLength <- lengths(MyInter)
MLength

# Adding a variable for the number of wind farms per region
NFinland$WFarms <- MLength
plot(NFinland[,"WFarms"], main="Wind farms per region")


#----------------------#
# Final considerations #
#----------------------#
## Functions (from library sf) used in this script
?st_as_sf
?st_geometry
?st_crs
?st_transform
?aggregate
?st_intersects
?st_union
