# This R script is written in the context of the Digital Methods Summer School 2015
# The goal of this script is to translate longitude and latitude toward country names
# A second goal is to map country values on a map

# [[PROJECT AND FILE SPECIFIC SCRIPT - USE THIS AS AN INSPIRATION]]

####################################################################################
####################################################################################

## READING THE DATA (tab seperated)

data <- read.csv("lovewins_instagram_100_iterations_media.csv", sep ="\t")

## ADDING TWO EMPTY VARIABLES TO THE DATA

data$latitude = NA
data$longitude = NA

## IF THE DATA IS MISSING - KEEP THE NEW VARIABLES EMPTY
## IF THE DATA IS NOT MISSING - SPLIT THE LOCATION VARIABLE IN LONGITUDE AND LATITUDE

for (i in 1:nrow(data)){
        if (data$location[i] == ""){
             data$latitude[i] = ""
             data$longitude[i] = ""
        }
        else {
                data$longitude[i] = strsplit(as.character(data$location[i]), ", ")[[1]][1]
                data$latitude[i] = strsplit(as.character(data$location[i]), ", ")[[1]][2]
        }
}

## CREATE A POINT OBJECT TO PROCESS THE TRANSLATION AND ADD THE ID SO WE CAN MERGE IN THE END
         
points <- data[, c(13,14,1)]
points$longitude <- as.numeric(points$longitude)
points$latitude <- as.numeric(points$latitude)
points <- points[complete.cases(points),]

## PROCESSING THE POINTS OBJECT - TRANSLATION OF LONG AND LAT TO COUNTRY

if("sp" %in% rownames(installed.packages()) == FALSE) {install.packages("sp")}
if("rworldmap" %in% rownames(installed.packages()) == FALSE) {install.packages("rworldmap")}
library(sp)
library(rworldmap)

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees

coords2country = function(points)
{  
        countriesSP <- getMap(resolution='low')
        #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
        
        # convert our list of points to a SpatialPoints object
        
        # pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
        
        #setting CRS directly to that from rworldmap
        pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
        
        # use 'over' to get indices of the Polygons object containing each point 
        indices = over(pointsSP, countriesSP)
        
        # return the ADMIN names of each country
        indices$ADMIN  
        #indices$ISO3 # returns the ISO3 code 
        #indices$continent   # returns the continent (6 continent model)
        #indices$REGION   # returns the continent (7 continent model)
}

new.data <- data.frame(id = points$id, country = as.character(coords2country(points[,1:2])))

## CREATE A SUMMARY FOR THE PLOT

summary <- as.data.frame(table(new.data$country))

## PLOT 1 -- USING MAPCOUNTRYDATA

        #create a map-shaped window

        mapDevice('x11')

        #join to a coarse resolution map

        spdf <- joinCountryData2Map(summary, joinCode="NAME", nameJoinColumn="Var1")
        
        mapCountryData(spdf, nameColumnToPlot="Freq", catMethod="fixedWidth")

## PLOT 2 -- USING THE GOOGLE API

        G1<- gvisGeoMap(summary, "Var1", "Freq",  options=list(dataMode="regions", width=600, height=300))
        
        plot(G1)
