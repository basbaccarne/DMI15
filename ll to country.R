# This R script is written in the context of the Digital Methods Summer School 2015
# The goal of this script is to translate longitude and latitude toward country names
# A second goal is to map country values on a map

# [[PROJECT AND FILE SPECIFIC SCRIPT - USE THIS AS AN INSPIRATION]]

####################################################################################
####################################################################################

## READING THE DATA (tab seperated)

import <- read.csv("THE DATA.csv")

import$timeID <- as.Date(as.POSIXct(as.numeric(as.character(import$timestamp)), origin = "1970-01-01", tz = "GMT"))

for (i in 1:nrow(import)){
        if(is.na(import$timeID[i])){
                import$timeID[i] <- strptime(import$timestamp[i], "%m/%e/%Y %H:%M:%S")
        }      
}

for (i in 1:nrow(import)){
        if(is.na(import$timeID[i])){
                import$timeID[i] <- strptime(import$timestamp[i], "%e/%m/%Y %H:%M")
        }      
}

## ADDING TWO EMPTY VARIABLES TO THE DATA

import$lat = NA
import$long = NA

## REMOVE ALL MISSING DATA

data <- import[complete.cases(import$longitude),]
data <- data[!data$longitude=="",]

## IF THE DATA IS NOT MISSING - SPLIT THE LOCATION VARIABLE IN LONGITUDE AND LATITUDE

for (i in 1:nrow(data)){
                data$lat[i] = strsplit(as.character(data$longitude[i]), ", ")[[1]][2]
                data$long[i] = strsplit(as.character(data$longitude[i]), ", ")[[1]][1]
}

## CREATE A POINT OBJECT TO PROCESS THE TRANSLATION AND ADD THE ID SO WE CAN MERGE IN THE END
      
points <- data[,c(13,14,1,2,3,4,5,6,7,8,9,10,11,12)]
points$lat <- as.numeric(points$lat)
points$long <- as.numeric(points$long)

data <- data[-which(is.na(points$lat)),]
points <- points[complete.cases(points$lat),]

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

map.data <- as.character(coords2country(as.matrix(points[,1:2])))
data$country <- map.data

## CREATE A SUMMARY FOR THE PLOT

data_1 <- data[data$timeID<"2015-06-26" & data$timeID>"2015-06-24",]
data_2 <- data[data$timeID<"2015-06-27" & data$timeID>"2015-06-24",]
data_3 <- data[data$timeID<"2015-06-28" & data$timeID>"2015-06-24",]
data_4 <- data[data$timeID<"2015-06-29" & data$timeID>"2015-06-24",]
data_5 <- data[data$timeID<"2015-06-30" & data$timeID>"2015-06-24",]
data_6 <- data[data$timeID<"2015-07-01" & data$timeID>"2015-06-24",]
data_7 <- data[data$timeID<"2015-07-02" & data$timeID>"2015-06-24",]
data_8 <- data[data$timeID<"2015-07-03" & data$timeID>"2015-06-24",]

summary_1 <- as.data.frame(table(data_1$country))
summary_2 <- as.data.frame(table(data_2$country))
summary_3 <- as.data.frame(table(data_3$country))
summary_4 <- as.data.frame(table(data_4$country))
summary_5 <- as.data.frame(table(data_5$country))
summary_6 <- as.data.frame(table(data_6$country))
summary_7 <- as.data.frame(table(data_7$country))
summary_8 <- as.data.frame(table(data_8$country))


## PLOT 1 -- USING MAPCOUNTRYDATA

        #create a map-shaped window

        mapDevice('x11')
        spdf <- joinCountryData2Map(summary_1, joinCode="NAME", nameJoinColumn="Var1")
        mapCountryData(spdf, nameColumnToPlot="Freq", catMethod="fixedWidth")

        mapDevice('x11')
        spdf <- joinCountryData2Map(summary_2, joinCode="NAME", nameJoinColumn="Var1")
        mapCountryData(spdf, nameColumnToPlot="Freq", catMethod="fixedWidth")

        mapDevice('x11')
        spdf <- joinCountryData2Map(summary_3, joinCode="NAME", nameJoinColumn="Var1")
        mapCountryData(spdf, nameColumnToPlot="Freq", catMethod="fixedWidth")
        
        mapDevice('x11')
        spdf <- joinCountryData2Map(summary_4, joinCode="NAME", nameJoinColumn="Var1")
        mapCountryData(spdf, nameColumnToPlot="Freq", catMethod="fixedWidth")
        
        mapDevice('x11')
        spdf <- joinCountryData2Map(summary_5, joinCode="NAME", nameJoinColumn="Var1")
        mapCountryData(spdf, nameColumnToPlot="Freq", catMethod="fixedWidth")
        
        mapDevice('x11')
        spdf <- joinCountryData2Map(summary_6, joinCode="NAME", nameJoinColumn="Var1")
        mapCountryData(spdf, nameColumnToPlot="Freq", catMethod="fixedWidth")
        
        mapDevice('x11')
        spdf <- joinCountryData2Map(summary_7, joinCode="NAME", nameJoinColumn="Var1")
        mapCountryData(spdf, nameColumnToPlot="Freq", catMethod="fixedWidth")
        
        mapDevice('x11')
        spdf <- joinCountryData2Map(summary_8, joinCode="NAME", nameJoinColumn="Var1")
        mapCountryData(spdf, nameColumnToPlot="Freq", catMethod="fixedWidth")


## PLOT 2 -- USING THE GOOGLE API

        G1<- gvisGeoMap(summary, "Var1", "Freq",  options=list(dataMode="regions", width=600, height=300))
        
        plot(G1)

## EXPORT FOR FUSION TABLES

for (i in 1:nrow(data)){
        if(data$stance[i]=="anti-program"){
                data$stancecolor[i] <- "small_blue"   
        } else if (data$stance[i]=="program"){
                data$stancecolor[i] <- "small_green"  
        }
}

for (i in 1:nrow(data)){
        if(data$query[i]=="loveloses"){
                data$querycolor[i] <- "small_yellow"   
        } else if (data$query[i]=="jesuswins"){
                data$querycolor[i] <- "small_purple"  
        }
}


write.csv(data, "GEOoutput.csv")
