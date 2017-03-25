########################################################################
#
#
# Smart Cities hack
#
# 
#
########################################################################

library(ggplot2)
library(lattice)
library(sqldf)
library(tidyr)

setwd("/Users/Anika/Documents/R/SmartCities") #set working directory
getwd()                           #print the working directory

#install.packages('devtools')
devtools::install_github("rstats-db/bigrquery")

library(bigrquery)

projectid <- "spheric-crow-161317"

query <- "SELECT
  TIMESTAMP_TRUNC(pickup_datetime,
    MONTH) month,
  COUNT(*) trips
FROM
  `smart_cities_data.median_rents_neighborhood_studio`
GROUP BY
  1
ORDER BY
  1"

query_studio <- "SELECT * 
FROM 
  `smart_cities_data.median_rents_zipcode_studio`
"

df_studio = query_exec(query_studio, projectid, useLegacySql = FALSE)
head(df_studio)
nrow(df_studio) #87

df_studiony <- df_studio[(df_studio$State=="NY"),]
unique(df_studiony$State) #NY
unique(df_studiony$CountyName)

colnames(df_studiony) <- c("RegionName", "City", "State", "Metro", "CountyName", "SizeRank", "2011_01", "2011_02", "2011_03", 
                           "2011_04", "2011_05", "2011_06", "2011_07", "2011_08", "2011_09", "2011_10", "2011_11", "2011_12", 
                           "2012_01", "2012_02", "2012_03", "2012_04", "2012_05", "2012_06", "2012_07", "2012_08", "2012_09",
                           "2012_10", "2012_11", "2012_12", "2013_01", "2013_02", "2013_03", "2013_04", "2013_05", "2013_06",
                           "2013_07", "2013_08", "2013_09", "2013_10", "2013_11", "2013_12", "2014_01", "2014_02", "2014_03",
                           "2014_04", "2014_05", "2014_06", "2014_07", "2014_08", "2014_09", "2014_10", "2014_11", "2014_12",
                           "2015_01", "2015_02", "2015_03", "2015_04", "2015_05", "2015_06", "2015_07", "2015_08", "2015_09",
                           "2015_10", "2015_11", "2015_12", "2016_01", "2016_02", "2016_03", "2016_04", "2016_05", "2016_06",
                           "2016_07", "2016_08", "2016_09", "2016_10", "2016_11", "2016_12", "2017_01")

head(df_studiony)
data_long <- gather(df_studiony, CountyName, SizeRank, "2011_01":"2017_01", factor_key=TRUE)
data_long


###### plotting
plot(df_studiony$SizeRank, as.factor(df_studiony$CountyName))




####### GIS ############
#install.packages("rgdal")
#install.packages("leaflet")
#install.packages("sp")
library(rgdal)
library(leaflet)
library(sp)
"GeoJSON" %in% ogrDrivers()$name

map = readOGR("ParksProperties.geojson", "OGRGeoJSON")
plot(map)
proj4string(map) #WGS84

# Make 2263 projection
EPSG <- make_EPSG()
NY <- with(EPSG,EPSG[grepl("New York",note) & code==2263,]$prj4)
NY

# try a reproject method
map.df <- fortify(map)
rats <- SpatialPoints(data[,c("longitude","latitude")],proj4string=CRS(wgs.84))
rats <- spTransform(rats,CRS(NY))
rats.df <- data.frame(coordinates(rats))
ggplot(map.df, aes(x=long,y=lat,group=group))+
  geom_path()+
  geom_point(data=rats.df, aes(x=longitude,y=latitude, group=NULL),
             colour="red", alpha=0.2, size=1)+
  ggtitle("Projection: NAD83.2263")+
  coord_fixed()

# set color scheme
pal <- colorNumeric("viridis", NULL)

nycounties <- map

leaflet(nycounties) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~pal(log10(pop)),
              label = ~paste0(county, ": ", formatC(pop, big.mark = ","))) %>%
  addLegend(pal = pal, values = ~log10(pop), opacity = 1.0,
            labFormat = labelFormat(transform = function(x) round(10^x)))

######## Gis different version ######
r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
map = readOGR("ParksProperties.geojson", "OGRGeoJSON")
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

nyc_parks <- readOGR("ParksProperties.geojson", 'OGRGeoJSON', verbose = F)

# Show the first 10 rows
head(nyc_parks@data, 10)
head(map@data, 10)

# leaflet
leaflet(nyc_neighborhoods) %>%
  addTiles() %>% 
  addPolygons(popup = ~nieghborhoods) %>%
  addProviderTiles("CartoDB.Positron")

# makes pretty map of parks!
leaflet() %>% 
  addTiles() %>%
  setView(lng = -73.98, lat = 40.75, zoom = 13) %>% 
  addPolygons(data = map, popup = ~acres) %>%
  addProviderTiles("CartoDB.Positron")

###### park data as data frame
map_df <- as.data.frame(map@data)
nrow(map_df)
summary(map_df)

hist(as.numeric(map_df$acres))
boxplot(as.numeric(map_df$acres), map_df$waterfront)
boxplot(map_df$typecatego, as.numeric(map_df$acres))

plot(map_df$typecatego,as.numeric(map_df$acres), xlab = "type", ylab = "acres", pch = 19, col = rgb(0, 0, 0, 0.1))

# boxplots of acerage by park type
ggplot(map_df, aes(x=typecatego, y=as.numeric(acres))) +
  geom_boxplot() + theme(axis.text.x=element_text(hjust=0, angle=90)) + 
  xlab("Park Type") + ylab("Area (acres)") + labs(title="Park area by type") +
  theme(plot.title = element_text(hjust = 0.5))
