library(RCurl)
library(RJSONIO)
library(plyr)
library(ggmap)

# Function to list of businesses from Google place API
encontrar<-function(lugar,radius,keyword){
  
  # radius in meters
  #lugar is coordinates from google maps by hand
  coor<-paste(lugar[1],lugar[2],sep=",")
  baseurl<-"https://maps.googleapis.com/maps/api/place/radarsearch/json?"
  google_key<-c("AIzaSyAsMMnXeilWUUnryc32NVIpGV8wstZcbzA")
  
  q<-paste(baseurl,"location=",coor,"&radius=",radius,"&keyword=",keyword,"&key=",google_key, sep="")
  print(q)
  
  data1<-fromJSON(q)
  
  
  lat <- as.data.frame(sapply(data1$results, function(x) {x$geometry$location[1]}))
  long <- as.data.frame(sapply(data1$results, function(x) {x$geometry$location[2]}))
  df <- cbind(lat, long)
  colnames(df) <- c('Lat', 'Long')
  return(df)
}

# Coordinates of central park, geographical center of New York
rad <- 5000 # 5000m radius
coordsCP <-c(40.7829, -73.9654)
T2<-encontrar(lugar = coordsCP,radius = rad,"Tourist")
Museum <- encontrar(lugar = coordsCP,radius = rad,"Museum")

# Check on map
map <- get_googlemap('Central Park NYC', zoom = 12)
ggmap(map) + geom_point(data = Museum, aes(x = Long, y= Lat)) + 
  geom_point(data = TouristAttractions, aes(x = Long, y= Lat, color = 'red')) + 
  geom_point(data = T2, aes(x = Long, y= Lat, color = 'green'))

# Get coordinates from downtown
coordsDT <-c(40.725, -74.00)
# Get Tourist locations and museums downtown
rad2 <- 2000 # 2000m
T2DT<-encontrar(lugar = coordsDT, radius = rad2, "Tourist")
MuseumDT <- encontrar(lugar = coordsDT,radius = rad2, "Museum")

# Check on map
ggmap(map) + geom_point(data = MuseumDT, aes(x = Long, y= Lat)) + 
  geom_point(data = T2DT, aes(x = Long, y= Lat, color = 'red')) 

# Combine all data, remove repeats and write to csv file 
totdf<-rbind(T2DT, MuseumDT, T2, TouristAttractions, Museum) 
totdf <- distinct(totdf)
write.csv(totdf, 'TouristAttractions.csv')

