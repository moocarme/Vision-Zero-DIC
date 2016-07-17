library(RCurl)
library(RJSONIO)
library(plyr)
library(ggmap)

url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    t1 <- lapply(x$results, function(x) {
      x[sapply(x, is.null)] <- NA
      unlist(x)
    })
    t2 <- do.call('rbind', t1)
    return(c(lat, lng, location_type, formatted_address, t2))
  } else {
    return(c(NA,NA,NA, NA))
  }
}

address <- geoCode("NYC tourist attractions")
address

# ====================


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

coordsCP <-c(40.7829, -73.9654)
T2<-encontrar(lugar = coordsCP,radius = 5000,"Tourist")
Museum <- encontrar(lugar = coordsCP,radius = 5000,"Museum")

map <- get_googlemap('Central Park NYC', zoom = 12)
ggmap(map) + geom_point(data = Museum, aes(x = Long, y= Lat)) + 
  geom_point(data = TouristAttractions, aes(x = Long, y= Lat, color = 'red')) + 
  geom_point(data = T2, aes(x = Long, y= Lat, color = 'green'))

coordsDT <-c(40.725, -74.00)
T2DT<-encontrar(lugar = coordsDT,radius = 2000,"Tourist")
MuseumDT <- encontrar(lugar = coordsDT,radius = 2000,"Museum")

ggmap(map) + geom_point(data = MuseumDT, aes(x = Long, y= Lat)) + 
  geom_point(data = T2DT, aes(x = Long, y= Lat, color = 'red')) 

totdf<-rbind(T2DT, MuseumDT, T2, TouristAttractions, Museum) 
totdf <- distinct(totdf)
write.csv(totdf, 'TouristAttractions.csv')

