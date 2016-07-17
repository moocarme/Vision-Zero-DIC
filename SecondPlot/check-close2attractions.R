library(sp)
library(raster)
library(rgdal)
library(dplyr)
library(rgeos)
library(spatstat)

shp <-'../Vision-Zero/nybb_16b/nybb.shp'
ny <- readOGR(shp, ogrListLayers(shp)[1], stringsAsFactors=FALSE)
map <- spTransform(ny, CRS("+proj=longlat +datum=WGS84"))


num.Rands = 10000
long.Min = -74.025; long.Max = -73.9
lat.Min = 40.695; lat.Max = 40.88
random.Longs = runif(num.Rands, long.Min, long.Max)
random.Lats = runif(num.Rands, lat.Min, lat.Max)

dat <- data.frame(Longitude = random.Longs,
                  Latitude  = random.Lats)
LongLats <- dat
# Assignment modified according
coordinates(dat) <- ~ Longitude + Latitude
# Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
proj4string(dat) <- proj4string(map)

rand.points <- cbind(over(dat, map), LongLats) %>% 
  filter(BoroName == 'Manhattan') %>%
  dplyr::select(Latitude, Longitude)
 
# # Check points lie in manhattan
nycmap <- get_googlemap('Central Park NYC', zoom = 12)
ggmap(nycmap) + geom_point(data = rand.points, aes(x = Longitude, y= Latitude))

# get attractions
tourist_df <- read_csv('../Vision-Zero/TouristAttractions.csv')

# get Vision Zero data
vz_df <- read_csv('../Vision-Zero/VZpersonsInjured.csv')

# get subway data
subway_df <- read_csv('../Vision-Zero/NYC_Transit_Subway_Entrance_And_Exit_Data.csv')
subway_df <- subway_df %>% 
  dplyr::group_by(`Station Name`) %>%
  dplyr::filter(`Station Latitude` == min(`Station Latitude`)) %>%
  dplyr::ungroup() %>%
  dplyr::select(Station = `Station Name`, 
                Lat = `Station Latitude`, 
                Long = `Station Longitude`) %>%
  dplyr::distinct()

# Use approx from http://www.movable-type.co.uk/scripts/latlong.html
ave.km.per.Lat.NY <- 111.2
ave.km.per.Long.NY <- 84.24

tourist_df_lst <- structure(list(lon = tourist_df$Long*ave.km.per.Long.NY, 
                       lat = tourist_df$Lat*ave.km.per.Lat.NY),
                  .Names = c("lon", "lat"), 
                  row.names = c(NA, as.integer(nrow(tourist_df))), 
                  class = "data.frame")

subway_df_lst <- structure(list(lon = subway_df$Long*ave.km.per.Long.NY, 
                                 lat = subway_df$Lat*ave.km.per.Lat.NY),
                            .Names = c("lon", "lat"), 
                            row.names = c(NA, as.integer(nrow(subway_df))), 
                            class = "data.frame")

rand.points_lst <- structure(list(lon = rand.points$Longitude*ave.km.per.Long.NY, 
                       lat = rand.points$Latitude*ave.km.per.Lat.NY),
                  .Names = c("lon", "lat"), row.names = c(NA, nrow(rand.points)), 
                  class = "data.frame")

vz_lst <- structure(list(lon = vz_df$LONGITUDE*ave.km.per.Long.NY, 
                                  lat = vz_df$LATITUDE*ave.km.per.Lat.NY),
                             .Names = c("lon", "lat"), row.names = c(NA, nrow(vz_df)), 
                             class = "data.frame")

tourist_df_sp <- SpatialPoints(tourist_df_lst)
subway_df_sp <- SpatialPoints(subway_df_lst)
rand.points_sp <- SpatialPoints(rand.points_lst)
vz_sp <- SpatialPoints(vz_lst)
rand.points_lst$nearest.tourist.attr <- apply(gDistance(rand.points_sp, 
                                                   tourist_df_sp, byid=TRUE), 2, min)
vz_lst$nearest.tourist.attr <- apply(gDistance(vz_sp, tourist_df_sp, byid=TRUE), 2, min)

rand.points_lst$nearest.subway <- apply(gDistance(rand.points_sp, 
                                                        subway_df_sp, byid=TRUE), 2, min)
vz_lst$nearest.subway <- apply(gDistance(vz_sp, subway_df_sp, byid=TRUE), 2, min)

# Time by 1000 to convert to metres
rand_min_Dists_df <- data.frame(dists = rand.points_lst$nearest.tourist.attr*1000)
vz_min_Dists_df <- data.frame(dists = vz_lst$nearest.tourist.attr*1000)
rand_min_Dists2subway_df <- data.frame(dists = rand.points_lst$nearest.subway*1000)
vz_min_Dists2subway_df <- data.frame(dists = vz_lst$nearest.subway*1000)

mean.Randpts2Attr <- mean(log(rand_min_Dists_df$dists))
sd.Randpts2Attr <- sd(rand_min_Dists_df$dists)
mean.vzpts2Attr <- mean(log(vz_min_Dists_df$dists)) 
sd.vzpts2Attr <- sd(vz_min_Dists_df$dists)

mean.Randpts2subway <- mean(log(rand_min_Dists2subway_df$dists))
sd.Randpts2subway <- sd(rand_min_Dists2subway_df$dists)
mean.vzpts2subway <- mean(log(vz_min_Dists2subway_df$dists)) 
sd.vzpts2subway <- sd(vz_min_Dists2subway_df$dists)

# hist for tourist attractions
plot1 <- ggplot() + 
  geom_histogram(data = log(rand_min_Dists_df), 
                 aes(x = dists, y=..count../sum(..count..), fill= 'green', color = 'green'), 
                 fill = "green", alpha = 0.2, bins = 70) +
  geom_histogram(data = log(vz_min_Dists_df), 
                 aes(x = dists, y=..count../sum(..count..), fill= 'red', color = 'red'), 
                 fill = "red",  alpha = 0.2, bins = 70) + 
  geom_vline(xintercept = (mean.Randpts2Attr), color = 'green') +
  geom_vline(xintercept = (mean.vzpts2Attr), color = 'red') +
  xlim(c(2.5, 9.5)) + labs(list(Title = 'Log Distance to Closest Tourist Attraction'
                              , y = 'Normalized Count', x = 'Log distance(m)')) +
  ylim(c(0, 0.042)) +
    scale_colour_manual(name="group", values=c("red" = "red", "green"="green"), 
                        labels=c("green"="Random Uniform Distribution", "red"="Injured Persons Distribution")) +
    scale_fill_manual(name="group", values=c("red" = "red", "green"="green"), 
                      labels=c("green"="Random Uniform Distribution", "red"="Injured Persons Distribution"))

plot1
plot1_data <- ggplot_build(plot1)

# hist for subway
plot2 <- ggplot() + 
  geom_histogram(data = log(rand_min_Dists2subway_df), 
                 aes(x = dists, y=..count../sum(..count..), fill= 'green', color = 'green'), 
                 fill = "green", alpha = 0.2, bins = 50) +
  geom_histogram(data = log(vz_min_Dists2subway_df), 
                 aes(x = dists, y=..count../sum(..count..), fill= 'red', color = 'red'), 
                 fill = "red",  alpha = 0.2, bins = 50) + 
  geom_vline(xintercept = (mean.Randpts2Attr), color = 'green') +
  geom_vline(xintercept = (mean.vzpts2Attr), color = 'red') +
  xlim(c(0.5, 8)) + labs(list(Title = 'Log Distance to Closest Subway'
                                , y = 'Normalized Count', x = 'Log distance(m)')) +
  # ylim(c(0, 0.09)) +
  scale_colour_manual(name="group", values=c("red" = "red", "green"="green"), 
                      labels=c("green"="Random Uniform Distribution", "red"="Injured Persons Distribution")) +
  scale_fill_manual(name="group", values=c("red" = "red", "green"="green"), 
                    labels=c("green"="Random Uniform Distribution", "red"="Injured Persons Distribution"))
plot2
plot2_data <- ggplot_build(plot2)
write_csv((vz_min_Dists2subway_df), 'vzMinDist2subway.csv')
write_csv((rand_min_Dists2subway_df), 'randMinDist2subway.csv')
write_csv((vz_min_Dists_df), 'vzMinDist.csv')
write_csv((rand_min_Dists_df), 'randMinDist.csv')

t.test(log(rand_min_Dists_df$dists), log(vz_min_Dists_df$dists))
t.test(log(rand_min_Dists2subway_df$dists), log(vz_min_Dists2subway_df$dists))