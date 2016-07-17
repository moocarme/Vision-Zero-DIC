library(rgdal)
library(geojsonio)
library(sp)
library(rgdal)
library(rgeos)
library(ggplot2)
library(ggthemes)
library(gdata)
library(jsonlite)
library(leaflet)

# fils<-0
# fils[1] <- '../Vision-Zero/nybb_16b/nybb.shp'
# nyc <- readOGR(fils[1], ogrListLayers(fils[1])[1], stringsAsFactors=FALSE)
# plot(nyc, lwd=0.5, asp=1)

# fils[2] <- '../Vision-Zero/speed_limit_shapefile_WGS/speed_limit_WGS.shp'
# nyc2 <- readOGR(fils[2], ogrListLayers(fils[2])[1], stringsAsFactors=FALSE)
# plot(nyc2, lwd=0.5, asp=1)

# =======================================================

# Data munging - geojson -> dataframe ================
Lion <- geojson_read('../Vision-Zero/LionStreet.geojson')
Lion[["features"]][[3]][[2]][["Street"]]

# json format to dataframe where columns are the varios features
steet1 <- lapply(Lion$features, function(x) {
x[sapply(x[[2]], is.null)] <- NA
unlist(x[[2]])
})

streetdf <- do.call('rbind', steet1)
streetdf2 <-as.data.frame(apply(streetdf,2,function(x) trim(x)))
# ===================================================

# Data reduction
streetdf2_red <- streetdf2 %>%
  select(Street, NumLanes = Number_Total_Lanes, 
         TravLanes = Number_Travel_Lanes) %>%
  distinct() %>%
  mutate(Street = as.character(Street), 
         NumLanes = as.numeric(NumLanes),
         TravLanes = as.numeric(TravLanes),
         ParkLanes = as.numeric(ParkLanes))

# Get vision zero data
vzData <- read_csv('../../Vision-Zero-Analysis/data/NYPD_Motor_Vehicle_Collisions.csv')

# Data reduction, cleaning and joining
vzData_red <- vzData %>%
  select(Street = `ON STREET NAME`, 
         NumInj = `NUMBER OF PERSONS INJURED`) %>%
  filter(!is.na(Street)) %>%
  group_by(Street) %>%
  summarise(Tot.Inj = sum(NumInj)) %>%
  arrange(desc(Tot.Inj))

tjoin <- vzData_red %>%
  inner_join(streetdf2_red, by = 'Street') %>%
  group_by(Street) %>%
  filter(NumLanes == max(NumLanes))

tjoin2 <- tjoin %>%
  group_by(TravLanes) %>%
  summarise(meanInj = mean(Tot.Inj), TotStreets = n()) %>%
  mutate(AveInj.perStreet = meanInj/TotStreets)

# Calculate t-test
t.test(tjoin2$TravLanes, tjoin2$meanInj)

# Plot with polynomial fit 
plot_lanes <- ggplot(data = tjoin2, aes(x = TravLanes, y = meanInj)) + 
  geom_point(aes(color = meanInj)) + 
  geom_smooth(method = lm, formula = y ~ x) +
  scale_color_gradient(low = 'red', high= 'yellow')

plot_lanes
plot_lanes_data <- ggplot_build(plot_lanes)

# write to csv
write_csv(tjoin2, 'NumLanesMeanInj.csv')

# Get data for determining close proximity to subway
vzLatLong <- vzData %>%
  filter( `NUMBER OF PERSONS INJURED` > 0, BOROUGH == 'MANHATTAN') %>%
  select(LATITUDE, LONGITUDE,
         NumInj = `NUMBER OF PERSONS INJURED`) 

# write to csv
write.csv(vzLatLong, 'VZpersonsInjured.csv')


# # Need to join more streets together, only around 800 common 
# streetNamesVZ <- unique(vzData$`ON STREET NAME`)
# streetNamesSDF <- unique(streetdf2$Street)
# 
# length(dplyr::intersect(streetNamesVZ, streetNamesSDF))
