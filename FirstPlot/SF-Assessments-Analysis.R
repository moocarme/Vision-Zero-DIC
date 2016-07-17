library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

sf_data <- read_csv('data/Historic_Secured_Property_Tax_Rolls.csv') 
str(sf_data)

# Most common class
common <- sf_data %>%
  select(class = `Property Class Code`) %>%
  mutate(nrows = length(class)) %>%
  group_by(class) %>%
  summarise(fraction = n()/nrows[1]) %>%
  arrange(desc(fraction))
sprintf("%.10f", common[1,2])
# 0.4707253227

# Median improvement value
# Using fiscal year to determine last entry since both involve "Closed Roll"

medImprov <- sf_data %>%
  select(BLNum = `Block and Lot Number`, 
         ImprovVal = `Closed Roll Assessed Improvement Value`,
         Year = `Closed Roll Fiscal Year`) %>%
  filter(ImprovVal != 0 & !is.na(ImprovVal)) %>%
  group_by(BLNum) %>%
  filter(Year == max(Year)) %>%
  ungroup() %>%
  summarise(median = median(ImprovVal))
sprintf("%.10f", medImprov$median)
# 209161.0000000000

# Mean improvement value per neighbourhood
mean_neighborhood_Improv <- sf_data %>%
  select(BLNum = `Block and Lot Number`, 
         ImprovVal = `Closed Roll Assessed Improvement Value`,
         Year = `Closed Roll Fiscal Year`,
         Neighborhood = `Neighborhood Code`) %>%
  filter(ImprovVal != 0 & !is.na(ImprovVal)) %>%
  group_by(BLNum) %>%
  filter(Year == max(Year)) %>%
  ungroup() %>%
  group_by(Neighborhood) %>%
  summarise(Neighborhood.mean = mean(ImprovVal)) %>%
  arrange(Neighborhood.mean)
  
sprintf("%.10f", mean_neighborhood_Improv[nrow(mean_neighborhood_Improv), "Neighborhood.mean"] 
        - mean_neighborhood_Improv[1, "Neighborhood.mean"])
# 12611913.1424464472

# Largest area neighbourhood ==================================
# Calculate average change in lat Long in kms 

# Helper functions =========================

LatLong2km <- function(lat1, long1, lat2, long2){
  R = 6373 # radius of earth in kms
  dlong = long2 - long1
  dlat = lat2 - lat1
  a = (sin(dlat/2))^2 + cos(lat1) * cos(lat2) * (sin(dlong/2))^2
  c = 2 * atan2( sqrt(a), sqrt(1-a) )
  d = R * c
  return(d)
}

ellipseArea <- function(semiMajor, semiMinor) return(pi*semiMajor*semiMinor)
# Convert degrees to radians
deg2rad <- function(deg) return(deg*pi/180)
rad2deg <- function(rad) return(rad*180/pi)

# ================

Lat_Long <- sf_data %>%
  select(Location) %>% 
  separate(Location, into =c('Lat', 'Long'), sep = ',') %>%
  mutate(Lat = as.numeric(gsub('\\(','', Lat)), 
         Long = as.numeric(gsub('\\)','', Long)))
maxLong = deg2rad(max(Lat_Long$Long, na.rm = T))
minLong = deg2rad(min(Lat_Long$Long, na.rm = T))
maxLat = deg2rad(max(Lat_Long$Lat, na.rm = T))
minLat = deg2rad(min(Lat_Long$Lat, na.rm = T))

# Calculate average number of kms per lat and long in the area we're interested in
ave.km.per.Lat <- LatLong2km(maxLat, (maxLong+minLong)/2, minLat, (maxLong+minLong)/2)/
  (rad2deg(maxLat)-rad2deg(minLat))
ave.km.per.Long <- LatLong2km((maxLat+minLat)/2, maxLong, (maxLat+minLat)/2, minLong)/
  (rad2deg(maxLong)-rad2deg(minLong))

NeighborhoodAreas <- sf_data %>%
  select(BLNum = `Block and Lot Number`,
         Neighborhood = `Neighborhood Code`, 
         Year = `Closed Roll Fiscal Year`, Location) %>%
  group_by(BLNum) %>%
  filter(Year == max(Year)) %>%
  ungroup() %>%
  filter(!is.na(Location) & !is.na(Neighborhood)) %>%
  separate(Location, into = c('Lat', 'Long'), sep = ',') %>%
  mutate(Lat = as.numeric(gsub('\\(','', Lat)), 
         Long = as.numeric(gsub('\\)','', Long))) %>%
  group_by(Neighborhood) %>%
  summarise(sd.Lat = sd(Lat), sd.Long = sd(Long)) %>%
  mutate(sd.Lat.km = ave.km.per.Lat*sd.Lat, 
         sd.Long.km = ave.km.per.Long*sd.Long,
         Area = ellipseArea(sd.Long.km, sd.Lat.km)) %>%
  arrange(desc(Area))
sprintf("%.10f",NeighborhoodAreas$Area[1])
# 3.0346293784

# Land Values
landVal <- sf_data %>% 
  select(Year = `Closed Roll Fiscal Year`,
         Land.Value = `Closed Roll Assessed Land Value`) %>%
  filter(!is.na(Year)) %>%
  group_by(Year) %>%
  summarise(mean.Land.Value = mean(Land.Value)) %>%
  arrange(Year) %>%
  mutate(log.Price.change = log(mean.Land.Value/mean.Land.Value[1]),
         YearInc = Year-Year[1]) 
  
lmfit <- lm(landVal$log.Price.change ~ landVal$YearInc -1) # -1 forces though the fit through the origin
sprintf("%.10f",lmfit$coefficients)
# 0.0585460672

# What is the difference between the average number of units in buildings 
# build in or after 1950, and that for buildings built before 1950? Consider 
# only buildings that have non-zero units reported, and ignore buildings with 
# obviously incorrect years. To try to avoid the effect of improvements to 
# buildings, use the earliest record for each property, not the latest.

minYear = 1800
maxYear = year(Sys.Date())
build_units<- sf_data %>% 
  select(BLNum = `Block and Lot Number`,
         Year = `Closed Roll Fiscal Year`,
         Year.Built = `Year Property Built`,
         Units = `Number of Units`) %>%
  filter(Units > 0 & !is.na(Year) & Year.Built > minYear & 
           Year.Built < maxYear) %>%
  group_by(BLNum) %>%
  filter(Year == min(Year)) %>%
  ungroup() %>%
  mutate(BeforeAfter1950 = ifelse(Year.Built>= 1950, 'Post-1950', 'Pre-1950')) %>%
  group_by(BeforeAfter1950) %>%
  summarise(mean.Units = mean(Units))

# a few have over 4000 units, 1930 market st - seem to be large office/ commercial buildings
sprintf("%.10f", build_units[build_units$BeforeAfter1950 == 'Post-1950', 2] - 
          build_units[build_units$BeforeAfter1950 == 'Pre-1950', 2])
# 0.5389125439

# Considering only properties with non-zero numbers of bedrooms and units, 
# calculate the average number of bedrooms per unit in each zip code. Use 
# the ratio of the means instead of the mean of the ratio. What is this ratio 
# in the zip code where it achieves its maximum? 

aveBeds_zip <- sf_data %>%
  select(zip = `Zipcode of Parcel`,
         NumBeds = `Number of Bedrooms`, 
         NumUnits = `Number of Units`) %>%
  filter(NumBeds > 0 & NumUnits > 0) %>%
  group_by(zip) %>%
  summarise(mean.Beds = mean(NumBeds), 
            mean.Units = mean(NumUnits)) %>%
  mutate(Bed.Unit.Ratio = mean.Beds/mean.Units) %>%
  arrange(desc(Bed.Unit.Ratio))
sprintf("%.10f", aveBeds_zip[1,'Bed.Unit.Ratio'])
# 3.8768601648 # seems a little high

# Estimate how built-up each zip code is by comparing the total property area 
# to the total lot area. What is the largest ratio of property area to surface 
# area of all zip codes? 

builtup_zip <- sf_data %>%
  select(zip = `Zipcode of Parcel`, 
         Prop.Area = `Property Area in Square Feet`, 
         Lot.Area = `Lot Area`) %>%
  filter(Lot.Area > 1 & Prop.Area > 1 & # some lots were less that 1 sqft
           !is.na(Lot.Area), !is.na(zip)) %>%
  mutate(Prop.Lot.Ratio = Prop.Area/Lot.Area) %>%
  group_by(zip) %>%
  summarise(mean.Lot.Area = mean(Lot.Area), 
            mean.Prop.Area = mean(Prop.Area)) %>%
  mutate(Prop.Lot.Ratio = mean.Prop.Area/mean.Lot.Area) %>%
  arrange(desc(Prop.Lot.Ratio))

sprintf("%.10f", builtup_zip[1,'Prop.Lot.Ratio'])
# 13.5792093146
# 94104 - finiancial district sf - small zip code with high number of skyscrapers
# others in top 3 also include financial district but larger area