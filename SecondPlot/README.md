# Examining the relationship between distance from tourist attractions and subways and the number of injuries

I hypothesize that there are more injuries in places where pedestrians congregate. 2 examples of these in New York City are around tourist attractions such as museums and subway stops. As a preliminary analysis I look at the density of traffic injuries in Manhattan, taken from the NYPD motor collisions dataset which can be found [here](https://data.cityofnewyork.us/Public-Safety/NYPD-Motor-Vehicle-Collisions/h9gi-nx95), at points with 3 or more injuries. On this overlay, the locations of subway stations that can be found [here](https://data.ny.gov/Transportation/NYC-Transit-Subway-Entrance-And-Exit-Data/i9wp-a4ja), and popular tourist attractions using the [Google Places API](https://developers.google.com/places/).

I find that in general, moreinjuries occur close to subway stops and tourist attractions in a manner that is statistically significant compared to a random uniform distribution of locations in New York City.

CSV files are small reduced datasets useful for retrieving plots quickly, and for use between file

get-Tourist-Attractions.R uses the google places API to get the locations of tourist attractions in NYC and outputs them to the file TouristAttractions.csv

check-close2attraction.R measure the distance from vision zero injuries to the closest subway and tourist attractions, and creates histogram plots of the distributions. csv files are also written to add to the app

plotHists_minDist2subway_tourist folder contains the data and app file to create the shiny app that switches between subway and tourist attraction distributions.