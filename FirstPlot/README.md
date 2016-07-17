# Examining the relationship between number of lanes on streets and the mean number of injuries that occur

I hypothesize that the number of traffic-related injuries may be determined by features of the 
street on which they occur. One example is the number of lanes the road has.

I decided to explore this relationship directly by joining two datasets, the NYPD motor collision dataset,
which can be found [here](https://data.cityofnewyork.us/Public-Safety/NYPD-Motor-Vehicle-Collisions/h9gi-nx95), to obtain 
the number of persons injured on a given street, and extracting the pertinent information 
from the New York City LION street geojson file, that can be found [here](http://www1.nyc.gov/site/planning/data-maps/open-data/dwn-lion.page), which contains properties of the streets, 
including the number of lanes. From these two datasets I was able to obtain the average number 
of injuries on streets with a given number of lanes.

I find that, in general, increasing the number of lanes increases the number of injuries.
 Except for one lane roads, in which a disproportionately large amount of injuries occur. 
 
Vision-Zero-Steet-Analysis.R uses the NYPD motor vehicle collisions dataset and the lion geojson file, joins the datasets and extracts the relationship between the number of lanes and the mean number of injuries.

Rmd and html files are also generated so that the static plots can be displayed easily on the web
 
