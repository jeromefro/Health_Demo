library(rvest)
library(dplyr)
library(ggmap)

url <- "http://www.cancer.gov/researchandfunding/extramural/cancercenters/find-a-cancer-center"
webpage <- html(url)

centerNames <- webpage %>% html_nodes("#institution-listing2 a span") %>% html_text()

centerNames

coordinates <- sapply(centerNames, geocode)
coordinates.2 <- matrix(unlist(coordinates), nrow = 68, byrow = TRUE)
sum(complete.cases(coordinates.2))

centerNames[!complete.cases(coordinates.2)]
addressMissing <- c("4501 X Street, Suite 3003 Sacramento, California 95817",
                    "10010 North Torrey Pines Road La Jolla, California 92037",
                    "200 Hawkins Drive 5970Z JPP Iowa City, Iowa 52242", 
                    "600 Main Street Bar Harbor, Maine 04609", 
                    "450 Brookline Avenue Boston, Massachusetts 02215", 
                    "660 South Euclid Avenue Campus Box 8109 St. Louis, Missouri 63110", 
                    "85950 Nebraska Medical Center Omaha, Nebraska 68198", 
                    "One Medical Center Drive Lebanon, New Hampshire 03756", 
                    "550 First Avenue 1201 Smilow Building New York, New York 10016", 
                    "1 Bungtown Road Cold Spring Harbor, New York 11724", 
                    "Medical Center Boulevard Winston-Salem, North Carolina 27157", 
                    "11100 Euclid Avenue, Wearn 151 Cleveland, Ohio 44106",
                    "300 West 10th Avenue, Suite 159 Columbus, Ohio 43210", 
                    "3601 Spruce Street Philadelphia, Pennsylvania 19104", 
                    "One Baylor Place MS: BCM305 Houston, Texas 77030",
                    "P.O. Box 19024, D1-060 Seattle, Washington 98109")

coordinatesMissing <- sapply(addressMissing, geocode)
coordinatesMissing.2 <- matrix(unlist(coordinatesMissing), nrow = 16, byrow = TRUE)
coordinatesMissing.2

lonLat <- coordinates.2
lonLat[!complete.cases(lonLat)] <- coordinatesMissing.2
lonLat

lonLat <- data.frame(centerNames, lonLat)
names(lonLat) <- c("Name", "lon", "lat")

library(ggplot2)
library(maps)

load("./Data/usMap.rda")
state_map <- map_data("state")
ggplot(data = state_map, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), color = "grey40", size = 0.5) +
  geom_point(data = lonLat[lonLat$Name != "University of Hawaii Cancer Center",],
             aes(x = lon, y = lat), color = "red")

badCoordinates <- lonLat$lon > 0 | lonLat$lon < -150
sum(badCoordinates)

centerNames[badCoordinates]
badNames <- c("Sidney Kimmel Cancer Center at Thomas Jefferson University", 
              "Cancer Therapy & Research Center")
newAddresses <- c("233 South 10th Street, Philadelphia, Pennsylvania 19107", 
                  "7979 Wurzbach Road, San Antonio, Texas 78229")
newCoord <- sapply(newAddresses, geocode)
unlist(newCoord)

which(lonLat$Name %in% badNames)
lonLat[56,2:3] <- unlist(newCoord)[1:2]
lonLat[56,2:3]
lonLat[60,2:3] <- unlist(newCoord)[3:4]
lonLat[60,2:3]

ggplot(data = state_map, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), color = "grey40", size = 0.5) +
  geom_point(data = lonLat[lonLat$Name != "University of Hawaii Cancer Center",],
             aes(x = lon, y = lat), color = "red") +
  geom_text(data=lonLat[lonLat$Name != "University of Hawaii Cancer Center",], 
                hjust=0.5, vjust=-0.5, aes(x=lon, y=lat, label=Name), 
                colour="gold2", size=4 )


which(lonLat$Name == "Sanford-Burnham Medical Research Institute")
newCoord <- geocode("10901 North Torrey Pines Road, La Jolla, California 92037")
newCoord

lonLat[11, 2:3]
lonLat[11,2:3] <- newCoord
lonLat[11,2:3]


ggplot(data = state_map, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), color = "grey40", size = 0.5) +
  geom_point(data = lonLat[lonLat$Name != "University of Hawaii Cancer Center",],
             aes(x = lon, y = lat), color = "red") +
  geom_text(data=lonLat[lonLat$Name != "University of Hawaii Cancer Center",], 
            hjust=0.5, vjust=-0.5, aes(x=lon, y=lat, label=Name), 
            colour="gold2", size=2 )

save(lonLat, file="./Data/NCI.rda")


library(plotly)

py <- plotly()

viz <- ggplot(data = state_map, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), color = "grey40", size = 0.5) +
  geom_point(data = lonLat[lonLat$Name != "University of Hawaii Cancer Center",],
             aes(x = lon, y = lat, text = Name), color = "gold")

out <- py$ggplotly(viz, kwargs=list(filename="nci-test", fileopt="overwrite"))
plotly_url <- out$response$url
