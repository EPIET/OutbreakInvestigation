# R CPD MAPPING EXERCISE

###########################################################################
# 00. LOAD REQUIRED PACKAGES AND FUNCTIONS

# This function on the PHE Gitlab server will check for required packages, load them or install and load if missing.
source("https://gitlab.phe.gov.uk/phe/r/packages/phemisc/raw/master/functions/packageloader.R")

# REQUIRED PACKAGES:

# - rgdal, ggmap, maptools, leaflet: for importing shape files and drawing maps
# - ggplot2, RColorBrewer, scales: for drawing graphics, selecting colour palattes, defining scales 
# - htmlwidgets: for exporting interactive graphics to html
# - here: shortcut to set working directory without explicit file paths
# - rgisws: package developed by PHE GIS team to map postcodes to UK geographies

# Use the function to load required packages from the CRAN repository:
using("rgdal", 
      "ggmap", 
      "ggplot2", 
      "maptools", 
      "scales", 
      "leaflet", 
      "leaflet",
      "htmlwidgets",
      "RColorBrewer", 
      "here", 
      repository = "cran")

# Use the function to load required packages from the PHE Gitlab repository:
using("rgisws", repository = "gitlab", subdir = "ERD/GIS")



###########################################################################
# 01. Read in (import) your data and get geographies to map it:

# Set your working directory to the folder containing this script:
wd <- here()

setwd(paste0(wd, "/Participants/"))
#setwd(wd)

# Read in data
data <- read.csv("data/dummy.mapping.data.csv", stringsAsFactors = FALSE)

# Remove lab postcode for now 
data$lab.postcode <- NULL

# Look at data
colnames(data)

head(data)

# Define geographies you want to fetch:
columns <- c('longitude', 'latitude', 'phec')

# Map postcodes to geographies with the postcode_lookup function in the rgisws package:
postcodes.geog <- postcode_lookup(postcodes = data$home.postcode, col_names = columns, xy = TRUE, return_names = TRUE)[, c("pcds", "x", "y", "latitude", "longitude", "phec_nm")]

names(postcodes.geog)[names(postcodes.geog) == 'x'] <- 'easting'
names(postcodes.geog)[names(postcodes.geog) == 'y'] <- 'northing'

# Append returned geographies to data:
data <- merge(data, postcodes.geog, by.x = "home.postcode", by.y = "pcds", all.x = TRUE)

###########################################################################
# 02. Now let's try plotting a map - using google maps as a backdrop for our cases

# Plot the cases on a spot map using ggmap 

## Create a boundary box to zoom manually
bbox <- ggmap::make_bbox(longitude, latitude, data, f = 0.5)

## Get a map centred on the boundary box
map_loc <- get_map(location = bbox, source = 'google', maptype = "roadmap", crop = FALSE, color = "bw")

## Plot the map in the background
map <- ggmap(map_loc, extent = 'device', maprange = FALSE)

## Add the points for each case: here they have been coloured by the variable sex
map <- map + geom_point(data = data, aes(x = longitude, y = latitude, fill = sex), colour = "black", pch = 21, size = 5) +
  scale_fill_manual(values = c("darkred", "turquoise")) +
  labs(fill = "Sex") +
  scale_size(guide = "none")

## View the map
map

## Save the map as a .pdf
ggsave("Map of cases - google.pdf", width = 9, height = 9, dpi = 300)


###########################################################################
# 03. Another point map - this time with shapefiles:

# Plot shapefile by itself (Shows borders of the 9 PHE Centres)

## Read in shape file for PHE Centres from the 'shapefiles' folder:
map <- readOGR("shapefiles")

## Fortify the map (i.e. convert to a data.frame so you can append the data you want to plot)
map.fort <- fortify(map)

## Plot the PHEC polygons:
p <- ggplot(map.fort, aes(x = long, y = lat, group = group))

p <- p + geom_polygon(colour = "black", fill = "grey")

p <- p + coord_fixed()

p <- p +  theme_nothing(legend = TRUE)

p

# Plot cases onto the map:

## Set group:
data$group = 1

## Plot the map with PHEC polygons and cases: ## doesn't work!
p <- p + geom_point(data = data, 
                   aes(x = "easting", y = "northing", group = "group"), 
                   aes(x = easting, y = northing, group = group), 
                   colour = "#892034", size = 4.5)

p

## Save the map as a .pdf

setwd(wd)

ggsave("Map of cases - shapefile.pdf", width = 9, height = 9, dpi = 300)

###########################################################################
# 04. Another point map - stratified by sex:

# plot cases onto shapefile, coloured by sex

library(maptools)

setwd(paste0(wd, "/shapefiles"))

map = readShapePoly("En_PHE_Centre.shp")

map.fort = fortify(map)

data$group = 1

p = ggplot(map.fort, aes(x = long, y = lat, group = group))

p = p + geom_polygon(colour = "white", fill = "grey")

p = p + coord_fixed()

p = p + theme_nothing(legend = TRUE)


p = p + geom_point(data = data, 
                   aes(x = easting, y = northing, group = group, 
                       colour = sex), size = 4, alpha = 0.8)

p = p + scale_color_manual(values = c("#892034", "#00AB8E"))


map <- readOGR("shapefiles")

map.fort <- fortify(map)

data$group <- 1

p <- ggplot(map.fort, aes(x = long, y = lat, group = group))

p <- p + geom_polygon(colour = "white", fill = "grey")

p <- p + coord_fixed()

p <- p + theme_nothing(legend = TRUE)


p <- p + geom_point(data = data, 
                   aes(x = easting, y = northing, group = group, colour = sex),
                   size = 4, alpha = 0.8)

p <- p + scale_color_manual(values = c("#892034", "#00AB8E"))

p

###########################################################################
# 05. Where are the cases most concentrated?  Let's see with a heat map:


# plot a heat map of cases 

require(maptools)

setwd(paste0(wd, "/shapefiles"))

map = readShapePoly("En_PHE_Centre.shp")

map.fort = fortify(map)

data$group = 1

p = ggplot(map.fort, aes(x = long, y = lat, group = group))

p = p + geom_polygon(colour = "black", fill = "grey")

p = p + coord_fixed()

p = p +  theme_nothing(legend = TRUE)

p = p + geom_point(data = data, 
                   aes(x = easting, y = northing, group = group), 
                   colour = "#892034", size = 4)

p = p + geom_density2d(data = data, aes(x = easting, y = northing))


p = p + stat_density2d(data = data,
                       aes(x = easting, y = northing, fill = ..level.., alpha = ..level..), 
                       size = 0.01,  bins = 16, geom = "polygon")

p = p + scale_fill_gradient(low = "green", high = "red", guide = F) 

p = p + scale_alpha(range = c(0, 0.3), guide = F)

###########################################################################
# Plot a heat map of cases 

map <- readOGR("shapefiles")

map.fort <- fortify(map)

data$group <- 1

p <- ggplot(map.fort, aes(x = long, y = lat, group = group))

p <- p + geom_polygon(colour = "black", fill = "grey")

p <- p + coord_fixed()

p <- p +  theme_nothing(legend = TRUE)

p <- p + geom_point(data = data, 
                   aes(x = easting, y = northing, group = group), 
                   colour = "#892034", size = 4)

p <- p + geom_density2d(data = data, aes(x = easting, y = northing))


p <- p + stat_density2d(data = data,
                       aes(x = easting, y = northing, fill = ..level.., alpha = ..level..), 
                       size = 0.01,  bins = 16, geom = "polygon")

p <- p + scale_fill_gradient(low = "green", high = "red", guide = F) 

p <- p + scale_alpha(range = c(0, 0.3), guide = F)

p

###########################################################################
# 06. Viewing case density by PHE Centre: a choropleth map

# plot choropleth map 

library(maptools)
library(scales)

setwd(paste0(wd, "/shapefiles"))

map = readShapePoly("En_PHE_Centre.shp")

map.fort = fortify(map)

counts = as.data.frame(xtabs(~phec_PHECNM, data))

map.id.lookup = data.frame(id = row.names(map@data), 
                           PHECNM = map@data$PHECNM)

counts = merge(counts, map.id.lookup, 
               by.x = "phec_PHECNM", by.y = "PHECNM", all.y = T)

counts$Freq[is.na(counts$Freq)] = 0

map.fort = merge(map.fort, counts, by = "id", all.x = T)

p = ggplot(map.fort, aes(x = long, y = lat, group = group))

p = p + geom_polygon(aes(fill = Freq))

p = p + geom_path(colour = "black", size = 0.5)

p = p + coord_fixed()

p = p +  scale_fill_distiller(palette = "Blues",
                              breaks = pretty_breaks(n = 8))

p = p + guides(fill = guide_legend(reverse = TRUE))

p = p + theme_nothing(legend = TRUE)

###########################################################################
# Plot choropleth map 
map <- readOGR("shapefiles")

map.fort <- fortify(map)

counts <- as.data.frame(xtabs(~phec_nm, data))

map.id.lookup <- data.frame(id = row.names(map@data), 
                           PHECNM = map@data$PHECNM)

counts <- merge(counts, map.id.lookup, 
               by.x = "phec_nm", by.y = "PHECNM", all.y = T)

counts$Freq[is.na(counts$Freq)] <- 0

map.fort <- merge(map.fort, counts, by = "id", all.x = T)

p <- ggplot(map.fort, aes(x = long, y = lat, group = group))

p <- p + geom_polygon(aes(fill = Freq))

p <- p + geom_path(colour = "black", size = 0.5)

p <- p + coord_fixed()

p <- p +  scale_fill_distiller(palette = "Blues",
                              breaks = pretty_breaks(n = 8))

p <- p + guides(fill = guide_legend(reverse = TRUE))

p <- p + theme_nothing(legend = TRUE)

p

###########################################################################
# 07. Time series maps - show cases by year of onset:

# plot cases onto shapefile, stratified by year

require(maptools)

setwd(paste0(wd, "/shapefiles"))

map = readShapePoly("En_PHE_Centre.shp")

map.fort = fortify(map)

data$group = 1

p = ggplot(map.fort, aes(x = long, y = lat, group = group))

p = p + geom_polygon(colour = "white", fill = "grey")

p = p + coord_fixed()

p = p +  theme_nothing(legend = TRUE)

p = p + geom_point(data = data, 
                   aes(x = easting, y = northing, group = group), 
                   colour = "#892034", size = 4)

p = p + facet_wrap(~spec.year, ncol = 3)

###########################################################################
# Plot cases onto shapefile, stratified by year

map <- readOGR("shapefiles")

map.fort <- fortify(map)

data$group <- 1

p <- ggplot(map.fort, aes(x = long, y = lat, group = group))

p <- p + geom_polygon(colour = "white", fill = "grey")

p <- p + coord_fixed()

p <- p +  theme_nothing(legend = TRUE)

p <- p + geom_point(data = data, 
                   aes(x = easting, y = northing, group = group), 
                   colour = "#892034", size = 4)

p <- p + facet_wrap(~spec.year, ncol = 3)

p


###########################################################################
# 08. Plot cases on an interative map 

library(leaflet)


myleaflet <- leaflet() %>% 
  # Add open street view map
  addTiles() %>% 
 # Add markers
  addMarkers(lng = data$longitude,
             lat = data$latitude,
             popup = paste("ID:", data$id, "<br/>",
                           "Sex:", data$sex, "<br/>", 
                           "Conf:", data$conf))



# Save the interactive map as a .html file (explore in your browser):
saveWidget(myleaflet, file = "Map of cases - interactive.html")


###########################################################################
# END OF TEMPLATE #









