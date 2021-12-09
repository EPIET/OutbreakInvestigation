###############################################################################
# title: "Outbreak investigation module: mapping exercise"
# author: "Adapted from material by Daniel Gardiner and Amy Mikhail"
# date: "10 December 2021"
###############################################################################


###############################################################################
# 01. INSTALL AND LOAD PACKAGES

# Create a list of CRAN packages you will need for this session:
pkglist <- c("here", 
             "data.table",
             "dtplyr",
             "tidyverse",
             "tidygeocoder",
             "ggplot2", 
             "sf", 
             "osmdata",
             "ggmap",
             "scales",
             "tmap",
             "leaflet",
             "leaflet.extras2",
             "htmlwidgets")

# Check if packages are already installed, if not install them:
invisible(lapply(pkglist, function(x) 
  if (!requireNamespace(x, quietly = TRUE)) install.packages(x)))


# Loop through the CRAN packages in the list with lapply and load them:
invisible(lapply(pkglist, library, character.only = TRUE))


###############################################################################
# 02. SET WORKING DIRECTORY:

# Check the location of your current working directory:
getwd()

# Define path to working directory with here() and set it: 
setwd(here::here())


###############################################################################
# 03. IMPORT DATA:

# Read in raw data (not geocoded - if you want to try geocoding yourself):
rdmdata <- readr::read_csv(file = "data/CaseStudy_RDM_anon_data.csv")

# Read in population data for health regions (called PHE centres):
phecpop <- readr::read_csv(file = "data/PHEC_population.csv")


###############################################################################
# 04. CLEAN POSTCODES:

#######

# Function to ensure space between incode and outcode in postcodes:
add_space <- function(postcodes){
  
  # If a space is present between incode and outcode, return as is:
  if(grepl(pattern = "?(\\s)", x = postcodes) == TRUE){
    
    npc = postcodes
    
  } else {
    # If a space is missing between incode and outcode, add one:
    npc = stringr::str_replace(string = postcodes, 
                               pattern = "(?=.{3}$)",
                               replacement = " ")
  }
  # Return the postcodes with space added:
  return(npc)
}

#######

# Use the add_space function to correct postcode format:
rdmdata <- rdmdata %>% 
  rowwise() %>% 
  mutate(postcode_home = add_space(postcode_home))

# Now we need to add a new column to the case data indicating the country:
rdmdata <- rdmdata %>% 
  mutate(country = "UK")


###############################################################################
# 05. GEOCODE:

# Use the residential (home) postcodes and country to fetch geocoordinates:
rdmdata <- rdmdata %>% 
  tidygeocoder::geocode(postalcode = postcode_home, 
                        country = country, 
                        method = "osm", 
                        lat = "home_lat", 
                        long = "home_long")

###############################################################################
# 06. CREATE BOUNDARY BOX - FOR UK:

# First, define the boundaries of the map you want to import:
ukbb <- osmdata::getbb(place_name = "UK", featuretype = "country")


# Next, get the base layer map matching this boundary box: 
ukmap <- ggmap::get_map(location = ukbb, 
                        maptype = "toner-background", 
                        source = "osm")

# Have a quick look at the base map:
ggmap(ukmap)


###############################################################################
# 06. CREATE BOUNDARY BOX - FOR ENGLAND:

# First, define the boundaries of the map you want to import:
englandbb <- osmdata::getbb(place_name = "England", featuretype = "settlement")


# Next, get the base layer map matching this boundary box: 
englandmap <- ggmap::get_map(location = englandbb, 
                             maptype = "toner-background", 
                             source = "osm")

# Have a quick look at the base map:
ggmap(englandmap)


###############################################################################
# 07. CREATE BOUNDARY BOX - FOR SPECIFIC COORDINATES:

# First define the boundary box with case coordinates:
cbbox <- ggmap::make_bbox(lon = home_long,
                          lat = home_lat, 
                          data = rdmdata,
                          f = 0.1)


# Next, get the base layer map matching this boundary box: 
coordinatesmap <- ggmap::get_map(location = cbbox, 
                                 maptype = "toner-background", 
                                 source = "osm")

# Have a quick look at the base map:
ggmap(coordinatesmap)


###############################################################################
# 08. ADD CASE GEOLOCATIONS TO STATIC MAP:

# Create the case map:
pointmap <- ggmap::ggmap(coordinatesmap, 
                         extent = 'panel', 
                         maprange = FALSE) +
  geom_point(data = rdmdata, 
             aes(x = home_long, y = home_lat), 
             colour = "#238443",
             fill = "darkred",
             alpha = 0.5,
             size = 4, 
             shape = 21, 
             show.legend = TRUE) +
  labs(x = "Longitude", y = "Latitude")

# Have a look at the map:
pointmap  

# Save the map as a .pdf
ggplot2::ggsave(filename = "Map of cases - static.pdf", 
                plot = pointmap, 
                width = 9, 
                height = 9, 
                dpi = 300)


###############################################################################
# 09. STRATIFY CASES BY WHOLE GENOME SEQUENCING CLADE:

# Update the RDM clade variable (give it pretty labels):
rdmdata <- rdmdata %>% 
  mutate(rdm.clade = recode(.x = rdm.clade, 
                            `0` = "B. Other cluster", 
                            `1` = "A. RDM cluster"))


# Create the stratified map:
stratamap <- ggmap::ggmap(coordinatesmap, 
                          extent = 'panel', 
                          maprange = FALSE) +
  geom_point(data = rdmdata, 
             aes(x = home_long, y = home_lat, fill = rdm.clade), 
             colour = "#238443",
             alpha = 0.5,
             size = 4, 
             shape = 21, 
             show.legend = TRUE) +
  labs(x = "Longitude", y = "Latitude") + 
  scale_fill_manual(values = c("darkred", "turquoise")) + 
  labs(fill = "RDM WGS clade") + 
  scale_size(guide = "none")


# Have a look at the map:
stratamap  

# Save the map as a .pdf
ggplot2::ggsave(filename = "Map of cases - stratified by clade.pdf", 
                plot = stratamap, 
                width = 9, 
                height = 9, 
                dpi = 300)


###############################################################################
# 10. PLOT STRATIFIED CASES ON SHAPE-FILE DERIVED MAPS:

# Read in shape file for PHE Centres from the 'shapefiles' folder:
phecen <- sf::st_read(dsn = "shapefiles/En_PHE_Centre.shp")

# Convert the shapefile from eastings & northings to longitude & latitude:
phecll <- sf::st_transform(x = phecen, crs = 4326)

# Plot the shapefile as the base layer and add case coordinates on top:
phecmap <- ggplot2::ggplot() + 
  geom_sf(data = phecll, 
          colour = "black", 
          fill = "#004529",
          alpha = 0.5,
          size = 0.75) +
  coord_sf() + 
  geom_point(data = rdmdata, 
             aes(x = home_long, y = home_lat, fill = rdm.clade), 
             colour = "#238443",
             alpha = 0.5,
             size = 4, 
             shape = 21, 
             show.legend = TRUE) +
  labs(x = "Longitude", y = "Latitude") + 
  scale_fill_manual(values = c("darkred", "turquoise")) + 
  labs(fill = "RDM WGS clade") + 
  scale_size(guide = "none")

# View the map:
phecmap

# Save the map:
ggplot2::ggsave(filename = "Map of cases - stratified with shapefile.pdf", 
                plot = phecmap, 
                width = 9, 
                height = 9, 
                dpi = 300)


###############################################################################
# 11. FACET CASE MAPS OVER TIME:

# Facet phecmap by year:
tsmap <- phecmap + facet_wrap(~year, ncol = 3)

# View the maps:
tsmap

# Save the maps:
ggplot2::ggsave(filename = "Map of cases - stratified by year.pdf", 
                plot = tsmap, 
                width = 9, 
                height = 9, 
                dpi = 300)

###############################################################################
# 12. CREATE DENSITY MAP - CONTOUR:

# Create the contour map with geom_density2d:
contourmap <- ggplot2::ggplot() + 
  geom_sf(data = phecll, 
          colour = "black", 
          fill = "#004529",
          alpha = 0.5,
          size = 0.75) +
  coord_sf() + 
  geom_density2d(data = rdmdata, 
                 mapping = aes(x = home_long, 
                               y = home_lat, 
                               alpha = 0.5), # Change alpha for +/- resolution
                 inherit.aes = FALSE, 
                 contour_var = "count")

# View the contour map:
contourmap

# Save the contour map:
ggplot2::ggsave(filename = "Map of cases - heatmap with contours.pdf", 
                plot = contourmap, 
                width = 9, 
                height = 9, 
                dpi = 300)


###############################################################################
# 13. CREATE DENSITY MAP - HEAT:

# Create the heatmap with stat_density2d:
heatmap <- ggplot2::ggplot() + 
  geom_sf(data = phecll, 
          colour = "black", 
          fill = "#004529",
          alpha = 0.5,
          size = 0.75) +
  coord_sf() + 
  stat_density2d(data = rdmdata, 
                 mapping = aes(x = home_long, 
                               y = home_lat, 
                               fill = ..level.., 
                               alpha = ..level..), 
                 size = 0.01,  
                 bins = 50, # Changing the bins will change how the map looks
                 geom = "polygon", 
                 inherit.aes = FALSE) + 
  scale_fill_gradient(low = "blue", 
                      high = "red", 
                      guide = "none") + 
  scale_alpha(range = c(0, 0.5),
              guide = "none")

# View the heatmap with colour levels:
heatmap

# Save the heatmap:
ggplot2::ggsave(filename = "Map of cases - heatmap with shading.pdf", 
                plot = heatmap, 
                width = 9, 
                height = 9, 
                dpi = 300)


###############################################################################
# 14. CREATE DENSITY MAP - CALCULATE INCIDENCE:

# Create the table of summary counts of cases by PHE Centre:
incidencetab <- rdmdata %>% 
  group_by(PHECNM) %>% 
  summarise(cases = length(caseid))

# # Add population data to the table:
incidencetab <- phecpop %>% 
  left_join(incidencetab, by = "PHECNM")


# Change NA counts to 0 for incidence calculation:
incidencetab <- incidencetab %>% 
  mutate(cases = ifelse(is.na(cases), 0, cases))

# Calculate crude incidence per 100,000 population:
incidencetab <- incidencetab %>% 
  mutate(Incidence = round((cases/Population)*100000, 2))


# Merge incidence and case counts with sf object:
phecll <- phecll %>% 
  left_join(incidencetab, by = "PHECNM")


###############################################################################
# 15. CREATE DENSITY MAP - CHOROPLETH:

# Create the choropleth map:
cimap <- ggplot2::ggplot(phecll) + 
  geom_sf(mapping = aes(fill = Incidence), 
          colour = "black", 
          size = 0.5) +
  coord_sf() + 
  scale_fill_distiller(palette = "Blues", 
                       breaks = scales::pretty_breaks(n = 8), 
                       direction = 1) +
  guides(fill = guide_legend(title = "Crude incidence per 100 000", 
                             reverse = FALSE)) + 
  theme_nothing(legend = TRUE)

# View the map:
cimap

# Save the map:
ggplot2::ggsave(filename = "Map of cases - choropleth incidence.pdf", 
                plot = cimap, 
                width = 9, 
                height = 9, 
                dpi = 300)


###############################################################################
# 16. CREATE DENSITY MAP - INTERACTIVE LEAFLET:

# Create the interactive map:
rdmleaflet <- leaflet() %>% 
  # Add open street map (default base layer)
  addTiles() %>% 
  # Add markers with descriptive labels:
  addMarkers(lng = rdmdata$home_long,
             lat = rdmdata$home_lat,
             popup = paste("ID: ", rdmdata$caseid, "<br/>",
                           "Epilink: ", rdmdata$case.type, "<br/>", 
                           "RDM clade: ", rdmdata$rdm.clade), 
             clusterOptions = markerClusterOptions()) 

# View the map:
rdmleaflet

# Save the map:
htmlwidgets::saveWidget(widget = rdmleaflet, 
                        file = "Map of cases - interactive.html")


###############################################################################
############ END ##############################################################
