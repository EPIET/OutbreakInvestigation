## ----package_installs---------------------------------------------------------

##################################################################
# INSTALL PACKAGES

# Check if the 'pacman' package is installed, if not install it:
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")



## ----package_loading----------------------------------------------------------

##################################################################
# LOAD LIBRARIES

# Load the required libraries into the current R session:
pacman::p_load(rio,
               here,
               janitor,
               tidyverse,
               tidygeocoder,
               sf,
               osmdata,
               ggmap,
               scales,
               leaflet,
               htmlwidgets)


## ----import csv files---------------------------------------------------------

###########################################################################
# Import outbreak case linelist

# Import the outbreak case line list, which includes residence coordinates:
caselist <- rio::import(file = here("data", 
                                    "CaseStudy_RDM_anon_data_coords.csv")) %>%
  # Clean variable names:
  janitor::clean_names() %>% 
  
  # Create nice labels for the Raw drinking milk (RDM) cluster variable:
  dplyr::mutate(wgs_rdm_cluster = case_when(
    wgs_rdm_cluster == TRUE ~ "A. Raw milk cluster", 
    wgs_rdm_cluster == FALSE ~ "B. Other cluster")) 


###########################################################################
# Import population data for health regions

# Import the population data for health regions (called PHE centres):
region_pop <- rio::import(file = here("data", 
                                      "PHEC_population.csv")) %>% 
  
  # Clean variable names: 
  janitor::clean_names()


## ----import shape files-------------------------------------------------------

# Import the shape files for 9 health regions (called PHEC or PHE Centres): 
region_sf <- sf::st_read(dsn = here("shapefiles", "En_PHE_Centre.shp")) %>% 
  
  # Clean variable names:
  janitor::clean_names()


## -----------------------------------------------------------------------------

region_sf <- sf::st_transform(x = region_sf, crs = 4326)


## ----calculate incidence------------------------------------------------------

# Create the table of summary counts of cases by PHE Centre:
incidencetab <- caselist %>% 
  group_by(phecnm) %>% 
  summarise(cases = length(caseid))

# Add population data to the table:
incidencetab <- region_pop %>% 
  left_join(incidencetab, by = "phecnm")

# Change NA counts to 0 for incidence calculation:
incidencetab <- incidencetab %>% 
  mutate(cases = ifelse(is.na(cases), 0, cases))

# Calculate crude incidence per 100,000 population:
incidencetab <- incidencetab %>% 
  mutate(Incidence = round((cases/population)*100000, 2))

# Update the health regions shapefile by merging the incidence table with it:
region_sf <- region_sf %>% 
  left_join(incidencetab, by = "phecnm")


## ----boundary box UK----------------------------------------------------------

# First, define the boundaries of the map you want to import:
ukbb <- osmdata::getbb(place_name = "United Kingdom", featuretype = "country")


# Next, get the base layer map matching this boundary box: 
ukmap <- ggmap::get_map(location = ukbb, 
                        maptype = "terrain", 
                        source = "stamen")

# Have a quick look at the base map:
ggmap(ukmap)



## ----boundary box England-----------------------------------------------------

# First, define the boundaries of the map you want to import:
englandbb <- osmdata::getbb(place_name = "England", featuretype = "settlement")


# Next, get the base layer map matching this boundary box: 
englandmap <- ggmap::get_map(location = englandbb, 
                             maptype = "terrain", 
                             source = "stamen")

# Have a quick look at the base map:
ggmap(englandmap)


## ----boundary box case coordinates--------------------------------------------

# First define the boundary box with case coordinates:
cbbox <- ggmap::make_bbox(lon = home_long,
                          lat = home_lat, 
                          data = caselist,
                          f = 0.1)


# Next, get the base layer map matching this boundary box: 
coordinatesmap <- ggmap::get_map(location = cbbox, 
                                 maptype = "terrain", 
                                 source = "stamen")

# Have a quick look at the base map:
ggmap(coordinatesmap)


## ----point map with web tiles-------------------------------------------------

# Fetch the selected base map tiles:
pointmap_web <- ggmap::ggmap(coordinatesmap, 
                             extent = 'panel', 
                             maprange = FALSE) +
  
  # Add case residence coordinates:
  geom_point(data = caselist, 
             aes(x = home_long, 
                 y = home_lat), 
             colour = "black",
             fill = "darkred",
             alpha = 0.5,
             size = 4, 
             shape = "circle filled", 
             show.legend = TRUE) +
  
  # Label x and y axes of map:
  labs(x = "Longitude", 
       y = "Latitude")
  
# Have a look at the map:
pointmap_web  


## -----------------------------------------------------------------------------

# Save the map as a .pdf
ggplot2::ggsave(filename = "Map of cases - static.pdf", 
              plot = pointmap_web, 
              width = 9, 
              height = 9, 
              dpi = 300)



## ----point map with shape files-----------------------------------------------

# Plot an empty frame:
pointmap_sf <- ggplot2::ggplot() + 
  
  # Add the base map using health region shape files:
  geom_sf(data = region_sf, 
          colour = "black", 
          fill = "darkgreen",
          alpha = 0.5,
          size = 0.75) +
  
  # Add the case home residence coordinates:
  geom_point(data = caselist, 
             aes(x = home_long, 
                 y = home_lat), 
             colour = "black",
             fill = "darkred",
             alpha = 0.5,
             size = 4, 
             shape = "circle filled", 
             show.legend = TRUE) +
  
  # Label the x and y axes:
  labs(x = "Longitude", 
       y = "Latitude")

# View the map:
pointmap_sf


## ----stratified point map with web tiles--------------------------------------

# Create the base map from selected OpenStreetMap tiles:
stratamap_web <- ggmap::ggmap(coordinatesmap, 
                              extent = 'panel', 
                              maprange = FALSE) +
  
  # Add the case coordinates:
  geom_point(data = caselist, 
             aes(x = home_long, 
                 y = home_lat, 
                 fill = wgs_rdm_cluster), # Here we specify the stratifier
             colour = "black",
             alpha = 0.5,
             size = 4, 
             shape = "circle filled", 
             show.legend = TRUE) +
  
  # Label the x and y axes:
  labs(x = "Longitude", 
       y = "Latitude") + 
  
  # Manually specify the colours of each stratum:
  scale_fill_manual(values = c("darkred", 
                               "turquoise")) + 
  
  # Manually specify the title of the strata legend:
  labs(fill = "Whole genome sequencing cluster")

  
# Have a look at the map:
stratamap_web  



## ----stratified point map with shape files------------------------------------

# Plot an empty frame:
stratamap_sf <- ggplot2::ggplot() + 
  
  # Add the base map using health region shape files:
  geom_sf(data = region_sf, 
          colour = "black", 
          fill = "darkgreen",
          alpha = 0.5,
          size = 0.75) +
  
  # Add the case home residence coordinates:
  geom_point(data = caselist, 
             aes(x = home_long, 
                 y = home_lat, 
                 fill = wgs_rdm_cluster), # Here we specify the stratifier
             colour = "black",
             alpha = 0.5,
             size = 4, 
             shape = "circle filled", 
             show.legend = TRUE) +
  
  # Label the x and y axes:
  labs(x = "Longitude", 
       y = "Latitude") +
  
  # Manually specify colours for strata levels:
  scale_fill_manual(values = c("darkred", 
                               "turquoise")) +
  
  # Manually specify legend title for strata:
  labs(fill = "Whole genome sequencing cluster:") 

# View the map:
stratamap_sf



## ----stratified point map time series-----------------------------------------

# Facet the stratified shape file map by year:
tsmap <- stratamap_sf + facet_wrap(~year, 
                                   ncol = 3) +
  
  theme(legend.position = "bottom")

# View the maps:
tsmap


## ----contourmap of case counts------------------------------------------------

# Create an empty frame:
contourmap <- ggplot2::ggplot() + 
  
  # Add the base map (health region shape files):
  geom_sf(data = region_sf, 
          colour = "black", 
          fill = "darkgreen",
          alpha = 0.5,
          size = 0.75) +
  
  # Aggregate cases by density to create the contour lines:
  geom_density2d(data = caselist, 
                 mapping = aes(x = home_long, 
                               y = home_lat, 
                               alpha = after_stat(level)),
                 contour_var = "count") # Count cases per grid square

# View the contour map:
contourmap


## ----heatmap of case counts---------------------------------------------------

# Create an empty frame:
heatmap <- ggplot2::ggplot() + 
  
  # Add the base map (health region shape file):
  geom_sf(data = region_sf, 
          colour = "black", 
          fill = "darkgreen",
          alpha = 0.5,
          size = 0.75) +
  
  # Add the case coordinates:
  stat_density2d(data = caselist, 
                 mapping = aes(x = home_long, 
                               y = home_lat, 
                               fill = ..level.., 
                               alpha = ..level..), 
                 linewidth = 0.01,  
                 bins = 50, # Changing the bins will change how the map looks
                 geom = "polygon") + 
  
  # Define the colour gradient for the heatmap:
  scale_fill_gradient(low = "blue", 
                      high = "red") + 
  
  # Define the minimum and maximum transparency levels for the heatmap colours:
  scale_alpha(range = c(0, 0.5), 
              guide = "none") # This suppresses the transparency legend

# View the heatmap with colour levels:
heatmap


## ----choropleth map of incidence----------------------------------------------

# Create the choropleth map with shape file and incidence data:
cimap <- ggplot2::ggplot(region_sf) + 
  
  # Set the values to plot and fill the regions with as incidence:
  geom_sf(mapping = aes(fill = Incidence), 
          colour = "black", 
          size = 0.5) +
  
  # Specify how the fill colours should be set:
  scale_fill_distiller(
    # Use shades of blue for the fill
    palette = "Blues", 
    # Break up the incidence scale into 8 groups:
    breaks = scales::breaks_pretty(n = 8), 
    # Use the default order of colours (-1 to reverse):
    direction = 1) +
  
  # Set the legend title and colour order in the legend:
  guides(fill = guide_legend(title = "Crude incidence per 100 000", 
                             reverse = FALSE)) + 
  
  # Apply the ggplot theme_nothing to remove grid lines:
  theme_nothing(legend = TRUE)

# View the map:
cimap


## ----interactive map----------------------------------------------------------

# Create the interactive map:
clustermap <- leaflet() %>% 
  
  # Add open street map (default base layer)
  addTiles(options = tileOptions(maxZoom = 18)) %>% 
  
  # Add transformed shapefile of regions
  addPolygons(data = region_sf, 
              weight = 5, 
              col = "black") %>% 
 
  # Add markers for case residence with descriptive labels:
  addMarkers(lng = caselist$home_long,
             lat = caselist$home_lat,
             popup = paste("ID: ", caselist$caseid, "<br/>",
                           "Epilink: ", caselist$epi_outbreak, "<br/>", 
                           "WGS cluster: ", caselist$wgs_rdm_cluster), 
             clusterOptions = markerClusterOptions()) 

# View the map:
clustermap


## ----save html map------------------------------------------------------------

# Save the map:
htmlwidgets::saveWidget(widget = clustermap, 
                        file = "Map of cases - interactive.html")



## ----add_space function-------------------------------------------------------

# Function to ensure space between incode and outcode in postcodes:
add_space <- function(postcodevar){
  
  # If a space is present between incode and outcode, return as is:
  if(grepl(pattern = "?(\\s)", x = postcodevar) == TRUE){
    
    pcwithspace = postcodevar
  
    } else {
    # If a space is missing between incode and outcode, add one:
    pcwithspace = stringr::str_replace(string = postcodevar, 
                               pattern = "(?=.{3}$)",
                               replacement = " ")
    }
  # Return the postcodes with space added:
  return(pcwithspace)
}



## ----clean postcodes----------------------------------------------------------

###############################################################################
# Read in the raw data set (where postcodes are not in the correct format):
caselist <- rio::import(file = here("data", "CaseStudy_RDM_anon_data.csv")) %>%
  
  # Procuess the following commands by row:
  rowwise() %>% 
  
  # Correct the format of the home residence postcodes:
  mutate(postcode_home = add_space(postcode_home)) %>% 
  
  # Correct the format of the UK travel (day trip) postcodes:
  mutate(postcode_uk_travel = add_space(postcode_uk_travel)) %>%
  
  # Correct the format of the exposure location postcodes:
  mutate(postcode_exposure = add_space(postcode_exposure))


## ----add_country--------------------------------------------------------------

# Import the raw data (contains clean postcodes but the are not yet geocoded):
caselist <- rio::import(file = here("data", 
                                    "CaseStudy_RDM_anon_data_cleanpcodes.csv")) %>%
  
  # Create a new column with the country name for the addresses to be geocoded:
  mutate(country = "UK")


## ----geocode, eval=FALSE------------------------------------------------------
#  # Use the residential (home) postcodes and country to fetch geocoordinates:
#  caselist <- caselist %>%
#  
#    # Geocode case residence postcodes:
#    tidygeocoder::geocode(postalcode = postcode_home,
#                          country = country,
#                          method = "osm",
#                          lat = "home_lat",
#                          long = "home_long") %>%
#  
#    # Geocode case UK travel (day trip) postcodes:
#    tidygeocoder::geocode(postalcode = postcode_uk_travel,
#                          country = country,
#                          method = "osm",
#                          lat = "travel_lat",
#                          long = "travel_long") %>%
#  
#    # Geocode case exposure location postcodes:
#    tidygeocoder::geocode(postalcode = postcode_exposure,
#                          country = country,
#                          method = "osm",
#                          lat = "exposure_lat",
#                          long = "exposure_long")
#  

