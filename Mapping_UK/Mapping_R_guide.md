Outbreak investigation module: mapping exercise
================
Adapted from material by Daniel Gardiner and Amy Mikhail
10 December 2021

-   [Preamble](#preamble)
-   [How to use this resource](#how-to-use-this-resource)
-   [Introduction](#introduction)
-   [Required packages](#required-packages)
-   [Setting your working directory](#setting-your-working-directory)
-   [Reading in the data](#reading-in-the-data)
-   [Geocoding the data](#geocoding-the-data)
    -   [Postcode cleaning:](#postcode-cleaning)
    -   [Geocoding cleaned postcodes:](#geocoding-cleaned-postcodes)
-   [Mapping cases](#mapping-cases)
    -   [Creating static background
        maps:](#creating-static-background-maps)
    -   [Overlaying static maps with case
        coordinates:](#overlaying-static-maps-with-case-coordinates)
    -   [Stratifying static maps by other
        variables:](#stratifying-static-maps-by-other-variables)
    -   [Creating a static point map with
        shapefiles](#creating-a-static-point-map-with-shapefiles)
-   [Dealing with overlapping points](#dealing-with-overlapping-points)
    -   [Separating maps by time:](#separating-maps-by-time)
    -   [Creating a heatmap of case
        counts](#creating-a-heatmap-of-case-counts)
    -   [Creating a choropleth map of incidence
        rates](#creating-a-choropleth-map-of-incidence-rates)
    -   [Creating an interactive cluster
        map](#creating-an-interactive-cluster-map)
-   [References](#references)
-   [Appendix](#appendix)
    -   [A. Creating regular
        expressions](#a-creating-regular-expressions)
    -   [B. Calculating density in contour and
        heatmaps](#b-calculating-density-in-contour-and-heatmaps)

# Preamble

This case study was originally developed by Amy Mikhail and Daniel
Gardiner for a beginner’s R course for epidemiologists that was run in
Public Health England from 2017 to 2019. The case study is based on an
outbreak that occurred in the South West of England in 2014. The data
sets used in this exercise are derived from the original outbreak, but
they have been anonymised (postcodes jittered and patient identifying
information removed).

The material was then adapted slightly for the UK FETP Outbreak Tools
module in 2019.

Substantial changes have been made to this version to bring it up to
date. In particular:

-   PHE-specific packages have been replaced by publicly available
    packages from CRAN
-   Outdated packages and soon-to-be deprecated packages have been
    replaced
-   Where possible, R packages compatible with the tidyverse have been
    selected
-   The example code in this R markdown guide has been substantially
    updated

# How to use this resource

This R markdown file demonstrates the code you will need to answer the
questions posed in the practical guide for this case study.

The answers are not provided, but there should be sufficient information
in the examples below to investigate the geospatial epidemiology of this
outbreak, even if you are relatively new to R. You will however need a
basic understanding of R syntax, as it is outside the scope of this
practical to explain it fully.

For each processing step, **one example** has been provided; while
working through the practical tasks you may need to repeat this code for
other variables in the data set or adapt some of the arguments as
needed.

There are several different “data wrangling” packages in R (the main
three approaches being base R, `data.table` and `dplyr`). Base R syntax
can be very long-winded and few people use it any more for data
wrangling tasks, as there are now better options.

This guide uses `dplyr` for manipulation of data sets (adding and
updating variables) and other packages from the `tidyverse` for all the
mapping tasks. The `tidyverse` suite of packages are becoming
increasingly popular as they share an intuitive syntax and make code
easier to write and read. If you are working with very large data sets,
however, the `data.table` package may be more suitable as it has been
designed for fast execution and to minimise demands on available memory
(RAM). A relatively new package, `dtplyr` has been created for those who
want to take advantage of the greater speed and efficacy of `data.table`
but prefer to use `dplyr` syntax.

There are many different options for performing tasks such as geocoding
and cartography in R. In the examples below, packages have been selected
on the basis that:

-   They are part of the tidyverse or compatible with it;
-   They have the capacity to work with data from any country;
-   The object formats are inter-operable and require little to no
    transformation.

Finally, note that wherever a package that is not part of base R has
been used, the function is preceded by the package name, e.g.:
`stringr::str_replace()` is the `str_replace` function from the
`stringr` package. This should make it easier to identify which packages
you will need to use when writing your own mapping code. For the same
reason, wherever possible arguments within functions have been
explicitly named, so that you can identify and read about the accepted
values for these arguments in the package help files.

Happy mapping!

# Introduction

For this exercise, you have been provided with a line list of cases from
an outbreak associated with raw drinking milk that was contaminated with
shiga toxin-producing *Escherichia coli* (STEC), which occured in 2014.

The primary exposure was at the farm producing the raw milk in the South
West of England. Samples of raw milk from this farm were also positive
for the same strain of STEC as cases.

Initially, cases were defined as laboratory confirmed cases of STEC that
had visited the implicated farm. However, whole genome sequencing showed
that there were a number of additional cases within the same STEC
cluster that had no known history of visiting the farm. A number of
cases within this cluster were also detected up to 5 years prior to this
outbreak.

The outbreak control team has therefore asked you to do some geospatial
analysis and particularly wants to know:

1.  Are pre-outbreak cases also clustered near the farm in the South
    West?
2.  Are all cases from the outbreak year (2014) clustered near the farm
    in the South West?
3.  If not, does their spatial distribution pattern indicate an
    alternative source for this strain of STEC?

To help with further investigations, 2014 cases that had not previously
been associated with the implicated farm were re-interviewed to
determine if they had any less direct links. Some of these cases had
bought raw milk from a mobile van that was distributing raw milk from
the farm to multiple locations across South England.

For this geospatial analysis, you have been provided with a raw data set
containing three sets of postcodes:

1.  Patient residence postcodes (where the patients live)
2.  Postcodes of locations visited in the UK in the week before symptom
    onset
3.  Exposure postcodes (where the patients were most likely exposed to
    the raw milk)

Some other basic patient data has also been provided, which you may find
useful to stratify the maps and gain some insights into the spatial
epidemiology of this outbreak.

# Required packages

You can use this section to install and load the packages that you will
need for this session. Some brief details on why each package was
selected are provided below:

-   `here` - setting of working directory containing materials for this
    practical
-   `tidyverse` - collection of packages based on tidy data format and
    dplyr syntax
-   `tidygeocoder` - geocoding (converting UK postcodes to longitude and
    latitude)
-   `sf` - converts meta data and coordinates to a “simple feature”
    object for maps
-   `osmdata` - free OpenStreetMap resource for creating map boundary
    boxes
-   `ggmap` - “grammar of graphics” package for creating and displaying
    static maps
-   `scales` - create pretty breaks for choropleth maps
-   `leaflet` - java package for making interactive maps with more
    complex features
-   `htmlwidgets` - allows saving of leaflet maps as html files

The first element in the code below compiles all the required packages
into a list.

The second element will check if you already have these packages in your
R library and install them if not (note - you will need an internet
connection and to have selected your preferred CRAN mirror prior to
running this line).

Once any missing packages have been installed, the third element will
load all the required packages from your R library. If you have any
problems with package installation or loading, please ask the
facilitators for help before proceeding.

``` r
##################################################################
# INSTALL PACKAGES

# Create a list of CRAN packages you will need for this session:
pkglist <- c("here", 
             "tidyverse",
             "tidygeocoder",
             "sf", 
             "osmdata",
             "ggmap",
             "scales",
             "leaflet",
             "htmlwidgets")

# Check if packages are already installed, if not install them:
invisible(lapply(pkglist, function(x) 
  if (!requireNamespace(x, quietly = TRUE)) install.packages(x)))


##################################################################
# LOAD PACKAGES:

# Loop through the CRAN packages in the list with lapply and load them:
invisible(lapply(pkglist, library, character.only = TRUE))
```

# Setting your working directory

In this practical we will use the `here` package to identify the working
directory. Ideally, open the .Rproj file in the “Mapping” folder that
contains all the material for this practical and create or modify R
scripts from within this R project file. A `.here` place-holder has
already been added to the “Mapping” folder for convenience; this will
automatically set the working directory to the directory in which the
`.here` file is located, irrespective of where you saved the “Mapping”
folder on your computer.

When reading in files in any sub-folders, you can use the `here` package
to define the relative path (the root directory is wherever the `.here`
file is located).

``` r
# Check the location of your current working directory:
getwd()

# Define path to working directory with here() and set it: 
setwd(here::here())
```

# Reading in the data

In this section you can read in (import) the raw data sets. You can find
the data sets in a sub-folder called `data` which should be in the
Mapping folder provided to you for this exercise.

**Note there are three raw data sets:**

-   CaseStudy_RDM_anon_data.csv - this contains the raw anonymised data
    set
-   CaseStudy_RDM_anon_data_coords.csv - this contains the same dataset
    but already populated with geocoordinates
-   PHEC_population.csv - this contains population data for health
    regions in England

The code below shows you how to read in the raw case and population data
with:

-   `readr::read_csv()` from the tidyverse package `readr`

You can also use:

-   `data.table::fread()` command from the `data.table` package
-   `read.csv()` from base R

All of these options will import the raw data *from a comma separated
values file* into R as a `data.frame`.

**Notes on importing data:**

1.  You do not need to read in the data with coordinates unless you have
    difficulties with the geocoding step (next).
2.  If you use a comma `,` as the decimal separator in your country and
    you are using dplyr code, choose `readr::read_csv2()` instead of
    `readr::read_csv()`. csv2 assumes that a period `.` is being used as
    the decimal separator and will therefore assume that columns in a
    .csv file are separated by a semi-colon `;`.
3.  If your data is in a Microsoft Excel file, you can either save as a
    .csv before starting your analysis, or use other r packages like
    `readxls` or `openxlsx`, respectively, to import your data.

We will read in the data using the `readr` package, which is part of the
tidyverse and automatically imports the data as a tibble. The advantage
of tibbles is that if the variables in your data have descriptive
labels, tibbles will keep the descriptive labels as well as the variable
names. Other tabular formats will not do this.

``` r
# Read in raw data (not geocoded - if you want to try geocoding yourself):
rdmdata <- readr::read_csv(file = "data/CaseStudy_RDM_anon_data.csv")
```

    ## Rows: 59 Columns: 39

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (14): epidef, subtype, microdef, sequenced, epi.cluster, studydate, sex,...
    ## dbl  (8): caseid, rdm.clade, year, month, week, day, age, severe
    ## lgl (17): asymptomatic, hus, hospitalised, diarrhoea, blood, nausea, vomit, ...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Read in population data for health regions (called PHE centres):
phecpop <- readr::read_csv(file = "data/PHEC_population.csv")
```

    ## Rows: 9 Columns: 2

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (1): PHECNM
    ## dbl (1): Population

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

# Geocoding the data

**Geocoding** (the process of converting addresses to geographic
coordinates) is done by using an address-to-coordinates lookup table,
which is typically accessed from either a public database or a local
mirror. It can be challenging to geocode using the public databases as
in order to ensure fair use, these often have a cap on the number of
postcodes you can geocode at one time. Conversely, geocoding from a
local mirror database created by your institution requires permission to
access that database from your organisation and can generally only be
accessed via your organisational account.

## Postcode cleaning:

Because the `tidygeocoder` package is not UK-specific, it will not
recognise UK postcodes unless they are in the correct format and the
country name is also supplied.

A quick preview of the postcode data with `head(rdmdata$postcode_home)`
for example, shows you that the space that normally separates the
in-code and out-code part of UK postcodes is missing for postcodes with
7 characters.

We can correct this by using a regular expression to add the spaces back
in. The function below first checks if there is already a space in the
postcode; if there is, it returns it as-is. If the space is missing, it
counts back 3 characters from the end of the postcode (right side) and
adds a space before the third-last character. Note that this function is
not vectorised, so we need to loop over individual rows (postcodes) to
apply it to the data.

``` r
###############################################################################

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

###############################################################################

# Use the add_space function to correct postcode format:
rdmdata <- rdmdata %>% 
  rowwise() %>% 
  mutate(postcode_home = add_space(postcode_home))

# Now we need to add a new column to the case data indicating the country:
rdmdata <- rdmdata %>% 
  mutate(country = "UK")
```

## Geocoding cleaned postcodes:

We can now geocode the cleaned postcodes.

For this practical exercise, we will be using the publicly available
package [tidygeocoder](https://jessecambon.github.io/tidygeocoder/) from
CRAN, which is tidyverse compatible and performs batch look-ups of
addresses and postcodes (geocoding) or geographic coordinates (reverse
geocoding). Matching values are returned as new columns, which you can
add directly to your data.frame, tibble or data.table.

The package interfaces with [multiple data
providers](https://jessecambon.github.io/tidygeocoder/articles/geocoder_services.html),
each of which have their own terms and conditions for performing
geocoding batch queries. We will use the OpenStreetMap (OSM) service
Nominatum, which is a free service that will work with addresses or
postcodes from any country. Results are returned at a rate of 1 query
per second.

Note that OSM may block users who perform the same queries multiple
times (see their terms of use
[here](https://operations.osmfoundation.org/policies/nominatim/)). If
you encounter any difficulties during this practical session, please
skip this step and use the pre-geocoded dataset
`CaseStudy_RDM_anon_data_coords.csv` instead.

``` r
# Use the residential (home) postcodes and country to fetch geocoordinates:
rdmdata <- rdmdata %>% 
  tidygeocoder::geocode(postalcode = postcode_home, 
                        country = country, 
                        method = "osm", 
                        lat = "home_lat", 
                        long = "home_long")
```

    ## Passing 59 addresses to the Nominatim single address geocoder

    ## Query completed in: 60.3 seconds

# Mapping cases

In the following sections, we will explore the spatial distribution of
the cases by creating maps using the `sf`, `ggmap`, `ggplot2` and
`leaflet` packages. These packages are publicly available on CRAN and
also tidyverse compatible.

For creating static maps, we will also need to download the coordinates
that will define the edges of our background map. We will do this using
the `osmdata` package, which fetches background maps for a given
boundary area from OpenStreetMap.

## Creating static background maps:

To create a static map of cases, we will first import data to create a
background map of the UK using the `osmdata` package. You can define the
boundary box for the map you want to import simply by typing the country
name (or region and country name if you want to focus on one region). A
list of country names and their corresponding codes is provided on the
[OpenStreetMap Nominatim country code wiki
page](https://wiki.openstreetmap.org/wiki/Nominatim/Country_Codes). Note
that OSM is fairly flexible with country names and will usually accept
commonly used abbreviations as well as the official name or code.

We will then use this boundary box to fetch a nice background map from
OpenStreetMap with the `ggmap` package. Note that it used to be possible
to bulk geocode and define boundary boxes with Google maps as well, but
this is now exclusively a paid service. If your organisation does pay
for this service, you can use it by providing an API key as one of the
arguments. For this exercise, we will only use OpenStreetMap as it is a
free service.

This map will be used as the base layer.

``` r
# First, define the boundaries of the map you want to import:
ukbb <- osmdata::getbb(place_name = "United Kingdom", featuretype = "country")


# Next, get the base layer map matching this boundary box: 
ukmap <- ggmap::get_map(location = ukbb, 
                        maptype = "toner-background", 
                        source = "osm")

# Have a quick look at the base map:
ggmap(ukmap)
```

![](Mapping_R_guide_files/figure-gfm/boundary%20box%20UK-1.png)<!-- -->

Getting a map of the whole of the UK produces a lot of empty space
(ocean) because it includes some small islands to the north of Scotland.
In this outbreak, all the cases are actually located in England (a level
4 administrative boundary within the UK). We can zoom in by creating a
boundary box just for England instead:

``` r
# First, define the boundaries of the map you want to import:
englandbb <- osmdata::getbb(place_name = "England", featuretype = "settlement")


# Next, get the base layer map matching this boundary box: 
englandmap <- ggmap::get_map(location = englandbb, 
                             maptype = "toner-background", 
                             source = "osm")

# Have a quick look at the base map:
ggmap(englandmap)
```

![](Mapping_R_guide_files/figure-gfm/boundary%20box%20England-1.png)<!-- -->

We already know that a lot of the cases are concentrated in the South
West of England (near the farm that was the likely source of the
contaminated raw milk). We can be even more precise with the boundary
box by supplying it with the range (maximum and minimum) of coordinates
defining the area in which the cases are found:

``` r
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
```

![](Mapping_R_guide_files/figure-gfm/boundary%20box%20case%20coordinates-1.png)<!-- -->

Note that when defining the boundary box, the `f` value (fraction by
which the range should be extended beyond the minimum and maximum
coordinates) will affect the zoom level of the map. A higher resolution
map will also include more place names at a lower administrative level.

Choose the base map that you think best defines the area of interest
before proceeding to the next section.

## Overlaying static maps with case coordinates:

In this section we will add the case coordinates to the base map in a
new layer with `ggmap` to create a point map. You can also adjust the
colour, shape and size of the points.

``` r
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
```

![](Mapping_R_guide_files/figure-gfm/point%20map%20basic-1.png)<!-- -->

``` r
# Save the map as a .pdf
ggplot2::ggsave(filename = "Map of cases - static.pdf", 
              plot = pointmap, 
              width = 9, 
              height = 9, 
              dpi = 300)
```

## Stratifying static maps by other variables:

It might be useful to see how cases are distributed when they are
stratified by a grouping variable. Cases were originally defined in
terms of time, place and STEC subtype. However later, whole genome
sequencing results became available. This showed that not all case
isolates fell into the the same whole genome sequencing cluster as the
strain of STEC that had been found in the raw milk (hereafter referred
to as the RDM clade). The variable that we will use for this
stratification is called `rdm.clade`.

``` r
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
```

![](Mapping_R_guide_files/figure-gfm/point%20map%20with%20stratification-1.png)<!-- -->

``` r
# Save the map as a .pdf
ggplot2::ggsave(filename = "Map of cases - stratified by clade.pdf", 
              plot = stratamap, 
              width = 9, 
              height = 9, 
              dpi = 300)
```

## Creating a static point map with shapefiles

In this section, we will use some shape files of PHE Centres (9 health
regions in England) to create the base layer for our map and plot the
points on top, as before.

Note that the shapefiles were created using a different coordinate
system (British National Grid, eastings and northings) and need to be
transformed to longitude and latitude before we can overlay the case
coordinates. Fortunately the `sf` package has a function to transform
date from one coordinate system to another. The `crs` argument of the
function `st_transform()` requires the EPSG code of the system you want
to transform the data to. A useful guide to the EPSG codes of two common
coordinate reference systems can be found
[here](https://geo.nls.uk/urbhist/guides_coordinates.html).

This time, we will use the `ggplot2` package to create the empty frame
for the map, as it is not possible to have an empty frame with the
`ggmap` package.

Once the shapefiles have been transformed and added as a base layer, we
can add the stratified case coordinates on top using the same code as
for the previous map.

``` r
# Read in shape file for PHE Centres from the 'shapefiles' folder:
phecen <- sf::st_read(dsn = "shapefiles/En_PHE_Centre.shp")
```

    ## Reading layer `En_PHE_Centre' from data source 
    ##   `C:\Users\amymi\Documents\MyDocs\07_Consults\EPIET_GIS_R_casestudy\OutbreakInvestigation\Mapping_UK\shapefiles\En_PHE_Centre.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 9 features and 6 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 82672 ymin: 5342.7 xmax: 655604.7 ymax: 657534.1
    ## Projected CRS: OSGB 1936 / British National Grid

``` r
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
```

![](Mapping_R_guide_files/figure-gfm/static%20map%20with%20shapefile%20stratified%20by%20WGS%20clade-1.png)<!-- -->

``` r
# Save the map:
ggplot2::ggsave(filename = "Map of cases - stratified with shapefile.pdf", 
                plot = phecmap, 
                width = 9, 
                height = 9, 
                dpi = 300)
```

# Dealing with overlapping points

One challenge with static point maps is that if the points overlap, it
is difficult to see what the density of cases is in a given area. In
this section, we will explore four different ways of overcoming this
problem:

1.  Producing separate maps for each time interval
2.  Creating contour and heat maps of case counts
3.  Creating a choropleth map of crude incidence rates per head of
    population
4.  Creating an interactive map with zoom-regulated clustering

We will not be looking at ways to analyse spatial density as this falls
outside the scope of this practical, but the impact of selected area
size on relative density should be taken into account when interpreting
these types of maps.

## Separating maps by time:

In addition to stratifying by the fill colour of the point, maps can
also easily be stratified by separating them out according to a grouping
variable such as time, with the function `ggplot2::facet_wrap()`.

In this example, we will look at the previous map and stratify it by
year of onset to investigate the change in case distribution over time.

``` r
# Facet phecmap by year:
tsmap <- phecmap + facet_wrap(~year, ncol = 3)

# View the maps:
tsmap
```

![](Mapping_R_guide_files/figure-gfm/static%20map%20time%20series-1.png)<!-- -->

``` r
# Save the maps:
ggplot2::ggsave(filename = "Map of cases - stratified by year.pdf", 
                plot = tsmap, 
                width = 9, 
                height = 9, 
                dpi = 300)
```

## Creating a heatmap of case counts

To create this map we will begin as we did in the previous section,
using the shapefile as our base layer and adding the coordinates on top.
Two basic options are available to add a heatmap layer:

1.  `geom_density2d()` which adds contour lines
2.  `stat_density2d()` which adds shades of colour indicating increasing
    density

You can increase the number of `bins` and `alpha` arguments to view case
distributions at a higher resolution or finer grain, respectively.

``` r
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
                               alpha = 0.5),
                 inherit.aes = FALSE, 
                 contour_var = "count")

# View the contour map:
contourmap
```

![](Mapping_R_guide_files/figure-gfm/contourmap%20of%20case%20counts-1.png)<!-- -->

``` r
# Save the contour map:
ggplot2::ggsave(filename = "Map of cases - heatmap with contours.pdf", 
                plot = contourmap, 
                width = 9, 
                height = 9, 
                dpi = 300)
```

``` r
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
```

![](Mapping_R_guide_files/figure-gfm/heatmap%20of%20case%20counts-1.png)<!-- -->

``` r
# Save the heatmap:
ggplot2::ggsave(filename = "Map of cases - heatmap with shading.pdf", 
                plot = heatmap, 
                width = 9, 
                height = 9, 
                dpi = 300)
```

## Creating a choropleth map of incidence rates

In this section we will create a choropleth map of crude incidence per
100 000 population by PHE Centre. To do this, you will need to prepare
the following:

1.  Summary table of case counts by PHE Centre (variable is called
    `PHECNM`)
2.  Add the population for each PHE Centre to this table from the data
    set `phecpop`
3.  Calculate incidence per 100 000 population and add this to the table
4.  Merge this table with the sf object containing the polygon
    coordinates `phecll`

We will then use the `ggplot2` package to create the choropleth map.

``` r
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
```

Now that we have added incidence rate to the `sf` object, we can create
the choropleth map:

``` r
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
```

![](Mapping_R_guide_files/figure-gfm/choropleth%20map%20of%20incidence-1.png)<!-- -->

``` r
# Save the map:
ggplot2::ggsave(filename = "Map of cases - choropleth incidence.pdf", 
                plot = cimap, 
                width = 9, 
                height = 9, 
                dpi = 300)
```

## Creating an interactive cluster map

One final option we can explore for separating out overlapping data
points is the creation of an interactive map. To create the map, we will
use the `leaflet` package, with the clustering feature `clusterOptions`
activated. This feature “collapses” cases when the map is zoomed out and
represents the cases in a circular marker with a numeric total. When
zoomed in, individual cases will segregate.

Leaflet maps can be included in html reports and are a very useful way
to display multiple aspects of the data set in one figure, as they can
be richly annotated. We will use the `popup` command to annotate each
point with some data about the case from our case dataset. Note that
because the leaflet map is a html object, we need to create the
annotations using html syntax for line breaks (`<br/>`).

When distributing leaflet maps for public consumption, users are
strongly advised to set a zoom limit - otherwise personal data (the
exact location of the patient’s house) will be visible at the maximum
zoom level. On the other hand for internal use, this feature can be very
useful for investigating outbreaks that have micro-spatial patterns,
such as street-level variations in the spatial distribution of cases
(e.g. due to contaminated waterworks or proximity to a cooling tower).

``` r
# Create the interactive map:
rdmleaflet <- leaflet() %>% 
  # Add open street map (default base layer)
  addTiles() %>% 
 # Add markers with descriptive labels:
  addMarkers(lng = rdmdata$home_long,
             lat = rdmdata$home_lat,
             popup = paste("ID: ", rdmdata$caseid, "<br/>",
                           "Epilink: ", rdmdata$epi.cluster, "<br/>", 
                           "RDM clade: ", rdmdata$rdm.clade), 
             clusterOptions = markerClusterOptions()) 

# View the map:
rdmleaflet
```

<div id="htmlwidget-226a489ad3981561dea8" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-226a489ad3981561dea8">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addMarkers","args":[[51.81214,50.39154,50.32372,50.410802925,50.41288,50.41035,50.403,50.39287,50.40226,50.40427,50.3993130416667,50.4960734199053,50.498969,50.475314163496,50.4591783173542,50.45095055,50.9364397,50.93061,50.21785,53.80493635,54.97034,50.65269225,50.6612,51.054,50.91926,50.4370860020001,52.04049,52.05421,51.0567,52.04784,54.0516728339321,50.84603,50.4566607606596,53.99683,54.003,50.4578465067531,53.62889,50.97291,52.44796,52.602995,52.6627689169087,50.5302439932249,50.5839547911975,51.5237231625,51.52517,51.5606,52.3838458694444,51.1009,50.14595,50.13416,51.09993,50.13627,50.7306645,51.47117,50.73265925,50.47092,50.11714,50.84651,52.9530904],[-0.13719,-4.04326,-4.11117,-4.15039726707164,-4.14594,-4.14773,-4.15911,-4.04235,-4.03995,-4.13508,-4.14162126128629,-3.57240252473155,-3.5701341,-3.50964430819227,-3.59542293151897,-3.58483525,-0.551472625322293,-0.56201,-5.0857,-1.4717054180074,-2.15204,-1.31003114375,-1.30044,-1.80547,-2.62395,-3.57378918711265,-2.69863,-2.41696,-1.3032,-2.42901,-2.80167344922484,-0.13096,-3.5605893746241,-1.13037,-1.11999,-3.55765305703083,-1.78018,-1.30157,-1.51488,-1.05964424079722,0.170939198407307,-3.55922722242588,-3.66308916780563,-0.125019473764865,-0.12483,-0.09247,-1.5335653015664,-1.36417,-5.20326,-5.20883,-1.3508,-5.21646,-3.5423502,-0.0515,-3.53813446325243,-4.08531,-5.55063,-3.39578,-1.2117587],null,null,null,{"interactive":true,"draggable":false,"keyboard":true,"title":"","alt":"","zIndexOffset":0,"opacity":1,"riseOnHover":false,"riseOffset":250},["ID:  86 <br/> Epilink:  Farm A 2009 <br/> RDM clade:  A. RDM cluster","ID:  112 <br/> Epilink:  Farm A 2009 <br/> RDM clade:  B. Other cluster","ID:  434 <br/> Epilink:  School A 2009 <br/> RDM clade:  A. RDM cluster","ID:  435 <br/> Epilink:  School A 2009 <br/> RDM clade:  B. Other cluster","ID:  738 <br/> Epilink:  School A 2009 <br/> RDM clade:  B. Other cluster","ID:  739 <br/> Epilink:  School A 2009 <br/> RDM clade:  B. Other cluster","ID:  743 <br/> Epilink:  School A 2009 <br/> RDM clade:  B. Other cluster","ID:  813 <br/> Epilink:  Farm A 2009 <br/> RDM clade:  B. Other cluster","ID:  815 <br/> Epilink:  Farm A 2009 <br/> RDM clade:  B. Other cluster","ID:  1018 <br/> Epilink:  School A 2009 <br/> RDM clade:  B. Other cluster","ID:  1022 <br/> Epilink:  School A 2009 <br/> RDM clade:  B. Other cluster","ID:  1042 <br/> Epilink:  Farm B 2009 <br/> RDM clade:  B. Other cluster","ID:  1272 <br/> Epilink:  Farm B 2009 <br/> RDM clade:  B. Other cluster","ID:  1282 <br/> Epilink:  Farm B 2009 <br/> RDM clade:  B. Other cluster","ID:  2139 <br/> Epilink:  Farm B 2010 <br/> RDM clade:  A. RDM cluster","ID:  2149 <br/> Epilink:  Farm B 2010 <br/> RDM clade:  B. Other cluster","ID:  2345 <br/> Epilink:  School B 2010 <br/> RDM clade:  A. RDM cluster","ID:  2442 <br/> Epilink:  School B 2010 <br/> RDM clade:  A. RDM cluster","ID:  4344 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  4471 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  4536 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  4634 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  4646 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  4743 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  4752 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  4814 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  4823 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  5075 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  5121 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  5132 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  5700 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  5752 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  5758 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  5767 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  5781 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  5784 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  5802 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  8427 <br/> Epilink:  RDM 2014 <br/> RDM clade:  A. RDM cluster","ID:  8448 <br/> Epilink:  RDM 2014 <br/> RDM clade:  A. RDM cluster","ID:  8474 <br/> Epilink:  RDM 2014 <br/> RDM clade:  A. RDM cluster","ID:  8506 <br/> Epilink:  RDM 2014 <br/> RDM clade:  A. RDM cluster","ID:  8512 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  8514 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  8579 <br/> Epilink:  RDM 2014 <br/> RDM clade:  A. RDM cluster","ID:  8580 <br/> Epilink:  RDM 2014 <br/> RDM clade:  A. RDM cluster","ID:  8624 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  8672 <br/> Epilink:  RDM 2014 <br/> RDM clade:  A. RDM cluster","ID:  8701 <br/> Epilink:  RDM 2014 <br/> RDM clade:  A. RDM cluster","ID:  8713 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  8726 <br/> Epilink:  Sporadic <br/> RDM clade:  B. Other cluster","ID:  8745 <br/> Epilink:  RDM 2014 <br/> RDM clade:  A. RDM cluster","ID:  8814 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  9413 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  9417 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  9419 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  9712 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  9741 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  9755 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster","ID:  9796 <br/> Epilink:  Sporadic <br/> RDM clade:  A. RDM cluster"],null,{"showCoverageOnHover":true,"zoomToBoundsOnClick":true,"spiderfyOnMaxZoom":true,"removeOutsideVisibleBounds":true,"spiderLegPolylineOptions":{"weight":1.5,"color":"#222","opacity":0.5},"freezeAtZoom":false},null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[50.11714,54.97034],"lng":[-5.55063,0.170939198407307]}},"evals":[],"jsHooks":[]}</script>

``` r
# Save the map:
htmlwidgets::saveWidget(widget = rdmleaflet, 
                        file = "Map of cases - interactive.html")
```

# References

Further information about this outbreak and other spatial analyses
performed is provided in the below publication:

Butcher H, Elson R, Chattaway MA, Featherstone CA, Willis C, Jorgensen
F, Dallman T, Jenkins C, McLauchlin J, Beck C, & Harrison S (2016).
Whole genome sequencing improved case ascertainment in an outbreak of
Shiga toxin-producing Escherichia coli O157 associated with raw drinking
milk. Epidemiology and Infection, 144(13), 2812-2823.
<https://doi.org/10.1017/S0950268816000509>

# Appendix

Following feedback on this exercise from outbreak module participants in
December 2021, additional details have been provided below on two more
complex techniques that are included in the code.

## A. Creating regular expressions

At the beginning of this exercise, postcodes were re-formatted to
include a space between the outcode and incode with a function called
`add_space`. This function used regular expressions, or “regex” to
determine if a space was already present in the code or not, and where
missing, add the space as the fourth-last character in the text string.

While this has nothing specifically to do with mapping, it is quite
useful to understand and be able to construct regular expressions as
they are language agnostic (the same expressions can be used in STATA,
python, SQL etc.) and can be used for many data cleaning tasks (wherever
patterns within text strings need to be identified, extracted or
replaced).

Although this topic is too extensive to cover in this appendix, the
following resources are a useful starting point:

1.  [Regular expressions every R programmer should
    know](https://www.jumpingrivers.com/blog/regular-expressions-every-r-programmer-should-know/) -
    this blog post introduces users to some basics.
2.  [A complete beginner’s guide to regular expressions in
    R](https://regenerativetoday.com/a-complete-beginners-guide-to-regular-expressions-in-r/) -
    this is a tutorial on how to construct regular expressions, with
    some worked examples
3.  [R help page on regular
    expressions](https://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html) -
    although most regular expressions are language agnostic, a few
    shortcuts for certain commonly used expressions (for example
    searching for a string that contains only letters) have been adapted
    specifically for R. This R help page details these shortcuts and a
    few other essentials.

## B. Calculating density in contour and heatmaps

The function that actually calculates density is buried within other
functions called by `ggplot2`, which ultimately determine the colour
intensities on a heatmap, or the distance between contour lines for a
contour map. Below is a short primer explaining the basics. Some
suggested reading is also included at the end.

Contour maps and heat maps are ways of visualising the density of a
population (or cases). **Population density is the number of people per
unit area**. To visualise this on a map, we need to divide the map into
squares representing our unit area. For example, if we want to know the
density of the population per square meter, we would divide up the map
into squares, each one representing one square meter. If there are two
points (cases) in one square, this square has a density of 2, and so on.
If we changed the unit area to 0.5 meters squared, fewer points might
fall within that area, so in this example our density might be reduced
from 2 to 1 single case. Equally, if we increase the unit area to
e.g. 10 meters squared, one “square” might now include a lot more cases,
e.g. 50. Changing the unit area (or other factors that influence
density) in a contour or heatmap has a similar effect to changing the
bins in a choropleth map.

Choosing an appropriate unit for the area to calculate density is
important as it will change how the map looks. In the code we used in
this practical, the argument `alpha` within `stat_density2d` to produce
the heatmap is assigned to “level”. This is a bit obscure, but “level”
is the name of a new variable that R creates in the data set while
preparing the plot, and R fills this new variable with a density
estimate.

The default density estimate value is calculated by `ggplot2` in the
background, using the `kde2d` function (kernel density estimation in 2
dimensions) from the `MASS` package. This function in turn uses a
“normal reference distribution”, also known as “Silverman’s rule of
thumb” to calculate the bandwidths. Selecting the appropriate method to
calculate bandwidths for density estimates is a complex topic.

Some resources on bandwidth calculation have been provided below:

1.  [The importance of kernel density estimation
    bandwdith](https://aakinshin.net/posts/kde-bw/) - this very readable
    blog post explains the history behind different methods for
    calculating bandwidths and sign-posts the relevant functions in R,
    Python, Matlab and Wolfram Mathematica.
2.  [Contours of a 2D density
    estimate](https://ggplot2.tidyverse.org/reference/geom_density_2d.html) -
    this vignette shows how density estimates are calculated when making
    graphics with `ggplot2`.