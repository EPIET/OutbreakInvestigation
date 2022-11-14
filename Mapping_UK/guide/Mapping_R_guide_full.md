Mapping in R: STEC raw milk tutorial
================
Adapted from material by Daniel Gardiner and Amy Mikhail
10 December 2021

-   [Preamble](#preamble)
-   [How to use this resource](#how-to-use-this-resource)
-   [Preparation and package
    installs](#preparation-and-package-installs)
    -   [Installing required packages](#installing-required-packages)
    -   [File organization and
        housekeeping](#file-organization-and-housekeeping)
-   [Introduction to the tutorial](#introduction-to-the-tutorial)
    -   [Background](#background)
    -   [Data sets](#data-sets)
    -   [Questions to answer](#questions-to-answer)
-   [Geospatial investigations in R](#geospatial-investigations-in-r)
    -   [Reading in the data](#reading-in-the-data)
    -   [Geocoding the data](#geocoding-the-data)
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
    -   [A. Cleaning address data with regular
        expressions](#a-cleaning-address-data-with-regular-expressions)
        -   [Postcode cleaning:](#postcode-cleaning)
        -   [More on regular expressions](#more-on-regular-expressions)
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
questions posed in this case study.

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
easier to write and read.

If you are working with very large data sets, however, the `data.table`
package may be more suitable as it has been designed for fast execution
and to minimise demands on available memory (RAM). A relatively new
package, `dtplyr` has been created for those who want to take advantage
of the greater speed and efficacy of `data.table` but prefer to use
`dplyr` syntax.

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

# Preparation and package installs

To work through this tutorial, you will need:

-   a recent version of R (>= 4.1.x) and Rstudio (>= 2022.x)
-   required packages installed (see below)
-   internet connection (required for geocoding and downloading static
    maps)
-   this guide (provided in the course materials folder, also
    [accessible
    online](https://github.com/EPIET/OutbreakInvestigation/blob/master/Mapping_UK/guide/Mapping_R_guide.md))
-   the data sets (provided in the course materials folder)

## Installing required packages

This section describes how to install and load the packages that you
will need for this mapping tutorial. Some brief details on why each
package was selected are provided below:

-   `pacman` - Checks for, installs (if needed) and loads multiple
    packages at once
-   `rio` - import multiple different data types with a single command
-   `here` - import and export data using relative file paths
-   `janitor` - handy functions for data cleaning
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

To ensure that the package installation goes smoothly, you have been
provided with a separate package installation script.

1.  Prior to running this script, you should close any open instances of
    R and RStudio
2.  Open a fresh instance of RStudio
3.  Within RStudio, browse for and open the package installation script
4.  Restart R by going to `Session` and then `Restart R` in the top
    toolbar of the RStudio menu as shown in the image below.
5.  Run the code in the script as instructed, to install the packages.
6.  If prompted to update or install additional packages, select the
    `From CRAN` option.
7.  If you encounter any issues, please contact the organizers for help.

![Restarting R from RStudio](images/Restart_r.png)

After starting a fresh RStudio and R session, you will first need to
install the `pacman` package. This package has a function which makes
installing and loading other packages much easier. The code below will
first check to see if `pacman`is already installed in your R library,
and if not, install it:

``` r
##################################################################
# INSTALL PACKAGES

# Check if the 'pacman' package is installed, if not install it:
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
```

Next, you can use the `pacman::p_load()` function to check if the rest
of the packages required for this tutorial are already installed in your
R library. Any missing packages will be automatically installed with
this function.

``` r
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
               htmlwidgets, 
               install = TRUE, 
               update = TRUE)
```

    ## package 'blob' successfully unpacked and MD5 sums checked
    ## 
    ## The downloaded binary packages are in
    ##  C:\Users\amymi\AppData\Local\Temp\RtmpIZ0mVB\downloaded_packages

Note that each time you restart R or RStudio, you will need to rerun the
`pacman::p_load()` function to load the required libraries into your
current session. We therefore suggest you copy this code and paste it at
the top of the R script that you will use for this exercise.

The argument `update = TRUE` will automatically install the most recent
versions of the listed packages if those versions are higher than the
ones in your R library. Once you have installed the packages for this
course, we suggest setting this option to `FALSE` in your R script.

If you have any problems with package installation or loading, please
contact the facilitators for help before proceeding.

## File organization and housekeeping

In this tutorial, files are imported and outputs exported using relative
file paths with the help of the `here` package.

1.  For this to work seamlessly, we suggest that you download the folder
    containing all the course materials and save it on your desktop.
2.  Next, open the folder and double-click on the the ‘Mapping_UK.Rproj’
    file, which will open a new instance of RStudio.  
3.  If you click on the ‘files’ pane in RStudio, you should now be able
    to see all the sub-folders containing the course materials.
4.  Go to the `File` menu in RStudio and select `New file --> R script`.
5.  Name the R script (e.g. `My_GIS_code.R`) and save it in the same
    folder.
6.  Add the `pacman::p_load()` function including the required packages
    as shown in the code chunk above to the top of your script, changing
    the `update` argument to `FALSE`.
7.  Run this code to load the required libraries into your current R
    session.

You are now ready to begin.

# Introduction to the tutorial

## Background

For this exercise, you have been provided with a line list of cases from
an outbreak associated with raw drinking milk that was contaminated with
shiga toxin-producing *Escherichia coli* (STEC), which occurred in 2014.

Initially, the outbreak was identified after 9 people who had visited a
farm producing raw milk in the South West of England in September and
October 2014 fell ill. The farm routinely produced and sold raw milk at
local markets, but was also open to visitors in the spring and summer
time. Visitors were exposed either through interacting with the animals
and their environment or from drinking a sample of the raw milk. Samples
of raw milk from this farm were also tested and positive for the same
strain of STEC as cases.

Sequencing the isolates from the 9 cases revealed that they fell into
the same 5-single nucleotide polymorphism (SNP) cluster (i.e. were very
closely related genetically). Isolates from the raw drinking milk also
fell into this cluster.

Surprisingly, the whole genome sequencing cluster also included isolates
from 36 other cases that were not known to have visited the implicated
farm during the outbreak period. Twelve of these cases were new
(identified late 2014 or early 2015) and had not yet been associated
with any outbreak. The remaining additional cases were historic
(identified between 2009 and 2012) and had been associated with other
outbreaks linked to two farms and two schools which were also located in
the South West of England. The OCT decided to include the other cases
associated with the historic outbreaks in the line list too.

Information on the exposures of both historic and new cases was
available, because the National STEC enhanced surveillance system had
been implemented in 2009. The routine enhanced surveillance
questionnaire for STEC specifically asks about exposure to raw milk,
farm visits and any overnight stays outside the normal place of
residence in the 14 days before symptom onset. Where possible, the full
addresses (including UK postcode) of any locations visited during the
incubation period are recorded on the questionnaire.

Routine whole genome sequencing for STEC was piloted from January 2014,
with retrospective sequencing of randomly selected samples from all
outbreaks investigated in the previous 5 years. The outbreak control
team is interested to know whether the other cases with isolates in the
same 5-SNP WGS cluster may also have been exposed to raw milk or farm
animals from the same region in England; to determine this they have
asked you to undertake some geospatial analysis.

To conduct your analysis, the OCT has provided you with a case line list
that includes information on whole genome sequencing clustering,
association with investigated outbreaks, geospatial exposures, and
exposure to raw animal products.

The following variables will be critical for your analysis:

-   `wgs_rdm_cluster`: Indicates if case isolates are part of the WGS
    cluster associated with this raw drinking milk (RDM) outbreak
    (`TRUE` if yes, `FALSE` if they are from a different WGS cluster)
-   `epi_outbreak`: Indicates which outbreak investigation each case is
    linked to. Options are this outbreak (RDM 2014), Farm A, Farm B,
    School A, School B, or sporadic (not linked to an outbreak).
-   `postcode_home`: UK postcode of residence
-   `postcode_uk_travel`: UK postcode of other overnight stays in the 14
    days before onset
-   `postcode_exposure`: UK postcode where exposure to raw milk most
    likely occurred.

Note that `postcode_uk_travel` only includes places other than the
case’s normal residence where they spent at least one night, while
`postcode_exposure` is the postcode of the location where the OCT thinks
exposure for that case most likely occurred; this also includes day
visits to farms, or the case’s own residence if they live close to the
implicated farm.

Note that the `postcode_exposure` for all 9 cases in the `RDM 2014` is
the postcode of the farm in the South West of England that had been
identified as the source of infection for this outbreak.

## Data sets

For this tutorial, the following data sets have been provided:

1.  `CaseStudy_RDM_anon_data.csv` - raw data set
2.  `CaseStudy_RDM_anon_data_cleanpcodes.csv` - raw data with
    pre-cleaned postcodes
3.  `CaseStudy_RDM_anon_data_coords.csv` - raw data with geographic
    coordinates added
4.  `PHEC_population.csv` - regional population data for choropleth
    incidence maps

We suggest that you start working with the secomd data set, that has
pre-cleaned postcodes as the function required to clean the postcodes is
a little complex, and this will be described later in the tutorial.

The third data set has been provided as a back-up in case you have
difficulties geocoding the three postcode variables. Geocoding can
sometimes be very slow or not work if there is too much traffic to the
server at the same time.

## Questions to answer

As explained in the introduction, the OCT were surprised to learn that
some additional cases, both historic and new, were also linked to this
outbreak via whole genome sequencing. By mapping the cases in different
ways and looking at their geo-spatial distribution, what conclusions can
you draw about the following questions?

1.  Does the geo-spatial distribution of cases confirm or refute the
    hypothesis that the source of infection for all cases in the WGS
    cluster is in the South West region?
2.  Based on their geo-spatial distribution over time, are the historic
    cases likely to have been exposed to the same source of infection as
    cases from the current RDM outbreak or a different source?
3.  Overall, does the geo-spatial evidence best support the cluster
    defined by whole genome sequencing (`wgs_rdm_cluster`) or by
    epidemiological links (`epi_outbreak`)?

# Geospatial investigations in R

In this section, the example code below shows how to iteratively build
different types of maps that will be helpful for your investigation. To
complete your exploration of the data and answer the questions, you will
need to apply this code to the three different postcode locations,
stratifying by WGS cluster, epi outbreak, or other variables that seem
useful to you.

## Reading in the data

In this section you can read in (import) the raw data sets into R. You
can find the data sets in a sub-folder called `data` which should be in
the folder of course materials provided to you for this exercise.

We suggest you use the package `rio` to import the data as this package
simplifies data imports by being able to import multiple different types
of data (.csv, .xls, .xlsx, .rds) with a single command:
`rio::import()`.

This code also uses the `here` package as it provides some simple syntax
for building relative file paths, that will work on any computer when
you share your code provided everyone starts from a .Rproj file in the
root of the course materials folder.

Lastly before starting to interrogate the data, we will use the
`clean_names()` function from the `janitor` package. This function
removes non-alphanumeric characters and spaces from column names in a
data set, to make them easier to reference in r code. It will also
change all the column names to lower case. Spaces are replaced with an
underscore.

``` r
##################################################################
# IMPORT RAW DATA:

# Use this data set for the first part of this case study:
rdmdata <- rio::import(file = here("data", "CaseStudy_RDM_anon_data_cleanpcodes.csv"))

# Use this data set if you want to try cleaning postcodes and geocoding yourself:
# rdmdata <- rio::import(file = here("data", "CaseStudy_RDM_anon_data.csv"))

# Use this data set if you have trouble geocoding:
# rdmdata <- rio::import(file = here("data", "CaseStudy_RDM_anon_data_coords.csv"))

# Clean variable names:
rdmdata <- rdmdata %>% 
  janitor::clean_names()


##################################################################
# Read in population data for health regions (called PHE centres):
phecpop <- rio::import(file = here("data", "PHEC_population.csv"))

# Clean variable names:
phecpop <- phecpop %>% 
  janitor::clean_names()
```

## Geocoding the data

**Geocoding** (the process of converting addresses to geographic
coordinates) is done by using an address-to-coordinates lookup table,
which is typically accessed from either a public database or a local
mirror. It can be challenging to geocode using the public databases as
in order to ensure fair use, these often have a cap on the number of
postcodes you can geocode at one time. Conversely, geocoding from a
local mirror database created by your institution requires permission to
access that database from your organisation and can generally only be
accessed via your organisational account.

In this tutorial, we will use the package `tidygeocoder`, which can
derive geographic coordinates (longitude and latitude) given a full
address from any country. As UK postcodes are sufficiently specific to
provide a longitude and latitude for a given address, we can geocode
these with `tidygeocoder` provided that we also provide the country
name. In preparation for this step, we will add a column to the dataset
called `country`:

``` r
# Add a new column to the case data indicating the country:
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
                        long = "home_long") %>% 
  tidygeocoder::geocode(postalcode = postcode_uk_travel, 
                        country = country, 
                        method = "osm", 
                        lat = "travel_lat", 
                        long = "travel_long") %>% 
  tidygeocoder::geocode(postalcode = postcode_exposure, 
                        country = country, 
                        method = "osm", 
                        lat = "exposure_lat", 
                        long = "exposure_long")
```

    ## Passing 59 addresses to the Nominatim single address geocoder

    ## Query completed in: 60.6 seconds

    ## Passing 59 addresses to the Nominatim single address geocoder

    ## Query completed in: 60.5 seconds

    ## Passing 47 addresses to the Nominatim single address geocoder

    ## Query completed in: 48.3 seconds

You now have new variables containing latitude and longitude for the
three postcode variables as follows:

-   `postcode_home` –> `home_lat` and `home_long`
-   `postcode_uk_travel` –> `travel_lat` and `travel_long`
-   `postcode_exposure` –> `exposure_lat` and `exposure_long`

These variables will be used for mapping the cases.

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

![](Mapping_R_guide_full_files/figure-gfm/boundary%20box%20UK-1.png)<!-- -->

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

![](Mapping_R_guide_full_files/figure-gfm/boundary%20box%20England-1.png)<!-- -->

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

![](Mapping_R_guide_full_files/figure-gfm/boundary%20box%20case%20coordinates-1.png)<!-- -->

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
             aes(x = exposure_long, y = exposure_lat), 
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

![](Mapping_R_guide_full_files/figure-gfm/point%20map%20basic-1.png)<!-- -->

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
stratification is called `wgs_rdm_cluster`.

``` r
# Update the RDM clade variable (give it pretty labels):
rdmdata <- rdmdata %>% 
  mutate(wgs_rdm_cluster = case_when(wgs_rdm_cluster == FALSE ~ "B. Other cluster", 
                                     wgs_rdm_cluster == TRUE ~ "A. RDM cluster")) 
          

# Create the stratified map:
stratamap <- ggmap::ggmap(coordinatesmap, 
                         extent = 'panel', 
                         maprange = FALSE) +
  geom_point(data = rdmdata, 
             aes(x = home_long, y = home_lat, fill = wgs_rdm_cluster), 
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

![](Mapping_R_guide_full_files/figure-gfm/point%20map%20with%20stratification-1.png)<!-- -->

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
             aes(x = home_long, y = home_lat, fill = wgs_rdm_cluster), 
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

![](Mapping_R_guide_full_files/figure-gfm/static%20map%20with%20shapefile%20stratified%20by%20WGS%20clade-1.png)<!-- -->

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

![](Mapping_R_guide_full_files/figure-gfm/static%20map%20time%20series-1.png)<!-- -->

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

![](Mapping_R_guide_full_files/figure-gfm/contourmap%20of%20case%20counts-1.png)<!-- -->

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

![](Mapping_R_guide_full_files/figure-gfm/heatmap%20of%20case%20counts-1.png)<!-- -->

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
    `phecnm`)
2.  Add the population for each PHE Centre to this table from the data
    set `phecpop`
3.  Calculate incidence per 100 000 population and add this to the table
4.  Merge this table with the sf object containing the polygon
    coordinates `phecll`

We will then use the `ggplot2` package to create the choropleth map.

``` r
# Create the table of summary counts of cases by PHE Centre:
incidencetab <- rdmdata %>% 
  group_by(phecnm) %>% 
  summarise(cases = length(caseid))

# # Add population data to the table:
incidencetab <- phecpop %>% 
  left_join(incidencetab, by = "phecnm")


# Change NA counts to 0 for incidence calculation:
incidencetab <- incidencetab %>% 
  mutate(cases = ifelse(is.na(cases), 0, cases))

# Calculate crude incidence per 100,000 population:
incidencetab <- incidencetab %>% 
  mutate(Incidence = round((cases/population)*100000, 2))


# Merge incidence and case counts with sf object:
phecll <- phecll %>% 
  left_join(incidencetab, by = c("PHECNM" = "phecnm"))
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

![](Mapping_R_guide_full_files/figure-gfm/choropleth%20map%20of%20incidence-1.png)<!-- -->

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

If desired, you can also superimpose the boundaries of health regions
ontop of a leaflet map, with the `addPolygons()` command, using an sf
(shape file) object as input. An example of this is shown in the code
below.

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
  # Add transformed shapefile of regions
  addPolygons(data = phecll, weight = 5, col = "black") %>% 
 # Add markers with descriptive labels:
  addMarkers(lng = rdmdata$home_long,
             lat = rdmdata$home_lat,
             popup = paste("ID: ", rdmdata$caseid, "<br/>",
                           "Epilink: ", rdmdata$epi_outbreak, "<br/>", 
                           "RDM clade: ", rdmdata$wgs_rdm_cluster), 
             clusterOptions = markerClusterOptions()) 

# View the map:
rdmleaflet
  
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

## A. Cleaning address data with regular expressions

### Postcode cleaning:

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

# Read in the raw data set (where postcodes are not in the correct format):
rdmdata <- rio::import(file = here("data", "CaseStudy_RDM_anon_data.csv"))

# Use the add_space function to correct postcode format:
rdmdata <- rdmdata %>% 
  rowwise() %>% 
  mutate(postcode_home = add_space(postcode_home)) %>% 
  mutate(postcode_uk_travel = add_space(postcode_uk_travel)) %>% 
  mutate(postcode_exposure = add_space(postcode_exposure))
```

### More on regular expressions

In the above code, postcodes were re-formatted to include a space
between the outcode and incode with a function called `add_space`. This
function used regular expressions, or “regex” to determine if a space
was already present in the code or not, and where missing, add the space
as the fourth-last character in the text string.

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
