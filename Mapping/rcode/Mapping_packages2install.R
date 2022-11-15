###########################################################################
# OIM R MAPPING CASE STUDY - REQUIRED PACKAGES


# 01. Check that pacman is installed --------------------------------------

# Check if the 'pacman' package is installed, if not install it:
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")



# 02. Install the remaining packages --------------------------------------

# Install the required packages with pacman:
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


############################################################################


