###############################################
# GIS TUTORIAL - R PACKAGE INSTALLATIONS
###############################################



# Step 00. Start a fresh R session ----------------------------------

# Packages already loaded in the background may prevent smooth installation.
# It is therefore recommended that you close any other open scripts before
# running this script.

# You can then start a fresh R session within RStudio by going to:
# Session (RStudio toolbar) --> select 'Restart R'
# If this is successful you should see a message 'Restarting R ...'
# in the console.


# Step 01. install pacman package -----------------------------------------

# Check if the 'pacman' package is installed, if not install it:
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")



# Step 02. Install remaining packages -------------------------------------

# Install the required packages:
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

# Note that this code will also update any packages on this list that are 
# already in your library, if there is a more up-to-date version on CRAN.

# If you have installed some of the packages relatively recently, you can set 
# this to false.

# You may be prompted to update some dependent packages too; if so, choose to 
# update the ones on CRAN.

# You can also use the pacman::p_load() function to load the required packages 
# into your current R session.  

##############################################################################
