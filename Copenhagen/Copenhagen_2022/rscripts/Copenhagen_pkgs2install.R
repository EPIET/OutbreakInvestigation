########################################################
# COPENHAGEN CASE STUDY - PACKAGES TO INSTALL
########################################################

# INSTRUCTIONS:

# Open a fresh RStudio session
# Make sure no scripts, files or data are open.
# Go to Session --> Restart R and select it to start a fresh R session
# Highlight this script and press the run button
# Do not attempt to do anything in R until it is finished; 
# The script will take a few minutes
# Happy R coding!


# Install pacman ----------------------------------------------------------

# Check if the 'pacman' package is installed, if not install it:
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")



# Install remaining packages ----------------------------------------------

# Load the required libraries into the current R session:
pacman::p_load(rio, 
               here, 
               tidyverse, 
               skimr, 
               janitor,
               lubridate,
               gtsummary, 
               flextable,
               officer,
               EpiStats,
               epikit, 
               apyramid, 
               scales, 
               update = TRUE)

###########################################################################

