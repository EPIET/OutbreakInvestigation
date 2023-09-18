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


# Set CRAN mirror (optional) ----------------------------------------------

# Set CRAN mirror:
cran_mirror <- getOption("repos")
cran_mirror["CRAN"] <- "https://cran.ma.imperial.ac.uk/"
options(repos = cran_mirror)


# Install pacman ----------------------------------------------------------

# Check if the 'pacman' package is installed, if not install it:
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")



# Install remaining packages ----------------------------------------------

# Check for and install packages required to compile R markdown case study:
pacman::p_load(knitr,
               bookdown,
               tufte, 
               update = TRUE)


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
               #EpiStats,
               epikit, 
               apyramid, 
               scales, 
               update = TRUE)

# Temporarily install EpiStats package from Github release:
pacman::p_load_gh(
  "Epiconcept-Paris/EpiStats@release_1.4-1_2020-04-21"
)


###########################################################################

