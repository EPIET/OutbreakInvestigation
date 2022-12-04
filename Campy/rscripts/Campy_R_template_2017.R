## ---- eval=F------------------------------------------------------------------
#  getwd()

## ----load_packages, results='hide'--------------------------------------------

# Check if the 'pacman' package is installed, if not install it:
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

# Load required packages with pacman:
pacman::p_load(rio,
               here,
               Hmisc,
               epiR,
               epitools,
               ggplot2,
               scales,
               ISOweek,
               knitr)


## ----install_packages, eval = FALSE-------------------------------------------
#  install.packages("package name in quotation makrs")

## ----source_functions---------------------------------------------------------
#These scripts need to be present in your working directory

# Adds a function to create epicurves
source(here::here("rscripts", "epicurve.v.1.8.R")) 

# Adds a function to create output similar to cctable or cstable in Stata
source(here::here("rscripts", "single.variable.analysis.v0.2.R")) 

## ----import_data--------------------------------------------------------------
campy <- rio::import(here::here("data", "campy.dta"))

## ----view_data, eval=F--------------------------------------------------------
#  # to browse your data, use the View command
#  View(campy)

## ----tabulate_data, eval=F----------------------------------------------------
#  # View  data set
#  View(campy)
#  
#  # Assess a single variable using the table function
#  table(campy$datesym, useNA = "always")
#  
#  # View number of controls and cases
#  table(campy$case, useNA = "always")

## ----summarise_data, eval=F---------------------------------------------------
#  # str provides an overview of the number of observations and variable types
#  str(campy)
#  
#  # summary provides mean, median and max values of your variables
#  summary(campy)
#  
#  # summary of age
#  summary(campy$age)
#  
#  # describe (from Hmisc package) provides no. of observations, missing values, unique levels of each variable
#  describe(campy)

## ----recode_diluted-----------------------------------------------------------
campy$diluted <- ifelse(campy$concentrated == 1 | campy$powder == 1, 1, 0)

## ----normality_test-----------------------------------------------------------
shapiro.test(campy$age)

## ----histogram_cases----------------------------------------------------------
age_hist_cases <- qplot(campy$age[campy$case == 1],
                       xlab = "Age",
                       ylab = "Count",
                       main = "Histogram of the age of the cases ",
                       binwidth = 1)
age_hist_cases

## ----histogram_controls-------------------------------------------------------
age_hist_controls <- qplot(campy$age[campy$case == 0],
                       xlab = "Age",
                       ylab = "Count",
                       main = "Histogram of the age of the controls ",
                       binwidth = 1)
age_hist_controls

## ----wilcox_age_case----------------------------------------------------------
wilcox.test(age ~ case, data = campy)

## ----ttest_age_case-----------------------------------------------------------
t.test(age ~ case, var.equal = TRUE, data = campy)

## ----epicurve-----------------------------------------------------------------
epicurve_campy <- epicurve(campy, 
                           date.col = "datesym", 
                           time.period = "day", 
                           start.at = "2009-05-25", 
                           stop.at = "2009-06-15",
                           xlab = "Date of symptom onset", 
                           ylab = "Count",
                           col.pal = 4, 
                           label.breaks = 0, 
                           epi.squares = TRUE, 
                           na.rm = TRUE)

# As epicurve_campy is a ggplot object, it is possible to tailor it as desired
epicurve_campy <- epicurve_campy +
                  # rotating the x axis label by 90               
                  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                  # adding a title
                  ggtitle("Campylobacter outbreak cases by date of onset, May-June 2009") +
                   # centring the title and reducing its size 
                  theme(plot.title = element_text(hjust = 0.5, size = 11)) 

epicurve_campy

# You can save the epicurve as follows
ggsave(filename = "epicurve.png")

## ----factorise_variables------------------------------------------------------
# We list the outcome/exposure variables
vars <- c("case", 
          "sex", 
          "supply", 
          "tap", 
          "bottled", 
          "filter", 
          "well", 
          "pacifier1", 
          "pacifier2", 
          "dishwasher", 
          "microwave1", 
          "microwave2", 
          "breastfeeding", 
          "concentrated", 
          "powder", 
          "freshmilk", 
          "dilutetap", 
          "diluted")


# Convert all of those variables to factor variables and re-order the levels to aid interpretation
for (var in vars) {
  campy[,var] <- factor(campy[,var],levels = c(1,0)) 
}

## ----calculate_odds_ratios----------------------------------------------------
# Create a table with exposure and outcome variables
sex <- table(campy$sex, campy$case)

# Apply epi.2by2 function to the table
uni_sex <- epi.2by2(sex, method = "case.control")
uni_sex

supply <- table(campy$supply, campy$case)
uni_supply <- epi.2by2(supply, method = "case.control")
uni_supply

well <- table(campy$well, campy$case)
uni_well <- epi.2by2(well, method = "case.control")
uni_well

## ----calculate_or_loop--------------------------------------------------------
vars2 <- c("sex", 
           "supply", 
           "tap", 
           "bottled", 
           "filter", 
           "well", 
           "pacifier1", 
           "pacifier2", 
           "dishwasher", 
           "microwave1", 
           "microwave2", 
           "breastfeeding", 
           "concentrated", 
           "powder", 
           "freshmilk", 
           "dilutetap", 
           "diluted")

# Create an empty list to store the output of the loop
output <- list()

for (var in vars2) {
  # We make a table with each exposure variable and the case variable
  table <- table(campy[,var], campy$case) 
  # apply epi.2by2 function to each table
  uni_table <- epi.2by2(table, method = "case.control")
  # Save the results in the output list
  output[[var]] <- uni_table
}

output

## ----factor2numeric-----------------------------------------------------------
vars <- c("case", 
          "sex", 
          "supply", 
          "tap", 
          "bottled", 
          "filter", 
          "well", 
          "pacifier1", 
          "pacifier2", 
          "dishwasher", 
          "microwave1", 
          "microwave2", 
          "breastfeeding", 
          "concentrated", 
          "powder", 
          "freshmilk", 
          "dilutetap", 
          "diluted")


# Convert factor to character to numeric
for (var in vars) {
  campy[,var] <- as.numeric(as.character(campy[,var])) 
}

## ----calculate_risk_ratios_cctable--------------------------------------------
vars2 <- c("sex", 
           "supply", 
           "tap", 
           "bottled",
           "filter",
           "well", 
           "pacifier1",
           "pacifier2",
           "dishwasher",
           "microwave1",
           "microwave2",
           "breastfeeding",
           "concentrated",
           "powder",
           "freshmilk",
           "dilutetap", 
           "diluted")


# Use the sva function, specifying each element of the function
a <- sva(campy, 
         outcome = "case", 
         exposures = c(vars2),
         measure = "or", 
         verbose = TRUE)


## ----convert_rr_output_to_table-----------------------------------------------
kable(a, digits = 2)

## ----factorise_variables_again------------------------------------------------
# The outcome and exposure variables were defined above as vars

# Convert all of those variables to factor variables and re-order the levels to aid interpretation
for (var in vars) {
  campy[,var] <- factor(campy[,var],levels = c(1,0)) 
}

## ----tap_or-------------------------------------------------------------------
tap <- table(campy$tap, campy$case)
uni_tap <- epi.2by2(tap, method = "case.control")
uni_tap

## ----stratify_by_water--------------------------------------------------------
# Based on supply = 1
tap1 <- table(campy$tap[campy$supply == 1], campy$case[campy$supply == 1])
tap_supp1 <- epi.2by2(tap1, method = "case.control")
tap_supp1

# Based on supply = 0
tap0 <- table(campy$tap[campy$supply == 0], campy$case[campy$supply == 0])
tap_supp0 <- epi.2by2(tap0, method = "case.control")
tap_supp0

