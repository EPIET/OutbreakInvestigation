## ----load_pacman--------------------------------------------------------------

# Check if the 'pacman' package is installed, if not install it:
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")



## ----load_libraries-----------------------------------------------------------

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
               scales)


## ----working_directory, eval=FALSE--------------------------------------------
#  
#  here::here()
#  

## ----import_raw_data----------------------------------------------------------

# Import the raw data set:
linelist <- rio::import(here::here("data", "Copenhagen_raw.csv"))


## ----check_data---------------------------------------------------------------

# Skim the data to get a summary of the column types and values:
skimr::skim(linelist)


## ----check_date_format--------------------------------------------------------

# Check date element order:
head(linelist$dayonset)


## ----correct_date_format------------------------------------------------------

# Update linelist:
linelist <- linelist %>% 
  
  # Change column to date class:
  dplyr::mutate(dayonset = lubridate::dmy(dayonset))

# Check class of updated column:
class(linelist$dayonset)

# Check that updated format is correct:
head(linelist$dayonset)


## ----onset_histogram----------------------------------------------------------

# Check distribution of onset dates with a histogram:
hist(linelist$dayonset, breaks = "day")


## ----format_time_col----------------------------------------------------------

# Check format of time variable:
head(linelist$starthour)


## ----combine_date_time--------------------------------------------------------

linelist <- linelist %>% 
  # Combine dayonset and starthour in a new date time variable:
  mutate(onset_datetime = if_else(!is.na(dayonset) & 
                                    !is.na(starthour),
                                  lubridate::ymd_h(
                                    paste(dayonset, starthour)), 
                                  NA_POSIXct_))


## ----dayonset_format----------------------------------------------------------

head(linelist$dayonset)


## ----starthour_format---------------------------------------------------------

head(linelist$starthour)


## ----onset_datetime_format----------------------------------------------------

head(linelist$onset_datetime)


## ----skim_dose_cols-----------------------------------------------------------

drskim <- linelist %>% 
  # Select all columns with names that end in upper case 'D':
  select(ends_with("D", ignore.case = FALSE)) %>% 
  # Produce the skim summary table
  skimr::skim()


## ----maxrange_dose_cols-------------------------------------------------------

# Check range of maximum values across the selected columns:
range(drskim$numeric.p100)


## ----check_dose_cols----------------------------------------------------------

# Create summary table for dose response columns:
drtable <- linelist %>% 
  
  # Select all the columns with column names that end in upper case 'D':
  select(ends_with("D", ignore.case = FALSE)) %>% 
  
  # Create the summary table, excluding missing values:
  gtsummary::tbl_summary(missing = "no") %>% 
  
  # Convert to flextable:
  gtsummary::as_flex_table()

# Print the summary table:
drtable


## ----format_logical_cols------------------------------------------------------

# Convert cols to logical:
linelist <- linelist %>% 
  
  mutate(across(
    
    # Select columns that are numeric and where all values are 0, 1 or NA
    .cols = where(function(x) is.numeric(x) & all(x %in% c(0, 1, NA))), 
    
    # Convert columns matching these criteria to logical
    .fns = as.logical))



## ----hist_age-----------------------------------------------------------------

hist(linelist$age)


## ----factor_group-------------------------------------------------------------

linelist <- linelist %>% 
  
  # Convert group to a factor and label 0 as teacher, 1 as student:
  mutate(group = factor(group, labels = c("teacher", "student")))


## ----crosstab_group_age-------------------------------------------------------

# Create cross-tab:
janitor::tabyl(dat = linelist, age, group)


## ----age_recode---------------------------------------------------------------

# Update incorrect ages to the correct values with case_when:
linelist <- linelist %>% 
  
  mutate(age = 
           case_when(
             # Where the respondent is 16 and a teacher, change their age to 61:
             age == 16 & group == "teacher" ~ 61, 
             # where the respondent is 8 or 180 and a student, change their age to 18:
             age %in% c(8, 180) & group == "student" ~ 18, 
             # Keep remaining values as is: 
             TRUE ~ as.numeric(age)
             )
         )


## ----crosstab_group_age_corrected---------------------------------------------

# Create cross-tab:
janitor::tabyl(dat = linelist, age, group)


## ----factor_class_levels------------------------------------------------------

# Check variable sex:
janitor::tabyl(linelist, sex)

# Check variable class:
janitor::tabyl(linelist, class)


## ----factor_convert_class2----------------------------------------------------

# Get linelist and pipe it in:
linelist <- linelist %>% 
  
  # Now call 'mutate' to update the variables:
  mutate(
    sex = factor(sex, labels = c("female", "male")),
    class = factor(class)
    )


## ----skim_clean_final---------------------------------------------------------

# Final skim of the data before analysis:
skimr::skim(linelist)


## ----incubation_col-----------------------------------------------------------

# Start with linelist:
linelist <- linelist %>% 
  
  # Create new column for meal date and time:
  mutate(meal_datetime = lubridate::ymd_hm("2006-11-11 18:00"))
  

## ----define_gastrosymptoms----------------------------------------------------

# Define a list of gastro symptom columns:
gastro_cols <- c("diarrhoea", "bloody", "vomiting")


# Start with linelist:
linelist <- linelist %>% 
  
  # Create a new column called gastrosymptoms:
  mutate(gastrosymptoms = case_when(
    
    # gastrosymptoms = FALSE if none of the gastro columns are TRUE:
    if_all(.cols = all_of(gastro_cols), 
           .fns = ~ !. %in% c(TRUE)) ~ FALSE,
    
    # gastrosymptoms = TRUE if any of the gastro columns are TRUE:
    if_any(.cols = any_of(gastro_cols), 
           .fns = ~ . == TRUE) ~ TRUE
    
  ))



## ----no_gastrosymptoms--------------------------------------------------------
# How many people had no data for any of the symptoms of interest?
nosymptoms <- linelist %>% 
  filter(if_all(.cols = all_of(gastro_cols), .fns = ~ is.na(.))) %>% 
  count() %>% 
  pull()

# Print result:
nosymptoms


## ----no_gastrosymptoms_meal---------------------------------------------------

# Of these, how many also had a meal at the school dinner party?
nosymptoms_meal <- linelist %>% 
  filter(if_all(.cols = all_of(gastro_cols), 
                .fns = ~ is.na(.)) & meal == TRUE) %>% 
  count() %>% 
  pull()

# Print results:
nosymptoms_meal


## ----exposure_cols------------------------------------------------------------

# Get list of variable names in the data:
names(linelist)

# Choose the exposure variables:

# Create list of exposure variables to analyse:
exposure_cols <- c("tuna", 
                   "shrimps", 
                   "green", 
                   "veal", 
                   "pasta", 
                   "rocket", 
                   "sauce", 
                   "bread", 
                   "champagne", 
                   "beer", 
                   "redwine", 
                   "whitewine")


## ----define_ate_anything------------------------------------------------------

# Start with the linelist:
linelist <- linelist %>% 
  
  # Create ate_anything column:
  mutate(ate_anything = case_when(
    
    # ate_anything = FALSE if none of the exposure columns are TRUE:
    if_all(.cols = all_of(exposure_cols), 
           .fns = ~ !. %in% c(TRUE)) ~ FALSE,
    
    # ate_anything = TRUE if any of the exposure columns are TRUE:
    if_any(.cols = any_of(exposure_cols), 
           .fns = ~ . == TRUE) ~ TRUE

  ))


## ----check_meal---------------------------------------------------------------

# Start with linelist:
linelist %>% 
  
  # Cross-tabulate meal with ate_anything:
  tabyl(meal, ate_anything)


## ----recode_meal--------------------------------------------------------------

# Start with linelist:
linelist <- linelist %>% 
  
  # Change respondents who ate a food or drink item to meal = TRUE:
  mutate(meal = if_else(
    condition = meal == FALSE & ate_anything == TRUE, 
    true = TRUE, 
    false = meal
    ))


## ----define_casedef_nomaxtime-------------------------------------------------

# Start with linelist:
linelist <- linelist %>% 

  # Create case definition:
  mutate(
    case = case_when(
      
      # Cases have any gastro symptoms with onset after the meal:
      meal == TRUE & 
        gastrosymptoms == TRUE & 
        !is.na(onset_datetime) & 
        (onset_datetime >= meal_datetime) 
      ~ TRUE, 
      
      # Non cases have no gastro symptoms but ate a meal at the party:
      meal == TRUE & 
        (gastrosymptoms == FALSE | 
           # ... or have gastro symptoms but onset is before the meal:
           (gastrosymptoms == TRUE & (onset_datetime < meal_datetime))) 
      ~ FALSE,
      
      # Define excluded individuals as those with no record of a meal:
      meal == FALSE | is.na(meal) 
      ~ NA
      
      )
    )


## ----casedef_nomax_check------------------------------------------------------

# First check how many people ate a meal at the dinner party:
atemeal <- linelist %>% 
  filter(meal == TRUE) %>% 
  count(meal) %>% 
  pull()

# Print the result:
atemeal

# Next, check how many people ate a meal AND had onset after the meal:
atemealsick <- linelist %>% 
  filter(meal == TRUE & (onset_datetime >= meal_datetime)) %>% 
  count(meal) %>% 
  pull()

# Print the result:
atemealsick

# Finally check how many people ate a meal AND fell ill afterwards with 
# any of diarrhoea, bloody diarrhoea, or vomiting:
atemealsickcase <- linelist %>% 
  filter(meal == TRUE & 
           (diarrhoea == TRUE | bloody == TRUE | vomiting == TRUE) & 
           (onset_datetime >= meal_datetime)) %>% 
  count(meal) %>% 
  pull()

# Print the result:
atemealsickcase


## ----define_incubation--------------------------------------------------------

# First, we can makes sure that only case incubation times are examined:
linelist <- linelist %>% 
  
  # Update incubation to be NA if case is not TRUE:
  mutate(incubation = ifelse(test = case == TRUE, 
                             yes = onset_datetime - meal_datetime, 
                             no = NA))


## ----median_incubation--------------------------------------------------------

# Median incubation time:
med_incubation <- median(linelist$incubation, na.rm = TRUE)

# Print the result:
med_incubation


## ----check_case_before_update-------------------------------------------------

# Tabulate case:
janitor::tabyl(dat = linelist, case)


## ----update_case_def----------------------------------------------------------

# Update the case definition to limit to onset three days after meal:
linelist <- linelist %>% 
  
  mutate(case = case_when(
    
    # If respondent is a case but onset is more than two days after the meal
    # Change them to a non-case (FALSE)
    case == TRUE & (onset_datetime > (meal_datetime + days(2))) ~ FALSE, 
    
    # For everyone else, keep their current case status as is:
    TRUE ~ case
    
    )
  )


## ----check_case_after_update--------------------------------------------------

# Retabulate cases to see if anything has changed:
janitor::tabyl(dat = linelist, case)


## ----check_onset_after_update-------------------------------------------------

# Cross-tabulate onset date time with case status:
janitor::tabyl(dat = linelist, onset_datetime, case) %>% 
  adorn_totals()


## ----view_future_visitors, eval=FALSE-----------------------------------------
#  
#  # Subset and open the records in the viewer:
#  View(subset(linelist, onset_datetime == ymd_hms("2020-06-11 11:00:00")))
#  

## ----drop_nacase--------------------------------------------------------------

linelist <- linelist %>%

  # Remove rows where case is NA:
  drop_na(case)


## ----export_clean_data--------------------------------------------------------

rio::export(x = linelist, 
            file = here::here("data", "Copenhagen_clean.xlsx"))


## ----import_clean_data--------------------------------------------------------

# Import the cleaned data set using rio and here packages:
linelist <- rio::import(here::here("data", "Copenhagen_clean.xlsx")) %>% 
  
  # Convert character columns to factors:
  mutate(across(.cols = where(is.character), 
                .fns = as.factor))


## ----sex_tabyl----------------------------------------------------------------

linelist %>% 
  
  janitor::tabyl(case, sex)


## ----sex_gtsummary------------------------------------------------------------

tabsex <- linelist %>%
  
  # Select person characteristics to summarise:
  select(case, age, sex, group, class) %>% 
  
  # Create the summary table:
  gtsummary::tbl_summary(
    
    # Stratify by case:
    by = case, 
    
    # Statistics to use for continuous and categorical variables:
    statistic = list(all_continuous() ~ "{mean} ({sd})",        
                     all_categorical() ~ "{n} ({p}%)"), 
    
    # Calculate row percentages:
    percent = "row",
    
    # Don't show missing values:
    missing = "no"
    
  ) %>% 
  
  # Add tests of statistical significance of differences between groups:
  add_p(test = list(all_continuous() ~ "t.test",       # Continuous variables
                    all_categorical() ~ "chisq.test"), # Categorical variables
        
        # Identify grouping variable:
        group = case,
                 
        # Define any test arguments that deviate from the default:
        test.args = list(all_tests("t.test") ~ 
                           list(var.equal = FALSE), 
                         all_tests("chisq.test") ~ 
                           list(simulate.p.value = TRUE))) %>% 
  
  # Add totals:
  add_overall() %>% 
  
  # Individually label which row had which statistical test: 
  separate_p_footnotes() %>% 
  
  # Make variable names bold and italics:
  bold_labels() %>% 
  italicize_labels() %>% 
  
  # Modify header:
  modify_header(
    label = "**Characteristic**",
    stat_0 = "**Overall**\n *N* = {N}",
    stat_1 = "**Non-case**\n *N* = {n}",
    stat_2 = "**Case**\n *N* = {n}", 
    p.value = "**P value**"
    
    ) %>% 
  
  # Convert to flextable:
  gtsummary::as_flex_table()

# Print the table:
tabsex


## ----define_age_cat-----------------------------------------------------------

linelist <- linelist %>% 
  
  # Create age categories:
  mutate(age_cat = epikit::age_categories(
    
    # Name of age column:
    x = age, 
    
    # Define the age categories:
    breakers = c(0, 10, 16, 18, 20, 50, 70)
    
    )
  )


## ----check_age_cat------------------------------------------------------------

# Check age categories:
janitor::tabyl(linelist, age_cat)


## ----age_sex_pyramid----------------------------------------------------------

# Pipe linelist:
agesex <- linelist %>% 
  
  # Filter for cases only:
  filter(case == TRUE) %>% 
  
  # Create age sex pyramid:
  apyramid::age_pyramid(
  
  # Specify column containing age categories:
  age_group = "age_cat",
  
  # Specify column containing sex:
  split_by = "sex", 
  
  # Don't show midpoint on the graph:
  show_midpoint = FALSE
    
  )

# Print plot:
agesex



## ----symptom_vars-------------------------------------------------------------

# Check list of variable names:
names(linelist)

# Create list of symptom variables:
symptoms <- c("diarrhoea", 
              "bloody", 
              "vomiting", 
              "abdo", 
              "nausea", 
              "fever", 
              "headache", 
              "jointpain")


## ----symptoms_tab-------------------------------------------------------------

# Create summary table:
tabsymptoms <- linelist %>%
  
  # Select person characteristics to summarise:
  select(case, all_of(symptoms)) %>% 
  
  # Create the summary table:
  gtsummary::tbl_summary(
    
    # Stratify by case:
    by = case, 
    
    # Statistics to use for continuous and categorical variables:
    statistic = list(all_continuous() ~ "{mean} ({sd})",        
                     all_categorical() ~ "{n} ({p}%)"), 
    
    # Don't show missing values:
    missing = "no",
    
    # Sort the data:
    sort = list(everything() ~ "frequency"), 
    
    # Create nice labels:
    label  = list(
      diarrhoea   ~ "Diarrhoea",                           
      bloody      ~ "Dysentary",
      vomiting    ~ "Vomiting",
      abdo        ~ "Abdominal pain",
      nausea      ~ "Nausea", 
      fever       ~ "Fever", 
      headache    ~ "Headache", 
      jointpain   ~ "Joint pain")
    
  ) %>% 
  
  # Add tests of statistical significance of differences between groups:
  add_p(test = list(all_continuous() ~ "t.test",       # Continuous
                    all_categorical() ~ "chisq.test"), # Categorical
        
        # Identify grouping variable:
        group = case,
                 
        # Define any test arguments that deviate from the default:
        test.args = list(all_tests("t.test") ~ 
                           list(var.equal = FALSE), 
                         all_tests("chisq.test") ~ 
                           list(simulate.p.value = TRUE))) %>% 
  
  # Add totals:
  add_overall() %>% 
  
  # Individually label which row had which statistical test: 
  separate_p_footnotes() %>% 
  
  # Make variable names bold and italics:
  bold_labels() %>% 
  italicize_labels() %>% 
  
  # Modify header:
  modify_header(
    label = "**Characteristic**",
    stat_0 = "**Overall**\n *N* = {N}",
    stat_1 = "**Non-case**\n *N* = {n}",
    stat_2 = "**Case**\n *N* = {n}", 
    p.value = "**P value**"
    
    ) %>% 
  
  # Convert to flextable for printing:
  gtsummary::as_flex_table()

# Print the table:
tabsymptoms



## ----symptom_barplot----------------------------------------------------------

# Create nice labels for case definition:
caselabs <- ggplot2::as_labeller(c(`FALSE` = "Non-case", 
                                   `TRUE` = "Case"))

# Select variables and cases:
symptom_bar <- linelist %>% 
  
  # Select symptom columns:
  select(case, c(all_of(symptoms))) %>%
  
  # Drop NAs:
  drop_na() %>% 
  
  # Reshape (pivot longer):
  pivot_longer(!case, 
               names_to = "Symptoms", 
               values_drop_na = TRUE) %>% 
  
  # Keep only TRUE values:
  filter(value == TRUE) %>% 
  
  # Group by symptoms and case:
  group_by(Symptoms, case) %>% 
  
  # Count for each symptom by case:
  summarise(count = n()) %>% 
  
  # Create plot:
  ggplot(aes(
    
    # Order symptom bars so most common ones are ontop:
    x = reorder(Symptoms, desc(count), decreasing = TRUE), 
    y = count)) +
  
  # Display bars as proportions
  geom_bar(stat = "identity") +
  
  # Update x axis label:
  xlab("Symptoms") +
  
  # Update y axis label:
  ylab("Proportion of respondents") +
  
  # Flip plot on its side so symptom labels are clear:
  coord_flip() +
  
  # Facet the plot by (labelled) case:
  facet_wrap(facets = "case",
             labeller = caselabs,
             ncol = 2)

# Print plot:
symptom_bar


## ----attack_prop_overall------------------------------------------------------

# Create table of case status:
total_ap <- tabyl(linelist, case) %>% 
  
  # Add row totals:
  adorn_totals(where = "row") %>% 
  
  # Add percentages with 1 digit after the decimal point:
  adorn_pct_formatting(digits = 1) %>% 
  
  # Filter to rows where case is TRUE:
  filter(case == TRUE) %>% 
  
  # Select the column percent:
  select(percent) %>% 
  
  # Extract (pull) the value from this cell:
  pull()

# Print result:
total_ap


## ----attack_prop_students-----------------------------------------------------

# Attack proportion for students:
students_ap <- tabyl(linelist, group, case) %>% 
  
  adorn_totals("row") %>% 
  
  adorn_percentages("row") %>% 
  
  adorn_pct_formatting(digits = 1) %>% 
  
  filter(group == "student") %>% 
  
  select(`TRUE`) %>% 
  
  pull()

# Attack proportion for teachers:
teachers_ap <- tabyl(linelist, group, case) %>% 
  
  adorn_totals("row") %>% 
  
  adorn_percentages("row") %>% 
  
  adorn_pct_formatting(digits = 1) %>% 
  
  filter(group == "teacher") %>% 
  
  select(`TRUE`) %>% 
  
  pull()

# Print result:
students_ap
teachers_ap


## ----attack_prop--------------------------------------------------------------

# Table to calculate attack proportions:
attack_prop <- linelist %>% 
  
  # Select columns:
  select (case, group, class, sex) %>% 
  
  # Create table:
  tbl_summary(
    # Stratified by case
    by = case, 
    # With row percentages
    percent = "row") %>%
  
  # Add totals:
  add_overall() %>%
  
  # Add p values:
  add_p() %>% 
  
  # Make variable names bold and italics:
  bold_labels() %>% 
  italicize_labels() %>% 
  
  # Modify header:
  modify_header(
    label = "**Characteristic**",
    stat_0 = "**Overall**\n *N* = {N}",
    stat_1 = "**Non-case**\n *N* = {n}",
    stat_2 = "**Case**\n *N* = {n}", 
    p.value = "**P value**"
  )


# Print table:
attack_prop


## ----incubation_ggplot--------------------------------------------------------

incplot <- linelist %>% 
  
  # Remove missing values:
  drop_na(incubation) %>% 
  
  # Create an empty ggplot frame:
  ggplot() +
  
  # Add a histogram of incubation:
  geom_histogram(aes(x = incubation), 
                 
                 # Set bin widths to 6 hours:
                 binwidth = 6) +
  
  # Adapt scale to better fit data
  scale_x_continuous(breaks = seq(0, 48, 6)) + 
  
  # Label x and y axes:
  labs(x = "Incubation period in 6-hour bins",
       y = "Number of cases")

# Print plot:
incplot



## ----epicurve_date------------------------------------------------------------

# Fetch data:
epicurve_date <- linelist %>% 
  
  # Filter for cases where dayonset is not missing:
  filter(case == TRUE & !is.na(dayonset)) %>% 
  
  # Add factor onset_day to ggplot aesthetic:
  ggplot(aes(x = dayonset)) + 
  
  # Add geom_bar:
  geom_bar() +
  
  # Adapt scale to data and adjust axis label angle:
  scale_x_datetime(
    date_breaks = "1 day",
    labels = label_date_short()) +
  
  # Update x and y axis labels:
  labs(x = "Date of onset", 
       y = "Number of cases") +
  
  # Remove unnecessary grid lines:
  theme_bw()

# Print epicurve:
epicurve_date



## ----epicurve_hour------------------------------------------------------------

# Fetch data:
epicurve_hour <- linelist %>% 
  
  # Filter for cases where dayonset is not missing:
  filter(case == TRUE & !is.na(onset_datetime)) %>% 
  
  # Add factor onset_day to ggplot aesthetic:
  ggplot(aes(x = onset_datetime)) + 
  
  # Add geom_histogram:
  geom_bar() +
  
  # Adjust x axis scales to a suitable unit:
  scale_x_datetime(
    date_breaks = "6 hours", 
    labels = label_date_short()) +
  
  # Update x and y axis labels:
  labs(x = "Date and time of onset", 
       y = "Number of cases") +

  
  # Remove unnecessary grid lines:
  theme_bw()

# Print epicurve:
epicurve_hour


## ----epicurve_stratified------------------------------------------------------

# Fetch data:
epicurve_strata <- linelist %>% 
  
  # Filter for cases where dayonset is not missing:
  filter(case == TRUE & !is.na(onset_datetime)) %>% 
  
  # Add factor onset_day to ggplot aesthetic:
  ggplot(aes(x = onset_datetime, fill = group)) + 
  
  # Add nicer fill colours:
  scale_fill_manual(values = c("darkred", "lightblue")) +
  
  # Add geom_histogram:
  geom_bar() +
  
  # Adjust x axis scales to a suitable unit:
  scale_x_datetime(
    date_breaks = "6 hours", 
    labels = label_date_short()) +
  
  # Update x and y axis labels:
  labs(x = "Date and time of onset", 
       y = "Number of cases", 
       fill = "Group", 
       title = "Epicurve of the outbreak, stratified by sex",
       subtitle = str_glue("Copenhagen, November 2006, N = {sum(linelist$case)}")) +

  # Stratify by sex:
  facet_wrap(facets = "sex",
             ncol = 2) +
  
  # Add theme:
  theme_bw()

# Print epicurve:
epicurve_strata




## ----odds_ratios_table--------------------------------------------------------

# Pipe in the data:
ortab <- linelist %>% 
  
  # Select the variables to analyse:
  select(all_of(exposure_cols), case) %>% 
  
  # Remove missing values:
  drop_na() %>% 
  
  # Calculate risk ratios and tabulate results:
  gtsummary::tbl_uvregression(
    
    # Choose the model (generalised linear model)
    method = glm, 
    
    # Name the outcome variable:
    y = case,
    
    # Choose the model family:
    method.args = list(family = binomial(link = "logit")),
    
    # Exponentiate the results:
    exponentiate = TRUE, 
    
    # Show results for binary variables on a single row:
    show_single_row = exposure_cols
    
  ) %>% 
  
  gtsummary::as_flex_table()
  
# Print the results table:
ortab


## ----rr_table_raw-------------------------------------------------------------

# Pipe in the data:
rrtab <- linelist %>% 
  
  # Select the variables to analyse:
  select(all_of(exposure_cols), case) %>% 
  
  # Remove missing values:
  drop_na() %>% 
  
  # Calculate risk ratios:
  EpiStats::cstable(
    
    # Define outcome variable:
    cases = "case", 
    
    # Define exposure columns:
    exposure = c(exposure_cols),
    
    # Calculate Fisher's exact test for P values:
    exact = TRUE, 
    
    # Sort output by relative risk:
    sort = "rr"
    
    )


## ----rr_table_publication-----------------------------------------------------

# Define style for flextable border line
border_style = officer::fp_border(color = "black", width = 1)


  # Extract the data.frame from the list of outputs:
  rrtab_pub <- rrtab$df %>% 
    
    # Clean column names:
    janitor::clean_names() %>% 
    
    # Convert row names with exposures to first column:
    rownames_to_column(var = "exposure") %>% 
    
    # Merge lower and upper 95% CI in a new column:
    tidyr::unite(col = "ci95", 
                 starts_with("ci_"), 
                 sep = " - ", 
                 remove = TRUE)  %>% 
    
    # Convert to a flextable:
    flextable::flextable() %>% 
    
    # Set column labels:
    flextable::set_header_labels(values = list(
      exposure = "Exposure",
      tot_exp = "Total",
      exp_cases = "Cases",
      ar_percent = "Attack %", 
      tot_unex = "Total", 
      unex_cases = "Cases", 
      ar_percent_2 = "Attack %", 
      rr = "RR",
      ci95 = "95% CI", 
      p_fisher = "P (Fisher)")) %>% 
    
    # Add title header row:
    flextable::add_header_row(
      values = c("Exposure", 
                 "Exposed", 
                 "Unexposed", 
                 "Relative Risk", 
                 "P (Fisher)"), 
      colwidths = c(1, 3, 3, 2, 1)) %>% 
    
    # Merge cells in header rows:
    merge_at(i = 1:2, j = 1, part = "header") %>% 
    merge_at(i = 1:2, j = 10, part = "header") %>% 
    
    # Right align numeric results columns:
    flextable::align(j = 2:10, 
                     align = "right", 
                     part = "body") %>%
    
    # Center align top header row:
    flextable::align(i = 1, 
                     j = 2:9, 
                     align = "center", 
                     part = "header") %>% 
    
    # Right align second header row:
    flextable::align(i = 2, 
                     j = 2:9, 
                     align = "right", 
                     part = "header") %>%
    
    # Make merged cells on top header row bold:
    flextable::bold(i = 1:2, 
                    j = c(1, 10), 
                    bold = TRUE, 
                    part = "header") %>% 
    
    # Make other cells on top header row bold:
    flextable::bold(i = 1, 
                    j = 2:9, 
                    bold = TRUE, 
                    part = "header") %>% 
    
    # Make bottom header row italic:
    flextable::italic(i = 2, 
                      j = 2:9, 
                      italic = TRUE, 
                      part = "header") %>% 

    # Add highlighting of significant values:
    flextable::bg(i = ~ p_fisher < 0.05, 
                  j = 1:10,
                  bg = "yellow") %>%
    
    # Add vertical borders between sections:
    flextable::vline(part = "all", j = 1, border = border_style) %>% 
    flextable::vline(part = "all", j = 4, border = border_style) %>%
    flextable::vline(part = "all", j = 7, border = border_style) %>%
    flextable::vline(part = "all", j = 9, border = border_style) %>%
    
    # Autofit column width to text width:
    flextable::set_table_properties(
      width = 1, 
      layout = "autofit")


# Print the results table:
rrtab_pub


## ----htest_sex_case-----------------------------------------------------------

# Start with linelist:
sexcasetab <- linelist %>% 
  
  # Create a 2x2 table with tabyl:
  janitor::tabyl(sex, case) %>% 
  
  # Perform chi square test:
  janitor::chisq.test(tabyl_results = TRUE)
  
# Extract observed results:
sexcasetab$observed

# Extract expected results:
sexcasetab$expected

# Print statistical results:
sexcasetab


## ----class_normtest-----------------------------------------------------------

# Perform the Shapiro-Wilk test on class:
shapiro.test(as.numeric(linelist$class))


## ----class_hist---------------------------------------------------------------

# Create histogram of class:
hist(as.numeric(linelist$class))


## ----class_wilcox_test--------------------------------------------------------

# Perform the Wilcoxon rank sum test:
wilcox.test(as.numeric(class) ~ case, data = linelist)

## ----age_norm_test------------------------------------------------------------

# Check if age overall follows a normal distribution:
shapiro.test(linelist$age)


## ----age_norm_test_students---------------------------------------------------

# Check if age of students follows a normal distribution:
linelist %>% 
  
  filter(group == "student") %>% 
  
  select(age) %>% 
  
  pull() %>% 
  
  shapiro.test()


## ----agesex_wilcox_test-------------------------------------------------------

# Perform Wilcoxon rank sum test on age and sex:
wilcox.test(age ~ sex, data = linelist)



## ----dose_response_veal-------------------------------------------------------

# Perform the Wilcoxon rank sum test on number of veal portions:
wilcox.test(vealD ~ case, data = linelist)


## ----dose_response_pasta------------------------------------------------------

# Perform the Wilcoxon rank sum test on number of veal portions:
wilcox.test(pastaD ~ case, data = linelist)


## ----doseresponse_vars_rr-----------------------------------------------------

### Pipe in the data:
rrdrtab <- linelist %>% 
  
  # Select the variables to analyse:
  select(ends_with("D", ignore.case = FALSE), case) 


### Create list of dose response variables:
dr_cols <- names(rrdrtab)[!names(rrdrtab) %in% c("case")]


### Pass dose response columns to formula:
dr_cols %>% 
  
  # Set dose resonse column names:
  set_names() %>%
  
  # Map them to the wilcox.test function:
  purrr::map(.f = ~ wilcox.test(
    
    # Name the data set:
    data = linelist, 
    
    # Paste together the test formula, using '.x' as a wild card:
    as.formula(str_c(.x,"~","case"))))


## ----back2binary--------------------------------------------------------------

# Convert exposures of interest to numeric:
stratall <- linelist %>% 
  
  # Mutate across to convert them to numeric:
  mutate(across(.cols = c(case, pasta, veal, champagne), 
                .fns = ~ as.numeric(.)))


## ----veal_stratifier----------------------------------------------------------

# Pass data to the csinter function:
vealstrata <- csinter(x = stratall, 
                      cases = "case", 
                      exposure = "pasta", 
                      by = "veal")

# Extract second data.frame with summary results:
vealstrata_pub <- vealstrata$df2 %>% 
  
  # Convert to a flextable:
  flextable::qflextable()

# Print table:
vealstrata_pub


## ----pasta_stratifier---------------------------------------------------------

# Pass data to the csinter function:
pastastrata <- csinter(x = stratall, 
                       cases = "case", 
                       exposure = "veal", 
                       by = "pasta")

# Extract second data.frame with summary results:
pastastrata_pub <- pastastrata$df2 %>% 
  
  # Convert to a flextable:
  flextable::qflextable()

# Print table:
pastastrata_pub


## ----champ_stratifier---------------------------------------------------------

# Pass data to the csinter function:
champstrata <- csinter(x = stratall, 
                       cases = "case", 
                       exposure = "champagne", 
                       by = "pasta")

# Extract second data.frame with summary results:
champstrata_pub <- champstrata$df2 %>% 
  
  # Convert to a flextable:
  flextable::qflextable()

# Print table:
champstrata_pub


## ----pasta_veal_assoc---------------------------------------------------------

# Start with the linelist:
linelist %>% 
  drop_na(pasta, veal) %>% 
  tabyl(pasta, veal) %>% 
  fisher.test()


