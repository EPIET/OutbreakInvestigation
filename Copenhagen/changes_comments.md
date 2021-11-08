---
title: Updating R version of Copenhagen Case Study
subtitle: Notes and comments 
author: Johannes Boucsein
---


# General remarks:

*	Use `` notation for R-words throughout rmd-doc!
*	This companion wants too much: It introduces basic concepts of R (assigning, indexing, logical statements, etc), introduces intermediate concepts (data cleaning, modelling), and is a case study at the same time. This is too many goals and too much info in one piece. Suggestion: clearly define requirements (e.g. the above named basic operations) that need to be mastered before using this companion. Then, this companion can actually focus on being a case study and introduce some advanced concept while doing so. 
*	Get rid of stata-references

# RStudio projects

*	Split section title: “R-Studio and R-Studio projects”

# Setting your working directory

*	relative directory for reproducibility! -> use `here::here()`
*	Introduce suggested folder-structure for project
*	Maybe unite this and section above to “setting up your project” as it deals with similar concepts

# Installing packages and functions

*	Rewrite the text: intro packages, installing packages, then loading packages

# Data management and R scripts

## Reading in datasets

*	Update: read_csv()

## Browsing your data set

## Savingh your code in R scripts

*	introduce .rmd and notebooks for reproducibility
*	Encourage to annotate as much as possible

## Describing your data set

*	Update with more up-to-date packages / funs! Tidyr::glimpse(), skimr::glimpse() -> creates tibbles, works with dplyr, can be piped <3; visual: ggpairs() for continuous data
*	Introduce concept, that you can achieve the same thing in R in many different ways

# Data cleaning and recoding in R

## Check the dataset “Copenhagen.csv”

*	Remove introduction of logical statements -> this should be required knowledge at this point
*	Completely update to use dplyr-verbs: filter(), select(), and pipeing

## Recoding missing values, age and sex

*	First, elaborate how we find wrong / missing values. Here, they are given, but in real life you needa find them! 
*	Recode sex into categorical, not binary. Use forcats::
*	Not sure, if we should introduce scoped variants at this point: across(), if_any(), if_all(). They are extremely useful, but tricky and might be difficult to understand when not having worked with dplyr before. Maybe add as a spoiler for self-studying?

## Create Case Definition

`R	cph$case[(is.na(cph$vomiting) | is.na(cph$diarrhoea) | is.na(cph$bloody)) & cph$meal == 1] <- 0 `
makes no sense: The case definition is a ∃ condition, not a ∀ condition! Here, if at least one of the symptoms is unknown, the entire person is considered non-case, even if the one of the other symptoms might be present! Better combine two step approach:
`if_any(c(diarrhoea, vomiting, bloody), ~ . %in% TRUE) ~ TRUE`
`if_all(c(diarrhoea, vomiting, bloody), ~ %in% FALSE) ~ FALSE`
then:
`if_all(c(diarrhoea, vomiting, bloody), ~ is.na(.) ~ FALSE`
-> still treats NAs as FALSE, but at least keeps persons for which one of the symptoms is present! Also: greatly reduces the number of NAs.
* DELVE IN DEEPER – still room for improvement! (if_any(fooditems)==TRUE ~ meal == TRUE)! -> MEAL == TRUE & if_all(symptoms) %in% c(NA, FALSE) -> case == FALSE 

## Saving cleaned data

*	change description! Saving is important, particularly in longer projects where you keep separate scripts / notebooks

# Descriptive analysis in R

# Describing time in R

# Conduct additional investigations

# Univariable analysis (cohort and case control)

# Different statistical tests in R

# Stratified analysis

# Integrate the analysis steps in one master script file

# Conclusions of the case study
