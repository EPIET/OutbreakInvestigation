Outbreak Investigation in R
================
20 November 2019

<!-- README.md is generated from README.Rmd. Please edit that file -->

### Background:

This repository is for the development and maintenance of R teaching
material for use in the EPIET (European Programme of Intervention
Epidemiology Training) and UK FETP (United Kingdom Field Epidemiology
Training Programme) **Outbreak Investigation (OI) module**.

The module includes two computer-based outbreak investigation case
studies and a GIS mapping exercise; the help guides for these exercises
have been translated from the STATA version into R.

This repository contains a folder for each of the following resources:

  - Copenhagen case study R practical guide
  - Campylobacter case study R practical guide
  - GIS mapping exercise R practical guide and template code

### Acknowledgements:

This material is based on equivalent practical guides for STATA and
ArcGIS, respectively. Each guide is presented as an R markdown document;
please see the title pages for copyright details and a list of authors
and contributors.

### Requirements:

Each exercise requires the use of one or more teaching datasets, which
are included in this repository.

To run the R markdown guides or R templates, participants and
facilitators will need to install the following software:

  - R (download and install the latest version from CRAN
    [here](https://cran.r-project.org/))
  - RStudio (download the latest version as an installer or ready-to-use
    `.zip` file [here](https://rstudio.com/products/rstudio/download/))
  - Rtools (download and install the latest version from CRAN
    [here](https://cran.r-project.org/bin/windows/Rtools/))

A list of required R packages and functions, as well as code to install
them has been included in the first chunk of each R markdown document.

### How to use:

After installing the above software, clone this repository either by
clicking on the green `Clone or download` button on this page, or by
entering the following command into git bash:

``` r
git clone https://github.com/EPIET/OutbreakInvestigation.git
```

Then:

  - Open the .Rproj file in RStudio
  - Navigate to the `Files` tab within RStudio and open the relevant
    case study folder
  - Click on the relevant R markdown document to open it

To provide a printed copy of the R markdown practical guide for
participants:

  - Open the relevant R markdown document in RStudio (as above)
  - Click on the `Knit` button and select `Knit to pdf`
  - This will save a .pdf version of the document in your working
    directory, which can then be printed.

### Maintenance:

This project is currently being maintained by [Amy
Mikhail](https://github.com/AmyMikhail).

Contributions are welcome: please contact the maintainer to request
access.

To report bugs or make feature requests, please post an issue
[here](https://github.com/EPIET/OutbreakInvestigation/issues).
