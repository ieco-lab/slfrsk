library(tidyverse)
library(devtools)
library(pkgdown)
library(roxygen2)
# library(fs)
# library(usethis)

# TODO

#options(ENTREZ_KEY = ncbi_key)

#pkgdown::build_articles()
pkgdown::preview_site()
pkgdown::build_site()
pkgdown::build_site(lazy = TRUE)

load_all()
document()
check()
build()
install()

use_package("dplyr", type = "Imports")
use_package("stringr", type = "Imports")
use_package("ggplot2", type = "Imports")
use_package("here", type = "Imports") #added by NAH
use_package("tools", type = "Imports") #added by NAH
use_package("usethis", type = "Imports") #added by NAH
use_package("FAOSTAT", type = "Imports") #added by MRH

#----
# Data
countries_market <- read_csv(file.path(here(), "data-raw","wine_market_FAOSTAT_data_8-31-2020.csv"))
usethis::use_data(countries_market)

#----
# Vignettes

usethis::use_vignette("vignette-010-tidy-data")
usethis::use_vignette("vignette-020-quadrant-plots")
# missing vignette initiation code 021
usethis::use_vignette("vignette-030-risk-maps")
usethis::use_vignette("vignette-040-ee-data")
usethis::use_vignette("vignette-041-market-plot")

#----
## ONLY RUN CAREFULLY

# Functions

use_r("function name")                      # Make functions
use_r("function name")                      # Test functions
use_testthat()
use_test("function name")

# Build

use_package_doc()
use_mit_license("iEcoLab")                  # License
use_readme_rmd()                            # edit GitHub README here?
rmarkdown::render("README.Rmd")             # or use "Knit HTML" in RStudio

if (interactive()) {                        # maybe good or bad...
  suppressMessages(require(devtools))
}

use_build_ignore("[.]ini$", escape = FALSE)
use_build_ignore("sandbox", escape = FALSE)
use_build_ignore("README.html", escape = FALSE)
use_git_ignore("desktop.ini")
