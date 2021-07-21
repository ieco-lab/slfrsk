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
pkgdown::build_site(lazy = FALSE)
pkgdown::build_site(lazy = TRUE)

load_all()
document()
check()
build()
install()

use_package("dplyr", type = "Imports")
use_package("ENMTools", type = "Imports") #added by NAH, technically raster is a depend and rgeos ggplot2 gridExtra are all imports
#use_package("FAOSTAT", type = "Imports") #added by MRH
use_package("ggplot2", type = "Imports")
use_package("ggrepel", type = "Imports") #added by NAH
use_package("stringr", type = "Imports")
use_package("here", type = "Imports") #added by NAH
use_package("tools", type = "Imports") #added by NAH
use_package("usethis", type = "Imports") #added by NAH
use_package("raster", type = "Imports") #added by NAH
use_package("rgdal", type = "Imports") #added by NAH
use_package("rgeos", type = "Imports") #added by NAH
use_package("scales", type = "Imports") #added by NAH
use_package("patchwork", type = "Imports") #added by NAH
use_package("grid", type = "Imports") #added by NAH
use_package("gridExtra", type = "Imports") #added by NAH
use_package("DHARMa", type = "Imports") #added by NAH
use_package("cleangeo", type = "Imports") #added by NAH
use_package("plotly", type = "Imports") #added by NAH
use_package("spocc", type = "Imports") #added by NAH
use_package("scrubr", type = "Imports") #added by NAH
use_package("humboldt", type = "Imports") #added by NAH
use_package("ggfortify", type = "Imports") #added by NAH
use_package("RColorBrewer", type = "Imports") #added by NAH
use_package("tigris", type = "Imports") #added by NAH
use_package("sf", type = "Imports") #added by NAH
use_package("magrittr", type = "Imports") #added by NAH
use_package("lubridate", type = "Imports") #added by NAH
use_package("stargazer", type = "Imports") #added by NAH
use_package("RStoolbox", type = "Imports") #added by NAH

use_package("lycormap", type = "Suggests") #added by NAH
use_package("lycordata", type = "Suggests") #added by NAH
use_package("tcltk", type = "Suggests") #added by NAH
use_package("doParallel", type = "Suggests") #added by NAH



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
