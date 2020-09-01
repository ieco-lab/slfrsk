library(FAOSTAT)

?`FAOSTAT-package`

download_faostat_bulk()
read_faostat_bulk()

# -----------------------------------------------------------------------------
 url_bulk_site <- "http://fenixservices.fao.org/faostat/static/bulkdownloads"
 url_crops <- file.path(url_bulk_site, "Production_Crops_E_All_Data_(Normalized).zip")
 url_forestry <- file.path(here(),url_bulk_site, "Forestry_E_All_Data_(Normalized).zip")


 # Create a folder to store the data
 data_folder <- "data_raw"
 dir.create(data_folder)

 # Download the files
 download_faostat_bulk(url_bulk = url_forestry, data_folder = data_folder)
 download_faostat_bulk(url_bulk = url_crops, data_folder = data_folder)

 # Read the files and assign them to data frames
 production_crops <- read_faostat_bulk("data_raw/Production_Crops_E_All_Data_(Normalized).zip")
 forestry <- read_faostat_bulk("data_raw/Forestry_E_All_Data_(Normalized).zip")

 # Save the data frame in the serialized RDS format for fast reuse later.
 saveRDS(production_crops, "data_raw/production_crops_e_all_data.rds")
 saveRDS(forestry,"data_raw/forestry_e_all_data.rds")

# <><

FAOsearch()
test = getFAO(query = .LastSearch)

FAOquery.df = data.frame(
  varName = c("arableLand", "cerealExp", "cerealProd"),
  domainCode = c("RL", "TP", "QC"),
  itemCode = c(6621, 1944, 1717),
  elementCode = c(5110, 5922, 5510),
  stringsAsFactors = FALSE
)

## Download the data from FAOSTAT
FAO.lst = with(
  FAOquery.df,
  getFAOtoSYB(
    name = varName,
    domainCode = domainCode,
    itemCode = itemCode,
    elementCode = elementCode,
    useCHMT = TRUE,
    outputFormat = "wide"
  )
)
FAO.lst$entity[, "arableLand"] = as.numeric(FAO.lst$entity[, "arableLand"])
