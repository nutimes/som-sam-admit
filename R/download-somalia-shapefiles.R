#'
#' Download zipfile of Somalia shapefiles from Humanitarian Data Exchange
#'
#' @param url Download URL for Somalia shapefiles from Humanitarian Data
#'   Exchange. This is set to https://data.humdata.org/dataset/ec140a63-5330-4376-a3df-c7ebf73cfc3c/resource/cc9ada4b-aee2-4745-ab59-98e7ef4ce037/download/som_adm_ocha_20250108_ab_shp.zip
#'   by default.
#' @param destfile Download path for Somalia shapefiles zipfile. This is set to
#'   "data-raw/Somalia_shp.zip" by default.
#' @param overwrite Logical. Should `destfile` be overwritten if present?
#'   Default is FALSE.
#'
#' @returns A downloaded zipfile as specified in `destfile`.
#'
#'
#' 

download_som_shp <- function(
  url = "https://data.humdata.org/dataset/ec140a63-5330-4376-a3df-c7ebf73cfc3c/resource/cc9ada4b-aee2-4745-ab59-98e7ef4ce037/download/som_adm_ocha_20250108_ab_shp.zip",
  destfile = "data-raw/somalia-shp.zip",
  overwrite = FALSE) {
  
## Check whether destfile exists and download accordingly ----
if (file.exists(destfile)) {
  if (overwrite) {
    warning(
      paste0("`", destfile, "` exists and `overwrite = TRUE`. )"),
      paste0("Downloading `", destfile, "`.")
    )
    download.file(url = url, destfile = destfile, mode = "wb")
  } else {
    warning(
      paste0("`", destfile, "` exists and `overwrite = FALSE`. )"),
      paste0("`", destfile, "` will not be downloaded.")
    )
  }
} else {
  download.file(url = url, destfile = destfile, mode = "wb")
}

## Return download path ----
destfile
}
