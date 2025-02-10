# Spatial data extraction

csnm.spatial = read.csv("raw_data/2025.01.12_SiteList.csv") |>
  sf::st_as_sf(coords = c("lon", "lat"))

sf::st_crs(csnm.spatial) = 4326

mapview::mapview(csnm.spatial)

basic.clim = function(folderpath, points) {
  #Read rasters
  files <- dir(folderpath, pattern = "*.tif", full.names = TRUE)
  # Stack rasters
  alldata = terra::rast(files)
  #extract raster data for each point
  # Extract data from ET rastStack
  data = terra::extract(alldata, points, xy = TRUE, bind = TRUE) %>%
    data.frame() %>%
    dplyr::rename(lon = x, lat = y)
}

elev = basic.clim(folderpath = "datasets/worldclim", points = csnm.spatial)

write.csv(elev |>relocate(wc2.1_30s_elev, .after = Site),
          "output/2025.01.12_TableS1_SiteList.csv", row.names = FALSE)
