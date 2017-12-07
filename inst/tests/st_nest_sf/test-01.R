library(tidyverse)
library(sf)

nc <- st_read(system.file("gpkg/nc.gpkg", package = "sf"), quiet = TRUE)

st_nest_sf(nc)
