# Load packages ----
library(tidyverse)
library(RCurl)
library(googledrive)
library(magrittr)
library(stringr)
library(miscgis)

# Testing googledrive package ----

ds <- drive_search()

ds_df <-
  ds %>%
  mutate(
    name = set_names(name,id),
    parent = map(files_resource,
                 ~ .x %>%
                   extract2("parents") %>%
                   unlist %>%
                   {if_else(is_null(.), NA_character_,.)}) %>%
      map_chr(~name[.x])
  ) %>%
  select(name,parent,id,files_resource)


drive_ls("~/data/futurewise/") %>% # this is the verb to use to access files
  extract2("id") %>%
  map(drive_get)

# My upload and download functions (WIP) ----

# these functions are now maintained in the miscgis package (6/15/2017)

# Testing my functions: simple feature ----

library(sf)

nc <- st_read(system.file("shape/nc.shp", package="sf"))

drive_nc <- drive_upload_obj(x = nc,
                             filename = "nc.rds",
                             write_fun = write_rds,
                             .zip = TRUE,
                             .share = TRUE,
                             folder = drive_path("~/data/futurewise/4-ready/"))

nc_from_drive <- drive_read(dribble = drive_nc,
                            read_fun = read_rds,
                            .unzip = TRUE)

# Testing my functions: csv ----

drive_chk <- drive_upload_obj(x = chickwts,
                              filename = "chickwts.csv",
                              write_fun = write_csv,
                              .zip = FALSE,
                              .share = TRUE,
                              folder = drive_path("~/data/futurewise/1-raw/"))

chk_from_drive <- drive_read(dribble = drive_chk,
                             read_fun = read_csv,
                             .unzip = FALSE)
