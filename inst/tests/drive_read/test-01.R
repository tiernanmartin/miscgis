# Setup ----

library(tidyverse)
library(stringr)
library(sf)
library(googledrive)
# library(miscgis)
library(forcats)
library(leaflet)
library(mapview)
library(miscgis)
library(htmltools)
library(snakecase)

options(httr_oob_default=TRUE)

# Text file ----

txt_fp <- "./inst/tests/drive_read/target_folder/example.txt"

dr_id <- as_id("0B5Pp4V6eCkhrZWRqeTBsVGI2djg")

test_txt <-
  drive_read(dr_id = dr_id,
             .tempfile = FALSE,
             path = txt_fp,
             read_fun = read_file)

test_text_tmp <-
  drive_read(dr_id = dr_id,
             .tempfile = TRUE,
             read_fun = read_file)

# Text file (zipped) ----

zip_dir <- "./inst/tests/drive_read/target_folder"

target_name <- "example.txt"

dr_id <- as_id("0B5Pp4V6eCkhrUnZLVXNGRDROU2c")

test_zip_txt <-
  drive_read_zip(dr_id = dr_id,
                 .tempdir = FALSE,
                 dir_path = zip_dir,
                 read_fun = read_file,
                 target_name = target_name)

test_zip_txt_tmp <-
  drive_read_zip(dr_id = dr_id,
                 .tempdir = TRUE,
                 read_fun = read_file,
                 target_name = target_name)

# Folder (zipped) ----

zip_dir <- "./inst/tests/drive_read/target_folder"

target_name <- "example/example.txt"

dr_id <- as_id("0B5Pp4V6eCkhrYUJ1QzNPVG9lcnc")

test_zip_folder <-
  drive_read_zip(dr_id = dr_id,
                 .tempdir = FALSE,
                 dir_path = zip_dir,
                 read_fun = read_file,
                 target_name = target_name)

test_zip_txt_tmp <-
  drive_read_zip(dr_id = dr_id,
                 .tempdir = TRUE,
                 read_fun = read_file,
                 target_name = target_name)
