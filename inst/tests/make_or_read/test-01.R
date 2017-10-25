# Setup ----

library(tidyverse)
library(stringr)
library(sf)
library(googledrive)
library(miscgis)


options(httr_oob_default=TRUE)

# Text file exists ----

txt_fp <- "./inst/tests/drive_read/target_folder/example.txt"

file.exists(txt_fp)

test_txt_exists <- make_or_read(txt_fp,
                     {
                             dr_id <- as_id("0B5Pp4V6eCkhrZWRqeTBsVGI2djg")

                             test_txt <-
                                     drive_read(dr_id = dr_id,
                                                .tempfile = FALSE,
                                                path = txt_fp,
                                                read_fun = read_file)

                     },
                     {
                             read_file(txt_fp)
                     })

# Text file exists doesn't exist----

unlink(txt_fp)

file.exists(txt_fp)

test_txt_exists <- make_or_read(txt_fp,
                     {
                             dr_id <- as_id("0B5Pp4V6eCkhrZWRqeTBsVGI2djg")

                             test_txt <-
                                     drive_read(dr_id = dr_id,
                                                .tempfile = FALSE,
                                                path = txt_fp,
                                                read_fun = read_file)

                     },
                     {
                             read_file(txt_fp)
                     })
