fp <- paste(getwd(),"inst/tests/zip_pithy/mtcars.csv",sep = "/")

zip_fp <- paste(getwd(),"inst/tests/zip_pithy/mtcars.zip",sep = "/")

write.csv(mtcars, fp)

zip_pithy(zipfile = zip_fp,files = fp)

lapply(list(fp, zip_fp),file.remove)
