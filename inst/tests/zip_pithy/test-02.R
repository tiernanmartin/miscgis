fp_1 <- paste(getwd(),"zip_pithy/mtcars.csv",sep = "/")

fp_2 <- paste(getwd(),"zip_pithy/iris.csv",sep = "/")

zip_fp_1 <- paste(getwd(),"zip_pithy/mtcars.zip",sep = "/")

zip_fp_2 <- paste(getwd(),"zip_pithy/iris.zip",sep = "/")

write.csv(mtcars, fp_1)

write.csv(iris, fp_2)

zip_pithy(zipfile = zip_fp,files = list(fp_1,fp_2))

lapply(list(fp, zip_fp),file.remove)
