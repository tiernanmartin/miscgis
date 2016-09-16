insertZIPDownloadTemplate <- function(){
        template <- paste0("
url <- '' # direct URL to the file download

temp <- tempfile() # create a temporary file to hold the compressed download

download(url, dest = temp, mode='wb') # download the file

unzip (temp, exdir = './2_inputs/') # extract the file to the project folder

")
        rstudioapi::insertText(template)

}

