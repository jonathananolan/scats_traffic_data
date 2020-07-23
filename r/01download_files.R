library(tidyverse)
library(data.table)
library(fst)
# Create a function that import SCATS csv data from Vic gov opend data website
# https://discover.data.vic.gov.au/dataset/traffic-signal-volume-data
download_and_unzip <- function(filename_url) {

base_url <- "https://vicroadsopendatastorehouse.vicroads.vic.gov.au/opendata/Traffic_Measurement/SCATS/VSDATA/"
zip_file <- paste0("scats_data/", filename_url)
#check if the zip file is already there before wasting your internet downloading it again. 
if(!file.exists(zip_file){
download.file(paste0(base_url, filename_url), zip_file)
unzip(zip_file,exdir = "scats_data")
}
}

#List the ZIP files we are interested in
zip_files <- c("VSDATA_202001.zip",
               "VSDATA_202002.zip",
               "VSDATA_202003.zip",
               "VSDATA_202004.zip",
               "VSDATA_202005.zip",
               "VSDATA_202006.zip",
               "VSDATA_202007.zip")

#import the list of zip file we are interested in, and unzip them into CSV files. 
map(zip_files,download_and_unzip)

#Now we have a big folder filled with glorious CSV files. Let's import them and put them into one big dataframe. 
file_names <- list.files(pattern = "\\.csv$", full.names = TRUE,path = "scats_data",recursive = TRUE)
scats_data <- map_df(file_names, fread)

#Now we can save that big file as an fst
fst::write_fst(scats_data,path = "processing_files")
