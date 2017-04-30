# load required libraries
library(readr)
library(data.table)
library(dplyr)

# Set location of source files as working directory
setwd("c:/myFiles/")

# Get list of available CSV files
filelist <- list.files(pattern="mobileease_2017_02_10.csv")

# Import and merge availabe source login data into temporary dataframe 
Login <- do.call(rbind, lapply(filelist, fread))

# Write data to output file for later analysis
write_csv(Login, "original_MobileLogin.csv")