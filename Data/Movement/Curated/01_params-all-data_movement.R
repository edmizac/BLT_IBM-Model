# Script name: 01_params-all-data.R
# Script purpose: make a stable version from the raw seed dispersal data without having to modify it (rename columns, etc) everytime

# Date created: 2022-11-16d
# Author: Eduardo Zanette

## Notes --------------------------- 
#
#

## Options -------------------------
# (plotting, memory limit, decimal digits)
# 
our_crs <- "+proj=utm +zone=22 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

## Packages -------------------------
library("here")
library("lubridate")
library("hms")
library("dplyr")
library("readxl")
library("sf")



