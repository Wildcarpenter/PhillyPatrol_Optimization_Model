
# SET UP

library(tidyverse)
library(sf)
library(knitr)
library(dplyr)
library(spdep)
library(caret)
library(ckanr)
library(FNN)
library(grid)
library(gridExtra)
library(ggcorrplot) 
library(corrr)   
library(kableExtra)
library(jtools)    
library(ggstance) 
library(ggpubr)
library(broom.mixed) 
library(RColorBrewer)

knitr::opts_chunk$set(echo = TRUE)

# CRIME DATA
## get the crime data (2022-now)
## https://opendataphilly.org/datasets/crime-incidents/

crime <- st_read("C:/Users/25077/Desktop/crime_data/crime.shp") %>%
  st_transform(crs = st_crs(2272), check = TRUE)