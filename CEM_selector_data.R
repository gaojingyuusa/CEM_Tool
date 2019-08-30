# build a file for all the data cleaning documents
library(shiny)
library(leaflet)
library(rgdal)
library(raster)
library(dplyr)
library(formattable)
library(shinydashboard)
library(WDI)
library(rsconnect)

# function to make factor text
text.fun <- function(data){
  data[,sapply(data, class) == "factor"] <- sapply(data[,sapply(data, class) == "factor"],as.character)
  data
}


# WDI file: data_file contains all the indicator data needed for the structural and aspirational comparators
data_file <- read.csv("cem_full_new.csv", header=T) %>% text.fun()
names(data_file)[1] <- "iso3"

# country list
country <- unique(data_file$countryname)

# WBG classification file: class_file contains information such as which country belongs to which group defined by WBG like region and income level
class_file <- read.csv("iso_class.csv", header=T) %>% text.fun()

# Indicator class file: indicator_file contains the description of each indicator used in the tool
indicator_file <- read.csv("indicator.csv", header=T) %>% text.fun()
names(indicator_file)[1] <- "Description"
# Structural Break data from Miodrag
struc_break_file <- read.csv("CEM_stru_break.csv", header=T) %>% text.fun()

# ISO and Name concordance table
iso_name <- unique(data_file[,c("iso3","countryname")])

# Typology list: user end
typology_list <- read.csv("typologylist.csv", header=T)
names(typology_list)[1] <- "option"

# Typology master table
typology_master <- read.csv("typology_master.csv", header=T)
names(typology_master)[1] <- "name"







