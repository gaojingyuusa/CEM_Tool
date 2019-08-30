# function to label if countries are in the same region (a dummy variable)

# 1 function to find out the group of target country
region <- function(target,group){
  class_file[class_file$Code==target,group]
}

# 2 function to find out the indicator code
indicator <- function(name){
  indicator_file[indicator_file$Description==name, "Name"]
}
  #2.1 function to find out the WDI indicator code
wdi_ind <- function(code){
  indicator_file[indicator_file$Name==code, "Code"]
}

# 3 function to calculate summary data of select country
summary <- function(data, country, start, end, indicator){
  subset(data, year >= start & year <= end & countryname == country, select=c(indicator))[[1]] %>% mean(na.rm=T)
}

# 3 function to convert country name into ISO code
iso_code <- function(name){
  iso_name[iso_name$countryname==name,"iso3"]
}

# 3 function to convert ISO into country name
name_code <- function(iso){
  iso_name[iso_name$iso3==iso,"countryname"]
}

# 3 typology function

typo <- function(input){
  dummy <- c("Landlocked", "SmallStates", "IslandStates", "FragileStates", "G20", "Commodity")
     group1 <- typology_list[typology_list$option==input,"group"][[1]] %>% as.character()
  if(group1 %in% dummy) {
    typology_master[typology_master[[group1]]=="Yes", "iso3"] %>% as.character()
  } else {
    typology_master[typology_master[[group1]]==input, "iso3"] %>% as.character()
  }
}
