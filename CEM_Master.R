
# Deal with CEM Master file that contains everything needed for the tool
library(tidyr)
library(stringr)
library(dplyr)
library(mFilter)
library(xts)

# Importing data from csv
master_file <- read.csv("Micro_Master.csv")
master_file$var <- NULL
names(master_file)[1] <- "Indicator"
master_file$value <- as.numeric(as.character(master_file$value))
master_file$Year <- as.integer(master_file$Year)

macro <- read.csv("Macro_Master.csv")
names(macro)[1] <- "Indicator"
macro$value <- as.numeric(as.character(macro$value))
macro$Year <- as.integer(macro$Year)

master_file <- rbind(master_file, macro)


# Abnormal data filter
src <- c("WMS","HCI","GSMA","BTI","EIU","DB","CPI","GFIN","BL")
indc <- c("HK_5","HK_6","ICT_5",
          "ICT_6","ICT_7","ICT_8","ICT_9","ICT_13","ICT_10","ICT_11","ICT_12","INV_4",
          "INV_5","INV_6","INV_8",
          "CMP_4","CMP_5","CMP_6",
          "FIN_58", "FIN_59","FIN_60","FIN_61","FIN_62", "FDI_12","GTG_WDI",
          "FDI_1","FDI_2","FDI_3","FDI_4","FDI_5","FDI_6","FDI_7","FDI_8","FDI_12")

# Normal data subset
normal_dt <- subset(master_file, !(Source %in% src) & !(Indicator %in% indc))


# Fixed year data subset: where indicators for a fixed year are needed

fixed_dt <- subset(master_file, 
                   # WMS = 2014
                   (Source == "WMS" & Year == 2014)|
                   (Source == "HCI" & Year == 2019)|
                   (Source == "BL" & Year == 2010)|
                   (Source == "EIU" & Year == 2018)|
                   (Source == "GFIN" & Year == 2017)|
                   (Indicator %in% c("HK_5","HK_6") & Year == 2017)
                   )


# End year data subset: where indicators for the end year selected are needed
end_dt <- subset(master_file, Source %in% c("DB","CPI") | Indicator %in% c("FIN_58","FIN_59","FIN_60","FIN_61","FIN_62"))

# Latest year available
last_dt <- subset(master_file,
                  Source %in% c("GSMA","BTI") |
                  Indicator %in% c("INV_4", "INV_5", "INV_6", "ICT_5", "ICT_6", "ICT_7", "ICT_8", "ICT_9", "ICT_13", "ICT_5", "CMP_4", "CMP_5", "CMP_6")
                  ) %>%
                  group_by(Indicator, ISO) %>%
                  mutate(max.year = max(as.numeric(Year))) %>% filter(Year == max.year) %>% select(-max.year) %>% as.data.frame()

# Lastest year available
# Only keep the latest year function
sum(is.na(last_dt$value)) # Check if there are missing data in the value column. If not, then we can figure out the most recent year directly.


# Target country only
# HP-filter using GTG_WDI
## Extract GTG_WDI subset data
gtg_hp <- subset(master_file, Indicator == "GTG_WDI") %>% as.data.frame()

# Only Target country: FDI series from 1-8, and 12
target_dt <- subset(master_file, ISO == "CHN" & Indicator %in% c("FDI_1","FDI_2","FDI_3","FDI_4","FDI_5","FDI_6","FDI_7","FDI_8","FDI_12")) %>%
  mutate(identifier=paste0(ISO,"_Target")) %>% select(-ISO, -Source)









#chn_gtg <- subset(gtg_hp, ISO == "AFG" & Year <=2002) %>% na.omit()
#chn_ts <- ts(chn_gtg$value, start=c(min(chn_gtg$Year)), end=c(max(chn_gtg$Year)),frequency = 1)


#opar <- par(no.readonly=TRUE)
#chn.hp <- hpfilter(chn_ts, type="lambda", freq=6.25)

#chn_gtg$Trend <- as.vector(chn.hp$trend)
#names(chn_gtg)[ncol(chn_gtg)] <- "GTG_WDI_Trend"
#names(chn_gtg)[ncol(chn_gtg)-1] <- "GTG_WDI"
#chn_gtg <- subset(chn_gtg,select=c("ISO","Year","GTG_WDI_Trend","GTG_WDI")) %>% gather(Indicator, value, GTG_WDI:GTG_WDI_Trend)
#chn_gtg$ISO <- paste0(chn_gtg$ISO,"_Target")
#names(chn_gtg)[1] <- "identifier"

#plot(chn.hp)



