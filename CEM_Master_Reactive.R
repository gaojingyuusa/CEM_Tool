

# Normal indicators that can be filtered by full range of years and comparators
normal_result <- reactive({
  
## Individual comparators
# Retrieve individual comparators + target country list
basis_inv <- final_list()[1:7,]

# Normal: Loop to retrieve and append individual comparator's data with new identifier
normal_inv <- data.frame()
for (i in seq_along(basis_inv$isocode)){
  repl <- subset(normal_dt, ISO == basis_inv$isocode[i] & Year >=input$TT_ST & Year <= input$TT_ED) %>% 
    mutate(identifier=paste0(basis_inv$isocode[i],"_",basis_inv$group[i])) %>% 
    select(-Source, -ISO) 
  normal_inv <- rbind(normal_inv,repl)
}

# Fixed year: Loop to retrieve and append individual comparator's data with new identifier
fixed_inv <- data.frame()
for (i in seq_along(basis_inv$isocode)){
  repl <- subset(fixed_dt, ISO == basis_inv$isocode[i]) %>% 
    mutate(identifier=paste0(basis_inv$isocode[i],"_",basis_inv$group[i])) %>% 
    select(-Source, -ISO) 
  fixed_inv <- rbind(fixed_inv,repl)
}

# End year:  Loop to retrieve and append individual comparator's data with new identifier
end_inv <- data.frame()
for (i in seq_along(basis_inv$isocode)){
  repl <- subset(end_dt, ISO == basis_inv$isocode[i]) %>% 
    mutate(identifier=paste0(basis_inv$isocode[i],"_",basis_inv$group[i])) %>% 
    select(-Source, -ISO) 
  end_inv <- rbind(end_inv,repl)
}

# Latest year:  Loop to retrieve and append individual comparator's data with new identifier
last_inv <- data.frame()
for (i in seq_along(basis_inv$isocode)){
  repl <- subset(last_dt, ISO == basis_inv$isocode[i]) %>% 
    mutate(identifier=paste0(basis_inv$isocode[i],"_",basis_inv$group[i])) %>% 
    select(-Source, -ISO) 
    last_inv <- rbind(last_inv,repl)
}




## Aggregate 3 typologies

# Normal: Define a function to do the trick
typ_cal <-function(test, start, end){
  typo_iso <- final_list()$isocode[final_list()$group==test]
  sub_tp <- subset(normal_dt, ISO %in% typo_iso & Year >=start & Year <= end) 
  sub_tp <- aggregate(x=sub_tp$value, by=list(sub_tp$Year, sub_tp$Indicator), FUN=mean, na.rm=T)
  names(sub_tp) <- c("Year", "Indicator", "value")
  sub_tp$identifier <- test
  sub_tp}

# Normal: Loop to retrieve, transform and append aggregated typology group data
normal_typ <- data.frame()
basis_typ <- unique(final_list()$group[8:nrow(final_list())])
for (j in basis_typ){
  repl <- typ_cal(j, input$TT_ST, input$TT_ED)
  normal_typ <- rbind(normal_typ, repl)
}


# End: Define a function to do the trick
typ_cal_end <-function(test, end){
  typo_iso <- final_list()$isocode[final_list()$group==test]
  sub_tp <- subset(end_dt, ISO %in% typo_iso & Year == end) 
  # if sub_tp has zero, which means there is no data for this year, we should skip the following aggregation process
  if(nrow(sub_tp)==0){
    subtp <- data.frame()
  } else {
  
  sub_tp <- aggregate(x=sub_tp$value, by=list(sub_tp$Year, sub_tp$Indicator), FUN=mean, na.rm=T)
  names(sub_tp) <- c("Year", "Indicator", "value")
  sub_tp$identifier <- test
  }
  sub_tp
  }


# End year: Loop to retrieve, transform and append aggregated typology group data
end_typ <- data.frame()
basis_typ <- unique(final_list()$group[8:nrow(final_list())])
for (j in basis_typ){
  repl <- typ_cal_end(j, input$TT_ED)
  end_typ <- rbind(end_typ, repl)
}

# Fixed year: Loop to retrieve, transform and append aggregated typology group data
# Define a function to do the trick
typ_cal_fixed <-function(test){
  typo_iso <- final_list()$isocode[final_list()$group==test]
  sub_tp <- subset(fixed_dt, ISO %in% typo_iso) 
  sub_tp <- aggregate(x=sub_tp$value, by=list(sub_tp$Year, sub_tp$Indicator), FUN=mean, na.rm=T)
  names(sub_tp) <- c("Year", "Indicator", "value")
  sub_tp$identifier <- test
  sub_tp}

# Fixed year: Loop to retrieve, transform and append aggregated typology group data
fixed_typ <- data.frame()
basis_typ <- unique(final_list()$group[8:nrow(final_list())])
for (j in basis_typ){
  repl <- typ_cal_fixed(j)
  fixed_typ <- rbind(fixed_typ, repl)
}


# Latest year: Loop to retrieve, transform and append aggregated typology group data
typ_cal_last <- function(test){
  typo_iso <- final_list()$isocode[final_list()$group==test]
  sub_tp <- subset(last_dt, ISO %in% typo_iso) 
  sub_tp <- aggregate(x=sub_tp$value, by=list(sub_tp$Indicator), FUN=mean, na.rm=T) 
  sub_tp$identifier <- test
  names(sub_tp) <- c("Indicator", "value", "identifier")
  sub_tp$Year <- NA
  sub_tp}

# Latest year: Loop to retrieve, transform and append aggregated typology group data
last_typ <- data.frame()
basis_typ <- unique(final_list()$group[8:nrow(final_list())])
for (j in basis_typ){
  repl <- typ_cal_last(j)
  last_typ <- rbind(last_typ, repl)
}


# HP Filter using Target country data
chn_gtg <- subset(gtg_hp, ISO == basis_inv$isocode[1] & Year>=input$TT_ST ) %>% na.omit()

if (nrow(chn_gtg)==0){
 chn_gtg <- data.frame()
} else {
chn_ts <- ts(chn_gtg$value, start=c(min(chn_gtg$Year)), end=c(max(chn_gtg$Year)),frequency = 1)
opar <- par(no.readonly=TRUE)
chn.hp <- hpfilter(chn_ts, type="lambda", freq=6.25)

chn_gtg$Trend <- as.vector(chn.hp$trend)
names(chn_gtg)[ncol(chn_gtg)] <- "GTG_WDI_Trend"
names(chn_gtg)[ncol(chn_gtg)-1] <- "GTG_WDI"
chn_gtg <- subset(chn_gtg,select=c("ISO","Year","GTG_WDI_Trend","GTG_WDI")) %>% gather(Indicator, value, GTG_WDI:GTG_WDI_Trend)
chn_gtg$ISO <- paste0(chn_gtg$ISO,"_Target")
names(chn_gtg)[1] <- "identifier"
}


# Target country only indicators: FDI series (FDI_1-8, 12)
target_dt <- subset(master_file, ISO == basis_inv$isocode[1] & Year>=input$TT_ST & Year<=input$TT_ED & Indicator %in% c("FDI_1","FDI_2","FDI_3","FDI_4","FDI_5","FDI_6","FDI_7","FDI_8","FDI_12")) %>%
  mutate(identifier=paste0(ISO,"_Target")) %>% select(-ISO, -Source)


## Append individual and typology
# From long to wide with column names stored
full <- rbind(normal_inv, normal_typ, fixed_inv, fixed_typ, end_inv, end_typ, last_inv, last_typ, chn_gtg, target_dt) %>% spread(identifier, value)
# Reorder the name in the following order: Target, Struc1, Struc2, Struc3, Aspr1, Aspr2, Aspr3, Typo1, Typo2, Typo3
ordername <- names(full)
full <- full[c("Indicator","Year",
               ordername[grep("Target",ordername)],
               ordername[grep("Struc_1",ordername)],
               ordername[grep("Struc_2",ordername)],
               ordername[grep("Struc_3",ordername)],
               ordername[grep("Apsr_1",ordername)],
               ordername[grep("Apsr_2",ordername)],
               ordername[grep("Apsr_3",ordername)],
               ordername[grep(basis_typ[1],ordername)],
               ordername[grep(basis_typ[2],ordername)],
               ordername[grep(basis_typ[3],ordername)])]


## Output the table of normal data
#check <- rbind(normal_inv, normal_typ, fixed_inv, fixed_typ, end_inv, end_typ, last_inv, last_typ)
#check[check$Indicator=="CMP_4",]
#rbind(normal_inv, normal_typ, fixed_inv, fixed_typ, end_inv, end_typ, last_inv, last_typ, chn_gtg)

# Add additional columns that highlight year identifiers for certain variables
# Total start
full$TT_START[1] <- as.numeric(input$TT_ST)
# Total end
full$TT_END[1] <- as.numeric(input$TT_ED)
# Historical end
full$HS_END[1] <- as.numeric(input$HS_ED)
# Recent start
full$RS_START[1] <- as.numeric(input$RS_ST)

# ES: Enterprise Survey latest year for target country
full$ES_LAST <- max(normal_dt[normal_dt$Source=="ES" & normal_dt$ISO==iso_code(input$TARGET),"Year"])

# WEF: World Economic Forum latest year for target country
full$WEF_LAST <- max(last_dt[last_dt$Source=="WEF" & last_dt$ISO==iso_code(input$TARGET),"Year"])

# DB: Doing Business Indicators latest year(end year for target country)
full$DB_LAST <- max(end_dt[end_dt$Source=="DB" & end_dt$ISO==iso_code(input$TARGET),"Year"])


full
})













