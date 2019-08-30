






# Test data query from data

# Comparators+Target
isocode <- c("CHN", "JPN", "USA", "CHL", "BRA", "RUS", "CHL", "IDN", "MYS", "MEX", "COL", "SGP","KOR")
group <- c("Target", "Stuc_1","Stuc_2","Stuc_3","Aspr_1","Aspr_2","Aspr_3","High income","High income","ASEAN","ASEAN","OECD","OECD")

basis <- data.frame(
  isocode = isocode,
  group = group,
  stringsAsFactors = F
)

# Subset user defined data


## Individual comparators
basis_inv <- basis[1:7,]
normal_inv <- data.frame()
for (i in seq_along(basis_inv$isocode)){
  repl <- subset(normal_dt, ISO == basis_inv$isocode[i] & Year >=2005 & Year <= 2010) %>% 
    mutate(identifier=paste0(basis_inv$isocode[i],"_",basis_inv$group[i])) %>% 
    select(-Source, -ISO) 
  normal_inv <- rbind(normal_inv,repl)
}



## Aggregate 3 typologies
# Define a function to do the trick
typ_cal <-function(test, start, end){
  typo_iso <- basis$isocode[basis$group==test]
  sub_tp <- subset(normal_dt, ISO %in% typo_iso & Year >=start & Year <= end) 
  sub_tp <- aggregate(x=sub_tp$value, by=list(sub_tp$Year, sub_tp$Indicator), FUN=mean, na.rm=T)
  names(sub_tp) <- c("Year", "Indicator", "value")
  sub_tp$identifier <- test
  sub_tp}

# run a loop to do the trick
normal_typ <- data.frame()
basis_typ <- unique(basis$group[8:nrow(basis)])

for (j in basis_typ){
  repl <- typ_cal(j, 2005, 2010)
  normal_typ <- rbind(normal_typ, repl)
}


## Append all data together
full <- rbind(normal_inv, normal_typ) %>% spread(identifier, value)
ordername <- names(full)
full <- full[c("Indicator","Year",
               ordername[grep("Target",ordername)],
               ordername[grep("Stuc_1",ordername)],
               ordername[grep("Stuc_2",ordername)],
               ordername[grep("Stuc_3",ordername)],
               ordername[grep("Aspr_1",ordername)],
               ordername[grep("Aspr_2",ordername)],
               ordername[grep("Aspr_3",ordername)],
               ordername[grep(basis_typ[1],ordername)],
               ordername[grep(basis_typ[2],ordername)],
               ordername[grep(basis_typ[3],ordername)]
)]



