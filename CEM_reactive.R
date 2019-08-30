# this script stores all reactive elements for the shiny app

# 1 reactive component of target country group

country <- reactive({
  input$TARGET
})

country.region <- reactive({
  region(data_file[data_file$countryname==input$TARGET %>% unique,"iso3"],"Region")
})

  country.region.txt <- reactive({
    paste("Region:",region(data_file[data_file$countryname==input$TARGET %>% unique,"iso3"],"Region"))
  })
  
  period <- reactive({
    paste0(input$TARGET," ",input$YEAR[1],"-",input$YEAR[2]," Mean")
  })

# 1.a reactive component of target country income
country.income <- reactive({
  region(data_file[data_file$countryname==input$TARGET %>% unique,"iso3"],"Income.group")
})

# 1.b reactive component of target country landlocked
country.landlocked <- reactive({
  paste("Landlocked:",ifelse(region(data_file[data_file$countryname==input$TARGET,"iso3"] %>% unique,"landlocked")==1,"Landlocked","Not Landlocked"))
})

# 1.c reactive component of target country smallstates
country.small <- reactive({
  paste("Small States:",ifelse(region(data_file[data_file$countryname==input$TARGET,"iso3"] %>% unique,"smallstates")==1,"Small state","Not small state"))
})

# 1.d reactive component of target country smallstates
country.fcs <- reactive({
  paste("Fragile & Conflicted States:",ifelse(region(data_file[data_file$countryname==input$TARGET,"iso3"] %>% unique,"fcs")==1,"FCS","Not FCS"))
})


# 2 Structural: reactive dataset selected by the user
struc_data <- reactive({
  # convert to short name
  ind <- c(input$INDICATOR1,input$INDICATOR2,input$INDICATOR3,input$INDICATOR4,input$INDICATOR5,input$INDICATOR6) %>% sapply(indicator) #%>% unique()
  ind <- ind[!ind=="Select"]
  middle <- subset(data_file, year>=input$YEAR[1] & year<=input$YEAR[2] ,select=c("countryname","iso3","year",ind)) 
  middle <- aggregate(middle, by = list(middle$countryname,middle$iso3), FUN=mean, na.rm=T) 
  result <- middle[,!names(middle) %in% c("iso3","countryname","year")]
  names(result)[1:2] <- c("countryname","iso3")
  result
})


# 2.a match indicator and its weight
struc_match <- reactive({
  data_source <- data.frame(
    selected = c(indicator(input$INDICATOR1),indicator(input$INDICATOR2),indicator(input$INDICATOR3),indicator(input$INDICATOR4),indicator(input$INDICATOR5),indicator(input$INDICATOR6)),
    weight = c(input$W1,input$W2,input$W3,input$W4,input$W5,input$W6),
    stringsAsFactors = F
  ) %>% subset(selected!="Select") #%>% mutate(WDICode=wdi_ind(selected))
 #data_source$WDICode <- wdi_ind(data_source$selected)
 data_source
})


# 2.b create table for the structural ranking
struc_ranking <- reactive({
  struc_rank <- struc_data() %>% mutate(add=1)
  struc_rank[,3:ncol(struc_rank)] <- sapply(struc_rank[,3:ncol(struc_rank)], function(x) rank(-x, na.last="keep")) 
  target <- struc_rank[struc_rank$countryname == input$TARGET,3:ncol(struc_rank)] %>% as.matrix()
  
  # calculate the difference of ranking between target country and all other countries in absolute value
  struc_rank[,3:ncol(struc_rank)] <- sweep(struc_rank[,3:ncol(struc_rank)],2, target,"-") %>% sapply(abs)
  #struc_rank <- struc_rank[,!names(struc_rank)=="add"]
  # inser the weight vector
  weight <- c(struc_match()[,"weight"],0)#%>% as.matrix()
  # multiply with the vector of weights assigned to each indicator
  struc_rank[,3:ncol(struc_rank)] <- sweep(struc_rank[,3:ncol(struc_rank)],2, weight,"*")
  struc_rank$weighted_dif <- rowSums(struc_rank[,!names(struc_rank) %in% c("iso3","countryname")],na.rm=T)/sum(weight)
  struc_rank$na_percent <- apply(struc_rank,1, function(x) sum(is.na(x))/length(weight))
  struc_rank$weighted_dif[struc_rank$weighted_dif==0|struc_rank$na_percent>=0.40] <- NA
  # rows with values more than half
  struc_rank$result <- rank(-struc_rank$weighted_dif, na.last = "keep")
  struc_rank 
})

# 2.c create table that contains the ranking of each country 
struc_ranking_raw <- reactive({
  struc_rank_raw <- struc_data() %>% mutate(add=1)
  struc_rank_raw[,3:ncol(struc_rank_raw)] <- sapply(struc_rank_raw[,3:ncol(struc_rank_raw)], function(x) rank(-x, na.last="keep")) 
  struc_rank_raw
})

# 2.d structural peers top 10
struc_result <- reactive({
    full <- merge(class_file, struc_ranking(),by.x="Code", by.y="iso3")
  if(input$RESTRICTION=="all"){
    full <- full
  } else if (input$RESTRICTION=="small"){
    full <- full[full$smallstates==1,]
  } else if (input$RESTRICTION=="region"){
    full <- full[full$Region==country.region() & !is.na(full$weighted_dif),]
  } else if (input$RESTRICTION=="fcs"){
    full <- full[full$fcs==1,]
  }
  
  struc_list <- full %>% arrange(desc(-weighted_dif)) %>% slice(1:input$STRUC_TOP) %>% select(Code, countryname, weighted_dif)
  names(struc_list) <- c("ISO","Structural Comparators","Weighted_Distance")
  struc_list
#  temp <- subset(struc_data(),iso3 %in% struc_list$Code)
#  merge(temp, struc_list, by.x="iso3", by.y="Code") %>% arrange(desc(-weighted_dif)) %>% select(-weighted_dif)
  #merge(temp,struc_list, by.x=Code, by.y=iso3)
  #struc_data()[struc_data()$iso3 %in% struc_list[[1]],]
  #full %>% arrange(desc(-weighted_dif)) %>% slice(1:10) %>% select(Code)
  #select(-Region, -Income.group, -landlocked, - smallstates, -fcs, - add, -weighted_dif, - na_percent, - result)
})

# 2.c structural comparators raw data table
struc_result_data <- reactive({
  temp <- subset(struc_data(),iso3 %in% struc_result()$ISO)
  result <- merge(temp, struc_result(), by.x="iso3", by.y="ISO") %>% arrange(desc(-Weighted_Distance)) %>% select(-"Structural Comparators")
  names(result)[1:2] <- c("ISO", "Structural Comparators")
  result
})

# 2.d structural indicator average for tartget country
ind1 <- reactive({
  validate({
    need(input$INDICATOR1 != "Select", "Loading...")
  })
  subset(struc_data(),countryname==input$TARGET, select=c(indicator(input$INDICATOR1)))[1,1] %>% round(2)
})

ind2 <- reactive({
  validate({
    need(input$INDICATOR2 != "Select", "Loading...")
  })
  subset(struc_data(),countryname==input$TARGET, select=c(indicator(input$INDICATOR2)))[1,1] %>% round(2)
})

ind3 <- reactive({
  validate({
    need(input$INDICATOR3 != "Select", "Loading...")
  })
  subset(struc_data(),countryname==input$TARGET, select=c(indicator(input$INDICATOR3)))[1,1] %>% round(2)
})

ind4 <- reactive({
  validate({
    need(input$INDICATOR4 != "Select", "Loading...")
  })
  subset(struc_data(),countryname==input$TARGET, select=c(indicator(input$INDICATOR4)))[1,1] %>% round(2)
})

ind5 <- reactive({
  validate({
    need(input$INDICATOR5 != "Select", "Loading...")
  })
  subset(struc_data(),countryname==input$TARGET, select=c(indicator(input$INDICATOR5)))[1,1] %>% round(2)
})

ind6 <- reactive({
  validate({
    need(input$INDICATOR6 != "Select", "Loading...")
  })
  subset(struc_data(),countryname==input$TARGET, select=c(indicator(input$INDICATOR6)))[1,1] %>% round(2)
})


# 3 As[pirational: reactive dataset selected by the user

# 3.a aspirarional indicator data table
aspr_data <- reactive({
  middle <- subset(data_file, year>=input$YEAR[1] & year<=input$YEAR[2] ,select=c("countryname","iso3","year",indicator(input$ASPR))) 
  middle <- aggregate(middle, by = list(middle$countryname,middle$iso3), FUN=mean, na.rm=T) 
  result <- middle[,!names(middle) %in% c("iso3","countryname","year")]
  names(result)[1:2] <- c("countryname","iso3")
  result$rank <- rank(-result[,3], na.last="keep") %>% as.integer()
  result
})

# 3.b ranking of aspirational indicator data table
# aspr_rank <- reactive({
# aspr_rank <- aspr_data() 
# aspr_rank[,3] <- rank(-aspr_rank[,3], na.last="keep")
#  aspr_rank
# })

# 3.c mean and ranking of target country in aspirational indicator
 # mean
aspr_target_mean <- reactive({
  subset(aspr_data(),countryname==input$TARGET, select=c(indicator(input$ASPR)))[1,1] %>% round(2)
})
 # rank
aspr_target_rank <- reactive({
  subset(aspr_data(),countryname==input$TARGET, select=c("rank"))[1,1] 
})
 # max
aspr_target_max <- reactive({
  max(subset(aspr_data(),select=c(indicator(input$ASPR)))[[1]],na.rm=T)
})

# 3.c create country list that are within the determined range of rank or value
  # within rank
aspr_within_rank <- reactive({
  aspr_data()[aspr_data()[[4]]>=input$RANK_U & aspr_data()[[4]]<=input$RANK_L,2] %>% na.omit()
})
  # within value
aspr_within_value <- reactive({
  aspr_data()[aspr_data()[[3]]>=input$VALUE_L & aspr_data()[[3]]<=input$VALUE_U,2] %>% na.omit()
})

# 3.d create result table using aspr_within_rank or aspr_within_value to filter struc_rank
aspr_result <- reactive({
  if(input$RANKVALUE=="rank") {
    middle <- aspr_within_rank()
  } else if (input$RANKVALUE=="value") {
    middle <- aspr_within_value()
  }
  aspr_list <- subset(struc_ranking(),iso3 %in% middle) %>% arrange(desc(-weighted_dif)) %>% slice(1:input$ASPR_TOP) %>% select(iso3, countryname, weighted_dif)
  names(aspr_list) <- c("ISO", "Aspirational Comparators", "Weighted_Distance")
  aspr_list %>% na.omit()
 # temp <- subset(struc_data(),iso3 %in% aspr_list$iso3)
 # merge(temp, aspr_list, by.x="iso3", by.y="iso3") %>% arrange(desc(-weighted_dif)) %>% select(-weighted_dif)
})

# 3.e aspirational comparators raw data table
aspr_result_data <- reactive({
  temp <- subset(struc_data(),iso3 %in% aspr_result()$ISO)
  result <- merge(temp, aspr_result(), by.x="iso3", by.y="ISO") %>% arrange(desc(-Weighted_Distance)) %>% select(-"Aspirational Comparators") %>% na.omit()
  names(result)[1:2] <- c("ISO", "Aspirational Comparators")
  result <- merge(aspr_data()[,2:3],result,by.x="iso3",by.y="ISO",all.y=T)
  # reorder columns properly
  part_1 <- names(result)[c(1,3)]
  part_2 <- names(result)[2] 
#  part_2 <- paste0("Aspirational",part_2_m)
  part_3 <- names(result)[c(4:length(names(result)))]
  result <- result[c(part_1, part_2, part_3)]
  names(result)[3] <- paste0("aspirational_",gsub(".x","",part_2))
  names(result)[4] <- gsub(".y","",names(result)[4])
  result
  
})


# 4 Structural Break chart
# 4.a create table based on target country and selected period
break_data <- reactive({
  subset(struc_break_file, country == iso_code(input$TARGET) & year >= input$YEAR2[1] & year <= input$YEAR2[2], select=c("country","year","gdpgrowth"))
})

# 4.b breakpoint
break_point <- reactive({
  point <- as.character(input$YEAR2[2])
  subset(struc_break_file, country == iso_code(input$TARGET) & year == input$YEAR2[1]) %>% select(contains(point))
})

# 4.e breakpoint text
break_point_txt <- reactive({
  if(is.na(break_point()[[1]])){
    result <- "No break point"
  } else {
    result <- paste0("Break point: ",break_point()[[1]])
  }
  result
})



# 5 Final list of comparators

  # Period
  end_period <- reactive({
    bg <- as.numeric(input$TT_ST)+3
    c(bg:2017)
  })
  
  # his period start = start
  his_start <- reactive({
    as.numeric(input$TT_ST)
  })
  
  his_end <- reactive({
    start <- as.numeric(input$TT_ST)+1
    end <- as.numeric(input$TT_ED)-1
    c(start:end)
  })
  
  # recent period end = end
  recent_end <- reactive({
    input$TT_ED
  })
  
  recent_start <- reactive({
    start <- as.numeric(input$HS_ED)+1
    end <- as.numeric(input$TT_ED) -1
    c(start:end)
  })
  
  
  
final_list <- reactive({
  # Structural and Aspirational
  name <- c(iso_code(input$TARGET), iso_code(input$STRUT1),iso_code(input$STRUT2),iso_code(input$STRUT3),iso_code(input$ASPR1),iso_code(input$ASPR2),iso_code(input$ASPR3)) #%>% iso_code()
  code <- c("Target","Struc_1","Struc_2","Struc_3","Apsr_1","Apsr_2","Apsr_3")
  
  result <- data.frame(
    name,
    code,
    stringsAsFactors = F
  )
  
  # Typology 1
  t1 <- data.frame(
  name=typo(input$TYPO1),
  code=rep(input$TYPO1,length(typo(input$TYPO1))),
  stringsAsFactors = F
                  )
  
  # Typology 2
  t2 <- data.frame(
    name=typo(input$TYPO2),
    code=rep(input$TYPO2,length(typo(input$TYPO2))),
    stringsAsFactors = F
  )
  
  # Typology 3
  t3 <- data.frame(
    name=typo(input$TYPO3),
    code=rep(input$TYPO3,length(typo(input$TYPO3))),
    stringsAsFactors = F
  )
  
  result <- rbind(result, t1, t2, t3)
  names(result) <- c("isocode","group")
  result
  
 # Typology
 # 
 
})

# Updated list of comparators conditional on the previous selection of user
strut2 <- reactive({       # Target country shouldn't appear in comparator list at alol
  remove <- c(input$STRUT1,input$TARGET)
  medium <- unique(data_file$countryname)
  result <- medium[! medium %in% remove] 
  result
})

strut3 <- reactive({
  remove <- c(input$STRUT1,input$STRUT2,input$TARGET)
  medium <- unique(data_file$countryname)
  result <- medium[! medium %in% remove] 
  result
})

aspr2 <- reactive({
  remove <- c(input$ASPR1,input$TARGET)
  medium <- unique(data_file$countryname)
  result <- medium[! medium %in% remove] 
  result
})

aspr3 <- reactive({
  remove <- c(input$ASPR1,input$ASPR2,input$TARGET)
  medium <- unique(data_file$countryname)
  result <- medium[! medium %in% remove] 
  result
})

typo2 <- reactive({
  remove <- input$TYPO1
  medium <- unique(typology_list$option)
  result <- medium[! medium %in% remove] 
  result
})

typo3 <- reactive({
  remove <- c(input$TYPO1,input$TYPO2)
  medium <- unique(typology_list$option)
  result <- medium[! medium %in% remove] 
  result
})

source("CEM_Master_Reactive.R", local=T)
