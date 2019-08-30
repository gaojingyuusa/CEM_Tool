# set up the server file for CEM country selection tool

library(shiny)
library(plotly)
library(tidyr)
library(stringr)
library(openxlsx)
library(shinycssloaders)
source("CEM_com_functions.R", local=F)
source("CEM_selector_data.R", local=T)
source("CEM_Master.R", local=T)
# major server function
shinyServer(function(input, output, session){
  source("CEM_reactive.R", local=T)
  
  ## Output in customization pane
  output$country <- renderText({
    country()
  })
  
  output$country.txt <- renderText({
    country()
  })
  
  output$period <- renderText({
    period()
  })
  
  # Region value box
  output$country.region <- renderText({
    country.region()
  })
   # for the information panel in sidebar only
   output$country.region.txt <- renderText({
     country.region.txt()
   })
  # Income value box
  output$country.income <- renderText({
    paste("Income Group:",country.income())
  })
  # Landlocked 
  output$country.land <- renderText({
    country.landlocked()
  })
  # small state 
  output$country.small <- renderText({
    country.small()
  })
  # Landlocked 
  output$country.fcs <- renderText({
    country.fcs()
  })
  
  # Structural break
  output$break_data <- renderTable({
    break_data()
  })
  
  output$break_point <- renderTable({
    break_point()
  })
  
  output$break_point_txt <- renderText({
    break_point_txt()
  })
  
  output$break_plot <-renderPlot({
    break_data()
    k <- ggplot(break_data(),aes(break_data()[,2],break_data()[,3])) + 
      geom_line(size=1,color="#002244") + labs(title = "Structural Break: GDP Growth",x="\nYear",y="GDP growth, %\n") + #theme_minimal() + #x_time() + 
      theme(legend.position="none") + 
      theme(panel.background = element_rect(fill='white'),
            plot.title=element_text(hjust=0.4,face="bold",colour="#676669"),
            axis.title.x=element_text(hjust=0.4,vjust=0.5),
            axis.title.y=element_text(hjust=0.5, vjust=0)
            ) + 
      geom_vline(xintercept = break_point()[[1]],color="#009FDA")
    k
    # ggplotly(k) %>% layout(yaxis=list(titlefont=list(size=1)),xaxis=list(titlefont=list(size=1)),height=200) 
  })

# Structural data table
  output$struc_table <- renderTable({
    struc_data()
  })
  
  # download button for the list of comparators
  output$DOWNLOAD_STRUC_LIST <- downloadHandler(
    filename = function() {
      paste("download/",Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, sheetName="List", gridLines = T)
      writeData(wb, 1, x = struc_result())
      #    addWorksheet(wb, struc_data(),sheetName="Data 2", gridLines = F)
      saveWorkbook(wb, file)
      
      #    write.xlsx(struc_data(), file, row.names = FALSE, sheetName="People")
    }
  )
  
  # download button for full data in the "Data" tab
  output$DOWNLOAD_STRUC <- downloadHandler(
    filename = function() {
      paste("download/",Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, sheetName="Value", gridLines = T)
      addWorksheet(wb, sheetName="Global Rank", gridLine = T)
      addWorksheet(wb, sheetName="Rank Differenced")
      addWorksheet(wb, sheetName="Data Source")
      writeData(wb, 1, x = struc_data())
      writeData(wb, 2, x = struc_ranking_raw() %>% select(-add))
      writeData(wb, 3, x = struc_ranking() %>% select(-add, -na_percent,-result))
      writeData(wb, 4, x = struc_match() %>% select(-weight))
  #    addWorksheet(wb, struc_data(),sheetName="Data 2", gridLines = F)
      saveWorkbook(wb, file)

  #    write.xlsx(struc_data(), file, row.names = FALSE, sheetName="People")
    }
  )
  
  output$DOWNLOAD_STRUC_TOP <- downloadHandler(
    filename = function() {
      paste("download/",Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, sheetName="Data", gridLines = T)
      writeData(wb, 1, x = struc_data() %>% subset(iso3 %in% struc_result()$ISO))
      #    addWorksheet(wb, struc_data(),sheetName="Data 2", gridLines = F)
      saveWorkbook(wb, file)
      
      #    write.xlsx(struc_data(), file, row.names = FALSE, sheetName="People")
    }
  )
  
  
  output$DOWNLOAD_ASPR_LIST <- downloadHandler(
    filename = function() {
      paste("download/",Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, sheetName="List", gridLines = T)
      writeData(wb, 1, x = aspr_result())
      #    addWorksheet(wb, struc_data(),sheetName="Data 2", gridLines = F)
      saveWorkbook(wb, file)
      
      #    write.xlsx(struc_data(), file, row.names = FALSE, sheetName="People")
    }
  )
  
 
  
  output$DOWNLOAD_ASPR_TOP <- downloadHandler(
    filename = function() {
      paste("download/",Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, sheetName="Data", gridLines = T)
      writeData(wb, 1, x = aspr_result_data())
      #    addWorksheet(wb, struc_data(),sheetName="Data 2", gridLines = F)
      saveWorkbook(wb, file)
      
      #    write.xlsx(struc_data(), file, row.names = FALSE, sheetName="People")
    }
  )
  
   
  
  
  
  
  
  output$DOWNLOAD_CEM <- downloadHandler(
    filename = function() {
 #     paste("CEM_input_",Sys.Date(), ".xlsx", sep = "")
      paste("CEM_input_",Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      
      withProgress(
      
        message = paste0("Preparing your CEM data table..."),
        value = 0,
        {
 #     wb <- createWorkbook()
  #    addWorksheet(wb, sheetName="Data", gridLines = T)
        incProgress(5/10)
  #    writeData(wb, 1, x = normal_result())
        incProgress(9/10)
  #    saveWorkbook(wb, file)
        write.csv(normal_result(),file, row.names = F)
        }
                  )
    }
  )
  
  
    # Match indicator and weight
  output$str_matchind <- renderTable({
    struc_match()
  })
    # Structural rank 
  output$struc_rank <- renderTable({
    struc_ranking()
  })
    # Structural result: full list
  output$struc_result <- renderTable(
    struc_result()
  )
    # Structural result: data of the full list
  output$struc_result_data <- renderTable(
    struc_result_data()
  )
    # Average for target country
  output$ind1 <- renderText(
    ind1()
  )
  output$ind2 <- renderText(
    ind2()
  )
  output$ind3 <- renderText(
    ind3()
  )
  output$ind4 <- renderText(
    ind4()
  )
  output$ind5 <- renderText(
    ind5()
  )
  output$ind6 <- renderText(
    ind6()
  )

# Aspirational data table
  # Aspirational indicator table
  output$aspr_data <- renderTable(
    aspr_data()
  )
  
  # Aspirational indicator ranking
  output$aspr_rank <- renderTable(
    aspr_rank()
  )
  
  # Aspirational indicaotr of target
   # Mean
  output$aspr_target_mean <- renderText(
    aspr_target_mean()
  )
   # Rank
  output$aspr_target_rank <- renderText(
    aspr_target_rank()
  )
  
   # Max
  output$aspr_target_max <- renderText(
    aspr_target_max()
  )
   # Dynamic default value in lower bound of rank and value
    observe({
    updateNumericInput(session, "RANK_L",
                      value=aspr_target_rank()
          )})
    
    observe({
      updateNumericInput(session, "VALUE_L",
                         value=aspr_target_mean()
      )})
    
    observe({
      updateNumericInput(session, "VALUE_U",
                         value=aspr_target_max(),
                         max=aspr_target_max()
      )})
    
    # Aspirational within rank list of countries
    output$aspr_within_rank <- renderTable(
      aspr_within_rank()
    )
    
    # Aspsrational within value list of countries
    output$aspr_within_value <- renderTable(
      aspr_within_value()
    )
    
    # Result list of countries
    output$aspr_result <- renderTable(
      aspr_result()
    )
    
    # Aspirational data table of the result
    output$aspr_result_data <- renderTable(
      aspr_result_data()
    )
    
    
    # Global maps of comparators
     # Structural comparators
    output$struc_map <- renderPlotly({
   
      LAND_ISO <- struc_result()[,"ISO"]
      name <- struc_result()[,"Structural Comparators"]
      value <- rep(1, length(LAND_ISO))
      
      data <- data.frame(LAND_ISO, value, name, stringsAsFactors = F)
      data[nrow(data) + 1,] = list(iso_code(input$TARGET),0, input$TARGET)
   #   data$name <- name_code(data$LAND_ISO)
      
      # Run your code:
      g <- list(
        showframe = FALSE,
        showland = TRUE,
        landcolor = "#002244",
        showcoastlines = FALSE,
        projection = list(type = 'orthographic'),
        resolution = '100',
        showcountries = TRUE,
        countrycolor = "white",
        showocean = TRUE,
        oceancolor = '#F2F2F2',
        showlakes = TRUE,
        lakecolor = '#DBDBDB'
      )
      
      plot_geo(data) %>%
        add_trace(
          z = ~value, locations = ~LAND_ISO,
          color = ~value, colors = c('#87FFF4','#009FDA'),
          showscale=FALSE, text=~paste(data$name),hoverinfo="text", hoverlabel=list(bgcolor="white")
        ) %>% 
        layout(geo = g, showlegend = FALSE, hovermode = 'closest')
    })
    
     # Aspirational comparators
    output$aspr_map <- renderPlotly({
      
      LAND_ISO <- aspr_result()[,"ISO"]
      name <- aspr_result()[,"Aspirational Comparators"]
      value <- rep(1, length(LAND_ISO))
      
      data <- data.frame(LAND_ISO, value, name, stringsAsFactors = F)
      data[nrow(data) + 1,] = list(iso_code(input$TARGET),0, input$TARGET)
      #   data$name <- name_code(data$LAND_ISO)
      
      # Run your code:
      g <- list(
        showframe = FALSE,
        showland = TRUE,
        landcolor = "#002244",
        showcoastlines = FALSE,
        projection = list(type = 'orthographic'),
        resolution = '100',
        showcountries = TRUE,
        countrycolor = "white",
        showocean = TRUE,
        oceancolor = '#F2F2F2',
        showlakes = TRUE,
        lakecolor = '#DBDBDB'
      )
      
      plot_geo(data) %>%
        add_trace(
          z = ~value, locations = ~LAND_ISO,
          color = ~value, colors = c('#87FFF4','#009FDA'),
          showscale=FALSE, text=~paste(data$name),hoverinfo="text", hoverlabel=list(bgcolor="white")
        ) %>% 
        layout(geo = g, showlegend = FALSE, hovermode = 'closest')
    })
    
    
  
  
    
    
  # Final period of time selection
    
    # End period
    observe({
      updateSelectInput(session, "TT_ED",
                         choices=end_period())
      })
    # His start
   # output$his_start <- renderText({
  #    his_start()
  #  })
    
    observe({
      updateSelectInput(session, "HS_ST",
                        choices=his_start())
    })
    
    
    observe({
      updateSelectInput(session, "HS_ED",
                        choices=his_end())
    })
    
    # Recent end 
 #   output$recent_end <- renderText({
 #     recent_end()
 #   })
    
    observe({
      updateSelectInput(session, "RS_ST",
                        choices=recent_start())
    })
    
    observe({
      updateSelectInput(session, "RS_ED",
                        choices=recent_end())
    })
  
 # Selection panel of comparators update
    observe({
      updateSelectInput(session, "STRUT2",
                        choices=strut2())
    })
    
    observe({
      updateSelectInput(session, "STRUT3",
                        choices=strut3())
    })
    
    observe({
      updateSelectInput(session, "ASPR2",
                        choices=aspr2())
    })
    
    observe({
      updateSelectInput(session, "ASPR3",
                        choices=aspr3())
    })
    
    observe({
      updateSelectInput(session, "TYPO2",
                        choices=typo2())
    })
    
    observe({
      updateSelectInput(session, "TYPO3",
                        choices=typo3())
    })
    
    
    
  # Final list
    output$final_list <- renderTable({
      final_list()
    })
    
  # Final normal table (to be appended to the abnormal data table)
    output$normal_result <- renderTable({
      normal_result()
    })
    
  
})