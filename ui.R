# User interface of CEM country selection website

library(shiny)
library(DT)
library(plotly)
library(tidyr)
library(stringr)
library(openxlsx)
library(shinycssloaders)
library(shinyWidgets)
source("CEM_com_functions.R", local=F)
source("CEM_selector_data.R", local=T)
source("CEM_Master.R", local=T)

fluidPage(
  
  # Style elements
  tags$head(
    tags$style(HTML(
                    "
                    #navbar  {background-color: white;}
                    ",
                    
                    "
                    .form-group, .selectize-control {
                    margin-bottom: 2px;
                    }
                    .box-body {
                    padding-bottom: 2px;
                    }
                    ",
                    
                    ".irs-bar {",
                    "  border-color: transparent;",
                    "  background-color: transparent;",
                    "}",
                    ".irs-bar-edge {",
                    "  border-color: transparent;",
                    "  background-color: transparent;",
                    "}"
            
                    
                    
                   )
              )
           ),
  
  # Maintitle
  titlePanel(
    fluidRow(
      
          column(3,
                 img(src="CEM_logo.png", height="150",align="center")
                 ),
          
          column(9,
                 h1(" "),
                 img(src="wbg_efi.png", height="100")
                 
                 )             
            )  
            ),
  
  # Sidebar   
 # sidebarLayout(
    
    # Customization Pane
  #  sidebarPanel(id="sidebar",
          #       setSliderColor(c("#002244", "#009FDA"), c(1, 2)),
         #        chooseSliderSkin("Nice"),
              #   img(src="CEM_logo.png", height="150",align="center"),
              #   h3(strong("TARGET COUNTRY"),style="color:#002244"),
              #   selectInput("TARGET","STEP 1 Select Target Country",
              #               choices=country,"Albania", multiple = F),
              #   h4(""),
             #    sliderInput("YEAR","STEP 2 Select Period", min=1960, max=2018,value=c(2012,2017),sep=""),
             #    h4(""),
            #     h5(strong("Profile")),
            #     h4(textOutput("country"),style="color:#009FDA"),
            #     textOutput("country.region.txt"),
            #     textOutput("country.income"),
            #     textOutput("country.land"),
            #     textOutput("country.small"),
            #     textOutput("country.fcs"),
            #     h3(strong("STRUCTURAL BREAK"),style="color:#009FDA"),
            #     sliderInput("YEAR2","Structural Break Period", min=1981, max=2017,value=c(1981,2017),sep=""),
            #     h4(" "),
            #     h5(textOutput("break_point_txt"), style="color:#009FDA"),
          #       tableOutput("break_point"),
                # tableOutput("break_data"),
            #     h4(" "),
            #     withSpinner(plotOutput("break_plot",height="200px"),color="#009FDA"),
            #     img(src="wbg_efi.png", height="50")
                # checkboxInput("REGION", "Regional Peers", value=FALSE),
                # checkboxInput("LAND", "Landlocked", value=FALSE),
                # checkboxInput("SMALL", "Small States", value=FALSE),
                # checkboxInput("FCS", "Fragile & Conflicted States", value=FALSE)
       #         ),
    
    # Main Panel: Structural and Aspirational
 #   mainPanel(
      navlistPanel(#type="tabs",
                  well=F,
    
                  ## Instruction Page
                  tabPanel("Introduction"),
                  tabPanel("STEP 1: Target Country",
                           
                           setSliderColor(c("#002244", "#009FDA"), c(1, 2)),
                           chooseSliderSkin("Nice"),
                           
                          
                      #     h3(strong("TARGET COUNTRY"),style="color:#002244"),
                      
                           h5(strong("INSTRUCTION")),
                      #   p("Structural comparators are defined as countries that are similar to the target country in terms of selected indicators."),
                           h5("Pick a target country from the dropdown list as your country to analyse. ", style="color:#009FDA"),
                      
                      
                      
                      
                           selectInput("TARGET","Select Target Country",
                                       choices=country,"Albania", multiple = F),
                           h4(""),
                           h5(strong("Basic Information")),
                           h4(textOutput("country"),style="color:#009FDA"),
                           textOutput("country.region.txt"),
                           textOutput("country.income"),
                           textOutput("country.land"),
                           textOutput("country.small"),
                           textOutput("country.fcs")
                           
                           ),
                  
                  tabPanel("STEP 2: Select Period",
                           
                           h5(strong("INSTRUCTION")),
                           #   p("Structural comparators are defined as countries that are similar to the target country in terms of selected indicators."),
                           h5("Step 2A is to determine period to select your structural and aspirational comparators in STEP 3 and STEP 4. Step 2B is to determine structural break year given your input period of time.", style="color:#009FDA"),
                           
              
                           
                           sliderInput("YEAR","STEP 2A: Select Period", min=1960, max=2018,value=c(2012,2017),sep=""),
    
                           h4(" "),        
                      #     h3(strong("STRUCTURAL BREAK"),style="color:#009FDA"),
                           sliderInput("YEAR2","STEP 2B: Structural Break", min=1981, max=2017,value=c(1981,2017),sep=""),
                           h4(" "),
                           h5(textOutput("break_point_txt"), style="color:#009FDA"),
                           h4(" "),
                           withSpinner(plotOutput("break_plot",height="200px"),color="#009FDA"),
                           img(src="wbg_efi.png", height="50")
                         
                           
                  ),
                  
                  
                  
                  ## Structural Comparators
                  tabPanel("STEP 3: Structural Comparators",
                        #  h3("Find Structural Comparators", style="color:#002244"),
                        #   h3(textOutput("country.txt"), style="color:#009FDA"),
                         h5(strong("INSTRUCTION")),
                        #   p("Structural comparators are defined as countries that are similar to the target country in terms of selected indicators."),
                         h5("Structural comparators are the peer countries that are most similar to your target country in terms of selected indicators. Here the similarity is measured by how close other countries are to the target countries based on their relative global ranking of each indicator. You can pick up to three structural indicators to select your structural peer countries.", style="color:#009FDA"),
                         h5(strong("Select Structural Indicators (up to 6)")),
                      # Title of indicator selections
                         fluidRow(
                           column(4,
                                 "Structural indicators"
                                 ),

                           column(3,
                                  "Weight"
                                 ),
                           
                           column(5,
                                  textOutput("period")
                                 )
                           
                                 ),
                      # Indicator 1
                         fluidRow(
                           column(4,#style="margin-bottom:0px;padding-right:0px;padding-bottom:0px",
                               selectInput("INDICATOR1",NULL,choices=unique(indicator_file$Description),selected="GDP (constant 2010 US$)")
                                 ),
  
                           column(3, 
                                conditionalPanel(
                                  condition = "input.INDICATOR1 != 'Select'",
                                  numericInput("W1",NULL,value=1)
                                                )
                                 ),
                           
                           column(5, 
                                  conditionalPanel(
                                    condition = "input.INDICATOR1 != 'Select'",
                                    h4(textOutput("ind1"), style="margin-top:2%;color:#009FDA")
                                  )
                                  )
                           
                                 ),
                      # Indicator 2    
                      fluidRow(
                        column(4, #style="margin-top:0px;padding-right:0px;padding-top:0px",
                               selectInput("INDICATOR2",NULL,choices=unique(indicator_file$Description),selected="Select")
                        ),
                        
                        column(3, 
                               conditionalPanel(
                                 condition = "input.INDICATOR2 != 'Select'",
                                 numericInput("W2",NULL,value=1)
                               )
                        ),
                        
                        column(5, 
                               conditionalPanel(
                                 condition = "input.INDICATOR2 != 'Select'",
                                 h4(textOutput("ind2"), style="margin-top:2%;color:#009FDA")
                               )
                               )
                        
                      
                              ),
                      # Indicator 3
                      fluidRow(
                        column(4,
                               selectInput("INDICATOR3",NULL,choices=unique(indicator_file$Description),selected="Select")
                        ),
                        
                        column(3, 
                               conditionalPanel(
                                 condition = "input.INDICATOR3 != 'Select'",
                                 numericInput("W3",NULL,value=1)
                                               )
                              ),
                        
                        column(5, 
                               conditionalPanel(
                                 condition = "input.INDICATOR3 != 'Select'",
                                 h4(textOutput("ind3"), style="margin-top:2%;color:#009FDA")
                               )
                               )
                              ),
                      
                      # Indicator 4
                      fluidRow(
                        column(4,
                               selectInput("INDICATOR4",NULL,choices=unique(indicator_file$Description),selected="Select")
                        ),
                        
                        column(3, 
                               conditionalPanel(
                                 condition = "input.INDICATOR4 != 'Select'",
                                 numericInput("W4",NULL,value=1)
                                               )
                              ),
                        
                        column(5, 
                               conditionalPanel(
                                 condition = "input.INDICATOR4 != 'Select'",
                                 h4(textOutput("ind4"), style="margin-top:2%;color:#009FDA")
                               )
                               )
                        
                        
                              ),
                      
                      # Indicator 5
                      fluidRow(
                        column(4,
                               selectInput("INDICATOR5",NULL,choices=unique(indicator_file$Description),selected="Select")
                               ),
                        
                        column(3, 
                               conditionalPanel(
                                 condition = "input.INDICATOR5 != 'Select'",
                                 numericInput("W5",NULL,value=1)
                                               )   
                              ),
                        
                        column(5, 
                               conditionalPanel(
                                 condition = "input.INDICATOR5 != 'Select'",
                                 h4(textOutput("ind5"), style="margin-top:2%;color:#009FDA")
                               )
                               )
                        
                              ),
                      
                      # Indicator 6
                      fluidRow(
                        column(4,
                               selectInput("INDICATOR6",NULL,choices=unique(indicator_file$Description),selected="Select")
                        ),
                        column(3, 
                               conditionalPanel(
                                 condition = "input.INDICATOR6 != 'Select'",
                                 numericInput("W6",NULL,value=1)
                                               )
                              ),
                        
                        column(5, 
                               conditionalPanel(
                                 condition = "input.INDICATOR6 != 'Select'",
                                 h4(textOutput("ind6"), style="margin-top:2%;color:#009FDA")
                               )
                               )
                        
                              ),
                      
                      h3("                      "),
                      radioButtons("RESTRICTION","Optional: Select Category", choices=c("All"="all","Regional"="region","Landlocked"="landlocked", "Small States"="small"), inline=T, selected="all"),
                      
                      # Structural data table
                      h3("                      "),
          tabsetPanel(type="tabs",
                      tabPanel("Results",  
                      h4("                      "),
                      h4("Structural Comparators: Top Most Structurally Similar Countries",style="margin-top:20px;color:#009FDA"), 
                      
                      fluidRow(
                        column(3,
                        numericInput("STRUC_TOP","Number of Comparators", value=10, max=200),
                        p(" "),
                        downloadButton("DOWNLOAD_STRUC_LIST","Download the List", class="butt1",
                                       tags$head(tags$style(".butt1{background-color:#009FDA;color:white;}")))
                              )
                      ),
                      
                      h4(" "),
                  
               #       radioButtons("RESTRICTION","Select Category", choices=c("All"="all","Regional"="region","Landlocked"="landlocked", "Small States"="small"), inline=T, selected="all"),
                           
                      fluidRow(
                          column(5,
                          tableOutput("struc_result")
                                ),
                          column(6,
                                 plotlyOutput("struc_map")
                                 )
                              ),
               
                      p("Note: In descending order of similarity", style="color:grey")
            #          tableOutput("struc_rank"),
           #           tableOutput("struc_table"),
          #            tableOutput("str_matchind")
                      ),
                      tabPanel("Data", 
                      h4("      "),
                      p("Table below contains the actual value of structural indicators per structural comparators, which is the simply average of selected period of years.", style="color:grey"),
                      
                      
                      # Download button
                      fluidRow(
                        column(3, style="margin:0px",
                               downloadButton("DOWNLOAD_STRUC_TOP","Download Selected Data", class="butt1",
                                              tags$head(tags$style(".butt1{background-color:#002244;} .butt1{color: white; }")))
                               ),
                        column(4,
                               downloadButton("DOWNLOAD_STRUC","Download Full Data (All Countries)", class="butt2",
                                              tags$head(tags$style(".butt2{background-color:#009FDA;} .butt2{color: white;}")))
                               )
                        
                      ),
                      
                      h4(" "),
                      
                      tableOutput("struc_result_data")
             #         tableOutput("struc_table")
                      )
                      )
                          ),
          
          
         tabPanel("STEP 4: Aspirational Comparators",
             
             #  h3("Find Aspiratio Comparators", style="color:#002244"),
    #         h3(textOutput("country.txt"), style="color:#009FDA"),
             #   p("Structural comparators are defined as countries that are similar to the target country in terms of selected indicators."),
             
             h5(strong("INSTRUCTION")),
             h5("After you have selected your structural indicators, pick an aspirational indicator where you would like other countries that have better performance than your target countries. You can define performance either measured by higher global ranking or better actual value.", style="color:#009FDA"),
    
             h5(strong("Select Aspirational Indicator"),style="margin-bottom:10px"),
             # Title of indicator selections
             fluidRow(
               column(4,
                      "Aspirational Indicator"
               ),
               
               column(2,
                      "Rank"
                     # textOutput("period")
               ),
               
               column(2,
                      "Average"
                      # textOutput("period")
               ),
               
               column(2,
                      "Max"
                      # textOutput("period")
               )
                     ),
    
              fluidRow(
               column(4,#style="margin-bottom:0px;padding-right:0px;padding-bottom:0px",
                    selectInput("ASPR",NULL,choices=unique(indicator_file$Description),selected="GDP (constant 2010 US$)")
                     ),
               
               column(2, 
                    conditionalPanel(
                    condition = "input.ASPR != 'Select'",
                    h4(textOutput("aspr_target_rank"), style="margin-top:5%;color:#009FDA")
                                    )
                     ),
               
               column(2, 
                      conditionalPanel(
                        condition = "input.ASPR != 'Select'",
                        h4(textOutput("aspr_target_mean"), style="margin-top:5%;color:#009FDA")
                                      )
                     ),
               
               column(2, 
                      conditionalPanel(
                        condition = "input.ASPR != 'Select'",
                        h4(textOutput("aspr_target_max"), style="margin-top:5%;color:#009FDA")
                                      )
                      )
               
                     ),
               
               # determine by rank or by value
               h4("  "),
               h5(strong("Lower and Upper Bound"),style="margin-bottom:0px"),
    
               # set the lower and upper bound numeric input
               fluidRow(
               
               column(2,  
               h5("Lower Rank",style="color:#009FDA;margin-bottom:0px")
                     ),
               column(2,
               h5("Upper Rank",style="color:#009FDA;margin-bottom:0px")
                     ),
               column(1,
               h4("")       
                      ),
               column(2,  
                      h5("Lower Value",style="color:#002244")
               ),
               column(2,
                      h5("Upper Value",style="color:#002244")
               )
               
               
                       ),
               fluidRow(
               
        
               column(2,
                      numericInput("RANK_L",NULL,value=1, min=1, max=233, step=1)
                     ),

               column(2,
                      numericInput("RANK_U",NULL,value=1, min=1, max=233, step=1)
                     ),
               column(1,
               h4("")
                     ),
               column(2,
                      numericInput("VALUE_L",NULL,value=1, step=1)
               ),
               
               column(2,
                      numericInput("VALUE_U",NULL,value=1, step=1)
               )
                       ),
              
              h3("      "),
              radioButtons("RANKVALUE","Select Criteria", choices=c("By Rank"="rank","By Value"="value"), inline=T, selected="rank"),
              h3("      "), 
      # results and data tabs for aspirational comparators
      tabsetPanel(type="tabs",
                tabPanel("Results",
                          h4("Aspirational Comparators: Top 10 Most Structurally Similar Countries Ahead",style="margin-top:20px;color:#009FDA"),
 #                         radioButtons("RANKVALUE","Select Category", choices=c("By Rank"="rank","By Value"="value"), inline=T, selected="rank"),
                        #  tableOutput("aspr_within_rank")
              fluidRow(
               column(3,
                numericInput("ASPR_TOP","Number of Comparators", value=10, max=200),
                p(" "),
                downloadButton("DOWNLOAD_ASPR_LIST","Download the List", class="butt1",
                               tags$head(tags$style(".butt1{background-color:#002244;} .butt1{color: white; }")))
                     )
                      ),
                h4(" "),         
              fluidRow(
                column(5,
                          tableOutput("aspr_result"),
                          p("Note: In descending order of similarity", style="color:grey")
                      ),
                
                column(6,
                          plotlyOutput("aspr_map")
                      )   
                      )
                        ),
                tabPanel("Data",
                         h4(" "),
                         p("Table below contains the actual value of structural indicators per structural comparators, which is the simply average of selected period of years. Listed countries are countries within selected range only.", style="color:grey"),
                         
                         downloadButton("DOWNLOAD_ASPR_TOP","Download Selected Data", class="butt1",
                                        tags$head(tags$style(".butt1{background-color:#002244;} .butt1{color: white; }"))),
                         h4(" "),
                         tableOutput("aspr_result_data")
                        )
                
                 )
    
    
    
             
                 ),
      
        tabPanel("STEP 5: Finalize CEM Input",
                 
                 h5(strong("INSTRUCTION")),
                 #   p("Structural comparators are defined as countries that are similar to the target country in terms of selected indicators."),
                 h5("Select your period analysis, including total period, historical period and recent period. Next step is to finalize your list of comparators that are going to be output in your CEM data table. This should include 3 structural peers, 3 aspirational peers, and 3 group typologies that are commonly used in WBG.", style="color:#009FDA"),
                 
                 
                 h4("Period of Analysis",style="color:#009FDA"),
                 # Period selection
                 fluidRow(
                   column(3,
                         h4("Total Period")
                         ),
                   column(3,
                          selectInput("TT_ST",NULL, choices=c(1980:2014), 2000)
                         ),
                   column(3,
                          selectInput("TT_ED",NULL, choices=c(1980:2018), NULL)
                         )
                         ),
                 
                 fluidRow(
                   column(3,
                          h4("Historical Period")
                   ),
                   column(3,
                         selectInput("HS_ST",NULL, choices=(1980:2018), NULL)
                        #  p(textOutput("his_start"),style="padding-left:50px")
                   ),
                   column(3,
                          selectInput("HS_ED",NULL, choices=c(1980:2018), NULL)
                   )
                 ),
                 
                 fluidRow(
                   column(3,
                          h4("Recent Period")
                   ),
                   column(3,
                          selectInput("RS_ST",NULL, choices=c(1980:2018), NULL)
                   ),
                   column(3,
                          selectInput("RS_ED",NULL, choices=c(1980:2018), NULL)
                      #    textOutput("recent_end")
                   )
                 ),
             
                 
                 # List of comparators selected
                 
                   # 3 Structural comparators
                 h4(" "),
                 h4("Final List of Comparators",style="color:#009FDA"),
                 fluidRow(
                 column(4,
                 selectInput("STRUT1","Structural Comparators",
                             choices=unique(data_file$countryname),"China", multiple = F),
                 selectInput("STRUT2",NULL,
                             choices=unique(data_file$countryname),"Albania", multiple = F),
                 selectInput("STRUT3",NULL,
                             choices=unique(data_file$countryname),"Albania", multiple = F)
                 ),
                   # 3 Aspirational comparators
                 column(4,
                 selectInput("ASPR1","Aspirational Comparators",
                             choices=unique(data_file$countryname),"Albania", multiple = F),
                 selectInput("ASPR2",NULL,
                             choices=unique(data_file$countryname),"Albania", multiple = F),
                 selectInput("ASPR3",NULL,
                             choices=unique(data_file$countryname),"Albania", multiple = F)
                 )
                 ),
                 h4(" "),
                   # 3 Typology groups
                 fluidRow(
                 column(4,
                 selectInput("TYPO1","Other Group Comparators",
                             choices=c(
                               
                               "High income",                     "Low income",                     "Lower middle income",             "Upper middle income",             "EAP, excluding high income",    
                               "ECA, excluding high income",      "LAC, excluding high income",      "MNA, excluding high income",      "SAR, excluding high income",      "SSA, excluding high income",     
                               "EAP, all",                       "ECA, all",                        "LAC, all",                        "MNA, all",                       "NAM, all",                       
                               "SAR, all",                        "SSA, all",                        "BRICS",                           "OECD",                            "OPEC",                           
                               "G20",                             "ASEAN",                           "Caribbean",                       "CIS",                             "ECOWAS",                         
                               "Euro Union",                     "GCC",                             "Pacific Islands",                 "Blend",                           "IBRD",                           
                               "IDA",                             "Advanced, IMF",                   "Emerging & Developing, IMF",      "Low Income Developing Countries", "Land-locked",                    
                               "Small States",                    "Island States",                   "Fragile States",                  "EMU",                             "HIPC",                           
                               "Non-HIPC",                        "Oil Exporter",                    "Oil Importer",                    "Commodity Exporting Countries" 
                             ),
                             
                   
                             "High income", multiple = F),
                 
                 selectInput("TYPO2",NULL,
                             choices=c(
                               
                               "High income",                     "Low income",                     "Lower middle income",             "Upper middle income",             "EAP, excluding high income",    
                               "ECA, excluding high income",      "LAC, excluding high income",      "MNA, excluding high income",      "SAR, excluding high income",      "SSA, excluding high income",     
                               "EAP, all",                       "ECA, all",                        "LAC, all",                        "MNA, all",                       "NAM, all",                       
                               "SAR, all",                        "SSA, all",                        "BRICS",                           "OECD",                            "OPEC",                           
                               "G20",                             "ASEAN",                           "Caribbean",                       "CIS",                             "ECOWAS",                         
                               "Euro Union",                     "GCC",                             "Pacific Islands",                 "Blend",                           "IBRD",                           
                               "IDA",                             "Advanced, IMF",                   "Emerging & Developing, IMF",      "Low Income Developing Countries", "Land-locked",                    
                               "Small States",                    "Island States",                   "Fragile States",                  "EMU",                             "HIPC",                           
                               "Non-HIPC",                        "Oil Exporter",                    "Oil Importer",                    "Commodity Exporting Countries" 
                             ),
                             "OECD", multiple = F),
                 
                 selectInput("TYPO3",NULL,
                             choices=c(
                               
                               "High income",                     "Low income",                     "Lower middle income",             "Upper middle income",             "EAP, excluding high income",    
                               "ECA, excluding high income",      "LAC, excluding high income",      "MNA, excluding high income",      "SAR, excluding high income",      "SSA, excluding high income",     
                               "EAP, all",                       "ECA, all",                        "LAC, all",                        "MNA, all",                       "NAM, all",                       
                               "SAR, all",                        "SSA, all",                        "BRICS",                           "OECD",                            "OPEC",                           
                               "G20",                             "ASEAN",                           "Caribbean",                       "CIS",                             "ECOWAS",                         
                               "Euro Union",                     "GCC",                             "Pacific Islands",                 "Blend",                           "IBRD",                           
                               "IDA",                             "Advanced, IMF",                   "Emerging & Developing, IMF",      "Low Income Developing Countries", "Land-locked",                    
                               "Small States",                    "Island States",                   "Fragile States",                  "EMU",                             "HIPC",                           
                               "Non-HIPC",                        "Oil Exporter",                    "Oil Importer",                    "Commodity Exporting Countries" 
                             ),
                             "IBRD", multiple = F)
                       )
                 ),
                 
                   # List table
                 h4(" "),
                 h5("You are almost done! Click the following blue button to download data table that will be equipped to your CEM 2.0 Excel tool. It might take a few seconds."),
                 downloadButton("DOWNLOAD_CEM","Download CEM Data Input", class="butt1",
                                tags$head(tags$style(".butt1{background-color:#002244;} .butt1{color: white; }")))#,
#                 tableOutput("normal_result")
                 
                 
        
                )
     
    
    
    
  # DON'T TOUCH PARENTHESIS BELOW THIS LINE  
    
    
                 )
          #   )
          #     )
  

  
)

