
library(shiny)
library(shinydashboard)
library(dplyr)
library(igraph)
library(tidygraph)
library(tidyr)
library(ggraph)
library(readr)
library(sf)
library(ggplot2)
library(leaflet)
library(rgdal)
library(units)

# source('C:/Users/Cassiano/Hubic/Amazon_birds/R_Amazon_birds/SOURCE_SCAN_network_14.R', local =  TRUE)


# UI

shinyUI(

     dashboardPage(
        
        dashboardHeader(title = "SCAN engine"),
        
        dashboardSidebar( width = '230px',
                          
                          sidebarMenu(
                              
                              menuItem("Species maps and Cs ", tabName = "maps_and_Cs"),
                              
                              menuItem("SCAN analysis", tabName = "scan")
                              
                          ) # menu items
        ),
        
        dashboardBody(
            
            tabItems(
                
                tabItem("maps_and_Cs",
                        
                    fluidRow(
                        
                        box(
                            
                                    tags$h3("Map of Species Distributions"),
                            
                            fileInput( inputId = "filemap", label = "Pls choose a shapefile (.shp +  .shx + .dbl + .prj) with all species identified in the first column", accept = c('.shp','.dbf','.sbn','.sbx','.shx',".prj"),width = '500px', multiple=TRUE),  
                            
                            checkboxInput("modify_crs", "Change map projection (crs)", value = FALSE),
                            
                            conditionalPanel(condition = "input.modify_crs == true", 
                                                
                                 numericInput(inputId = "map_projection", label = "Choose a crs code to project the map", value = 4326)
                            ),
                            
                                    tags$h6("WGS84: 4326; SIRGAS2000: 4674, 31985; SAD69: 4618"),
                            
                            actionButton("get_map", "Read the map"),
                            
                                    tags$h4("Map sample"),
                            
                                    tags$h5("See the list of all species below"),
                            
                            plotOutput("map_shp")
                            
                        ), # map
                        
                        box(
                            
                                    tags$h3("Spatial Congruence Cs"),
                                     
                                    tags$h5("You can calculate the Spatial Congruence (Hargrove Index - Cs) based on
                                             the species distributions in the map, or upload a csv file  structure as  species 1 ('sp1'), species 2 ('sp2'), and 'Cs' (numeric 0-1)."),
                            
                            actionButton("calculate_Cs","Apply Cs index to map"),
                                             
                                    #tags$h6("Do NOT change TAB while running ! This may take a long time depending on the number of species ... "),
                                     
                            # radioButtons(inputId = "calculateCs", label = "Apply Cs formula to uploaded map or upload a .csv file ?", 
                            #              selected = character(0), choices = c('Calculate' = "calculateCs", "Upload csv file" = 'Cs_upload_csv')),
                                          
                            
                            
                            checkboxInput("Cs_upload_csv", "Upload a Cs csv file instead"),
                            
                            conditionalPanel( condition = "input.Cs_upload_csv == true ",
                                              
                                   fileInput( inputId = "Cs_table", label = "Select a Cs table .csv file", accept = c("text/csv","text/comma-separated-values,text/plain",".csv" ))
                            ), 
                            
                            # tags$h4("Choose a lower limit to spatial congruence Cs ?"),
                            #  
                            checkboxInput("apply_filter_Cs", "Apply filter to Cs", value = FALSE),

                            conditionalPanel( condition = "input.apply_filter_Cs == true",
                                              
                                    numericInput( inputId = "filter_Cs", label = "Minimum Cs value (default = 0.1)", value = 0.1)
                            ),
                            
                            downloadButton("download_Cs", "Download Cs.csv"),
                            
                            verbatimTextOutput("check_Cs_tables"),
                            
                            tags$h5("Table of species' spatial similarities Cs (head and tail)"),
                            
                            
                            
                                tableOutput("Cs_head"),
                                
                                tableOutput("Cs_tail")
                            
                            
                                 
                        ) # Cs
                    ),
                
                    fluidRow(
                        
                        box( width = 6, 
                             
                            tags$h4("Check column names and species in the maps"), 
                                     
                            textOutput("map_shp_names"),
                            
                            tableOutput("map_species"),
                            
                            tags$h5("check graph nodes and edges"),
                            
                            tableOutput("graph_nodes"),
                            
                            tableOutput("graph_edges")
                        )
                    )
                ),
                
                tabItem("scan",
                        
                    fluidRow(
                        
                        box(

                            tags$h3("SCAN"),

                            tags$h6("After calculating or uploading a Cs table of pairwise spatial similarities, you can parameterize and run SCAN!"),

                            numericInput(inputId = "resolution", label = "Select the resolution (interval between Ct)", value = 0.02),

                            sliderInput( inputId = "threshold_min_max", label = "Select the threshold range to SCAN", value = c(0.8, 1), min = 0.2, max = 1, step = 0.01 ),

                            numericInput(inputId = "max_diameter", label = "Choose maximum network diameter for a chorotype", value = 15),

                            checkboxInput(inputId = "overlap", label = "Overlap between all species?", value = TRUE),

                            actionButton("run_scan", "SCAN!")

                        ),
                        
                        box(

                            tags$h5("Check if Cs data table has the columns sp1, sp2, and the numeric Cs"),

                            tableOutput("Cs_table"),

                            tags$h4("Wait for the results and check the parameters of the SCAN analysis"),

                            tableOutput("parameters"),

                            # selectInput("dataset", "Download SCAN results one by one",choices = c("chorotypes", "all_spp_summary", "all_spp", "graph_nodes", "graph_edges")),

                            uiOutput("names_scan_list"),  # input$dataset is here in serverUI

                            # Download
                            downloadButton("downloadData", "Download"),
                            
                            
                            

                        )
                    ),
                        
                    # fluidRow(
                    #             
                    #     dataTableOutput('table'),
                    #     
                    #     tags$h3('test'),
                    #             
                    #     tableOutput("scan_chorotypes")
                    #         
                    # )
                    
                )
            )   
        )
    )
)


