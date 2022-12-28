#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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

extrafont::loadfonts(device="win")



# UI

shinyUI(

     dashboardPage(
        
        dashboardHeader(title = "SCAN engine"),
        
        dashboardSidebar( width = '230px',
                          
                          sidebarMenu(
                              
                              menuItem("Species maps and Cs ", tabName = "maps_and_Cs"),
                              
                              menuItem("SCAN analysis", tabName = "scan"),
                              
                              menuItem("SCAN viewer v1.1", tabName = "SCAN_viewer"),
                              
                              menuItem("about SCAN", tabName = "about_SCAN")
                              
                          ) # menu items
        ),
        
        dashboardBody(
            
            tabItems(
                
                tabItem("maps_and_Cs",
                        
                    fluidRow(
                        
                        box(tags$h3("Map of Species Distributions"),
                            
                            fileInput(width = '500px', 
                                      inputId = "filemap", 
                                      label = "Pls choose a shapefile (.shp +  .shx + .dbl + .prj) with all species identified in the first column",  
                                      accept = c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), 
                                      multiple=TRUE),
                        
                            checkboxInput("modify_crs", "Change map projection (crs)", value = FALSE),
                            
                            conditionalPanel(   condition = "input.modify_crs == true",
                                                
                                                numericInput(inputId = "map_projection", label = "Choose a crs code to project 
                                                            
                                                             the map", value = 4326) #  (WGS84: 4326; SIRGAS2000: 4674,31985 etc)
                            ),
                            
                            tags$h6("WGS84: 4326; SIRGAS2000: 4674, 31985; SAD69: 4618"),
                            
                            actionButton("get_map", "Check all species and print a map (if n > 250 spp only a subset)"),
                            
                            tags$h4("Map sample"),
                            
                            tags$h5("See list of species below the map"),
                            
                            plotOutput("map_shp")
                            
                        ), # map
                        
                        box( tags$h3("Spatial Congruence Cs"),
                             
                             tags$h5(""),
                             
                             tags$h5("You can calculate the Spatial Congruence (Hargrove Index - Cs) based on
                                     the species distributions in the map, or upload a csv file  structure as  species 1 ('sp1'), species 2 ('sp2'), and 'Cs' (numeric 0-1)."),
                             
                             tags$h6("Do NOT change TAB while running ! This may take a long time depending on the number of species ... "),
                             
                             radioButtons(inputId = "calculate_Cs", label = "Apply Cs formula to uploaded map or upload a .csv file ?",
                                          
                                          selected = character(0),
                                          
                                          choices = c('Apply Index' = "calculate_Cs", "Upload .csv file" = "upload_Cs.csv")),
                             # MOD 
                             conditionalPanel( condition = "input.calculate_Cs == 'upload_Cs.csv'", 
                                               
                                               fileInput( inputId = "Cs_table", label = "Cs table in csv file", 
                                                          
                                                          accept = c("text/csv","text/comma-separated-values,text/plain",".csv" ) ),
                                               
                             ),
                             
                             
                             
                             
                             tags$h4("Choose the lower limit to species pairs spatial similarities Cs ?"),
                             
                             checkboxInput("apply_filter_Cs", "Apply filter to Cs", value = FALSE),
                             
                             conditionalPanel( condition = "input.apply_filter_Cs",
                                               
                                    numericInput(inputId = "filter_Cs", label = "Minimum Cs value (default = 0.1)", value = 0.1) 
                            ),
                            # tags$h5("Species pairs with spatial overlap (head)"),
                            # tableOutput("overlapping_head"),
                             
                            tags$h5("Table of species' spatial similarities Cs (head)"),
                            
                            # Download
                            downloadButton("download_Cs", "Download Cs.csv"),
                         
                            tableOutput("Cs_head")
                                 
                        ) # Cs
                    ),
                
                    fluidRow(
                        
                        box( width = 6, 
                             
                            tags$h4("Check column names and species in the maps"), 
                                     
                            textOutput("map_shp_names"),
                            
                            tableOutput("map_species")         
                        )
                    )
                ),
                
                tabItem("scan",
                        
                        fluidRow(
                            
                            box( tags$h3("SCAN"),
                                 # checkboxInput(inputId = "use_semicolon_delimited_csv", label = "use semicolon delimited csv", value = TRUE),
                                 
                                 tags$h6("After calculating or uploading a Cs table of pairwise spatial similarities, you can parameterize and run SCAN!"),
                                 
                                 tags$h6(""),
                                 
                                 numericInput(inputId = "resolution", label = "Select the resolution (interval between Ct)", value = 0.02),
                                 
                                 sliderInput( inputId = "threshold_min_max", label = "Select the threshold range to SCAN", 
                                              
                                              value = c(0.8, 1), min = 0.2, max = 1, step = 0.01 ),
                                 
                                 numericInput(inputId = "max_diameter", label = "Choose maximum network diameter for a chorotype", value = 15),
                                 
                                 checkboxInput(inputId = "overlap", label = "Overlap between all species?", value = TRUE),
                                 
                                 actionButton("run", "SCAN!")
                                 
                            ),
                            
                            box(tags$h5("Check if Cs data table has the columns sp1, sp2, and the numeric Cs"),
                                
                                tableOutput("Cs_table"),
                                
                                tags$h4("Wait for the results and check the parameters of the SCAN analysis"),
                                
                                tableOutput("parameters"),
                                
                                # selectInput("dataset", "Download SCAN results one by one",choices = c("chorotypes", "all_spp_summary", "all_spp", "graph_nodes", "graph_edges")),
                                
                                uiOutput("names_scan_list"),  # input$dataset is here in serverUI
                                
                                # Download
                                downloadButton("downloadData", "Download"),
                                
                            ),
                            
                            mainPanel(
                                
                                fluidRow(
                                    
                                    dataTableOutput('table')
                                )
                            )
                        )
                ),
                
                tabItem("SCAN_viewer",
                        
                        fluidRow(
                            box(width = 2,
                                
                                conditionalPanel(     condition = "input.graph_or_csv == false",
                                                      selectInput(inputId = "g", label = "choose a SCAN graph:",
                                                                  choices = c("SCANlist_graph"))
                                ),
                                
                                checkboxInput("graph_or_csv", "Input csv graph?", FALSE),
                                
                                conditionalPanel(     condition = "input.graph_or_csv == true",
                                                      
                                                      fileInput(inputId = "graph_nodes", label = "graph NODES in csv file", accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                                                      
                                                      fileInput(inputId = "graph_edges", label = "graph EDGES in csv",  accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
                                ),
                                
                                # sliderInput(inputId = "threshold", label = "threshold", value = 0.91, min = 0.21, max = 0.97, step = 0.02),
                                
                                numericInput(inputId = "threshold", label = "threshold between 0 and 1", value = 0.9, max = 1, min = 0.1),
                                
                                selectInput(inputId = "palette", label = "choose palette",choices = c("Dark2","Set1","Paired","Accent","Spectral","Greens", "Reds","BrBG" , "RdYlGn","PiYG")),
                                
                                selectInput("layout", "graph layout", choices = c("fr", "kk", "dh", "drl", "mds", "gem")),
                                
                                numericInput("map_alpha", "map alpha", max = 1, min = 0.01, value = 0.2),
                                
                                menuItem("Map & Graph", tabName = "map_graph"),
                                
                                menuItem("Map", tabName = "map_ggplot"),
                                
                                menuItem("about SCAN", tabName = "about_SCAN")
                                
                                
                            ),
                            
                            box(width = 10,
                                
                                uiOutput("original_components"),
                                
                                box(    tags$h4("Map"), leafletOutput("map_plot", height = "500px"), width = 8  ),
                                
                                box(    tags$h4("Graph"),plotOutput("graph_plot"), width = 4   )
                            )
                        )
                ),
                
                tabItem("about_SCAN",   
                        
                        fluidRow(
                            
                            box(
                                tags$h3("SCAN"),
                                tags$h5("
                            Chorotypes are unique combinations of species with spatial congruences Cs higher between themselves than to any species of other such groups.
                            In SCAN species groupings are relative to (and determined by) thresholds of congruence Ct.
                            Each chorotype is a 'community' (in network terminology), as represented in the graph: links are Cs values.
                            The map depicts the actual spatial distribution of each component species of a chorotype.
                            Chorotype may 'evolve' as thresholds get lower, grouping more species, until a criterion of spatial overlap is violated.
                            Some groups exist only at higher Cts; others only at low Cts - it depends on the ecology and history of species and environments.
                            see Gatto & Cohn-Haft 2021 - PlosOne https://doi.org/10.1371/journal.pone.0245818" )
                            )
                        )
                )
            )   
        )
    )
)

