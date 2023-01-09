
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
                              
                              menuItem("SCAN analysis", tabName = "scan"),
                              
                              menuItem("SCAN viewer v1.1", tabName = "SCAN_viewer"),
                              
                              menuItem("about SCAN", tabName = "about_SCAN")
                              
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
                            
                            # >>>>   
                            conditionalPanel( condition = "input.calculate_Cs > 0",   tags$h6("Do NOT change TAB while running ! This may take a long time depending on the number of species ... ")   ),         
                            
                            checkboxInput("Cs_upload_csv", "Upload a Cs csv file instead"),
                            
                            conditionalPanel( condition = "input.Cs_upload_csv == true ",
                                              
                                   fileInput( inputId = "Cs_table", label = "Select a Cs table .csv file", accept = c("text/csv","text/comma-separated-values,text/plain",".csv" ))
                            ), 
                            
                            # tags$h4("Choose a lower limit to spatial congruence Cs ?"),
                            # checkboxInput("apply_filter_Cs", "Apply filter to Cs?", value = FALSE),
                            # conditionalPanel( condition = "input.apply_filter_Cs == true", numericInput( inputId = "filter_Cs", label = "Minimum Cs value (default = 0.1)", value = 0.1) ),
                            
                            downloadButton("download_Cs", "Download Cs.csv"),
                            
                            ' ',
                            
                            verbatimTextOutput("check_Cs_tables"),
                            
                            ' ',
                            
                            tags$h5("Table of species' spatial similarities Cs (head and tail)"),
                            
                            
                            fluidRow(
                                
                                column(  width = 3,   tableOutput("Cs_head")  ),
                                    
                                column(  width = 3,   tableOutput("Cs_tail")  )
                            )
                            
                                 
                        ) # Cs
                    ),
                
                    fluidRow(
                        
                         box(
                             
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
                        
                        box( width = 4,

                                    tags$h3("SCAN"),

                                    tags$h6("After calculating or uploading a Cs table of pairwise spatial similarities, you may parameterize and run SCAN!"),

                            numericInput(inputId = "resolution", label = "Resolution (interval between Ct)", value = 0.02),

                            numericInput(inputId = "threshold_max", label = "Max value of threshold Ct", value = 0.9, min = 0.05, max = 1),
                            
                            numericInput(inputId = "threshold_min", label = "Min value of threshold Ct", value = 0.2, min = 0.05, max = 0.9),
                            
                            #sliderInput( inputId = "threshold_min_max", label = "Select the threshold range to SCAN", value = c(0.8, 1), min = 0.2, max = 1, step = 0.01 ),

                            numericInput(inputId = "max_diameter", label = "Choose maximum network diameter for a chorotype", value = 15),

                            checkboxInput(inputId = "overlap", label = "Overlap between all species?", value = TRUE),

                            actionButton("run_scan", "SCAN!")

                        ),
                        
                        box( width = 8,

                            tags$h5("Check if Cs data table has the columns sp1, sp2, and the numeric Cs"),

                            tableOutput("Cs_table"),

                            tags$h4("Wait for the results and check the parameters of the SCAN analysis"),

                            tableOutput("parameters"),

                            uiOutput("names_scan_list"),  # input$dataset is here in serverUI

                            downloadButton("downloadData", "Download"),
                        )
                    ),
                        
                    fluidRow(
                        
                        tags$h4('SCAN results - download data preview'),

                        dataTableOutput('table_download_preview'),

                        tags$h4('Chorotypes'),

                        tableOutput("scan_chorotypes")
                    )
                ),
                
                tabItem("SCAN_viewer",
                        
                        fluidRow(
                            
                            box(width = 2,
                                
                                conditionalPanel( condition = "input.graph_from_csv == false",
                                      #selectInput(inputId = "g", label = "choose a SCAN graph:",  choices = ls()[ls() |> grep(pattern = "SCANlist")])
                                      tags$h5("Using the graph calculated. To upload nodes and edges .csv files check the box below")
                                ),
                                
                                checkboxInput("graph_from_csv", "Input .csv graph?", FALSE),
                                
                                conditionalPanel(     condition = "input.graph_from_csv == true",
                                                      
                                      fileInput(inputId = "graph_nodes", label = "graph NODES in csv file", accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                                      
                                      fileInput(inputId = "graph_edges", label = "graph EDGES in csv",  accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
                                ),
                                
                                numericInput(inputId = "threshold", label = "threshold between 0 and 1", value = 0.9, max = 1, min = 0.1),
                                
                                selectInput(inputId = "palette", label = "choose palette", choices = c("Dark2","Set1","Paired","Accent","Spectral","Greens", "Reds","BrBG" , "RdYlGn","PiYG")),
                                
                                selectInput("layout", "graph layout", choices = c("fr", "kk", "dh", "drl", "mds", "gem")),
                                
                                numericInput("map_alpha", "map alpha", max = 1, min = 0.01, value = 0.2),
                            ),
                            
                            box(width = 10,
                                
                                uiOutput("original_components"),
                                
                                box(    tags$h4("Map"),  leafletOutput("map_plot", height = "500px"), width = 8  ),
                                
                                box(    tags$h4("Graph"),  plotOutput("graph_plot"), width = 4   )
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


