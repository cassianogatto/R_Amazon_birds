#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram

dashboardPage(
    
    dashboardHeader(title = "SCAN Viewer v1"),
    
    dashboardSidebar(
        
        sidebarMenu(
            
            conditionalPanel(     condition = "input.graph_or_csv == false",
                                  selectInput(inputId = "g", label = "choose a SCAN list with a graph:",  
                                              choices = ls()[grep( ls(), pattern = "bird")] %>% sort(decreasing =  T))
            ),
            
            checkboxInput("graph_or_csv", "Input csv graph?", FALSE),
            
            conditionalPanel(     condition = "input.graph_or_csv == true",
                                  
                                  fileInput(inputId = "graph_nodes", label = "graph NODES in csv file", accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                                  
                                  fileInput(inputId = "graph_edges", label = "graph EDGES in csv",  accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
            ),
            
            sliderInput(inputId = "threshold", label = "threshold", value = 0.91, min = 0.21, max = 0.97, step = 0.02),
            
            selectInput(inputId = "palette", label = "choose palette",choices = c("Dark2","Set1","Paired","Accent","Spectral","Greens", "Reds","BrBG" , "RdYlGn","PiYG")),
            
            selectInput("layout", "graph layout", choices = c("fr", "kk", "dh", "drl", "mds", "gem")),
            
            numericInput("map_alpha", "map alpha", max = 1, min = 0.01, value = 0.2),
            
            menuItem("Map & Graph", tabName = "map_graph"),
            
            menuItem("Map", tabName = "map_ggplot"),
            
            menuItem("about SCAN", tabName = "about_SCAN")
            
        )
    ),
    
    dashboardBody(
        
        tabItems(
            
            tabItem("map_graph",
                    
                    fluidRow(
                        
                        uiOutput("original_components"),
                        
                        box(    tags$h4("Map"), leafletOutput("map_plot", height = "700px"), width = 8  ),
                        
                        box(    tags$h4("Graph"),plotOutput("graph_plot"), width = 4   ),
                    ),
                    
                    fluidRow(     dataTableOutput("g_sub_table")    )
            ),
            
            tabItem("map_ggplot",
                    
                    fluidRow(
                        
                        box(   tags$h4("Map"), plotOutput("ggplot_map"), width = 8  ),
                        
                        box(    tags$h4("Graph"),plotOutput("graph_plot2"), width = 4   ),
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
        
    )   )
