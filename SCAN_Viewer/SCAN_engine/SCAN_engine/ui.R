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
library(ggraph)
library(readr)
library(sf)
library(ggplot2)
library(leaflet)
library(rgdal)
extrafont::loadfonts(device="win")

#source functions SCAN and fix to igraph's to_subgraph
{
    library(dplyr)
    library(igraph)
    library(tidygraph)
    library(ggraph)
    library(readr)
    library(sf)
    library(ggplot2)
    library(leaflet)
    extrafont::loadfonts(device="win")
    
    # fix igraph's 'to_subgraph' function (it has a bug :-(  ) 
    to_subgraph <- function(graph, ..., subset_by = NULL, delete.vertices = TRUE) {
        if (is.null(subset_by)) {
            subset_by <- active(graph)
            message('SCAN network subsetting by ', subset_by, ' Ct = ', threshold)
        }
        ind <- as_tibble(graph, active = subset_by)
        ind <- mutate(ind, .tidygraph_index = seq_len(n()))
        ind <- filter(ind, ...)
        ind <- ind$.tidygraph_index
        subset <- switch(
            subset_by,
            nodes = induced_subgraph(graph, ind),
            edges = subgraph.edges(graph, ind, delete.vertices = delete.vertices)
        )
        list(subgraph = as_tbl_graph(subset))
    }
    
    SCAN_lite = function(graph = C, max_Ct = 1, min_Ct = 0.8, Ct_resolution = - 0.02,
                         max_diameter = 10, mark_overlap = TRUE, filter_overlap = FALSE,
                         filter_diameter = FALSE, filter_out_spp = c() 
    ) {
        #  setup
        chorotypes = list()
        g_spp_all = tibble()
        g_summary_all = tibble()
        if(isTRUE(filter_overlap)) {mark_overlap = TRUE}
        if(isTRUE(mark_overlap)) {graph = graph %>% activate(nodes) %>% mutate(no_overlap = NA)}
        if (Ct_resolution > 0){ Ct_resolution = Ct_resolution * (-1)}
        
        #### MAIN LOOP ----
        for(threshold in seq(max_Ct,min_Ct,Ct_resolution)){
            
            # any species to be filtered out? (1)
            if(length(filter_out_spp) != 0) {
                graph = graph %>% morph(to_subgraph, subset_by = "nodes",  name %in% filter_out_spp,
                                        remove_multiples = TRUE, delete.vertices= TRUE) %>% mutate(filter = 1) %>% 
                    unmorph() } 
            
            # get the communities (components) using criteria of Cs, overlap and diameter (2)
            graph = partial_components(graph = graph, threshold = threshold, filter_diameter = filter_diameter, 
                                       filter_overlap = filter_overlap)
            
            # get statistics by component (3)
            g = map_by_component(graph = graph, threshold = threshold)
            
            # filter by diameter (4)
            g = g %>% activate(nodes) %>%
                mutate("filter" = ifelse(get(paste0("diameter",threshold)) > max_diameter, threshold, NA))  
            
            g = g %>% activate(nodes) %>% mutate("betweenness{threshold}" := round(betweenness(g),1))
            
            # species' list
            g_spp = g %>% activate(nodes) %>% as_tibble() %>% 
                group_by(name, get(paste0("components",threshold)), get(paste0("diameter",threshold)),
                         get(paste0("order",threshold)), get(paste0("centrality",threshold))) %>% 
                summarize(Ct = threshold, betweenness = get(paste0("betweenness",threshold))) %>% 
                select(1,Ct, components = 2, diameter = 3, order = 4, centrality = 5, betweenness) %>% 
                arrange(name)
            
            # communities
            g_summary = g %>% activate(nodes) %>% as_tibble %>% group_by(get(paste0("components",threshold)),
                                                                         get(paste0("order",threshold))) %>% 
                summarize(Ct = threshold, chorotype_spp = paste(name, collapse = ", "), richness_spp = n(),
                          diameter = max(get(paste0("diameter",threshold))), 
                          max_centrality = max(get(paste0("centrality",threshold))),
                          max_betweenness = max(get(paste0("betweenness", threshold)))) %>% 
                select(component = 1, Ct, chorotype_spp, richness_spp, diameter, max_centrality, max_betweenness)
            
            # check overlap        
            if(isTRUE(mark_overlap)){
                
                g_spp = g_spp %>% mutate(no_overlap = NA)
                g_summary = g_summary %>% mutate(no_overlap = NA)
                
                are_connected = data.frame()
                for(comp in g_summary$component){
                    spp = g_summary %>% filter(component == comp) %>% pull(.,"chorotype_spp") %>% strsplit(.,", ") %>% .[[1]]
                    
                    for (sp1 in spp){
                        for(sp2 in spp[which(spp != sp1)]){
                            conn = tibble(species1 = sp1, species2 = sp2, 
                                          connected = igraph::are.connected(graph, sp1,sp2))
                            are_connected = rbind(are_connected, conn)
                        }       }        }
                
                # if all species in a component which sp1 belongs are connected -> TRUE
                connected_nodes_in_components =  are_connected %>% 
                    group_by(species1) %>% summarize(all_connected = ifelse(all(connected), TRUE, FALSE)) %>% 
                    left_join(g_spp, by = c("species1" = "name")) %>% select(component = 4, name = 1,2) %>% 
                    arrange(component, name)
                
                # identify and remove communities in which not all components are connected (overlapped)
                all_connected_components = connected_nodes_in_components %>% group_by(component) %>%
                    summarize(all_connected = ifelse(all(all_connected), TRUE, FALSE))
                
                not_connected_components = all_connected_components %>% filter(all_connected == FALSE) %>% pull(.,"component")
                spp_in_not_connected_components = g_spp %>% filter(components %in% not_connected_components) %>% 
                    pull(.,'name')
                
                
                if(length(not_connected_components) > 0 & isTRUE(mark_overlap)) {
                    
                    # write non-all-overlapped chorotypes an species 
                    g_spp = g_spp %>% mutate(no_overlap = replace(NA, name %in% spp_in_not_connected_components, threshold ) )
                    
                    g_summary = g_summary %>% mutate(no_overlap = replace(NA, component %in% not_connected_components, threshold))
                    
                    # write those non all-overlapped components to non-overlap column in graph
                    if(isTRUE(mark_overlap)) {
                        graph = graph %>% morph(to_subgraph, subset_by = "nodes", 
                                                # criteria to write non-overlap for the first (and only) time in graph
                                                is.na(no_overlap) &                                    # [4]
                                                    name %in% spp_in_not_connected_components,
                                                remove_multiples = TRUE, delete.vertices= TRUE) %>% 
                            mutate(no_overlap = threshold) %>% 
                            unmorph()
                    }
                }
            }
            
            # update tables
            
            g_spp_all = rbind(g_spp_all, g_spp)
            
            g_summary_all = rbind(g_summary_all, g_summary)
            
        }  # main loop ends
        
        # summarize results and return list of objects
        
        if(isTRUE(mark_overlap)){
            chorotypes[['chorotypes']] = g_summary_all %>% group_by(chorotype_spp, richness_spp, diameter) %>% 
                summarise(Ct_max = max(Ct), Ct_min = min(Ct), max_centrality = max(max_centrality),
                          max_betweenness = max(max_betweenness), no_overlap = max(no_overlap)) %>% 
                arrange(chorotype_spp, desc(Ct_max))
            
            chorotypes[['all_spp_summary']] = g_spp_all %>% group_by(name, components, order) %>% 
                summarise(max_Ct = max(Ct),
                          min_Ct = min(Ct), max_diam = max(diameter), min_diam = min(diameter),
                          max_between = max(betweenness), no_overlap = max(no_overlap))
            
        } else {
            chorotypes[['chorotypes']] = g_summary_all %>% group_by(chorotype_spp, richness_spp, diameter) %>% 
                summarise(Ct_max = max(Ct), Ct_min = min(Ct), max_centrality = max(max_centrality),
                          max_betweenness = max(max_betweenness)) %>% 
                arrange(chorotype_spp, desc(Ct_max))
            
            chorotypes[['all_spp_summary']] = g_spp_all %>% group_by(name, components, order) %>% summarise(max_Ct = max(Ct),
                                                                                                            min_Ct = min(Ct), max_diam = max(diameter), min_diam = min(diameter),
                                                                                                            max_between = max(betweenness))
        }
        
        chorotypes[['all_spp']] = g_spp_all
        
        chorotypes[['graph']] = graph
        
        chorotypes[["parameters"]] = tibble(max_diameter = max_diameter, max_Ct = max_Ct, min_Ct = min_Ct, 
                                            Ct_resolution = Ct_resolution, mark_overlap = mark_overlap,
                                            filter_overlap = filter_overlap, filter_diameter = filter_diameter, 
                                            filter_out_spp = filter_out_spp)
        
        return(chorotypes)
        
        
    }
    
    partial_components = function (graph = graph, threshold = threshold, filter_diameter = FALSE, 
                                   filter_depth = FALSE, filter_overlap = FALSE, ...){
        print("using 'igraph::group_components' - see more options of community structurig in '?group_components'")
        
        if(isTRUE(filter_overlap) & isTRUE(filter_diameter)) { graph %>% morph(to_subgraph, subset_by = "edges", 
                                                                               (Cs >= threshold & is.na(.N()$no_overlap[from]) & is.na(.N()$filter[from])), # check the node respective to the 'from' edge table
                                                                               remove_multiples = TRUE, delete.vertices= TRUE) %>% 
                activate(edges) %>% mutate("Ct{threshold}" := TRUE) %>% 
                activate(nodes) %>% mutate("Ct{threshold}" := TRUE) %>%
                mutate("components{threshold}" := group_components("weak")) %>% # identify connected elements in COMPONENTS (simpler commuity definition)
                unmorph()
        } else {
            if(isTRUE(filter_overlap)) {
                graph %>% morph(to_subgraph, subset_by = "edges", 
                                (Cs >= threshold & is.na(.N()$no_overlap[from])), 
                                remove_multiples = TRUE, delete.vertices= TRUE) %>% 
                    activate(edges) %>% mutate("Ct{threshold}" := TRUE) %>% 
                    activate(nodes) %>% mutate("Ct{threshold}" := TRUE) %>%
                    mutate("components{threshold}" := group_components("weak")) %>% 
                    unmorph()
            } else {
                
                if(isTRUE(filter_diameter)) {
                    graph %>% morph(to_subgraph, subset_by = "edges", 
                                    (Cs >= threshold & is.na(.N()$filter[from])), 
                                    remove_multiples = TRUE, delete.vertices= TRUE) %>% 
                        activate(edges) %>% mutate("Ct{threshold}" := TRUE) %>% 
                        activate(nodes) %>% mutate("Ct{threshold}" := TRUE) %>%
                        mutate("components{threshold}" := group_components("weak")) %>% # identify connected elements
                        unmorph()
                    
                } else{
                    graph %>% morph(to_subgraph, subset_by = "edges", Cs >= threshold, 
                                    remove_multiples = TRUE, delete.vertices= TRUE) %>% 
                        activate(edges) %>% mutate("Ct{threshold}" := TRUE) %>% 
                        activate(nodes) %>% mutate("Ct{threshold}" := TRUE) %>%
                        mutate("components{threshold}" := group_components("weak")) %>% 
                        unmorph()
                }       }       }       }
    
    map_by_component = function(graph = graph, threshold = threshold){ #, filter_diameter = filter_diameter
        
        # 'I'll suspend this filtering here by now'
        # if(filter_depth) {graph = graph %>% activate(nodes) %>% filter(is.na(depth_filter))} 
        
        graph %>% activate(edges) %>% filter(!is.na(get(paste0("Ct",threshold)))) %>% 
            activate(nodes) %>% filter(!is.na(get(paste0("Ct",threshold)))) %>% 
            # split by components
            morph(to_split, group_by = get(paste0("components",threshold)), subset_by = "nodes") %>% 
            # diameter = depth in SCAN
            mutate("diameter{threshold}" := graph_diameter(unconnected = TRUE),
                   # order = richness of species
                   "order{threshold}" := graph_order(),
                   # centrality by component
                   "centrality{threshold}" := centrality_degree()) %>% 
            # # betweenness of each node - (cannot calculate betweenness like the above parameters... don't know why...)
            # mutate("betweenness" = betweenness()) %>% 
            
            unmorph()
    }
    
}

# Define UI for application that draws a histogram
shinyUI(
    
    # theme = bslib::bs_theme(bootswatch = "darkly"),
    
    dashboardPage(
        
        dashboardHeader(title = "SCAN engine"),
        
        dashboardSidebar( width = '200px',
                          
                          sidebarMenu(
                              
                              menuItem("Species distribution maps", tabName = "map_shp"),
                              
                              menuItem("Cs calculus", tabName = "Cs_calculus"),
                              
                              menuItem("SCAN analysis", tabName = "scan"),
                              
                              menuItem("SCAN_viewer_v1.1", tabName = "SCAN_viewer"),
                              
                              menuItem("about SCAN", tabName = "about_SCAN")
                              
                          )
        ),
        
        dashboardBody(
            
            tabItems(
                
                tabItem("map_shp",
                        
                    fluidRow(
                        
                        inputPanel(width = 4,
                        
                            fileInput(inputId = "filemap", label = "Pls choose a shapefile (.shp +  .shx + .dbl + .prj) with all species identified in the first column",  
                                      accept = c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), multiple=TRUE),
                            
                            numericInput(inputId = "map_projection", label = "Choose a crs code to project the map (WGS84: 4326; SIRGAS2000: 4674, etc)", value = 4326), # sirgas 4674, sad69, etc in filechoice
                            
                            actionButton("get_map", "get species and sample map!")
                        ),
                        
                        box( tags$h4("column names and species in the maps"), 
                                     
                                textOutput("map_shp_names"),
                                     
                                tableOutput("map_species") 
                        ),
                        
                        box(width = 8,  tags$h4("Map sample"), plotOutput("map_shp")  )
                    ),
                          
                ),
                
                tabItem(
                    
                    fluidRow(
                        
                        box(
                            
                            
                        )
                    )
                ),
                
                tabItem("scan",
                        
                        fluidRow(
                            
                            box( tags$h3("SCAN"),
                                 
                                 fileInput( inputId = "Cs_table", label = "Cs table in csv file", accept = c("text/csv","text/comma-separated-values,text/plain",".csv" ) ),
                                 
                                 # checkboxInput(inputId = "use_semicolon_delimited_csv", label = "use semicolon delimited csv", value = TRUE),
                                 
                                 numericInput(inputId = "resolution", label = "Select the resolution (interval between Ct)", value = 0.02),
                                 
                                 sliderInput( inputId = "threshold_min_max", label = "Select the threshold range to SCAN", 
                                              
                                              value = c(0.8, 1), min = 0.2, max = 1, step = 0.01 ),
                                 
                                 numericInput(inputId = "max_diameter", label = "Choose maximum network diameter for a chorotype", value = 15),
                                 
                                 checkboxInput(inputId = "overlap", label = "Overlap between all species?", value = TRUE),
                                 
                                 actionButton("run", "SCAN!")
                                 
                            ),
                            # Input: Choose dataset ----
                            box(tags$h5("Check if Cs data table has the columns sp1, sp2, and the numeric Cs"),
                                
                                tableOutput("Cs_table"),
                                
                                tags$h4("Wait for the results and check the parameters of the SCAN analysis"),
                                
                                tableOutput("parameters"),
                                
                                # selectInput("dataset", "Download SCAN results one by one",choices = c("chorotypes", "all_spp_summary", "all_spp", "graph_nodes", "graph_edges")),
                                
                                uiOutput("names_scan_list"),  # input$dataset is here in serverUI
                                
                                # Button
                                downloadButton("downloadData", "Download"),
                                
                            ),
                            
                            # Main panel for displaying outputs ----
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
                                
                                # conditionalPanel(     condition = "input.graph_or_csv == false",
                                #                       selectInput(inputId = "g", label = "choose a SCAN list with a graph:",
                                #                                   choices = ls()[grep( ls(), pattern = "SCANli")] %>% sort(decreasing =  T))
                                # ),
                                
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
                                
                                numericInput(inputId = "threshold", label = "threshold between 0 and 1", value = 0.91, max = 1, min = 0.1),
                                
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
