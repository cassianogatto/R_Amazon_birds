ui <- dashboardPage(
    
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
                        
                        box(    tags$h4("Map"), leafletOutput("map_plot", height = "500px"), width = 8  ),
                        
                        box(    tags$h4("Graph"),plotOutput("graph_plot"), width = 4   ),
                    ),
                    
                    fluidRow(     dataTableOutput("g_sub_table")    )
            ),
            
            tabItem("map_ggplot",
                
                fluidRow(
                    
                    box(   tags$h4("Map"), plotOutput("ggplot_map"), width = 7  ),
                    
                    box(    tags$h4("Graph"),plotOutput("graph_plot2"), width = 5   ),
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

server <- function(input,output,session){
    
    options(shiny.maxRequestSize=100*1024^2) 
    
    #                                                     ___REACTIVE__
    threshold <- reactive({   input$threshold   })
    
    g_full <- reactive({    
        
        if(!isTRUE(input$graph_or_csv)){
                
            get(input$g) %>% .[['graph']] %>% activate(edges) %>% select(from, to, Cs) %>%  filter(Cs >= threshold()) %>% 
                    activate(nodes) %>% filter(!is.na(get(paste0("components",threshold())))) %>% 
                    select(name, comps = paste0('components',threshold())) %>% arrange(comps, name)
            
            } else {
                
                node_file <- input$graph_nodes
                nodes <- read.csv2(node_file$datapath, header = TRUE)
                
                edge_file <- input$graph_edges
                edges <- read.csv2(edge_file$datapath, header = TRUE)
                
                g <- tbl_graph(nodes = nodes, edges = edges, directed = F)
                
                g <- g %>% 
                    activate(edges) %>% select(from, to, Cs) %>%  filter(Cs >= threshold()) %>% 
                    activate(nodes) %>% filter(!is.na(get(paste0("components",threshold())))) %>% 
                    select(name, comps = paste0('components',threshold())) %>% arrange(comps, name)
                
                g
                    
                }
    })
    
    original_components <- reactive({
        
        g_full() %>% activate(nodes) %>%  select(comps) %>%
            arrange(comps) %>% pull() %>% unique()
    })
    
    g_sub <- reactive({
        
        g_sub <- g_full() %>% activate(nodes) %>%
            filter(comps %in% input$selected_components)
        
        g_sub
    })
    
    g_map_4326 <- reactive({
        
        g_spp <- g_sub() |> activate(nodes) |> as_tibble()
        
        g_map1 <- right_join( map, g_spp, by = c('sp' = 'name')) %>% select(comps, everything())
        
        # create leaflet
        
        g_map_4326 <- g_map1 |> st_transform(crs = 4326)
    })
    
    pal <- reactive({ colorFactor(  palette = input$palette, domain = original_components()) })
    
    output$original_components <- renderUI({
        
        components<- original_components()
        
        checkboxGroupInput("selected_components", paste("Choose the chorotypes at Ct =", threshold() ) , components, inline = TRUE, selected = NULL) #ifelse(input$select_all_components == TRUE,components, NULL )) #components ) #try ifelse later  ifelse(input$select_all_components == 1, components, NULL)  to select all - but did not work
    })
    
    output$g_sub_table <- renderDataTable({  
        
        g_sub() |> activate(nodes) |> as_tibble() |> 
            group_by(comps) |> summarise(n_spp = n(), species = paste0(name, collapse = ','))
    })
    
    #                                                     ______MAP____
    output$map_plot <- renderLeaflet({
        
        # pal <- colorFactor(input$palette, domain = original_components() )#colorBin(input$palette, domain = original_components() ) #input$selected_components)# colorNumeric # pal <- colorBin(input$palette, domain = original_components(), bins = 7)
        
        # labels <- sprintf("%s %s", g_map_4326$comps, g_map_4326$sp) %>% lapply(htmltools::HTML)   # %s use the first 'string'
        
        # leaflet
        g_map_4326() |> leaflet() |> addTiles() |>
            addPolygons(
                weight = 1,
                fillColor = ~ pal()(comps),
                color = "black", dashArray = "1",
                fillOpacity = input$map_alpha
                # highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
            ) # %>% leaflet::addLegend(  pal = pal, values = ~comps,  opacity = 0.7, title = "Chorotypes" )
    })
    
    output$ggplot_map <- renderPlot({
        ggplot(data = g_map_4326() %>% arrange(comps)) + 
            geom_sf( aes(fill = comps), # THIS IFELSE STAT. TURNS FILL TO CONTINUOUS... use distiller, otherwise scale_fill_brewer to discrete palette
                     alpha = input$map_alpha, color = 'black', show.legend = F) + 
            # geom_sf(data = sa, fill = NA, color = 'black') +
            scale_fill_distiller( direction = 1, palette =   input$palette, na.value = "transparent", aesthetics = "fill") + #start = 0.2, end = 0.8, #Diverging  BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral
            # scale_fill_continuous(values = palette(g_map_4326$comps)) + 
            ggtitle(paste0("Ct = ", threshold()), sub = paste('components:', input$selected_components)) + # pull() %>% unique())) + 
            theme_minimal() 
    })
    
    output$graph_plot <- renderPlot({
        
        lay <- create_layout(g_sub(), layout = input$layout)      # lou <- cluster_louvain(g_sub) hmmm there are other options to graph building...
        ggraph(lay) +
            geom_edge_link(aes(alpha = (Cs+0.75)) , width = 1.25 , show.legend = FALSE) +
            geom_node_point(aes(fill = comps), # how to synchronize with palette used in MAP? ~pal ??
                            size =  (degree(g_sub(), mode="all") + 20) / 4, shape = 21, show.legend = FALSE) +
            scale_fill_distiller( direction = 1, palette = input$palette, na.value = "transparent", aesthetics = "fill") +
            # scale_fill_manual(values = pal(as.factor(comps))) +
            # scale_fill_brewer (values = palette()(comps) ) +
            geom_node_text(aes(label = name), size = 4, col = "black", repel=TRUE) +
            labs( subtitle = paste0("Ct = ", threshold() )) +
            theme_graph()
        
        # tring to implementate hulls showing groups... not succesfull
        # basic_graph2 <-  basic_graph1 + geom_mark_hull(aes(x, y, group = comps), label = comps, label.fontsize = 15, fill = "transparent", lty = "dotted", concavity = 1, expand = unit(3, "mm"), alpha = 0.05) + theme(legend.position = "none")
    })
    
    output$graph_plot2 <- renderPlot({
        
        lay <- create_layout(g_sub(), layout = input$layout)      # lou <- cluster_louvain(g_sub) hmmm there are other options to graph building...
        
        #
        #basic_graph <- 
        
        ggraph(lay) +
            
            geom_edge_link(aes(alpha = (Cs+0.75)) , width = 1.25 , show.legend = FALSE) +
            
            geom_node_point(aes(fill = comps), # how to synchronize with palette used in MAP? ~pal ??
                            size =  (degree(g_sub(), mode="all") + 10) / 4, shape = 21, show.legend = FALSE) +
            
            scale_fill_distiller( direction = 1, palette = input$palette, na.value = "transparent", aesthetics = "fill") +
            # scale_fill_manual(values = pal(as.numeric(comps))) +
            # scale_fill_continuous(palette = input$palette, na.value = "transparent", values = pal()(comps)  ) +
            geom_node_text(aes(label = name), size = 4, col = "black", repel=TRUE) +
            
            labs( subtitle = paste0("Ct = ", threshold() )) +
            
            theme_graph()
        
        # tring to implementate hulls showing groups... not succesfull
        # basic_graph2 <-  basic_graph1 + geom_mark_hull(aes(x, y, group = comps), label = comps, label.fontsize = 15, fill = "transparent", lty = "dotted", concavity = 1, expand = unit(3, "mm"), alpha = 0.05) + theme(legend.position = "none")
    })
    
}

shinyApp(ui,server) 

# can't match the color scales between  leaflet and ggplot - the leafleat palette is ok but can't transpose it to ggplot mapping of colors
