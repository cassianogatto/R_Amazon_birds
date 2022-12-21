#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

options(shiny.maxRequestSize=100*1024^2) 

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
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
    
    #                                                     ____OUTPUTS__
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
        
        pal <- colorFactor(input$palette, domain = original_components() )#colorBin(input$palette, domain = original_components() ) #input$selected_components)# colorNumeric # pal <- colorBin(input$palette, domain = original_components(), bins = 7)
        
        # labels <- sprintf("%s %s", g_map_4326$comps, g_map_4326$sp) %>% lapply(htmltools::HTML)   # %s use the first 'string'
        
        # leaflet
        g_map_4326() |> leaflet() |> addTiles() |>
            
            addPolygons(
                weight = 1,
                fillColor = ~pal(comps),
                color = "black", dashArray = "1",
                fillOpacity = input$map_alpha
                # highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
            ) # %>% leaflet::addLegend(  pal = pal, values = ~comps,  opacity = 0.7, title = "Chorotypes" )
    })
    
    output$ggplot_map <- renderPlot({
        
        ggplot(data = g_map_4326() %>% arrange(comps)) + 
            
            geom_sf( aes(fill = as.factor(comps)), # THIS IFELSE STAT. TURNS FILL TO CONTINUOUS... use distiller, otherwise scale_fill_brewer to discrete palette
                     alpha = input$map_alpha, color = 'black', show.legend = F) + 
            
            # geom_sf(data = sa, fill = NA, color = 'black') +
            
            scale_fill_brewer( direction = 1, palette =   input$palette, na.value = "transparent", aesthetics = "fill") + #start = 0.2, end = 0.8, #Diverging  BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral
            
            ggtitle(paste0("Ct = ", threshold()), sub = paste('components:', input$selected_components)) + # pull() %>% unique())) + 
            
            theme_minimal() 
    })
    
    
    #                                                     ___GRAPH_____
    output$graph_plot <- renderPlot({
        
        lay <- create_layout(g_sub(), layout = input$layout)      # lou <- cluster_louvain(g_sub) hmmm there are other options to graph building...
        
        #
        #basic_graph <- 
        
        ggraph(lay) +
            
            geom_edge_link(aes(alpha = (Cs+0.75)) , width = 1.25 , show.legend = FALSE) +
            
            geom_node_point(aes(fill = comps), # how to synchronize with palette used in MAP? ~pal ??
                            size =  (degree(g_sub(), mode="all") + 10) / 4, shape = 21, show.legend = FALSE) +
            
            # this scale is not working, also...
            scale_fill_distiller( direction = 1, palette = input$palette, na.value = "transparent", aesthetics = "fill") +
            # scale_fill_manual(values = pal(as.numeric(comps))) +
            
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
            
            geom_node_point(aes(fill = as.factor(comps)), # how to synchronize with palette used in MAP? ~pal ??
                            size =  (degree(g_sub(), mode="all") + 10) / 4, shape = 21, show.legend = FALSE) +
            
            # this scale is not working, also...
            scale_fill_brewer( direction = 1, palette = input$palette, na.value = "transparent", aesthetics = "fill") +
            # scale_fill_manual(values = pal(as.numeric(comps))) +
            
            geom_node_text(aes(label = name), size = 4, col = "black", repel=TRUE) +
            
            labs( subtitle = paste0("Ct = ", threshold() )) +
            
            theme_graph()
        
        # tring to implementate hulls showing groups... not succesfull
        # basic_graph2 <-  basic_graph1 + geom_mark_hull(aes(x, y, group = comps), label = comps, label.fontsize = 15, fill = "transparent", lty = "dotted", concavity = 1, expand = unit(3, "mm"), alpha = 0.05) + theme(legend.position = "none")
    })
    
})
