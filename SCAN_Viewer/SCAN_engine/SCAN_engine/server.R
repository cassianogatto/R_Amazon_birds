library(shiny)
library(shinydashboard)
library(dplyr)
library(igraph)
library(tidygraph)
library(sf)
library(leaflet)
library(ggplot2)
library(ggraph)

# scan source
source("SCAN_lite_source.R")

shinyServer(function(input,output,session){
    
    options(shiny.maxRequestSize=1000*1024^2) # this is required for uploading large datasets
    
    # https://community.rstudio.com/t/shinyfiles-and-shapefiles/89099/4
    map <- eventReactive(input$get_map,{
        
        shpdf <- input$filemap
        
        if(is.null(shpdf)){    return()    }
        
        previouswd <- getwd()
        
        uploaddirectory <- dirname(shpdf$datapath[1])
        
        setwd(uploaddirectory)
        
        for(i in 1:nrow(shpdf)){   file.rename(shpdf$datapath[i], shpdf$name[i])    }
        
        setwd(previouswd)
        
        map <- readOGR(paste(uploaddirectory, shpdf$name[grep(pattern="*.shp$", shpdf$name)], sep="/"))#,  delete_null_obj=TRUE)
        # map <- spTransform(map, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
        map <- map |> st_as_sf(.) |> select(sp = 1, geometry)
        
        # map <- map |> st_transform(crs = input$map_projection)
    })
    
    output$map_species <- renderTable(   species_map <-  map() |> st_drop_geometry() |> select(sp) |> summarise(species = paste(sp,collapse = ', '))  )
    
    output$map_shp_names <- renderText( map() |> names())
    
    output$map_shp <- renderPlot(   map_shp <- if(nrow(map()) > 10) {   map()[1:10, "geometry"] |> plot() } else { map()[,"geometry"] |> plot() }   )
    
    
    Cs <- reactive({ 
        
        Cs_file <- input$Cs_table
        
        Cs <- read.csv(Cs_file$datapath, header = TRUE) 
        
    })
    
    graph <- reactive({    graph <- Cs() |>   select(from = sp1, to = sp2, Cs)   |>    as_tbl_graph( directed = FALSE )    })
    
    SCANlist <- eventReactive(input$run,{
        
        SCANlist <- SCAN_lite(
            graph = graph(),
            max_Ct =  input$threshold_min_max[2],
            min_Ct =  input$threshold_min_max[1],
            Ct_resolution =  input$resolution,
            max_diameter = input$max_diameter,
            mark_overlap = input$overlap,
            filter_overlap = input$overlap
        )
        
        SCANlist[['graph_nodes']] <- SCANlist[['graph']] |> activate(nodes) |> as_tibble()
        
        SCANlist[['graph_edges']] <- SCANlist[['graph']] |> activate(edges) |> as_tibble()
        
        SCANlist
    })
    
    #   ___OUTPUTS___
    
    output$Cs_table <- renderTable({ Cs() |> head()  })
    
    output$parameters <- renderTable({   SCANlist()[['parameters']]  })
    
    # select download
    output$names_scan_list <- renderUI({
        
        names <- names(SCANlist())
        
        selectInput(inputId = "dataset", label = "Choose a dataset to download (preview belo)", 
                    choices = names[names != "graph"] )
    })
    
    # Reactive value for selected dataset ----
    datasetInput <- reactive({
        switch(input$dataset,
               "chorotypes" = SCANlist()[['chorotypes']],
               "all_spp_summary" = SCANlist()[['all_spp_summary']],
               "all_spp" = SCANlist()[['all_spp']],
               # "graph" = cat('choose nodes or edges to save a csv'),
               "graph_nodes" = SCANlist()[['graph_nodes']],
               "graph_edges" = SCANlist()[['graph_edges']]
        )
    })
    
    # Table of selected dataset ---- # https://shiny.rstudio.com/articles/download.html
    output$table <- renderDataTable({
        ifelse(input$dataset == 'graph_edges', output <- datasetInput() |> head(), output <- datasetInput()  )
        
        output
    })
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(input$dataset, ".csv", sep = "")
        },
        content = function(file) {
            write.csv(datasetInput(), file, row.names = FALSE)
        }
    )
    
    # viewer v1,1
    
    #                                                     ___REACTIVE__
    threshold <- reactive({   input$threshold   })
    
    g_full <- reactive({    
        
        if(!isTRUE(input$graph_or_csv)){
            # get an object from environment
            # get(input$g) %>% .[['graph']] %>% activate(edges) %>% select(from, to, Cs) %>%  filter(Cs >= threshold()) %>% 
            #     activate(nodes) %>% filter(!is.na(get(paste0("components",threshold())))) %>% 
            #     select(name, comps = paste0('components',threshold())) %>% arrange(comps, name)
            
            # here trying to use switch to choose among possible objects - did not work
            # switch(input$graph, "SCANlist" = SCANlist()[['graph']])
            # input$graph
            
            g_full <- SCANlist()[['graph']]
            
        } else {
            
            node_file <- input$graph_nodes
            nodes <- read.csv(node_file$datapath, header = TRUE)
            
            edge_file <- input$graph_edges
            edges <- read.csv(edge_file$datapath, header = TRUE)
            
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
    
    g_map <- reactive({
        
        g_spp <- g_sub() |> activate(nodes) |> as_tibble()
        
        g_map1 <- right_join( map(), g_spp, by = c('sp' = 'name')) %>% select(comps, everything())
        
        # g_map1 |> st_transform(crs = 4326)
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
        
        # labels <- sprintf("%s %s", g_map$comps, g_map$sp) %>% lapply(htmltools::HTML)   # %s use the first 'string'
        
        # leaflet
        g_map() |> leaflet() |> addTiles() |>
            addPolygons(
                weight = 1,
                fillColor = ~ pal()(comps),
                color = "black", dashArray = "1",
                fillOpacity = input$map_alpha
                # highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
            ) # %>% leaflet::addLegend(  pal = pal, values = ~comps,  opacity = 0.7, title = "Chorotypes" )
    })
    
    output$ggplot_map <- renderPlot({
        ggplot(data = g_map() %>% arrange(comps)) + 
            geom_sf( aes(fill = comps), # THIS IFELSE STAT. TURNS FILL TO CONTINUOUS... use distiller, otherwise scale_fill_brewer to discrete palette
                     alpha = input$map_alpha, color = 'black', show.legend = F) + 
            # geom_sf(data = sa, fill = NA, color = 'black') +
            scale_fill_distiller( direction = 1, palette =   input$palette, na.value = "transparent", aesthetics = "fill") + #start = 0.2, end = 0.8, #Diverging  BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral
            # scale_fill_continuous(values = palette(g_map$comps)) + 
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
        
    })
    
})
