
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
        
        map <- map |> group_by(sp) |> summarise()
        
        if (isTRUE(input$map_projection)) {map <- map |> st_transform(crs = input$map_projection)}
        
        map
    })
    
    overlapping <- reactive({ 
        
        sf_use_s2(FALSE)
        
        overlapping <- st_intersects(map(), map(), sparse = F) |> as_tibble() |> 
            
            setNames(map()$sp) |> mutate(sp1 = map()$sp) |> select(sp1, everything()) |> 
            
            pivot_longer(cols = !1, names_to = "sp2") |> filter(value) |> filter(sp1 != sp2) |> 
            
            filter(!duplicated(paste0(pmax(sp1, sp2), pmin(sp1, sp2))))
        
        overlapping
        
    })
    
    # output$overlapping_head <- renderTable(  overlapping |> as_tibble() |>  head()   )
    
    Cs_pre <- reactive( {
        
        if(is.null(input$calculate_Cs)){ return()}
        
        if(input$calculate_Cs == "upload_Cs.csv" & is.null(input$Cs_table)){return()}
        
        if(input$calculate_Cs == "calculate_Cs"){
            
            areas <- overlap_function(tab_species = overlapping(), map = map())

            areas <- areas |> mutate( area_sp1 = area_sp1 |> units::set_units("km^2"),

                            area_sp2 = area_sp2 |> units::set_units("km^2"),

                            area_overlap = area_overlap |> units::set_units("km^2")
            )

            Cs <- areas |> mutate(Cs = units::drop_units((area_overlap / area_sp1) * (area_overlap / area_sp2))) |> select(sp1,sp2, Cs)

            Cs
            
        } else {
        
        Cs_file <- input$Cs_table
        
        Cs <- read.csv(Cs_file$datapath, header = TRUE) 
        
        }
        
        Cs <- Cs |> arrange(desc(Cs)  )
        
    })
    
    Cs <- reactive({
        
        if(isTRUE(input$apply_filter_Cs)) { Cs <- Cs_pre() |> filter( Cs > (input$filter_Cs) ) } else { Cs <- Cs_pre() }
        
        Cs <- Cs |> select(sp1, sp2, Cs)
    })
    
    # if(isTRUE(input$apply_filter_Cs)) { Cs <- Cs() |> filter( Cs > input$filter_Cs ) } # better outside reactive main to avoid recalculation
    
    
    
    
    output$download_Cs <- downloadHandler(
        
        filename = function() { paste("Cs_table", ".csv", sep = "")  },
        
        content = function(file) {  write.csv(Cs(), file, row.names = FALSE)    }
    )
    
    
    
    
    
    output$Cs_head <- renderTable(    if(nrow(Cs()) > 0){    Cs() |> head()    } else { return()    }    )
    
    output$map_species <- renderTable(   species_map <-  map() |> st_drop_geometry() |> select(sp) |> summarise(species = paste(sp,collapse = ', '))  )
    
    output$map_shp_names <- renderText(  paste(map() |> names(), sep = ',')   )
    
    output$map_shp <- renderPlot(   map_shp <- if(nrow(map()) > 250) { 
        
                map()[1:200, ] |>  plot(col = sf.colors(categorical = TRUE, alpha = 0.5)) 
        
        } else {  map()[,] |> plot(col = sf.colors(categorical = TRUE, alpha = 0.5)) }   
    )
    
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
        
        filename = function() { paste(input$dataset, ".csv", sep = "")  },
        
        content = function(file) {  write.csv(datasetInput(), file, row.names = FALSE)    }
    )
    
    # viewer v1,1
    
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
    
    
    
   
        
    #source functions SCAN and fix to igraph's to_subgraph
    {
        
        overlap_function = function(tab_species = overlapping(), map = map()){
            
            sf_use_s2(FALSE)
            
            tab = tibble(sp1 = NA, sp2 = NA, area_sp1 = NA, area_sp2 = NA, 
                         area_overlap = NA, .rows = nrow(tab_species))
            
            for (i in 1:nrow(tab_species)){
                
                sp1 = tab_species$sp1[i]
                
                sp2 = tab_species$sp2[i]
                
                map1 = map |> filter(sp == sp1) |> st_geometry()
                
                map2 = map |> filter(sp == sp2) |> st_geometry()
                
                tab$area_overlap[i] = st_intersection(map1, map2) |> st_area() |> sum() # units::set_units(.,km^2)
                
                tab$sp1[i] = sp1
                
                tab$area_sp1[i] = st_area(map1)
                
                tab$sp2[i] = sp2
                
                tab$area_sp2[i] = st_area(map2)
            }
            tab
        }
        
        # fix igraph's 'to_subgraph' function (it had a bug :-(  )
        
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
                             filter_diameter = FALSE, filter_out_spp = c() ) 
        {
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
        
        map_by_component = function(graph = graph, threshold = threshold){
            
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
                
                unmorph()
        }
    }
        
    
    
})
