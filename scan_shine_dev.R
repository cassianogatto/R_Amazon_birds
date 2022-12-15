ui <- dashboardPage(
    
    dashboardHeader(title = "SCAN engine"),
    
    dashboardSidebar(
        
        sidebarMenu(
            
            menuItem("Map and Cs calculus", tabName = "map_Cs"),
            
            menuItem("SCAN analysis", tabName = "scan"),
            
            menuItem("about SCAN", tabName = "about_SCAN")
            
        )
    ),
    
    dashboardBody(
        
        tabItems(
            
            tabItem("map_Cs",  box("Hello World")  ),
            
            tabItem("scan",
                    
                    fluidRow(
                        
                        box( tags$h3("SCAN"),
                        
                            fileInput(
                                
                                inputId = "Cs_table", label = "Cs table in csv file", accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
                            ),
                            
                            numericInput(inputId = "resolution", label = "Select the resolution (interval between Ct)", value = 0.02),
                            
                            sliderInput(
                                inputId = "threshold_min_max", label = "Select the threshold range to be analyzed", 
                                value = c(0.84, 0.94), min = 0.2, max = 1, step = 0.05
                            ),
                            
                            numericInput(inputId = "max_diameter", label = "Choose maximum network diameter for a chorotype", value = 15),
                            
                            checkboxInput(inputId = "overlap", label = "Overlap between all species?", value = TRUE),
                            
                            actionButton("run", "SCAN!")
                        ),
                        # Input: Choose dataset ----
                        box(tags$h5("Check Cs data table columns (sp1, sp2, Cs)"),
                            
                            tableOutput("Cs_table"),
                            
                            tags$h4("wait for the results and check the parameters of the SCAN analysis"),
                            
                            tableOutput("parameters"),
                            
                            # selectInput("dataset", "Download SCAN results one by one",choices = c("chorotypes", "all_spp_summary", "all_spp", "graph_nodes", "graph_edges")),
                            
                            uiOutput("names_scan"),  # input$dataset is here in serverUI
                            
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

server <- function(input,output,session){
    
    options(shiny.maxRequestSize=100*1024^2) # this is required for uploading large datasets
    
    Cs <- reactive({ 
        Cs_file <- input$Cs_table
        Cs <- read.csv2(Cs_file$datapath, header = TRUE) 
        })
    
    graph <- reactive({
            graph <- Cs() |> 
                #mutate(Cs = round(Cs,2)) %>% 
                select(from = sp1, to = sp2, Cs) %>%
                as_tbl_graph(directed=FALSE)
    })
    
    SCANlist <- eventReactive(input$run,{
        
            SCANlist <- SCAN_lite(
                            graph = graph(),
                            max_Ct =  input$threshold_min_max[2],# 'automatic detection of max threshold in the Cs dataset': max(graph %>% activate(edges) %>% as_tibble %>% .$Cs),
                            min_Ct =  input$threshold_min_max[1],
                            Ct_resolution =  input$resolution * (-1),
                            max_diameter = input$max_diameter,
                            mark_overlap = input$overlap,
                            filter_overlap = input$overlap
         )
        
        SCANlist[['graph_nodes']] <- SCANlist[['graph']] |> activate(nodes) |> as_tibble()
            
        SCANlist[['graph_edges']] <- SCANlist[['graph']] |> activate(edges) |> as_tibble()
        
        SCANlist
    })
    
    
    #                                                     ___OUTPUTS___
    output$Cs_table <- renderTable({ Cs() |> head()  })
    
    # output$chorotypes <- renderDataTable({ SCANlist()[['chorotypes']] })
    # 
    # output$all_spp_summary <- renderTable({  SCANlist()[['all_spp_summary']]    })
    # 
    # output$all_spp <- renderDataTable({     SCANlist()[['all_spp']]    })
    # 
    # output$graph_nodes <- renderDataTable({     SCANlist()[['graph']] |> activate(nodes) |> as_tibble()    })
    # 
    # output$graph_edges <- renderDataTable({     SCANlist()[['graph']] |> ativate(edges) |> as_tibble()    })
    
    output$parameters <- renderTable({   SCANlist()[['parameters']]  })
    
    output$names_scan <- renderUI({
        
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
    # https://shiny.rstudio.com/articles/download.html
    # Table of selected dataset ---- 
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
    
# sources ----
    {    
    SCAN_lite = 
        function(graph = C,
                 max_Ct = max(graph %>% activate(edges) %>% as_tibble %>% .$Cs),
                 min_Ct = 0.76,
                 Ct_resolution = - 0.02,
                 max_diameter = 10,
                 mark_overlap = TRUE,
                 filter_overlap = FALSE,
                 filter_diameter = FALSE, 
                 filter_out_spp = c() 
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
            # if(isTRUE(filter_overlap) & isTRUE(filter_diameter)){
            #         
            #         filter = (Cs >= threshold & is.na(.N()$no_overlap[from]) & is.na(.N()$filter[from]))
            #         
            # } else {
            #         
            #         if(isTRUE(filter_overlap)){ filter = (Cs >= threshold & is.na(.N()$no_overlap[from]))
            #                 
            #         } else {
            #                 
            #                 if(isTRUE(filter_diameter)){ filter = (Cs >= threshold & is.na(.N()$filter[from]))
            #                 
            #                 } else{ filter  = Cs >= threshold
            #                 
            #  }       }        }
            # <- trying to make code shorter with a filter        
            
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
    
}

shinyApp(ui,server) 

