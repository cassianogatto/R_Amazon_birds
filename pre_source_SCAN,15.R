
partial_components.15 = function (graph = graph, threshold = threshold, filter_diameter = FALSE, 
                                        filter_depth = FALSE, filter_overlap = FALSE, ...){
                
                if(isTRUE(filter_overlap) & isTRUE(filter_diameter)) { 
                                graph %>% morph(to_subgraph, subset_by = "edges", 
                                       (Cs >= threshold & is.na(.N()$no_overlap[from]) & is.na(.N()$filter[from])), # check the node respective to the 'from' edge table
                                       remove_multiples = TRUE, delete.vertices= TRUE) %>% 
                                mutate("components{threshold}" := group_components("weak")) %>% # identify connected elements in COMPONENTS (simpler commuity definition)
                                unmorph()
                } else {
                        if(isTRUE(filter_overlap)) {
                                graph %>% morph(to_subgraph, subset_by = "edges", 
                                                (Cs >= threshold & is.na(.N()$no_overlap[from])), 
                                                remove_multiples = TRUE, delete.vertices= TRUE) %>% 
                                        mutate("components{threshold}" := group_components("weak")) %>% 
                                        unmorph()
                        } else {
                                
                                if(isTRUE(filter_diameter)) {
                                        graph %>% morph(to_subgraph, subset_by = "edges", 
                                                        (Cs >= threshold & is.na(.N()$filter[from])), 
                                                        remove_multiples = TRUE, delete.vertices= TRUE) %>% 
                                                mutate("components{threshold}" := group_components("weak")) %>% # identify connected elements
                                                unmorph()
                                } else{
                                        graph %>% morph(to_subgraph, subset_by = "edges", Cs >= threshold, 
                                                        remove_multiples = TRUE, delete.vertices= TRUE) %>% 
                                                mutate("components{threshold}" := group_components("weak")) %>% 
                                                unmorph()
                                }       }       }       }

map_by_component.15 = function(graph = graph, threshold = threshold){
        
        graph %>% activate(nodes) %>% filter(!is.na(get(paste0("components",threshold)))) %>% 
                # split by components
                morph(to_split, group_by = get(paste0("components",threshold)), subset_by = "nodes") %>% 
                        mutate("diameter{threshold}" := graph_diameter(unconnected = TRUE),
                               "order{threshold}" := graph_order(),
                               "centrality{threshold}" := centrality_degree()) %>% 
                unmorph() }

SCAN_lite.15 = function(graph = C, max_Ct = max(graph %>% activate(edges) %>% as_tibble %>% .$Cs),
                 min_Ct = 0.75, Ct_resolution = -0.02, max_diameter = 10,
                 mark_overlap = TRUE, filter_overlap = TRUE, filter_diameter = TRUE, 
                 filter_out_spp = c()) {
                
        # in SCAN::partial_components using 'igraph::group_components' - see more options of community structurig in '?group_components'
        #  setup
                chorotypes = list()
                g_spp_all = tibble()
                g_summary_all = tibble()
                if(isTRUE(filter_overlap)) {mark_overlap = TRUE}
                if(isTRUE(mark_overlap)) {graph = graph %>% activate(nodes) %>% mutate(no_overlap = NA)}
                if(isTRUE(filter_diameter)) {graph = graph %>% activate(nodes) %>% mutate(filter = NA)}
                # species to be filtered out from the main table (there will be no data about these species in this SCAN round)
                if(length(filter_out_spp) != 0) { graph = graph %>% morph(to_subgraph, subset_by = "nodes",  name %in% filter_out_spp,
                                        remove_multiples = TRUE, delete.vertices= TRUE) %>% mutate(filter = 1) %>% 
                                unmorph() } 
                
        #### MAIN LOOP ####
        for(threshold in seq(max_Ct,min_Ct,Ct_resolution)){
                        
                print(paste("Ct =", threshold) )
                # get the communities (components) using criteria of Cs, overlap and diameter (2)
                graph = partial_components.15(graph = graph, threshold = threshold, filter_diameter = filter_diameter, 
                           filter_overlap = filter_overlap)
                # get statistics by component (3)
                g = map_by_component.15(graph = graph, threshold = threshold)
                
                # MAX DIAMETER FILTER spp(4)
                if(any(g %N>% select(filter) %>% pull %>% is.na(.))){
                        
                        over_diam_spp = g %N>% as_tibble %>% filter( get(paste0('diameter',threshold)) > max_diameter) %>% 
                                select(name, contains("diameter"))
                       # A PARTIR AQUI gPODE SER UM TIBBLE
                         #filter in g
                        g = g %>% activate(nodes) %>% 
                                mutate(filter = ifelse(get(paste0("diameter",threshold)) > max_diameter, threshold, NA))
                }
                        
                # betweenness calculus
                g = g %>% activate(nodes) %>% mutate("betweenness{threshold}" := betweenness(g) %>% round(.,1))
                
                # species' list
                g_spp = g %>% activate(nodes) %>% as_tibble() %>% 
                        group_by(name, get(paste0("components",threshold)), get(paste0("diameter",threshold)),
                                 get(paste0("order",threshold)), get(paste0("centrality",threshold))) %>% 
                        summarize(Ct = threshold, betweenness = get(paste0("betweenness",threshold))) %>% 
                        select(1,Ct, components = 2, diameter = 3, order = 4, centrality = 5, betweenness) %>% 
                        arrange(name)
                
                # communities
                g_summary = g %>% activate(nodes) %>% as_tibble %>% group_by(get(paste0("components",threshold)),  get(paste0("order",threshold))) %>% 
                        summarize(Ct = threshold, chorotype_spp = paste(name, collapse = ", "), richness_spp = n(),
                                  diameter = max(get(paste0("diameter",threshold))), 
                                  max_centrality = max(get(paste0("centrality",threshold))),
                                  max_betweenness = max(get(paste0("betweenness", threshold)))) %>% 
                        select(component = 1, Ct, chorotype_spp, richness_spp, diameter, max_centrality, max_betweenness)
                        
        #### check overlap        
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
        
        # UPDATE GRAPH write those non all-overlapped components to non-overlap column in graph
        if(isTRUE(mark_overlap)) { graph = graph %>% morph(to_subgraph, subset_by = "nodes", 
                                        # criteria to write non-overlap for the first (and only) time in graph
                                        is.na(no_overlap) & name %in% spp_in_not_connected_components,
                                        remove_multiples = TRUE, delete.vertices= TRUE) %>% 
                                mutate(no_overlap = threshold) %>% 
                                unmorph()
}       }       }
                
                # UPDATE GRAPH write diameter in those species whose  component was larger than max_diameter
                if(isTRUE(filter_diameter)) { graph = graph %>% morph(to_subgraph, subset_by = "nodes", 
                                   is.na(filter) & name %in% (over_diam_spp %>% select(name) %>% pull),
                                   remove_multiples = TRUE, delete.vertices= TRUE) %>% 
                        mutate(filter = threshold) %>% 
                        unmorph()
                        }       
                
                # update tables
                g_spp_all = rbind(g_spp_all, g_spp)
                g_summary_all = rbind(g_summary_all, g_summary)
                        
        }  # main loop ends
                
        # summarize results and return list of objects
        if(isTRUE(mark_overlap)){
                chorotypes[['chorotypes']] = g_summary_all %>% group_by(chorotype_spp, richness_spp) %>% 
                        summarise(Ct_max = max(Ct), Ct_min = min(Ct), max_diam = max(diameter),max_centrality = max(max_centrality),
                                  max_betweenness = max(max_betweenness), no_overlap = max(!is.na(no_overlap))) %>% 
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
        print(chorotypes$parameters)
        
        return(chorotypes)
}

# test = SCAN_lite.15(graph = C, max_Ct = 0.65, min_Ct = 0.65, Ct_resolution = -0.05, max_diameter = 12,filter_diameter = T)

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
