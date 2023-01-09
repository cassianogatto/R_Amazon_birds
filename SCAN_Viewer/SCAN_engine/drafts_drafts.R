# filter species by name
textInput(inputId = "filter_species", label = "Filter species by name")

# filter species inside reactive map()
if(!is.null(input$filter_species)){map <- map |> filter(! sp %in% input$filter_species)}



library(shiny)
library(shinydashboard)

library(dplyr)
library(igraph)
library(tidygraph)
library(sf)

library(units)

library(tidyr)
library(ggraph)
library(readr)

library(ggplot2)
library(leaflet)
library(rgdal)


map = st_read("C:/Users/cassiano/hubic/Amazon_monkeys/Monkeys_shapefiles_II.shp")

map = map |> group_by(sp) |> summarise()

# species list
spp <- primates |> st_drop_geometry() |> select(sp)

# unique combinations of species - see expand() for tables  # not needed anymore see obs below overlapping
comb <- spp |> expand(sp1 = sp, sp2 = sp )  |> 
        
        filter(!duplicated(paste0(pmax(sp1, sp2), pmin(sp1, sp2)))) |> 
    
        filter(sp1 != sp2)

# comb => 23220 rows

# vector with only combinations of overlapping polygon pairs!!! 
# y = Sys.time()

overlapping <- reactive({ st_intersects(map(), map(), sparse = F) |> as_tibble() |> 
                
        setNames(map()$sp) |> mutate(sp1 = map()$sp) |> select(sp1, everything()) |> 
        
        pivot_longer(cols = !1, names_to = "sp2") |> filter(value) |> filter(sp1 != sp2) |> 
        
        filter(!duplicated(paste0(pmax(sp1, sp2), pmin(sp1, sp2))))
        
})

# versão normal

overlapping_combinations <- functin(map = map){
    
    st_intersects(map, map, sparse = F) |> as_tibble() |> 
        
        setNames(map$sp) |> mutate(sp1 = map$sp) |> select(sp1, everything()) |> 
        
        pivot_longer(cols = !1, names_to = "sp2") |> filter(value) |> filter(sp1 != sp2) |> 
        
        filter(!duplicated(paste0(pmax(sp1, sp2), pmin(sp1, sp2))))
    }

ov <- st_intersects(map, map, sparse = F) |> as_tibble() |> 
    
    setNames(map$sp) |> mutate(sp1 = map$sp) |> select(sp1, everything()) |> 
    
    pivot_longer(cols = !1, names_to = "sp2") |> filter(value) |> filter(sp1 != sp2) |> 
    
    filter(!duplicated(paste0(pmax(sp1, sp2), pmin(sp1, sp2))))



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


# y =Sys.time()


areas = overlap_function(tab_species = ov, map = map)

areas  = areas |> mutate( area_sp1 = area_sp1 |> units::set_units("km^2"),
                          area_sp2 = area_sp2 |> units::set_units("km^2"),
                          area_overlap = area_overlap |> units::set_units("km^2")
                          )

Cs <- areas |> mutate(Cs = (area_overlap / area_sp1) * (area_overlap / area_sp2)) |> select(sp1,sp2, Cs) 
    
# Sys.time()- y            


Cs <- Cs |> filter( Cs > 0.3) 
    







# overlapping => 2973 rows

# Sys.time() - y  # 5.15849 secs !
# the time difference is irrelevant, at least for about 200 species

# sf::sf_use_s2(FALSE)
# 
# overlapping |> mutate(area_overlap = st_area(  
#                           
#             st_intersection( primates[primates$sp == sp1 , "geometry"], 
#                                  
#                                 primates[primates$sp == sp2, "geometry"])))  #  |> units::set_units(.,km^2)
#                 
                
# overlap_function = function(tab_names = overlapping, map = primates){
#     
#     results = data_frame(area = rep(NA, nrow(tab_names)))
#     
#     for (i in 1:nrow(tab_names)){
#         
#         map1 = map |> filter(sp == tab_names[i,]) |> st_geometry()
#         
#         map2 = map |> filter(sp == tab_names[i,"sp2"]) |> st_geometry()
#         
#         intersection <- map1 |> st_intersection(map2)
#         
#         intersection <- intersection |> st_area() #|>  units::set_units(.,km^2)
#         
#         results$area[i] <- intersection
#         
#     }
#     
# }




    
# overlapping |> mutate( pastel = paste(sp1,sp2))                
# 
# overlap_function = function(sp1 = sp1, sp2 = sp2){
#     
#     st_area(
#         
#         st_intersection( 
#             
#             primates |> filter(sp == sp1) |> st_geometry(), 
#             
#             primates |> filter(sp == sp2) |> st_geometry()
#             
#         ) 
#          
#     )
# }
#         
    
get_map <- function(df = overlapping, row = 1, map = primates, col = 'sp'){
    
        map[map |> select(paste(col)),]
}    



overlaps <- st_overlaps(map1, map2, sparse = F)
    
    names(overlaps) <- map2 |> st_drop_geometry() |> select(paste0(col_sp)) |> pull()
    
    overlaps <- overlaps |> mutate(sp1 = map1 |> st_drop_geometry() |> select(paste0(col_sp)) |> pull()) |> select(sp1, everything())
    
    




overlaps <- overlaps |> as.data.frame()





overlaps <- overlaps |> pivot_longer(cols = 2:4, names_to = 'sp2')

overlaps |> filter(value)

overlaps_df <- as.data.frame(as.table(overlaps))






                                                     (
    test[test$sp[i] == comb_spp[i,"sp1"]), st_geometry(test[test$sp == comb_spp$sp2[i]]))


Sys.time() - y


comb_spp <- comb_spp |> mutate(intersects = st_intersects(
        test[test$sp %in% comb_spp$sp1] |> st_drop_geometry() ,
        test[test$sp %in% comb_spp$sp2] |> st_geometry()
    )
)

comb_spp <- comb_spp |> mutate(st_intersection( 
    map()[map()$sp == sp1,] |> st_geometry(), 
    map()[map()$sp == sp2,] |> st_geometry()
    )
)
# area overlaps

map_overlaps <- st_intersection(map(),map())


# test <- st_read(file.choose())









# Check for 'invalid' shapes and fix them
invalid = !(st_is_valid(map()))
if(any(invalid)){ map()[invalid,] = map()[invalid,] %>% st_make_valid() }
invalid = !(st_is_valid(map()))
invalid = map()[invalid,"sp"]

'Filter all distributions larger than the Amazon area?'
area_map = map %>% st_area %>% units::set_units(.,km^2)
area_map = cbind(map %>% st_drop_geometry(), area_map)

area_amaz = amaz %>% st_area %>% units::set_units(.,km^2)


# calculate overlap
y = Sys.time()
ov = st_intersection(map[841:965,],map)
ar_ov = ov %>% st_area() %>% units::set_units(.,km^2)
ar_ov = ov %>% st_drop_geometry() %>% cbind(ar_ov) %>% select(sp1 = sp, sp2 = sp.1, area_ov = ar_ov) %>% as_tibble
# filter 'spurious' overlaps
ar_ov = ar_ov %>% filter(area_ov >  units::set_units(100, km^2))
area_overlaps = rbind(area_overlaps,ar_ov)
ov = c()

'whole map (cannot run - there is not enough memory to allocate)'
area_overlaps %>% group_by(sp1) %>% summarise(n())
area_overlaps %>% group_by(sp2) %>% summarise(n())

Cs_overlaps = area_overlaps %>% left_join(.,area_map %>% as_tibble, by = c('sp1' = 'sp')) %>% select(1:3,area_sp1 = area_map)
Cs_overlaps = Cs_overlaps %>% left_join(.,area_map %>% as_tibble, by = c('sp2' = 'sp')) %>% select(1:4,area_sp2 = area_map)
Cs_overlaps = Cs_overlaps %>% mutate(Cs = round((area_ov/area_sp1)*(area_ov/area_sp2),3)) %>% select(1,2,Cs,everything())
# filters
Cs_overlaps = Cs_overlaps %>% filter(sp1 != sp2)
Cs_overlaps = Cs_overlaps %>% filter(Cs > units::set_units(0.0099,1))

Cs = Cs_overlaps %>% select(1,2,3)
Cs %>% arrange(desc(Cs))

# Cs %>% write.csv2(.,'Cs_spatial_congruence_Amazon_birds.csv')
Cs = read.csv('Cs_spatial_congruence_Amazon_birds.csv', header = T, sep = ',') %>% as_tibble()
#### CREATE GRAPH OBJECT  ####
C  = Cs %>% mutate(Cs = round(Cs,2)) %>% 
    select(from = sp1, to = sp2, Cs) %>%
    as_tbl_graph(directed=FALSE)

### Simplify in IGRAPH and convert back to tbl_graph
# avoid double edges connections but see the attributes functions at https://igraph.org/r/doc/igraph-attribute-combination.html
C = C %>% igraph::simplify(remove.multiple = TRUE, remove.loops = FALSE, edge.attr.comb="first")
C = C %>% as_tbl_graph(directed = FALSE)
C = C %>% activate(edges) %>% select(from,to, Cs)
C = C %>% activate(nodes) %>% mutate(.tidygraph_index = seq_len(n()))







# function
y = Sys.time()

st_inter  = list()

for(i in 1:50){
    
    map_sp1 <- test[which(test$sp == comb[i,'sp1']),'sp'] |> st_geometry()
    
    map_sp2 <- test[which(test$sp == comb[i,'sp2']),'sp'] |> st_geometry()
    
    st_inter = st_overlaps(map_sp1, map_sp2, sparse = F)
}

library(stringr)

Sys.time() - y   

gui <- test |> filter(sp %in% c("Ateles paniscus", "Saguinus midas", "Chirop sagul", "Saimiri sciureus"))

ateles <- test |> filter(sp |> str_detect("Ateles"))


map_test = test[1:20,]

# tool to find species combinations whose distributions overlap in some way (st_overlaps does not detect fully overlapped spp)
y = Sys.time()
intersects = st_intersects(map_test, map_test, sparse = F) |> as_tibble() |> setNames(map_test$sp) |> mutate(sp1 = map_test$sp) |> select(sp1, everything()) |> 
    
    pivot_longer(cols = 2:5, names_to = "sp2") |> filter(value)
Sys.time() - y




### testing SCAN

Cs = read.csv("C:/Users/Cassiano/Hubic/Amazon_monkeys/Cs_Monkeys.csv") # is the Cs calculated with engine: it seems ok

map  = st_read("C:/Users/Cassiano/Hubic/Amazon_monkeys/Monkeys_shapefiles_II.shp")

C = Cs |> as_tbl_graph(directed = F)

C = C %>% igraph::simplify(remove.multiple = TRUE, remove.loops = FALSE, edge.attr.comb="first")
C = C %>% as_tbl_graph(directed = FALSE)
C = C %>% activate(edges) %>% select(from,to, Cs)
C = C %>% activate(nodes) %>% mutate(.tidygraph_index = seq_len(n()))

graph <- Cs |>   select(from = sp1, to = sp2, Cs)   |>    
    
    as_tbl_graph( directed = FALSE )


SCAN_lite(graph = C, filter_overlap = TRUE)


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
    
    SCAN_lite = function(graph = C, max_Ct = 1, min_Ct = 0.8, Ct_resolution = - 0.02,
                         max_diameter = 10, mark_overlap = TRUE, filter_overlap = TRUE,
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
            
            chorotypes[['all_spp_summary']] = g_spp_all %>% 
                group_by(name, components, order) %>% 
                summarise(max_Ct = max(Ct),
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
    
}
