# lets analyse the biogeography of the birds of the Amazon through SCAN_network

# set working directory
setwd("C:/Users/cassiano/hubic/Amazon_birds/R_Amazon_birds")
# read SCAN functions and load required libraries
source("C:/Users/cassiano/hubic/Amazon_birds/R_Amazon_birds/SOURCE_SCAN_network_14.R", echo=TRUE)
# load distribution maps of species
map = st_read("D:SIG2018_30GB/spp50_AEs_ecoreg.shp")
sa = st_read("D:/SIG2018_30GB/South_America/South_America.shp") # crs 4326 WGS84
sa = sa %>% st_union
amaz = st_read("D:/SIG2018_30GB/amazonia_sensulatissimo.shp")
st_crs(amaz) = 4326

# check column names and non-valid features
# map %>% str
map = map %>% select(sp = "SCINAME", geometry)
# map %>% names

# change CRS https://epsg.io/31985  SIRGAS 2000 25S to fix invalid features!! only way I managed to get some maps to work properly
# SIRGAS geographic is 4674
sirgas = 31977#4674
map = map %>% st_transform(crs = sirgas)  
sa = sa %>% st_transform(crs = sirgas)
amaz = amaz %>% st_transform(crs = sirgas)

# Check for 'invalid' shapes and fix them
invalid = !(st_is_valid(map))
if(any(invalid)){ map[invalid,] = map[invalid,] %>% st_make_valid }
invalid = !(st_is_valid(map))
map[invalid,]

'Filter all distributions larger than the Amazon area?'
area_map = map %>% st_area %>% units::set_units(.,km^2)
area_map = cbind(map %>% st_drop_geometry(), area_map)

area_amaz = amaz %>% st_area %>% units::set_units(.,km^2)

# filter those species with area of distribution larger than amazon sensulatissimo
"177 widely-distributed species were excluded"
map_bkup = map
map = map[-which(area_map$area_map > area_amaz),] # Simple feature collection with 965 features and 1 field

# calculate overlap
y = Sys.time()
ov = st_intersection(map[841:965,],map)
ar_ov = ov %>% st_area() %>% units::set_units(.,km^2)
ar_ov = ov %>% st_drop_geometry() %>% cbind(ar_ov) %>% select(sp1 = sp, sp2 = sp.1, area_ov = ar_ov) %>% as_tibble
# filter 'spurious' overlaps
ar_ov = ar_ov %>% filter(area_ov >  units::set_units(100, km^2))
area_overlaps = rbind(area_overlaps,ar_ov)
ov = c()

# Save 
write.csv2(area_overlaps, 'area_overlaps_0-965.csv')
# Sys.time() - y # mais ou menos 1hora pra cada 100 spp (x allmap)

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

# backup C
C %>% activate(nodes) %>% as_tibble() %>% write.csv2(.,"C_graph_nodes.csv")
C %>% activate(edges) %>% as_tibble() %>% write.csv2(.,"C_graph_edges.csv")

#########################
 ### SCAN network  ####
########################
# last MOD with mark_overlap and filer_overlap distinction  # don't know if max_diameter is working
SCANlist9791 = SCAN_lite(graph = C,       max_Ct = 0.97,    min_Ct = 0.91,    Ct_resolution = -0.02, max_diameter = 15,  mark_overlap =TRUE)
SCANlist8981 = SCAN_lite(graph = C,       max_Ct = 0.89,    min_Ct = 0.81,    Ct_resolution = -0.02, max_diameter = 15,  mark_overlap =TRUE)
SCANlist7971 = SCAN_lite(graph = C,       max_Ct = 0.79,    min_Ct = 0.71,    Ct_resolution = -0.02, max_diameter = 15,  mark_overlap =TRUE)
SCANlist6961 = SCAN_lite(graph = C,       max_Ct = 0.69,    min_Ct = 0.61,    Ct_resolution = -0.02, max_diameter = 15,  mark_overlap =TRUE)
SCANlist5953 = SCAN_lite(graph = C,       max_Ct = 0.59,    min_Ct = 0.53,    Ct_resolution = -0.02, max_diameter = 15,  mark_overlap =TRUE)
SCANlist5147 = SCAN_lite(graph = C,       max_Ct = 0.51,    min_Ct = 0.47,    Ct_resolution = -0.02, max_diameter = 15,  mark_overlap =TRUE)
SCANlist4543 = SCAN_lite(graph = C,       max_Ct = 0.45,    min_Ct = 0.43,    Ct_resolution = -0.02, max_diameter = 15,  mark_overlap =TRUE)
SCANlist4141 = SCAN_lite(graph = C,       max_Ct = 0.41,    min_Ct = 0.41,    Ct_resolution = -0.02, max_diameter = 15,  mark_overlap =TRUE)
SCANlist3939 = SCAN_lite(graph = C,       max_Ct = 0.39,    min_Ct = 0.39,    Ct_resolution = -0.02, max_diameter = 15,  mark_overlap =TRUE)
SCANlist3737 = SCAN_lite(graph = C,       max_Ct = 0.37,    min_Ct = 0.37,    Ct_resolution = -0.02, max_diameter = 15,  mark_overlap =TRUE)
 SCANlist3535 = SCAN_lite(graph = C,       max_Ct = 0.35,    min_Ct = 0.35,    Ct_resolution = -0.02, max_diameter = 15,  mark_overlap =TRUE)
SCANlist3333 = SCAN_lite(graph = C,       max_Ct = 0.33,    min_Ct = 0.33,    Ct_resolution = -0.02, max_diameter = 15,  mark_overlap =TRUE)
'Time difference of 8.089723 hours' #(35 e 33)


SCANlist3131 = SCAN_lite(graph = C,       max_Ct = 0.31,    min_Ct = 0.31,    Ct_resolution = -0.02, max_diameter = 15,  mark_overlap =TRUE)
SCANlist2929 = SCAN_lite(graph = C,       max_Ct = 0.29,    min_Ct = 0.29,    Ct_resolution = -0.02, max_diameter = 15,  mark_overlap =TRUE)

'Time difference of 11.59903 hours !!!'
SCANlist2727 = SCAN_lite(graph = C,       max_Ct = 0.27,    min_Ct = 0.27,    Ct_resolution = -0.02, max_diameter = 15,  mark_overlap =TRUE)
y = Sys.time()
SCANlist2525 = SCAN_lite(graph = C,       max_Ct = 0.25,    min_Ct = 0.25,    Ct_resolution = -0.02, max_diameter = 15,  mark_overlap =TRUE)
Sys.time() - y

# updatinsession
SCANlist2121 = list()
SCANlist2121$chorotypes = read.csv2("2121_chorotypes.csv")
SCANlist2121$all_spp_summary = read.csv2("2121_all_spp.summary.csv")
SCANlist2121$all_spp = read.csv2("2121_all_spp.csv")
SCANlist2121$graph = tbl_graph(nodes = read.csv2("2121_nodes.csv"), edges = read.csv2("2121_edges.csv"), directed = F)
SCANlist2121$parameters = read.csv2('2121_parameters.csv')

'birds is the object combining all scanned data'
'UPDATE SCAN_lite list'
## functions ##
update_graph = function(g1 = list_a$graph, g2 = list_b$graph){
        
        na = g1 %>% activate(nodes) %>% as_tibble()
        nb = g2 %>% activate(nodes) %>% as_tibble()
        ea = g1 %>% activate(edges) %>% as_tibble()
        eb = g2 %>% activate(edges) %>% as_tibble()
        
        nab = left_join(na,nb)
        eab = left_join(ea,eb)
        
        tbl_graph(nodes = nab, edges = eab, directed = F)
}

update_SCAN_list = function(list_a, list_b){
        updatedSCANlist = list()
        # chorotypes tibble
        updatedSCANlist$chorotypes = rbind(list_a$chorotypes, list_b$chorotypes)
        updatedSCANlist$chorotypes = updatedSCANlist$chorotypes %>% group_by(chorotype_spp, richness_spp) %>% 
                summarise(no_overlap = max(no_overlap),Ct_max = max(Ct_max), Ct_min = min(Ct_min), diameter = max(diameter), 
                          max_centrality = max(max_centrality), max_betweenness = max(max_betweenness)) %>% 
                arrange(desc(Ct_max), desc(richness_spp), chorotype_spp)
        # summary for all species
        updatedSCANlist$all_spp_summary = rbind(list_a$all_spp_summary, list_b$all_spp_summary) %>% 
                group_by(name, no_overlap,components, order) %>% 
                summarise(max_Ct = max(max_Ct), min_Ct = min(min_Ct), max_diam = max(max_diam),
                          min_diam = min(min_diam), max_between = max(max_between))
        
        # detailed species accounts
        updatedSCANlist$all_spp = rbind(list_a$all_spp, list_b$all_spp)
        
        # graph of spatial interactions DO NOT USE GRAPH_JOIN -> it duplicates the edges
        updatedSCANlist$graph =  #graph_join(list_a$graph, list_b$graph) %>% to_undirected() %>%  igraph::simplify() %>% as_tbl_graph(directed = F) # this loses the Cs column ...
                update_graph(g1 = list_a$graph, g2 = list_b$graph)
        # combined descriptive parameters
        updatedSCANlist[["parameters"]] = tibble(max_diameter = max(list_a$parameters$max_diameter,list_b$parameters$max_diameter),
                                        max_Ct = max(list_a$parameters$max_Ct, list_b$parameters$max_Ct), 
                                        min_Ct = min(list_a$parameters$min_Ct, list_b$parameters$min_Ct), 
                                        Ct_resolution = list_a$parameters$Ct_resolution,
                                        mark_overlap = list_a$parameters$mark_overlap,
                                        filter_overlap = list_a$parameters$filter_overlap,
                                        filter_diameter = list_a$parameters$filter_diameter)
        updatedSCANlist
}

bird = SCANlist9791
bird = update_SCAN_list(list_a = bird, list_b = SCANlist8981)
bird = update_SCAN_list(list_a = bird, list_b = SCANlist7971)
bird = update_SCAN_list(list_a = bird, list_b = SCANlist6961)
bird = update_SCAN_list(list_a = bird, list_b = SCANlist5953)
bird = update_SCAN_list(list_a = bird, list_b = SCANlist5147)
bird = update_SCAN_list(list_a = bird, list_b = SCANlist4543)
bird = update_SCAN_list(list_a = bird, list_b = SCANlist4141)
bird = update_SCAN_list(list_a = bird, list_b = SCANlist3939)
bird = update_SCAN_list(list_a = bird, list_b = SCANlist3737)
bird = update_SCAN_list(list_a = bird, list_b = SCANlist3535)
bird = update_SCAN_list(list_a = bird, list_b = SCANlist3333)
bird = update_SCAN_list(list_a = bird, list_b = SCANlist3131)
bird = update_SCAN_list(list_a = bird, list_b = SCANlist2929)
bird = update_SCAN_list(list_a = bird, list_b = SCANlist2727)
bird = update_SCAN_list(list_a = bird, list_b = SCANlist2525)
bird = update_SCAN_list(list_a = bird, list_b = SCANlist2323)
bird_bckup = bird
'filtering out species already "non-overlapped" - a thousand times faster'
#using bird
'completing the SCAN process to low levels of congruence, removing most already analyzed and non-overlapped species...'
#using filtered C
# current_threshold = threshold
# filter_out_spp = bird$graph %>% activate(nodes) %>% filter(!is.na(no_overlap) & (no_overlap > current_threshold)) %>% as_tibble() %>% select(name) %>% pull()
# filter_in_spp = C %>% activate(nodes) %>% filter(!name %in% filter_out_spp) %>% as_tibble() %>% select(name) %>% pull()

SCANlist3531b = SCAN_lite(graph = C %>% activate(nodes) %>% filter(!name %in% filter_out_spp), 
                max_Ct = 0.35, min_Ct = 0.25, Ct_resolution = -0.02, max_diameter = 15,  mark_overlap =TRUE)

SCANlist2515 = SCAN_lite(graph = C %>% activate(nodes) %>% filter(!name %in% filter_out_spp), 
                                    max_Ct = 0.35, min_Ct = 0.25, Ct_resolution = -0.02, max_diameter = 15,  mark_overlap =TRUE)

'UPDATE bird list'
bird = update_SCAN_list(list_a = bird, list_b = SCANlist3531b)  #  <- NOT WORKING with SCANlists based on bird$graph


# save SCAN list
write.csv2(bird[[1]], 'bird$chorotypes.csv')
write.csv2(bird[[2]], 'bird$all_spp_summary.csv')
write.csv2(bird[[3]], 'bird$all_spp.csv')
write.csv2(bird[[4]] %>% activate(nodes) %>% as_tibble, 'bird$graph_nodes.csv')
write.csv2(bird[[4]] %>% activate(edges) %>% as_tibble, 'bird$graph_edges.csv')
write.csv2(bird[[5]], 'bird$parameters.csv')

# summarized version of chorotypes (filtered to non-overlap)
write.csv2(bird$chorotypes %>% filter(is.na(no_overlap)) %>%  group_by(chorotype_spp, richness_spp) %>% summarise(Ct_max = max(Ct_max), Ct_min = min(Ct_min), min_diam = min(diameter)) %>% arrange(desc(Ct_max), desc(richness_spp), chorotype_spp),
        'chorotypes_summarised.csv')
#purge 
rm(l1,l2,l3, graph, chorotypes, bs) #, basic_map, basic_map5)
#######################################################################################################
#### PLOTS ####
library(extrafont)
loadfonts(device = "win")
extrafont::loadfonts(device="win")
library(ggplot2)
library(ggforce)

# customizing maps transforming crs to 4618 SAD69
# MAPS
map = map %>% st_transform(crs = 4618) # EPSG:4618  SAD69 -- Brazil
# SA & rivers
sa = st_read("C:/SIG2018_lite/south_america_clean_shape/south_america_clean_shape.shp")
sa = sa %>% st_transform(crs = st_crs(4618)) %>% st_as_sf() %>% select(geometry = 1)
sa = sa %>% mutate(component = 0, sp = 'sa') %>% select(component, sp, geometry)
rivers = st_read("C:/SIG2018_lite/Rivers/majorrivers_0_0/majorrivers.shp")
rivers = rivers %>% st_transform(crs = 4618)
rivers = rivers %>% select(sp = NAME) %>%  st_filter(sa)
rivers = rivers %>% group_by() %>% summarise() %>% mutate(sp = 'rivers') %>% select(sp, geometry)
amaz = amaz %>% st_transform(crs = 4618)
tab = bird$chorotypes %>% mutate(fst = first(chorotype_spp %>% strsplit(., split = ',') %>% first())) %>% 
        arrange(fst, diameter, desc(Ct_max)) %>% select(-'fst')
# save table
tab %>% write.csv2("Bird_chorotypes_SA_.97-.49.csv")
chorotypes$all_spp %>% write.csv2(.,"chorotypes_all_spp_.97-.49.csv")


#### SET THRESHOLD & SUBSET GRAPH - updated in plot_graph_maps_2.R ####
g = bird$graph
# SETUP
{save = FALSE; part = 1; cut = 9; select_chorotypes = TRUE; 
device1 = 4; device2 = 5
#palette =  "Accent" #'Greens'#"Spectral" # "Reds"#
palette = "Spectral" #'Diverging', #"BrBG" , #RdYlGn","PiYG", #Diverging  BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral
alfa_map = 0.2
}
#  set threshold 
threshold = 0.77; " (the threshold value chosen here must match a column defined in the SCAN object$graph)"
{
g_spp = g %>% activate(nodes) %>% as_tibble %>% 
        filter(get(paste0("Ct",threshold))) %>% 
        select(name, paste0('components',threshold)) %>% 
        arrange(get(paste0('components',threshold)), name) %>% unique
n_comp = g_spp %>% select(contains('component')) %>% pull() %>% unique() %>% length(); n_comp

g_full = g %>% activate(nodes) %>% #as_tibble %>% group
        filter(get(paste0("Ct",threshold))) %>% #select(Ct0.69)
        select(name, paste0('components',threshold)) %>% 
        activate(edges) %>% select(from, to, Cs, paste0('Ct',threshold)) %>% filter(get(paste0("Ct",threshold))) %>% 
        activate(nodes) %>% arrange(get(paste0('components',threshold)), name)

# right join to preserve sf attributes in tibble (the left object is spatial - the result inherits its properties)
g_map1 = right_join( map, g_spp, by = c('sp' = 'name')) %>% 
        select(component = paste0('components',threshold), everything())
}                                                                                       # use_sa = TRUE; # if(isTRUE(use_sa)){ g_map2 = rbind(g_map1, sa) }

## CREATE LIST OF CHOROTYPES TO BE PLOTTED 
print('all_components'); all_components =  g_full %>% activate(nodes) %>% select(contains('components')) %>% pull() %>% unique(); all_components
## LIST OF CHOROTYPES to be show
list_chorotypes = c(3:5);#1:13,15:21);
# cooking graph and map objects
{
# partial g_sub
if(isTRUE(select_chorotypes)){ g_sub = g_full %>% activate(nodes) %>% filter(get(paste0('components', threshold))  %in% list_chorotypes); ind = ''
} else {if(part == 1) {g_sub = g_full %>% activate(nodes) %>% filter(get(paste0('components', threshold))  < cut); ind = 'A'
} else {g_sub = g_full %>% activate(nodes) %>% filter(get(paste0('components', threshold))  >= cut); ind = 'B'}}

{n_comp = g_sub %>% activate(nodes) %>% select(contains('components')) %>% pull() %>% unique() %>% length()
        comps = g_sub %>% activate(nodes) %>% select(contains('components')) %>% pull() %>% unique() %>% paste0(.,collapse = ", ")
        print('components ready to  print')
        print(comps)}

# get only selected species to the map 
{g_spp = g_sub %>% activate(nodes) %>% as_tibble()
        # right join to preserve sf attributes in tibble (the left object is spatial - the result inherits its properties)
g_map1 = right_join( map, g_spp, by = c('sp' = 'name')) %>% 
                select(component = paste0('components',threshold), everything())
}

#  CHECK both map and graph species and components
g_map1 %>% st_drop_geometry() %>% arrange(sp) %>%  group_by(component) %>% summarise(spp = paste0(sp, collapse = ', '))
g_sub %>% activate(nodes) %>% select(name, comp = paste0('components', threshold)) %>% as_tibble() %>% 
        group_by(comp) %>% arrange(name) %>%  summarise(spp = paste(name, collapse = ', ')) 
                                
# graph setups                       # choose palette: #'Diverging', #"BrBG" , #RdYlGn","PiYG", #Diverging  BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral
{deg <- degree(g_sub, mode="all")
lay = create_layout(g_sub, layout = 'graphot') # 'fr') # 'dh') # 'mds')#"drl") # other options kk, fr, ldl, gem
lou <- cluster_louvain(g_sub)
label  = FALSE}

basic_graph = 
        ggraph(lay) + 
        geom_edge_link(aes(alpha = (Cs+0.75)) , width = 1.25 , show.legend = FALSE) + #
        geom_node_point(aes(fill = ifelse(!is.na(get(paste0("components",threshold))),as.factor(get(paste0("components",threshold))), NA)),
                        size =  (degree(g_sub, mode="all") + 12) / 4, shape = 21, show.legend = FALSE) + 
        scale_fill_distiller( direction = 1, palette = palette, #"Spectral",
                              na.value = "transparent", aesthetics = "fill") + #"BrBG" , #RdYlGn","PiYG",
        geom_node_text(aes(label = name), size = 3, col = "black", repel=TRUE) +
        labs( subtitle = paste0("Ct = ", threshold)) +
        theme_graph() 
# add hull and community labels
basic_graph2 = basic_graph +
        geom_mark_hull(aes(x, y, group = get(paste0("components",threshold))
                           ,label = get(paste0("components",threshold))),
                       label.fontsize = 15,
                       fill = "transparent", lty = "dotted",
                       concavity = 1, 
                       expand = unit(3, "mm"), 
                       alpha = 0.05) +
        theme(legend.position = "none")
# if(isTRUE(save)) ggsave(paste0("Graph_Ct", threshold,"_",ind,".png"))
if(FALSE) {# graph_Ct = get(paste0('graph_Ct',threshold))
# graph_Ct + facet_nodes(~ as.factor(get(paste0("components",threshold)))) + facet_nodes(~class)
# simple
# ggraph(lay) + geom_edge_link() + geom_node_point() + # size = 5, shape = 21, color = 'black') + geom_node_text(aes(label = name)) + #, size = 4.5, repel=TRUE) +  theme_graph()
# fancy map plots 'https://cran.r-project.org/web/packages/rinat/vignettes/species-distribution.html' 
}
# centroids as x,y aesthetics to hull
g_map1 = g_map1 %>% mutate(centroids = st_centroid(geometry), x = st_coordinates(centroids)[,1], 
                           y = st_coordinates(centroids)[,2])
basic_only = FALSE
# Sophisticated maps
if(isTRUE(basic_only)){
        basic_map = 
                ggplot(data = g_map1 %>% arrange(component)) + 
                geom_sf( aes(fill = component), # THIS IFELSE STAT. TURNS FILL TO CONTINUOUS... use distiller, otherwise scale_fill_brewer to discrete palette
                         alpha = alfa_map, color = 'black', show.legend = F) + 
                geom_sf(data = sa, fill = NA, color = 'black') +
                scale_fill_distiller( direction = 1, palette =   palette, #"Spectral", #"BrBG" , #RdYlGn","PiYG", #Diverging  BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral #scale_fill_continuous( na.value = "transparent") +    # low = 'black', high = 'green',
                                      na.value = "transparent", aesthetics = "fill") + #start = 0.2, end = 0.8, #Diverging  BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral
                ggtitle(paste0("Birdss     Ct = ", threshold, "     ", n_comp, " chorotypes"),
                        sub = paste('components:', comps)) +# pull() %>% unique())) + 
                theme_minimal() 
} else {
        
        basic_map2 = 
                ggplot(data = g_map1 %>% arrange(component)) + 
                geom_sf( aes(fill =  as.numeric(component)), # if fill is factor use scale_fill_brewer which is discrete
                         alpha = alfa_map, color = 'black', show.legend = F) + 
                geom_sf(data = sa, fill = NA, color = 'black') +
                
                geom_point(aes(x,y), size = 0.7, alpha = 0.5) +
                
                geom_mark_hull(aes(x,y,  group = component, label = component), #col = "transparent",
                               expand = unit(2, "mm"), radius = unit(1, "mm"),alpha = 0.5,
                               label.fill = "transparent", lty = 'dotted', 
                               con.size = 1, label.fontsize = 15) +
                
                scale_fill_distiller( direction = 1, palette =   palette, #"Spectral", #"BrBG" , #RdYlGn","PiYG", #scale_fill_continuous( na.value = "transparent") +    # low = 'black', high = 'green',
                                      na.value = "transparent", aesthetics = "fill") + #start = 0.2, end = 0.8, #Diverging  BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral
                ggtitle(paste0("Birds     Ct = ", threshold, "     ", n_comp, " chorotypes"),
                        sub = paste('components:', comps)) +# pull() %>% unique())) + 
                theme_minimal() + 
                theme(legend.position = "none")
}
# expand axes here according to this factor
exp_fact = c(0.01,0.01)
box = st_bbox(g_map1)

# zoom based on bbox
basic_map4 = basic_map2 +
        scale_x_continuous(expand = exp_fact) +
        scale_y_continuous(expand = exp_fact) +
        coord_sf(xlim = c(box[1], box[3]),
                 ylim = c(box[2], box[4]))


}
if(pg) {dev.set(device1); basic_graph2}
if(pm) {dev.set(device2); basic_map4}

"threshold"; threshold;'species by component'; g_spp %>% group_by(component = get(paste0("components",threshold))) %>% summarise(spp = paste(name, collapse = ", ")) %>% mutate(threshold = threshold)

save_map_components = function(...) st_write(g_map1, paste0("map2_Ct",threshold,"_",paste0(list_chorotypes,collapse = '_'),".shp"), driver = "ESRI shapefile")


#### Extras, fixes, etc ####
## fixing issues with update_SCAN_list
gg =   bird$graph %>% activate(nodes) %>% as_tibble() # %>% select(name,contains("no_overlap")) %>% write.csv2(., "no_overlaps_cols.csv")
gg = gg %>%  mutate(across(contains("no_overlap"), \(x) {ifelse(is.na(x), 0, x)})) %>% 
        rowwise() %>% mutate(no_overlap = max(c_across(contains("no_overlap")))) %>% ungroup() %>%
        mutate(no_overlap = ifelse(no_overlap ==0, NA, no_overlap)) %>% 
        select(name, .tidygraph.index = .tidygraph_index.x, no_overlap, contains("components"), contains("Ct0."))

g = tbl_graph(nodes = gg, edges = bird$graph %>% activate(edges) %>% as_tibble, directed = F)
bird$graph = g
#

# basic_map3 = basic_map2 + #         scale_x_continuous(expand = exp_fact) +
#         scale_y_continuous(expand = exp_fact) +
#         coord_sf(xlim = c(range(g_map1$x)[1], range(g_map1$x)[2]) ,#-80,-50), # may use absolute limits
#                  ylim = c(range(g_map1$y)[1], range(g_map1$y)[2])) #c(-20,20))  

# zoom based on bbox No hulls 
# basic_map5 = basic_map +
#         scale_x_continuous(expand = exp_fact) +
#         scale_y_continuous(expand = exp_fact) +
#         coord_sf(xlim = c(box[1], box[3]),
#                  ylim = c(box[2], box[4]))


# fix duplicated names in bird$graph
'igraph::simplify'

# test = graph_join(bs4543$graph,bs4543$graph)
# test = igraph::simplify(test)
# test = test %>% as_tbl_graph(directed = FALSE)
# 
# g = igraph::simplify(g)
# g = g %>% as_tbl_graph(directed = FALSE)
# 
# g
# g %>% activate(nodes) %>% as_tibble %>% group_by(name) %>% summarise(n = n()) %>% filter(n != 5)
# 
# test = g %>% activate(nodes) %>% as_tibble %>% filter(str_detect(name, 'Amazilia'))
# 

'two tables to graph
tbl_graph(charactersdf,interactionsdf, directed=F)'

# get the nodes data frame and fix it
# x = seq(97,43, -2); paste0('Ct0.',x,' = any(Ct0.', x, '), components0.', x, ' =  ifelse( !all(is.na(components0.', x,')), max(components0.', x, ', na.rm=T), NA),') %>% write.table(.,'clipboard')
# nodes_ref = bird$graph %>% activate(nodes) %>% as_tibble() %>% group_by(name, .tidygraph_index) %>% summarise()
# nodes_ref %>% arrange(.tidygraph_index)
# 
# 
# 
# nodes = bird$graph %>% activate(nodes) %>% as_tibble() %>% group_by(name, .tidygraph_index) %>% 
#         summarise(  no_overlap = max(no_overlap), #%>% filter(!is.na(no_overlap))
#                    Ct0.97 = any(Ct0.97), components0.97 =  ifelse( !all(is.na(components0.97)), max(components0.97, na.rm=T), NA),
#                    Ct0.95 = any(Ct0.95), components0.95 =  ifelse( !all(is.na(components0.95)), max(components0.95, na.rm=T), NA),
#                    Ct0.93 = any(Ct0.93), components0.93 =  ifelse( !all(is.na(components0.93)), max(components0.93, na.rm=T), NA),
#                    Ct0.91 = any(Ct0.91), components0.91 =  ifelse( !all(is.na(components0.91)), max(components0.91, na.rm=T), NA),
#                    Ct0.89 = any(Ct0.89), components0.89 =  ifelse( !all(is.na(components0.89)), max(components0.89, na.rm=T), NA),
#                    Ct0.87 = any(Ct0.87), components0.87 =  ifelse( !all(is.na(components0.87)), max(components0.87, na.rm=T), NA),
#                    Ct0.85 = any(Ct0.85), components0.85 =  ifelse( !all(is.na(components0.85)), max(components0.85, na.rm=T), NA),
#                    Ct0.83 = any(Ct0.83), components0.83 =  ifelse( !all(is.na(components0.83)), max(components0.83, na.rm=T), NA),
#                    Ct0.81 = any(Ct0.81), components0.81 =  ifelse( !all(is.na(components0.81)), max(components0.81, na.rm=T), NA),
#                    Ct0.79 = any(Ct0.79), components0.79 =  ifelse( !all(is.na(components0.79)), max(components0.79, na.rm=T), NA),
#                    Ct0.77 = any(Ct0.77), components0.77 =  ifelse( !all(is.na(components0.77)), max(components0.77, na.rm=T), NA),
#                    Ct0.75 = any(Ct0.75), components0.75 =  ifelse( !all(is.na(components0.75)), max(components0.75, na.rm=T), NA),
#                    Ct0.73 = any(Ct0.73), components0.73 =  ifelse( !all(is.na(components0.73)), max(components0.73, na.rm=T), NA),
#                    Ct0.71 = any(Ct0.71), components0.71 =  ifelse( !all(is.na(components0.71)), max(components0.71, na.rm=T), NA),
#                    Ct0.69 = any(Ct0.69), components0.69 =  ifelse( !all(is.na(components0.69)), max(components0.69, na.rm=T), NA),
#                    Ct0.67 = any(Ct0.67), components0.67 =  ifelse( !all(is.na(components0.67)), max(components0.67, na.rm=T), NA),
#                    Ct0.65 = any(Ct0.65), components0.65 =  ifelse( !all(is.na(components0.65)), max(components0.65, na.rm=T), NA),
#                    Ct0.63 = any(Ct0.63), components0.63 =  ifelse( !all(is.na(components0.63)), max(components0.63, na.rm=T), NA),
#                    Ct0.61 = any(Ct0.61), components0.61 =  ifelse( !all(is.na(components0.61)), max(components0.61, na.rm=T), NA),
#                    Ct0.59 = any(Ct0.59), components0.59 =  ifelse( !all(is.na(components0.59)), max(components0.59, na.rm=T), NA),
#                    Ct0.57 = any(Ct0.57), components0.57 =  ifelse( !all(is.na(components0.57)), max(components0.57, na.rm=T), NA),
#                    Ct0.55 = any(Ct0.55), components0.55 =  ifelse( !all(is.na(components0.55)), max(components0.55, na.rm=T), NA),
#                    Ct0.53 = any(Ct0.53), components0.53 =  ifelse( !all(is.na(components0.53)), max(components0.53, na.rm=T), NA),
#                    Ct0.51 = any(Ct0.51), components0.51 =  ifelse( !all(is.na(components0.51)), max(components0.51, na.rm=T), NA),
#                    Ct0.49 = any(Ct0.49), components0.49 =  ifelse( !all(is.na(components0.49)), max(components0.49, na.rm=T), NA),
#                    Ct0.47 = any(Ct0.47), components0.47 =  ifelse( !all(is.na(components0.47)), max(components0.47, na.rm=T), NA),
#                    Ct0.45 = any(Ct0.45), components0.45 =  ifelse( !all(is.na(components0.45)), max(components0.45, na.rm=T), NA),
#                    Ct0.43 = any(Ct0.43), components0.43 =  ifelse( !all(is.na(components0.43)), max(components0.43, na.rm=T), NA))
# 
# 'edges' # explorations show that is edges is a mess - I'll abandon Ct info at edges and use the original C edges with Cs between spp
# # edges = bird$graph %>% activate(edges) %>% as_tibble() 
# # edges %>% group_by(from) %>% summarise()
# # x = seq(97,43, -2); paste0('Ct0.',x,' = any(Ct0.', x, '), components0.', x, ' =  ifelse( !all(is.na(components0.', x,')), max(components0.', x, ', na.rm=T), NA),') %>% write.table(.,'clipboard')
# # 
# # edges %>% group_by(from) %>% summarise(mm = max(Ct0.49)) %>% filter(!is.na(mm))
# # 
# # bird$graph %>% activate(edges) %>% as_tibble()
# 
# 'g = igraph::simplify(g)
# g = g %>% as_tbl_graph(directed = FALSE)'
# 
# 'edges = C %>% activate(edges) %>% as_tibble()
# 
# birdgraph = tbl_graph(nodes,edges, directed=F)'

jesussaves = function(){
        print("Gees is saving this bullshit!!!")
        save.image("C:/Users/cassiano/hubic/Amazon_birds/R_Amazon_birds.RData")}

