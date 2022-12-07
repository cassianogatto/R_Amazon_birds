#setups
g = bird$graph 
g = test$graph
 {save = FALSE; part = 1; cut = 9; select_chorotypes = TRUE; 
device1 = 4; device2 = 5
pg = T;pm =T;
palette = c('Accent')#"Spectral"#'Accent'#'Greens' # "Reds"#"BrBG" , #RdYlGn","PiYG", #  BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral
alfa_map = 0.2
layout =c('fr') # 'graphot') # 'fr') # 'dh') # 'mds')#"drl") # other options kk, fr, ldl, gem
}
#### set threshold  " (the threshold value chosen here must match a column defined in the SCAN object$graph)"####
threshold = 0.47
# 1st main routine
{        g_spp = g %>% activate(nodes) %>% as_tibble %>% 
                filter(!is.na(get(paste0("components",threshold)))) %>% 
                select(name, paste0('components',threshold)) %>% 
                arrange(get(paste0('components',threshold)), name) %>% unique
        n_comp = g_spp %>% select(contains('component')) %>% pull() %>% unique() %>% length(); n_comp
        
        g_full = g %>% activate(nodes) %>% 
                filter(!is.na(get(paste0("components",threshold)))) %>% 
                select(name, paste0('components',threshold)) %>% 
                activate(edges) %>% select(from, to, Cs) %>%  filter(Cs >= threshold) %>% 
                activate(nodes) %>% arrange(get(paste0('components',threshold)), name)
        
        # right join to preserve sf attributes in tibble (the left object is spatial - the result inherits its properties)
        g_map1 = right_join( map, g_spp, by = c('sp' = 'name')) %>% 
                select(component = paste0('components',threshold), everything())}  #
# total components (chorotypes)
print('all_components'); all_components = g_full %>% activate(nodes) %>% select(comps = contains('components')) %>% arrange(comps) %>% pull() %>% unique(); all_components
if (F) {g_spp %>% group_by(component = get(paste0("components",threshold))) %>% summarise(spp = paste(name, collapse = ", "), n_spp = n()) %>% 
        mutate(threshold = threshold)  %>% View()} else {g_spp %>% group_by(component = get(paste0("components",threshold))) %>% summarise(spp = paste(name, collapse = ", "), n_spp = n()) %>% mutate(threshold = threshold)}
#### LIST OF CHOROTYPES to be show ####
#list_chorotypes = scan('clipboard')[]
list_chorotypes = c(2:3, 5,6,8,9,10,12,13)#17,20,21,25,32,35)#c(3:6,8,9,11:16)# c(1:36)#c(6,8,9,11:23,25,28,29,31:35); # seq(1,31); #seq(17,31,1) #seq(2, 16, 2) #seq(1,15, by = 2)# c(15:25) ;# c(1:15) 
# cooking graph and map objects # 2nd main routine 
{
# partial g_sub
if(isTRUE(select_chorotypes)){ g_sub = g_full %>% activate(nodes) %>% filter(get(paste0('components', threshold))  %in% list_chorotypes); ind = ''
        } else {if(part == 1) {g_sub = g_full %>% activate(nodes) %>% filter(get(paste0('components', threshold))  < cut); ind = 'A'
        } else {g_sub = g_full %>% activate(nodes) %>% filter(get(paste0('components', threshold))  >= cut); ind = 'B'}}
{n_comp = g_sub %>% activate(nodes) %>% select(contains('components')) %>% pull() %>% unique() %>% length()
        comps = g_sub %>% activate(nodes) %>% select(contains('components')) %>% pull() %>% unique() %>% paste0(.,collapse = ", ")
        print('components ready to  print')
        print(comps)} #   n_comp
# get only selected species to the map 
{g_spp = g_sub %>% activate(nodes) %>% as_tibble()
        # right join to preserve sf attributes in tibble (the left object is spatial - the result inherits its properties)
        g_map1 = right_join( map, g_spp, by = c('sp' = 'name')) %>% 
                select(component = paste0('components',threshold), everything())
} #  g_spp
#  CHECK both map and graph species and components
g_map1 %>% st_drop_geometry() %>% group_by(component) %>% summarise(spp = paste0(sp, collapse = ', '))
g_sub %>% activate(nodes) %>% select(name, comp = paste0('components', threshold)) %>% as_tibble() %>% 
        group_by(comp) %>% summarise(spp = paste(name, collapse = ', '))
# graph setups
{lay = create_layout(g_sub, layout = layout)# 
# lou <- cluster_louvain(g_sub)
} # settings
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
        
        
}#2nd main 
if(pg) {dev.set(device1); basic_graph2}
if(pm) {dev.set(device2); basic_map4}
"threshold"; threshold;'species by component'; g_spp %>% group_by(component = get(paste0("components",threshold))) %>% 
        summarise(spp = paste(name, collapse = ", "), n_spp = n()) %>% mutate(threshold = threshold) #%>% View()# %>% write.table(.,'clipboard')#

if(FALSE){save_map_components = function(...) st_write(g_map1, paste0("map2_Ct",threshold,"_",paste0(list_chorotypes,collapse = '_'),".shp"), driver = "ESRI shapefile")}
