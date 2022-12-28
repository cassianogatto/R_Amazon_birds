# filter species by name
textInput(inputId = "filter_species", label = "Filter species by name")

# filter species inside reactive map()
if(!is.null(input$filter_species)){map <- map |> filter(! sp %in% input$filter_species)}


library(dplyr)
library(shiny)
library(shinydashboard)
library(sf)
library(tidyr) # crossing, expand
library(stringr) # str_detect
library(tidyr)
library(units)

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
    
    
}



overlaps <- overlaps |> as.data.frame()





overlaps <- overlaps |> pivot_longer(cols = 2:4, names_to = 'sp2')

overlaps |> filter(value)

overlaps_df <- as.data.frame(as.table(overlaps))






                                                     (
    test[test$sp[i] == comb_spp[i,"sp1"]), st_geometry(test[test$sp == comb_spp$sp2[i]]))
}

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
