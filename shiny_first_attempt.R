{
    ui <- fluidPage( shinytheme("cosmo"),  
                     
                     titlePanel('SCAN'),
                     
                     sidebarPanel(
                         
                         # tags$h3("SCAN parameters"),
                         
                         selectInput(inputId = "g", label = "choose a SCAN list with a graph:",  choices = ls()[grep(ls(), pattern = "bird")] %>% sort(decreasing =  T)),
                         
                         sliderInput(inputId = "threshold", label = "threshold", value = 0.91, min = 0.25, max = 0.97, step = 0.02),
                         
                         # tags$h3("graph & map parameters"),
                         # 
                         # selectInput("layout", "graph layout", choices = c("fr", "kk", "db", "mds", "ldl", "gem")), # 'fr') # 'dh') # 'mds')#"drl") # other options kk, fr, ldl, gem
                         # 
                         selectInput(inputId = "palette", label = "choose palette",
                                     choices = c("Accent","Spectral","Divergent","Greens", "Reds","BrBG" , "RdYlGn","PiYG")),
                         
                         #checkboxInput("highlight_components", "highlight components", TRUE),
                         
                         # selectInput("map", "shapefile with species distributions", choices = ls()[grep(ls(), pattern = "map")]),
                         
                         #checkboxInput("plot_sa", "plot South America line", TRUE),  # later use global lines   
                         #checkboxInput("rivers", "plot rivers", TRUE), # later use global rivers
                         
                         numericInput("map_alpha", "map alpha", max = 1, min = 0.01, value = 0.2)
                     ),    
                     
                     mainPanel(
                         
                         # tags$h3("outputs"),
                         
                         #textOutput("original_components"),
                         
                         uiOutput("original_components"),
                         
                         dataTableOutput("g_sub_table"),
                         
                         #plotOutput("graph_plot"),
                         
                         leafletOutput("map_plot")
                     ),
    )
}
server <- function(input,output,session){
    
    threshold <- reactive({   input$threshold   })
    
    g_full <- reactive({    
        
        g_full <-get(input$g) %>% .[['graph']] %>% activate(edges) %>% select(from, to, Cs) %>%  filter(Cs >= threshold()) %>% 
            activate(nodes) %>% filter(!is.na(get(paste0("components",threshold())))) %>% 
            select(name, comps = paste0('components',threshold())) %>% arrange(comps, name)
        
        g_full
        
    })
    
    original_components <- reactive({
        
        g_full() %>% activate(nodes) %>%  select(comps) %>%
            arrange(comps) %>% pull() %>% unique()
        })
    
    
    output$original_components <- renderUI({
        
        components<- original_components()
        
        checkboxGroupInput("selected_components", "Choose Components", components, inline = TRUE)#, selected = components ) #try ifelse later with a reactive check eventReactive(input$check_all, { to select all
        })
    
    g_sub <- reactive({
        g_sub <- g_full() %>% activate(nodes) %>%
            filter(comps %in% input$selected_components)
        
        g_sub
        })
    
    output$g_sub_table <- renderDataTable({  g_sub() |> activate(nodes) |> as_tibble() |> 
            group_by(comps) |> summarise(n_spp = n(), species = paste0(name, collapse = ','))
    })
    
    output$graph_plot <- renderPlot({   
        
        g_sub() |> plot()
    })
    
    ## renderLeaflet ##
    #_____________________________________________ 
    output$map_plot <- renderLeaflet({
        
        g_spp <- g_sub() |> activate(nodes) |> as_tibble()
        
        g_map1 <- right_join( map, g_spp, by = c('sp' = 'name')) %>% select(comps, everything())
        
        # create leaflet
        
        g_map_4326 <- g_map1 |> st_transform(crs = 4326)
        
        # setup
        pal <- colorBin(input$palette, domain = original_components(), bins = 7)
        
        # labels <- g_map_4326$comps |>  lapply(htmltools::HTML)# sprintf("%s %s", g_map_4326$comps, g_map_4326$sp) %>% lapply(htmltools::HTML)   # %s use the first 'string'
        
        # leaflet
        g_map_4326[] |> leaflet() |> addTiles() |>
            
            addPolygons(
                fillColor = ~pal(comps),
                color = "black", dashArray = "1",
                label = comps,
                fillOpacity = input$map_alpha)
        
    })
    #_____________________________________________
}

shinyApp(ui,server) 
