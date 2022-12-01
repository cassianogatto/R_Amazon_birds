
list_SCAN_graphs = c("bird$graph", "SCANlist9791$graph")

ui <- fluidPage(    
        titlePanel('SaCANa'),
         
                sidebarPanel(
                        selectInput(inputId = "g",
                            label = "choose a graph:",
                            choices = list_SCAN_graphs),
                
                        sliderInput(inputId = "threshold", label = "threshold", value = 0.50, min = 0.25, max = 0.97, step = 0.02),
                ),
        mainPanel(    
                textOutput("g"),
                textOutput("threshold"),
                tableOutput("graf")
        )
)

server <- function(input,output,session){
        
        output$g <- renderText( paste('graph', input$g ))
        
        output$threshold <- renderText( input$threshold  )
        
        output$graf <- renderTable(
                get(input$g) %>% activate(nodes) %>% as_tibble() %>% head()# %>%  select(name, contains(paste(input$threshold))) %>% filter(!is.na(paste0("components",input$threshold)))
         )
}

shinyApp(ui, server)
