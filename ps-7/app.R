library(shiny)

df <- read.csv('merged_thing.csv') %>% 
  select(-X)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Polling Errors and Question Responses"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = "question",
                     label = "Select a Polling Topic:",
                     choices = c('NAFTA', 'Tariffs', 'Tax Reform', 'Trump Economy', 'Single Payer Healthcare'),
                     selected = 'NAFTA')
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlotly({
     if(input$question == 'NAFTA'){
     df %>% 
       select(v_9, error, nafta) %>% 
       mutate('newname' = paste(str_to_upper(substr(v_9, 1, 2)), "-", substr(v_9, 3, 4), sep='')) %>%
       filter(!is.na(nafta)) %>% 
       plot_ly(x = ~nafta, y = ~error, 
               text = ~newname,
               color = ~newname) %>% 
       layout(xaxis = list(title = '% Support/Agree with Statement'), 
              yaxis = list(title = 'Error in predicting Democratic Advantage'))}
     
     else if(input$question == 'Tariffs'){
       df %>% 
         select(v_9, error, tariff) %>% 
         mutate('newname' = paste(str_to_upper(substr(v_9, 1, 2)), "-", substr(v_9, 3, 4), sep='')) %>%
         filter(!is.na(tariff)) %>% 
         plot_ly(x = ~tariff, y = ~error, 
                 text = ~newname,
                 color = ~newname) %>% 
         layout(xaxis = list(title = '% Support/Agree with Statement'), 
                yaxis = list(title = 'Error in predicting Democratic Advantage'))}
     
     else if(input$question == 'Tax Reform'){
       df %>% 
         select(v_9, error, taxreform) %>% 
         mutate('newname' = paste(str_to_upper(substr(v_9, 1, 2)), "-", substr(v_9, 3, 4), sep='')) %>%
         filter(!is.na(taxreform)) %>% 
         plot_ly(x = ~taxreform, y = ~error, 
                 text = ~newname,
                 color = ~newname) %>% 
         layout(xaxis = list(title = '% Support/Agree with Statement'), 
                yaxis = list(title = 'Error in predicting Democratic Advantage'))}
     
     else if(input$question == 'Trump Economy'){
       df %>% 
         select(v_9, error, trumpecon) %>% 
         mutate('newname' = paste(str_to_upper(substr(v_9, 1, 2)), "-", substr(v_9, 3, 4), sep='')) %>%
         filter(!is.na(trumpecon)) %>% 
         plot_ly(x = ~trumpecon, y = ~error, 
                 text = ~newname,
                 color = ~newname) %>% 
         layout(xaxis = list(title = '% Support/Agree with Statement'), 
                yaxis = list(title = 'Error in predicting Democratic Advantage'))}
     
     else if(input$question == 'Single Payer Healthcare'){
       df %>% 
         select(v_9, error, singlepay) %>% 
         mutate('newname' = paste(str_to_upper(substr(v_9, 1, 2)), "-", substr(v_9, 3, 4), sep='')) %>%
         filter(!is.na(singlepay)) %>% 
         plot_ly(x = ~singlepay, y = ~error, 
                 text = ~newname,
                 color = ~newname) %>% 
         layout(xaxis = list(title = '% Support/Agree with Statement'), 
                yaxis = list(title = 'Error in predicting Democratic Advantage'))}
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

