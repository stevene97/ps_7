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
                     selected = 'NAFTA'),
         
         textOutput('caption')
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
       layout(title = "Polling Errors and Questions",
              xaxis = list(title = '% of Respondents who support NAFTA'), 
              yaxis = list(title = 'Error in predicting Democratic Advantage'),
              subtitle = list(title = 'Weak positive correlation'))}
     
     else if(input$question == 'Tariffs'){
       df %>% 
         select(v_9, error, tariff) %>% 
         mutate('newname' = paste(str_to_upper(substr(v_9, 1, 2)), "-", substr(v_9, 3, 4), sep='')) %>%
         filter(!is.na(tariff)) %>% 
         plot_ly(x = ~tariff, y = ~error, 
                 text = ~newname,
                 color = ~newname) %>% 
         layout(xaxis = list(title = '% of Respondents who Support Tariffs'), 
                yaxis = list(title = 'Error in predicting Democratic Advantage'))}
     
     else if(input$question == 'Tax Reform'){
       df %>% 
         select(v_9, error, taxreform) %>% 
         mutate('newname' = paste(str_to_upper(substr(v_9, 1, 2)), "-", substr(v_9, 3, 4), sep='')) %>%
         filter(!is.na(taxreform)) %>% 
         plot_ly(x = ~taxreform, y = ~error, 
                 text = ~newname,
                 color = ~newname) %>% 
         layout(xaxis = list(title = '% of Respondents who support Trump\'s tax reform'), 
                yaxis = list(title = 'Error in predicting Democratic Advantage'))}
     
     else if(input$question == 'Trump Economy'){
       df %>% 
         select(v_9, error, trumpecon) %>% 
         mutate('newname' = paste(str_to_upper(substr(v_9, 1, 2)), "-", substr(v_9, 3, 4), sep='')) %>%
         filter(!is.na(trumpecon)) %>% 
         plot_ly(x = ~trumpecon, y = ~error, 
                 text = ~newname,
                 color = ~newname) %>% 
         layout(xaxis = list(title = '% who believe that Trump\'s economic policies are beneficial'), 
                yaxis = list(title = 'Error in predicting Democratic Advantage'))}
     
     else if(input$question == 'Single Payer Healthcare'){
       df %>% 
         select(v_9, error, singlepay) %>% 
         mutate('newname' = paste(str_to_upper(substr(v_9, 1, 2)), "-", substr(v_9, 3, 4), sep='')) %>%
         filter(!is.na(singlepay)) %>% 
         plot_ly(x = ~singlepay, y = ~error, 
                 text = ~newname,
                 color = ~newname) %>% 
         layout(xaxis = list(title = '% who believe in single-payer healthcare'), 
                yaxis = list(title = 'Error in predicting Democratic Advantage'))}
   })
   
   output$caption <- renderText({
     if(input$question == 'NAFTA'){
       "Positive correlation (excluding the outlier, CA-49) between the percent of 
       people who support NAFTA and the error in predicting Democratic Advantages."
     }
     
     else if(input$question == 'Tariffs'){
       "Positive correlation between the percent of 
       people who support tariffs on steel and aluminum imposed by President Trump
       and the error in predicting Democratic Advantages."
     }
     
     else if(input$question == 'Tax Reform'){
       "Positive correlation between the percent of 
       people who support President Trump's tax reform bill and the 
       error in predicting Democratic Advantages."
     }
     
     else if(input$question == 'Trump Economy'){
       "Positive correlation between the percent of 
       people who agree that President Trump's policies have made their family's economic situation better
       and the error in predicting Democratic Advantages."
     }
     
     else if(input$question == 'Single Payer Healthcare'){
       "Negative correlation between the percent of 
       people who support single-payer healthcare and the error in predicting Democratic Advantages."
     }
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

