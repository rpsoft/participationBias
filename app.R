#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)


source("misclassification_bias_2019.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   includeCSS("styles.css"),
  
   # Application title
   titlePanel("Non-differential misclassifcation of exposure"),
          h4("Using only data from the Participants of the study, finding borderline significant value, and a <1 OR, suggesting that regular use is assoc with lower odds of developing melanoma",class="header"),
          dataTableOutput('table1'),
          textOutput("fisher1"),
          hr(),
          h4("Using only the data from the Non-participants, we find an OR > 1, and is not significant.",class="header"),
          dataTableOutput('table2'),
          textOutput("fisher2"),
          hr(),
          h4("Combining both participants and non-participants, we see the 'true' OR for this sample is 1.25.",class="header"),
          dataTableOutput('table3'),
          textOutput("fisher3")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$table1 <- renderDataTable(stang09_p, options = list(paging = FALSE,searching = FALSE) )
    output$fisher1 <- renderText( paste0("Fisher test values : ", f1r) )
  
    output$table2 <- renderDataTable(stang09_np_short, options = list(paging = FALSE,searching = FALSE) )
    output$fisher2 <- renderText( paste0("Fisher test values : ", f2r) )
    
    output$table3 <- renderDataTable(stang09_full, options = list(paging = FALSE,searching = FALSE) )
    output$fisher3 <- renderText( paste0("Fisher test values : ", ffr) )
    
}
# Run the application 
shinyApp(ui = ui, server = server)

