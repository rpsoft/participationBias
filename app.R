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
   #includeCSS("styles.css"),
  
   
   titlePanel("Non-participation bias example"),
   h3("Mobile Phone Use and Risk of Uveal Melanoma: Results of the Risk Factors for Uveal Melanoma Case-Control Study"),
   h6("Stang, A. et al. (2009) Journal of the National Cancer Institute, https://doi.org/10.1093/jnci/djn441
    and 
    Lash, T.L, Fox, M.P, and Fink, A.K. (2009) Applying Quantitative Bias Analysis to Epidemiologic Data)"), 

  p("The study by Stang et al. (2009) reported on a case-control study investigating the association between mobile phone use and risk of uveal melanoma. 
      Cases for the study were recruited from a referral centre, and three types of controls (population, siblings and opthalmologists).", 
       br(),
       br(),
      "We're going to focus on the analysis using the population controls. 

      486 Cases were identified as being eligable for the study, and 459 went on to participate (94%). 
      Matched controls were selected from the population, and 55% agreed to participate. 
      The questionnaire asked participants to rate mobile phone use as 'Never', 'Sporodic' and 'Regular' (amongst other questions)",
    br(),
    br()),
   
   navlistPanel(
     "Status",
     tabPanel("Participants",
              p("Using only data from the participants of the study, we compare those who report regular use and those who report never using a mobile phone"),
              DT::dataTableOutput('table1'),
              textOutput("fisher1"),
              p("The Odds Ratio is < 1. This suggests that regular use of a mobile phone", em("decreases"), "the odds of developing of uveal melanoma. 
                The 95% CI does not include the null value of 1, incating evidence of a significant association",
                br(),
                br(),
                "Now, consider the differences in rates of participation between the cases (94%) and the controls (55%). This suggests there may be a non-participation bias")
     ),
     tabPanel("Non-participants",
              p("Those who declined to participate in the larger study were recontacted and asked if they would answer a shorter question.", 
                br(),
                 "10 of the 27 non-participating cases and 284 of the 663 non-participating controls completed this questionnaire. 
                 Only two categories were included: Regular use or no use"),
              DT::dataTableOutput('table2'),
              textOutput("fisher2"),
              p(br(), "Using the data from the non-participants, the odds ratio is now > 1, suggetsing that odds of uveal melanoma are", em("increased"), "with regular Use of a mobile phone
                The 95% confidence interval includes the null value of 1, therefore, the increase is not statistically significant.",
                br(),
                br(),
                "Let's look at what the results are if we combine the data from participants and non-participants.")
              
     ),
     tabPanel("Participants & Non-participants",
              p("We can combine the data for participants and non-participants:"),
              DT::dataTableOutput('table3'),
              textOutput("fisher3"),
              br(),
              p("The 95% confidence interval includes the null value of 1, therefore, in the full* sample, there is no increased odds in being diagnosed with uveal melanoma if you're a regular mobile phone user, compared to a non-user.",
                br(),
                br(),
                "* Note that this isn't really the full sample - only some of the non-participants agreed to complete the smaller questionnaire")
              
     )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$table1 <- DT::renderDataTable({ DT::datatable(stang09_p, rownames=TRUE,  filter='none', options = list(dom = 't'))})
    output$fisher1 <- renderText( paste0("Odds Ratio = ", f1r, "   95%CI = (", f1ll, ", ", f1ul, ")" ))
  
    output$table2 <- DT::renderDataTable({ DT::datatable(stang09_np_short, rownames=TRUE,  filter='none', options = list(dom = 't'))})
    output$fisher2 <- renderText( paste0("Odds Ratio = ", f2r, "   95%CI = (", f2ll, ", ", f2ul, ")" ))
    
    output$table3 <- DT::renderDataTable({ DT::datatable(stang09_full, rownames=TRUE,  filter='none', options = list(dom = 't'))})
    output$fisher3 <- renderText( paste0("Odds Ratio = ", ffr, "   95%CI = (", ffll, ", ", fful, ")" ))
    
}
# Run the application 
shinyApp(ui = ui, server = server)

