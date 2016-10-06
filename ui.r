# ui.R

shinyUI(fluidPage(
  titlePanel("Data Science Capstone Project"),
  
  fluidRow(
    column(12,
           br(),
           h4("Please type a phrase in the box below."),
           br(),
           br(),
           h4("You will see the predicted next word of your phrase."),
           br(),
           br()
    )
  ),
  
  fluidRow(
    column(12,
           textInput("input_str", 
                     label = "Enter your word here:", 
                     value = " "
           )             
    )    
  ),
  

  fluidRow(
    column(12,
           br(),
           h4("Predicted next word:"), 
           verbatimTextOutput("text2")            
    )
  )
))