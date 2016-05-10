library(shiny)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  titlePanel("Campaign Performance Monitor"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("update", "Run Query"),
      hr(),
      h2("Income"),
      sliderInput(inputId = "consultores", label = h3("#Consultores"), min = 5, max = 30, value = 10),
      radioButtons(inputId = "utilizacion", label = "Utilizacion",
                   choices = list("80%" = 0.8, "90%" = 0.9, "100%" = 1), 
                   selected = 0.8),
      numericInput(inputId="rate", label ="Rate/Hr", value = "100", min = 80, max = 250, step = 10),
      hr(),
      h2("Costs"),
      numericInput(inputId="server", label ="Server", value = 2000, min = 1000, max = 2500, step = 100),
      numericInput(inputId="computer", label ="Computer", value = 1500, min = 1000, max = 2500, step = 100),
      numericInput(inputId="training", label ="Training", value = 470, min = 300, max = 1500, step = 10),
      numericInput(inputId="software", label ="Software", value = 1000, min = 800, max = 2500, step = 100)
    ),
    
    ##1000-5000
          
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("pieDetail"),
      plotOutput("pieSummary")
    )
  )
))