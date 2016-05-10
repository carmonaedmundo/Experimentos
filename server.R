library(shiny)
library(ggplot2)

shinyServer(function(input, output) {

  dataP <- reactive({
        consultores <- input$consultores
        utilizacion <- as.numeric(input$utilizacion)
        rate <- input$rate
        
        server  <- input$server
        software  <- input$software
        training<- input$training
        computer  <- input$computer
        # 
        # consultores <- 10
        # utilizacion <- .8
        # rate <- 100
        # 
        # server  <- 1500
        # software  <- 100
        # training<- 470
        # computer  <- 1500
        
        billngHours <- (253*9)* utilizacion #Dias laborales * 40 Hras * utilizacion
        revenueConsultant <- billngHours *rate 
        totalAnualRevenue <- revenueConsultant * consultores
        totalCost <- (software+training+(computer/2))*consultores + server
        df <- data.frame(
          variable = c("Revenue", "Cost"),
          value = c(totalAnualRevenue, totalCost)
        )
        dfC <- data.frame(
          variable = c("server", "software","training","computer"),
          value = c(server, software*consultores,training*consultores,computer*consultores)
        )
        
        pie<-ggplot(df, aes(x = "", y = value, fill = variable)) +
          geom_bar(width = 1, stat = "identity") +
          coord_polar("y", start = pi / 3) +
          labs(title = "Deloitte")
        plot2 <- ggplot(dfC, aes(x="", y = value, fill = variable)) + geom_bar(stat = "identity")
        lista <- list(pie,plot2)

  })
  

  output$pieSummary <- renderPlot({
    dataP()[[1]]
  })
  
  output$pieDetail <- renderPlot({
    dataP()[[2]]
  })
  
})