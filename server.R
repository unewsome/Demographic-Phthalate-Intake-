


shinyServer(function(input, output) {
   
  output$phthPlot <- renderPlot({
    plot_data <- phth1%>%
      group_by_(input$f1)%>%
      summarise(Mean_Pht=mean(Total_Pht, na.rm = TRUE))
    
    ggplot(plot_data(), aes_string(x=input$f1,y=Mean_Pht), fill = "blue")+geom_col()+
      xlab(paste("Incidence of ", input$f1))+ylab('Mean Phthalate Metabolite in Urine (ng/mL')+
      ggtitle(paste(                   "Comparison of Phthalate Metabolite Excretion by", input$f1))
      
  })
  
  output$multiTable <- DT::renderDataTable(phth1%>%
                                             group_by_(input$f1, input$f2, input$f3)%>%
                                             summarise(Mean_Pht=mean(Total_Pht, na.rm = TRUE)))
  

})
