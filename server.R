


shinyServer(function(input, output) {
   
  output$phthPlot <- renderPlot({
    plot_data <- phth1%>%
      group_by_(input$f1)%>%
      summarise(Mean_Pht=mean(Total_Pht, na.rm = TRUE))
    
    total_mean <- mean(phth1$Total_Pht, na.rm = TRUE)
    
    ggplot(data=plot_data, aes_string(x=input$f1,y="Mean_Pht"))+geom_col(fill = "navy")+
      xlab(paste("Incidence of ", input$f1))+ylab('Mean Phthalate Metabolite in Urine (ng/mL')+
      ggtitle(paste(                   "Comparison of Phthalate Metabolite Excretion by", input$f1))+
      geom_hline(yintercept = total_mean, show.legend = TRUE, color = "red")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18))+
      theme(plot.title = element_text(size=20))+
      theme(axis.text.y = element_text(size = 18))+
      theme(axis.title.x = element_text(size=20))+
      theme(axis.title.y = element_text(size=20))
      
  })
  
  output$multiTable <- DT::renderDataTable(phth1%>%
                                             group_by_(input$f2, input$f3, input$f4)%>%
                                             summarise(Mean_Pht=mean(Total_Pht, na.rm = TRUE)))
}) 
  



