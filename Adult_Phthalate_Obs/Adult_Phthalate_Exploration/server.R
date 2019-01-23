library(shiny)
library(dplyr)



shinyServer(function(input, output) {
  
  
output$phthPlot <- renderPlot({
 plot_data<-eventReactive(input$go, {phth1%>%
          group_by_(input$f1)%>%
          summarise(mean_pht=mean(Total_Pht,na.rm=TRUE))
  
   })
    ggplot(plot_data(),aes_string(x=input$f1,y="mean_pht"))+
      geom_col()+xlab(paste("Incidence of ",input$f1)) +ylab('Mean Phthalate Metabolite in Urine (ng/mL')+
      ggtitle(       "Comparison of Phthalate Metabolite Excretion by " )
    
        
    })


})

  #Change colors of graph
  
  #Perform z or t-test for significance for yes mean and no mean
 
  # output$significance<- case_when(
  #   _____   ~ "According to the ____ test, the difference in mean between ____ and _____ is statistically significant."
  #   
  # )
 
 
  

