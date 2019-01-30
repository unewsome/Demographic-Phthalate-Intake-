


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
      theme(axis.title.y = element_text(size=18))
      
  })
  
  output$multiTable <- DT::renderDataTable(phth1%>%
                                             group_by_(input$f2, input$f3, input$f4)%>%
                                             summarise(Mean_Pht=mean(Total_Pht, na.rm = TRUE)))
                                             
  
  
  
  output$Significance <- renderText ({
    
    # see <- phth1%>%
    #   filter(Age_Class==input$f5 | Race_Ethnicity==input$f5 | Cancer == input$f5 | Diabetes == input$f5)
    
    # t <- t.test(see$Total_Pht,phth1$Total_Pht)
    # 
    # tstat <- t[["statistic"]]
    # 
    # tp <- t[['p-value']]
    # 
    # print(t)
    
    paste("The average phthalate metabolite found for the selected representative sample (" , input$f5 ," ) is compared to the opposite condition. For example, a positive diagnosis will be compared to a negative diagnosis.  Note the p-value")
    
    # paste("t = ", tstat)
    # 
    # paste("p-value =", tp)
    
   })
  
  
  output$t <- renderInfoBox({
    
    see <- phth1%>%
      filter(Age_Class==input$f5 | Race_Ethnicity==input$f5 | Gender == input$f5 | Diabetes == input$f5)
    
    t <- t.test(see$Total_Pht,phth1$Total_Pht)
    
    tstat <- t[["statistic"]]
    
    tp <- t[['p-value']]
    
    infoBox("t statistic =", tstat, icon = )
    
    infoBox("p-value = ", tp, icon =)
      

    
    
  })
})
  
    # infoBox(
    #   paste("The average phthalate metabolite found for the selected representative sample (" , input$f5 ," ) is compared to the total population of this dataset."
    #         /n, (" Note the following results of a t-test:" /n)
    #              # "t = " tstat /n,
    #              # "p-value =" tp)
    # )
    # )
    # 
    #  if (tstat > tp) {print("This representative sample is statistically significant from the datapopulation")} 
    # else {print("This representative sample is not statistically significant from the data population.")}})
