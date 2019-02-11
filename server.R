


shinyServer(function(input, output) {
   
  output$bknd <- renderUI({
    HTML("Phthalates are industrial chemicals found in many plastic consumer products, personal care products, and food packaging and processing materials.  As a result, phthalates can be absorbed through the skin, ingested, or inhaled.  Exposure to these environmental chemicals are rapidly metabolized and excreted from the human body.  Measures of the urine concentration of phthalate metabolites indirectly shed light on the potential amount of exposure for phthalates on a regular basis.",
         "<p><p>", "Consequently, research has attributed a connection between phthalate metabolites, obesity, and reproductive and developmental disease and disorders.  In addition, age, race and ethnicity have also shown a considerable difference in phthalate metabolites. The focus of this exploration is to evaluate the urinary concentration of phthalate monesters and research.",
         "<p><p>" , "The dataset for this exploration was collected from the National Health and Nutrition Examination Survey (NHANES).",
         "<p><p>", "Considerations: (1) Only individuals 18 and older were evaluated, (2) Data was evaluated from the years 2015-2016, (3) Basic demographics and common medical conditions were investigated."
    )
  })
  
  
  
  
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
                                             summarise('Mean Phthalate Metabolite (ng/mL)'=signif(mean(Total_Pht, na.rm = TRUE))))
                                             
  
  output$model_phth <- renderPlot({
    
    fit <- lm(formula=Total_Pht ~ Age, data = phth1 )
    
    augment(fit)%>%
      ggplot(mapping = aes(x=Age)) +
      geom_point(mapping = aes(y=Total_Pht))+
      geom_line(mapping = aes(y=.fitted, colour="Best-Fit"), color = "red")+
      ggtitle(   "Comparison of Total Phthalate Metabolite Concentration as a Function of Age of Participants")+
      theme(axis.text.x = element_text(size = 18))+
      theme(plot.title = element_text(size=17))+
      theme(axis.text.y = element_text(size = 18))+
      theme(axis.title.x = element_text(size=20))+
      theme(axis.title.y = element_text(size=18))#change y for a logarithmic scale
  })
    
    output$take <- renderUI({
      HTML("There appears to be a difference in the average phthalate concentration for various populations. Men, individuals over 65 years of age, and Non-Hispanic Black individuals appeared to have a higher amount of phthalate urinary concentration.  ",
           "<p><p>", "In addition, there were 11 individuals in the study that had an exponentially higher amount of phthalate metabolite concentration.  If these outliers were removed, similar results would still reveal a slightly higher amount of phthalate concentration for men and a considerably higher concentration for Non-Hispanic Blacks.  Nevertheless, individuals that were 65 years of age and older and individuals that were between 30 and 49 in age had about the same average phthalate concentration.",
           "<p><p>" , "Further research should be conducted to further investigate the suspected concerns presented in this exploration.",
           "<p><p><p><h2>", "References & Resources", "<p>",
           "<p><p><h6>", "CBS Interactive Inc. (2019, Jan. 3). 'Sicker, Fatter, Poorer': The cost of hormone-disrupting chemicals. Retrieved from https://www.cbsnews.com/news/sicker-fatter-poorer-the-cost-of-hormone-disrupting-chemicals/ ",
           "<p><p>", "Kohn, M. C., Parham, F., Masten, S. A., Portier, C. J., Shelby, M. D., Brock, J. W., & Needham, L. L. (2000). Human exposure estimates for phthalates. Environmental health perspectives, 108(10), A440-2.",
           "<p><p>", "Ingredients (2018, Feb. 22), Retrieved from https://www.fda.gov/cosmetics/productsingredients/ingredients/ucm128250.htm#pht",
           "<p><p>", "Phthalates (2017, May 31). Tox Town - US National Library of Medicine. Retrieved at https://toxtown.nlm.nih.gov/chemicals-and-contaminants/phthalates"
      )
    })
    
})
  
    