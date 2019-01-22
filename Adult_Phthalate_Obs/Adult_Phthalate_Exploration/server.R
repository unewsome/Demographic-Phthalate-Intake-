
library(shiny)


shinyServer(function(input, output) {
   
  data<-eventReactive(input$go, {phth1%>%
    group_by(input$f1,input$f2,input$f3)%>%
    summarise(mean_pht=mean(`Total Pht`,na.rm=TRUE))
    mutate(Diagnosis= case_when (
      input$f1=="Gender" & input$f2="Blank" & input$f3="Blank"~c("Female","Male"),
      input$f1=="Race/Ethnicity" & input$f2="Blank" & input$f3="Blank"~c("Hispanic","Mexican American","Non-Hispanic Asian","Non-Hispanic Black","Non-Hispanic Multi-racial","Non-Hispanic White"),
      input$f1=="Age" & input$f2="Blank" & input$f3="Blank"~Diagnosis,
      input$f1=="Cancer?" & input$f2=="Blank"& input$f3=="Blank"~c("Yes", "No", "Unknown"),
      input$f1=="Thyroid Issue?" & input$f2=="Blank" & input$f3=="Blank"~c("Yes", "No", "Unknown"),
      input$f1=="Diabetes?"& input$f2=="Blank" & input$f3=="Blank"~c("Yes", "No", "Borderline", "Unknown")
      input$f1=="Gender"& input$f2=="Age" & input$f3=="Blank"~
      input$f1=="Gender"& input$f2=="Race/Ethnicity" & input$f3=="Blank"
      input$f1=="Gender"& input$f2=="Cancer?" & input$f3=="Blank"~
      input$f1=="Gender"& input$f2=="Thyroid Issues?" & input$f3=="Blank"~
      input$f1=="Gender"& input$f2=="Diabetes?" & input$f3=="Blank"
      input$f1=="Gender"& input$f2=="Age" & input$f3=="Blank"~c("Female 18-29 yoa", "Male 18-29 yoa", "Female 30-49 yoa", "Male 30-49 yoa", "Female 50-65 yoa", "Male 50-65 yoa","Female 65 and up", "Male 65 and up yoa"),
      input$f1=="Gender"& input$f2=="Race/Ethnicity" & input$f3=="Blank"~c("Hispanic Male","Hispanic Female","Mexican American Female","Mexican American Male", "Non-Hispanic Asian Female","Non-Hispanic Asian Male","Non-Hispanic Black Female", "Non-Hispanic Black Male","Non-Hispanic Multi-racial Female", "Non-Hispanic Multi-racial Male","Non-Hispanic White Female", "Non-Hispanic White Male"),
      input$f1=="Gender"& input$f2=="Cancer?" & input$f3=="Blank"~c("Female-Positive Diagnosis","Male-Positive Diagnosis", "Female-Negative Diagnosis", "Male-Negative Diagnosis","Female-Unknown Diagnosis","Male-Unknown Diagnosis"),
      input$f1=="Gender"& input$f2=="Thyroid Issues?" & input$f3=="Blank"~c("Female-Positive Diagnosis","Male-Positive Diagnosis", "Female-Negative Diagnosis", "Male-Negative Diagnosis","Female-Unknown Diagnosis","Male-Unknown Diagnosis"),
      input$f1=="Gender"& input$f2=="Diabetes" & input$f3=="Blank"~c("Female-Positive Diagnosis","Male-Positive Diagnosis", "Female-Negative Diagnosis", "Male-Negative Diagnosis","Female-Borderline Diagnosis","Male-Borderline Diagnosis", "Female-Unknown Diagnosis","Male-Unknown Diagnosis"),
      input$f1=="Age"& input$f2=="Gender" & input$f3=="Blank"~c("Female 18-29 yoa", "Male 18-29 yoa", "Female 30-49 yoa", "Male 30-49 yoa", "Female 50-65 yoa", "Male 50-65 yoa","Female 65 and up", "Male 65 and up yoa")
      input$f1=="Age"& input$f2=="Race/Ethnicity" & input$f3=="Blank"~
      input$f1=="Age"& input$f2=="Cancer?" & input$f3=="Blank"~c("Positive Diagnosis 18-29 yoa","Negative Diagnosis 18-29 yoa", "Unknown Diagnosis 18-29 yoa","Positive Diagnosis 30-49 yoa","Negative Diagnosis 30-49 yoa", "Unknown Diagnosis 30-49 yoa", "Positive Diagnosis 50-65 yoa","Negative Diagnosis 50-65 yoa", "Unknown Diagnosis 50-65 yoa","Positive Diagnosis 65 and up yoa","Negative Diagnosis 65 and up yoa", "Unknown Diagnosis 65 and up yoa"),
      input$f1=="Age"& input$f2=="Thyroid Issues?" & input$f3=="Blank"~c("Positive Diagnosis 18-29 yoa","Negative Diagnosis 18-29 yoa", "Unknown Diagnosis 18-29 yoa","Positive Diagnosis 30-49 yoa","Negative Diagnosis 30-49 yoa", "Unknown Diagnosis 30-49 yoa", "Positive Diagnosis 50-65 yoa","Negative Diagnosis 50-65 yoa", "Unknown Diagnosis 50-65 yoa","Positive Diagnosis 65 and up yoa","Negative Diagnosis 65 and up yoa", "Unknown Diagnosis 65 and up yoa"),
      input$f1=="Age"& input$f2=="Diabetes?" & input$f3=="Blank"~c("Positive Diagnosis 18-29 yoa","Negative Diagnosis 18-29 yoa","Borderline Diagnosis 18-29 yoa" ,"Unknown Diagnosis 18-29 yoa","Positive Diagnosis 30-49 yoa","Negative Diagnosis 30-49 yoa", "Borderline Diagnosis 30-49 yoa","Unknown Diagnosis 30-49 yoa", "Positive Diagnosis 50-65 yoa","Negative Diagnosis 50-65 yoa","Borderline Diagnosis 50-65 yoa", "Unknown Diagnosis 50-65 yoa","Positive Diagnosis 65 and up yoa","Negative Diagnosis 65 and up yoa", "Borderline diagnosis 65 and up yoa", "Unknown Diagnosis 65 and up yoa" )

      # input$f1=="Gender"& input$f2=="Race/Ethnicity" & input$f3=="Blank"
    ))})
      
    
  output$phthPlot <- renderPlot({
    ggplot(data(),aes(x=Diagnosis, y=mean_pht))+
      geom_col()+xlab("Incidence of " )+ylab('Mean Phthalate Metabolite in Urine (ng/mL')+
      ggtitle(       "Comparison of Phthalate Metabolite Excretion by " )
    
  #Change colors of graph
  
  #Perform z or t-test for significance for yes mean and no mean
 
  # output$significance<- case_when(
  #   _____   ~ "According to the ____ test, the difference in mean between ____ and _____ is statistically significant."
  #   
  # )
 
  })
  
})
