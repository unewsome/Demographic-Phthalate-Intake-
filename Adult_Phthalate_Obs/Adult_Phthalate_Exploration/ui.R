library(shiny)


shinyUI(fluidPage(
  
  
  titlePanel("Exploration of Pthalate Exposure Amongst Various Populations"),
  
  
  
  #Side bar equals 3 drop down menus representing factors to alter with execute/update button... delayed response.
  
  sidebarLayout(
    sidebarPanel(  selectInput(inputId="f1",label="Factor 1",factors),
                   selectInput(inputId="f2",label = "Factor 2", factors1),
                   selectInput(inputId="f3",label="Factor 3",factors1),
                   actionButton(inputId="go" ,label="Update")), #enter all information on side bar?))
 
    
    # Show a plot of the generated distribution
    mainPanel(
       
      #Graph with resulting mean value based on factors... graph x & y axes change along with title
      
      plotOutput("phthPlot"),
      textOutput("significance")
    )
  )
))
