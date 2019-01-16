#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Exploration of Pthalate Exposure Amongst Various Populations"),
  
  # Sidebar with a slider input for number of bins 
  
  
  #Side bar equals 3 drop down menus representing factors to alter with execute/update button... delayed response.
  
  
  sidebarLayout(
    sidebarPanel(
       sliderInput("bins",
                   "Number of bins:",
                   min = 1,
                   max = 50,
                   value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       
      #Graph with resulting mean value based on factors... graph x & y axes change along with title
      
      
      
      plotOutput("distPlot")
    )
  )
))
