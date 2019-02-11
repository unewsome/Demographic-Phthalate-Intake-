library(shinythemes)
library(shiny)
library(shinydashboard)


shinyUI(fluidPage(theme = shinytheme("united"),
  
                  
titlePanel("Exploring Phthalate Metabolite Concentration for Various Populations"),


navlistPanel(
  "
  Evaluate Variables",
  
  helpText("Select a variable to investigate the phthalate metabolite."),
  
  tabPanel('Background Info', box(htmlOutput("bknd"), title = "Background Information", footer = "See 'Take Aways' Tab for References", width = 10)
         
             ),
  
  tabPanel("Single Variable", box(width = 2, radioButtons(inputId = "f1", label = "Variables",  choices = factors, selected = NULL)), box(width = 10, plotOutput("phthPlot", height = 600, width = 600))),
  
  tabPanel("Multiple Variables", fluidRow(box(width = 4, radioButtons(inputId = "f2", label = "Variables",  choices = factors)), box(width = 4, radioButtons(inputId = "f3", label = "Variables",  choices = factors)), box(width = 4, radioButtons(inputId = "f4", label = "Variables",  choices = factors))),
                                 fluidRow(box(width = 11, DT::dataTableOutput("multiTable")))
                                 ),
  
  tabPanel("Modeling Phthalate Metabolite Concentration and Age", box(width = 12, plotOutput("model_phth", height = 800, width = 800))),
  
  tabPanel("Take Aways", box(htmlOutput("take"), title = "Take Aways", width = 10)
           
  ),
    
      tags$style(type="text/css", #Code suppresses error message when no input is selected
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }")




)))
