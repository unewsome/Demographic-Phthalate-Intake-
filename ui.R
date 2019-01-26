library(shinythemes)
library(shiny)
library(shinydashboard)


shinyUI(fluidPage(theme = shinytheme("journal"),
  
                  
titlePanel("Exploring Phthalate Metabolite Concentration for Various Populations"),


navlistPanel(
  "Evaluate Variables",
  
  tabPanel("Single Variable", box(width = 2, radioButtons(inputId = "f1", label = "Variables",  choices = factors, selected = NULL)), box(width = 10, plotOutput("phthPlot", height = 600, width = 600))),
  
  tabPanel("Multiple Variables", fluidRow(box(width = 4, radioButtons(inputId = "f2", label = "Variables",  choices = factors)), box(width = 4, radioButtons(inputId = "f3", label = "Variables",  choices = factors)), box(width = 4, radioButtons(inputId = "f4", label = "Variables",  choices = factors))),
                                 fluidRow(box(width = NULL, DT::dataTableOutput("multiTable")))
                                 ),
  tabPanel("Significance", textOutput("SS"))
  


)
)

)









# dashboardPage(
#               dashboardHeader(title = "Pthalate Exposure Amongst Various Populations in 2015-2016",titleWidth = 600),
#               dashboardSidebar(tags$blockquote("Select a variable to investigate:"),
#                     radioButtons(inputId = "f1", label = "Variables",  choices = factors, selected = NULL)),
#               
#               
#               dashboardBody(
#                 fluidRow(title = "Single Variable Phthalate Metabolite Visual Exploration", solidHeader = TRUE,
#                     plotOutput("phthPlot", height = 700, width =  700)))
              
                  # tabPanel("Multiple Variable",  
                  #          box(title = "Multiple Variable Phthalate Metabolite Dataset Exploration", status="success", solidHeader = TRUE,
                  #              DT::dataTableOutput("multiTable", height = 600, width = 600)),
                  #          fluidRow(box(tags$blockquote("In evaluating the average phthalate metabolite concentration, are the representative samples statistically different from the entire population?")))
                           
                           
              #              
              #     )
              #   )
              #   
              # )    







# dashboardSidebar(
#    sidebarMenu(
#      menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
#      menuItem("Background", tabName = "Background"),
#      menuItem("Single Variable", tabName = "Single Variable"),
#      menuItem("Double Variable", tabName = "Double Variable")
#    )
#  ),
# 
# 
# 
#  body <- dashboardBody(
#     tabItems(
#       tabItem(tabName = "Background", "Background Information" )
 
     
 #     fluidRow(box(width = 12, tabItem(tabName = "Background"),title = "Background Information")),
 #     fluidRow(column(width = 3, box(width = NULL, tabItem(tabName = "Single Variable"), title = "Single Variable for Visual Review")),
 #              column(width = 9, box(width = NULL, plotOutput("phthPlot")) 
 #              )),
 #     fluidRow(column(width = 4, box(width = NULL, tabItem(tabName = "Double Variable", 
 #            box(width = 12, DT::dataTableOutput("multiTable"))))
 # )

# checkboxInput(inputId="f1",label="Variable to Consider (choose one):" ,factors),
# checkboxInput(inputId="f1",label="Variable to Consider (choose one):" ,factors),
# checkboxInput(inputId="f2",label="2nd variable to Consider:",factors),#(checkboxInput(inputId="f3",label="Optional Variable to Consider:",factors)), 