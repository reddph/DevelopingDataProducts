library(shiny)
library(ggplot2)

data("diamonds")

# UI for basic data exploration and prediction of diamond prices in the wellknown 
# diamonds dataset using Random Forest 
shinyUI(pageWithSidebar(
  
    # Application title
    headerPanel("Diamond Price Prediction With RF Regression"),
    
    # Sidebar with controls to select the variable to plot price against the variable
    sidebarPanel(
        selectInput("variable", "Variable:",
                    names(diamonds)[-which(names(diamonds) %in% c("price"))]
                    #names(diamonds)
        )
    ),
    
    # Use of tabsets for user to switch context for stages of exploration and RF regression
    mainPanel(
        tabsetPanel(
            tabPanel("Documentation",includeHTML("./diamondRegressionHelp.html")),
            tabPanel("Exploration",plotOutput("diamondPricePlot")),
            tabPanel("Price PDF",plotOutput("priceDensityPlot")),
            tabPanel("Data Summary", dataTableOutput("summary")),
            tabPanel("RF Regression", dataTableOutput("rfPredictionAccuracy")),
            tabPanel("Prediction", plotOutput("testPredictionMSE")),
            tabPanel("Importance",plotOutput("variableImportancePlot")),
            tabPanel("NumberOfTrees", plotOutput("MSE_vs_NumTrees")),
            class = "span8"
        )
    )
))
