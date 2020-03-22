# -------------------------------------------------------------------------------
#     
# TITLE: COVID-19 Tracker: Days since N -- `ui.R` file
# AUTHOR: Anthony Nguyen; @anguyen1210
# URL: mentalbreaks.shinyapps.io/covid19/
# MORE INFO: mentalbreaks.rbind.io/posts/covid-19-tracker/
# 
# -------------------------------------------------------------------------------    

library(shiny)
library(shinyWidgets)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    #google analytics tracking
    tags$head(includeHTML(("google_analytics.html"))),

    # Application title
    titlePanel("COVID-19 Tracker"), #Days Since \uD835\uDE15

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
                     radioButtons("radio_button", label = "Outcome",
                                  choices = list("confirmed cases" = 1, "deaths" = 2, "recovered" = 3), 
                                  selected = 1, inline=TRUE),
            
                     uiOutput("choose_country"),
                     
                     numericInput("num", label = "Days since outcome \u2265 ", value = 100),
                     hr(),
                     fluidRow(column(3, verbatimTextOutput("value"))),
                     strong("Source:"),  
                     p("The data source for this app is made public by the Johns Hopkins
                       Center for Systems Science and Engineering (JHU CSSE) and is ", 
                       a("available on Github. ", href="https://github.com/CSSEGISandData/COVID-19"),
                       "Additional information about the data and its collection is available on the ",
                       a("JHU CSSE 'Mapping 2019-nCov' blog post.",
                         href = "https://systems.jhu.edu/research/public-health/ncov/")),
                     br(),
                     strong("Contact:"),
                     p("Please visit ",
                       a("my blog", href = "https://mentalbreaks.rbind.io/posts/covid-19-tracker/"), 
                       ("for more background on the app and to leave comments or suggestions.")),
                     br(),
                     strong("Data accessed: "),
                     textOutput("stamp")
                     
                    ),
        mainPanel(
            tabsetPanel(
                tabPanel("Plot", 
                         #fluidRow(...)
                         br(),
                         plotOutput("plot1"),
                         div(style = "position: relative; left: 3em; bottom: 0.0em;",
                             dropdown( 
                                 downloadButton(outputId = "download_plot1", label = "Download plot"),
                                 size = "xs",
                                 icon = icon("download", class = "opt"), 
                                 up = TRUE
                                )
                            ),
                         br(),
                         plotOutput("plot2"),
                         div(style = "position: relative; left: 3em; bottom: 0.0em;",
                             dropdown( 
                                 downloadButton(outputId = "download_plot2", label = "Download plot"),
                                 size = "xs",
                                 icon = icon("download", class = "opt"), 
                                 up = TRUE
                                )
                            )
                         ),
                
                tabPanel("Data",
                         br(),
                         div(style = "position: relative; left: 1em; bottom: 0.5em",
                             dropdown( 
                                 downloadButton(outputId = "download_table", label = "Download table"),
                                 size = "xs",
                                 icon = icon("download", class = "opt"), 
                                 up = TRUE
                             )
                         ),
                         tableOutput("dat_table"))
                
            )
            
        )
    )
))
  