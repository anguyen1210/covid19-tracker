# -------------------------------------------------------------------------------
#     
# TITLE: COVID-19 Tracker: Days since N -- `ui.R` file
# AUTHOR: Anthony Nguyen; @anguyen1210
# URL: mentalbreaks.shinyapps.io/covid19/
# MORE INFO: mentalbreaks.rbind.io/posts/covid-19-tracker/
# 
# -------------------------------------------------------------------------------    


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    #google analytics tracking
    tags$head(includeHTML(("google_analytics.html"))),

    # Application title
    titlePanel("COVID-19 Tracker"), #Days Since \uD835\uDE15

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
                     h3("Select variables"),
                     radioButtons("radio_outcome", label = "Outcome",
                                  choices = list("confirmed cases" = 1, "deaths" = 2),  #, "recovered" = 3
                                  selected = 1, inline=TRUE),
                     
                     radioButtons("radio_level", label = "Observation level",
                                  choices = list("global" = 1, "national" = 2),  #, "recovered" = 3
                                  selected = 1, inline=TRUE),
                     
                     conditionalPanel(condition = "input.radio_level==1",
                                      uiOutput("choose_country_global")
                                    ),
                     
                     conditionalPanel(condition = "input.radio_level==2",
                                      selectInput("country_from_national",
                                                         "Country",
                                                         choices = sort(as.character(unique(national_confirmed$country))), #c("Australia", "Canada", "China", "US"),
                                                         multiple = FALSE,
                                                         selected = "US"
                                                         ),
                                      # uiOutput("choose_country_national"),
                                      uiOutput("choose_state_national")
                                    ),
                     hr(), 
                     
                     h3("Normalize data"),
                     radioButtons("radio_pop", label = "Y-axis: total",
                                  choices = list("absolute" = 1, "population adjusted" = 2),
                                  selected = 1, inline=TRUE), 
                     numericInput("num", label = "X-axis: days since outcome \u2265 ", value = 100),
                     hr(),
                     
                     h3("Customize plot"),
                     radioButtons("radio_lsetting", label = "Line setting",
                                  choices = list("straight segments" = 1, "fitted" = 2), 
                                  selected = 1, inline=TRUE),
                     radioButtons("radio_ref", label = "Reference lines",
                                  choices = list("none"=0, "1"=1, "2"=2, "3"=3), 
                                  selected = 0, inline=TRUE),
                     conditionalPanel(condition = "input.radio_ref==1 & input.radio_pop==1",
                                      numericInput(inputId = "ref_num1_1", 
                                                   label = "Doubling time (in days)",
                                                   min = 1, max = 30, value = 2)),
                     conditionalPanel(condition = "input.radio_ref==2 & input.radio_pop==1",
                                      numericInput(inputId = "ref_num2_1", 
                                                   label = "Doubling time (in days)",
                                                   min = 1, max = 30, value = 2),
                                      numericInput(inputId = "ref_num2_2", 
                                                   label = "Doubling time (in days)",
                                                   min = 1, max = 30, value = 3)),
                     conditionalPanel(condition = "input.radio_ref==3 & input.radio_pop==1",
                                      numericInput(inputId = "ref_num3_1", 
                                                   label = "Doubling time (in days)",
                                                   min = 1, max = 30, value = 1),
                                      numericInput(inputId = "ref_num3_2", 
                                                   label = "Doubling time (in days)",
                                                   min = 1, max = 30, value = 3),
                                      numericInput(inputId = "ref_num3_3", 
                                                   label = "Doubling time (in days)",
                                                   min = 1, max = 30, value = 7)),
                     hr(),
                     
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
                         tableOutput("dat_table")),
                
                tabPanel("Source",
                         br(),
                         h3("Code and additional information"),
                         p("The code for this Shiny app can be found on my ", 
                            a("github repo.", href="https://github.com/anguyen1210/covid19-tracker")),
                         br(),
                         p("For additional background, and to leave comments or suggestions, 
                            please visit ",
                           a("my blog.", 
                             href= "https://mentalbreaks.rbind.io/posts/covid-19-tracker/"), 
                            "Additional features and edits will be added on an ongoing basis. 
                           All comments, suggestions and pull requests welcome."), 
                         br(),
                         h3("Data - Global"),
                         br(),
                         p("The ", 
                           a("global data", 
                             href="https://github.com/CSSEGISandData/COVID-19"), 
                           " for this app is provided by the Johns Hopkins Center for Systems Science 
                           and Engineering (JHU CSSE). Additional information about the data and its 
                           collection is available on the ", 
                           a("JHU CSSE 'Mapping 2019-nCov' blog post.", 
                             href="https://systems.jhu.edu/research/public-health/ncov/")),
                         br(),
                         p("Global population data extracted from the ",
                           a("World Bank WDI database", 
                             href="https://databank.worldbank.org/source/world-development-indicators"), 
                           "current as of 2018."),
                         br(),
                         h3("Data - National"),
                         br(),
                         p("For a select number of countries, national data is available showing 
                           the number of confirmed cases and deaths at the state/province/territory 
                           level. For national data that does not come from the JHU CSSE dataset, 
                           please consult the relevant links listed below for additional information 
                           on the specific data source and its update frequency."),
                         br(),
                         strong("Australia"),
                         br(),
                         p("Australian territory-level data included in the JHU CSSE dataset."),
                         br(),
                         p(a("Australian territory population data", 
                             href="https://www.abs.gov.au/ausstats/abs@.nsf/Latestproducts/3101.0Main%20Features3Sep%202019?opendocument&tabname=Summary&prodno=3101.0&issue=Sep%202019&num=&view="), 
                           "provided by the Australian Bureau of Statistics."),
                         br(),
                         strong("Canada"),
                         br(),
                         p("Canadian province-level data included in the JHU CSSE dataset."),
                         br(),
                         p(a("Canada province population data ", 
                             href="https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710000901"), 
                           "provided by Statistics Canada, current as of 2019."),
                         br(),
                         strong("China"),
                         br(),
                         p("China province-level data included in the JHU CSSE dataset.")  ,
                         br(),
                         p(a("China province population data ", 
                             href="https://en.wikipedia.org/wiki/Provinces_of_China"), 
                           "taken from Wikipedia."),
                         br(),
                         strong("Spain"),
                         br(),
                         p(a("Spain province-level data ", 
                             href="https://covid19.isciii.es/"), 
                           "aggregated by the Spanish Ministry Health (Ministerio de Sandidad/ 
                           Instituto de Salud Carlos III)"),
                         br(),
                         p(a("Spain province population data ", 
                             href="https://www.ine.es/"), 
                           "provided by the Spanish National Statistical Institute (Instituto 
                           Nacional de Estadística)"),
                         br(),
                         strong("Switzerland"),
                         br(),
                         p(a("Switzerland canton data ", 
                             href="https://www.corona-data.ch/"), 
                           "provided by Daniel Probst, by way of aggregated data from the Zurich 
                           Statistical Office (OpenZH)."),
                         br(),
                         p(a("Switzerland canton population data ", 
                             href="https://www.bfs.admin.ch/bfs/en/home.html"), 
                             "provided by the Swiss Federal Statistical Office."),
                         br(),
                         strong("United States"),
                         br(),
                         p(a("U.S. state-level data ", 
                             href="https://github.com/nytimes/covid-19-data"), 
                           "is provided by the New York Times. Additional information about the data 
                           and its collection is available in ", 
                           a("the accompanying NYT article.", 
                             href="https://www.nytimes.com/article/coronavirus-county-data-us.html")),
                         br(),
                         p(a("US state and territory population data ", 
                             href="https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html"), 
                           "provided by the US Census Bureau."),
                         br())
                
            )
            
        )
    )
))
  