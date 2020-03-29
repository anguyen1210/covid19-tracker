# -------------------------------------------------------------------------------
#     
# TITLE: COVID-19 Tracker: Days since N -- `server.R` file
# AUTHOR: Anthony Nguyen; @anguyen1210
# URL: mentalbreaks.shinyapps.io/covid19/
# MORE INFO: mentalbreaks.rbind.io/posts/covid-19-tracker/
# 
# -------------------------------------------------------------------------------    

    
#load required packages
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(countrycode)) install.packages("countrycode", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(directlabels)) install.packages("directlabels", repos = "http://cran.us.r-project.org")

# load custom functions and plotting theme
source("dsn_tools.R")

# get new data structure release
url_global_confirmed <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
url_global_deaths <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'
url_pop <- 'https://raw.githubusercontent.com/anguyen1210/covid19-tracker/master/wdi_SP.POP.TOTL_2018.csv'

global_confirmed <- read_csv(url(url_global_confirmed))
global_deaths <- read_csv(url(url_global_deaths))
pop <- read_csv(url(url_pop))

# transform data
global_confirmed <- prep_global(global_confirmed, pop) 
global_deaths <- prep_global(global_deaths, pop) 

# -------------------------------------------------------------------------------


shinyServer(function(input, output) {
    
    # Subset based on country selection
    
    current_selection <- reactiveVal(list("Switzerland", "Spain", "US", "Italy"))
    
    observeEvent(input$country_from_dat, {
        current_selection(input$country_from_dat)
    })

    output$choose_country <- renderUI({
        selectizeInput("country_from_dat", 
                       "Country", 
                       choices = sort(as.character(global_confirmed$country)), 
                       multiple=TRUE,
                       selected = current_selection(),
                       options = list(maxItems = 6) 
                       )
    })
    
    dat_sub <- reactive({
        if (input$radio_outcome == 1){
            subset(global_confirmed, country %in% input$country_from_dat) %>% std_date_to_n(., input$num)
        } else { 
            subset(global_deaths, country %in% input$country_from_dat) %>% std_date_to_n(., input$num)
        }
    })
    
    # -------------------------------------------------------------------------
    
    # Define plotting elements: timestamp, custom title/labels, line settings
    
    output$stamp <- renderText({ 
        format(Sys.time(), "%d %b %Y, %H:%M %Z")
    })
    
    pcaption <- reactive({
        pcaption <- paste0("Source: mentalbreaks.shinyapps.io/covid19/ \n Data/Access: JHU CSSE, ", format(Sys.time(), "%d %b %Y, %H:%M %Z")) 
    })
    
    ptitle1 <- reactive({ 
        if (input$radio_outcome==1 & input$radio_pop==1){
            ptitle1 <- "Coronavirus COVID-19: Confirmed cases"
        } else if (input$radio_outcome==1 & input$radio_pop==2){
            ptitle1 <- "Coronavirus COVID-19: Confirmed cases (per million habitants)"
        } else if (input$radio_outcome==2 & input$radio_pop==1){
            ptitle1 <- "Coronavirus COVID-19: Deaths"
        } else {
            ptitle1 <- "Coronavirus COVID-19: Deaths (per million habitants)"
        }
    })
    
    ptitle2 <- reactive({ 
        if (input$radio_outcome==1 & input$radio_pop==1){
            ptitle2 <- "Coronavirus COVID-19: Confirmed cases (log scale)"
        } else if (input$radio_outcome==1 & input$radio_pop==2){
            ptitle2 <- "Coronavirus COVID-19: Confirmed cases (log scale, per million habitants)"
        } else if (input$radio_outcome==2 & input$radio_pop==1){
            ptitle2 <- "Coronavirus COVID-19: Deaths (log scale)"
        } else {
            ptitle2 <- "Coronavirus COVID-19: Deaths (log scale, per million habitants)"
        }
    })
    
    xlabel <- reactive({ 
        if (input$radio_outcome==1){
            xlabel <- paste0("Days since confirmed cases \u2265 ", input$num)
        } else {
            xlabel <- paste0("Days since deaths \u2265 ", input$num)
        }
    })
    
    
    # -------------------------------------------------------------------------
    
    # Render subsetted table and plots
    
    output$dat_table <- renderTable({
        dat_sub()
    })
    
    total_vs_country <- reactive({
        if (input$radio_lsetting==1 & input$radio_pop==1){
            p1 <- ggplot(dat_sub(), aes(x=days_since_n, y=count, group = country, color=country)) +
                geom_line(size=.5, alpha=0.6, show.legend = FALSE) +
                geom_point(aes(shape=country), alpha= 0.4, show.legend = FALSE) +
                theme_lineplot() +
                scale_color_brewer(palette="Dark2") +
                scale_x_continuous(limits = c(min(dat_sub()$days_since_n), max(dat_sub()$days_since_n)+3)) +
                scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
                geom_dl(aes(label=country), method=list(dl.trans(x = x + 0.2), "last.bumpup", cex = .6)) +
                ggtitle(ptitle1()) +
                ylab("Total") +
                xlab(xlabel())+
                labs(caption = pcaption())
        } else if (input$radio_lsetting==1 & input$radio_pop==2){
            p1 <- ggplot(dat_sub(), aes(x=days_since_n, y=count_per_mil, group = country, color=country)) +
                geom_line(size=.5, alpha=0.6, show.legend = FALSE) +
                geom_point(aes(shape=country), alpha= 0.4, show.legend = FALSE) +
                theme_lineplot() +
                scale_color_brewer(palette="Dark2") +
                scale_x_continuous(limits = c(min(dat_sub()$days_since_n), max(dat_sub()$days_since_n)+3)) +
                scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
                geom_dl(aes(label=country), method=list(dl.trans(x = x + 0.2), "last.bumpup", cex = .6)) +
                ggtitle(ptitle1()) +
                ylab("Total") +
                xlab(xlabel())+
                labs(caption = pcaption())
        } else if (input$radio_lsetting==2 & input$radio_pop==1){
            p1 <- ggplot(dat_sub(), aes(x=days_since_n, y=count, group = country, color=country)) +
                geom_smooth(method='loess', se=FALSE, size=.5, alpha=0.6, show.legend = FALSE) +
                geom_point(aes(shape=country), alpha= 0.4, show.legend = FALSE) +
                theme_lineplot() +
                scale_color_brewer(palette="Dark2") +
                scale_x_continuous(limits = c(min(dat_sub()$days_since_n), max(dat_sub()$days_since_n)+3)) +
                scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
                geom_dl(aes(label=country), method=list(dl.trans(x = x + 0.2), "last.bumpup", cex = .6)) +
                ggtitle(ptitle1()) +
                ylab("Total") +
                xlab(xlabel())+
                labs(caption = pcaption())
        } else {
            p1 <- ggplot(dat_sub(), aes(x=days_since_n, y=count_per_mil, group = country, color=country)) +
                geom_smooth(method='loess', se=FALSE, size=.5, alpha=0.6, show.legend = FALSE) +
                geom_point(aes(shape=country), alpha= 0.4, show.legend = FALSE) +
                theme_lineplot() +
                scale_color_brewer(palette="Dark2") +
                scale_x_continuous(limits = c(min(dat_sub()$days_since_n), max(dat_sub()$days_since_n)+3)) +
                scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
                geom_dl(aes(label=country), method=list(dl.trans(x = x + 0.2), "last.bumpup", cex = .6)) +
                ggtitle(ptitle1()) +
                ylab("Total") +
                xlab(xlabel())+
                labs(caption = pcaption())
        }
        
    })
    
    output$plot1 <- renderPlot({
        print(total_vs_country())
    })
    
    logtotal_vs_country <- reactive({
        if (input$radio_lsetting==1 & input$radio_pop==1){
            p2 <- ggplot(dat_sub(), aes(x=days_since_n, y=count, group = country, color=country)) +
                geom_line(size=.5, alpha=0.6, show.legend = FALSE) +
                geom_point(aes(shape=country), alpha= 0.4, show.legend = FALSE) +
                theme_lineplot() +
                scale_color_brewer(palette="Dark2") +
                scale_x_continuous(limits = c(min(dat_sub()$days_since_n), max(dat_sub()$days_since_n)+3)) +
                scale_y_log10(labels = function(x) format(x, scientific = FALSE)) +
                geom_dl(aes(label=country), method=list(dl.trans(x = x + 0.2), "last.bumpup", cex = .6)) +
                ggtitle(ptitle2()) +
                ylab("Total (log scale)") +
                xlab(xlabel()) +
                labs(caption = pcaption())
        } else if (input$radio_lsetting==1 & input$radio_pop==2){
            p2 <- ggplot(dat_sub(), aes(x=days_since_n, y=count_per_mil, group = country, color=country)) +
                geom_line(size=.5, alpha=0.6, show.legend = FALSE) +
                geom_point(aes(shape=country), alpha= 0.4, show.legend = FALSE) +
                theme_lineplot() +
                scale_color_brewer(palette="Dark2") +
                scale_x_continuous(limits = c(min(dat_sub()$days_since_n), max(dat_sub()$days_since_n)+3)) +
                scale_y_log10(labels = function(x) format(x, scientific = FALSE)) +
                geom_dl(aes(label=country), method=list(dl.trans(x = x + 0.2), "last.bumpup", cex = .6)) +
                ggtitle(ptitle2()) +
                ylab("Total (log scale)") +
                xlab(xlabel()) +
                labs(caption = pcaption())
        } else if (input$radio_lsetting==2 & input$radio_pop==1){
            p2 <- ggplot(dat_sub(), aes(x=days_since_n, y=count, group = country, color=country)) +
                geom_smooth(method='loess', se=FALSE, size=.5, alpha=0.6, show.legend = FALSE) +
                geom_point(aes(shape=country), alpha= 0.4, show.legend = FALSE) +
                theme_lineplot() +
                scale_color_brewer(palette="Dark2") +
                scale_x_continuous(limits = c(min(dat_sub()$days_since_n), max(dat_sub()$days_since_n)+3)) +
                scale_y_log10(labels = function(x) format(x, scientific = FALSE)) +
                geom_dl(aes(label=country), method=list(dl.trans(x = x + 0.2), "last.bumpup", cex = .6)) +
                ggtitle(ptitle2()) +
                ylab("Total (log scale)") +
                xlab(xlabel()) +
                labs(caption = pcaption())
        } else {
            p2 <- ggplot(dat_sub(), aes(x=days_since_n, y=count_per_mil, group = country, color=country)) +
                geom_smooth(method='loess', se=FALSE, size=.5, alpha=0.6, show.legend = FALSE) +
                geom_point(aes(shape=country), alpha= 0.4, show.legend = FALSE) +
                theme_lineplot() +
                scale_color_brewer(palette="Dark2") +
                scale_x_continuous(limits = c(min(dat_sub()$days_since_n), max(dat_sub()$days_since_n)+3)) +
                scale_y_log10(labels = function(x) format(x, scientific = FALSE)) +
                geom_dl(aes(label=country), method=list(dl.trans(x = x + 0.2), "last.bumpup", cex = .6)) +
                ggtitle(ptitle2()) +
                ylab("Total (log scale)") +
                xlab(xlabel()) +
                labs(caption = pcaption())
        }
        
    })
    
    output$plot2 <- renderPlot({
        print(logtotal_vs_country())
    })
    # -------------------------------------------------------------------------
    
    # Define download functionality
    
    output$download_table <- downloadHandler(
        filename = function() {
            paste0(input$dat_table, "table_", format(Sys.time(), "%Y%m%d"), ".csv", sep = "")
            },
        content = function(file) {
            write.csv(dat_sub(), file, row.names = FALSE)
        }
    )
    
    output$download_plot1 <- downloadHandler(
        filename = function() { 
            paste0(input$plot1, "plot_total_", format(Sys.time(), "%Y%m%d"), ".png", sep = "") 
            },
        content = function(file) {
            ggsave(file, plot = total_vs_country(), device = "png", width=7, height =5)
        }
    )
    
    output$download_plot2 <- downloadHandler(
        filename = function() { 
            paste0(input$plot2, "plot_logtotal_", format(Sys.time(), "%Y%m%d"), ".png", sep = "") 
        },
        content = function(file) {
            ggsave(file, plot = logtotal_vs_country(), device = "png", width=7, height =5)
        }
    )
    
   
})
