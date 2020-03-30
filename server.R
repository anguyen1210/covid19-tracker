# -------------------------------------------------------------------------------
#     
# TITLE: COVID-19 Tracker: Days since N -- `server.R` file
# AUTHOR: Anthony Nguyen; @anguyen1210
# URL: mentalbreaks.shinyapps.io/covid19/
# MORE INFO: mentalbreaks.rbind.io/posts/covid-19-tracker/
# 
# -------------------------------------------------------------------------------    

    
shinyServer(function(input, output) {
    
    # Subset based on country selection
    
    ## Set default global selection
    current_selection_global <- reactiveVal(list("Switzerland", "Spain", "Italy", "US"))
    
    observeEvent(input$country_from_global, {
        current_selection_global(input$country_from_global)
    })
    
    ## Set default national-country selection
    current_selection_national <- reactiveVal("US")
    
    observeEvent(input$country_from_national, {
        current_selection_national(input$country_from_national)
    })
    
    ## Set default national-state selection
    current_selection_state <- reactiveVal(list("New York", "Washington", "Texas", "Louisiana"))
    
    observeEvent(input$state_from_national, {
        current_selection_state(input$state_from_national)
    })

    # select countries, global
    output$choose_country_global <- renderUI({
        selectizeInput("country_from_global", 
                       "Country", 
                       choices = sort(as.character(global_confirmed$country)), 
                       multiple = TRUE,
                       selected = current_selection_global(),
                       options = list(maxItems = 6) 
                       )
    })
    
    output$choose_country_national <- renderUI({
        selectInput("country_from_national",
                       "Country",
                       choices = sort(as.character(national_confirmed$country)),
                       multiple=FALSE,
                       selected = current_selection_national()
                       #options = list(maxItems = 6)
        )
    })
    
    output$choose_state_national <- renderUI({
        selectizeInput("state_from_national", 
                       "State/Province/Territory", 
                       choices = national_confirmed %>% filter(country==input$country_from_national) %>% select(province_state) %>% arrange(province_state), 
                       multiple = TRUE,
                       selected = current_selection_state()
                       #options = list(maxItems = 6) 
                       )
    })
    
    dat_sub <- reactive({
        if (input$radio_level == 1 & input$radio_outcome == 1){
            subset(global_confirmed, country %in% input$country_from_global) %>% std_date_to_n(., input$num, country)
        } else if (input$radio_level == 1 & input$radio_outcome == 2) { 
            subset(global_deaths, country %in% input$country_from_global) %>% std_date_to_n(., input$num, country)
        } else if (input$radio_level == 2 & input$radio_outcome == 1) {
            subset(national_confirmed, province_state %in% input$state_from_national) %>% std_date_to_n(., input$num, province_state)
        } else {
            subset(national_deaths, province_state %in% input$state_from_national) %>% std_date_to_n(., input$num, province_state)
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
    
    ptitle_global <- reactive({ 
        if (input$radio_outcome==1 & input$radio_pop==1){
            ptitle_global <- "Coronavirus COVID-19: Confirmed cases"
        } else if (input$radio_outcome==1 & input$radio_pop==2){
            ptitle_global <- "Coronavirus COVID-19: Confirmed cases (per million habitants)"
        } else if (input$radio_outcome==2 & input$radio_pop==1){
            ptitle_global <- "Coronavirus COVID-19: Deaths"
        } else {
            ptitle_global <- "Coronavirus COVID-19: Deaths (per million habitants)"
        }
    })
    
    ptitle_global_log <- reactive({ 
        if (input$radio_outcome==1 & input$radio_pop==1){
            ptitle_global_log <- "Coronavirus COVID-19: Confirmed cases (log scale)"
        } else if (input$radio_outcome==1 & input$radio_pop==2){
            ptitle_global_log <- "Coronavirus COVID-19: Confirmed cases (log scale, per million habitants)"
        } else if (input$radio_outcome==2 & input$radio_pop==1){
            ptitle_global_log <- "Coronavirus COVID-19: Deaths (log scale)"
        } else {
            ptitle_global_log <- "Coronavirus COVID-19: Deaths (log scale, per million habitants)"
        }
    })
    
    ptitle_national <- reactive({ 
        if (input$radio_outcome==1 & input$radio_pop==1){
            ptitle_national <- paste0("Coronavirus COVID-19: ", input$country_from_national, ", Confirmed cases")
        } else if (input$radio_outcome==1 & input$radio_pop==2){
            ptitle_national <- paste0("Coronavirus COVID-19: ", input$country_from_national, ", Confirmed cases (per 100K habitants)")
        } else if (input$radio_outcome==2 & input$radio_pop==1){
            ptitle_national <- paste0("Coronavirus COVID-19: ", input$country_from_national, ", Deaths")
        } else {
            ptitle_national <- paste0("Coronavirus COVID-19: ", input$country_from_national, ", Deaths (per 100K habitants)")
        }
    })
    
    ptitle_national_log <- reactive({ 
        if (input$radio_outcome==1 & input$radio_pop==1){
            ptitle_national_log <- paste0("Coronavirus COVID-19: ", input$country_from_national, ", Confirmed cases (log scale)")
        } else if (input$radio_outcome==1 & input$radio_pop==2){
            ptitle_national_log <- paste0("Coronavirus COVID-19: ", input$country_from_national, ", Confirmed cases (log scale, per 100K habitants)")
        } else if (input$radio_outcome==2 & input$radio_pop==1){
            ptitle_national_log <- paste0("Coronavirus COVID-19: ", input$country_from_national, ", Deaths (log scale)")
        } else {
            ptitle_national_log <- paste0("Coronavirus COVID-19: ", input$country_from_national, ", Deaths (log scale, per 100K habitants)")
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
    
    cumtotal <- reactive({
        #global plots
        if (input$radio_level == 1 & input$radio_lsetting==1 & input$radio_pop==1){
            p1 <- plot_line(dat_sub(), days_since_n, count, country, country, 
                              ptitle_global(), xlabel(), pcaption())
                
        } else if (input$radio_level == 1 & input$radio_lsetting==1 & input$radio_pop==2){
            p1 <- plot_line(dat_sub(), days_since_n, count_per_mil, country, country, 
                            ptitle_global(), xlabel(), pcaption())
                
        } else if (input$radio_level == 1 & input$radio_lsetting==2 & input$radio_pop==1){
            p1 <- plot_smooth(dat_sub(), days_since_n, count, country, country, 
                            ptitle_global(), xlabel(), pcaption())
                
        } else  if (input$radio_level == 1 & input$radio_lsetting==2 & input$radio_pop==2){
            p1 <- plot_smooth(dat_sub(), days_since_n, count_per_mil, country, country, 
                              ptitle_global(), xlabel(), pcaption())
            
        } else if (input$radio_level == 2 & input$radio_lsetting==1 & input$radio_pop==1) {
        #national plots
            p1 <- plot_line(dat_sub(), days_since_n, count, province_state, province_state, 
                            ptitle_national(), xlabel(), pcaption())
                
        } else if (input$radio_level == 2 & input$radio_lsetting==1 & input$radio_pop==2){
            p1 <- plot_line(dat_sub(), days_since_n, count_per_100k, province_state, province_state, 
                            ptitle_national(), xlabel(), pcaption())
                
        } else if (input$radio_level == 2 & input$radio_lsetting==2 & input$radio_pop==1){
            p1 <- plot_smooth(dat_sub(), days_since_n, count, province_state, province_state, 
                            ptitle_national(), xlabel(), pcaption())
            
        } else {
            p1 <- plot_smooth(dat_sub(), days_since_n, count_per_100k, province_state, province_state, 
                              ptitle_national(), xlabel(), pcaption())
        }
    })
    
    output$plot1 <- renderPlot({
        print(cumtotal())
    })
    
    logtotal <- reactive({
        #global log scale
        if (input$radio_level == 1 & input$radio_lsetting==1 & input$radio_pop==1){
            p2 <- plot_line_log(dat_sub(), days_since_n, count, country, country, 
                            ptitle_global_log(), xlabel(), pcaption())
                
        } else if (input$radio_level == 1 & input$radio_lsetting==1 & input$radio_pop==2){
            p2 <- plot_line_log(dat_sub(), days_since_n, count_per_mil, country, country, 
                                ptitle_global_log(), xlabel(), pcaption())
                
        } else if (input$radio_level == 1 & input$radio_lsetting==2 & input$radio_pop==1){
            p2 <- plot_smooth_log(dat_sub(), days_since_n, count, country, country, 
                                ptitle_global_log(), xlabel(), pcaption())
                
        } else if (input$radio_level == 1 & input$radio_lsetting==2 & input$radio_pop==2){
            p2 <- plot_smooth_log(dat_sub(), days_since_n, count_per_mil, country, country, 
                                  ptitle_global_log(), xlabel(), pcaption())
            
        } else if (input$radio_level == 2 & input$radio_lsetting==1 & input$radio_pop==1){
        # national log scale
            p2 <- plot_line_log(dat_sub(), days_since_n, count, province_state, province_state, 
                                ptitle_national_log(), xlabel(), pcaption())
                
        } else if (input$radio_level == 2 & input$radio_lsetting==1 & input$radio_pop==2){
            p2 <- plot_line_log(dat_sub(), days_since_n, count_per_100k, province_state, province_state, 
                                ptitle_national_log(), xlabel(), pcaption())
                
        } else if (input$radio_level == 2 & input$radio_lsetting==2 & input$radio_pop==1){
            p2 <- plot_smooth_log(dat_sub(), days_since_n, count, province_state, province_state, 
                                ptitle_national_log(), xlabel(), pcaption())
                
        } else {
            p2 <- plot_smooth_log(dat_sub(), days_since_n, count_per_100k, province_state, province_state, 
                                ptitle_national_log(), xlabel(), pcaption())
        }
        
    })
    
    output$plot2 <- renderPlot({
        print(logtotal())
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
            ggsave(file, plot = cumtotal(), device = "png", width=7, height =5)
        }
    )
    
    output$download_plot2 <- downloadHandler(
        filename = function() { 
            paste0(input$plot2, "plot_logtotal_", format(Sys.time(), "%Y%m%d"), ".png", sep = "") 
        },
        content = function(file) {
            ggsave(file, plot = logtotal(), device = "png", width=7, height =5)
        }
    )
    
   
})
