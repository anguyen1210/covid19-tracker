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
    current_selection_global <- reactiveVal(list("Switzerland", "Spain", "Italy", "France", "Germany"))
    
    observeEvent(input$country_from_global, {
        current_selection_global(input$country_from_global)
    })
    
    ## Set default national-country selection
    current_selection_national <- reactiveVal("Switzerland")
    
    observeEvent(input$country_from_national, {
        current_selection_national(input$country_from_national)
    })
    
    ## Set default national-state selection
    current_selection_state <- reactiveVal(list("Genève", "Vaud", "Valais", "Zürich", "Ticino"))
    
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
                       multiple = FALSE,
                       selected = current_selection_national()
        )
    })
    
    output$choose_state_national <- renderUI({
        selectizeInput("state_from_national", 
                       "State/Province/Territory", 
                       choices = national_confirmed %>% filter(country==input$country_from_national) %>% select(province_state) %>% arrange(province_state), 
                       multiple = TRUE,
                       selected = current_selection_state(),
                       options = list(maxItems = 6) 
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
    
    dat_sub2 <- reactive({
        if (input$radio_level == 1 & input$radio_outcome == 1){
            subset(global_confirmed, country %in% input$country_from_global) %>% prep_dailyplot(., "2020-02-01", country)
        } else if (input$radio_level == 1 & input$radio_outcome == 2) { 
            subset(global_deaths, country %in% input$country_from_global) %>% prep_dailyplot(., "2020-02-01", country)
        } else if (input$radio_level == 2 & input$radio_outcome == 1) {
            subset(national_confirmed, province_state %in% input$state_from_national) %>% prep_dailyplot(., "2020-02-01", province_state)
        } else {
            subset(national_deaths, province_state %in% input$state_from_national) %>% prep_dailyplot(., "2020-02-01", province_state)
        }
        
    })
    
    ref_numbers <- reactive({
        if (input$radio_ref==1){
            input$ref_num1_1
        } else if (input$radio_ref==2) { 
            c(input$ref_num2_1, input$ref_num2_2)
        } else if (input$radio_ref==3) {
            c(input$ref_num3_1, input$ref_num3_2, input$ref_num3_3)
        }
        
    })
    
    ref_df <- reactive({
        if (input$radio_ref != 0){
            get_ref_dt_counts(dat_sub(), ref_numbers())
        }
        
    })
    
    # -------------------------------------------------------------------------
    
    # Define plotting elements: timestamp, custom title/labels, line settings
    
    output$stamp <- renderText({ 
        format(Sys.time(), "%d %b %Y, %H:%M %Z")
    })
    
    pcaption <- reactive({
        pcaption <- paste0("mentalbreaks.shinyapps.io/covid19/ \n Data accessed: ", format(Sys.time(), "%d %b %Y, %H:%M %Z")) 
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
    
    dailytitle <- reactive({ 
        if (input$radio_outcome==1){
            dailytitle <- "Daily confirmed new cases"
        } else {
            dailytitle <- "Daily confirmed new deaths"
        }
    })
    
    
    # -------------------------------------------------------------------------
    
    # Render subsetted table and plots
    
    output$dat_table <- renderTable({
        display_table <- dat_sub()
        display_table$date <- as.character(display_table$date)
        display_table
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
    
    logtotal_ref <- reactive({
        if (input$radio_ref!=0) {
            p2_ref <- logtotal()
            p2_ref$layers <- c(geom_line(data=ref_df(), aes(x=days_since_n, y=count, group=ref_label, linetype=ref_label), color="grey80", alpha=0.7),
                           geom_text(data=ref_df() %>% group_by(ref_label) %>% slice(which.max(count)),
                                     aes(x=days_since_n +2, y=count, group=ref_label, label=ref_label),
                                     color="grey80", alpha=0.8, size=2.25),
                           p2_ref$layers)
            p2_ref
        }
        
    })
    
    output$plot2 <- renderPlot({
        if (input$radio_ref!=0 & input$radio_pop==1){
            print(logtotal_ref())
        }
        else {
            print(logtotal())
        }
    })
    
    dailytotal <- reactive({
        if (input$radio_level == 1){
            p3 <- plot_col(dat_sub2(), country, country, dailytitle(), pcaption())
        } else {
            p3 <- plot_col(dat_sub2(), province_state, province_state, dailytitle(), pcaption())
        }
    })
    
    output$plot3 <- renderPlot({
        print(dailytotal())
    }, height = function(){
        if (input$radio_level == 1){
            200*n_distinct(input$country_from_global)
        } else {
        200*n_distinct(input$state_from_national)
        }
        }
    )
    
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
