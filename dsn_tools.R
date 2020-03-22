# -------------------------------------------------------------------------------
#     
# TITLE: COVID-19 Tracker: Days since N -- Custonm functions for shiny app
# AUTHOR: Anthony Nguyen; @anguyen1210
# URL: mentalbreaks.shinyapps.io/covid19/
# MORE INFO: mentalbreaks.rbind.io/posts/covid-19-tracker/
# 
# -------------------------------------------------------------------------------  

#the functions defined below require the following libraries, which are loaded in the main application script
# if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
# if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
# if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
# if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")

#this function adds pivots data to long format for plotting, and adds derived columns 
prep_dat <- function(df){
    
    df_allterritories <- df %>% filter(!str_detect(`Province/State`, ',')) %>% 
        group_by(`Country/Region`) %>% 
        bind_rows(summarise_all(., ~ if (is.numeric(.)) sum(.) else "(All territories)")) %>% 
        filter(`Province/State` == "(All territories)")
    
    df <- bind_rows(df, df_allterritories)
    
    #merge and clean Province/State column to Country column
    df$country <- paste0(df$`Country/Region`, ", ", df$`Province/State`)
    df$country <- str_replace_all(df$country, ", NA", "")
    
    #drop unneccessary columns
    df <- df %>% select(-`Province/State`, -`Country/Region`, -Lat, -Long)
    
    #pivot long
    df <- df %>% pivot_longer(-country, names_to = "date", values_to = "count") 
    
    #gen 'growth_factor'
    df <- df %>% group_by(country) %>% mutate(diff = count - lag(count))
    df <- df %>% group_by(country) %>% mutate(growth_factor = diff/lag(diff))
    
    return(df)
    
}


#creates a new column in the reshaped (long) df, counting days since confirmed \geq n
std_date_to_n <- function(df, n){
    df <- df %>% filter(count >= n)
    
    df <- df %>% group_by(country) %>% 
        mutate('days_since_n' = row_number()-1)
    
    return(df)
}


#define plotting theme
theme_lineplot <- function(...){
    theme_minimal() +
    theme(
        text = element_text(family = "sans", color = "#22211d"), 
        plot.caption = element_text(color = "#22211d", size=6),
        legend.position = "none",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.x = element_line(color = "#22211d", 
                             size = .25, linetype = "solid"),
        #plot.background = element_rect(fill = "#f5f5f2", color = NA) 
        panel.background = element_rect(fill = "#f5f5f2", color = NA)
        ) 
}

