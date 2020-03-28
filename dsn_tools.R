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
# if(!require(countrycode)) install.packages("countrycode", repos = "http://cran.us.r-project.org")
# if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")

#this function cleans country cols, adds population, calculates 'count_per_mil' 
prep_global <- function(df, pop){
    #aggregate Australia, Canada, China for global table 
    national_agg <- df %>% 
        filter(`Country/Region` %in% c("Australia", "Canada", "China")) %>% 
        group_by(`Country/Region`) %>% 
        summarise_if(is.numeric, sum)
    
    #drop "Diamond Princess", "Recovered" and all countries already subsetted into the `national` table
    df <- df %>% 
        filter(is.na(`Province/State`) | `Province/State`!="Diamond Princess") %>% 
        filter(`Country/Region`!= "Diamond Princess") %>% 
        filter(is.na(`Province/State`) | `Province/State` !="Recovered") %>% 
        filter(!(`Country/Region` %in% c("Australia", "Canada", "China")))
    
    #add national aggregates to global table
    df <- bind_rows(df, national_agg)
    
    #convert remaining `Province/State` territories `Country` in global table  
    df <- df %>%  mutate(country = ifelse(is.na(`Province/State`), `Country/Region`, `Province/State`))
    
    #drop unneccessary columns from global
    df <- df %>% select(-`Province/State`, -`Country/Region`, -Lat, -Long)
    
    #pivot long, global
    df <- df %>% pivot_longer(-country, names_to = "date", values_to = "count") 
    
    #add iso3 codes to global
    df$iso2c <- countrycode::countrycode(df$country, origin = 'country.name', destination = 'iso2c')
    df <- df %>% select(iso2c, everything())
    
    #fill in missing codes
    df$iso2c[df$country=="Eswatini"] <- "SZ"
    df$iso2c[df$country=="St Martin"] <- "MF"
    df$iso2c[df$country=="Channel Islands"] <- "JG"
    df$iso2c[df$country=="Kosovo"] <- "XK"
    
    #add population figures to global
    df <- df %>% 
        left_join(select(pop, population, iso2c), by = c("iso2c" = "iso2c"))
    
    #reorder, calculate 'count_per_mil'
    df <- df %>% select(iso2c, country, population, date, count)
    df$count_per_mil <- df$count/df$population * 1000000
    
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

