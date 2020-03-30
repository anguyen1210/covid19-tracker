# -------------------------------------------------------------------------------
#     
# TITLE: COVID-19 Tracker: Days since N -- `global.R` file
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

# -----------------------------------------------------------------------------

# DEFINE CUSTOM FUNCTIONS


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
    df$iso2c <- suppressWarnings(countrycode::countrycode(df$country, origin = 'country.name', destination = 'iso2c'))
    df <- df %>% select(iso2c, everything())
    
    #fill in missing codes
    df$iso2c[df$country=="Eswatini"] <- "SZ"
    df$iso2c[df$country=="St Martin"] <- "MF"
    df$iso2c[df$country=="Channel Islands"] <- "JG"
    df$iso2c[df$country=="Kosovo"] <- "XK"
    
    #add population figures to global
    df <- df %>% left_join(select(pop, population, iso2c), by = c("iso2c" = "iso2c"))
    
    #reorder, calculate 'count_per_mil'
    df <- df %>% select(iso2c, country, population, date, count)
    df$count_per_mil <- df$count/df$population * 1000000
    
    return(df)
}


prep_national <- function(df, pop){
    df_nat <- df %>% filter(`Country/Region` %in% c("Australia", "Canada", "China"))
    
    df_nat <- df_nat %>% rename(province_state = `Province/State`)
    df_nat <- df_nat %>% rename(country = `Country/Region`)
    
    #drop "Diamond Princess", "Grand Princess", and "Recovered" from `df_nat` table
    df_nat <- df_nat %>% filter(province_state !="Diamond Princess")
    df_nat <- df_nat %>% filter(province_state !="Grand Princess")
    df_nat <- df_nat %>% filter(province_state !="Recovered") 
    
    #drop unneccessary columns from national
    df_nat <- df_nat %>% select(-Lat, -Long)
    
    #pivot long, national
    df_nat <- df_nat %>% pivot_longer(-c(province_state, country), names_to = "date", values_to = "count") 
    
    #add population figures to national
    df_nat <- df_nat %>% left_join(select(pop, iso3166_2, province_state, pop_province_state), by = c("province_state" = "province_state"))
    
    #reorder, calculate 'count_per_100k'
    df_nat <- df_nat %>% select(iso3166_2, country, province_state, pop_province_state, date, count)
    df_nat$count_per_100k <- df_nat$count/df_nat$pop_province_state * 100000
    
    return(df_nat)
}


prep_national_nyt <- function(df_nyt, pop, outcome){
    
    outcome <- enquo(outcome)
    
    #select cols from nyt data
    df_us <- df_nyt %>% select(state, date, !!outcome)
    
    #format data to be consistent with 'national' table
    df_us$date <- format(df_us$date,"%m/%d/%y")
    df_us$country <- "US" 
    names(df_us) <- c("province_state", "date", "count", "country")
    
    #add state population figures
    df_us <- df_us %>% left_join(select(pop, iso3166_2, province_state, pop_province_state), by = c("province_state" = "province_state"))
    
    #reorder final columns
    df_us <- df_us %>% select(iso3166_2, country, province_state, pop_province_state, date, count)
    
    #add count_per_100k
    df_us$count_per_100k <- df_us$count/df_us$pop_province_state * 100000
    
    return(df_us)
}


#creates a new column in the reshaped (long) df, counting days since confirmed \geq n
std_date_to_n <- function(df, n, grouping_var){
    
    grouping_var <- enquo(grouping_var)
    
    df <- df %>% filter(count >= n)
    
    df <- df %>% group_by(!!grouping_var) %>% 
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


#define plotting function: geom_line
plot_line <- function(df, x, y, group, color, title_input, xlab_input, caption_input){
    
    x <- enquo(x)
    y <- enquo(y)
    group <- enquo(group)
    color <- enquo(color)
    
    ggplot(df, aes(!!x, !!y, group =!!group, color=!!color)) +
        geom_line(size=.5, alpha=0.6, show.legend = FALSE) +
        geom_point(aes(shape=!!group), alpha= 0.4, show.legend = FALSE) +
        theme_lineplot() +
        scale_color_brewer(palette="Dark2") +
        scale_x_continuous(limits = c(suppressWarnings(min(df$days_since_n)), suppressWarnings(max(df$days_since_n)+3))) +
        scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
        geom_dl(aes(label=!!group), method=list(dl.trans(x = x + 0.2), "last.bumpup", cex = .6)) +
        ggtitle(title_input) +
        ylab("Total") +
        xlab(xlab_input)+
        labs(caption = caption_input)
}


#define plotting function: geom_smooth
plot_smooth <- function(df, x, y, group, color, title_input, xlab_input, caption_input){
    
    x <- enquo(x)
    y <- enquo(y)
    group <- enquo(group)
    color <- enquo(color)
    
    ggplot(df, aes(!!x, !!y, group =!!group, color=!!color)) +
        geom_smooth(method='loess', se=FALSE, size=.5, alpha=0.6, show.legend = FALSE) +
        theme_lineplot() +
        scale_color_brewer(palette="Dark2") +
        scale_x_continuous(limits = c(suppressWarnings(min(df$days_since_n)), suppressWarnings(max(df$days_since_n)+3))) +
        scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
        geom_dl(aes(label=!!group), method=list(dl.trans(x = x + 0.2), "last.bumpup", cex = .6)) +
        ggtitle(title_input) +
        ylab("Total") +
        xlab(xlab_input)+
        labs(caption = caption_input)
}


#define plotting function: geom_line, log scale
plot_line_log <- function(df, x, y, group, color, title_input, xlab_input, caption_input){
    
    x <- enquo(x)
    y <- enquo(y)
    group <- enquo(group)
    color <- enquo(color)
    
    ggplot(df, aes(!!x, !!y, group =!!group, color=!!color)) +
        geom_line(size=.5, alpha=0.6, show.legend = FALSE) +
        geom_point(aes(shape=!!group), alpha= 0.4, show.legend = FALSE) +
        theme_lineplot() +
        scale_color_brewer(palette="Dark2") +
        scale_x_continuous(limits = c(suppressWarnings(min(df$days_since_n)), suppressWarnings(max(df$days_since_n)+3))) +
        scale_y_log10(labels = function(x) format(x, scientific = FALSE)) +
        geom_dl(aes(label=!!group), method=list(dl.trans(x = x + 0.2), "last.bumpup", cex = .6)) +
        ggtitle(title_input) +
        ylab("Total (log scale)") +
        xlab(xlab_input)+
        labs(caption = caption_input)
}

#define plotting function: geom_smooth, log scale
plot_smooth_log <- function(df, x, y, group, color, title_input, xlab_input, caption_input){
    
    x <- enquo(x)
    y <- enquo(y)
    group <- enquo(group)
    color <- enquo(color)
    
    ggplot(df, aes(!!x, !!y, group =!!group, color=!!color)) +
        geom_smooth(method='loess', se=FALSE, size=.5, alpha=0.6, show.legend = FALSE) +
        theme_lineplot() +
        scale_color_brewer(palette="Dark2") +
        scale_x_continuous(limits = c(suppressWarnings(min(df$days_since_n)), suppressWarnings(max(df$days_since_n)+3))) +
        scale_y_log10(labels = function(x) format(x, scientific = FALSE)) +
        geom_dl(aes(label=!!group), method=list(dl.trans(x = x + 0.2), "last.bumpup", cex = .6)) +
        ggtitle(title_input) +
        ylab("Total (log scale)") +
        xlab(xlab_input)+
        labs(caption = caption_input)
}


# -----------------------------------------------------------------------------

# GET DATA

# links to current data
url_global_confirmed <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
url_global_deaths <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'
url_nyt_states <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv'

# read in data
global_confirmed_raw <- read_csv(url(url_global_confirmed), col_types=cols())
global_deaths_raw <- read_csv(url(url_global_deaths), col_types=cols())
national_nyt <- read_csv(url(url_nyt_states), col_types=cols())

# read in population tables
pop_global <- read_csv(file = "pop_global.csv", col_types=cols())
pop_national <- read_csv(file = "pop_national.csv", col_types=cols())
pop_national_us <- read_csv(file = "pop_national_us.csv", col_types=cols())

# transform data
##global
global_confirmed <- prep_global(global_confirmed_raw, pop_global) 
global_deaths <- prep_global(global_deaths_raw, pop_global) 

## national data
national_confirmed <- prep_national(global_confirmed_raw, pop_national)
national_deaths <- prep_national(global_deaths_raw, pop_national)

national_confirmed_us <- prep_national_nyt(national_nyt, pop_national_us, cases)
national_deaths_us <- prep_national_nyt(national_nyt, pop_national_us, deaths)

##merge jhu and nyt national data
national_confirmed <- bind_rows(national_confirmed, national_confirmed_us)
national_deaths <- bind_rows(national_deaths, national_deaths_us)


