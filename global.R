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
    df$date <- as.Date(df$date, format = "%m/%d/%y")
    df$count_per_mil <- df$count/df$population * 1000000
    
    #diff for daily increase
    df <- df %>% group_by(country) %>% mutate(diff = count - lag(count))
    
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
    df_nat$date <- as.Date(df_nat$date, format = "%m/%d/%y")
    df_nat$count_per_100k <- df_nat$count/df_nat$pop_province_state * 100000
    
    #diff for daily increase
    df_nat <- df_nat %>% group_by(province_state) %>% mutate(diff = count - lag(count))
    
    return(df_nat)
}


prep_national_nyt <- function(df_nyt, pop, outcome){
    
    outcome <- enquo(outcome)
    
    #select cols from nyt data
    df_us <- df_nyt %>% select(state, date, !!outcome)
    
    #format data to be consistent with 'national' table
    #df_us$date <- format(df_us$date,"%m/%d/%y")
    df_us$country <- "US" 
    names(df_us) <- c("province_state", "date", "count", "country")
    
    #add state population figures
    df_us <- df_us %>% left_join(select(pop, iso3166_2, province_state, pop_province_state), by = c("province_state" = "province_state"))
    
    #reorder final columns
    df_us <- df_us %>% select(iso3166_2, country, province_state, pop_province_state, date, count)
    
    #add count_per_100k
    df_us$count_per_100k <- df_us$count/df_us$pop_province_state * 100000
    
    #diff for daily increase
    df_us <- df_us %>% group_by(province_state) %>% mutate(diff = count - lag(count))
    
    return(df_us)
}


# prep_national_es <- function(df_es, pop, outcome_es){
#     
#     outcome <- enquo(outcome_es)
#     
#     #select cols from spanish data
#     df <- df_es %>% select(ccaa_iso, fecha, !!outcome)
#     df <- df %>% filter(!is.na(fecha))
#     df$ccaa_iso <- str_replace_all(df$ccaa_iso, "ME", "ML")
#     df$iso3166_2 <- paste0("ES-", df$ccaa_iso)                             
#     df <- df %>% select(-ccaa_iso)
#     
#     names(df) <- c("date", "count", "iso3166_2")
#     df$date <- as.Date(df$date, format = "%d/%m/%Y")
#     #df$date <- format(df$date,"%m/%d/%y")
#     
#     df <- df %>% left_join(select(pop_national, iso3166_2, country, province_state, pop_province_state), by = c("iso3166_2" = "iso3166_2"))
#     
#     #reorder final columns
#     df <- df %>% select(iso3166_2, country, province_state, pop_province_state, date, count)
#     
#     #add count_per_100k
#     df$count_per_100k <- df$count/df$pop_province_state * 100000
#     
#     #diff for daily increase
#     df <- df %>% group_by(province_state) %>% mutate(diff = count - lag(count))
#     
#     return(df)
# }


prep_national_ch <- function(df_ch_outcome, pop){
    
    df <- df_ch_outcome %>% select(-CH)
    df <- df %>% pivot_longer(-Date, names_to = "iso3166_2", values_to = "count") 
    df$iso3166_2 <- paste0("CH-", df$iso3166_2)
    df <- df %>% rename(date = "Date")
    #df$date <- format(df$date,"%m/%d/%y")
    
    df <- df %>% left_join(select(pop_national, iso3166_2, country, province_state, pop_province_state), by = c("iso3166_2" = "iso3166_2"))
    
    #reorder final columns
    df <- df %>% select(iso3166_2, country, province_state, pop_province_state, date, count)
    
    #add count_per_100k
    df$count_per_100k <- df$count/df$pop_province_state * 100000
    
    #diff for daily increase
    df <- df %>% group_by(province_state) %>% mutate(diff = count - lag(count))
    
    return(df)
}


#creates a new column in the reshaped (long) df, counting days since confirmed \geq n
std_date_to_n <- function(df, n, grouping_var){
    
    grouping_var <- enquo(grouping_var)
    
    df <- df %>% filter(count >= n)
    
    df <- df %>% group_by(!!grouping_var) %>% 
        mutate('days_since_n' = row_number()-1)
    
    return(df)
}


#cuts the data for all points before 'date', adds column with 5-day moving average
prep_dailyplot <- function(df, date_start, grouping_var){
    
    grouping_var <- enquo(grouping_var)
    
    df <- df %>% filter(date >= date_start) 
    
    df <- df %>% group_by(!!grouping_var) %>% 
        mutate(MA_5d = caTools::runmean(diff, 5))
    
    return(df)
}


#function to get a df with arbitrary number of reference lines
get_ref_dt_counts <-function(dat_sub, ...){
    doublingtime <- c(...)
    ref_dt_counts <- as_tibble()
    max_y <- max(dat_sub$count)
    
    for (i in doublingtime){
        temp <- tibble(
            country = NA,
            doubling_time = i,
            count = cumprod(c(min(dat_sub$count), 
                              rep(1+(log(2)/i), max(dat_sub$days_since_n)))),
            days_since_n = 0:max(dat_sub$days_since_n), 
            ref_label = paste0(i, " day", "\ndoubling time")
        )
        ref_dt_counts <- rbind(ref_dt_counts, temp)
    }
    
    ref_dt_counts <- ref_dt_counts %>% filter(count <= max_y)  
    
    return(ref_dt_counts)
}


#define theme for line plots
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


#define them for bar plots
theme_barplot <- function(...){
    theme_minimal() +
        theme(
            text = element_text(family = "sans", color = "#22211d"), 
            plot.caption = element_text(color = "#22211d", size=6),
            legend.position = "bottom",
            legend.key.size = unit(.5,"line"),
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
        geom_point(aes(shape=!!group), alpha= 0.4, show.legend = FALSE) +
        geom_line(size=.5, alpha=0.6, show.legend = FALSE) +
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
        geom_point(aes(shape=!!group), alpha= 0.4, show.legend = FALSE) +
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
        geom_point(aes(shape=!!group), alpha= 0.4, show.legend = FALSE) +
        geom_line(size=.5, alpha=0.6, show.legend = FALSE) +
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
        geom_point(aes(shape=!!group), alpha= 0.4, show.legend = FALSE) +
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

#define plotting function: geom_col
plot_col <- function(df, group, fill, title_input, caption_input){  
    
    group <- enquo(group)
    fill <- enquo(fill)
    
    ggplot(df, aes(x=date, y=diff, group=!!group, fill=!!fill)) + 
        geom_col(alpha=.6) +
        geom_line(aes(x=date, y=MA_5d, linetype = ""), color="Grey30") +
        facet_wrap(group, scales="free_y", ncol=1) + 
        theme_barplot() +
        scale_fill_brewer(palette="Dark2") +
        scale_x_date(date_breaks = "14 days" , date_labels = "%b-%d") +
        ggtitle(title_input) +
        ylab("Total") +
        xlab("") +
        labs(caption = caption_input,
             linetype = "5-day moving average", 
             fill = "Actual data")
}


# -----------------------------------------------------------------------------

# GET DATA

# links to current data
url_global_confirmed <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
url_global_deaths <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'
url_national_nyt_states <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv'
#url_national_es <- 'https://covid19.isciii.es/resources/serie_historica_acumulados.csv' 
#url_national_es <- 'https://cnecovid.isciii.es/covid19/resources/agregados.csv' #changed to new site as of May 9
#url_national_es <-'https://cnecovid.isciii.es/covid19/resources/datos_ccaas.csv' #I only updated this today, 2020-08-28
url_national_ch_confirmed <- 'https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/covid19_cases_switzerland_openzh.csv'
url_national_ch_deaths <- 'https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/covid19_fatalities_switzerland_openzh.csv'

# read in data
global_confirmed_raw <- read_csv(url(url_global_confirmed), col_types=cols())
global_deaths_raw <- read_csv(url(url_global_deaths), col_types=cols())
national_nyt <- read_csv(url(url_national_nyt_states), col_types=cols())
#national_es <- suppressWarnings(read_csv(url(url_national_es), col_types=cols(), locale = locale(encoding = 'LATIN1'))) #extra blank column added to end throwing up warnings
national_ch_confirmed <- read_csv(url(url_national_ch_confirmed), col_types=cols())
national_ch_deaths <- read_csv(url(url_national_ch_deaths), col_types=cols())

# read in population tables
pop_global <- read_csv(file = "pop_global.csv", col_types=cols())
pop_national <- read_csv(file="pop_national.csv", locale = locale(encoding = "latin1"), col_types=cols())

# transform data
##global
global_confirmed <- prep_global(global_confirmed_raw, pop_global) 
global_deaths <- prep_global(global_deaths_raw, pop_global) 

## national data
#AUS, CAN, CHN
national_confirmed <- prep_national(global_confirmed_raw, pop_national)
national_deaths <- prep_national(global_deaths_raw, pop_national)

#US
national_confirmed_us <- prep_national_nyt(national_nyt, pop_national, cases)
national_deaths_us <- prep_national_nyt(national_nyt, pop_national, deaths)

#ES
# national_confirmed_es <- prep_national_es(national_es, pop_national, num_casos)
# national_deaths_es <- prep_national_es(national_es, pop_national, num_casos) 

#CH
national_confirmed_ch <- prep_national_ch(national_ch_confirmed, pop_national)
national_deaths_ch <- prep_national_ch(national_ch_deaths, pop_national)

##merge jhu and nyt national data
national_confirmed <- bind_rows(national_confirmed, national_confirmed_us, national_confirmed_ch) #national_confirmed_es
national_deaths <- bind_rows(national_deaths, national_deaths_us, national_deaths_ch) #national_deaths_es, 

national_confirmed$count <- national_confirmed$count %>% replace_na(0)
national_deaths$count <- national_deaths$count %>% replace_na(0)