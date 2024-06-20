prepare_punching_data <- function(medal_data, select_countries, metric) {
    if (metric == "Population") {
        data <- medal_data %>%
            filter(Country %in% select_countries) %>%
            select(Country, Year, Population, Medals, Code) %>%
            group_by(Country) %>%
            mutate(all_medals = sum(Medals)) %>%
            group_by(Country) %>%
            distinct(Country, Code, all_medals, Population) %>%
            arrange(desc(Population))
    } else if (metric == "GDP per Capita") {
        data <- medal_data %>%
            filter(Country %in% select_countries) %>%
            select(Country, Year, `GDP per Capita`, Medals, Code) %>%
            group_by(Country) %>%
            mutate(all_medals = sum(Medals)) %>%
            group_by(Country) %>%
            distinct(Country, Code, all_medals, `GDP per Capita`) %>%
            arrange(desc(`GDP per Capita`))
    } else {
        stop("Invalid metric. Choose either 'Population' or 'GDP per Capita'.")
    }

    return(data)
}

