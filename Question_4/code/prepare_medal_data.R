prepare_medal_data <- function(Summer, Winter, GDP) {

    # Setting up the data
    summer_bind <- Summer %>% mutate(Season = "Summer")
    winter_bind <- Winter %>% mutate(Season = "Winter")

    # Combining Winter and Summer, adding Country, and ensuring Medals aren't over counted due to team sports
    all_olympics <- bind_rows(summer_bind, winter_bind) %>%
        rename(Code = Country) %>%
        left_join(GDP, by = "Code") %>%
        group_by(Year, Season, Gender, Event, Country, Medal) %>%
        arrange(Year, Season, Gender, Event, Country, Medal) %>%
        slice(1) %>%
        ungroup()

    # Preparing medal data
    medal_data <- all_olympics %>%
        filter(!is.na(Country)) %>%
        select(Year, Season, Country, Medal) %>%
        group_by(Year, Season, Country) %>%
        mutate(Medals = n()) %>%
        distinct(Year, Season, Country, Medals) %>%
        left_join(GDP, by = "Country") %>%
        mutate(`GDP per Capita` = round(`GDP per Capita`, 0)) %>%
        ungroup() %>%
        group_by(Country) %>%
        arrange(Year, Season) %>%
        mutate(Cumulative_Medals = cumsum(Medals)) %>%
        arrange(desc(Cumulative_Medals)) %>%
        ungroup()

    return(medal_data)
}
