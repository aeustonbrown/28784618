prepare_summer <- function(Summer, GDP) {

    # Adding GDP (with Country names) to the Summer Olympics Dataframe
    summer_merged <- Summer %>%
        rename(Code = Country) %>%
        left_join(GDP, by = "Code")

    # Ensuring that team sport medalists are not counted more than once
    summer_df <- summer_merged %>%
        group_by(Year, Gender, Event, Country, Medal) %>%
        arrange(Year, Gender, Event, Country, Medal) %>%
        slice(1) %>%
        ungroup()

    return(summer_df)
}