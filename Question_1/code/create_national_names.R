# Define a function to get national counts of baby names
create_national_names <- function(data) {
    national_names <- data %>%
        group_by(Name, Year, Gender) %>%
        summarise(Count = sum(Count, na.rm = TRUE)) %>%
        arrange(Name, Year) %>%
        ungroup()

    return(national_names)
}
