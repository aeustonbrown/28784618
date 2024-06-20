plot_track_gold <- function(data, my_events, title, subtitle, x_label, y_label) {

    track_data <- data %>%
        filter(Discipline == "Athletics") %>%
        filter(Event %in% my_events) %>%
        group_by(Country, Event) %>%
        mutate(Gold_Medals = sum(grepl("Gold", Medal, ignore.case = TRUE))) %>%
        filter(Gold_Medals > 0) %>%
        filter(!is.na(Country)) %>%
        ungroup() %>%
        distinct(Year, Country, Event, Gold_Medals)

    track_data$Event <- factor(track_data$Event, levels = my_events)

    p <- track_data %>%
        ggplot() +
        geom_bar(aes(x = Country, y = Gold_Medals, fill = Country), stat = "identity") +
        facet_wrap(~ Event, nrow = 3, scales="free_y") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        guides(fill = FALSE) +
        labs(title = title,
             subtitle = subtitle,
             x = x_label,
             y = y_label)

    return(p)
}
