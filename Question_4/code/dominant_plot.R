dominant_plot <- function(medal_data, dominant_countries, title = "Medals over time", subtitle = "", source = "Olympics data", cumulative = FALSE){

    # Plotting the data
    if (cumulative) {
        plot <- medal_data %>%
            filter(Country %in% dominant_countries) %>%
            ggplot() +
            geom_line(aes(Year, Cumulative_Medals, color = Country), alpha = 0.8, size = 1) +
            facet_wrap(~Country, nrow=4) +
            theme_bw() +
            theme(legend.position = "right") +
            labs(x = "",
                 y = "Cumulative Number of Medals",
                 title = title,
                 subtitle = subtitle,
                 caption = paste("Data Source:", source)) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
        plot <- medal_data %>%
            filter(Country %in% dominant_countries) %>%
            ggplot() +
            geom_line(aes(Year, Medals, color = Country), alpha = 0.8, size = 1) +
            facet_wrap(~Country, nrow=4) +
            theme_bw() +
            theme(legend.position = "right") +
            labs(x = "",
                 y = "Total Number of Medals",
                 title = title,
                 subtitle = subtitle,
                 caption = paste("Data Source:", source)) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }

    return(plot)
}