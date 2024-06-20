# Function to create plots based on metric
create_punching_plot <- function(data, metric, title, subtitle, source, xint, yint, xmin, xmax, ymin, ymax) {

    if (metric == "Population") {
        # Population plot
        plot <- data %>%
            filter(!(Country %in% c("United States", "China"))) %>%
            ggplot() +
            geom_point(aes(all_medals, Population, color = Country)) +
            geom_text(aes(all_medals, Population, label = Code), vjust = -0.6, hjust = 0.5, size = 2.6, color = "blue") +
            theme_minimal() +
            labs(
                title = title,
                subtitle = subtitle,
                caption =  paste("Data Source:", source),
                x = "Total Medals",
                y = "Population",
                color = "Country"
            ) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            geom_hline(yintercept = 50000000) +
            geom_vline(xintercept = 400) +
            geom_rect(aes(xmin = 400, xmax = max(all_medals) + 20, ymin = 0, ymax = 50000000), color = "red", alpha = 0) +
            guides(color = FALSE)

    } else if (metric == "GDP per Capita") {
        # GDP per Capita plot
        plot <- data %>%
            filter(!(Country %in% c("United States"))) %>%
            ggplot() +
            geom_point(aes(all_medals,`GDP per Capita`, color = Country)) +
            geom_text(aes(all_medals,`GDP per Capita`, label = Code), vjust = -0.6, hjust = 0.5, size = 2.6, color = "blue") +
            theme_minimal() +
            labs(
                title = title,
                subtitle = subtitle,
                caption = paste("Data Source:", source),
                x = "Total Medals",
                y = "GDP per Capita",
                color = "Country"
            ) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            geom_hline(yintercept = 20000) +
            geom_vline(xintercept = 400) +
            geom_rect(aes(xmin = 400, xmax = max(all_medals)+20, ymin = 0, ymax = 20000), color = "red", alpha = 0) +
            guides(color = FALSE)
    } else {
        stop("Invalid metric. Please specify 'Population' or 'GDP per Capita'.")
    }

    return(plot)
}
