preview_plot <- function(data, metric) {

    # Create the ggplot object
    fig <- data %>%
        ggplot(aes(x = Country, y = all_medals, size = !!sym(metric))) +
        geom_point() +
        theme_minimal() +
        labs(
            title = glue("Olympic Medals vs {metric}"),
            x = "Country",
            y = "Total Medals",
            size = metric
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    # Return the ggplot object
    return(fig)
}