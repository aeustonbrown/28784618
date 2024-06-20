percent_gdp_plot <- function(data, title = "War Aid to Ukraine across Countries as a Percent of GDP",
                                 subtitle = "", source = "Ukraine Aid Tracker",
                                 x_axis_label = "Country", y_axis_label = "Percentage of GDP",
                                 legend_label = "", xaxis_text_size = 8,
                                 yaxis_text_size = 7.1,
                                 legend_position = "right") {

    # Data processing
    proportions_data <- data %>%
        mutate(Group = ifelse(`EU member` == 1, "EU Members", "Other Countries")) %>%
        rename("Total Allocations" = "Total bilateral allocations($ billion)") %>%
        rename("Total Commitments" = "Total bilateral commitments($ billion)") %>%
        select(Group, Country, contains("Total"), `GDP in 2021($ billion)`, -`Share in EU allocations($ billion)`,
               -`Total bilateral commitments of short term($ billion)`) %>%
        mutate(`Commitments as percent of GDP` = round(`Total Commitments` / `GDP in 2021($ billion)` * 100, 2)) %>%
        mutate(`Allocations as percent of GDP` = round(`Total Allocations` / `GDP in 2021($ billion)` * 100, 2)) %>%
        select(-contains("Total"), -contains("2021")) %>%
        gather(Type, Amount, -Group, -Country)

    order_prop <- proportions_data %>%
        filter(Type %in% "Allocations as percent of GDP") %>%
        arrange(Amount) %>%
        pull(Country) %>%
        unique()

    # Plotting
    a <- proportions_data %>%
        plot_orderset(., Column = "Country", Order = order_prop) %>%
        ggplot() +
        geom_bar(aes(x = Country, y = Amount, fill = Group), stat = "identity", position = "dodge", width = 0.6) +
        labs(title = title,
             subtitle = subtitle,
             caption = glue::glue("Data Source: {source}"),
             x = x_axis_label,
             y = y_axis_label,
             fill = legend_label) +
        theme_minimal() +
        theme(
            axis.text.x = element_text(angle = 0, hjust = 1, size = xaxis_text_size),
            axis.text.y = element_text(angle = 0, hjust = 1, size = yaxis_text_size),
            legend.title = element_blank(),
            legend.position = legend_position
        ) +
        coord_flip() +
        facet_wrap(~Type, scales = "free_x")

    return(a)
}

