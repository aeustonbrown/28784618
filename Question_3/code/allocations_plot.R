allocations_plot <- function(data, title = "Aid",
                             subtitle = "", source = "Source Name",
                             x_axis_label = "Donor Group", y_axis_label = "Amount of Aid ($ Billion)",
                             legend_label = "Subtype", xaxis_text_size = 8,
                             yaxis_text_size = 7.1,
                             legend_position = "right") {

    # Data processing
    allocations_data <- data %>%
        mutate(Group = ifelse(`EU member` == 1, "EU Members", "Other Countries")) %>%
        select(Group, contains("allocations"), `Total bilateral commitments($ billion)`, -`Share in EU allocations($ billion)`) %>%
        group_by(Group) %>%
        summarise_all(sum, na.rm = TRUE) %>%
        gather(Subtype, `Amount($ billion)`, -Group, -contains("total")) %>%
        mutate(Subtype = ifelse(Subtype == "Financial allocations($ billion)", "Allocated Financial Aid",
                                ifelse(Subtype == "Military allocations($ billion)", "Allocated Military Aid",
                                       "Allocated Humanitarian Aid"))) %>%
        rename("Total Allocations" = "Total bilateral allocations($ billion)") %>%
        rename("Total Commitments" = "Total bilateral commitments($ billion)")

    order_alloc <- allocations_data %>%
        arrange(`Amount($ billion)`) %>%
        pull(Group) %>%
        unique()

    # Plotting
    c <- allocations_data %>%
        plot_orderset(., Column = "Group", Order = order_alloc) %>%
        ggplot() +
        geom_bar(aes(Group, `Amount($ billion)`, fill = Subtype), stat = "identity") +
        geom_point(aes(Group, `Total Commitments`, shape = "Total Commitments"), size = 3) +
        scale_shape_manual(name = "", values = c("Total Commitments" = 16)) +
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
        guides(shape = guide_legend(override.aes = list(size = 5))) # Adjust point size in legend

    return(c)
}