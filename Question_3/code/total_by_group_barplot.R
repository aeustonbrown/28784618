total_by_group_barplot <- function(data, title, subtitle, x_axis_label = "Donor Group", y_axis_label = "Amount ($ Billion)", fill_label = "Commitments vs Allocations", angle = 0, legend_title = "", legend_position = "top", palette = c("Total Commitments" = "orange", "Total Allocations" = "lightblue"), coord_flip_plot = TRUE) {

    # Data processing for count_group
    count_group <- data %>%
        mutate(Group = ifelse(`EU member` == 1, "EU Members", ifelse(Country %in% "United States", Country, "Other Countries"))) %>%
        group_by(Group) %>%
        summarise(Count = n())

    # Data processing for totals_data
    totals_data <- data %>%
        mutate(Group = ifelse(`EU member` == 1, "EU Members", ifelse(Country %in% "United States", Country, "Other Countries"))) %>%
        select(Group, contains("total"), -contains("short")) %>%
        group_by(Group) %>%
        summarise_all(sum, na.rm = TRUE) %>%
        rename("Total Allocations" = "Total bilateral allocations($ billion)") %>%
        rename("Total Commitments" = "Total bilateral commitments($ billion)") %>%
        gather(Type, Amount, -Group) %>%
        left_join(count_group, by = "Group")

    # Plotting
    p <- totals_data %>%
        ggplot() +
        geom_bar(aes(x = Group, y = Amount, fill = Type), stat = "identity", position = "dodge", width = 0.6) +
        labs(title = title,
             subtitle = subtitle,
             x = x_axis_label,
             y = y_axis_label,
             fill = fill_label) +
        scale_fill_manual(values = palette) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = angle, hjust = 1)) +
        theme(legend.title = element_blank(), legend.position = legend_position) +
        geom_label(data = totals_data %>% filter(Type == "Total Commitments") %>% filter(Group != "United States") %>% mutate(position = 15), aes(Group, position, label = glue("{Count} Members")), alpha = 0.8, size = 3)

    # Optionally flip coordinates
    if (coord_flip_plot) {
        p <- p + coord_flip()
    }

    return(p)
}
