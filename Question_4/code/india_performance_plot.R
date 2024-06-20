india_performance_plot <- function(data, title = "India's Comparative Performance in the Summer Olympics",
                                      subtitle = "", source = "Your Source",
                                      x_axis_label = "Country", y_axis_label = "Total Number of Medals Won",
                                      legend_label = "Category", xaxis_text_size = 8,
                                      yaxis_text_size = 7.1, labeljust=25,
                                      legend_position = "right") {

    # Finding the countries that are of a similar size economically:

    # Arrange the dataframe by GDP per Capita
    GDP_sorted <- data %>%
        select(Country, `GDP per Capita`) %>%
        group_by(Country) %>%
        slice(1) %>%
        ungroup() %>%
        arrange(`GDP per Capita`)

    # Find India's position directly using which()
    india_position <- which(GDP_sorted$Country == "India")

    # Select 5 countries below and 5 above India's position
    start <- max(india_position - 5, 1)
    end <- min(india_position + 5, nrow(GDP_sorted))

    # Slice the dataframe to get the desired rows, excluding India
    econ_similar <- GDP_sorted %>%
        slice(start:end) %>%
        pull(Country)

    emerging_econ <- c("Argentina", "Brazil", "China", "Indonesia", "India", "Korea", "Mexico", "Poland", "Turkey", "South Africa")
    s_american_countries <- c("India", "Argentina", "Brazil", "Uruguay", "Paraguay", "Chile", "Colombia")

    # Adapt Dataframe for plot and analysis: breakdown of different medal types over full period
    data_processed <- data %>%
        select(Country, Medal) %>%
        group_by(Country) %>%
        summarise(tot_medals = n()) %>%
        mutate(Category = ifelse(Country %in% econ_similar, "Similarly Sized Economies",
                                 ifelse(Country %in% emerging_econ, "Emerging Market Economies",
                                        ifelse(Country %in% s_american_countries, "Select South American Countries", NA)))) %>%
        filter(!is.na(Category)) %>%
        left_join(GDP_sorted, by = "Country") %>%
        mutate(`GDP per Capita` = round(`GDP per Capita`, 0))

    # Order According to GDP per Capita
    order1 <- data_processed %>%
        arrange(`GDP per Capita`) %>%
        pull(Country) %>%
        unique()

    # Order to place India at the top of Category
    order2 <- c("India", "Similarly Sized Economies", "Emerging Market Economies", "Select South American Countries")

    # Plotting
    plot <- data_processed %>%
        plot_orderset(Column = "Country", Order = order1) %>%
        mutate(Category = ifelse(Country %in% "India", "India", Category)) %>%
        ggplot() +
        geom_point(aes(Country, tot_medals, size = `GDP per Capita`, color = factor(Category, levels = order2)), stat = "identity", alpha = 0.8) +
        scale_color_manual(name = legend_label,
                           values = c("India" = "blue", "Similarly Sized Economies" = "red",
                                      "Emerging Market Economies" = "green", "Select South American Countries" = "purple")) +
        labs(title = title,
             subtitle = subtitle,
             x = x_axis_label, y = y_axis_label,
             caption = paste("Data Source:", source)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = xaxis_text_size),
              axis.text.y = element_text(size = yaxis_text_size),
              legend.position = legend_position) +
        geom_text(data = data_processed %>% mutate(tot_1 = tot_medals + labeljust), aes(Country, tot_1, label = tot_medals),
                  size = 2.5, alpha = 0.6)

    return(plot)
}
