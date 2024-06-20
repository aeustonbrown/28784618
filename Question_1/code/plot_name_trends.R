plot_name_trends <- function(national_names, gender, title, subtitle, source, note) {

    small_data <- national_names %>%
        arrange(Year) %>%
        group_by(Name) %>%
        mutate(Surge = Count / lag(Count) - 1) %>%
        ungroup() %>%
        mutate(Decade = paste0(floor(Year / 10) * 10, "s")) %>%
        filter(Year > 1949 & Year < 2010)

    if (gender == "M") {
        top_surge_names_boys <- small_data %>%
            filter(!is.na(Surge)) %>%
            filter(Gender == "M") %>%
            group_by(Name) %>%
            summarise(max_surge = max(Surge, na.rm = TRUE)) %>%
            arrange(desc(max_surge)) %>%
            top_n(30, max_surge) %>%
            pull(Name) %>%
            unique()

        plot_df_M <- small_data %>%
            filter(Name %in% top_surge_names_boys & Gender == "M") %>%
            group_by(Name, Decade) %>%
            summarise(Count = sum(Count)/1000)

        g1 <- plot_df_M %>% ggplot() +
            geom_point(aes(Name, Decade, size = Count), alpha = 0.9, color="lightblue") +
            guides(size = guide_legend(title = "Total Baby Names \n(in thousands)")) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(title = title,
                 subtitle = subtitle,
                 caption = paste("Note:", note, "\nData Source:", source))

        return(g1)
    }

    if (gender == "F") {
        top_surge_names_girls <- small_data %>%
            filter(!is.na(Surge)) %>%
            filter(Gender == "F") %>%
            group_by(Name) %>%
            summarise(max_surge = max(Surge, na.rm = TRUE)) %>%
            arrange(desc(max_surge)) %>%
            top_n(30, max_surge) %>%
            pull(Name) %>%
            unique()

        plot_df_F <- small_data %>%
            filter(Name %in% top_surge_names_girls & Gender == "F") %>%
            group_by(Name, Decade) %>%
            summarise(Count = sum(Count)/1000)

        g2 <- plot_df_F %>% ggplot() +
            geom_point(aes(Name, Decade, size = Count), alpha = 0.9, color="pink") +
            guides(size = guide_legend(title = "Total Baby Names \n(in thousands)")) +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(title = title,
                 subtitle = subtitle,
                 caption = paste("Note:", note, "\nData Source:", source))

        return(g2)
    }

    stop("Invalid gender. Please specify 'M' or 'F'.")
}
