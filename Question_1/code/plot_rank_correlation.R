# Function to process data and plot rank correlations over time
plot_rank_correlation <- function(data, title, subtitle, source, angle) {

    # Calculate rank correlations over time
    rank_cor_dat <- data %>%
        group_by(Year, Gender) %>%
        mutate(Rank = rank(-Count)) %>%
        filter(Rank < 26) %>%
        group_by(Name, Gender) %>%
        mutate(Rank_1_year = lead(Rank)) %>%
        ungroup() %>%
        group_by(Name, Gender) %>%
        mutate(Rank_2_year = lead(Rank, n = 2)) %>%
        ungroup() %>%
        group_by(Name, Gender) %>%
        mutate(Rank_3_year = lead(Rank, n = 3)) %>%
        ungroup() %>%
        group_by(Year, Gender) %>%
        mutate(`Rank-correlation (t, t+1)` = cor(Rank, Rank_1_year, use = "pairwise.complete.obs", method = "spearman")) %>%
        group_by(Year, Gender) %>%
        mutate(`Rank-correlation (t, t+2)` = cor(Rank, Rank_2_year, use = "pairwise.complete.obs", method = "spearman")) %>%
        group_by(Year, Gender) %>%
        mutate(`Rank-correlation (t, t+3)` = cor(Rank, Rank_3_year, use = "pairwise.complete.obs", method = "spearman")) %>%
        arrange(Year, Gender) %>%
        group_by(Year, Gender) %>%
        slice(1) %>%
        select(Year, Gender, contains("Correlation")) %>%
        gather(`Rank-correlation Year Pairs`, Correlation, -Year, -Gender)

    # Plot Rank Correlation Over Time
    rank_plot <- rank_cor_dat %>%
        ggplot() +
        geom_line(aes(Year, Correlation, color = `Rank-correlation Year Pairs`), alpha = 0.8, size = 0.65) +
        facet_wrap(~Gender, nrow = 2, labeller = labeller(Gender = c("F" = "Girls Names", "M" = "Boys Names"))) +
        theme_bw() +
        theme(legend.position = "right") +
        theme(axis.text.x = element_text(angle = angle, hjust = 1)) +
        labs(
            x = "Year (t)", y = "Rank-correlation",
            title = title,
            subtitle = subtitle,
            caption = paste("Data Source:", source)
        ) +
        scale_color_manual(values = c("red", "blue", "purple")) +
        scale_x_continuous(breaks = seq(min(rank_cor_dat$Year), max(rank_cor_dat$Year), by = 5)) +
        geom_vline(xintercept=1990,color="black", alpa=0.3, linetype = "dashed")

    return(rank_plot)
}