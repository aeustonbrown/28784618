plot_album_popularity <- function(data, cleaned_data) {
    plot <- cleaned_data %>%
        left_join((data %>%
                       mutate(album = clean_album_names(album)) %>%
                       group_by(album, release_date) %>%
                       mutate(album_type = str_extract(album, "\\(([^)]+)\\)") %>% str_replace_all("[()]", ""),
                              album = str_remove(album, "\\s*\\(([^)]+)\\)")) %>%
                       mutate(album = ifelse(str_detect(album, "Justice"), "...And Justice For All", album)) %>%
                       mutate(album = ifelse(str_detect(album, "Garage"), "Garage Inc.", album)) %>%
                       mutate(album = ifelse(str_detect(album, "Helping"), "Helping Hands...Live & Acoustic at The Masonic", album)) %>%
                       mutate(album = ifelse(str_detect(album, "Puppets"), "Master Of Puppets", album)) %>%
                       mutate(album_type = ifelse(is.na(album_type), NA, album_type)) %>%
                       group_by(album)),
                  by = c("album", "release_date")) %>%
        ungroup() %>%
        ggplot(aes(y = popularity, x = reorder(album, -as.numeric(release_date)))) +
        geom_boxplot() +
        labs(title = "Popularity by album", y = "Popularity", x = "Album") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    return(plot)
}