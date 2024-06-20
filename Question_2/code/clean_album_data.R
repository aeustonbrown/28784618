library(dplyr)
library(ggplot2)
library(stringr)

clean_album_data <- function(data) {
    cleaned_data <- data %>%
        mutate(album = clean_album_names(album)) %>%
        group_by(album, release_date) %>%
        mutate(album_type = str_extract(album, "\\(([^)]+)\\)") %>% str_replace_all("[()]", ""),
               album = str_remove(album, "\\s*\\(([^)]+)\\)")) %>%
        mutate(album = ifelse(str_detect(album, "Justice"), "...And Justice For All", album)) %>%
        mutate(album = ifelse(str_detect(album, "Garage"), "Garage Inc.", album)) %>%
        mutate(album = ifelse(str_detect(album, "Helping"), "Helping Hands...Live & Acoustic at The Masonic", album)) %>%
        mutate(album = ifelse(str_detect(album, "Puppets"), "Master Of Puppets", album)) %>%
        mutate(album_type = ifelse(is.na(album_type), NA, album_type)) %>%
        group_by(album, album_type, release_date) %>%
        summarise() %>%
        filter(!grepl("argentina|chile|brazil", album, ignore.case = TRUE)) %>%
        filter(!grepl("live", album_type, ignore.case = TRUE)) %>%
        filter(!grepl("deluxe", album_type, ignore.case = TRUE)) %>%
        group_by(album) %>%
        filter(ifelse(is.na(album_type), release_date == max(release_date[is.na(album_type)]), TRUE)) %>%
        mutate(num_album_types = n()) %>%
        filter(ifelse(num_album_types > 1, is.na(album_type), TRUE)) %>%
        group_by(album, release_date) %>%
        summarise() %>%
        arrange(desc(release_date))

    return(cleaned_data)
}