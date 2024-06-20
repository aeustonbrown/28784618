clean_album_names <- function(album) {
    album <- iconv(album, to = "UTF-8", sub = "byte")  # Convert invalid UTF-8 characters to bytes
    album <- gsub("\u0093", "-", album)  # Replace specific problematic character using Unicode code point
    album <- gsub("[^[:graph:][:print:]]", " ", album, perl = T)  # Remove non-graphical or non-printable characters
    album <- gsub("\\s+", " ", album)  # Replace multiple spaces with a single space
    album <- trimws(album)  # Trim leading and trailing spaces
    return(album)
}