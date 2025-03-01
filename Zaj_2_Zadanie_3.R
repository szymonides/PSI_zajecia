# Instalacja brakujących pakietów
install.packages("tm", dependencies=TRUE)
install.packages("wordcloud", dependencies=TRUE)
install.packages("RColorBrewer", dependencies=TRUE)
install.packages("ggplot2", dependencies=TRUE)

# Wczytanie bibliotek
library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)

# Wczytanie plików
text_2021 <- tolower(readLines("C:/Users/szymu/Downloads/Biden - First address to Congress_2021.txt", encoding="UTF-8"))
text_2024 <- tolower(readLines("C:/Users/szymu/Downloads/Biden - State of the Union 2024.txt", encoding="UTF-8"))

# Funkcja do czyszczenia tekstu
clean_text <- function(text) {
  text <- gsub("[[:punct:]]", " ", text) # Usunięcie znaków interpunkcyjnych
  text <- gsub("[[:digit:]]", "", text) # Usunięcie liczb
  text <- unlist(strsplit(text, " ")) # Podział na słowa
  stopwords <- c(stopwords("en"), "the", "and", "to", "of", "in", "a", "is", "for", "on", "that", "with", "as", "at", "by", "be", "it", "this", "from", "we", "our", "an", "but", "not", "their", "they", "are", "have", "has", "his", "her", "you", "your", "he", "she", "which", "or", "if", "its", "was", "were", "my", "me", "us", "about", "more", "can", "will", "would", "one", "out", "no", "up", "so", "who", "all", "what", "when", "where", "how")
  text <- text[!(text %in% stopwords)]
  text <- text[text != ""]
  return(text)
}

# Przetworzenie tekstów
words_2021 <- clean_text(text_2021)
words_2024 <- clean_text(text_2024)

# Zliczenie częstości słów
freq_2021 <- table(words_2021)
freq_2024 <- table(words_2024)

# Konwersja do data frame
freq_2021_df <- data.frame(WORD=names(freq_2021), FREQ=as.numeric(freq_2021))
freq_2024_df <- data.frame(WORD=names(freq_2024), FREQ=as.numeric(freq_2024))

# Tworzenie chmur słów
wordcloud(freq_2021_df$WORD, freq_2021_df$FREQ, min.freq = 4, colors = brewer.pal(9, "Blues"), main = "Chmura słów - Biden 2021")
wordcloud(freq_2024_df$WORD, freq_2024_df$FREQ, min.freq = 4, colors = brewer.pal(9, "Reds"), main = "Chmura słów - Biden 2024")

# Wykres słupkowy 10 najczęstszych słów
plot_bar_chart <- function(freq_df, title, color) {
  top_words <- freq_df[order(-freq_df$FREQ), ][1:10, ]
  ggplot(top_words, aes(x=reorder(WORD, -FREQ), y=FREQ)) +
    geom_bar(stat="identity", fill=color) +
    theme_minimal() +
    labs(title=title, x="Słowa", y="Częstość") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_bar_chart(freq_2021_df, "Najczęstsze słowa - Biden 2021", "blue")
plot_bar_chart(freq_2024_df, "Najczęstsze słowa - Biden 2024", "red")

# Porównanie priorytetów
common_2021 <- freq_2021_df$WORD[order(-freq_2021_df$FREQ)][1:20]
common_2024 <- freq_2024_df$WORD[order(-freq_2024_df$FREQ)][1:20]

cat("Najczęstsze słowa unikalne dla 2021:", setdiff(common_2021, common_2024), "\n")
cat("Najczęstsze słowa unikalne dla 2024:", setdiff(common_2024, common_2021), "\n")
cat("Najczęstsze słowa wspólne dla obu przemówień:", intersect(common_2021, common_2024), "\n")
