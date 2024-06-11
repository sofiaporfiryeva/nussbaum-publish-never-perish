library(bibtex)
library(ggplot2)
library(dplyr)
library(stringr)
library(bib2df)
library(ggforce)
library(gganimate)
library(tweenr)


# работа со статьями 

bib <- read.bib("/Users/sophyporfirieva/Desktop/Counting Nussbaum/nussbaum_publications.bib")
bib_df <- bib2df("/Users/sophyporfirieva/Desktop/Counting Nussbaum/nussbaum_publications.bib")
articles_only <- bib_df[bib_df$CATEGORY == "ARTICLE", ]

articles_only$AUTHOR <- gsub("Martha Nussbaum|Martha Craven Nussbaum|Martha CravenLove Nussbaum",
                             "Martha C. Nussbaum", articles_only$AUTHOR)

articles_only <- articles_only %>%
  arrange(AUTHOR)

articles_only <- articles_only %>%
  slice(-(166:197))

articles_only <- articles_only %>%
  filter(JOURNAL != "Proceedings of the Boston Area Colloquium of Ancient Philosophy")

# количество статей по годам

articles_per_year <- articles_only %>%
  group_by(YEAR) %>%
  summarise(count = n())

# строим график публикаций

articles_per_year$YEAR <- as.numeric(articles_per_year$YEAR)

articles_per_year <- articles_per_year %>%
  filter(!is.na(YEAR))


ggplot(articles_per_year, aes(x = YEAR, y = count)) +
  geom_line(color = "#B0C4DE", size = 1) +
  labs(title = "Martha Nussbaum: Publish Do Not Perish",
       x = "Year",
       y = "Number of Publications") +
  scale_x_continuous(breaks = seq(1972, 2023, by = 4)) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    panel.grid.major = element_line(color = "lightgray", size = 0.2),
    panel.grid.minor = element_line(color = "lightgray", size = 0.1)
  ) +
  transition_reveal(YEAR) + # анимация
  ease_aes('linear') 

anim_save("martha_nussbaum_publications_animation.gif") # сохраним

# посмотрим, в каких журналах чаще всего публиковалась Нуссбаум

journal_counts <- articles_only %>%
  group_by(JOURNAL) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

journal_counts_filtered <- journal_counts %>%
  filter(count >= 3)

# работа с книгами

books_only <- bib_df[bib_df$CATEGORY == "BOOK", ]
books_only$AUTHOR <- gsub("Martha Craven Nussbaum|Martha CravenLove Nussbaum", "Martha C. Nussbaum", books_only$AUTHOR)

unwanted_keys <- c(
  "Nussbaum1980-NUSAAS",
  "Nussbaum1987-NUSNFA",
  "Nussbaum1987-NUSICA",
  "Nussbaum1990-NUSLK",
  "Nussbaum1990-NUSFAA-2",
  "Nussbaum2000-NUSVND"
)

martha_books_clean <- martha_books[!martha_books$BIBTEXKEY %in% unwanted_keys, ]

martha_books_clean$YEAR <- as.numeric(martha_books_clean$YEAR)

# в каких издательствах публикуется Нуссбаум

book_publisher <- martha_books_clean %>%
  group_by(PUBLISHER) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

journal_counts_filtered <- journal_counts %>%
  filter(count >= 3)

# график с книгами

ggplot(articles_per_year, aes(x = YEAR, y = count)) +
  geom_line(color = "#B0C4DE", size = 1) +  # Линия синего цвета
  geom_vline(data = martha_books_clean, aes(xintercept = YEAR), color = "#F08080", linetype = "dashed", size = 1) +  # Пунктирные линии для книг
  labs(title = "Martha Nussbaum: Publish Do Not Perish",
       x = "Year",
       y = "Number of Publications") +
  scale_x_continuous(breaks = seq(1972, 2023, by = 4)) +  # Отображение каждого второго года
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    panel.grid.major = element_line(color = "lightgray", size = 0.2),
    panel.grid.minor = element_line(color = "lightgray", size = 0.1)
  ) +
  # Добавление года над пунктирной линией
  geom_text(data = martha_books_clean, aes(x = YEAR, y = max(articles_per_year$count) + 1, label = YEAR), 
            angle = 90, vjust = -0.1, color = "black")
