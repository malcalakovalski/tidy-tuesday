knitr::opts_chunk$set(echo = TRUE)
librarian::shelf(tidyverse, tidytext, scales, ggbrookings)
theme_set(theme_brookings())

# Load data from this week (spice girls)
tt <- tidytuesdayR::tt_load("2021-12-14")

# Glimpse Data

Take an initial look at the format of the data available.

tt %>%
  map(glimpse)

lyrics <- tt$lyrics
lyrics_by_member <- lyrics %>%
  separate_rows(section_artist, sep = ", | & | with | and ") %>%
  mutate(
    section_artist = str_remove_all(section_artist, "\\(|\\)|\\{|\\}")
  ) %>%
  filter(section_artist %in% c("Sporty", "Baby", "Scary", "Ginger", "Posh"))
lyrics_by_member %>%
  count(section_artist, sort = TRUE)

# Visualization

p <-
  lyrics_by_member %>%
  unnest_tokens(word, line) %>%
  count(section_artist, word) %>%
  bind_tf_idf(word, section_artist, n) %>%
  group_by(section_artist) %>%
  slice_max(tf_idf, n = 6) %>%
  ungroup() %>%
  mutate(
    section_artist = factor(section_artist,
                            levels = c("Sporty", "Baby",
                                       "Scary", "Ginger", "Posh")),
    word = reorder_within(word, tf_idf, section_artist)
  ) %>%
  ggplot(aes(tf_idf, word, fill = section_artist)) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  facet_wrap(vars(section_artist), scales = "free_y") +
  labs(title = "Tf-idf analysis of lyrics by member of Spice Girls",
       caption = "Source: Genius")

p


head(get_sentiments("bing"))

spice_sentiment <-
  lyrics_by_member %>%
  unnest_tokens(word, line) %>%
  count(section_artist, word) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment)

ggplot(spice_sentiment, aes(x=word, y=n))+  geom_col()+
  theme_minimal()+ ylab("Number of Negative Words in Austen Books")+ xlab("Book")
