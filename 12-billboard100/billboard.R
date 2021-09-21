
# Setup -------------------------------------------------------------------

librarian::shelf(tidyverse, ggbrookings, jcolors, ggtext)
billboard <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/billboard.csv')
audio <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/audio_features.csv')


# EDA ---------------------------------------------------------------------

glimpse(billboard)
glimpse(audio)

music <- left_join(billboard, audio, by = c('song_id', 'song', 'performer')) %>%
  mutate(week_id = as.Date(week_id, "%m/%d/%Y"))
music %>%
  mutate(week_id = as.Date(week_id, "%m/%d/%Y")) %>%
  filter(lubridate::year(week_id) >= 2020) %>%
  group_by(week_id) %>%
  summarise(tempo = mean(tempo, na.rm = TRUE)) %>%
  ggplot(aes(x = week_id, y = tempo)) +
  geom_line()

## FEATURES
features <-
  music %>%
 # filter(lubridate::year(week_id) >= 2020) %>%
  pivot_longer(c(danceability, energy, loudness, speechiness, acousticness, instrumentalness, liveness, valence, tempo)) %>%
  group_by(week_id, name) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  ggplot(aes(x = week_id, y = value, color = name)) +
  geom_line(alpha = 0.3) +
  geom_smooth(method = 'loess', size = 2) +
  facet_wrap(. ~ name, scales = 'free')

features +
  scale_color_jcolors('pal8') +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  theme_brookings() +
  theme(legend.position = 'none') +
  labs(x = NULL,
       y = NULL,
       title = "Billboard 100 songs have become more upbeat but less positive over time",
       subtitle = "Evolution of audio features",
       caption = "**Source:** Data.World by way of Sean Miller, Billboard.com and Spotify.")


path <-  here::here("12-billboard100/features")

ggsave(glue::glue("{path}.pdf"), width = 14.5, height = 10.5, device = cairo_pdf)

pdftools::pdf_convert(pdf = glue::glue("{path}.pdf"),
                      filenames = glue::glue("{path}.png"),
                      format = "png", dpi = 640)

## MODE
library('ggstream')
music %>%
  mutate(mode = forcats::as_factor(if_else(mode == 0, 'minor', 'major'))) %>%
  group_by(year = lubridate::year(week_id), mode) %>%
  summarise(count = n()) %>%
  drop_na() %>%
  ggplot(aes(x = year, count, fill = mode)) +
  geom_stream(type = 'proportional')

## CORR
audio %>%
  select(danceability, energy, loudness, speechiness, acousticness, instrumentalness, liveness, valence, tempo) %>%
  scale() %>%
  cor(use = "complete.obs") %>%
  corrplot::corrplot(method = 'color',
                     order = 'hclust',
                     type = 'upper',
                     diag = FALSE,
                     tl.col = 'black',
                     addCoef.col = "grey30",
                     number.cex = .7,
                     col = colorRampPalette(colors = c('red','white','blue'))(200),
                     main = 'Audio Feature Correlation',
                     mar = c(2,2,2,2),
                     family = 'Roboto',
                     number.digits = 1.

  )


