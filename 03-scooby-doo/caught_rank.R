# Setup -------------------------------------------------------------------
librarian::shelf(tidyverse, lubridate, systemfonts, ggtext, ragg,
                 ggbump)
scoobydoo <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv',
    na = 'NULL'
  )


# Tidy ----------------------------------------------------------------


as_decade <- function(year) {
  return(round(year / 10) * 10)
}

`%not_in%` <- Negate(`%in%`)

caught_rank <-
  scoobydoo %>%
  select(starts_with('caught'), date_aired) %>%
  mutate(across(where(is.character),
                as.logical)) %>%
  mutate(year = year(date_aired),
         decade = as_decade(year)) %>%
  pivot_longer(where(is.logical),
               names_to = 'caught_by',
               names_prefix = 'caught_') %>%
  mutate(caught_by = str_to_title(caught_by)) %>%
  filter(caught_by %not_in% c('Other', 'Not')) %>%
  count(caught_by, decade, wt = value) %>%
  arrange(desc(n)) %>%
  group_by(decade) %>%
  mutate(rank = rank(n, ties.method = 'random')) %>%
  ungroup()

caught_rank %>%
  ggplot(aes(x = decade, y = rank, color = caught_by)) +
  geom_bump(smooth = 8) +
  geom_point() +
  geom_text(data =  caught_rank %>% filter(decade == min(decade)),
            aes(x = decade - 1, label = caught_by), size = 5, hjust = 1, fontface = 'bold',
            family = 'Lato')  +
  geom_text(data =  caught_rank %>% filter(decade == max(decade)),
            aes(x = decade + 1, label = caught_by), size = 5, hjust = 0, family = 'Lato',
            fontface = 'bold') +
  geom_bump(size = 2, smooth = 8) +
  geom_point(size = 6) +
  scale_y_reverse(breaks = seq(1, 5, 1)) +
  scale_x_continuous(limits = c(1965, 2025),
                     breaks = seq(1970, 2020, 10)) +
  ggbrookings::scale_color_brookings() +
    cowplot::theme_minimal_grid() +
    theme(legend.position = "none",
          panel.grid.major = element_blank()) +
  theme(
    text =
      element_text(
        family = "Lato",
        face = "plain",
        colour = "black",
        size = 16,
        lineheight = 0.9,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        margin = margin(),
        debug = FALSE
      ),
    panel.background = element_rect(color = "#FAFAFA")
  ) +

    labs(title = "Shaggy switches to Sativa",
         subtitle = "Characters with most monsters caught by decade",
         x = NULL,
         y = NULL,
         caption = 'Source: Kaggle'
         )

ggsave(
  here::here('figures', '03-rank-caught.png'),
  width = 85 * (14 / 5),
  height = 53 * (14 / 5),
  units = 'mm',
  dpi = 300,
  type = 'cairo',
  device = agg_png()
)
