librarian::shelf(tidyverse, ggstream, ggbrookings, ggrepel)
tuesdata <- tidytuesdayR::tt_load('2021-08-10')
investment <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/investment.csv')
chain_investment <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/chain_investment.csv')
ipd <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/ipd.csv')


chain_investment %>%
  filter(meta_cat == 'Health') %>%
  ggplot(aes(x = year, y =gross_inv_chain, fill = category)) +
  geom_stream(type = 'proportional', show.legend = FALSE, extra_span = 0.5) +
  geom_stream_label(aes(label = category), type = 'proportional',
                    color = 'black',
                    size = 3.5,
                    face = 'bold')+
  scale_fill_brookings(palette = 'categorical') +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(breaks = seq(1950, 2010, 10)) +
  theme_brookings() +
  labs(title = 'Investment in private health equipment has exploded',
       subtitle = 'Gross investment in 2012 dollars, 1947-2017',
       caption = '**Source:** Bureau of Economic Analysis',
       x = NULL,
       y = NULL)


ggsave(
  here::here('figures', '07-health.png'),
  width = 85 * (14 / 5),
  height = 53 * (14 / 5),
  units = 'mm',
  dpi = 300,
  type = 'cairo',
  device = ragg::agg_png()
)
