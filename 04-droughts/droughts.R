
# Setup -------------------------------------------------------------------
library('tidyverse')
library('ggstream')
drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-20/drought.csv')

glimpse(drought)

drought %>%
  filter(drought_lvl %in% c('D1', 'D2', 'D3', 'D4'),
         area_pct >= 0,
         state_abb == 'CA') %>%


  ggplot(aes(x = valid_start, y = area_pct,  fill = drought_lvl,
             group = drought_lvl,
             label = drought_lvl)) +
  geom_stream(
    geom = "polygon",
    n_grid = 12000,
    bw = .1,
    size = 0,
    position = 'dodge'
  )
