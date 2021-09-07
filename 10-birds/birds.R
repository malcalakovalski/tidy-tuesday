
# Setup -------------------------------------------------------------------

librarian::shelf(tidyverse)
bird_baths <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-31/bird_baths.csv')

glimpse(bird_baths)


# EDA ---------------------------------------------------------------------

bird_baths %>%
  group_by(bioregions, urban_rural) %>%

  count(bird_type) %>% tally(n) %>% spread(urban_rural, n) %>%
  mutate(Rural = -Rural) %>%
  gather(Rural, Urban, key='type', value='count')
  ggplot(aes(x = bioregions, y = forcats::fct_reorder(bioregions, total), fill = urban_rural)) +
  geom_col()


