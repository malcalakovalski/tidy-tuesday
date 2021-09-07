
# Setup -------------------------------------------------------------------
librarian::shelf(tidyverse, ggridges)
lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')

glimpse(lemurs)
lemurs %>%
  mutate(taxon = fct_lump_n(as_factor(taxon), n = 10)) %>%
ggplot(aes(x = age_at_death_y, y = sex, fill = sex)) +
  ggridges::geom_density_ridges(alpha = 0.5, trim = TRUE) +
  ggbrookings::theme_brookings() +
  facet_wrap(~ taxon) +
  ggbrookings::scale_fill_brookings('semantic3')

