
# Setup -------------------------------------------------------------------
librarian::shelf(tidyverse, ggbump)
olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')


glimpse(olympics)

rank <-
  olympics %>%
  mutate(points = case_when(medal == 'Gold' ~ 3,
                            medal == 'Silver' ~ 2,
                            medal == 'Bronze' ~ 1,
                            TRUE ~ 0)) %>%
  count(year, team, wt = points) %>%
  group_by(team) %>%
  mutate(cumulative_medals = cumsum(n)) %>%
  group_by(year) %>%
  mutate(rank = rank(desc(cumulative_medals),
                     ties.method = 'random')) %>%
  arrange(year, rank) %>%
  ungroup() %>%
  filter(year > 1980)

top_10 <-
  olympics %>%
  mutate(points = case_when(medal == 'Gold' ~ 3,
                            medal == 'Silver' ~ 2,
                            medal == 'Bronze' ~ 1,
                            TRUE ~ 0)) %>%
  count(team, wt = points) %>%
  mutate(rank = rank(desc(n),
                     ties.method = 'random')) %>%
  arrange(desc(n)) %>%
  head(5)

# Setup -------------------------------------------------------------------
librarian::shelf(tidyverse, ggbump)
olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')


glimpse(olympics)

semi_join(rank, top_10, by = 'team') %>%
  ggplot(aes(x = year, y = rank, color = team)) +
  geom_bump(show.legend = FALSE)



