
# Setup -------------------------------------------------------------------

librarian::shelf(tidyverse, countrycode)
athletes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-03/athletes.csv')

custom_match <- c(YUG = 'Yugoslavia')
athletes %>%
  mutate(abb = str_extract(abb, '\\((.*?)\\)')) %>% drop_na(abb)
  mutate(abb = countrycode(abb, 'ioc', 'country.name')) %>%
  mutate(abb = str_extract(abb, "[A-Z]{3}")) %>% View()
  mutate(abb = case_when(abb == 'FRG' ~ 'GER',
                         TRUE ~ abb)) %>%

  View()
glimpse(athletes)
View(athletes)
countrycode(athletes$abb, origin = 'iso3c', destination = 'iso2c')

manu = 'Manuel ALCALA (FIN)'
regex = "\\((.*?)\\)"
regex = '\w'
regex = '\\w\\s\\w\\s'
str_replace(manu, regex, '')
