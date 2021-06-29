
# Setup -------------------------------------------------------------------
library('tidyverse')
tuesdata <- tidytuesdayR::tt_load("2021-06-29")


# Exploratory data analysis -----------------------------------------------


animal_rescues <- tuesdata$animal_rescues

glimpse(animal_rescues)

animal_rescues %>%
  group_by(animal_group_parent, cal_year) %>%
  add_count(name = "animal_rescue_counts")


data <-
  animal_rescues %>%
  mutate(
    animal_group_parent = recode(animal_group_parent,
      cat = "Cat",
      Budgie = "Bird",
      Pigeon = "Bird"
    ),
    animal_group_parent = case_when(
      str_detect(animal_group_parent, "Unknown") ~ "Unknown",
      TRUE ~ animal_group_parent
    ),
    across(where(is.character), factor)
  ) %>%
  filter(cal_year == "2020") %>%
  group_by(animal_group_parent) %>%
  count() %>%
  ungroup() %>%
  summarise(animal_group_parent,
    proportion = n / sum(n),
  )


ggplot(
  data,
  aes(x = reorder(animal_group_parent, proportion), y = proportion, fill = animal_group_parent)
) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(
    name = "",
    labels = scales::label_percent(accuracy = 1)
  ) +

  gghighlight::gghighlight(animal_group_parent == "Cat") +
  ggbrookings::theme_brookings() +
  ggbrookings::scale_fill_brookings() +
  labs(
    title = "Cats are adorable but also clumsy",
    subtitle = "Animal rescues in 2020",
    x = "",
    caption = '**Source:** The Guardian'
  )

ggsave(here::here('01-animal-rescue', 'bar_chart.png'),
       width=85 * (14/5), height=53 * (14/5),
       units = 'mm',
       dpi = 300)
