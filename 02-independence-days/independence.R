

# Setup -------------------------------------------------------------------
librarian::shelf(tidyverse, forcats, ggtext)
data <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv'
  )


# Exploratory data analysis -----------------------------------------------

top_colonizers <-
  independence %>%
  select(country, date = date_parsed, year, independence_from) %>%
  drop_na() %>%
  group_by(independence_from) %>%
  count(sort = TRUE) %>%
  head(5)

independence %>%
  semi_join(top_colonizers) %>%
  group_by()
add_count()

# Data cleaning ----------------------------------------------------------------

as_decade <- function(year) {
  return(round(year / 10) * 10)
}

independence <-
  data %>%
  mutate(across(where(is.character),
                as_factor)) %>%
  mutate(
  independence_from = fct_recode(
    independence_from,
    "Soviet Union" = "Russian Soviet Federative Socialist Republic",
    "Soviet Union" = "Soviet Union[55]",
    "Soviet Union" = "Soviet Union[80]",
    "Spain" = "Spanish Empire",
    "Spain" = "Spanish Empire[72]",
    "U.K." = "Kingdom of Great Britain",
    "U.K." = "United Kingdom",
    "Portugal" = "United Kingdom of Portugal, Brazil and the Algarves",
    "Ottoman Emp." = "Ottoman Empire"
  )
) %>%
  filter(year > 1700) %>%
  mutate(decade = as_decade(year),
         independence_from = fct_lump(independence_from, n=5)) %>%
  group_by(independence_from) %>%
  drop_na(independence_from)


# Plot --------------------------------------------------------------------


  independence %>%
    ggplot(aes(x=decade, fill=independence_from)) +
    geom_bar(show.legend = FALSE,
             position = position_dodge2(preserve = "single")) +
    ggbrookings::scale_fill_brookings(palette = 'categorical', reverse = TRUE)+
facet_grid( independence_from ~ .) +
    theme(
      strip.background =   element_rect(fill = "#003A79", colour = NA),
      plot.background =    element_rect(colour = "#FAFAFA"),
      plot.title =         ggtext::element_textbox_simple(
        # font size "large"
        size = rel(2),
        color = "#003A79",
        face = "bold",
        hjust = 0,
        vjust = 1,
        margin = margin(b = 8)
      ),
      plot.caption = ggtext::element_textbox_simple(),
      plot.title.position = "panel",
      plot.subtitle =      ggtext::element_textbox_simple(
        # font size "regular"
        hjust = 0,

        vjust = 1,
        margin = margin(b = 8)
      ),

      strip.text =         element_text(
        colour = "#FAFAFA",
        size = rel(0.5),
        face = 'bold'),

      panel.background =   element_rect(fill = NA, colour = NA),
      panel.border =       element_blank(),
      panel.grid =         element_line(colour = "#CCCCCC",
                                        size = rel(0.7),
                                        linetype = "dotted"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor =   element_blank(),
      panel.spacing =      unit(8 , "pt"),
      panel.spacing.x =    NULL,
      panel.spacing.y =    NULL,
      panel.ontop    =     NULL
    )  +
    labs(title = "**The U.K. took its time, didn't it?**",
         subtitle = "Timeline of independence from top five colonizers",
         caption = "**Source:** Wikipedia",
         x = "",
         y = "")

  ggsave(here::here('figures', '02-independence-days.png'),
         width=85 * (14/5), height=53 * (14/5),
         units = 'mm',
         dpi = 300)
