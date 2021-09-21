# Setup -------------------------------------------------------------------
librarian::shelf(tidyverse, ggtext, glue, magick, cowplot, waffle, showtext)

nominees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-21/nominees.csv')

# Wrangling ---------------------------------------------------------------

netflix <-
  nominees %>%
  filter(distributor == "Netflix") %>%
  count(year, type)

# Graphic -----------------------------------------------------------------

# Create labels
proportions <-
  netflix %>%
  group_by(year) %>%
  mutate(total = sum(n),
         label = glue("<br><b>{year}</b><br>**<span style='color:#E62A40; font-weight:bold'>{n}</i>** out of **<span style ='color:#ECBD3B; font-weight:bold'>{total}</i>**")) %>%
  ungroup() %>%
  filter(type == 'Winner') %>%
  pull(label, year)

p1 <-
  ggplot(netflix, aes(values = n, fill = type)) +
  geom_waffle(
    color = "white",
    size = .25,
    n_rows = 10,
    flip = TRUE
  ) +
  facet_wrap(
    ~ year,
    nrow = 1,
    strip.position = "bottom",
    labeller = as_labeller(proportions)
  ) +
  scale_x_discrete() +
  scale_y_continuous(
    labels = function(x)
      x * 10,
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = c('#E62A40', '#ECBD3B'),
                    name = '') +
  coord_equal() +
  theme_enhance_waffle() +
  theme(
    panel.grid = element_blank(),
    axis.ticks.y = element_line(),
    plot.background = element_rect(fill = "#F2F7FA"),
    panel.background = element_rect(fill = "#F2F7FA"),
    text = element_text(family = 'Lato'),
    plot.title = element_textbox_simple(
      size = rel(2.5),
      hjust = .5,
      family = "Mercury",
      face = "bold",
      margin = margin(t = 10, b = 25)
    ),
    plot.caption = element_textbox_simple(
      size = rel(1.5),
      family = "Lato Semibold",
      color = "black",
      margin = margin(t = 20)
    ),
    plot.subtitle = element_textbox_simple(
      size = rel(2),
      family = "Lato Semibold",
      color = "black"
    ),
    plot.margin = margin(
      t = 25,
      r = 10,
      b = 20,
      l = 15
    ),
    plot.title.position = 'plot',
    legend.position = 'top',
    legend.spacing.x = unit(1, 'cm'),
    legend.text = element_text(size = rel(2), family = 'Lato'),
    legend.background = element_rect(fill = '#F2F7FA'),
    axis.ticks = element_line(size = 0),
    strip.text = element_textbox_simple(
      padding = margin(5.5, 5.5, 5.5, 5.5),
      family = 'Lato',
      size = 14
    ),
    strip.background = element_rect(fill = NA)
  ) +
  guides(
    fill = guide_legend(
      reverse = TRUE,

      label.position = 'top',
      override.aes = list(shape = 1),
      title.position = 'top',
      keywidth = grid::unit(3, 'line'),
      keyheight = grid::unit(3, 'line')
    )
  ) +
  labs(title = "**Netflix had a record performance at the Emmys in 2021**",
       subtitle = "Number of Emmys nominations and wins for netflix",
       caption = "**Source:** Academy of Television Arts & Sciences.")

p1

# Adding logo -------------------------------------------------------------
logo <-
  image_read('13-emmys/logo.jpg') %>%
  image_convert('png') %>%
  image_transparent('white') %>%
  image_resize("400x400")


ggdraw() +
  draw_plot(p1) +
  draw_image(logo,
             x = 0.4,
             y = 0.6,
             hjust = 1,
             vjust = 1,
             halign = 1,
             valign = 1,
             width = 0.2)

# Saving ------------------------------------------------------------------

path <-  here::here("13-emmys/netflix")

ggsave(glue::glue("{path}.pdf"), width = 14.5, height = 10.5, device = cairo_pdf, bg = '#F2F7FA')

pdftools::pdf_convert(pdf = glue::glue("{path}.pdf"),
                      filenames = glue::glue("{path}.png"),
                      format = "png", dpi = 640)
