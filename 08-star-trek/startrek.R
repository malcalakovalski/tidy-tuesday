# Setup -------------------------------------------------------------------

librarian::shelf(tidyverse,
                 trekcolors,
                 showtext,
                 ggdark,
                 trekfont)

computer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-17/computer.csv')


# Load fonts --------------------------------------------------------------

font <- c("Khan", "StarNext", "FederationDS9Title", "Federation", "Klingon", "ModernVulcan", "TNGcast", "FederationStarfleet")
path <- system.file(paste0("fonts/", font, ".ttf"), package = "trekfont")
for(i in seq_along(font)) font_add(font[i], path[i])
font_families()
showtext_auto()


# Plot --------------------------------------------------------------------

p <-
  computer %>%
  drop_na(domain) %>%
  group_by(domain) %>%
  count(sort = T) %>%
  ggplot(aes(x = forcats::fct_reorder(domain, n), y = n, color = domain))+
  geom_lollipop(size = 1.75) +
  geom_text(
    aes(label = n),
    hjust = -0.65, nudge_y = 1,
    size = 4, fontface = "bold", family = font[1]
  ) +
  scale_y_continuous(expand = expansion(0.1)) +
  scale_color_trek('lcars_2357', reverse = TRUE) +
  coord_flip() +
  labs(title = 'Most common interactions',
         subtitle = 'Domain of interaction type count',
         x = NULL,
         y = NULL)  +
  dark_theme_bw(base_size = 16, base_family = font[2]) +
  theme(plot.margin = unit(c(0.5, 0.5, 2.5, 0.5), "lines"),
          legend.position = 'none')

logo <-
  image_read('08-star-trek/startrek-logo.png') %>%
  image_resize("400x400")

ggdraw() +
  draw_plot(p) +
  draw_image(logo,
             x = 1,
             y = 0.4,
             hjust = 1,
             vjust = 1,
             halign = 1,
             valign = 1,
             width = 0.15)

ggsave('figures/08-startrek.png',
       width = 85 * (14 / 5),
       height = 53 * (14 / 5),
       units = 'mm',
       dpi = 300,
       type = 'cairo',
       device = ragg::agg_png())
