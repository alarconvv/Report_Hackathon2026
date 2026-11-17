# Run from the report root: Rscript code/01_registrants_by_role.R
library(ggplot2)
library(paletteer)
colors <- as.character(paletteer_d("ggthemes::excel_Blue"))
d <- read.csv('data/registration_by_role.csv')
stopifnot(sum(d$registrants) == 132, !anyDuplicated(d$role), !anyNA(d),
          all(is.finite(d$registrants)), all(d$registrants >= 0),
          all(d$registrants == floor(d$registrants)))
staff_count <- sum(d$registrants[d$role == 'Staff'])
stopifnot(staff_count == 11)
d <- d[order(-d$registrants), ]
d$role <- factor(d$role, levels = rev(d$role))
p <- ggplot(d, aes(x = registrants, y = role)) +
  geom_col(fill = colors[1], width = 0.68) +
  geom_text(aes(label = registrants), hjust = -0.35, size = 2.9) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = 'Registration by role',
       x = 'Number of registration responses', y = NULL) +
  theme_minimal(base_size = 9.5) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(face = 'bold'), plot.title.position = 'plot', plot.margin = margin(10, 15, 10, 10))
ggsave('figures/registration_by_role.pdf', p, width = 3.4, height = 2.8, device = 'pdf')
