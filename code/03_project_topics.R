# Run from the report root: Rscript code/03_project_topics.R
library(ggplot2)
library(paletteer)
inventory <- read.csv('data/project_topics.csv', check.names = FALSE)
stopifnot(nrow(inventory) == 32, !anyDuplicated(inventory$Name_project),
          !anyNA(inventory[['Topic group']]), all(nzchar(inventory[['Topic group']])))
d <- as.data.frame(table(inventory[['Topic group']]), responseName = 'n_projects')
names(d)[1] <- 'topic_group'
d <- d[order(-d$n_projects, d$topic_group), ]
d$percentage <- 100 * d$n_projects / sum(d$n_projects)
d$legend <- factor(d$topic_group, levels = d$topic_group)
palette <- colorRampPalette(as.character(paletteer_d("ggthemes::excel_Blue")))(nrow(d))
p <- ggplot(d, aes(x = 1, y = n_projects, fill = legend)) +
  geom_col(width = 1, color = 'white', linewidth = 0.5) +
  geom_text(aes(x = 1.22, label = sprintf('%.1f%%', percentage)),
            position = position_stack(vjust = 0.5), size = 3.4) +
  scale_x_continuous(limits = c(0.5, 1.5), expand = c(0, 0)) +
  coord_polar(theta = 'y', start = pi / 4) +
  scale_fill_manual(values = palette) +
  labs(title = 'Student projects by primary topic',
       fill = NULL) +
  theme_void(base_size = 11) +
  theme(plot.title = element_text(face = 'bold'), legend.position = 'left',
        legend.text = element_text(size = 10), plot.caption = element_text(hjust = 0, size = 8),
        plot.margin = margin(10, 15, 10, 10))
ggsave('figures/project_topics.pdf', p, width = 9.3, height = 5.2, device = 'pdf')
