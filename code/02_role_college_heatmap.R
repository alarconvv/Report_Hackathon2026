# Run from the report root: Rscript code/02_role_college_heatmap.R
library(ggplot2)
library(paletteer)
colors <- as.character(paletteer_d("ggthemes::excel_Blue"))
mapping <- read.csv('data/registration_college_mapping.csv')
all_roles <- read.csv('data/registration_by_role.csv')
colleges <- c('College of Science & Mathematics', 'College of Management',
              'College of Liberal Arts', 'College of Education & Human Development',
              'Manning College of Nursing & Health Sciences', 'School for the Environment',
              'No college assigned')
labels <- c('Science &\nMathematics', 'Management', 'Liberal Arts',
            'Education &\nHuman Development', 'Manning Nursing &\nHealth Sciences',
            'School for the\nEnvironment', 'No college\nassigned')
stopifnot(sum(mapping$responses) == 132, !anyNA(mapping[c('role', 'college', 'responses', 'umb_affiliated')]),
          all(mapping$responses >= 0), all(mapping$responses == floor(mapping$responses)),
          all(mapping$college %in% colleges), all(mapping$role %in% all_roles$role), all(mapping$umb_affiliated %in% c('Yes', 'No')))
outside_count <- sum(mapping$responses[mapping$umb_affiliated == 'No'])
stopifnot(outside_count == 10)
staff_count <- sum(mapping$responses[mapping$umb_affiliated == 'Yes' & mapping$role == 'Staff'])
stopifnot(staff_count == 11)
mapping <- mapping[mapping$umb_affiliated == 'Yes' & mapping$role != 'Staff', ]
colleges <- colleges[colleges != 'No college assigned']
labels <- labels[seq_along(colleges)]
stopifnot(all(mapping$college %in% colleges))
roles <- aggregate(responses ~ role, mapping, sum)
names(roles)[2] <- 'registrants'
roles <- roles[match(all_roles$role[all_roles$role %in% roles$role], roles$role), ]
stopifnot(sum(roles$registrants) == 111, !any(c('External guest', 'Staff') %in% roles$role))
totals <- aggregate(responses ~ role + college, mapping, sum)
grid <- expand.grid(role = roles$role, college = colleges)
d <- merge(grid, totals, by = c('role', 'college'), all.x = TRUE, sort = FALSE)
d$responses[is.na(d$responses)] <- 0
role_totals <- tapply(d$responses, d$role, sum)
stopifnot(nrow(d) == nrow(roles) * length(colleges), sum(d$responses) == 111,
          all(role_totals[roles$role] == roles$registrants),
          !anyDuplicated(d[c('role', 'college')]))
d$role <- factor(d$role, levels = rev(roles$role))
d$college <- factor(d$college, levels = colleges)
p <- ggplot(d, aes(x = college, y = role, fill = responses)) +
  geom_tile(color = 'white', linewidth = 0.8) +
  geom_text(aes(label = responses, color = responses > max(responses) / 2), size = 3.8) +
  scale_color_manual(values = c('FALSE' = '#243447', 'TRUE' = 'white'), guide = 'none') +
  scale_fill_gradient(low = 'white', high = colors[1], name = 'Responses') +
  scale_x_discrete(labels = setNames(labels, colleges), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = 'Registration by role and UMass Boston college',
       x = 'UMass Boston colleges and schools', y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(), axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 10), plot.title = element_text(face = 'bold'),
        legend.position = 'right', plot.caption = element_text(hjust = 0, size = 8),
        plot.margin = margin(10, 15, 10, 10))
ggsave('figures/registration_role_college_heatmap.pdf', p, width = 10.5, height = 5.4, device = 'pdf')
