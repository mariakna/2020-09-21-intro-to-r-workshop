## data visualisation (folder data_raw)

library('tidyverse')
surveys_complete <- read_csv('surveys_complete.csv')
ggplot(data = surveys_complete)
ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length))
ggplot(data = surveys_complete, aes(x = weight, y = hindfoot_length)) +
  geom_point()

# Assign plot to a variable
surveys_plot <- ggplot(data = surveys_complete, 
                       mapping = aes(x = weight, y = hindfoot_length))

# Draw the plot
surveys_plot + 
  geom_point()

# challenge 1
ggplot(data = surveys_complete, aes(x = hindfoot_length, y = weight)) +
  geom_point()

surveys_plot + 
  geom_point(aes(x = hindfoot_length, y = weight))

# challenge 2
ggplot(data = surveys_complete, aes(x = weight)) +
  geom_histogram(binwidth = 3)

# customising
ggplot(data = surveys_complete, aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.5)

ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, color = "blue")

ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, aes(color = species_id))

# challenge 3
ggplot(data = surveys_complete, 
       mapping = aes(x = species_id, y = weight)) +
  geom_point(aes(color = plot_type))

# challenge 4
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_jitter(alpha = 0.3, color = "tomato") +
  geom_boxplot(alpha = 0)

# challenge 5
ggplot(data = surveys_complete,
       mapping = aes(x = species_id, y = weight))+
  geom_violin(alpha = 0, colour = "tomato")

# challenge 6
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = hindfoot_length)) +
  geom_jitter(alpha = 0.3, aes(color = as.factor(plot_id))) +
  geom_boxplot(alpha = 0)

# challenge 7
# Make a scatter plot of species_id on the x-axis and weight on the y-axis with a log10 scale
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_jitter(alpha = 0.3, aes(color = as.factor(plot_id))) +
  scale_y_log10() 
# or +
#  scale_y_continuous(trans='log10')

# plotting time series data

# counts per year for each genus
yearly_counts <- surveys_complete %>%
  count(year, genus)

# visualise
ggplot(data = yearly_counts, aes(x = year, y = n)) +
  geom_line()

# separate line for each genus
ggplot(data = yearly_counts, aes(x = year, y = n, group = genus)) +
  geom_line()

# challenge 8
ggplot(data = yearly_counts, aes(x = year, y = n, color = genus)) +
  geom_line()

yearly_counts %>% 
  ggplot(mapping = aes(x = year, y = n, color = genus)) +
  geom_line()

yearly_counts_graph <- surveys_complete %>%
  count(year, genus) %>% 
  ggplot(mapping = aes(x = year, y = n, color = genus)) +
  geom_line()
yearly_counts_graph

ggplot(data = yearly_counts, aes(x = year, y = n)) +
  geom_line() +
  facet_wrap(facets = vars(genus))

ggplot(data = yearly_counts, aes(x = year, y = n)) +
  geom_line() +
  facet_wrap(~genus)

# split the line in each plot by the sex of each individual measured
yearly_sex_counts <- surveys_complete %>%
  count(year, genus, sex)

ggplot(data = yearly_sex_counts, mapping = aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(facets =  vars(genus), ncol = 2)

# One column, facet by rows
ggplot(data = yearly_sex_counts, 
       mapping = aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_grid(rows = vars(genus))

# One row, facet by column
ggplot(data = yearly_sex_counts, 
       mapping = aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_grid(cols = vars(genus))

ggplot(data = yearly_sex_counts, 
       mapping = aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_grid(rows = vars(sex), cols = vars(genus))

# challenge 9
ggplot(data = yearly_sex_counts, 
       mapping = aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_grid(cols = vars(genus))

# themes
ggplot(data = yearly_sex_counts, 
       mapping = aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(vars(genus)) +
  theme_bw()

# challenge 10
# create a plot that depicts how the 
# average weight of each species changes through the years.
#
# Hint: need to do a group_by() and summarize() to get the data
# before plotting

yearly_weight <- surveys_complete %>%
  group_by(year, species_id) %>%
  summarize(avg_weight = mean(weight))

yearly_weight %>%
ggplot(aes(x=year, y=avg_weight, color = species_id)) +
  geom_line() +
  facet_wrap(~species_id) +
  theme_classic()

# more customisation
yearly_sex_counts %>%
  ggplot(aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(~genus) +
  labs(title = "Observed genera through time",
       x = "Year of observation",
       y = "Number of individuals") +
  theme_classic() +
  theme(text = element_text(size = 12),
        axis.text.x = element_text(color = "grey20", size = 10, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(color = "grey20", size = 10),
        strip.text = element_text(face = "italic"))
    
grey_theme <- theme(text = element_text(size = 12),
                    axis.text.x = element_text(color = "grey20", size = 10, angle = 90, hjust = 0.5, vjust = 0.5),
                    axis.text.y = element_text(color = "grey20", size = 10),
                    strip.text = element_text(face = "italic"))

yearly_sex_counts %>%
  ggplot(aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(~genus) +
  labs(title = "Observed genera through time",
       x = "Year of observation",
       y = "Number of individuals") +
  theme_classic() + grey_theme

# exporting a plot

# default - saves the last plot that was generated
ggsave("figures/my_plot.pdf", width = 15, height = 10, dpi = 300)
