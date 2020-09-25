remove.packages("tidyr")
install.packages("tidyr", repos = "http://cran.us.r-project.org")

## Pivoting data (convert between wide and long data)
# demonstration by Belinda Fabian on local RStudio with tidyr v1.0.0

# set up the environment
library(tidyverse)
setwd("~/Desktop/Intro_to_R")

# download and load the data

# download.file(url = "https://ndownloader.figshare.com/files/2292169",
#               destfile = "portal_data_joined.csv")

surveys <- read.csv("portal_data_joined.csv")


# get the data we want to pivot

surveys_gw <- surveys %>%
  filter(!is.na(weight)) %>%
  group_by(plot_id, genus) %>%
  summarize(mean_weight = mean(weight))


# go from long to wide data

surveys_wide <- surveys_gw %>% 
  pivot_wider(names_from = genus, values_from = mean_weight)

surveys_wide


# go from wide to long data

surveys_long <- surveys_wide %>% 
  pivot_longer(!plot_id, names_to = "genus", values_to = "mean_weight")

surveys_long
