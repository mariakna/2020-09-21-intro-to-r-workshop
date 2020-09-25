
Thursday <- c("selecting",
              "filtering", 
              "pipes",  
              "split-apply-combine",
              "reshape")

#------------------
# Lets get started!
#------------------
install.packages("tidyverse") 
library(tidyverse) # load the library 'tidyverse'

# dplyr used to manipulate data
# tidyr used for reshaping data

# Load the dataset
surveys <- read_csv("data_raw/portal_data_joined.csv")

# Check structure
str(surveys)



#-----------------------------------
# Selecting columns & filtering rows
#-----------------------------------
# selects columns plot_id, species_id and weight from data 'surveys'
select(surveys, plot_id, species_id, weight) 

# selects all columns except record_id and species_id
select(surveys, -record_id, -species_id) 

# Filter for a particular year
surveys_1995 <- filter(surveys, year == 1995)

surveys2 <- filter(surveys, weight < 5)
surveys_sml <- select(surveys2, species_id, sex, weight)
surveys_sml <- select(filter(surveys, weight < 5), species_id, sex, weight)



#-------
# Pipes
#-------
# The pipe --> %>%
# Shortcut --, Ctrl _ shift + m or command + shift + m
surveys %>% 
  filter(weight < 5) %>% 
  select(species_id, sex, weight)





# Challenge 1
# Using pipes, subset the ```surveys``` data to include animals collected before 1995 and
# retain only the columns ```year```, ```sex```, and ```weight```.

# Suggested Answers:
surveys_1995 <- surveys %>% 
  filter(year < 1995) %>% 
  # Ordering your columns does matter
  select(year, sex, weight)

animals_1995 <- surveys %>% 
  filter(year < 1995) %>% 
  select(year, sex, weight)

surveys %>% 
  filter(year <1995) %>% 
  select(year, sex, weight)


surveys %>%  select(year, sex, weight) %>%   filter(year < 1995)

#--------
# Mutate
#--------
# add new columns weight_kg and weight_lb
surveys_weights <- surveys %>% 
  mutate(weight_kg = weight / 1000,
         weight_lb = weight_kg * 2.2)

surveys %>% 
  mutate(weight_kg = weight / 1000,
         weight_lb = weight_kg * 2.2) %>% 
  head()

# Remove na's in weight column then add new col weight_kg
surveys %>% 
  filter(!is.na(weight)) %>% 
  mutate(weight_kg = weight/1000) %>% 
  head()

#-----------
# CHALLENGE
#-----------

# Create a new data frame from the ```surveys``` data that meets the following criteria: 
# contains only the ```species_id``` column and a new column called ```hindfoot_cm``` containing 
# the ```hindfoot_length``` values converted to centimeters. In this hindfoot_cm column, 
# there are no ```NA```s and all values are less than 3.

# Hint: think about how the commands should be ordered to produce this data frame!

# Place answers below

new_dataframe <- surveys %>% 
  filter(!is.na(hindfoot_length)) %>% 
  mutate(hindfoot_cm = hindfoot_length / 10) %>% 
  select(species_id, hindfoot_cm) %>% 
  filter(hindfoot_cm < 3)

surveys %>% 
  filter(!is.na(hindfoot_length)) %>% 
  mutate(hindfoot_cm = hindfoot_length / 10) %>%
  filter(hindfoot_cm < 3) %>% 
  select(species_id, hindfoot_cm)

new_dataframe <- surveys %>% 
  filter(!is.na(hindfoot_length), hindfoot_length < 30) %>% 
  mutate(hindfoot_cm = hindfoot_length / 10) %>% 
  select(species_id, hindfoot_cm)









#---------------------
# Split-apply-combine
#---------------------
# Summarise weight

# Group weight by sex after removing NAs in weight column 
surveys %>% 
  group_by(sex) %>% 
  summarise(mean_weight = mean(weight, na.rm = TRUE))

summary(surveys)
?summarize # find out information on summarize

surveys$sex <- as.factor(surveys$sex)

# Summarise weights by sex and species_id afte removing 
# nas from sex and weight.
surveys %>% 
  filter(!is.na(weight), !is.na(sex)) %>% 
  group_by(sex, species_id) %>% 
  summarise(mean_weight = mean(weight)) %>% 
  print(n=20) # print 20 rows to console


# Group the weights by sex and species_id
# Order the table by min_weight and in descending order
surveys %>% 
  filter(!is.na(weight), !is.na(sex)) %>% 
  group_by(sex, species_id) %>% 
  summarise(mean_weight = mean(weight),
            min_weight = min(weight)) %>% 
  arrange(desc(min_weight))

# Obtain the counts of each sex
surveys %>% 
  group_by(sex) %>% 
  summarise(count = n())

surveys_new <- surveys %>% 
  group_by(sex, species, taxa) %>% 
  summarise(count = n())








#-----------
# CHALLENGE
#-----------

# 1. How many animals were caught in each ```plot_type``` surveyed?


surveys %>% count(plot_type)

num_animals <- surveys %>% 
  group_by(plot_type) %>% 
  summarise(number_of_animals = n())

# 2. Use ```group_by()``` and ```summarize()``` to find the mean, min, and max hindfoot length 
#    for each species (using ```species_id```). Also add the number of observations 
#    (hint: see ```?n```).

hindfoot_info <- surveys %>%
  filter(!is.na(hindfoot_length)) %>% 
  group_by(species_id) %>% 
  summarise(mean_length = mean(hindfoot_length),
            min_length = min(hindfoot_length), 
            max_length = max(hindfoot_length),
            count = n())


# 3. What was the heaviest animal measured in each year? 
#    Return the columns ```year```, ```genus```, ```species_id```, and ```weight```.


surveys %>% 
  filter(!is.na(weight)) %>%
  group_by(year) %>%
  filter(weight == max(weight)) %>% 
  select(year, genus, species, weight) %>%
  arrange(year) %>% 
  distinct() # distinct is used to remove duplicate rows

surveys %>% 
  select(year, genus, species_id, weight) %>%  
  group_by(year) %>% 
  top_n(1, weight) %>% 
  arrange(year) %>% 
  distinct()



#-----------
# Reshaping
#-----------

# Spread: convert long data to wide data
# Gather: convert wide data to long data

surveys_gw <- surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(plot_id, genus) %>% 
  summarise(mean_weight = mean(weight))

surveys_wider <- surveys_gw %>% 
  spread(key = genus, value = mean_weight)

str(surveys_wider)

surveys_gather <- surveys_wider %>% 
  gather(key = genus, value = mean_weight, -plot_id)

surveys_wider %>% 
  gather(key = genus, value = mean_weight, Baiomys:Spermophilus)



#-----------
# CHALLENGE
#-----------
# 1. Spread the surveys data frame with year as columns, plot_id as rows,
#	and the number of genera per plot as the values. You will need to summarize before reshaping,
#	and use the function n_distinct() to get the number of unique genera within a particular chunk of data.
#	It’s a powerful function! See ?n_distinct for more.
surveys_spread_genera <- surveys %>% 
  group_by(plot_id, year) %>% 
  summarise(n_genera = n_distinct(genus)) %>% 
  spread(year, n_genera)

head(surveys_spread_genera)


# 2. Now take that data frame and gather() it again, so each row is a unique plot_id by year combination.
surveys_spread_genera2 <- surveys_spread_genera %>% 
  gather(key = year, value = n_genera, -plot_id)

# 3. The surveys data set has two measurement columns: hindfoot_length and weight.
#	This makes it difficult to do things like look at the relationship between mean values of each
#	measurement per year in different plot types. Let's walk through a common solution for this type of problem.
#	First, use gather() to create a dataset where we have a key column called measurement and a value column that
#	takes on the value of either hindfoot_length or weight.
#	Hint: You'll need to specify which columns are being pivoted.

surveys_long <- surveys %>% 
  gather("measurement", "value", hindfoot_length, weight)



# 4. With this new data set, calculate the average of each measurement in each year for each different plot_type.
#	Then spread() them into a data set with a column for hindfoot_length and weight.
#	Hint: You only need to specify the key and value columns for spread().


surveys_long2 <- surveys_long %>% 
  group_by(year, measurement, plot_type) %>% 
  summarise(mean_value = mean(measurement, na.rm = TRUE)) %>% 
  spread(measurement, mean_value)

tail(surveys_long2)






#----------------
# Exporting data
#----------------

write_csv(surveys_long2, path = "data_out/surveys_long2.csv")




