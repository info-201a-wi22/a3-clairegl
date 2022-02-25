incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(leaflet)
#install.packages(maps)
library(maps)
#install.packages("usmap")
library(usmap)

#INTRODUCTION + SUMMARY INFO

# Then, you will share at least 5 relevant values of interest. 
# These will likely be calculated using your DPLYR skills, answering questions such as: 
#   What is the average value of my variable across all the counties (in the current year)?
#   Where is my variable the highest / lowest?
#   How much has my variable change over the last N years?

#minimized data
incarceration_data_small <- incarceration_data %>% 
  group_by(year) %>% #NOT WORKING??!?!?
  select(year, state, aapi_jail_pop, white_jail_pop, total_jail_pop) %>% 
  na.omit()

#ca data
ca_data <- incarceration_data %>% 
  filter(state == "CA") %>% 
  group_by(year) %>% 
  select(year, aapi_jail_pop, white_jail_pop, total_jail_pop) %>% 
  na.omit()

#santa clara county data
santa_clara_data <- incarceration_data %>% 
  filter(state == "CA") %>% 
  filter(county_name == "Santa Clara County") %>% 
  group_by(year) %>% 
  select(year, aapi_jail_pop, white_jail_pop, total_jail_pop) %>% 
  na.omit()

#summary info
aapi_mean <- mean(incarceration_data_small$aapi_jail_pop)
aapi_mean

state_most_aapi <- incarceration_data %>% 
  filter(aapi_jail_pop == max(aapi_jail_pop, na.rm = TRUE)) %>% 
  select("state") %>% 
  pull(state)
state_most_aapi

aapi_mean_ca <- mean(ca_data$aapi_jail_pop)

county_most_aapi <- incarceration_data %>% 
  filter(aapi_jail_pop == max(aapi_jail_pop, na.rm = TRUE)) %>% 
  select("county_name") %>% 
  pull(county_name)
county_most_aapi

la_data <- incarceration_data %>% 
  filter(state == "CA") %>% 
  filter(county_name == "Los Angeles County") %>% 
  group_by(year) %>% 
  select(year, aapi_jail_pop, white_jail_pop, total_jail_pop) %>% 
  na.omit()

aapi_mean_la <- mean(la_data$aapi_jail_pop)
aapi_mean_la

aapi_mean_sc <- mean(santa_clara_data$aapi_jail_pop)
aapi_mean_sc


#TRENDS OVER TIME CHART

# The first chart that you'll create and include will show the trend over time of your variable/topic. 
# Think carefully about what you want to communicate to your user 
# (you may have to find relevant trends in the dataset first!). 
# Here are some requirements to help guide your design:
#   Show more than one, but fewer than ~10 lines: your graph should compare the trend 
#     of your measure over time. This may mean showing the same measure for different locations, 
#     or different racial groups. Think carefully about a meaningful comparison of locations 
#     (e.g., the top 10 counties in a state, top 10 states, etc.)
#   You must have clear x and y axis labels,
#   The chart needs a clear title 
#   You need a legend for your different line colors, and a clear legend title

#g1 data wrangling
graph1_data <- incarceration_data %>% 
  filter(state == "CA") %>% 
  filter(county_name == "Santa Clara County") %>% 
  group_by(year) %>% 
  select(year, aapi_jail_pop, white_jail_pop, latinx_jail_pop, total_jail_pop) %>% 
  na.omit()

#g1 code
#https://www.datanovia.com/en/blog/how-to-create-a-ggplot-with-multiple-lines/
#https://aosmith.rbind.io/2018/07/19/manual-legends-ggplot2/
graph1 <- ggplot(graph1_data,
                 mapping = aes(x = year)) +
  geom_line(aes(y = aapi_jail_pop, color = "#FF6B35")) +
  geom_line(aes(y = white_jail_pop, color = "#559CAD")) +
  geom_line(aes(y = latinx_jail_pop, color = "#8C5383")) +
  geom_line(aes(y = total_jail_pop, color = "black")) +
  labs(title = "Population of AAPIs VS Whites Vs Latinx jailed in Santa Clara County 1985 - 2018",
       x = "Year",
       y = "Population Jailed",
       color = "race") +
  scale_color_identity(name = "Racial Group",
                       breaks = c("#FF6B35", "#559CAD", "#8C5383", "black"),
                       labels = c("AAPI", "White", "Latinx", "County Total"),
                       guide = "legend")


graph1

# VARIABLE COMPARISON CHART

# The second chart that you'll create and include will show how two different (continuous) variables
# are related to one another. Again, think carefully about what such a comparison means, 
# and want to communicate to your user (you may have to find relevant trends in the dataset first!). 
# Here are some requirements to help guide your design:
#   You must have clear x and y axis labels,
#   The chart needs a clear title 
#   If you choose to add a color encoding (not required), 
#     you need a legend for your different color and a clear legend title

graph2_data_ca <- incarceration_data %>% 
  filter(state == "CA") %>% 
  filter(year == "2018") %>% 
  select(year, aapi_jail_pop, aapi_pop_15to64)

graph2_data_sc <- incarceration_data %>% 
  filter(state == "CA") %>% 
  filter(year == "2018") %>% 
  filter(county_name == "Santa Clara County") %>%
  select(year, aapi_jail_pop, aapi_pop_15to64)

graph2_data_whole <- incarceration_data %>% 
  filter(year == "2018") %>% 
  select(year, aapi_jail_pop, aapi_pop_15to64) %>% 
  na.omit()

graph2 <- ggplot() +
          geom_point(graph2_data_whole, 
                     mapping = aes(x = aapi_jail_pop, y = aapi_pop_15to64, color = "#FF6B35")) +
          geom_point(graph2_data_ca, 
                     mapping = aes(x = aapi_jail_pop, y = aapi_pop_15to64, color = "#559CAD")) +
          geom_point(graph2_data_sc, 
                    mapping = aes(x = aapi_jail_pop, y = aapi_pop_15to64, color = "#8C5383")) +
          labs(title ="Population of AAPI Jailed vs Population of Total AAPI",
               x = "Total AAPI Jailed Population", y = "AAPI Jailed Population Age 15-64") +
          scale_color_identity(name = "Location",
                      breaks = c("#FF6B35", "#559CAD", "#8C5383"),
                      labels = c("Nation", "California", "Santa Clara County"),
                      guide = "legend")


graph2

#MAP

# The last chart that you'll create and include will show how a variable is distributed geographically.
# Again, think carefully about what such a comparison means, and want to communicate to your user 
# (you may have to find relevant trends in the dataset first!). 
# Here are some requirements to help guide your design:
#   Your map needs a title
#   Your color scale needs a legend with a clear label
#   Use a map based coordinate system to set the aspect ratio of your map (see reading)
#   Use a minimalist theme for the map (see reading)

#map data wrangling
map_data <- incarceration_data %>% 
  select(year, state, county_name, aapi_jail_pop) %>% 
  filter(year == "2018") %>% 
  replace_na(list(aapi_jail_pop = 0)) %>% 
  group_by(state) %>% 
  mutate(mean = mean(aapi_jail_pop)) %>% 
  distinct(mean)

#map code
#https://jtr13.github.io/cc19/different-ways-of-plotting-u-s-map-in-r.html
map <- plot_usmap(data = map_data,
                  values = "mean") +
      scale_fill_continuous(low = "#FAF3DD", 
                  high = "#559CAD", 
                  name = "Average AAIP Persons Jailed", 
                  limits = c(0, 36)) +
      labs(title = "Average of AAPI Persons Jailed By State In 2018") +
      theme(legend.position = "right")

map



