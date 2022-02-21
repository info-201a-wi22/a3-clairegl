incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
library(dplyr)
library(tidyverse)
library(ggplot2)
library(leaflet)

#INTRODUCTION + SUMMARY INFO

# Then, you will share at least 5 relevant values of interest. 
# These will likely be calculated using your DPLYR skills, answering questions such as: 
#   What is the average value of my variable across all the counties (in the current year)?
#   Where is my variable the highest / lowest?
#   How much has my variable change over the last N years?


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
  filter(county_name == "Santa Clara County") %>% 
  group_by(year) %>% 
  select(year, aapi_jail_pop, white_jail_pop) %>% 
  replace_na(list(aapi_jail_pop = 0, white_jail_pop = 0))
  

#g1 code
graph1 <- ggplot(graph1_data,
                 mapping = aes(x = year)) +
          geom_line(aes(y = aapi_jail_pop), color = "red") +
          geom_line(aes(y = white_jail_pop), color = "blue") +
          ggtitle("Population of AAPIs VS Whites jailed in Santa Clara County over time") +
          xlab("Year") + ylab("Population jailed")
          #HOW TO GET LEGEND --> not based on buckets 
          #MAYBE ADD MORE LINES lol
          #ALSO ASK ABT RMD RENDERING
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


#MAP

# The last chart that you'll create and include will show how a variable is distributed geographically.
# Again, think carefully about what such a comparison means, and want to communicate to your user 
# (you may have to find relevant trends in the dataset first!). 
# Here are some requirements to help guide your design:
#   Your map needs a title
#   Your color scale needs a legend with a clear label
#   Use a map based coordinate system to set the aspect ratio of your map (see reading)
#   Use a minimalist theme for the map (see reading)




