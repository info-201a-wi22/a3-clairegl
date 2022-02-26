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


#summary data wrangling
incarceration_data_small <- incarceration_data %>% 
  group_by(year) %>% #NOT WORKING??!?!?
  select(year, state, aapi_jail_pop, white_jail_pop, total_jail_pop) %>% 
  na.omit()

ca_data <- incarceration_data %>% 
  filter(state == "CA") %>% 
  group_by(year) %>% 
  select(year, aapi_jail_pop, white_jail_pop, total_jail_pop) %>% 
  na.omit()

santa_clara_data <- incarceration_data %>% 
  filter(state == "CA") %>% 
  filter(county_name == "Santa Clara County") %>% 
  group_by(year) %>% 
  select(year, aapi_jail_pop, white_jail_pop, total_jail_pop) %>% 
  na.omit()

#summary info code
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

num_most_aapi <- incarceration_data %>% 
  filter(aapi_jail_pop == max(aapi_jail_pop, na.rm = TRUE)) %>% 
  select("aapi_jail_pop") %>% 
  pull(aapi_jail_pop)
num_most_aapi

year_most_aapi <- incarceration_data %>% 
  filter(aapi_jail_pop == max(aapi_jail_pop, na.rm = TRUE)) %>% 
  select("year") %>% 
  pull(year)
year_most_aapi

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

year_range <- max(santa_clara_data$year) - min(santa_clara_data$year)


#TRENDS OVER TIME CHART


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
  labs(title = "Jailed Population Of Different Groups In Santa Clara County From 1985 - 2018",
       x = "Year",
       y = "Population Jailed",
       color = "race") +
  scale_color_identity(name = "Racial Group",
                       breaks = c("#FF6B35", "#559CAD", "#8C5383", "black"),
                       labels = c("AAPI", "White", "Latinx", "County Total"),
                       guide = "legend")


graph1


# VARIABLE COMPARISON CHART


#g2 data wrangling
graph2_data_ca <- incarceration_data %>% 
  filter(state == "CA") %>% 
  filter(year == "2018") %>% 
  select(year, aapi_jail_pop, aapi_pop_15to64,county_name)

graph2_data_sc <- incarceration_data %>% 
  filter(state == "CA") %>% 
  filter(year == "2018") %>% 
  filter(county_name == "Santa Clara County") %>%
  select(year, aapi_jail_pop, aapi_pop_15to64, county_name)

graph2_data_whole <- incarceration_data %>% 
  filter(year == "2018") %>% 
  select(year, aapi_jail_pop, aapi_pop_15to64, county_name, state) %>% 
  na.omit()

#g2 code
graph2 <- ggplot() +
          geom_point(graph2_data_whole, 
                     mapping = aes(x = aapi_jail_pop, y = aapi_pop_15to64, color = "#FF6B35")) +
          geom_point(graph2_data_ca, 
                     mapping = aes(x = aapi_jail_pop, y = aapi_pop_15to64, color = "#559CAD")) +
          geom_point(graph2_data_sc, 
                    mapping = aes(x = aapi_jail_pop, y = aapi_pop_15to64, color = "#8C5383")) +
          labs(title ="Population of AAPI Jailed vs Population of Total AAPI In 2018",
               x = "Total AAPI Jailed Population", y = "AAPI Population Age 15-64") +
          scale_color_identity(name = "Location",
                      breaks = c("#FF6B35", "#559CAD", "#8C5383"),
                      labels = c("Nation", "California", "Santa Clara County"),
                      guide = "legend")


graph2


#MAP


#map data wrangling w fips + long lat coords
test_data <- incarceration_data %>% 
  filter(year == "2018")

#map data joining
county_united <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",")

county_shapes <- county_united  %>%
  left_join(county.fips, by="polyname")

map_data2 <- county_shapes %>%
  left_join(test_data, by="fips") %>% 
  filter(state == "CA")

#minimalist theme
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(), # remove axis lines
    axis.text = element_blank(), # remove axis labels
    axis.ticks = element_blank(), # remove axis ticks
    axis.title = element_blank(), # remove axis titles
    plot.background = element_blank(), # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank() # remove border around plot
  )

#map code
map <- ggplot(map_data2) + 
             geom_polygon(
                mapping = aes(x = long, y = lat, group = group, fill = aapi_jail_pop),
                color = "gray", size = 0.3) + 
              coord_map() + 
              scale_fill_continuous(limits = c(0, max(map_data2$aapi_jail_pop)), 
                                    na.value = "white", low = "#FAF3DD", high = "#559CAD",
                                    name = "AAIP Persons Jailed") + 
              blank_theme +
              ggtitle("AAPI Jailed Population of California Counties In 2018")

map





