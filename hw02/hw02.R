#Assignment 2
install.packages("gapminder")
library(gapminder)
install.packages("dplyr")    # alternative installation of the %>%
install.packages('tidyverse')
library(tidyverse)
gapminder

#1.1
subset <- filter(gapminder, country %in% c("Canada", "Mexico", "United States"), year > 1969,  year < 1981)

#1.2
subset %>% select(country,gdpPercap)

#1.3 all entries that have experienced a drop in life expectancy.
le_drop <- select(gapminder, country, year, continent, lifeExp) %>%
  group_by(country) %>%
  mutate(le_delta = lifeExp - lag(lifeExp))
  
  filter(le_drop, le_delta>0)


#1.4a max GDP per capita experienced by each country.
max_gdp <-
  select(gapminder, country, year, continent, gdpPercap) %>%
  group_by(country) %>%
  summarize(max_gdp_country = max(gdpPercap, na.rm = TRUE)) %>% 




  
  

  