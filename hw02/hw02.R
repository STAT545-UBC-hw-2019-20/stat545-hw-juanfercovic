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


#1.4b six rows: the rows with the three largest GDP per capita, and the rows with the three smallest GDP per capita


#1.5 scatterplot of Canada’s life expectancy vs. GDP per capita using ggplot2
  canada <- filter(gapminder, country %in% c("Canada"))
ggplot(data=canada,aes(x=log(gdpPercap),y=lifeExp)) + geom_point()  



#Exercise 2: Explore individual variables with dplyr


#I will use Gapminder, categorical variable country and numerical life expentancy
ex2 <- select(gapminder,year,country,lifeExp)
print(ex2)

#What are possible values (or range, whichever is appropriate) of each variable?
ex2 <- select(gapminder,year,country,lifeExp) %>%
dplyr::select(country)

#What values are typical? What’s the spread? What’s the distribution? Etc., tailored to the variable at hand.


















  