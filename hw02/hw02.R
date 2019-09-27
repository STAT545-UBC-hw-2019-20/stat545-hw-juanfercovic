#Assignment 2
install.packages("gapminder")
install.packages("dplyr")    # alternative installation of the %>%
install.packages('tidyverse')
install.packages("magrittr")
install.packages("qwraps2")
library(gapminder)
library(tidyverse)
library(dplyr)
library(magrittr)
library(qwraps2)
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
#What values are typical? What’s the spread? What’s the distribution? Etc., tailored to the variable at hand.

#I will use Gapminder, categorical variable country and numerical life expentancy
ex2 <- select(gapminder,year,country,lifeExp)

print(ex2)

#number of countries by continent included in the dataset (for the year 2002)
gapminder %>%
  filter(year == 2002) %>%
  count(continent, sort = TRUE)


#Years in the sample
gapminder %>%
  count(year, sort = TRUE)


#summary table for Life Exp.
args(summary_table)

summary_lifeexp <-
  list("Life Expentancy" =
         list("min" = ~ min(gapminder$lifeExp),
              "max" = ~ max(gapminder$lifeExp),
              "mean" = ~ mean(gapminder$lifeExp),
              "sd" = ~ sd(gapminder$lifeExp)),
       "Years" = 
         list("min" = ~ min(gapminder$year),
              "max" = ~ max(gapminder$year),
              "N" = ~ add_count(gapminder$year,)))

table <- summary_table(gapminder, summary_lifeexp)

#Overall summary
table
#Group by Continent
table_cont <- summary_table(dplyr::group_by(gapminder, continent), summary_lifeexp)



#Exercise 3: Explore various plot types

#A scatterplot of two quantitative variables.

#One other plot besides a scatterplot.

#4 Recycling
filter(gapminder, country == c("Rwanda", "Afghanistan"))

filter(gapminder, country %in% c("Rwanda", "Afghanistan"))
##Check the diff between == and %in%


#Tibble display













  