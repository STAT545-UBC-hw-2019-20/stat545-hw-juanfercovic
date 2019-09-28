#Assignment 2
#install.packages("gapminder")
#install.packages("dplyr")    # alternative installation of the %>%
#install.packages('tidyverse')
#install.packages("magrittr")
#install.packages("qwraps2")
#install.packages("rmarkdown")

library(gapminder)
library(tidyverse)
library(dplyr)
library(magrittr)
library(qwraps2)
library(ggplot2)
library(rmarkdown)

gapminder

## 1.1 and 1.2
#Use filter() to subset the gapminder data to three countries of your choice in the 1970’s.
#Use the pipe operator %>% to select “country” and “gdpPercap” from your filtered dataset in 1.1.

subset <- filter(gapminder, country %in% c("Canada", "Mexico", "United States"), year > 1969,  year < 1981)
subset %>% select(country,gdpPercap)

#1.3 all entries that have experienced a drop in life expectancy.
le_drop <- select(gapminder, country, year, continent, lifeExp) %>%
  group_by(country) %>%
  mutate(le_delta = lifeExp - lag(lifeExp))
  
  filter(le_drop, le_delta<0)


#1.4a max GDP per capita experienced by each country.
#  Filter gapminder so that it shows the max GDP per capita experienced by each country.
  gapminder %>%
  select(country, year, continent, gdpPercap) %>%
  arrange(country) %>%
  group_by(country) %>%
  top_n(1, wt = desc(gdpPercap)) ## gets the max


#1.5 scatterplot of Canada’s life expectancy vs. GDP per capita using ggplot2
  canada <- filter(gapminder, country %in% c("Canada")) 
ggplot(data=canada, aes(x=log(gdpPercap),y=lifeExp)) + geom_point()  

filter(gapminder, country %in% c("Canada")) %>%
ggplot(aes(x=log(gdpPercap),y=lifeExp)) + geom_point()


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
              "max" = ~ max(gapminder$year)
              ))
#Overall summary
summary_table(gapminder, summary_lifeexp)

#Group by Continent
summary_table(dplyr::group_by(gapminder, continent), summary_lifeexp)



#Exercise 3: Explore various plot types

#A scatterplot of two quantitative variables.

#Average Life expectancy in time by continent
gapminder %>%
  group_by(continent, year) %>%
  summarise(lifeExp=mean(lifeExp)) %>%
  ggplot(aes(x=year, y=lifeExp, color=continent)) +
  geom_line(size=1) + 
  geom_point(size=1.5) +
  geom_smooth(method="loess")

#Life expectancy in time by continent
gapminder %>%
ggplot(aes(year, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  theme(legend.position = 'none')


#One other plot besides a scatterplot.
gapminder %>%
ggplot(aes(x=continent, fill=continent)) + 
  geom_bar(aes(y=..count../12)) +
  labs(y="Number of countries") +
  guides(fill=FALSE)

#4 Recycling
filter(gapminder, country == c("Rwanda", "Afghanistan"))

filter(gapminder, country %in% c("Rwanda", "Afghanistan"))
##Check the diff between == and %in%


#Tibble display











  