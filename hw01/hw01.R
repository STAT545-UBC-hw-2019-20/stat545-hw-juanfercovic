
##Install packages

#install.packages('rmarkdown')
#install.packages('gapminder')
#install.packages('tibble')
#install.packages('DT')
#install.packages("dslabs")
#install.packages('tidyverse')
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
install.packages("ggplot2")
install.packages("colorspace")
install.packages("percentChange")
install.packages('tidyverse')
install.packages('tfplot')
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%




data(package="dslabs")
list.files(system.file("script", package = "dslabs"))

##first table of summary stats
library("dslabs")
data("greenhouse_gases")
co2=greenhouse_gases[,1:2]
greenhouse_gases_co2 <- greenhouse_gases %>%
  filter(gas == "CO2")
summary(greenhouse_gases_co2$concentration, greenhouse_gases_co2$year)


library("dslabs")
library(dplyr)
library(ggplot2)
data("greenhouse_gases")
greenhouse_gases_co2 <- greenhouse_gases %>%
  filter(gas == "CO2")
ggplot(greenhouse_gases_co2, aes(x = year, y = concentration)) +
  geom_point() + ggtitle("Concentration of CO2") +
  xlab("Year") + ylab("ppm") + theme(
    plot.title = element_text(hjust = 0.5))

#zoom to 1800 to 2000
greenhouse_gases_co2 <- greenhouse_gases %>%
  filter(gas == "CO2", year>1700)
ggplot(greenhouse_gases_co2, aes(x = year, y = concentration)) +
  geom_point() + ggtitle("Concentration of CO2") +
  xlab("Year") + ylab("ppm") + theme(
    plot.title = element_text(hjust = 0.5))



#Percent change in concentration of CO2
greenhouse_gases_co2 <- greenhouse_gases %>%
  filter(gas == "CO2")
pct_change <- (greenhouse_gases$concentration/lead(greenhouse_gases$concentration) - 1) * 100
summary(pct_change)

greenhouse_gases_co2 <- greenhouse_gases %>%
  filter(gas == "CO2")
pct_change <- (greenhouse_gases$concentration/lead(greenhouse_gases$concentration) - 1) * 100
summary(pct_change)
ggplot(greenhouse_gases_co2, aes(x = year, y = pct_change)) +
  geom_boxplot() + ggtitle("% change in concentration of CO2") +
  xlab("Year") + ylab("%") + theme(
    plot.title = element_text(hjust = 0.5))

greenhouse_gases_co2 <- greenhouse_gases %>%
  filter(gas == "CO2")
library(diffLog)
greenhouse_gases_co2$concentration <- diffLog(greenhouse_gases_co2$concentration)

