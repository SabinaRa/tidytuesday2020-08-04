# TidyTuesday Chellenge European Energy 2020-08-04
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-08-04/readme.md
# Contributor: Sabina Rako, Twitter: @ra_sabina


library(tidytuesdayR)
library(leaflet)
library(skimr)
library(data.table)
library(dplyr)
library(webshot)


# Import data
tuesdata <- tidytuesdayR::tt_load('2020-08-04')
energy_types <- tuesdata$energy_types
countries <- data.table::fread("data/countries.csv")

# Explore dataset
skim(energy_types)

# Join datasets by country code
energy_types_country <- energy_types %>% 
                        inner_join(countries, by = c("country" = "country"))  

# Explore dataset
skim(energy_types_country)

# Top 20 countries in 2018
top20 <-  energy_types_country %>% 
          filter(level == "Level 1") %>% 
          group_by(country_name) %>% 
          summarise(sum = sum(`2018`)) %>% 
          arrange(desc(sum)) %>% 
          head(20)

# Ajusting data for 2018
energy_types_country_ajusted <- energy_types_country %>% 
                                select(country_name,type,"2018", longitude, latitude)  %>% 
                                pivot_wider(names_from = type, values_from = "2018") %>% 
                                filter(country_name %in% top20$country_name) %>% 
                                mutate(Renewable = Hydro  + Wind + Solar + Geothermal )

# Data preparing for genereting charts 
EnergyTypes <-  energy_types_country_ajusted %>% 
                select(`Conventional thermal`, Nuclear,Renewable)

colors <- c("#3093e5", "#fcba50", "#4fc13c")

# Top 20 European countries in 2018 based on energy production in GWh

EUEnergy2018 <- leaflet()%>% 
                addProviderTiles(providers$CartoDB) %>% 
                addMinicharts(
                    energy_types_country_ajusted$longitude, energy_types_country_ajusted$latitude,
                    chartdata = EnergyTypes,
                    colorPalette = colors,
                    labelMinSize = 7,
                    showLabels = FALSE,
                    width = 60, height = 60
                              ) 


saveWidget(EUEnergy2018, 'EUEnergy2018.html', selfcontained = TRUE)
webshot("EUEnergy2018.html", file = "EUEnergy2018.png",
        cliprect = "viewport")
