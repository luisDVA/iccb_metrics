# Contributions by country

# packages
library(rnaturalearth)
library(rgeos)
library(ggplot2)
library(dplyr)
library(stringi)
library(ggthemes)
library(maptools)
library(sf)
library(scico)
library(extrafont)
library(here)
library(ggspatial)


# fonts
loadfonts(device = "postscript")
# get a world map
NE_countries <- ne_countries(scale = "small", type = "countries", returnclass = "sp")

# recenter
world <- nowrapRecenter(NE_countries, offset = 30)

# to simple features, then prepare country variable
worldsf <- st_as_sf(world)
worldsf <- worldsf %>% mutate(country = NE_countries@data$name_long)

# read the data
absflat <- read.csv(here("data", "absFlatNov2017.csv"), stringsAsFactors = FALSE)
# count pr. auth. countries
ctryCount <- absflat %>% count(authorCountry)
# manual edits so that country names match
ctryCount$authorCountry <-
  stri_replace_all_fixed(
    str = ctryCount$authorCountry,
    pattern = c("Russia", "South Korea", "The Netherlands", "USA", "UK"),
    replacement = c("Russian Federation", "Republic of Korea", "Netherlands", "United States", "United Kingdom"), vectorize_all = FALSE
  )

# merge
worldMapCount <- left_join(worldsf, ctryCount, by = c("country" = "authorCountry"))

# project
worldMapCountRob <- st_transform(x = worldMapCount, crs = "+proj=robin +lon_0=150 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
worldRobF <- worldMapCountRob %>% filter(country != "Greenland", country != "Antarctica")

# plot
ggplot(worldRobF) +
  geom_sf(aes(fill = n), color = "#3B4252", size = 0.12) +
  scale_fill_scico(
    palette = "davos", name = "Number of abstracts",
    direction = -1, end = 0.89, breaks = c(20, 100, 200)
  ) +
  guides(fill = guide_colorbar(
    frame.colour = "black",
    title.vjust = 0.25,
    label.position = "top"
  )) +
  theme(
    panel.grid.major.y = element_line(color = "red"),
    plot.background = element_blank(),
    panel.background = element_rect(fill = "#f1f2f3", color = "#f1f2f3"),
    rect = element_blank(),
    text = element_text(family = "Roboto"),
    legend.direction = "horizontal",
    legend.position = "bottom", legend.text = element_text(size = 7)
  ) +
  coord_sf() +
  annotation_north_arrow(
    location = "bl", height = unit(0.6, "cm"), width = unit(0.6, "cm"),
    style = north_arrow_orienteering
  )
