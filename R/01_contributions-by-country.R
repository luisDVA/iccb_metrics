# load packages
library(rnaturalearth)
library(rgeos)
library(ggplot2)
library(dplyr)
library(here)
library(stringi)
library(ggthemes)
library(maptools)
library(sf)
library(scico)
library(extrafont)

# register fonts
loadfonts()
# get a world map
NE_countries <- ne_countries(scale = "small", type = "countries", returnclass = 'sp')
# recenter
world <- nowrapRecenter(NE_countries, offset = 180-150)

# to simple features, then prepare country variable
worldsf <- st_as_sf(world)
worldsf <-  worldsf %>% mutate(country=NE_countries@data$name_long)

# read the data
absflat <- read.csv(here("data","abstracts.csv"),stringsAsFactors = FALSE)
# count pr. auth. countries
ctryCount <- absflat %>% count(author_country)
# manual edits so that country names match
ctryCount$author_country <- 
  stri_replace_all_fixed(str = ctryCount$author_country, 
                         pattern = c("Russia","South Korea","The Netherlands","USA","UK"),
                         replacement = c("Russian Federation","Republic of Korea","Netherlands","United States","United Kingdom"), vectorize_all = FALSE)

# merge 
worldMapCount <- left_join(worldsf,ctryCount,by=c("country"="author_country"))

# project (Robinson)
worldMapCountRob <- st_transform(x = worldMapCount, crs = "+proj=robin +lon_0=150 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

fig1 <- 
worldMapCountRob %>%  filter(country!="Greenland",country!="Antarctica") %>% 
    ggplot()+
    geom_sf(aes(fill=n),color = "#3B4252",size=0.12)+
    scale_fill_scico(palette = "davos", name="Number of contributions",direction = -1,end = 0.89)+
    coord_sf(datum = NA) + 
    theme(plot.background = element_rect(color = "black",size=0.5)) +
    theme(panel.background = element_rect(fill = "#f1f2f3", color = "#f1f2f3"))+
    theme(
      panel.grid = element_blank(), 
      line = element_blank(), 
      rect = element_blank(),
      text = element_text(family="Roboto"))+
    theme(legend.direction = "horizontal",
          legend.position = "bottom") +
    guides(fill=guide_colorbar(frame.colour = "black")) 

# export
#ggsave(fig1,filename = here("figures","fig1.pdf"),width = 7,height =4)
