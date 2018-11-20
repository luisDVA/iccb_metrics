# figure 4, arc diagram of study and author countries
library(here)
library(dplyr)
library(readr)
library(stringr)
library(countrycode)
library(circlize)
library(migest)
extrafont::loadfonts()

# read data
absflat <- read_csv(here("data","abstracts.csv"))

# subset 
connections <- absflat %>% select(author_country,author_country_world_bank_region,
                                  study_country,study_country_world_bank_region) %>% na.omit()
# recode Australia and NZ
connections <- connections %>% mutate(author_country_world_bank_region=
                         if_else(str_detect(author_country,"Australia"),"Australia",author_country_world_bank_region)) %>% 
                mutate(study_country_world_bank_region=
                         if_else(str_detect(study_country,"Australia"),"Australia",study_country_world_bank_region)) %>% 
                mutate(author_country_world_bank_region=
                          if_else(str_detect(author_country,"New Zealand"),"New Zealand",author_country_world_bank_region)) %>% 
                mutate(study_country_world_bank_region=
                          if_else(str_detect(study_country,"New Zealand"),"New Zealand",study_country_world_bank_region)) 

# quantify connections
flowDF <- connections %>% count(author_country_world_bank_region,study_country_world_bank_region)

# prepare df2 following example from migest package (https://gjabel.wordpress.com/category/r/migest/)
df0 <- read.csv(system.file("vidwp", "reg_flow.csv", package = "migest"), stringsAsFactors=FALSE)
df1 <- read.csv(system.file("vidwp", "reg_plot.csv", package = "migest"), stringsAsFactors=FALSE)

# rename cols to match example code
names(flowDF) <- names(df0)

# custom drawing order

regOrd2a <- c("North America","Latin America & Caribbean","Sub-Saharan Africa",
              "Middle East & North Africa","East Asia & Pacific","South Asia",
              "Europe & Central Asia","Australia","New Zealand")

regOrd2b <- c("North America|","Latin America|& Caribbean","Sub-Saharan|Africa",
              "Middle East|& North Africa","East Asia|& Pacific","South Asia|",
              "Europe|& Central Asia","Australia|","New Zealand|")


# building DF
df2 <- data_frame(region=regOrd2a,order1=seq(1:length(regOrd2a)),col1=df1$col1,regsplit=regOrd2b)
df2 <- df2 %>% separate(regsplit,into = c("reg1","reg2"),sep = "\\|",extra = "merge")
df2$reg1[4] <- ""
df2$reg2[4] <- ""
df2 <- df2 %>% mutate(reg3 ="",reg4="")
df2$reg3[4]  <- "Middle East &"
df2$reg4[4]  <- "North Africa"


#set up output
#cairo_pdf(filename = here("figures","fig4.pdf"),width = 7, height = 6, family = "Roboto")

##plot parameters

circos.clear()
circos.par(start.degree = 160, gap.degree = 4.5, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0.5,4),family="Roboto")

#increasing the gaps between sectors, start at 12 o'clock, ensure no gap between the chord and the sector at the begining
# subdue warning messages and have no margins in the plot
##chord diagram with user selected adjustments for bilateral migration flow data

chordDiagram(x = flowDF, grid.col = df2$col1,transparency = 0.3,
             order = df2$region, directional = 1,
             direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
             annotationTrack = "grid", annotationTrackHeight = c(0.05, 0.1),
             link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE)


# First line of arguments reads in the data (df0) and sets the colours base on the meta data (df1).
# Second line provides the order of outer sectors and indicates that chords should be directional.
# Third line indicates that the direction of the chords will be illustrated by both arrows and a difference in height. The
#  height difference is negative to make the chord shorter at the end (with the arrow head).
# Fourth line ensures the annotations outside the sectors are not plotted, but provides a track measures to later add 
#  annotatinos such as labels and axis (see below).
# Fifth line indicates the plot should use big arrows, sort the chords left to right in each sector and 
#  plots the smallest chords first.

##
##add in labels and axis
##

circos.trackPlotRegion(
  track.index = 1, 
  bg.border = 1, 
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    reg1 = df2$reg1[df2$region == sector.index]
    reg2 = df2$reg2[df2$region == sector.index]
    reg3 = df2$reg3[df2$region == sector.index]
    reg4 = df2$reg4[df2$region == sector.index]
    
    circos.text(x = (mean(xlim)+4), y = ifelse(test = nchar(reg1) == 0, yes = 4.8, no = 3.2), 
                labels = reg3, facing = "bending", cex = 1)
    circos.text(x = (mean(xlim)+13), y = ifelse(test = nchar(reg3) != 0, yes = 3.7, no = 3.2), 
                labels = reg4, facing = "bending", cex = 1)
    circos.text(x = mean(xlim), y = ifelse(test = nchar(reg2) == 0, yes = 2.7, no = 3.2), 
                labels = reg1, facing = "bending", cex = 1)
    circos.text(x = mean(xlim), y = 2, 
                labels = reg2, facing = "bending", cex = 1)
    circos.axis(h = "bottom", 
                major.at = seq(from = 0, to = xlim[2], by = ifelse(test = xlim[2]>10, yes = 50, no = 5)), 
                minor.ticks = 0, major.tick.percentage = 0.3,
                labels.niceFacing = FALSE,labels.cex = 0.7)
  }
)

#dev.off()
