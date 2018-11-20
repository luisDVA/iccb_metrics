# recursive partitioning
library(here)
library(dplyr)
library(party)
library(ggplot2)
library(caret)
library(ggalt)
library(data.tree)

# data preprocess 
# read data
absflat <- read.csv(here("data","abstracts.csv"))

# recode coauthors
absflat <- absflat%>% mutate(coauthRC=case_when(n_coauthors==0~"single author",
                                                n_coauthors>0 & n_coauthors<5~"small team",
                                                TRUE~"large team"))

# drop vars and recode response
absData <- absflat %>% select(pa_gender,pres_format,author_country_EFL_status,institution_type,coauthRC,published) %>% 
  mutate(published=if_else(published==TRUE,1,0))
# response
absDfac <- absData %>% mutate_all(.,funs(factor))
# general
fitGenCtree <- ctree(published~.,
                     data=absDfac) 
fitGenCfor <- cforest(published~.,
                      data=absDfac)


# source custom functions
source(here("R","helper_customtreeplots.R"))

# plot to svg  (edit with inkscape afterwards for presentability)
#svg(filename = "figCondtree.svg",width = 6, height = 4)
plot(fitGenCtree,inner_panel=innerWeights,
     terminal_panel=node_barplot2,
     tp_args = list(ylines = c(3, 5)))
#dev.off()

# variable importance
importancevars <- data.frame(varimp(fitGenCfor))
# recode and reorder
vardf <- 
  importancevars %>% tibble::rownames_to_column() %>% 
  mutate(varname=recode(rowname,"coauthRC"="coauthors\n(team size)",
                        "paGender"="gender",
                        "talkPoster"="presentation\nformat",
                        "EFLstatus"="EFL\nstatus",
                        "instType"="affiliation")) %>% rename(var_imp=2) 
#rightgrob <- 
ggplot(data=vardf,aes(x=forcats::fct_reorder(varname,var_imp),y=var_imp))+geom_lollipop()+
  coord_flip()+xlab("variable")+ylab("relative variable importance")+
  ggpubr::theme_pubclean()+theme(text = element_text(family = "Roboto",size = 14))

