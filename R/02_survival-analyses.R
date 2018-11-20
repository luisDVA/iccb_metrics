# figure 2, survival plot
# devtools::install_github('datarootsio/artyfarty')
library(dplyr)
library(stringi)
library(stringr)
library(zoo)
library(here)
library(survival)
library(survminer)
library(hrbrthemes)
library(ggrepel)
library(artyfarty)
library(cowplot)
library(readr)

#extrafont::loadfonts(device = "win")
#read flattened
absflat <- read_csv(here("data","abstracts.csv"))

# for summary data
absttpub <- absflat %>% filter(!is.na(time_to_publication_months))

# keep relevant columns
absSurv <- absflat %>% select(pa_gender,pres_format, time_to_publication_months,published,author_country_EFL_status,institution_type,n_coauthors)
# calculate time in months between conference and date when publication status was last checked (March 2017)
(as.yearmon("Dec 2017","%b %Y") - as.yearmon("Dec 2011","%b %Y")) *12
# set survival or censoring time
absSurv <- absSurv %>% mutate(timetttt=if_else(is.na(time_to_publication_months),72L,time_to_publication_months))

# binarize status
absSurv$status <- if_else(absSurv$published==TRUE,1,0)

# exclude publications before conference
absSurv$startTime <- 0
absSurv$stopTime <- ifelse(is.na(absSurv$time_to_publication_months),72,absSurv$time_to_publication_months)
absSurvFilt <- absSurv %>% filter(stopTime>0)

#### plotting objects via ggsurvplot fn 
# presenter gender
fitabsG <- survfit(Surv(time=stopTime,event=status) ~ pa_gender,
                   data = absSurvFilt)
gendplot <-     ggsurvplot(fitabsG)
gendSumm <- as.data.frame(summary(fitabsG)$table)

# coaut
absSurvFilt <-  absSurvFilt %>% mutate(coauthRC=case_when(n_coauthors==0~"single author",
                                                          n_coauthors>0 & n_coauthors<3~"small team",
                                                          TRUE~"large team"))

fitabcoaut <- survfit(Surv(time=stopTime,event=status) ~ coauthRC,
                      data = absSurvFilt)

coautPlot <- ggsurvplot(fitabcoaut)
coautSumm <- as.data.frame(summary(fitabcoaut)$table)

# put them all together
survdata_bind <- 
  list(gendplot$data.survplot,coautPlot$data.survplot) %>% 
  purrr::reduce(bind_rows)

# women-men  
byprGender <- 
  survdata_bind %>% filter(str_detect(strata,"pa_gender")) %>% 
  mutate(labelrep = if_else(time == max(time), as.character(pa_gender), NA_character_)) %>%  
  mutate(labelrep=recode(labelrep,"M"="male","F"="female")) %>% 
  ggplot(aes(x=time,y=surv,color=strata))+
  scale_color_manual(values=c("#0072b2","#d55e00"),guide=FALSE)+
  geom_line()+geom_ribbon(aes(ymin=lower,ymax=upper,fill=strata,linetype=NA),alpha=0.1)+
  scale_fill_discrete(guide=FALSE)+
  scale_x_continuous(breaks = seq(0,72,12),expand = c(0.03,0),limits = c(0,80))+
  scale_y_continuous(breaks = c(0,0.5,1),limits = c(0,1))+
  geom_text_repel(aes(label = labelrep),size=4,nudge_x = 1, na.rm = TRUE)+
  xlab("")+ylab("proportion unpublished")+
  geom_segment(aes(x=43,xend=43,y=0,yend=0.48),color="black",linetype=2,size=0.3)+
  theme_pubclean()+theme(text=element_text(family="Roboto",size = 13))

byprGender  
#coauthors
bycoauthors <- 
  survdata_bind %>% filter(str_detect(strata,"coauthRC")) %>% 
  mutate(labelrep = if_else(time == max(time), as.character(coauthRC), NA_character_)) %>%  
  ggplot(aes(x=time,y=surv,color=strata,fill=strata))+
  scale_color_manual(values=c("#0072b2","#d55e00","#009e73"),guide=FALSE)+
  geom_line()+geom_ribbon(aes(ymin=lower,ymax=upper,fill=strata,linetype=NA),alpha=0.1)+
  scale_fill_manual(values=c("#0072b2","#d55e00","#009e73"),guide=FALSE)+
  scale_x_continuous(breaks = seq(0,72,12),expand = c(0.03,0),limits = c(0,96))+
  scale_y_continuous(breaks = c(0,0.5,1),limits = c(0,1))+
  geom_text_repel(aes(label = labelrep),size=4,nudge_x = 2, na.rm = TRUE)+
  geom_segment(aes(x=34.5,xend=34.5,y=0,yend=0.48),color="black",linetype=2,size=0.3)+
  geom_segment(aes(x=46,xend=46,y=0,yend=0.48),color="black",linetype=2,size=0.3)+
  xlab("")+ylab("")+
  theme_pubclean()+theme(text=element_text(family="Roboto",size = 12))
bycoauthors    

################
# arrange
fig2 <- plot_grid(byprGender, bycoauthors,nrow = 1,ncol = 2,labels = c("a)","b)"),label_fontfamily = "Roboto",label_size = 12)
fig2s <- add_sub(fig2, "time (months)")
fig2sd <- ggdraw(fig2s)
fig2sd
#ggsave(fig2sd,filename = "fig2.svg",width = 12, height = 4, units = "in")
#ggsave(fig2sd,filename = here("figures","fig2.pdf"),width = 9, height = 3.3, units = "in", dpi = 200)


#### survival estimates mentioned in the text and Tables
# Cox proportional hazards model

# presenter gender
# recode for interpretation
absSurvFilt <- absSurvFilt %>% mutate(gendrecoded=recode(pa_gender,
                                                         "F"="bF","M"="aM" ))
fitabsG <- survfit(Surv(time=stopTime,event=status) ~ pa_gender,
                   data = absSurvFilt)
survdiff(Surv(time=stopTime,event=status) ~ pa_gender,
         data = absSurvFilt,rho=0)
summary(coxph(Surv(time=stopTime,event=status) ~ gendrecoded,
              data = absSurvFilt))


# coauthors
absSurvFilt <-  absSurvFilt %>% mutate(coauthRC=case_when(n_coauthors==0~"asingle author",
                                                          n_coauthors>0 & n_coauthors<4~"bsmall team",
                                                          TRUE~"clarge team"))
fitabcoaut <- survfit(Surv(time=stopTime,event=status) ~ coauthRC,
                      data = absSurvFilt)
survdiff(Surv(time=stopTime,event=status) ~ coauthRC,
         data = absSurvFilt,rho=0)
summary(coxph(Surv(time=stopTime,event=status) ~ coauthRC,
              data = absSurvFilt))

# language
survdiff(Surv(time=stopTime,event=status) ~ author_country_EFL_status,
         data = absSurvFilt,rho=0)
summary(coxph(Surv(time=stopTime,event=status) ~ author_country_EFL_status,
              data = absSurvFilt))

# format
survdiff(Surv(time=stopTime,event=status) ~ pres_format,
         data = absSurvFilt,rho=0)
summary(coxph(Surv(time=stopTime,event=status) ~ pres_format,
              data = absSurvFilt))




