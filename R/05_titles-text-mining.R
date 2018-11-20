# Figure 5 top words
library(readr)
library(tidytext)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)
library(hrbrthemes)
library(here)
library(patchwork)
library(extrafont)

# read data
absdata <- read_csv(here("data","abstracts.csv"))
# stopwords
stopWords <- data_frame(word=read_lines(here("data","minimal-stop.txt")))

# overall
titlesAllbg <- absdata %>% 
  mutate(title_all = talk_title) %>% unnest_tokens(BGword, talk_title, token = "ngrams", n=2) 
# separate 
bigrams_separatedAll <- titlesAllbg %>%
  separate(BGword, c("word1", "word2"), sep = " ")
# filtering
bigrams_filteredAll <- bigrams_separatedAll %>%
  filter(!word1 %in% stopWords$word) %>%
  filter(!word2 %in% stopWords$word)
# new bigram counts:
bigram_countsAll <- bigrams_filteredAll %>% 
  count(word1, word2, sort = TRUE)
# unite
bigrams_unitedAll <- bigrams_filteredAll  %>%
  unite(bigram, word1, word2, sep = " ")
# count
bigrams_united_countAll <- bigrams_unitedAll %>% count(bigram,sort=TRUE)

# just words
titlesAllug <- 
  absdata %>%  
  mutate(title_all = talk_title) %>% unnest_tokens(word, talk_title)
# remove stop words
titles_filteredAllug <- titlesAllug %>% anti_join(stopWords, by = "word")
# quantify
by_wordAllug <- titles_filteredAllug %>% count(word, sort = TRUE)

# df with both for everything 
topBGall <- bigrams_united_countAll %>% rename(ngram=bigram) %>% mutate(term="bigram")
topWall <- by_wordAllug %>%  rename(ngram=word) %>%  mutate(term="word")
# bind
topngramsAll <- bind_rows(topBGall,topWall)


# by format and status
#### pubished talks ####
titlesTalksbigPUB <- absdata %>% filter(published=="TRUE" & pres_format == "talk") %>%
  mutate(title_all = talk_title) %>% unnest_tokens(BGword, talk_title, token = "ngrams", n=2) 
# separate 
bigrams_separatedTalksPUB <- titlesTalksbigPUB %>%
  separate(BGword, c("word1", "word2"), sep = " ")
# filtering
bigrams_filteredTalksPUB <- bigrams_separatedTalksPUB %>%
  filter(!word1 %in% stopWords$word) %>%
  filter(!word2 %in% stopWords$word)
# new bigram counts:
bigram_countsTalksPUB <- bigrams_filteredTalksPUB %>% 
  count(word1, word2, sort = TRUE)
# unite
bigrams_unitedTalksPUB <- bigrams_filteredTalksPUB %>%
  unite(bigram, word1, word2, sep = " ")
# count
bigrams_united_countTalksPUB <- bigrams_unitedTalksPUB %>% count(bigram,sort=TRUE)

# now just words
titlesTalksPub <- 
  absdata %>% filter(published=="TRUE" & pres_format == "talk") %>% 
  mutate(title_all = talk_title) %>% unnest_tokens(word, talk_title)
# remove stop words
titles_filterTalkPub <- titlesTalksPub %>% anti_join(stopWords, by = "word")
# quantify
by_wordPubTalk <- titles_filterTalkPub %>% count(word, sort = TRUE)
# df with both for published presentations
topBGpubTalks <- bigrams_united_countTalksPUB %>% rename(ngram=bigram) %>% mutate(term="bigram")
topWpubTalks <- by_wordPubTalk %>%  rename(ngram=word) %>%  mutate(term="word")
# bind
topngramspubTalks <- bind_rows(topBGpubTalks,topWpubTalks)
# label
topngramspubTalks <- topngramspubTalks %>% mutate(presentations="published talks")


##### unpubished talks ####
titlesTalksbigUnPUB <- absdata %>% filter(published=="FALSE" & pres_format=="talk") %>%
  mutate(title_all = talk_title) %>% unnest_tokens(BGword, talk_title, token = "ngrams", n=2) 
# separate and filter for stop words
bigrams_separatedUnPUBTalks <- titlesTalksbigUnPUB %>%
  separate(BGword, c("word1", "word2"), sep = " ")
# filtering
bigrams_filteredUnPUBTalks <- bigrams_separatedUnPUBTalks %>%
  filter(!word1 %in% stopWords$word) %>%
  filter(!word2 %in% stopWords$word)
# new bigram counts:
bigram_countsUnPUBTalks <- bigrams_filteredUnPUBTalks %>% 
  count(word1, word2, sort = TRUE)
# unite
bigrams_unitedUnPUBTalks <- bigrams_filteredUnPUBTalks %>%
  unite(bigram, word1, word2, sep = " ")
# count
bigrams_united_countUnPUBTalks <- bigrams_unitedUnPUBTalks %>% count(bigram,sort=TRUE)
### now just words
titlesUnPubTalks <- 
  absdata %>% filter(published=="FALSE" & pres_format =="talk") %>% 
  mutate(title_all = talk_title) %>% unnest_tokens(word, talk_title)
# remove stop words
titles_filterUnPubTalk <- titlesUnPubTalks %>% anti_join(stopWords, by = "word")
# quantify
by_wordUnPubTalk <- titles_filterUnPubTalk %>% count(word, sort = TRUE)
# df with both for published presentations
topBGUnPubTalks <- bigrams_united_countUnPUBTalks %>% rename(ngram=bigram) %>% mutate(term="bigram")
topWUnPubTalks <- by_wordUnPubTalk %>% rename(ngram=word) %>%  mutate(term="word")
# bind
topngramsUnpubTalks <- bind_rows(topBGUnPubTalks,topWUnPubTalks)
# label
topngramsUnpubTalks <- topngramsUnpubTalks %>% mutate(presentations="unpublished talks")


#### pubished posters ####
titlesPosterbigPUB <- absdata %>% filter(published=="TRUE" & pres_format == "poster") %>%
  mutate(title_all = talk_title) %>% unnest_tokens(BGword, talk_title, token = "ngrams", n=2) 
# separate and filter for stop words
bigrams_separatedPosterPUB <- titlesPosterbigPUB %>%
  separate(BGword, c("word1", "word2"), sep = " ")
# filtering
bigrams_filteredPosterPUB <- bigrams_separatedPosterPUB %>%
  filter(!word1 %in% stopWords$word) %>%
  filter(!word2 %in% stopWords$word)
# new bigram counts:
bigram_countsPosterPUB <- bigrams_filteredPosterPUB %>% 
  count(word1, word2, sort = TRUE)
# unite
bigrams_unitedPosterPUB <- bigrams_filteredPosterPUB %>%
  unite(bigram, word1, word2, sep = " ")
# count
bigrams_united_countPosterPUB <- bigrams_unitedPosterPUB %>% count(bigram,sort=TRUE)
## now just words
titlesPosterPub <- 
  absdata %>% filter(published=="TRUE" & pres_format == "poster") %>% 
  mutate(title_all = talk_title) %>% unnest_tokens(word, talk_title)
# remove stop words
titles_filterPosterPub <- titlesPosterPub %>% anti_join(stopWords, by = "word")
# quantify
by_wordPubPoster <- titles_filterPosterPub %>% count(word, sort = TRUE)
# df with both for published presentations
topBGpubPoster <- bigrams_united_countPosterPUB  %>% rename(ngram=bigram) %>% mutate(term="bigram")
topWpubPoster <- by_wordPubPoster  %>% rename(ngram=word) %>%  mutate(term="word")
# bind
topngramspubPoster <- bind_rows(topBGpubPoster,topWpubPoster)
# label
topngramspubPoster <- topngramspubPoster %>% mutate(presentations="published posters")


#### unpubished Posters ####
titlesPostersbigUnPUB <- absdata %>% filter(published=="FALSE" & pres_format=="poster") %>%
  mutate(title_all = talk_title) %>% unnest_tokens(BGword, talk_title, token = "ngrams", n=2) 
# separate and filter for stop words
bigrams_separatedUnPUBPosters <- titlesPostersbigUnPUB %>%
  separate(BGword, c("word1", "word2"), sep = " ")
# filtering
bigrams_filteredUnPUBPosters <- bigrams_separatedUnPUBPosters %>%
  filter(!word1 %in% stopWords$word) %>%
  filter(!word2 %in% stopWords$word)
# new bigram counts:
bigram_countsUnPUBPosters <- bigrams_filteredUnPUBPosters %>% 
  count(word1, word2, sort = TRUE)
# unite
bigrams_unitedUnPUBPosters <- bigrams_filteredUnPUBPosters %>%
  unite(bigram, word1, word2, sep = " ")
# count
bigrams_united_countUnPUBPosters <- bigrams_unitedUnPUBPosters %>% count(bigram,sort=TRUE)
## now just words
titlesUnPubPosters <- 
  absdata %>% filter(published=="FALSE" & pres_format=="poster") %>% 
  mutate(title_all = talk_title) %>% unnest_tokens(word, talk_title)
# remove stop words
titles_filterUnPubPoster <- titlesUnPubPosters %>% anti_join(stopWords, by = "word")
# quantify
by_wordUnPubPoster <- titles_filterUnPubPoster %>% count(word, sort = TRUE)
# df with both for published presentations
topBGUnPubPosters <- bigrams_united_countUnPUBPosters  %>% rename(ngram=bigram) %>% mutate(term="bigram")
topWUnPubPosters <- by_wordUnPubPoster %>% rename(ngram=word) %>%  mutate(term="word")
# bind
topngramsUnpubPosters <- bind_rows(topBGUnPubPosters,topWUnPubPosters)
# label
topngramsUnpubPosters <- topngramsUnpubPosters %>% mutate(presentations="unpublished posters")

##### binding all four #####
topWords <- rbind(topngramspubTalks,topngramsUnpubTalks,topngramspubPoster,topngramsUnpubPosters)  
# add format label
topWords <- topWords %>% mutate(format=if_else(str_detect(presentations,"posters"),"poster","talk"),
                                pubStatus=if_else(str_detect(presentations,"unpublished"),"unpublished","published")) 
#topWordsNpres <- 
topWords %>% count(format) 

#### plotting ####
#all 

############
### 
doubCwords <- data_frame(word=c("new","climate","change"))
topWords_dc <- anti_join(topWords,doubCwords,by=c("ngram"="word"))

#### word frequencies ####
# talks vs posters
totalsfreq <- topWords %>% filter(term=="word") %>% group_by(format) %>% summarise(wfq=sum(n)) %>% ungroup()
topWordsTfreq <- left_join(topWords_dc,totalsfreq)


(leftgrob <- 
    topWordsTfreq %>% 
    mutate(reln=scale(n/wfq)) %>% 
    mutate(presentationsW=str_replace(presentations," ","\n")) %>% 
    top_n(30,reln) %>% 
    ggplot(aes(x = fct_reorder(ngram, reln), y = reln)) +
    ggalt::geom_lollipop() +
    scale_y_continuous(breaks = c(0,15,30))+
    theme_ipsum(base_size = 11, axis_title_size = 11,
                grid = "Y",axis_title_just = "c",axis_text_size = 8, 
                strip_text_size = 9,base_family = "Roboto Condensed")+
    coord_flip()+facet_wrap(~fct_relevel(presentationsW,c("unpublished\nposters","published\nposters","unpublished\ntalks","published\ntalks")),nrow =1)+
    labs(x="term",y="relative frequency (scaled)"))


# spread by category
(topWordsSp <- topWords %>% filter(term=="word") %>% spread(presentations,n))
(topWordsSp <- topWordsSp %>% replace(is.na(.), 0))

# distance matrix
termsDistmat <- (dist(t(as.matrix(topWordsSp[,5:8])),method = "euclidean"))
termsclust <- hclust(termsDistmat,method = "ward.D")
plot(termsclust)

library(ggraph)  
library(tidygraph)
library(dendextend)
dendcl <- as.dendrogram(termsclust)


# wrap labels
dendextend::labels(dendcl) <- str_replace(labels(dendcl)," ","\n")

right <- 
  ggraph(dendcl,layout = "dendrogram") + 
  geom_edge_fan(alpha = 0.2) + 
  scale_edge_size_continuous(range = c(1,3))+
  geom_node_text(aes(label = label), size = 2.4, 
                 repel = T,nudge_y = 4) +
  geom_node_point(size=1,shape=21) +
  theme_graph(base_family = "Roboto Condensed")+coord_flip()
right

figtextmin <- 
    leftgrob+right+plot_layout(nrow = 1,widths = c(2,1),tag_level = 'new')+plot_annotation(tag_levels = 'a',tag_suffix = ")")
figtextmin
#ggsave(figtextmin,filename = here("figures","fig5.pdf"),width = 8,height = 3.6, dpi = 300)
