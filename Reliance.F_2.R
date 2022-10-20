library(dplyr) #Data manipulation (also included in the tidyverse package)
library(tidytext) #Text mining
library(tidyr) #Spread, separate, unite, text mining (also included in the tidyverse package)
library(widyr) #Use for pairwise correlation

#Visualizations!
library(ggplot2) #Visualizations (also included in the tidyverse package)
library(ggrepel) #`geom_label_repel`
library(gridExtra) #`grid.arrange()` for multi-graphs
library(knitr) #Create nicely formatted output tables
library(kableExtra) #Create nicely formatted output tables
library(formattable) #For the color_tile function
library(circlize) #Visualizations - chord diagram
#install.packages("memery")
library(memery) #Memes - images with plots
#install.packages("magick")
library(magick) #Memes - images with plots (image_read)
#install.packages("yarrr")
library(yarrr)  #Pirate plot
#install.packages("radarchart")
library(radarchart) #Visualizations
library(igraph) #ngram network diagrams
library(ggraph) #ngram network diagrams


##############################################################
##Preperation
##############################################################
#define colors
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#D65E00")

#customize ggplot defoult theme
theme_home <- function(aticks = element_blank(),
                       pgminor = element_blank(),
                       lt = element_blank(),
                       lp = "none")
{
  theme(plot.title = element_text(hjust = 0.5), #Center the title
        axis.ticks = aticks, #Set axis ticks to on or off
        panel.grid.minor = pgminor, #Turn the minor grid lines on or off
        legend.title = lt, #Turn the legend title on or off
        legend.position = lp) #Turn the legend on or off
}

#Customize the text tables for consistency using HTML formatting
my_kable_styling <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                  full_width = FALSE)
}

#read data
home <- read_excel("/Users/takahiroyamada/Desktop/intern/Reliance Finance/home.xlsx", sheet = "Consoli")
sapply(home, function(x) sum(is.na(x)))
home <- home %>% na.omit()

library(lubridate)
home <- home %>% mutate(date, year_month = floor_date(home$date, unit = "quater"))
home <- home %>% separate(date, c("year","month"), sep="-", extra = "drop")
home$year <- as.numeric(home$year)
home$month <- as.numeric(home$month)

#glimpse(home)

#undesirable words
undesirable_words <- c("prince", "chorus", "repeat", "lyrics",
                       "theres", "bridge", "fe0f", "yeah", "baby",
                       "alright", "wanna", "gonna", "chorus", "verse",
                       "whoa", "gotta", "make", "miscellaneous", "2",
                       "4", "ooh", "uurh", "pheromone", "poompoom", "3121",
                       "matic", " ai ", " ca ", " la ", "hey", " na ",
                       " da ", " uh ", " tin ", "  ll", "transcription",
                       "repeats", "la", "da", "uh", "ah")


#create tidy format
home_tidy <- home %>% 
  unnest_tokens(word, text) %>% 
  filter(!word %in% undesirable_words) %>% 
  filter(!nchar(word) < 3) %>% 
  anti_join(stop_words)

#glimpse(home_tidy)

#distinkt word count per song
word_summary <- home_tidy %>% 
  filter(year>2016) %>% 
  mutate(year = ifelse(is.na(year),"NONE",year)) %>% 
  group_by(year,company) %>% 
  mutate(word_count = n_distinct(word)) %>%
  mutate(row = row_number()) %>% 
  select(row, Reviwed = year, Company = company, word_count) %>%
  distinct() %>% 
  ungroup()


##############################################################
## Word divercity
##############################################################
#word diversity
pirateplot(formula = word_count ~ Reviwed + Company, 
           data = word_summary,
           xlab = NULL, ylab = "Review distinct word coount",
           main = "Lexical Diversity Per year",
           pal = "google",
           point.o = .2,
           avg.line.o = 1,
           theme = 0,
           point.pch = 16,
           point.cex = 1.5,
           jitter.val = .1,
           cex.lab = .9, cex.names = .7)

##############################################################
## Review ammount by year
##############################################################
reviews_year <- home %>% 
  filter(year > "2014") %>% 
  select(name, year) %>% 
  group_by(year) %>% 
  summarise(name_count = n())

id <- seq_len(nrow(reviews_year))
reviews_year <- cbind(reviews_year, id)
label_data = reviews_year
number_of_bar = nrow(label_data)
angle = 90 - 360*(label_data$id-0.5)/number_of_bar
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle + 180, angle)

ggplot(reviews_year, aes(x= as.factor(id), y= name_count))+
  geom_bar(stat = "identity", fill = alpha("purple", 0.7))+
  geom_text(data = label_data, aes(x = id, y = name_count + 10, label = year, hjust = hjust), color = "black", alpha = 0.6, size = 3, angle = label_data$angle, inherit.aes = FALSE)+
  coord_polar(start=0)+
  ylim(-20, 1200)+ #need to be changed according to the obs number
  theme_minimal()+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-4,4),"in"),
        plot.title = element_text(margin= margin(t= 10, b= -10)))


##############################################################
## Relationship Between website and year
##############################################################
#install.packages("circlize")
library(circlize)
year_chart <- home %>% 
  filter(year>"2016") %>% 
  count(year, site)

circos.clear()
grid.col = c("2017" = my_colors[1], "2018" = my_colors[2], "2019" = my_colors[3], "2020s" = my_colors[4], "2021s" = my_colors[5], "Charted" = "grey", "Uncharted" = "grey")
circos.par(gap.after = c(rep(5, length(unique(year_chart[[1]]))-1)),15,
           rep(5,length(unique(year_chart[[2]]))-1),15)

chordDiagram(year_chart, grid.col=grid.col, transparency = .2)
title("Relationship Between Website and Year")


##############################################################
## Sentiment
##############################################################
##General sentiment overview
nrc <- get_sentiments("nrc") %>% 
  mutate(lexicon = "nrc")
bing <- get_sentiments("bing") %>% 
  mutate(lexicon = "bing")
afinn <- get_sentiments("afinn") %>% 
  mutate(lexicon = "AFINN")

afinn_senti <- afinn %>% 
  mutate(ifelse(value>=0,"positive",ifelse(value<0,"negative", value)))
colnames(afinn_senti)[4] <- "sentiment"
afinn_senti <- afinn_senti %>% 
  select(word,sentiment,lexicon)


new_sentiments <- rbind(afinn_senti,bing,nrc) %>% 
  group_by(lexicon) %>%
  mutate(words_in_lexicon = n_distinct(word)) %>%
  ungroup()

new_sentiments %>% 
  group_by(lexicon,sentiment,words_in_lexicon) %>% 
  summarise(distinct_words = n_distinct(word)) %>% 
  ungroup() %>% 
  spread(sentiment,distinct_words) %>% 
  mutate(lexicon = color_tile("lightblue","lightblue")(lexicon),
         words_in_lexicon = color_bar("lightpink")(words_in_lexicon)) %>% 
  my_kable_styling(caption = "Word Counts Per Lexicon")


##Reviews found in lexicon match ratio
home_tidy %>% 
  mutate(words_in_reviews = n_distinct(word)) %>% 
  inner_join(new_sentiments) %>% 
  group_by(lexicon,words_in_reviews,words_in_lexicon) %>% 
  summarise(lex_match_words = n_distinct(word)) %>% 
  ungroup() %>% 
  mutate(total_match_words = sum(lex_match_words),
         match_ratio = lex_match_words / words_in_reviews) %>% 
  select(lexicon, lex_match_words, words_in_reviews, match_ratio) %>% 
  mutate(lex_match_words = color_bar("lightpink")(lex_match_words),
         lexicon = color_tile("lightgreen", "lightgreen")(lexicon)) %>%
  my_kable_styling(caption = "Reviews Found In Lexicons")
  

##nrc analysis
home_bing <- home_tidy %>% 
  inner_join(get_sentiments("bing"))

home_nrc <- home_tidy %>% 
  inner_join(get_sentiments("nrc"))

home_nrc_sub <- home_tidy %>% 
  inner_join(get_sentiments("nrc")) %>% 
  filter(!sentiment %in% c("positive", "negative"))

##Competitor NRC Sentiment
nrc_plot <- home_nrc %>% 
  group_by(sentiment) %>% 
  summarise(word_count = n()) %>% 
  ungroup() %>% 
  mutate(sentiment = reorder(sentiment, word_count)) %>% 
  ggplot(aes(sentiment, word_count, fill = -word_count))+
  geom_col()+
  guides(fill = "none")+
  theme_home()+
  labs(x = NULL, y = "Word Count")+
  scale_y_continuous(limits = c(0,11000))+
  ggtitle("Market NRC Sentiment")+
  coord_flip()
plot(nrc_plot)


##Reliance Financial NRC Sentiment
nrc_rf_plot <- home_nrc %>% 
  filter(company=="rf")%>% 
  group_by(sentiment) %>% 
  summarise(word_count = n()) %>% 
  ungroup() %>% 
  mutate(sentiment = reorder(sentiment, word_count)) %>% 
  ggplot(aes(sentiment, word_count, fill = -word_count))+
  geom_col()+
  guides(fill = "none")+
  theme_home()+
  labs(x = NULL, y = "Word Count")+
  scale_y_continuous(limits = c(0,3000))+
  ggtitle("Reliance Financial NRC Sentiment")+
  coord_flip()
plot(nrc_rf_plot)

##Better NRC Sentiment
nrc_bt_plot <- home_nrc %>% 
  filter(company=="better")%>% 
  group_by(sentiment) %>% 
  summarise(word_count = n()) %>% 
  ungroup() %>% 
  mutate(sentiment = reorder(sentiment, word_count)) %>% 
  ggplot(aes(sentiment, word_count, fill = -word_count))+
  geom_col()+
  guides(fill = "none")+
  theme_home()+
  labs(x = NULL, y = "Word Count")+
  scale_y_continuous(limits = c(0,5000))+
  ggtitle("Better NRC Sentiment")+
  coord_flip()
plot(nrc_bt_plot)

##JVM NRC Sentiment
nrc_jvm_plot <- home_nrc %>% 
  filter(company=="jvm")%>% 
  group_by(sentiment) %>% 
  summarise(word_count = n()) %>% 
  ungroup() %>% 
  mutate(sentiment = reorder(sentiment, word_count)) %>% 
  ggplot(aes(sentiment, word_count, fill = -word_count))+
  geom_col()+
  guides(fill = "none")+
  theme_home()+
  labs(x = NULL, y = "Word Count")+
  scale_y_continuous(limits = c(0,5000))+
  ggtitle("Jvm NRC Sentiment")+
  coord_flip()
plot(nrc_jvm_plot)


##Competitor Bing Sentiment
bing_plot <- home_bing %>% 
  group_by(sentiment) %>% 
  summarise(word_count = n()) %>% 
  ungroup() %>% 
  mutate(sentiment = reorder(sentiment, word_count)) %>% 
  ggplot(aes(sentiment, word_count, fill = sentiment))+
  geom_col()+
  guides(fill = "none")+
  labs(x = NULL, y = "Word Count")+
  scale_y_continuous(limits = c(0, 10000))+
  ggtitle("Market Bing Sentiment")+
  coord_flip()
plot(bing_plot)

##Reliance Financial Bing Sentiment
bing_rf_plot <- home_bing %>% 
  filter(company=="rf")%>% 
  group_by(sentiment) %>% 
  summarise(word_count = n()) %>% 
  ungroup() %>% 
  mutate(sentiment = reorder(sentiment, word_count)) %>% 
  ggplot(aes(sentiment, word_count, fill = sentiment))+
  geom_col()+
  guides(fill = "none")+
  labs(x = NULL, y = "Word Count")+
  scale_y_continuous(limits = c(0, 5000))+
  ggtitle("Reliance Financial Bing Sentiment")+
  coord_flip()
plot(bing_rf_plot)

##Better Bing Sentiment
bing_bt_plot <- home_bing %>% 
  filter(company=="better")%>% 
  group_by(sentiment) %>% 
  summarise(word_count = n()) %>% 
  ungroup() %>% 
  mutate(sentiment = reorder(sentiment, word_count)) %>% 
  ggplot(aes(sentiment, word_count, fill = sentiment))+
  geom_col()+
  guides(fill = "none")+
  labs(x = NULL, y = "Word Count")+
  scale_y_continuous(limits = c(0, 5000))+
  ggtitle("Better Bing Sentiment")+
  coord_flip()
plot(bing_bt_plot)

##JVM Bing Sentiment
bing_jvm_plot <- home_bing %>% 
  filter(company=="jvm")%>% 
  group_by(sentiment) %>% 
  summarise(word_count = n()) %>% 
  ungroup() %>% 
  mutate(sentiment = reorder(sentiment, word_count)) %>% 
  ggplot(aes(sentiment, word_count, fill = sentiment))+
  geom_col()+
  guides(fill = "none")+
  labs(x = NULL, y = "Word Count")+
  scale_y_continuous(limits = c(0, 5000))+
  ggtitle("Jvm Bing Sentiment")+
  coord_flip()
plot(bing_jvm_plot)

####Company#### 
##Polarity and Percent Positive BIng
home_polarity_company <- home_bing %>% 
  count(sentiment, company) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(polarity = positive - negative,
         percent_positive = positive / (positive + negative)*100)

#Polarity by chart
plot1 <- home_polarity_company %>% 
  ggplot(aes(company, polarity, fill = company))+
  geom_col()+
  scale_fill_manual(values = my_colors[3:5])+
  geom_hline(yintercept = 0, color = "red") +
  theme_home() + theme(plot.title = element_text(size = 11))+
  xlab(NULL)+ ylab(NULL)+
  ggtitle("Polarity by Company")

#Percent positive by chart
plot2 <- home_polarity_company %>% 
  ggplot(aes(company, percent_positive, fill = company))+
  geom_col()+
  scale_fill_manual(values = c(my_colors[3:5]))+
  geom_hline(yintercept = 0, color = "red")+
  theme_home() + theme(plot.title = element_text(size = 11))+
  xlab(NULL) + ylab(NULL)+
  ggtitle("Percent Positive by Company")
print(plot2)
           
grid.arrange(plot1, plot2, ncol = 2)

####Website#### 
##Polarity and Percent Positive BIng 
home_polarity_site <- home_bing %>% 
  count(sentiment, site) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(polarity = positive - negative,
         percent_positive = positive / (positive + negative)*100)

#Polarity by chart
plot3 <- home_polarity_site %>% 
  ggplot(aes(site, polarity, fill = site))+
  geom_col()+
  scale_fill_manual(values = my_colors[1:5])+
  geom_hline(yintercept = 0, color = "red") +
  theme_home() + theme(plot.title = element_text(size = 11))+
  xlab(NULL)+ ylab(NULL)+
  ggtitle("Polarity by Website")

#Percent positive by chart
plot4 <- home_polarity_site %>% 
  ggplot(aes(site, percent_positive, fill = site))+
  geom_col()+
  scale_fill_manual(values = c(my_colors[1:5]))+
  geom_hline(yintercept = 0, color = "red")+
  theme_home() + theme(plot.title = element_text(size = 11))+
  xlab(NULL) + ylab(NULL)+
  ggtitle("Percent Positive by Website")

grid.arrange(plot3, plot4, ncol = 2)


##Polarity by year BIng 
home_polarity_year <- home_bing %>% 
  count(sentiment, year) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(polarity = positive - negative,
         percent_positive = positive / (positive + negative)*100)

#Porality Over Time
polarity_over_time <- home_polarity_year %>% 
  ggplot(aes(year, polarity, color = ifelse(polarity >= 0, my_colors[5], my_colors[4])))+
  geom_col()+
  geom_smooth(method = "loess", se = FALSE)+
  geom_smooth(method = "lm", se = FALSE, aes(color = my_colors[1]))+
  theme_home()+ theme(plot.title = element_text(size = 11))+
  xlab(NULL)+ ylab(NULL)+
  ggtitle("Porality Over Time")
print(polarity_over_time)

#Percent Positive Over Time
relative_polarity_over_time <- home_polarity_year %>% 
  ggplot(aes(year, percent_positive, color = ifelse(polarity >=0, my_colors[5], my_colors[4])))+
  geom_col()+
  geom_smooth(method = "loess", se = FALSE)+
  geom_smooth(method = "lm", se = FALSE, aes(color = my_colors[1]))+
  theme_home() + theme(plot.title = element_text(size = 11))+
  xlab(NULL)+ ylab(NULL)+
  ggtitle("Percent Positive Over Time")

grid.arrange(polarity_over_time, relative_polarity_over_time, ncol = 2)

####Reliance Financial####
####Sample too small to scale####
##Polarity by year BIng 
home_polarity_year_rf <- home_bing %>%
  filter(company == "rf") %>% 
  count(sentiment, year) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(polarity = positive - negative,
         percent_positive = positive / (positive + negative)*100)

#Porality Over Time
polarity_over_time_rf <- home_polarity_year_rf %>% 
  ggplot(aes(year, polarity, color = ifelse(polarity >= 0, my_colors[5], my_colors[4])))+
  geom_col()+
  geom_smooth(method = "loess", se = FALSE)+
  geom_smooth(method = "lm", se = FALSE, aes(color = my_colors[1]))+
  theme_home()+ theme(plot.title = element_text(size = 11))+
  xlab(NULL)+ ylab(NULL)+
  ggtitle("Porality Over Time Reliance.F")
print(polarity_over_time_rf)

#Percent Positive Over Time
relative_polarity_over_time_rf <- home_polarity_year_rf %>% 
  ggplot(aes(year, percent_positive, color = ifelse(polarity >=0, my_colors[5], my_colors[4])))+
  geom_col()+
  geom_smooth(method = "loess", se = FALSE)+
  geom_smooth(method = "lm", se = FALSE, aes(color = my_colors[1]))+
  theme_home() + theme(plot.title = element_text(size = 11))+
  xlab(NULL)+ ylab(NULL)+
  ggtitle("Percent Positive Over Time Reliance.F")

grid.arrange(polarity_over_time_rf, relative_polarity_over_time_rf, ncol = 2)


##Mood Ring by year
grid.col = c("2018" = my_colors[1],"2019" = my_colors[2],"2020" = my_colors[3],"2021" = my_colors[4],"2021" = my_colors[5],
             "positive" = "grey","trust" = "grey","anticipation" = "grey","joy" = "grey","negative" = "grey","fear" = "grey","surprise" = "grey","sadness" = "grey","anger" = "grey","disgust" = "grey")

year_mood <- home_nrc %>% 
  filter(year > 2017) %>% 
  count(sentiment, year) %>% 
  group_by(year,sentiment) %>% 
  summarise(sentiment_sum = sum(n)) %>% 
  ungroup()

circos.clear()
#set the gap size
circos.par(gap.after = c(rep(5, length(unique(year_mood[[1]]))-1),15,
                         rep(5, length(unique(year_mood[[2]]))-1),15))
chordDiagram(year_mood, grid.col = grid.col, transparency = .2)
title("Relationship Between Mood and Year")



##Mood Ring by company
grid.col = c("rf" = my_colors[1],"better" = my_colors[2],"jvm" = my_colors[3],
             "positive" = "grey","trust" = "grey","anticipation" = "grey","joy" = "grey","negative" = "grey","fear" = "grey","surprise" = "grey","sadness" = "grey","anger" = "grey","disgust" = "grey")

company_mood <- home_nrc %>%
  count(sentiment, company) %>% 
  group_by(company,sentiment) %>% 
  summarise(sentiment_sum = sum(n)) %>% 
  ungroup()

circos.clear()
#set the gap size
circos.par(gap.after = c(rep(3, length(unique(company_mood[[1]]))-1),15,
                         rep(3, length(unique(company_mood[[2]]))-1),15))
chordDiagram(company_mood, grid.col = grid.col, transparency = .2)
title("Relationship Between Mood and Company")


##Mood Ring by Website
grid.col = c("google" = my_colors[1],"zillow" = my_colors[2],"nextdoor" = my_colors[3],"yelp" = my_colors[4],
             "positive" = "grey","trust" = "grey","anticipation" = "grey","joy" = "grey","negative" = "grey","fear" = "grey","surprise" = "grey","sadness" = "grey","anger" = "grey","disgust" = "grey")

site_mood <- home_nrc %>%
  count(sentiment, site) %>% 
  group_by(site,sentiment) %>% 
  summarise(sentiment_sum = sum(n)) %>% 
  ungroup()

circos.clear()
#set the gap size
circos.par(gap.after = c(rep(4, length(unique(site_mood[[1]]))-1),15,
                         rep(4, length(unique(site_mood[[2]]))-1),15))
chordDiagram(site_mood, grid.col = grid.col, transparency = .2)
title("Relationship Between Mood and Website")


##Real-Time Sentiment General
year_month_bing <- home_bing %>% 
  filter(year > "2016") %>% 
  group_by(year_month, sentiment) %>% 
  count(year_month, sentiment) %>% 
  spread(sentiment, n) %>% 
  mutate(polarity = positive- negative,
         ratio = polarity / (positive + negative))

year_month_bing %>% 
  mutate(sentiment = ifelse(positive>negative,"positive","negative")) %>% 
  ggplot(aes(year_month, polarity, fill = sentiment))+
  geom_bar(stat = "identity")+
  theme_minimal() + theme(legend.position = "none")+
  xlab(NULL)+
  ggtitle("Sentiment Positive by Year-Quarter")+
  coord_flip()

##Real-Time Sentiment Reliance Financial 4Q
year_month_bing_rf <- home_bing %>%
  filter(year > "2016") %>% 
  filter(company == "rf") %>% 
  group_by(site, year_month, sentiment) %>% 
  count(site, year_month, sentiment) %>% 
  spread(sentiment, n) %>% 
  mutate(polarity = positive- negative,
         ratio = polarity / (positive + negative))

year_month_bing_rf %>% 
  mutate(sentiment = ifelse(positive>negative,"positive","negative")) %>% 
  ggplot(aes(year_month, polarity, fill = site))+
  geom_bar(stat = "identity")+
  guides(color = guide_legend(title = "Number of cylinders"),
         shape = guide_legend(title = "Number of cylinders")) +
  theme(legend.position = "bottom")+
  xlab(NULL)+
  ggtitle("Sentiment by Year-Quarter with Website")+
  coord_flip()


#####NRC Sentiment Company Analysis
home_nrc_sub %>% 
  filter(year > "2019") %>% 
  count(company,sentiment,year) %>% 
  mutate(sentiment = reorder(sentiment,n),company = reorder(company,n)) %>% 
  ggplot(aes(sentiment, n, fill = sentiment))+
  geom_col()+
  facet_wrap(year ~ company, scale = "free_x", labeller = label_both)+
  theme_home()+
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_blank())+
  labs(x = NULL, y = NULL)+
  ggtitle("NRC Sentiment Company Analysis")+
  coord_flip()

#####NRC Sentiment Website Analysis
home_nrc_sub %>% 
  filter(year > "2020") %>% 
  #filter(company == "rf") %>% 
  count(site,sentiment,year) %>% 
  mutate(sentiment = reorder(sentiment,n),site = reorder(site,n)) %>% 
  ggplot(aes(sentiment, n, fill = sentiment))+
  geom_col()+
  facet_wrap(year ~ site, scale = "free_x", labeller = label_both)+
  theme_home()+
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_blank())+
  labs(x = NULL, y = NULL)+
  ggtitle("NRC Sentiment Website Analysis")+
  coord_flip()


##############################################################
## N-gram
##############################################################
home_bigram <- home %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_separated <- home_bigram %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% undesirable_words) %>%
  filter(!word2 %in% undesirable_words)

##Bigrams Per Year: Company
#Because there is so much repetition in text, also filter out the cases where the two words are the same
bigram_year <- bigrams_filtered %>%
  filter(year > "2017") %>% 
  filter(word1 != word2) %>% 
  filter(word1 != "NA") %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  inner_join(home) %>% 
  count(bigram, year, sort = TRUE) %>% 
  group_by(year) %>% 
  slice(seq_len(7)) %>% 
  ungroup() %>% 
  arrange(year, n) %>% 
  mutate(row = row_number())
  
bigram_year %>% 
  ggplot(aes(row, n, fill = year))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~year, scales = "free_y")+
  xlab(NULL)+ ylab(NULL)+
  scale_x_continuous(
    breaks = bigram_year$row,
    labels = bigram_year$bigram)+
  theme_home()+
  theme(panel.grid.major.x = element_blank())+
  ggtitle("Bigrams Per Year")+
  coord_flip()


##Bigrams Per Year: Reliance Financial
#Because there is so much repetition in text, also filter out the cases where the two words are the same
bigram_year <- bigrams_filtered %>%
  filter(year > "2017") %>% 
  filter(company == "rf") %>% 
  filter(word1 != word2) %>% 
  filter(word1 != "NA") %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  inner_join(home) %>% 
  count(bigram, year, sort = TRUE) %>% 
  group_by(year) %>% 
  slice(seq_len(7)) %>% 
  ungroup() %>% 
  arrange(year, n) %>% 
  mutate(row = row_number())

bigram_year %>% 
  ggplot(aes(row, n, fill = year))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~year, scales = "free_y")+
  xlab(NULL)+ ylab(NULL)+
  scale_x_continuous(
    breaks = bigram_year$row,
    labels = bigram_year$bigram)+
  theme_home()+
  theme(panel.grid.major.x = element_blank())+
  ggtitle("Bigrams Per Year: Reliance Financial")+
  coord_flip()


####
##Sentiment negation with Bigram
AFINN <- get_sentiments("afinn")

not_words <- bigrams_separated %>% 
  filter(word1 == "not") %>% 
  inner_join(AFINN, by = c(word2 = "word")) %>% 
  count(word2, value, sort = TRUE) %>% 
  ungroup()

not_words %>% 
  mutate(contribution = n*value) %>% 
  arrange(desc(abs(contribution))) %>% 
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>% 
  ggplot(aes(word2, n*value, fill = n*value >0))+
  geom_col(show.legend = FALSE)+
  theme_home()+
  xlab("Words preceded by \"not\"")+ ylab("Sentiment score * Number of Occurrence")+
  ggtitle("Polar Sentiment of Words Preceded by Not")+
  coord_flip()
  ####

##Pairwise Comparisons
pwc <- home_tidy %>% 
  filter(n() >= 20) %>%  
  pairwise_count(word, name, sort = TRUE) %>% 
  filter(item1 %in% c("experience","process","recommend","team")) %>% 
  group_by(item1) %>% 
  slice(seq_len(7)) %>% 
  ungroup() %>% 
  mutate(row = -row_number())

pwc %>% 
  ggplot(aes(row, n, fill = item1))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  facet_wrap(~item1, scales = "free")+
  scale_x_continuous(
    breaks = pwc$row,
    labels = pwc$item2)+
  theme_home()+ theme(panel.grid.major.x = element_blank())+
  xlab(NULL)+ ylab(NULL)+
  ggtitle("Pairwise Count")+
  coord_flip()

##Pairwise Reliance Financial
pwc_rf <- home_tidy %>% 
  filter(company == "rf") %>% 
  filter(n() >= 50) %>%  
  pairwise_count(word, name, sort = TRUE) %>% 
  filter(item1 %in% c("experience","process","recommend","team")) %>% 
  group_by(item1) %>% 
  slice(seq_len(10)) %>% 
  ungroup() %>% 
  mutate(row = -row_number())

pwc_rf %>% 
  ggplot(aes(row, n, fill = item1))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  facet_wrap(~item1, scales = "free")+
  scale_x_continuous(
    breaks = pwc_rf$row,
    labels = pwc_rf$item2)+
  theme_home()+ theme(panel.grid.major.x = element_blank())+
  xlab(NULL)+ ylab(NULL)+
  ggtitle("Pairwise Count: Reliance Financial")+
  coord_flip()


##Pairwise correlation: all
home_tidy %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, name, sort = TRUE) %>%
  filter(item1 %in% c("experience","process","recommend","team")) %>%
  group_by(item1) %>%
  top_n(7) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation, fill = item1)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  facet_wrap(~item1, scales = 'free') +
  theme_home() + theme(panel.grid.major.x = element_blank()) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Pairwise Correlation") +
  coord_flip()

##Pairwise correlation: Reliance Financial
home_tidy %>%
  filter(company == "rf") %>% 
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, name, sort = TRUE) %>%
  filter(item1 %in% c("experience","process","recommend","time")) %>%
  group_by(item1) %>%
  top_n(7) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation, fill = item1)) +
  geom_bar(stat = 'identity', show.legend = FALSE) +
  facet_wrap(~item1, scales = 'free') +
  theme_home() + theme(panel.grid.major.x = element_blank()) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Pairwise Correlation Reliance.F" ) +
  coord_flip()



