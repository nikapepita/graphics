
packages <- c("readr","tidyverse","ggwordcloud","tidytext","SnowballC",
              "udpipe","ggridges", "multipanelfigure","ggrepel","extrafont")


package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
#load Data Tidy Tuesday
user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')

#Download English Model (udpipe package)
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)

#Map Model to the unnested word values from th euser reviews
#This is how we find the adjectives
user_tokens <- user_reviews %>% unnest_tokens(word,text)
user_model <- as_tibble(udpipe_annotate(ud_model, x = user_tokens$word))

#Code to remove stop words, obtain word sentiment, and filter for adjectives
user_reviews_augmented <- user_tokens %>% 
  filter(!word %in% stop_words$word) %>%
  inner_join(get_sentiments("bing")) %>%
  inner_join(user_model,by=c("word"="token")) %>%
  filter(upos %in% c("ADJ"))

#Simple code to provide the number of unique reviews mentioning the word along with avg rating and sentiment 
user_review_summary <- user_reviews_augmented %>%
  group_by(word) %>%
  summarize(mean_rating=mean(grade),
            sentiment=first(sentiment),
            mentions=n_distinct(user_name))

#Code I use to identify Negative Words and Positive words associated with each rating
words_of_interest <- user_review_summary %>%
  filter((mentions>=20 & mean_rating>7 & sentiment=="positive") | (mentions>=20 & mean_rating<5 & sentiment=="negative")) %>%
  filter(word!="wrong") %>%
  select(word:sentiment)

#Code to get dataset for ridgeline charts
#Needed the data in this format for the ridgeline to work properly
density_data <- user_reviews_augmented %>%
  inner_join(words_of_interest) %>%
  group_by(word) %>%
  mutate(mentions=n_distinct(user_name))%>%
  distinct(user_name, word, sentiment, mean_rating, grade, mentions)


#plot negativ words to the grade
#code for bottom right chart - almost identical to the negative chart
ridgeline <- ggplot() +
  geom_density_ridges_gradient(data=density_data %>% filter(sentiment=="positive"),
                               aes(x = grade,
                                   y = reorder(word, -mentions),
                               fill = ..x..),
                               scale = 1.5,
                               rel_min_height = 0.001,
                               gradient_lwd=0.5) +
  scale_fill_gradient2(low = "#F8766D",
                       high = "#00BFC4",
                       midpoint=5)+
  theme_ridges(font_size = 18)+
  theme(legend.position="none",
        axis.title = element_blank(),
        plot.title = element_text(hjust=0.5,
                                  size=16,
                                  color="grey25"),
        axis.text=element_text(hjust = 0.5,
                               size=14,
                               color="grey25"),
        axis.text.y=element_text(color="#00BFC4"))+
  labs(title="Positive Words Associated With Higher Ratings")

###Violin Data
violin <- ggplot(data=density_data %>% filter(sentiment=="positive"), aes(x=grade, y=reorder(word, -mentions))) + 
  geom_violin()+
  scale_fill_gradient2(low = "#F8766D",
                       high = "#00BFC4",
                       midpoint=5)+
  theme_ridges(font_size = 18)+
  theme(legend.position="none",
        axis.title = element_blank(),
        plot.title = element_text(hjust=0.5,
                                  size=16,
                                  color="grey25"),
        axis.text=element_text(hjust = 0.5,
                               size=14,
                               color="grey25"),
        axis.text.y=element_text(color="#00BFC4"))+
  labs(title="Positive Words Associated With Higher Ratings")

###Lollipop

# Most basic bubble plot
bubble <- density_data %>% filter(sentiment=="positive") %>%
  ggplot(aes(x=grade, y=word, size = mentions,color=mycolor)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Population (M)")

ridgeline
violin
bubble