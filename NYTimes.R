# Install needed packages if not already in library
#install.packages("tm")
#install.packages("quanteda")
#install.packages("tidyverse")
#install.packages("stringr")
#install.packages("tidytext")
#install.packages("jsonlite")
#install.packages("magrittr")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("curl")
#install.packages("qdap")
#install.packages("reshape2")

# Attaching needed packages
library(tm) # Text manipulation
library(quanteda) # Quantitative analysis of textual data
library(tidyverse) # Text Manipulation
library(stringr)  # Text Manipulation
library(tidytext) # Sentiment scores
library(jsonlite) # Article scraping
library(magrittr) # Creating easier to read pipelines
library(dplyr) # Data manipulation
library(ggplot2) # Plotting
library(curl) # Handling URLs
library(data.table) # Original form from NYT API
library(wordcloud) # wordcloud
library(qdapDictionaries) # Negation words
library(stringi) # list to matrix (to data frame)
library(lazyeval) # interpolate values function needed from this
library(reshape2) # cast to dataframe (opposite of melt)

setwd("~/R/win-library/Financial Analytics/Assignment 1")

# # API Key to New York Times API
# NYTIMES_KEY = "0ed1d32810b14dda98fc2c0f4f525f29"
# 
# # Modified from http://www.storybench.org/working-with-the-new-york-times-api-in-r/
# 
# # Let's set some parameters
# term <-
#   "trump+donald+j+(Per)" # The keywords to search for. Need to use + to string together separate words
# begin_date <- "20170120"
# end_date <- "20171231"
# 
# # The base URL for the download request returned as a string
# baseurl <-
#   paste0(
#     "http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",
#     term,
#     "&begin_date=",
#     begin_date,
#     "&end_date=",
#     end_date,
#     "&facet_filter=true&api-key=",
#     NYTIMES_KEY,
#     sep = ""
#   )
# 
# # The link in data frame format. Looks good.
# print(baseurl)
# 
# # Running the query. The maximum articles per page is 10
# initialQuery <- fromJSON(baseurl)
# maxPages <- round((initialQuery$response$meta$hits[1] / 10) - 1)
# print(maxPages)
# 
# # The API seems to let only download 201 pages each containing 10 articles at a time.
# # To decrease waiting time, the first patch is from 1 to 100.
# 
# pages <- list()
# for (i in 0:100) {
#   nytSearch <-
#     fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame()
#   message("Retrieving page ", i)
#   pages[[i + 1]] <- nytSearch
#   Sys.sleep(1)
# }
# 
# # Combine each article as a row in a data frame
# allNYTSearch <- rbind_pages(pages)
# 
# # The query starts from the newest so I'll run another query starting with the oldest date
# #print(max(allNYTSearch$response.docs.pub_date))
# #new_end_date <- gsub("-", "", substr(min(allNYTSearch$response.docs.pub_date), 1, 10))
# 
# pages2 <- list()
# for (i in 101:200) {
#   nytSearch <-
#     fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame()
#   message("Retrieving page ", i)
#   pages2[[i + 1]] <- nytSearch
#   Sys.sleep(1)
# }
# 
# allNYTSearch2 <- rbind_pages(pages2)
# 
# allNYTSearch <- rbind(allNYTSearch, allNYTSearch2)

# # Sort to have decreasing order by publication date
# 
# Articles <-
#   allNYTSearch[order(allNYTSearch$response.docs.pub_date, decreasing = TRUE), ]
# df <- apply(Articles, 2, as.character)
# 
# # Save the articles as CSV
# setwd("~/R/win-library/Financial Analytics/Assignment 1")
# write.csv(df, file = "NYT Articles 2017.csv")

#############################################################################################
# If the scraping is already done once, use the saved csv for analysis starting from here.  #
#############################################################################################
setwd("~/R/win-library/Financial Analytics/Assignment 1")

# clears the environment window pane in RStudio
rm(list = ls())

Articles <- read_csv("NYT Articles 2017.csv")

# If plotting doesn't work, set theme as: theme_set(theme_grey())

# Creating a function to assign a variable from the pipe operator
keep <-
  function(x, name) {
    assign(as.character(substitute(name)), x, pos = 1)
  }


# Visualize coverage by section
Articles %>%
  group_by(response.docs.type_of_material) %>%
  summarize(count = n()) %>%
  mutate(percent = (count / sum(count)) * 100) %>%
  ggplot() +
  geom_bar(
    aes(y = percent, x = response.docs.type_of_material, fill = response.docs.type_of_material),
    stat = "identity"
  ) + coord_flip()


# Visualize the days of coverage with 3 or more articles written
Articles %>%
  mutate(pubDay = gsub("T.*", "", response.docs.pub_date)) %>%
  group_by(pubDay) %>%
  summarise(count = n()) %>%
  filter(count >= 3) %>%
  keep(frequent) %>%
  ggplot() +
  geom_bar(aes(x = reorder(pubDay, count), y = count), stat = "identity") + coord_flip()

# Renaming and dropping unnecessary columns
Articles %>%
  mutate(docType = response.docs.document_type) %>%
  mutate(pubType = response.docs.type_of_material) %>%
  mutate(pubDay = gsub("T.*", "", response.docs.pub_date)) %>%
  mutate(Section = response.docs.section_name) %>%
  mutate(Headline = response.docs.headline.main) %>%
  mutate(Snippet = response.docs.snippet) %>%
  mutate(Authors = response.docs.byline.original) %>%
  mutate(pubSource = response.docs.source) %>%
  keep(data)


Articles <- data[, 33:length(colnames(data))]

# separating multimedia components from the articles
multimedia <- Articles[grep("multimedia", Articles$docType), ]
articles <- Articles[grep("article", Articles$docType), ]

# See Articles by section
articles %>%
  group_by(Section) %>%
  summarise(count = n()) %>%
  filter(count >= 30) %>%
  na.omit() %>%
  ggplot() +
  geom_bar(aes(x = reorder(Section, count), y = count), stat = "identity") + coord_flip() +
  labs(x = "", y = "", title = "Articles", subtitle = "Count by Section") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 8),
        axis.text = element_text(size = 8),
        axis.title.x = element_text(size = 8))

# Cleaning the column authors and splitting each author to own column
authors  <- articles$Authors %>%
  str_split_fixed("By | \\s+|and|\\,|Photographs by |Interview by |Compiled by ", n = 5) %>%
  unlist() %>%
  data.frame()

# Removing NA rows the hard way
authors[authors == ""] <- NA
NonNAindex <- which(!is.na(authors[1]))
authors[NonNAindex, 2] <- authors[NonNAindex, 1]
authors[, 1] <- NULL
colnames(authors) <- c("Author1", "Author2", "Author3", "Author4")

# Combining the dataframes
combined <- cbind(articles, authors)
fullrows <- complete.cases(combined[, 9])
clean_df <- combined[fullrows,]

#  Cleaning the dataframe further and saving all individual authors
authorcounts <- unlist(authors, use.names = FALSE) %>%
  as.character() %>%
  na.omit() %>%
  c() %>%
  str_replace_all("(^\\s+)|(\\s+$)", "") %>%
  data.frame() %>%
  set_names("Authors")


#  Plotting counts of authors that have been involved with more than 25 articles
authorcounts %>%
  group_by(Authors) %>%
  summarise(count = n()) %>%
  filter(count >= 40) %>%
  arrange(desc(count)) %>%
  print() %>%
  keep(frequent_authors) %>%
  ggplot() +
  geom_bar(aes(x = reorder(Authors, count), y = count), stat = "identity") + coord_flip() +
  labs(x = "", y = "", title = "Articles", subtitle = 'Count by Author') +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 8),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(size = 7), 
        axis.title.x = element_text(size = 8))

top_reporters <- as.character(unlist(frequent_authors[1:5, 1]))


# The editorial board has been involved in editorial publications only. The majority of the writings are news
clean_df %>%
  filter(
    str_detect(
      Authors,
      "JULIE HIRSCHFELD DAVIS|MAGGIE HABERMAN|MARK LANDLER|PETER BAKER|THE EDITORIAL BOARD"
    )
  ) %>%
  group_by(pubType) %>%
  summarise(count = n()) %>%
  ggplot() +
  geom_bar(aes(x = reorder(pubType, count), y = count), stat = "identity") + coord_flip()

#### Days when most articles written mentioning Trump: busy_days

clean_df %>%
  group_by(pubDay) %>%
  summarise(count = n()) %>%
  filter(count >= 3) %>%
  keep(busy_days) %>%
  ggplot() +
  geom_bar(aes(x = reorder(pubDay, count), y = count), stat = "identity") + coord_flip() +
  labs(x = "Date", Y = "Count", title = "Days with Most Articles Involving Trump")

#####################
# TEXT CLEANING     #
#####################

# Cleaning texts
all_texts <- clean_df %>%
  select(Headline, Snippet) %>%
  unlist() %>%
  iconv("", "UTF-8", "byte") %>%
  matrix(ncol = 2, nrow = length(clean_df[, 1])) %>%
  data.frame() %>%
  set_names(c("Headline", "Snippet"))

# Replacing the originals with cleaned ones
clean_df["Headline"] <- NULL
clean_df["Snippet"] <- NULL
clean_df <- cbind(clean_df, all_texts)

a_texts <- clean_df %>%
  filter(
    str_detect(
      Authors,
      "JULIE HIRSCHFELD DAVIS|MAGGIE HABERMAN|MARK LANDLER|PETER BAKER|THE EDITORIAL BOARD"
    )
  ) %>%
  select(Headline, Snippet, Author1, Author2, Author3, Author4)

remove_odd_a <- function(x)  gsub('â', "", x)

# Further cleaning of text
clean_text <- all_texts %>%
  unlist() %>%
  as.character() %>%
  removeNumbers() %>%
  removePunctuation() %>%
  remove_odd_a() %>%
  data.frame(stringsAsFactors = FALSE)

tidy_text <- clean_text %>%
  data.frame(stringsAsFactors = FALSE) %>%
  unnest_tokens(word, ".")

# Separating headlines and snippets

headlines <- clean_text[1:1737, ] %>%
  data.frame(stringsAsFactors = FALSE) %>%
  unnest_tokens(word, ".") %>%
  count(word, sort = TRUE)

snippets <- clean_text[1737:3473, ] %>%
  data.frame(stringsAsFactors = FALSE) %>%
  unnest_tokens(word, ".") %>%
  count(word, sort = TRUE)

# Frequency of words

tidy_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 200) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

headlines %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

snippets %>%
  filter(n > 200) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Wordclouds

data("stop_words")

headlines %>%
  anti_join(stop_words) %>%
  with(wordcloud(
    word,
    n,
    max.words = 100,
    colors = brewer.pal(3, "Spectral")
  ))

snippets %>%
  anti_join(stop_words) %>%
  with(wordcloud(
    word,
    n,
    max.words = 100,
    colors = brewer.pal(3, "Spectral")
  ))


snip_freq <- snippets %>%
  anti_join(stop_words) %>%
  mutate(total = length(word)) %>%
  mutate(prob = n / total)


# Negating words

snippets_bigram <- clean_text[1737:3473, ] %>%
  data.frame(stringsAsFactors = FALSE) %>%
  unnest_tokens(bigram, ".", token = "ngrams", n = 2) %>%
  mutate(linenumber = row_number())

snippets_separated <- snippets_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

snip_bi_filt <- snippets_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

snip_counts <- snip_bi_filt %>%
  count(word1, word2, sort = TRUE)

snippets_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

AFINN <- get_sentiments("afinn")

not_words <- snippets_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

not_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words Preceded By \"not\"") +
  ylab("Sentiment Score * Number of Occurrences") +
  coord_flip() +
  theme_minimal(base_size = 14)

# Load negation words from qdapDictionaries

data(negation.words)
negation_words <- c(negation.words, "without", "less")


snippets_negated_words <- snippets_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, score, sort = TRUE) %>%
  ungroup()

snippets_negated_words %>%
  mutate(contribution = n * score) %>%
  filter(abs(contribution) >= 2) %>%
  arrange(desc(contribution)) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) + scale_fill_brewer(palette = "Paired") +
  labs(x = "", 
       y = "Sentiment Score * Number of Occurrences", 
       title = "Articles", 
       subtitle = "Words Preceded By Negation Word") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 14),
        axis.text = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        legend.text = element_text(size = 10)) +
  coord_flip() 



############# THE TWEET SECTION ##################

# Download the needed packages if not already in library
#install.packages("rtweet")
#install.packages("httpuv")
#install.packages("lubridate")
#install.packages("readr")
#install.packages("tidyr")
#install.packages("sentimentr")

# Add needed packages
library(rtweet) # Updated Twitter package
library(httpuv) # URL help
library(lubridate) # datetime
library(readr) # Read rectangular text data
library(tidyr) # Tidy text manipulation
library(sentimentr) # Sentiment analysis
library(scales) # scale to log for frequency differences

# # Download tweets from the timeline, maximum of the API is 3200
# 
# trumpTBL <- get_timelines(c("realDonaldTrump"), n = 3200)
# 
# # we only need these columns for our analysis
# trumpTBL_Slim <- trumpTBL %>%
#   select(created_at, text, retweet_count, favorite_count, status_id)
# 
# # Save the results to CSV for later examination
# write.csv(trumpTBL_Slim, file = "TrumpTweets.csv")

#############################################################################################
# If the scraping is already done once, use the saved csv for analysis starting from here.  #
#############################################################################################

setwd("~/R/win-library/Financial Analytics/Assignment 1")

# Use the tweets downloaded earlier
tweets <- read_csv('TrumpTweets.csv')

tweets <- tweets %>%
  mutate(created_at = date(ymd_hms(created_at))) %>%
  mutate(X1 = "Trump")

ggplot(tweets,
       aes(
         x = created_at,
         xmin = as.Date("2017-01", "%Y-%m"),
         xmax = as.Date("2018-05",  "%Y-%m"),
         fill = X1
       )) +
  geom_histogram(
    bins = 20,
    show.legend = FALSE,
    col = "black",
    size = .1,
    fill = "lightblue"
  ) +
  labs(x = "", y = "", title = "Number of Trump Tweets Over Time") +
  scale_x_date(breaks = date_breaks("4 months"), labels = date_format("%m-%Y")) +
  theme_minimal(base_size = 14)

text_utf <- tweets$text %>%  iconv("", "UTF-8", "byte")

# Creating helpful functions needed for twitter text cleaning

remove_URL_rt <- function(x)   gsub("http[^[:space:]]*|^RT", "", x)
removearrows <- function(x)   gsub("<.*>", "", x)
myStopwords <-  c(setdiff(stopwords(language = 'english'), c("r", "big", "w")), "use", "see", "used", "via", "amp")
remove_odd_chars <-  function(x)  {  gsub('[~!$^&*()_+:?,./;-=%\"""--]', "", x) }

# Applying the cleaning functions
text_utf <- remove_URL_rt(text_utf)
text_utf <- removearrows(text_utf)
text <- removeWords(text_utf, myStopwords)
text <- remove_odd_chars(text)
text <- removeNumbers(text)
text <- stripWhitespace(text)
text <- tolower(text)

# Tokenization
tidy_tweets <- text %>%
  data.frame(stringsAsFactors = FALSE) %>%
  set_names("word") %>%
  unnest_tokens("word", word)

# EXTRACTING HASHTAGS AND USERNAMES
# define a hashtag and username extractor functions
hashtags <-
  function(x) {
    toupper(grep("^#", strsplit(x, " +")[[1]], value = TRUE))
  }

usernames <-
  function(x) {
    tolower(grep("^@.+", strsplit(x, " +")[[1]], value = TRUE))
  }

# List of the hashtags and usernames for each tweet
taglist <- vector(mode = "list", length(text))
userlist <- vector(mode = "list", length(text))

# Populating the prior
for (i in 1:length(text)) {
  taglist[[i]] <- hashtags(text[i])
}

for (i in 1:length(text)) {
  userlist[[i]] <- usernames(text[i])
}



# TWEET WORDCLOUDS

# Cleaning for wordcloud
hashtag_df <- t(stri_list2matrix(taglist)) %>%
  as.data.frame() %>%
  unlist() %>%
  data.frame() %>%
  set_names("tag") %>%
  na.omit() %>%
  count(tag, sort = TRUE)

# Hashtag wordcloud
hashtag_df %>%
  with(wordcloud(
    tag,
    n,
    max.words = 20,
    colors = brewer.pal(3, "Paired"),
    random.order = FALSE,
    scale = c(4, .4)
  ))



# Cleaning for wordcloud
username_df <- userlist %>%
  unlist() %>%
  as.character() %>%
  data.frame() %>%
  set_names("user") %>%
  group_by(user) %>%
  count(user, sort = TRUE)

# Username wordcloud
username_df %>%
  with(wordcloud(
    user,
    n,
    max.words = 20,
    colors = brewer.pal(3, "Paired"),
    random.order = FALSE,
    scale = c(4, .4)
  ))


## Most frequent words

text %>%
  data.frame(stringsAsFactors = FALSE) %>%
  unnest_tokens(word, ".") %>%
  count(word, sort = TRUE) %>%
  filter(n > 200) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Text wordcloud

tidy_tweets %>%
  filter(nchar(word) > 2) %>% 
  filter(!word == "the") %>% 
  count(word, sort = TRUE) %>%
  with(
    wordcloud(
      word,
      n,
      min.freq = 20,
      max.words = 50,
      rot.per = 0.35,
      colors = brewer.pal(3, "Paired"),
      random.order = FALSE,
      scale = c(3, .05)
    )
  )

simple_tweet_sentiment <- text %>%
  data.frame(stringsAsFactors = FALSE) %>%
  unnest_tokens(word, ".") %>%
  inner_join(AFINN, by = c(word = "word")) %>%
  count(word, score, sort = TRUE) %>%
  ungroup()

tweet_freq <- tidy_tweets %>%
  filter(created_at <= "2017-12-31") %>% 
  count(word, sort = TRUE) %>%
  mutate(total = length(word)) %>%
  mutate(prob = n / total)


tweets_bigram <- text %>%
  data.frame(stringsAsFactors = FALSE) %>%
  unnest_tokens(bigram, ".", token = "ngrams", n = 2) %>%
  mutate(linenumber = row_number())

tweets_separated <- tweets_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

tweet_bi_filt <- tweets_separated %>%
  filter(!word1 %in% myStopwords) %>%
  filter(!word2 %in% myStopwords)

tweet_counts <- tweet_bi_filt %>%
  count(word1, word2, sort = TRUE)

tweets_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

AFINN <- get_sentiments("afinn")

not_words <- tweets_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

not_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()

# Load negation words from qdapDictionaries
data(negation.words)
negation_words <- c(negation.words, "without", "less")


tweets_negated_words <- tweets_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

# Largest effects of negation terms. The sentiment seems to be worse than suggested
tweets_negated_words <- tweets_negated_words %>%
  mutate(contribution = n * score)

tweets_negated_words %>%
  filter(abs(contribution) > 2) %>%
  arrange(desc(contribution)) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) + scale_fill_brewer(palette = "Paired") +
  labs(x = "", 
       y = "Sentiment Score * Number of Occurrences", 
       title = "Tweets", 
       subtitle = "Words Preceded By Negation Word") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 14),
        axis.text = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        legend.text = element_text(size = 10)) +
  coord_flip()

sum(tweets_negated_words$contribution)

## Check the sentiment for words before a mention of news media

news_medias <- c("foxnews", "fox", "nytimes", "cnn")

## BIGRAM

tweets_bigram <- text %>%
  data.frame(stringsAsFactors = FALSE) %>%
  unnest_tokens(bigram, ".", token = "ngrams", n = 2) %>%
  mutate(linenumber = row_number())

tweets_separated <- tweets_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

tweet_bi_filt <- tweets_separated %>%
  filter(!word1 %in% myStopwords) %>%
  filter(!word2 %in% myStopwords)

sentiment_newsmedia_bigram <- tweet_bi_filt %>%
  filter(word2 %in% news_medias) %>%
  inner_join(AFINN, by = c(word1 = "word")) %>%
  count(word1, word2, score, sort = TRUE) %>%
  mutate(contribution = n * score) %>%
  mutate(media = ifelse(word2 == "fox", "foxnews", word2)) %>% 
  group_by(media) %>% 
  summarise(contribution = sum(contribution))

## TRIGRAM
tweets_trigram <- text %>%
  data.frame(stringsAsFactors = FALSE) %>%
  unnest_tokens(trigram, ".", token = "ngrams", n = 3) %>%
  mutate(linenumber = row_number())

tweets_separated <- tweets_trigram %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

tweet_tri_filt <- tweets_separated %>%
  filter(!word1 %in% myStopwords) %>%
  filter(!word2 %in% myStopwords) %>% 
  filter(!word3 %in% myStopwords)

sentiment_before_newsmedia <- tweet_tri_filt %>%
  filter(word3 %in% news_medias) %>%
  left_join(AFINN, by = c(word1 = "word")) %>%
  mutate(score_word1 = score, score = NULL) %>% 
  left_join(AFINN, by = c(word2 = "word")) %>% 
  mutate(score_word2 = score, score = NULL) 

sentiment_before_newsmedia <- sentiment_before_newsmedia[(rowSums(is.na(sentiment_before_newsmedia[,5:6])) < 2),]

# Replace NAs with 0 for all columns
# x %>% mutate_all(funs_(interp(~replace(., is.na(.),0))))


# Easier way would be to just sum the 1 and 2 day prior sentiments but it might not always give a true view.
# Here for example two_words prior is often wow (sentiment score of 4), but when followed by "failing" the
# sentiment of wow should not be even considered. The fact that many of the tweets also start with just one
# word preceding mention of newsmedia makes the sentiment less significant.
# So, this is not really needed.

sentiment_newsmedia_trigram <- sentiment_before_newsmedia %>% 
  mutate_all(funs_(interp(~replace(., is.na(.),0)))) %>% 
  count(word1, word2, word3, score_word1, score_word2, sort = TRUE) %>%
  mutate(contribution = n * (score_word1 + score_word2)) %>%
  mutate(media = ifelse(word3 == "fox", "foxnews", word3)) %>% 
  group_by(media) %>% 
  summarise(contribution = sum(contribution))


# Test if the first word is the one with the sentiment
sentiment_two_words_prior <- sentiment_before_newsmedia %>% 
  mutate_all(funs_(interp(~replace(., is.na(.),0)))) %>% 
  count(word1, word3, score_word1, sort = TRUE) %>%
  mutate(contribution = n * score_word1) %>%
  mutate(media = ifelse(word3 == "fox", "foxnews", word3)) %>% 
  group_by(media) %>% 
  summarise(contribution = sum(contribution))

# Create a dataframe of the observations and plot it

newsmedia_sentiments <- data.frame(sentiment_two_words_prior$contribution,
                                   sentiment_newsmedia_bigram$contribution) %>% 
  set_names(c("two", "one")) %>% 
  mutate(media = (c("CNN", "Fox", "NYTimes"))) %>% 
  melt(id.vars = 'media') %>% 
  mutate(media = reorder(media, value))
  


newsmedia_sentiments %>% 
  ggplot(aes(x = media, y = value, fill=factor(variable))) +
  geom_col(position = 'stack', width = 0.8) + scale_fill_brewer(palette = "Paired") +
  labs(x = "",
       y = "Sentiment Score * Number of Occurrences", 
       title = "Word Sentiment", 
       subtitle = "Number of Words Preceeding Mentions of News Media",
       fill='Words Prior') +
  coord_flip() +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 12),
    plot.subtitle = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title.y = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8)
  )
  

####################################################
# THE COMBINED PART STARTS HERE                    #
####################################################

# Combining the twitter and articles words

combined <- bind_rows(tweet_freq %>%
                        mutate(source = "Trump"),
                      snip_freq %>%
                        mutate(source = "NYTimes"))

combined_freqs <- combined %>%
  select(source, word, prob) %>%
  spread(source, prob) %>%
  arrange(NYTimes, Trump)



# Checking how the frequencies of words used differ between the two

ggplot(combined_freqs, aes(NYTimes, Trump)) +
  geom_jitter(
    alpha = 0.2,
    size = 2.5,
    width = 0.3,
    height = 0.3,
    colour = "lightblue"
  ) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  labs(x = "New York Times", y = "Donald Trump's Tweets", title = "Frequency of Words by Source") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 12),
    axis.text = element_text(size = 8),
    axis.title.y = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    legend.text = element_text(size = 8)
  )


# How about their Sentiments?

BING <- get_sentiments("bing")

# Probability of sentiments
sentiment_probs <- combined %>%
  inner_join(BING, by = c(word = "word")) %>%
  group_by(source, sentiment) %>%
  summarise(count = n()) %>%
  mutate(perc = count / sum(count)*100)


combined$n  <- NULL

polarity_chart <- combined %>%
  inner_join(BING, by = c(word = "word")) %>%
  count(sentiment, source) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(
    polarity = positive - negative,
    percent_positive = positive / (positive + negative) * 100
  )

## Function to fix contractions
fix.contractions <- function(x) {
  x <- gsub("'ll", " will", x)
  x <- gsub("'re", " are", x)
  x <- gsub("'ve", " have", x)
  x <- gsub("won't", "will not", x)
  x <- gsub("can't", "can not", x)
  x <- gsub("n't", " not", x)
  x <- gsub("'m", " am", x)
  x <- gsub("'d", " would", x)
  x <- gsub("'s", "", x)
  return(x)
}

remove_odd_a <- function(x)
  gsub('â', "", x)

articles_tidy <- clean_df

articles_tidy$Snippet <- articles_tidy$Snippet %>%
  as.character() %>%
  tolower() %>%
  removeNumbers() %>%
  removePunctuation() %>%
  remove_odd_a() %>%
  fix.contractions()

articles_tidy$Headline <- articles_tidy$Headline %>%
  as.character() %>%
  tolower() %>%
  removeNumbers() %>%
  removePunctuation() %>%
  remove_odd_a() %>%
  fix.contractions()

articles_tidy$pubDay <- articles_tidy$pubDay %>%
  ymd_hms() %>%
  date()

tidy_articles <- articles_tidy %>%
  unnest_tokens(word, Snippet)

# Creating helpful functions needed for twitter text cleaning

remove_URL_rt <- function(x)  gsub("http[^[:space:]]*|^RT", "", x)
removearrows <- function(x)  gsub("<.*>", "", x)
myStopwords <-
  c(setdiff(stopwords(language = 'english'), c("r", "big", "w")), "use", "see", "used", "via", "amp")
remove_odd_chars <-  function(x)    gsub('[~!$^&*()_+:?,./;-=%\"""---]', "", x)

tweets$text <- tweets$text %>%
  iconv("", "UTF-8", "byte") %>%
  remove_URL_rt() %>%
  removearrows() %>%
  removeWords(myStopwords) %>%
  remove_odd_chars() %>%
  removeNumbers() %>%
  stripWhitespace() %>%
  tolower()

# Only need the date, to combine with article data
tweets$created_at <- date(tweets$created_at)


tidy_tweets <- tweets %>%
  filter(created_at <= "2017-12-31") %>% 
  unnest_tokens(word, text)

tweet_time_text <- tidy_tweets %>%
  select(created_at, word) %>%
  mutate(date = created_at) %>%
  mutate(source = "Trump") %>%
  select(date, word, source)

article_time_text <- tidy_articles %>%
  select(pubDay, word) %>%
  mutate(date = pubDay) %>%
  mutate(source = "NYTimes") %>%
  select(date, word, source)

undesirable_words <- c("the", "realdonaldtrump")

# All words longer than 2 characters between 2017-01-20 and 2017-12-31 not including "the" and "realdonaldtrump"
tidy_combined <-
  left_join(tweet_time_text, article_time_text, by = c("date", "word")) %>%
  count(word, source.x, source.y, date, sort = TRUE) %>%
  filter(nchar(word) > 2) %>%
  filter(!(word %in% undesirable_words)) %>%
  filter(date <= '2017-12-31')

tidy_combined %>%
  na.omit() %>%
  group_by(date) %>%
  ggplot(aes(x = date, fill = n)) + 
  geom_histogram(
    bins = 24,
    show.legend = FALSE,
    col = 'black',
    size = .1,
    fill = "lightblue"
  ) +
  labs(x = '', y = '', title = "Number of Common Words Used Over Time") +
  scale_x_date(breaks = date_breaks("2 months"), labels = date_format("%m-%Y")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 12),
    axis.text = element_text(size = 8),
    axis.title.y = element_text(size = 10),
    legend.text = element_text(size = 8)
  )




sentiment_over_time <-
  bind_rows(tweet_time_text, article_time_text) %>%
  inner_join(BING, by = c(word = "word")) %>%
  filter(date <= '2017-12-31') %>%
  group_by(date, source, sentiment) %>%
  count(date, source, word, sentiment)

#summarise(count=n()) %>%
# mutate(perc=count/sum(count))

NYT_sentiment_over_time <- article_time_text %>%
  inner_join(BING, by = c(word = "word")) %>%
  filter(nchar(word) > 2) %>%
  filter(!(word %in% undesirable_words)) %>%
  filter(date <= '2017-12-31') %>%
  group_by(date) %>%
  count(date, sentiment)

tweet_sentiment_over_time <- tweet_time_text %>%
  inner_join(BING, by = c(word = "word")) %>%
  filter(nchar(word) > 2) %>%
  filter(!(word %in% undesirable_words)) %>%
  filter(date <= '2017-12-31')
group_by(date) %>%
  count(date, sentiment)


# New York Times Sentiment Over Time
NYT_sentiment_over_time %>%
  ggplot(aes(date)) + scale_fill_brewer(palette = "PuBu") +
  geom_histogram(aes(fill = factor(sentiment)),
                 bins = 11,
                 col = "black",
                 size = .1) +  # change binwidth
  scale_x_date(labels = date_format("%m")) +
  labs(x = 'Month',
       y = 'Number of Sentiment Words',
       title = "NY Times Sentiment Over Time",
       fill = "") +
  theme_minimal(base_size = 14)


# Trump Tweet Sentiment Over Time
tweet_sentiment_over_time %>%
  ggplot(aes(date)) + scale_fill_brewer(palette = "PuBu") +
  geom_histogram(aes(fill = factor(sentiment)),
                 bins = 11,
                 col = "black",
                 size = .1) +  # change binwidth
  scale_x_date(labels = date_format("%m-%Y")) +
  labs(x = 'Month',
       y = 'Number of Sentiment Words',
       title = "Trump Tweet Sentiment Over Time",
       fill = "") +
  theme_minimal(base_size = 14)

sentiment_over_time$comb_sent <-
  paste(substr(sentiment_over_time$source, 1, 2),
        sentiment_over_time$sentiment,
        sep = "_")


# Combined Sentiments over time

sentiment_over_time %>%
  ggplot(aes(date)) + scale_fill_brewer(palette = "PuBu") +
  geom_histogram(aes(fill = comb_sent),
                 bins = 11,
                 col = "black",
                 size = .1) + 
  stat_bin(
    bins = 11,
    geom = "text",
    colour = "black",
    size = 2,
    aes(
      label = ifelse(..count.. > 40, ..count.., ""),
      group = comb_sent
    ),
    position = position_stack(vjust = 0.5)
  ) +
  scale_x_date(labels = date_format("%m-%Y")) +
  labs(x = '',
       y = 'Number of Sentiment Words',
       title = "Combined Sentiments Over Time",
       fill = "") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 12),
    axis.text = element_text(size = 8),
    axis.title.y = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

### Plot the percentage of positive and negative (Probability of sentiments)
x <- sentiment_over_time %>% 
  group_by(source, sentiment) %>% 
  summarise(count=n()) %>% 
  mutate(perc = count / sum(count)*100)

ggplot(x, aes(
  x = factor(source),
  y = perc,
  fill = factor(sentiment)
)) +
  geom_bar(stat = "identity", width = 0.7, position = 'dodge') +
  labs(x = "",
       y = "Percent",
       fill = "",
       title = "Sentiment by Source",
       subtitle = "Ratio of Positive and Negative") +
  scale_fill_brewer(palette = "Paired") +
  geom_text(aes(label = round(perc,0)),
            colour = "black",
            size = 2,
            position = position_dodge(width = 0.75),
            vjust = -0.3) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 12),
    plot.subtitle = element_text(size = 8),
    axis.text = element_text(size = 8),
    axis.title.y = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

# Creating a percent neutral measure
percent_neutral <-
  bind_rows(tweet_time_text, article_time_text) %>%
  left_join(BING, by = c(word = "word")) %>%
  filter(date <= '2017-12-31') %>%
  filter(nchar(word) > 2) %>%
  filter(!(word %in% undesirable_words)) %>%
  group_by(date, source, word) %>%
  count(date, source, word) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count)) %>%
  left_join(BING, by = c(word = "word"))

# Plotting number of words used over time
percent_neutral %>%
  group_by(date, source) %>%
  ggplot(aes(date)) + scale_fill_brewer(palette = "Paired") +
  geom_histogram(
    aes(fill = source),
    bins = 11,
    col = "black",
    size = .1,
    position = 'dodge'
  ) +
  scale_x_date(labels = date_format("%m-%Y")) +
  labs(x = '',
       y = 'Number of Words',
       title = "Number of Words Used Over Time",
       fill = "") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 12),
    axis.text = element_text(size = 8),
    axis.title.y = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

# Continuing percent neutral
percent_neutral %>%
  na.omit() %>%
  mutate(not_neutral = "x")

percent_neutral <- percent_neutral %>%
  na.omit() %>%
  mutate(not_neutral = "x") %>%
  count(date, source, total, not_neutral) %>%
  group_by(date) %>%
  mutate(percentage_neutral = (total - n) / total) %>%
  mutate(percentage_not_neutral = 1 - percentage_neutral) %>%
  select(date, source, percentage_neutral, percentage_not_neutral)


# Calculate aggregated data per week
monthly_percent_neutral <- percent_neutral %>%
  mutate(month = as.Date(cut(date, breaks = "month"))) %>%
  group_by(source, month) %>%
  summarise(average = mean(percentage_not_neutral)) %>%
  mutate(NYTimes = average[1:12])

trump_avg <- monthly_percent_neutral$average[13:24]

monthly_percent_neutral <- monthly_percent_neutral[1:12, ] %>%
  mutate(Trump = trump_avg*100) %>%
  mutate(NYTimes = NYTimes * 100) %>% 
  ungroup() %>%
  select(month, NYTimes, Trump)

# And make the plot
ggplot(monthly_percent_neutral, aes(x = month)) +
  geom_line(aes(y = NYTimes, colour = 'NYTimes')) +
  geom_line(aes(y = Trump, colour = 'Trump')) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "2 month") +
  labs(x = '', y = 'Percent', 
       title = "Sentiment Ratio", 
       subtitle = 'Monthly Ratio of Words With Sentiment to All Words') +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(size = 12),
    plot.subtitle = element_text(size = 8),
    axis.title.y = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.title = element_blank()
  ) +
  scale_colour_manual(values = c("Blue", "Red"))

monthly_percent_neutral$average <- NULL

days <- date(busy_days$pubDay)


# Creating a variable to plot the sentiment on the dates with most articles
busy_day_sentiment <-
  bind_rows(tweet_time_text, article_time_text) %>%
  inner_join(AFINN, by = c(word = "word")) %>%
  filter(date <= '2017-12-31') %>%
  filter(nchar(word) > 2) %>%
  filter(!(word %in% undesirable_words)) %>%
  filter((as.character(date) %in% as.character(days))) %>%
  count(date, source, word, score) %>%
  mutate(contribution = n * score) %>%
  group_by(date, source) %>%
  summarise(sentiment_sum = sum(contribution)) %>%
  ungroup() %>%
  mutate(date = reorder(date, sentiment_sum))

# Reordered number of articles for each day of publications
article_counts_for_labels <-
  c(3, NA, 3, NA, 3, NA, 3, NA, 3, NA, 4, NA)

busy_day_sentiment %>%
  ggplot(aes(x = as.factor(date), y = sentiment_sum, label = source)) +  scale_fill_brewer(palette = "Paired") +
  geom_bar(stat = 'identity', aes(fill = source)) + scale_y_continuous(limits = c(-20, 20)) +
  labs(x = '',
       y = 'Sentiment Score * Number of Occurrences',
       title = "Sentiment Score on Days with Most Articles",
       fill = "") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    legend.text = element_text(size = 8)
  ) +
  coord_flip()

# Let's see what they're about --> The Muslim travel ban and visiting Japan
articles$pubDay <- date(ymd_hms(articles$pubDay))
x <- articles %>% filter(pubDay == "2017-02-10")
y <- tweets %>% filter(created_at == "2017-02-10")
y$text[2]

# --> The withdrawal from the Paris Agreement
x <- articles %>% filter(pubDay == "2017-03-06")
y <- tweets %>% filter(created_at == "2017-03-06")
y$text[2]
x$Snippet



# Test the sentiment score correlation
sentiment_by_date <-
  bind_rows(tweet_time_text, article_time_text) %>%
  inner_join(AFINN, by = c(word = "word")) %>%
  filter(date <= '2017-12-31') %>%
  count(date, source, word, score) %>%
  mutate(contribution = n * score) %>%
  group_by(date, source) %>%
  summarise(sentiment_sum = sum(contribution)) %>%
  dcast(date ~ source)

# Correlation test
cor.test(
  x = sentiment_by_date$NYTimes,
  y = sentiment_by_date$Trump,
  method = "pearson",
  use = "complete.obs"
)

# Test of the differences of absolute word score
absolute_word_sentiment <-
  bind_rows(tweet_time_text, article_time_text) %>%
  inner_join(AFINN, by = c(word = "word")) %>%
  filter(date <= '2017-12-31') %>%
  select(date, source, score) %>%
  group_by(source) %>%
  summarise(average = mean(abs(score)))

absolute_word_sentiment  

######### ADDITIONAL TEST ########################
# But not relevant for the purpose of this study #
##### Testing a network graph of the tweets ######

library(igraph)

# define a tag extractor function
hashtags <-
  function(x)
    toupper(grep("^#", strsplit(x, " +")[[1]], value = TRUE))

# Create a list of the tag sets for each tweet
taglist <- vector(mode = "list", length(text))

# ... and populate it
for (i in 1:length(text))
  taglist[[i]] <- hashtags(text[i])
# Now make a list of all unique hashtags
alltags <- NULL
for (i in 1:length(text))
  alltags <- union(alltags, taglist[[i]])

# Create an empty graph
hash.graph <- graph.empty(directed = FALSE)
# Populate it with nodes
hash.graph <- hash.graph + vertices(alltags)
# Populate it with edges
for (tags in taglist) {
  if (length(tags) > 1) {
    for (pair in combn(
      length(tags),
      2,
      simplify = FALSE,
      FUN = function(x)
        sort(tags[x])
    )) {
      if (pair[1] != pair[2]) {
        if (hash.graph[pair[1], pair[2]] == 0)
          hash.graph <- hash.graph + edge(pair[1], pair[2])
      }
    }
  }
}

hash.graph$layout <- layout.kamada.kawai
V(hash.graph)$color <- "tomato"
E(hash.graph)$color <- "black"
V(hash.graph)$label <- V(hash.graph)$name
V(hash.graph)$label.cex = 0.5
V(hash.graph)$size <- 1
V(hash.graph)$size2 <- 0.5
V(hash.graph)$shape <- "rectangle"
plot(hash.graph)

#####################################################
