# # https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a

#
###
#####
#######
# STM analiz sonuçları ve TIK sunumu için keywords lazım: START
#####
###
#

setwd("C:/Users/ozbay/OneDrive - XXXX/R tez")
library(dplyr)
library(stm)
library(stringr)
library(RColorBrewer)
library(wordcloud2)
library(tidytext)

# Final STM Model
load("C:/Users/ozbay/OneDrive - XXXX/R_May_2023/STM_Model_May_2023.RData")
load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/M_05_Feb_2023_Environment.RData") # import the original WOS data

############################################# START
# CAUTION: The order of the data I used for the STM model is not in the same order as the WOS original data.
# There is no mistake. The content of the data is the same but the row order is different.
# To use STM functions (e.g. findThoughts), it is necessary to put the WOS original data in the same order as out.
# When I say wos original, I mean the raw data from which I will extract the original title or abstract while analyzing the topics.
# As a resut: It is necessary to put the WOS original information in the same order as "out".
# load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/M_05_Feb_2023_Environment.RData") # import the original WOS data
M <- M %>% select(UT, TI, AB) # select only necessary columns
WOS_No_of_out <- as.data.frame(names(out[["documents"]])) # extract WOS numbers from "out"
names(WOS_No_of_out) <- "UT"
M_Original_reordered <- merge(WOS_No_of_out, M, by = "UT", all.x = TRUE) # take TI anda AB date and insert in WOS_No_of_out per WOS no.
names(WOS_No_of_out) <- "UT"
rm(M, New, New_1, Old, Old_1, WOS_No_of_out, dot_replacement_correction, dot_replacements, yedek)
############################################# END




# M_Original_reordered to be created per above codes
# Aşağıdaki kod ile bir alttaki R datasını ürettim. içinde keywords var. 
# C:/Users/ozbay/OneDrive - XXXX/R_örnekler_final/M orijinal keyword çekme M_Original_reordered tik7 TIK7 tez UME wos data birleştirme.R

# Import keywords data:
load("C:/Users/ozbay/OneDrive - XXXX/WoS data Mart 2022/SIRALAMA_FARKLI_SADECE_KEYWORDS_VE_TI_bulunan_STM_ANALIZINE_GIREN_WoS_data_2005_01_Mart_2022.RData")
M_TI_and_Keywords # includes keywords


# Create DE and ID columns based on matching UT values from M_TI_and_Keywords
M_Original_reordered$DE <- M_TI_and_Keywords$DE[match(M_Original_reordered$UT, M_TI_and_Keywords$UT)]
M_Original_reordered$ID<- M_TI_and_Keywords$ID[match(M_Original_reordered$UT, M_TI_and_Keywords$UT)]

# Removal of NA values with ""
M_Original_reordered$ID <- ifelse(is.na(M_Original_reordered$ID), "", M_Original_reordered$ID)
M_Original_reordered$DE <- ifelse(is.na(M_Original_reordered$DE), "", M_Original_reordered$DE)

M_Original_reordered$ID <- tolower(M_Original_reordered$ID) # ID Keywords Plus®
M_Original_reordered$DE <- tolower(M_Original_reordered$DE) # DE Author Keywords

# Aşağıdaki findThoughts ile keyword çekiyorum
# keyword_findThoughts <- findThoughts(STM_robot, texts= M_Original_reordered$DE, n=1, topics=c(44, 30, 13, 61, 32, 24, 19, 72, 60, 80, 22, 37, 8, 40, 41, 11, 66, 54, 31, 59, 55, 91))
keyword_findThoughts <- findThoughts(STM_robot, texts= M_Original_reordered$DE, n=1000, topics=c(1:91)) # first 1000 document's keywords
# keyword_findThoughts[["docs"]][["Topic 1"]]
# keyword_findThoughts[["docs"]][["Topic 91"]]

# Extracting keywords in to a data frame:
# Create an empty data frame with appropriate columns
all_topics_df <- data.frame(Topic = character(), Text = character(), stringsAsFactors = FALSE)

# Iterate over each topic
for(topic in names(keyword_findThoughts[["docs"]])) {
  # Extract data for the current topic
  topic_data <- keyword_findThoughts[["docs"]][[topic]]
  # Append to the data frame
  all_topics_df <- rbind(all_topics_df, data.frame(Topic = topic, keywords = topic_data))
} # all_topics_df is now a data frame with all topics and their data


library(stringr)
# Remove unnecessary spaces from all_topics_df
# all_topics_df$ORIGINAL <- all_topics_df$keywords
all_topics_df$keywords <- str_squish(all_topics_df$keywords)

# Replace all occurrences of ";;" with ";"
all_topics_df$keywords <- gsub(";;", ";", all_topics_df$keywords)
all_topics_df$keywords <- gsub("; ", ";", all_topics_df$keywords)
all_topics_df$keywords <- gsub(", ", ";", all_topics_df$keywords)
all_topics_df$keywords <- gsub(",", ";", all_topics_df$keywords)
all_topics_df$keywords <- gsub(";and ", " and ", all_topics_df$keywords)
all_topics_df$keywords <- gsub("&#8217;s", ";", all_topics_df$keywords)
all_topics_df$keywords <- gsub("#8211;", ";", all_topics_df$keywords)


all_topics_df$keywords <- gsub("#8211;", ";", all_topics_df$keywords)
all_topics_df$keywords <- gsub("#8211;", ";", all_topics_df$keywords)
all_topics_df$keywords <- gsub("#8211;", ";", all_topics_df$keywords)
all_topics_df$keywords <- gsub("#8211;", ";", all_topics_df$keywords)
all_topics_df$keywords <- gsub("b&#233;zier", "beizer;", all_topics_df$keywords)
all_topics_df$keywords <- gsub("&#8217;", ";", all_topics_df$keywords)
all_topics_df$keywords <- gsub("(&#174);", ";", all_topics_df$keywords)
all_topics_df$keywords <- gsub("(&#174);", ";", all_topics_df$keywords, fixed = TRUE) # fixed = TRUE, treat the pattern as a fixed string rather than a regular expression
all_topics_df$keywords <- gsub("#", "", all_topics_df$keywords, fixed = TRUE) # fixed = TRUE, treat the pattern as a fixed string rather than a regular expression
all_topics_df$keywords <- gsub(" & ", "_", all_topics_df$keywords, fixed = TRUE) # fixed = TRUE, treat the pattern as a fixed string rather than a regular expression
all_topics_df$keywords <- gsub("&;", ";", all_topics_df$keywords, fixed = TRUE) # fixed = TRUE, treat the pattern as a fixed string rather than a regular expression
all_topics_df$keywords <- gsub(" and ", " ", all_topics_df$keywords, fixed = TRUE) # fixed = TRUE, treat the pattern as a fixed string rather than a regular expression



# Replace spaces and hyphens with underscores in Combined_DE
# all_topics_df$keywords <- str_replace_all(all_topics_df$keywords, c(" " = "_", "-" = "_"))
all_topics_df$keywords <- str_replace_all(all_topics_df$keywords, c("-" = "_"))
# all_topics_df$keywords <- gsub("_and_", " ", all_topics_df$keywords)


library(dplyr)
# Group by "Topic" and combine "DE" values with a separator
all_topics_df <- all_topics_df %>%
  group_by(Topic) %>%
  summarize(topics_keywords = paste(keywords, collapse = "OGUZOZBAY"))

all_topics_df$topics_keywords <- gsub("OGUZOZBAYOGUZOZBAY", "OGUZOZBAY", all_topics_df$topics_keywords)
all_topics_df$topics_keywords <- gsub("OGUZOZBAYOGUZOZBAY", "OGUZOZBAY", all_topics_df$topics_keywords)
all_topics_df$topics_keywords <- gsub("OGUZOZBAYOGUZOZBAY", "OGUZOZBAY", all_topics_df$topics_keywords)
all_topics_df$topics_keywords <- gsub("OGUZOZBAYOGUZOZBAY", "OGUZOZBAY", all_topics_df$topics_keywords)
all_topics_df$topics_keywords <- gsub("OGUZOZBAY", ";", all_topics_df$topics_keywords)
# remove the semicolon (;) character if it appears at the beginning or at the end of the strings
all_topics_df$topics_keywords <- sub("^;", "", sub(";$", "", all_topics_df$topics_keywords))


# Diyorum ki çok kelimeli keywordleri _ ile birleştireyim
# Sonra bu ikisini (_ ile birleşmiş ve birleşmemişi) birleştiririm ayrı bir sütunda.
# Yeni sutuna göre tokenize ederim analiz sırasında frekaslı düşük olanları atarım.
# Çünkü bazı anahtar kelimeler üç dört kelimeli.
# all_topics_df$quasi_Ngram <- all_topics_df$topics_keywords
all_topics_df$quasi_Ngram <- gsub(" ", "_", all_topics_df$topics_keywords)
all_topics_df$keywords_with_NGrams <- paste(all_topics_df$quasi_Ngram, all_topics_df$topics_keywords, sep = ";")

# remove ";" for tokenization:
all_topics_df$keywords_with_NGrams <- gsub(";", " ", all_topics_df$keywords_with_NGrams)

# KAYNAK https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a
# TOKENIZATION of keywords_with_NGrams:
library(tidytext)
TOPIC_NO <- "Topic 37"
topic_N_keywords <- all_topics_df[which(all_topics_df$Topic == TOPIC_NO), ]

topic_N_keywords <-  topic_N_keywords %>%
  select(keywords_with_NGrams) %>%
  unnest_tokens(word, keywords_with_NGrams)
topic_N_keywords <- topic_N_keywords %>% count(word, sort=TRUE)



# I will remove common stop words
# custom list of common stopwords
my_stopwords <- c("in", "on", "the", "of")
# Remove rows from topic_N_keywords where word is a common stopword
topic_N_keywords <- topic_N_keywords[!topic_N_keywords$word %in% my_stopwords, ]




library(RColorBrewer)
library(wordcloud2)

set.seed(1234)
wordcloud2(topic_N_keywords,
           size= 0.5, 
           color='random-dark', # random-light 
           gridSize =  8,
           rotateRatio = 0.4,
           shape = 'circle',
           minSize = 7,  # kelime sayısını azaltıyor
           minRotation = -pi/4, maxRotation = pi/4,
           fontFamily = 'arial', fontWeight = 'normal') #  fontFamily = 'Segoe UI / fontWeight = normal, bold or 600 ...

# "The word cloud for Topic 13 visually presents the key keywords identified from its most representative documents. These were extracted from the first 50 documents of Topic 13"


library(wordcloud)
set.seed(1234) # for reproducibility 
wordcloud(topic_N_keywords$word, freq = topic_N_keywords$n, min.freq = 1, 
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"), scale=c(2,1))








000000000000000000000000000000000000

# STM analiz sonuçları ve TIK sunumu için keywords lazım: END














