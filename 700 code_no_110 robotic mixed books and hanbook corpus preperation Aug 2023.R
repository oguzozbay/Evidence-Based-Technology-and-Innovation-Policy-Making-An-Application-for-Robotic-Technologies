# I created a new corpus in August 2023 form 107 pdf book on robotics.
# I converted pdf books to plain text format and made some text cleaning in notepad++.
# the text in not excellently converted from pdf yo plain text, additionaly becasue os formulas, equations and tables thre ara problems in text.
# How evert it is a good text on robotics.
# I will lemmatize this new corpus. How ever I make some additional text cleaning in R



library(readxl)
# robotic_books_corpus_03_aug_2023 <- read_excel("C:/Users/ozbay/OneDrive - XXXX/libgen robotic corpus libgen all united/robotic_books_corpus_03_aug_2023.xlsx")


# I create a new column. I this new column I will remove all numbers and punctuation. Than I will delete row if only one word exist in this new column.
robotic_books_corpus_03_aug_2023$text_manipulation <- robotic_books_corpus_03_aug_2023$robotic_text
colnames(robotic_books_corpus_03_aug_2023)
# [1] "robotic_text"      "text_manipulation"

# STEP-1 ----
# Remove all punctuation and special characters except for spaces.
# Remove all numbers.
# Squish all spaces (removing leading/trailing spaces and replacing multiple spaces with a single one).
# Create a new column called "word_count".
# In the "word_count" column, count the number of words in the cleaned "text_manipulation" text.

library(dplyr)
library(stringr)

# Remove all non-alphabetical and non-space characters
robotic_books_corpus_03_aug_2023 <- robotic_books_corpus_03_aug_2023 %>%
  mutate(text_manipulation = str_replace_all(text_manipulation, "[^a-zA-Z\\s]", ""))

# Squish all spaces
robotic_books_corpus_03_aug_2023 <- robotic_books_corpus_03_aug_2023 %>%
  mutate(text_manipulation = str_squish(text_manipulation))

# Create a new column "word_count"
# Count the number of words in the cleaned "text_manipulation" text
robotic_books_corpus_03_aug_2023 <- robotic_books_corpus_03_aug_2023 %>%
  mutate(word_count = str_count(text_manipulation, "\\S+"))


robotic_big_corpus <- robotic_books_corpus_03_aug_2023 %>% filter(word_count > 1)
robotic_big_corpus$text_manipulation <- NULL
robotic_big_corpus$word_count <- NULL
rm(robotic_books_corpus_03_aug_2023)

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic mixed books and hanbook corpus preperation Aug 2023_ENVIRONMENT.RData")

library(spacyr)
spacy_install()
# Language model "en_core_web_sm" is successfully installed
spacy_initialize()

system.time({
  bic_corpus_lemma <- spacy_parse(robotic_big_corpus$robotic_text , multithread = TRUE, nounphrase = FALSE, lemma= TRUE) #
}) # elapsed

spacy_finalize()

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic mixed books and hanbook corpus preperation Aug 2023_ENVIRONMENT.RData")
# load("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic mixed books and hanbook corpus preperation Aug 2023_ENVIRONMENT.RData")


big_corpus_lemma <- bic_corpus_lemma
rm(bic_corpus_lemma) # rename the data and remove old one

# The function for untidy process ----
function_for_untidy_of_lemmas <- function(df) {
  # Add a new column "row_no"
  df$row_no <- df[,1]
  # Move the "row_no" column to be the second one
  df <- df[, c(1, ncol(df), 2:(ncol(df)-1))]
  # Clean up and convert to numeric the "row_no" column
  df[,2] <- gsub(pattern= "text", replacement = "", df[,2])
  df[,2] <- as.numeric(df[,2])
    # Group by "row_no" and summarise
  untidy_df <- df %>%
    group_by(df[[2]]) %>%
    summarise(lemmatized_text = paste0(lemma, collapse = ' '))
    # Rename columns
  names(untidy_df) <- c("row_no", "lemmatized_text")
    return(untidy_df)
}

# Use the function
untidy_big_corpus_lemma <- function_for_untidy_of_lemmas(big_corpus_lemma)

# Checking the result
untidy_big_corpus_lemma$original <- robotic_big_corpus$robotic_text
untidy_big_corpus_lemma$original <- NULL # I took a look at the dataframe and quickly checked, then deleted the original column
untidy_big_corpus_lemma <- na.omit(untidy_big_corpus_lemma)
untidy_big_corpus_lemma$lemmatized_text <- tolower(untidy_big_corpus_lemma$lemmatized_text)



# Preparing final form of data
text5_big_corpus <- robotic_big_corpus
names(text5_big_corpus) <- "text"
text5_big_corpus <- as.data.frame(text5_big_corpus)

text5_big_corpus_lemma <- data.frame(untidy_big_corpus_lemma$lemmatized_text)
names(text5_big_corpus_lemma) <- "text"


text5_big_corpus <- na.omit(text5_big_corpus)
text5_big_corpus$text <- tolower(text5_big_corpus$text)

text5_big_corpus_lemma <- na.omit(text5_big_corpus_lemma)
text5_big_corpus_lemma$text <- tolower(text5_big_corpus_lemma$text)

# Delete unnecessarry data from Environment
rm(untidy_big_corpus_lemma)
rm(robotic_big_corpus)
rm(big_corpus_lemma)

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic mixed books and hanbook corpus preperation Aug 2023_ENVIRONMENT.RData")
# At his point I have two data frames:
text5_big_corpus # downloaded 107 pdf book text
text5_big_corpus_lemma # lemmatized version of downloaded 107 pdf book text
# I will use above date while preparing GloVe word embeddings. 


# I will import patenst text which is used for STM
Patents_STM_lemmatized_text <- read_excel("C:/Users/ozbay/OneDrive - XXXX/libgen robotic corpus libgen all united/Patents_STM_lemmatized_text_final_for_Glove.xlsx", sheet = "Sheet 1")
text2_patents_STM <- Patents_STM_lemmatized_text
rm(Patents_STM_lemmatized_text)
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic mixed books and hanbook corpus preperation Aug 2023_ENVIRONMENT.RData")
