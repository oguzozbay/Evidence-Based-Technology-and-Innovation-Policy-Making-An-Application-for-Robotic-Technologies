
#NOT: Nihai Corpus= 183.997 words and 32.885.9590 tokens (15/08/2023)

#> I will create word embeddings from the texts I have. 
#> For this purpose, I will import the texts into R and then lemmatize some of them 
#> while using others in their original form.


# path of excel files:
#  C:\Users\ozbay\OneDrive - XXXX\R_TIK_6_GloVe\word embedding corpus UME temmuz 2023

# Names of excel files:
#> cited_ref_titles_part_1_UME_07_temmuz_2023.xlsx
#> cited_ref_titles_part_2_UME_07_temmuz_2023.xlsx
#> corpus_of_handook_books_and_articles.xlsx"
#> M_AB_untidy_specially_prepared_for_word_embedding_UME_07_temmuz_2023.xlsx
#> read_me_word embedding için metinler UME temmuz 2023.xlsx
#> robotic_patetns_AB_and_TI_corrected_text_UME_07_temmuz_2023.xlsx
#> STM_used_text_UME_07_temmuz_2023.xlsx
#> WoS_original_AB_and_TI_UME_07_temmuz_2023.xlsx

# Definitions of excel files:
#> 	cited_ref_titles_part_1_UME_07_temmuz_2023.xlsx	=	citer refences' titles 1st part
#> 	cited_ref_titles_part_2_UME_07_temmuz_2023.xlsx	=	citer refences' titles 2nd part
#> 	corpus_of_handook_books_and_articles.xlsx	=	manually copied and pasted corpus see oguz_note_2
#> 	M_AB_untidy_specially_prepared_for_word_embedding_UME_07_temmuz_2023.xlsx	=	see oguz_note
#> 	robotic_patetns_AB_and_TI_corrected_text_UME_07_temmuz_2023.xlsx	=	patents original test with some miss-spell corrections done.
#> 	STM_used_text_UME_07_temmuz_2023.xlsx	=	full text processes completed text which is used in STM
#> 	WoS_original_AB_and_TI_UME_07_temmuz_2023.xlsx	=	original AB and TI of WOS data




# file name of the text	                                                    version to be used	
#                                                                           original	lemmatize
# cited_ref_titles_part_1_UME_07_temmuz_2023.xlsx                               x         x
# cited_ref_titles_part_2_UME_07_temmuz_2023.xlsx	                              x       	x
# corpus_of_handook_books_and_articles.xlsx	                                    x	        x
# M_AB_untidy_specially_prepared_for_word_embedding_UME_07_temmuz_2023.xlsx	    x       	-
# robotic_patetns_AB_and_TI_corrected_text_UME_07_temmuz_2023.xlsx	            x         x
# STM_used_text_UME_07_temmuz_2023.xlsx	                                        x       	-
# WoS_original_AB_and_TI_UME_07_temmuz_2023.xlsx	                              x       	x




# STEP-1 Lemmatization of cited references' titles ----
library(readxl)
cited_ref_titles_part_1 <- read_excel("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/word embedding corpus UME temmuz 2023/cited_ref_titles_part_1_UME_07_temmuz_2023.xlsx")
cited_ref_titles_part_2 <- read_excel("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/word embedding corpus UME temmuz 2023/cited_ref_titles_part_2_UME_07_temmuz_2023.xlsx")
cited_ref_titles <- rbind(cited_ref_titles_part_1, cited_ref_titles_part_2)
rm(cited_ref_titles_part_1, cited_ref_titles_part_2)


library(spacyr)
library(dplyr)
library(stringr)
library(stringi)

spacy_install()
spacy_initialize()

system.time({
cited_ref_lemma <- spacy_parse(cited_ref_titles$titles_cleaned , multithread = TRUE, nounphrase = FALSE, lemma= TRUE) #
}) # elapsed
# spacy_finalize()
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/preparing word embedding of robotic corpus UME temmuz 2023_ENVIRONMENT.RData")

colnames(cited_ref_lemma)
cited_ref_lemma$row_no <- cited_ref_lemma$doc_id
cited_ref_lemma$row_no <- gsub(pattern= "text", replacement = "", cited_ref_lemma$row_no)
cited_ref_lemma <- cited_ref_lemma %>% relocate(row_no, .before = sentence_id) # for ease of evaluation
cited_ref_lemma$row_no <- as.numeric(cited_ref_lemma$row_no)
untidy_cited_ref_lemma <- cited_ref_lemma %>% group_by(row_no) %>% summarise(lemmatized_text = paste0(lemma, collapse = ' '))
untidy_cited_ref_lemma$original <- cited_ref_titles$titles_cleaned
untidy_cited_ref_lemma$original <- NULL
untidy_cited_ref_lemma <- na.omit(untidy_cited_ref_lemma)
untidy_cited_ref_lemma$lemmatized_text <- tolower(untidy_cited_ref_lemma$lemmatized_text)


# STEP-2 Lemmatization of patent titles and abstracts
library(readxl)
robotic_patetns_AB_and_TI <- read_excel("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/word embedding corpus UME temmuz 2023/robotic_patetns_AB_and_TI_corrected_text_UME_07_temmuz_2023.xlsx", 
                                          +     sheet = "corrected_patens_final")
system.time({
robotic_patetns_TI_lemma <- spacy_parse(robotic_patetns_AB_and_TI$title_corrected, multithread = TRUE, nounphrase = FALSE, lemma= TRUE)
robotic_patetns_AB_lemma <- spacy_parse(robotic_patetns_AB_and_TI$Abstract_corrected_NEW, multithread = TRUE, nounphrase = FALSE, lemma= TRUE)
}) # elapsed
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/preparing word embedding of robotic corpus UME temmuz 2023_ENVIRONMENT.RData")

robotic_patetns_TI_lemma$row_no <- robotic_patetns_TI_lemma[,1]
robotic_patetns_TI_lemma <- robotic_patetns_TI_lemma[, c(1, ncol(robotic_patetns_TI_lemma), 2:(ncol(robotic_patetns_TI_lemma)-1))] # moving last column after 1st column
robotic_patetns_TI_lemma[,2] <- gsub(pattern= "text", replacement = "", robotic_patetns_TI_lemma[,2])
robotic_patetns_TI_lemma[,2] <- as.numeric(robotic_patetns_TI_lemma[,2])

# STEP-2.1 Untidy of TI lemmas ----
column_index <- 2 # column index for grouping
# Group by and summarise
untidy_robotic_patetns_TI_lemma <- robotic_patetns_TI_lemma %>%
  group_by(robotic_patetns_TI_lemma[[column_index]]) %>% 
  summarise(lemmatized_text = paste0(lemma, collapse = ' '))
names(untidy_robotic_patetns_TI_lemma) <- c("row_no", "lemmatized_text")
# Check the result
untidy_robotic_patetns_TI_lemma$original <- robotic_patetns_AB_and_TI$title_corrected
untidy_robotic_patetns_TI_lemma$original <- NULL
untidy_robotic_patetns_TI_lemma <- na.omit(untidy_robotic_patetns_TI_lemma)
untidy_robotic_patetns_TI_lemma$lemmatized_text <- tolower(untidy_robotic_patetns_TI_lemma$lemmatized_text)

# STEP-2.2 Untidy of AB lemmas ----
# I decided to create a function to untidy lemmas.
# The function for untidy process
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
untidy_robotic_patetns_AB_lemma <- function_for_untidy_of_lemmas(robotic_patetns_AB_lemma)
# Checking the result
untidy_robotic_patetns_AB_lemma$original <- robotic_patetns_AB_and_TI$Abstract_corrected_NEW
untidy_robotic_patetns_AB_lemma$original <- NULL
untidy_robotic_patetns_AB_lemma <- na.omit(untidy_robotic_patetns_AB_lemma)
untidy_robotic_patetns_AB_lemma$lemmatized_text <- tolower(untidy_robotic_patetns_AB_lemma$lemmatized_text)


# STEP-3 Lemmatization of corpus_of_handook_books_and_articles
# NOTE: I did not lowercase the "corpus_of_handook_books_and_articles" before running this code (why? Because I forget).

library(readxl)
corpus_of_handook_books_and_articles <- read_excel("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/word embedding corpus UME temmuz 2023/corpus_of_handook_books_and_articles.xlsx", 
                                                   +     sheet = "robotic_books")

system.time({
  corpus_of_handook_books_and_articles_lemma <- spacy_parse(corpus_of_handook_books_and_articles$robot_books_articles_text , multithread = TRUE, nounphrase = FALSE, lemma= TRUE) #
}) # elapsed

# Use the function for untidy
untidy_corpus_of_handook_books_and_articles_lemma <- function_for_untidy_of_lemmas(corpus_of_handook_books_and_articles_lemma)
# Check the result
untidy_corpus_of_handook_books_and_articles_lemma$original <- corpus_of_handook_books_and_articles$robot_books_articles_text
untidy_corpus_of_handook_books_and_articles_lemma$original <- NULL
untidy_corpus_of_handook_books_and_articles_lemma <- na.omit(untidy_corpus_of_handook_books_and_articles_lemma)
untidy_corpus_of_handook_books_and_articles_lemma$lemmatized_text <- tolower(untidy_corpus_of_handook_books_and_articles_lemma$lemmatized_text)
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/preparing word embedding of robotic corpus UME temmuz 2023_ENVIRONMENT.RData")


# STEP-4.1 Lemmatization of WOS original AB and TI
library(readxl)
WoS_original_AB_and_TI <- read_excel("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/word embedding corpus UME temmuz 2023/WoS_original_AB_and_TI_UME_07_temmuz_2023.xlsx")
system.time({
  WoS_original_TI_lemma <- spacy_parse(WoS_original_AB_and_TI$TI, multithread = TRUE, nounphrase = FALSE, lemma= TRUE)
  WoS_original_AB_lemma <- spacy_parse(WoS_original_AB_and_TI$AB, multithread = TRUE, nounphrase = FALSE, lemma= TRUE)
}) # elapsed 3921.52 
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/preparing word embedding of robotic corpus UME temmuz 2023_ENVIRONMENT.RData")

# 24 temmuz 2023
# load("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/preparing word embedding of robotic corpus UME temmuz 2023_ENVIRONMENT.RData")


# STEP-4.2.1 
# Use the function for untidy WOS original TI
untidy_WoS_original_TI_lemma <- function_for_untidy_of_lemmas(WoS_original_TI_lemma)
# Check the result
untidy_WoS_original_TI_lemma$original <- WoS_original_AB_and_TI$TI
untidy_WoS_original_TI_lemma$original <- NULL
untidy_WoS_original_TI_lemma <- na.omit(untidy_WoS_original_TI_lemma)
untidy_WoS_original_TI_lemma$lemmatized_text <- tolower(untidy_WoS_original_TI_lemma$lemmatized_text)

# STEP-4.2.2
# Use the function for untidy WOS original AB
untidy_WoS_original_AB_lemma <- function_for_untidy_of_lemmas(WoS_original_AB_lemma)

untidy_WoS_original_AB_lemma$original <- WoS_original_AB_and_TI$AB
untidy_WoS_original_AB_lemma <- na.omit(untidy_WoS_original_AB_lemma)
untidy_WoS_original_AB_lemma$lemmatized_text <- tolower(untidy_WoS_original_AB_lemma$lemmatized_text)
untidy_WoS_original_AB_lemma$original <- NULL


# Summary up to now (created texts:
# step 1
untidy_cited_ref_lemma
# step 2
untidy_robotic_patetns_TI_lemma
untidy_robotic_patetns_AB_lemma
# step 3
untidy_corpus_of_handook_books_and_articles_lemma
# step 4
untidy_WoS_original_TI_lemma
untidy_WoS_original_AB_lemma

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/preparing word embedding of robotic corpus UME temmuz 2023_ENVIRONMENT.RData")
# At this point I got or already have necessary texts except for STM patent text. I am still working for STM of patents.
# I deleted all spacyr data in the environment

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/preparing word embedding of robotic corpus UME temmuz 2023_ENVIRONMENT_STEP2.RData")

# Now I will paste the second columns beneath the first columns.

# WoS data - Original
text1_WOS_Original <- data.frame(c(WoS_original_AB_and_TI$TI, WoS_original_AB_and_TI$AB))
names(text1_WOS_Original) <- "text"
text1_WOS_Original <- na.omit(text1_WOS_Original)
text1_WOS_Original$text <- tolower(text1_WOS_Original$text)

text1_WOS_Original_lemma <- data.frame(c(untidy_WoS_original_TI_lemma$lemmatized_text, untidy_WoS_original_AB_lemma$lemmatized_text))
names(text1_WOS_Original_lemma) <- "text"
text1_WOS_Original_lemma <- na.omit(text1_WOS_Original_lemma)

text1_WOS_specially_prepared_for_word_embedding <- read_excel("word embedding corpus UME temmuz 2023/M_AB_untidy_specially_prepared_for_word_embedding_UME_07_temmuz_2023.xlsx")
names(text1_WOS_specially_prepared_for_word_embedding) <- "text"
text1_WOS_specially_prepared_for_word_embedding <- na.omit(text1_WOS_specially_prepared_for_word_embedding)
text1_WOS_specially_prepared_for_word_embedding <- as.data.frame(text1_WOS_specially_prepared_for_word_embedding)

text1_WOS_STM_text <- read_excel("word embedding corpus UME temmuz 2023/STM_used_text_UME_07_temmuz_2023.xlsx")
names(text1_WOS_STM_text) <- "text"
text1_WOS_STM_text <- na.omit(text1_WOS_STM_text)
text1_WOS_STM_text <- as.data.frame(text1_WOS_STM_text)

sil_for_check <- data.frame(text1_WOS_Original[1], text1_WOS_Original_lemma[1])
rm(sil_for_check)

# Patents - Original
text2_patents_original <- data.frame(c(robotic_patetns_AB_and_TI$title_corrected, robotic_patetns_AB_and_TI$Abstract_corrected_NEW))
names(text2_patents_original) <- "text"
text2_patents_original <- na.omit(text2_patents_original)
text2_patents_original$text <- tolower(text2_patents_original$text)

text2_patents_original_lemma <- data.frame(c(untidy_robotic_patetns_TI_lemma$lemmatized_text, untidy_robotic_patetns_AB_lemma$lemmatized_text))
names(text2_patents_original_lemma) <- "text"
text2_patents_original_lemma <- na.omit(text2_patents_original_lemma)

sil_2_for_check <- data.frame(text2_patents_original[1], text2_patents_original_lemma[1]) # do this before "na.omit()"
rm(sil_2_for_check)


# Cited Ref titles
text3_cited_ref_titles <- as.data.frame(cited_ref_titles)
names(text3_cited_ref_titles) <- "text"
text3_cited_ref_titles <- na.omit(text3_cited_ref_titles)
text3_cited_ref_titles$text <- tolower(text3_cited_ref_titles$text)

text3_cited_ref_titles_lemma <- data.frame(untidy_cited_ref_lemma$lemmatized_text)
names(text3_cited_ref_titles_lemma) <- "text"
text3_cited_ref_titles_lemma <- na.omit(text3_cited_ref_titles_lemma)

sil_for_check <- data.frame(cited_ref_titles[1], untidy_cited_ref_lemma$lemmatized_text) # do this before "na.omit()"
rm(sil_for_check)

# Handbooks
text4_handbooks <- as.data.frame(corpus_of_handook_books_and_articles)
names(text4_handbooks) <- "text"
text4_handbooks <- na.omit(text4_handbooks)
text4_handbooks$text <- tolower(text4_handbooks$text)

text4_handbooks_lemma <- data.frame(untidy_corpus_of_handook_books_and_articles_lemma$lemmatized_text)
names(text4_handbooks_lemma) <- "text"
text4_handbooks_lemma <- na.omit(text4_handbooks_lemma)

sil_for_check <- data.frame(corpus_of_handook_books_and_articles[1], untidy_corpus_of_handook_books_and_articles_lemma$lemmatized_text) # do this before "na.omit()"
rm(sil_for_check)

save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/preparing word embedding of robotic corpus UME temmuz 2023_ENVIRONMENT_STEP2.RData")

# I imported all copus
load("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/preparing word embedding of robotic corpus UME temmuz 2023_ENVIRONMENT_STEP2.RData")
load("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic mixed books and hanbook corpus preperation Aug 2023_ENVIRONMENT.RData")



# At this point necessary texts are ready.
# I will unite all text.
objects() # show everything in environment

text1_WOS_Original # as downloaded from WOS (misspell corrections done)
text1_WOS_Original_lemma # lemmatized version of text1_WOS_Original
text1_WOS_specially_prepared_for_word_embedding
text1_WOS_STM_text # the text used in Structural topic model

text2_patents_original # as downloaded from patentscope (misspell corrections done)
text2_patents_original_lemma # lemmatized version of text2_patents_original
text2_patents_STM <- as.data.frame(text2_patents_STM)  # the text used in Structural topic model of Patents
names(text2_patents_STM) <- "text"

text3_cited_ref_titles # as downloaded from WOS (misspell corrections done)
text3_cited_ref_titles_lemma # lemmatized version of text3_cited_ref_titles

text4_handbooks # all texts converted from original pdf to txt with some loses. 
text4_handbooks_lemma # lemmatized version of text4_handbooks

text5_big_corpus # original text extracted from a totla of 107 pdf robotic books and hanbooks
text5_big_corpus_lemma # lemmatized version of text5_big_corpus

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/preparing word embedding of robotic corpus UME temmuz 2023_ENVIRONMENT_STEP2.RData")

# I will unite all texts
My_Robotics_Corpus <- data.frame(united_text = 
                                   rbind(text1_WOS_Original[1],
                                         text1_WOS_Original_lemma[1],
                                         text1_WOS_specially_prepared_for_word_embedding[1],
                                         text1_WOS_STM_text[1],
                                         text2_patents_original[1],
                                         text2_patents_original_lemma[1],
                                         text2_patents_STM[1], 
                                         text3_cited_ref_titles[1],
                                         text3_cited_ref_titles_lemma[1],
                                         text4_handbooks[1], 
                                         text4_handbooks_lemma[1], 
                                         text5_big_corpus[1], 
                                         text5_big_corpus_lemma[1]))


# I delete all except My_Robotics_Corpus
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic_big_corpus_final_aug_2023.RData")
load("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic_big_corpus_final_aug_2023.RData")

# NOW:
# 1. I will delete the punctuation marks.
# 2. I will delete the redundant spaces (white space)
# 3. I will delete row consisting of only one word

My_Robotics_Corpus$text <- tolower(My_Robotics_Corpus$text)

# STEP-1 Punctuation and Character cleaning ----
# Remove all non-alphanumeric characters except underscore (_) and space
My_Robotics_Corpus$text <- gsub(pattern= "[^[:alnum:]_[:space:]]+",  replacement = " ", My_Robotics_Corpus$text)
My_Robotics_Corpus$text <- My_Robotics_Corpus$text %>% stringr::str_squish()

# Remove digits except for the ones touching a letter
My_Robotics_Corpus$text <- gsub(pattern= "\\b[[:digit:]]+\\b",  replacement = " ", My_Robotics_Corpus$text) # removal of whole digits (e.g. A321 will stay)
#  \\b[[:digit:]]+\\b matches one or more digit characters ([[:digit:]]+) that form a complete word (\\b...\\b).
My_Robotics_Corpus$text <- My_Robotics_Corpus$text %>% stringr::str_squish()

# Convert text to latin-ASCII
My_Robotics_Corpus$text  <- stri_trans_general(My_Robotics_Corpus$text, "Latin-ASCII")
gc()

# I will delete rows that have only one word
library(stringr)
My_Robotics_Corpus <- My_Robotics_Corpus %>% mutate(n_words = str_count(text, "\\w+")) # count words
My_Robotics_Corpus <- My_Robotics_Corpus %>%   filter(n_words >= 2) # delete less than 2 words
gc()

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic_big_corpus_final_aug_2023.RData") # 12/08/2023

# I will extract unique characters
# Define a function to split the data frame into n roughly equal chunks
split_dataframe <- function(df, n = 10) {
  chunk_size <- ceiling(nrow(df) / n)
  split(df, ceiling(seq_along(df$text) / chunk_size))
}

# Split the data frame into 10 chunks
df_chunks <- split_dataframe(My_Robotics_Corpus, 10)

# Function to extract unique characters from a chunk of texts
get_unique_chars <- function(chunk) {
  concatenated_text <- paste0(chunk$text, collapse = "")
  unique(unlist(strsplit(concatenated_text, NULL)))
}
# Extract unique characters from each chunk
unique_chars_chunks <- lapply(df_chunks, get_unique_chars)
# Combine the unique characters from all chunks and get the final unique characters
unique_chars_of_corpus <- unique(unlist(unique_chars_chunks))
print(unique_chars_of_corpus)

rm(df_chunks)
rm(unique_chars_chunks)

# I will remove characters except for Latin and Greek (because of alfa, beta etc.)
# Function to remove the specified characters
remove_specific_chars <- function(text) {
  # Remove Arabic characters
  text <- gsub("[\u0600-\u06FF]", "", text)
  # Remove Chinese characters (Han ideographs)
  text <- gsub("[\u4E00-\u9FFF]", "", text)
  # Remove Korean characters (Hangul Syllables)
  text <- gsub("[\uAC00-\uD7AF]", "", text)
  # Remove Miscellaneous/Uncommon Latin Characters. 
  text <- gsub("[ȣƪƛȝʒ]", "", text)
  # Remove Arabic Presentation Forms-B characters
  text <- gsub("[\uFE70-\uFEFF]", "", text)
  # Remove Korean characters (Hangul Syllables)
  text <- gsub("[\uAC00-\uD7AF]", "", text)
  # Remove Ge'ez (Ethiopic) Script
  text <- gsub("[\u1200-\u137F]", "", text)
  # Remove Sinhalese (Sinhala) Script
  text <- gsub("[\u0D80-\u0DFF]", "", text)
  # Remove Cyrillic Script
  text <- gsub("[\u0400-\u04FF]", "", text)
  # Remove Hebrew characters
  text <- gsub("[\u05D0-\u05FF]", "", text)
  # Remove Syriac characters
  text <- gsub("[\u0700-\u074F]", "", text)
  # Remove Tamil numerals
  text <- gsub("[\u0BE6-\u0BFF]", "", text)
  # Remove IPA characters. Note: IPA has characters scattered in multiple Unicode ranges. 
  # The range given here removes some of the most common ones.
  text <- gsub("[\u0250-\u02AF]", "", text)
  # Remove Click consonants (These are specific characters and not a range)
  text <- gsub("[ǂǃ]", "", text)
  # Remove Miscellaneous/Uncommon Latin Characters. 
  text <- gsub("[ȣƪƛȝʒ^ˇº]", "", text)
  return(text)
} 

# Apply the function
system.time({
  My_Robotics_Corpus$text <- remove_specific_chars(My_Robotics_Corpus$text)
}) # # elapsed  


# I will extract unique characters again
# Split the data frame into 10 chunks
df_chunks <- split_dataframe(My_Robotics_Corpus, 10)
# Extract unique characters from each chunk
unique_chars_chunks <- lapply(df_chunks, get_unique_chars)
# Combine the unique characters from all chunks and get the final unique characters
unique_chars_of_corpus_step_2 <- unique(unlist(unique_chars_chunks))
print(unique_chars_of_corpus_step_2)
rm(df_chunks)
rm(unique_chars_chunks)

My_Robotics_Corpus$text <- tolower(My_Robotics_Corpus$text)
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic_big_corpus_final_aug_2023.RData") # 12/08/2023

# Now I will try to extract some meaningless word sequences.
# I will extract and delete some of the meaningless word sequences. For example: "k lh k l m k l"
# I will write a code which will look for n adjacent words that have less than m characters in total, any of word will no more than 2 characters.
# Then I will look the result
# Function for extracitin word sequences
extract_sequences <- function(text, n, m) {
  words <- unlist(str_split(text, " "))
  total_words <- length(words)
  # If there are fewer words than n in the sequence, return NULL immediately
  if (total_words < n) {
    return(NULL)
  }
  # Loop through each potential sequence
  for (i in 1:(total_words - n + 1)) {
    potential_sequence <- words[i:(i + n - 1)]
    # If any word in the potential sequence exceeds 2 characters, skip
    if (any(nchar(potential_sequence) > 2)) {
      next
    }
    # If the character count of the potential sequence (excluding spaces) is less than m, return the sequence
    sequence <- paste(potential_sequence, collapse = " ")
    if (nchar(gsub("\\s+", "", sequence)) < m) {
      return(sequence)
    }
  }
  return(NULL)
}

# Filter rows that have at least n words
filtered_data <- My_Robotics_Corpus[My_Robotics_Corpus$n_words >= 7, ]
# Extract the first 7-word sequence with less than 10 characters from each filtered row
system.time({
  sequences <- sapply(filtered_data$text, extract_sequences, n = 7, m = 10)
}) # elapsed 754.44 
sequences <- sequences[sapply(sequences, function(x) !is.null(x))]
sequences_of_7_words <- data.frame("word_sequences" = as.character(sequences), stringsAsFactors = FALSE)
sequences_of_7_words  <- sequences_of_7_words %>%
  group_by(word_sequences) %>%
  summarise(number = n()) %>% 
  arrange(desc(number)) # Group and count
rm(sequences)

# I will correct some 7 word sequences (it took almost 5 minute for each each!)
My_Robotics_Corpus$text <- gsub(pattern= "\\bd e c l a r a t i o n s\\b",  replacement = "declarations", My_Robotics_Corpus$text)
My_Robotics_Corpus$text <- gsub(pattern= "\\bd e f i n e\\b",  replacement = "define", My_Robotics_Corpus$text)
My_Robotics_Corpus$text <- gsub(pattern= "\\bd e c l a r a t i o n\\b",  replacement = "declaration", My_Robotics_Corpus$text)
My_Robotics_Corpus$text <- gsub(pattern= "\\bf l o a t c h a r\\b",  replacement = "floatchar", My_Robotics_Corpus$text)
My_Robotics_Corpus$text <- gsub(pattern= "\\bc o n t r o l\\b",  replacement = "control", My_Robotics_Corpus$text)
My_Robotics_Corpus$text <- gsub(pattern= "\\bo r i e n t a t i o n\\b",  replacement = "orientation", My_Robotics_Corpus$text)
My_Robotics_Corpus$text <- gsub(pattern= "\\bb u f f e r s i z e\\b",  replacement = "buffer size", My_Robotics_Corpus$text)
My_Robotics_Corpus$text <- gsub(pattern= "\\bl i k e r t s i z e\\b",  replacement = "likert size", My_Robotics_Corpus$text)
My_Robotics_Corpus$text <- str_replace_all(My_Robotics_Corpus$text, "\\bd o f\\b", "dof")

# I will look for 6 word sequences
filtered_data <- My_Robotics_Corpus[My_Robotics_Corpus$n_words >= 6, ] # Filter rows that have at least n words
# Extract the first 6-word sequence with less than 8 characters from each filtered row
system.time({
  sequences <- sapply(filtered_data$text, extract_sequences, n = 6, m = 8)
}) # elapsed
sequences <- sequences[sapply(sequences, function(x) !is.null(x))]
sequences_of_6_words <- data.frame("word_sequences" = as.character(sequences), stringsAsFactors = FALSE)
sequences_of_6_words  <- sequences_of_6_words %>%
  group_by(word_sequences) %>%
  summarise(number = n()) %>% 
  arrange(desc(number)) # Group and count
rm(sequences)
rm(filtered_data)

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic_big_corpus_final_aug_2023.RData") # 12/08/2023

# I will look for 5 word sequences
filtered_data <- My_Robotics_Corpus[My_Robotics_Corpus$n_words >= 5, ] # Filter rows that have at least n words
# Extract the first 6-word sequence with less than 8 characters from each filtered row
system.time({
  sequences <- sapply(filtered_data$text, extract_sequences, n = 5, m = 6)
}) # elapsed 780
sequences <- sequences[sapply(sequences, function(x) !is.null(x))]
sequences_of_5_words <- data.frame("word_sequences" = as.character(sequences), stringsAsFactors = FALSE)
sequences_of_5_words  <- sequences_of_6_words %>%
  group_by(word_sequences) %>%
  summarise(number = n()) %>% 
  arrange(desc(number)) # Group and count
rm(sequences)
rm(filtered_data)


My_Robotics_Corpus$text <- gsub(pattern= "\\bs e r i a l\\b",  replacement = "serial", My_Robotics_Corpus$text)
My_Robotics_Corpus$text <- gsub(pattern= "\\ba t t i t u d e\\b",  replacement = "attitude", My_Robotics_Corpus$text)
My_Robotics_Corpus$text <- gsub(pattern= "\\bp o r t\\b",  replacement = "port", My_Robotics_Corpus$text)
My_Robotics_Corpus$text <- gsub(pattern= "\\br e n a l\\b",  replacement = "renal", My_Robotics_Corpus$text)

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic_big_corpus_final_aug_2023.RData") # 12/08/2023

# It is possible to see word sequences with above code.
# Now I will delete those word sequences
# However find and replace method is extemmly time conuming for my huge data
# Function for replacing word sequences to "OGUZOZBAYDELETE" ----
replace_sequences <- function(text, n, m) {
  words <- unlist(str_split(text, " "))
  total_words <- length(words)
  modified_text <- text
  # If there are fewer words than n in the sequence, return the original text
  if (total_words < n) {
    return(text)
  }
  for (i in 1:(total_words - n + 1)) {
    potential_sequence <- words[i:(i + n - 1)]
    # If any word in the potential sequence exceeds 2 characters, skip
    if (any(nchar(potential_sequence) > 2)) {
      next
    }
    # If the character count of the potential sequence (excluding spaces) is less than m
    sequence <- paste(potential_sequence, collapse = " ")
    if (nchar(gsub("\\s+", "", sequence)) < m) {
      # Replace the sequence in the text with the placeholder
      modified_text <- gsub(pattern = paste0("\\b", sequence, "\\b"), 
                            replacement = "OGUZOZBAYDELETE", 
                            x = modified_text)
    }
  }
  return(modified_text)
}

# replacement of word sequences of 10 words
system.time({
  My_Robotics_Corpus$text_OGUZOZBAYDELETE <- sapply(My_Robotics_Corpus$text, replace_sequences, n = 10, m = 15)
}) # elapsed 750

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic_big_corpus_final_aug_2023.RData") # 14/08/2023
My_Robotics_Corpus$text_OGUZOZBAYDELETE <- My_Robotics_Corpus$text_OGUZOZBAYDELETE %>% stringr::str_squish()

# to see the replacements
replacement_1 <- My_Robotics_Corpus %>% select(text,text_OGUZOZBAYDELETE)
replacement_1 <- replacement_1 %>% mutate(is_equal = text == text_OGUZOZBAYDELETE)
replacement_1 <- replacement_1 %>% mutate(is_equal = text == text_OGUZOZBAYDELETE)
replacement_1 <- replacement_1 %>% filter(is_equal == FALSE) # to see the replacements
gc()

# removal of "OGUZOZBAYDELETE"
My_Robotics_Corpus$text_OGUZOZBAYDELETE <- gsub(pattern= "OGUZOZBAYDELETE",  replacement = " ", My_Robotics_Corpus$text_OGUZOZBAYDELETE)
My_Robotics_Corpus$text_OGUZOZBAYDELETE <- My_Robotics_Corpus$text_OGUZOZBAYDELETE %>% stringr::str_squish()

# Replacement of word sequences of 9 words
system.time({
  My_Robotics_Corpus$text_OGUZOZBAYDELETE <- sapply(My_Robotics_Corpus$text_OGUZOZBAYDELETE, replace_sequences, n = 9, m = 13)
}) # elapsed

# to see the replacements
replacement_2 <- My_Robotics_Corpus %>% select(text,text_OGUZOZBAYDELETE)
replacement_2 <- replacement_2 %>% mutate(is_equal = text == text_OGUZOZBAYDELETE)
replacement_2 <- replacement_2 %>% mutate(is_equal = text == text_OGUZOZBAYDELETE)
replacement_2 <- replacement_2 %>% filter(is_equal == FALSE) # to see the replacements

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic_big_corpus_final_aug_2023.RData") # 14/08/2023

# removal of "OGUZOZBAYDELETE"
My_Robotics_Corpus$text_OGUZOZBAYDELETE <- gsub(pattern= "OGUZOZBAYDELETE",  replacement = " ", My_Robotics_Corpus$text_OGUZOZBAYDELETE)
My_Robotics_Corpus$text_OGUZOZBAYDELETE <- My_Robotics_Corpus$text_OGUZOZBAYDELETE %>% stringr::str_squish()

My_Robotics_Corpus$n_words <- NULL
My_Robotics_Corpus$ratio <- NULL
My_Robotics_Corpus <- My_Robotics_Corpus %>% mutate(n_words = str_count(text_OGUZOZBAYDELETE, "\\w+")) # count words
# I will delete rows that have only one word
My_Robotics_Corpus <- My_Robotics_Corpus %>% filter(n_words >= 2) # delete less than 2 words

# replacement of sequences of 8 words
system.time({
  My_Robotics_Corpus$text_OGUZOZBAYDELETE <- sapply(My_Robotics_Corpus$text_OGUZOZBAYDELETE, replace_sequences, n = 8, m = 12)
}) # elapsed

# to see the replacements
replacement_3 <- My_Robotics_Corpus %>% select(text,text_OGUZOZBAYDELETE)
replacement_3 <- replacement_3 %>% mutate(is_equal = text == text_OGUZOZBAYDELETE)
replacement_3 <- replacement_3 %>% mutate(is_equal = text == text_OGUZOZBAYDELETE)
replacement_3 <- replacement_3 %>% filter(is_equal == FALSE) # to see the replacements

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic_big_corpus_final_aug_2023.RData") # 14/08/2023

# removal of "OGUZOZBAYDELETE"
My_Robotics_Corpus$text_OGUZOZBAYDELETE <- gsub(pattern= "OGUZOZBAYDELETE",  replacement = " ", My_Robotics_Corpus$text_OGUZOZBAYDELETE)
My_Robotics_Corpus$text_OGUZOZBAYDELETE <- My_Robotics_Corpus$text_OGUZOZBAYDELETE %>% stringr::str_squish()

# replacement of sequences of 7 words
system.time({
  My_Robotics_Corpus$text_OGUZOZBAYDELETE <- sapply(My_Robotics_Corpus$text_OGUZOZBAYDELETE, replace_sequences, n = 7, m = 10)
}) # elapsed

# to see the replacements
replacement_4 <- My_Robotics_Corpus %>% select(text,text_OGUZOZBAYDELETE)
replacement_4 <- replacement_4 %>% mutate(is_equal = text == text_OGUZOZBAYDELETE)
replacement_4 <- replacement_4 %>% mutate(is_equal = text == text_OGUZOZBAYDELETE)
replacement_4 <- replacement_4 %>% filter(is_equal == FALSE) # to see the replacements

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic_big_corpus_final_aug_2023.RData") # 14/08/2023

My_Robotics_Corpus$text_OGUZOZBAYDELETE <- gsub(pattern= "OGUZOZBAYDELETE",  replacement = " ", My_Robotics_Corpus$text_OGUZOZBAYDELETE)
My_Robotics_Corpus$text_OGUZOZBAYDELETE <- My_Robotics_Corpus$text_OGUZOZBAYDELETE %>% stringr::str_squish()

# replacement of sequences of 6 words
system.time({
  My_Robotics_Corpus$text_OGUZOZBAYDELETE <- sapply(My_Robotics_Corpus$text_OGUZOZBAYDELETE, replace_sequences, n = 6, m = 9)
}) # elapsed 711

# load("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic_big_corpus_final_aug_2023.RData")

# to see the replacements
replacement_5 <- My_Robotics_Corpus %>% select(text,text_OGUZOZBAYDELETE)
replacement_5 <- replacement_5 %>% mutate(is_equal = text == text_OGUZOZBAYDELETE)
replacement_5 <- replacement_5 %>% mutate(is_equal = text == text_OGUZOZBAYDELETE)
replacement_5 <- replacement_5 %>% filter(is_equal == FALSE) # to see the replacements

My_Robotics_Corpus$text_OGUZOZBAYDELETE <- gsub(pattern= "OGUZOZBAYDELETE",  replacement = " ", My_Robotics_Corpus$text_OGUZOZBAYDELETE)
My_Robotics_Corpus$text_OGUZOZBAYDELETE <- My_Robotics_Corpus$text_OGUZOZBAYDELETE %>% stringr::str_squish()

# replacement of sequences of 5 words
system.time({
  My_Robotics_Corpus$text_OGUZOZBAYDELETE <- sapply(My_Robotics_Corpus$text_OGUZOZBAYDELETE, replace_sequences, n = 5, m = 6) # I reduced m from 7 to 6
}) # elapsed 770

# to see the replacements
replacement_6 <- My_Robotics_Corpus %>% select(text,text_OGUZOZBAYDELETE)
replacement_6 <- replacement_6 %>% mutate(is_equal = text == text_OGUZOZBAYDELETE)
replacement_6 <- replacement_6 %>% mutate(is_equal = text == text_OGUZOZBAYDELETE)
replacement_6 <- replacement_6 %>% filter(is_equal == FALSE) # to see the replacements
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic_big_corpus_final_aug_2023.RData") # 14/08/2023

My_Robotics_Corpus$text_OGUZOZBAYDELETE <- gsub(pattern= "OGUZOZBAYDELETE",  replacement = " ", My_Robotics_Corpus$text_OGUZOZBAYDELETE)
My_Robotics_Corpus$text_OGUZOZBAYDELETE <- My_Robotics_Corpus$text_OGUZOZBAYDELETE %>% stringr::str_squish()

# replacement of sequences of 4 words
system.time({
  My_Robotics_Corpus$text_OGUZOZBAYDELETE <- sapply(My_Robotics_Corpus$text_OGUZOZBAYDELETE, replace_sequences, n = 4, m = 5) # I reduced m from 6 to 5
}) # elapsed 770

# to see the replacements
replacement_7 <- My_Robotics_Corpus %>% select(text,text_OGUZOZBAYDELETE)
replacement_7 <- replacement_7 %>% mutate(is_equal = text == text_OGUZOZBAYDELETE)
replacement_7 <- replacement_7 %>% mutate(is_equal = text == text_OGUZOZBAYDELETE)
replacement_7 <- replacement_7 %>% filter(is_equal == FALSE) # to see the replacements
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic_big_corpus_final_aug_2023.RData") # 14/08/2023

My_Robotics_Corpus$n_words <- NULL
My_Robotics_Corpus$ratio <- NULL
My_Robotics_Corpus <- My_Robotics_Corpus %>% mutate(n_words = str_count(text_OGUZOZBAYDELETE, "\\w+")) # count words
# I will delete rows that have only one word
My_Robotics_Corpus <- My_Robotics_Corpus %>% filter(n_words >= 2) # delete less than 2 words

My_Robotics_Corpus$text_OGUZOZBAYDELETE <- gsub(pattern= "OGUZOZBAYDELETE",  replacement = " ", My_Robotics_Corpus$text_OGUZOZBAYDELETE)
My_Robotics_Corpus$text_OGUZOZBAYDELETE <- My_Robotics_Corpus$text_OGUZOZBAYDELETE %>% stringr::str_squish()


My_Robotics_Corpus$n_words <- NULL
My_Robotics_Corpus$ratio <- NULL
My_Robotics_Corpus <- My_Robotics_Corpus %>% mutate(n_words = str_count(text, "\\w+")) # count words
# I will delete rows that have only one word
My_Robotics_Corpus <- My_Robotics_Corpus %>%   filter(n_words >= 2) # delete less than 2 words

save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic_big_corpus_final_aug_2023.RData") # 14/08/2023

My_Robotics_Corpus$text <- My_Robotics_Corpus$text_OGUZOZBAYDELETE
My_Robotics_Corpus$text_OGUZOZBAYDELETE <- NULL

# Now I will look the ratio of number of characters to number of words.
My_Robotics_Corpus <- My_Robotics_Corpus %>% mutate(n_words = str_count(text, "\\w+")) # count words
My_Robotics_Corpus <- My_Robotics_Corpus %>%   filter(n_words >= 2) # delete less than 2 words

My_Robotics_Corpus <- My_Robotics_Corpus %>% mutate(n_char = nchar(text), ratio = n_char / n_words) # create n_char and ratio
# I will manually find a good ratio.
# I sorted My_Robotics_Corpus$ratio and 3,34 seem good
# Now I will extrat ratio < 3,34 and look if ti is OK
check_ratio_3 <- My_Robotics_Corpus %>% filter (ratio < 3)
# keep row if ratio is >=3
My_Robotics_Corpus <- My_Robotics_Corpus %>% filter (ratio >= 3)
# I will delete  row including et al and consist of 5 words
# Add the et_al column initialized with zeros
My_Robotics_Corpus$et_al <- 0
# Update rows where n_words < 6 and 'et al' is found in the text
My_Robotics_Corpus$et_al[My_Robotics_Corpus$n_words < 6 & 
                           grepl("\\bet al\\b", My_Robotics_Corpus$text, ignore.case = TRUE)] <- 1
My_Robotics_Corpus <- My_Robotics_Corpus %>% filter (et_al == 0)

check_ratio_4 <- My_Robotics_Corpus %>% filter (ratio < 4)
My_Robotics_Corpus <- My_Robotics_Corpus %>% filter (ratio >= 3.4)



# Now I have a cleaned text as much as possible with a reasonable effort
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic_big_corpus_final_aug_2023.RData") # 14/08/2023

My_Robotics_Corpus$n_words <- NULL
My_Robotics_Corpus$ratio <- NULL
My_Robotics_Corpus$n_char <- NULL
My_Robotics_Corpus$et_al <- NULL
# I delete all environment except for My_Robotics_Corpus

library(text2vec)
##################################
# Training a GloVe word embedding
# https://medium.com/cmotions/nlp-with-r-part-2-training-word-embedding-models-and-visualize-results-ae444043e234
##################################


library(text2vec)
# We need to tokenize our already tokenized set as input for text2vec, re-use cleaned text in reviews_new
it <- itoken(My_Robotics_Corpus$text, 
             tokenizer = space_tokenizer,
            # BENCE GEREK YOK ids = Corpus_Lemmatized_new$ID,
             progressbar = TRUE)

# create a vocabulary out of the tokenset (stopword removal and bi-grams are optional)
vocab <- create_vocabulary(it) # use uni-grams

# text2vec has the option to prune the vocabulary of low-frequent words
vocab <- prune_vocabulary(vocab, term_count_min = 5) # Now, there are 183997 terms in the vocabulary 14/08/2023
# 183.997 words and 32.885.9590 tokens

# Constructing term-co-occurence matrix (TCM).
vectorizer <- vocab_vectorizer(vocab)
# Create a term-co-occurence: I will use a skipgram window of 5 (symmetrical)
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic_big_corpus_final_aug_2023.RData") # 14/08/2023
# load("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic_big_corpus_final_aug_2023.RData")

# x_max= 100 in the original paper.
# Pennington, J., Socher, R., & Manning, C. D. (2014, October). Glove: Global vectors for word representation. In Proceedings of the 2014 conference on empirical methods in natural language processing (EMNLP) (pp. 1532-1543).
# For all our experiments,xmax=100, α=3/4 ... with initial learning rate of 0.05.
# ... We run 50 iterations for vectors smaller than 300 dimensions, and 100 iterations otherwise (Pennington, et al. 2014).

# set up the embedding matrix and fit model
glove_model <- GloVe$new(rank = 300, x_max = 100, learning_rate = 0.04)

system.time({
glove_embedding = glove_model$fit_transform(tcm, n_iter = 200, n_threads = 8)
}) # elapsed: 13476.60


# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic_big_corpus_final_aug_2023.RData") # 14/08/2023

#> The GloVe model learns two sets of word vectors: main and context. 
#> Best practice is to combine both the main word vectors and the context word vectors into one matrix.
# combine main embedding and context embeddings (sum) into one matrix
dim(glove_embedding) # [1] 183997    300
dim(glove_model$components) # [1]    300 183997
glove_embedding_ORIGINAL <- glove_embedding # I take the copy of glove_embedding, because I will chang it below.

#> While both of word-vectors matrices can be used as result it usually better (idea from GloVe paper) to average or take a sum of main and context vector:
glove_embedding = glove_embedding + t(glove_model$components) # the transpose of the context matrix


#> Let’s find out how well GloVe is doing on our robotic texts.
word <- glove_embedding_ORIGINAL["control", , drop = FALSE] # I wonder if glove_embedding_YEDEK gives good results?
cos_sim = sim2(x = glove_embedding_ORIGINAL, y = word, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)

word <- glove_embedding["control", , drop = FALSE]
cos_sim = sim2(x = glove_embedding, y = word, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)


#> Let’s find out how well GloVe is doing on our robotic texts.
word <- glove_embedding_ORIGINAL["paper", , drop = FALSE] # I wonder if glove_embedding_YEDEK gives good results?
cos_sim = sim2(x = glove_embedding_ORIGINAL, y = word, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)

word <- glove_embedding["paper", , drop = FALSE]
cos_sim = sim2(x = glove_embedding, y = word, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)


# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic_big_corpus_final_aug_2023.RData") # 15/08/2023
# > embedding HARİÇ HERŞEYİ SİLİP AYRICA KAYDETTİM: save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/Glove_Model_Big_Corpus_xmax_100_15_Aug_2023.RData")


# Alternative modesl ----
# I tried various words mostly related to robotic (e.g. end, actuator, ai, artificial, servo, optic, steer, tire, surgery, aircraft)
# Result seem good.
# However results of words like paper and current are far away from their physical meaning.
# I will try different x_max values.

# In summary, a lower x_max makes the model more sensitive to less frequent word pairs, 
# while a higher x_max allows the model to consider a wider range of co-occurrence counts before treating them equally.

# load("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic_big_corpus_final_aug_2023.RData")

glove_model_xmax_20 <- GloVe$new(rank = 300, x_max = 20, learning_rate = 0.04)
system.time({
  glove_embedding_xmax_20 = glove_model_xmax_20$fit_transform(tcm, n_iter = 200, n_threads = 8)
})


glove_model_xmax_50 <- GloVe$new(rank = 300, x_max = 50, learning_rate = 0.04)
system.time({
  glove_embedding_xmax_50 = glove_model_xmax_50$fit_transform(tcm, n_iter = 200, n_threads = 8)
})

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic_big_corpus_final_aug_2023.RData") # 15/08/2023

glove_embedding_xmax_20 = glove_embedding_xmax_20 + t(glove_model_xmax_20$components)
glove_embedding_xmax_50 = glove_embedding_xmax_50 + t(glove_model_xmax_50$components)

rm(glove_embedding_ORIGINAL)
save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic_big_corpus_final_aug_2023.RData") # 15/08/2023

burada kaldım 16/08/2023 saat 20:36

# > üç tane embedding  HARİÇ HERŞEYİ SİLİP AYRICA KAYDETTİM: save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/Glove_Model_Big_Corpus_xmax_100_15_Aug_2023.RData")
glove_embedding
glove_embedding_xmax_20
glove_embedding_xmax_50

save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/Glove_Models_Embeddings_Big_Corpus_xmax_100_15_Aug_2023.RData")

burada_kaldım_16_08_023_SAAT_21_00 <- "Environemtn kaydettim" 





# Aşadaki işlemler modelleri karşılaştırmak için.
# KAYNAK:  https://medium.com/cmotions/nlp-with-r-part-2-training-word-embedding-models-and-visualize-results-ae444043e234

library(umap)


# GloVe dimension reduction
glove_umap <- umap(glove_embedding, n_components = 2, metric = "cosine", n_neighbors = 25, min_dist = 0.1, spread=2)
# 5 saaat civarında sürmüş.
# Dimensions of end result
dim(glove_umap)

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/My_Robotics_Corpus_Patent_STM_MISSING.RData")

system.time({
glove_umap_50 <- umap(glove_embedding, n_components = 2, metric = "cosine", n_neighbors = 50, min_dist = 0.1, spread=2)
}) # elapsed 384671 = 4,5 days 01 Aug 2023

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/My_Robotics_Corpus_Patent_STM_MISSING.RData")


burada kaldım 25.07.2022

# https://medium.com/cmotions/nlp-with-r-part-2-training-word-embedding-models-and-visualize-results-ae444043e234
# Put results in a dataframe for ggplot, starting with Word2Vec
# AŞAĞIDAKİ KOD DATAFRAME'E DÖNÜŞTÜREMEDİ!
df_glove_umap_50 <- as.data.frame(glove_umap_50, stringsAsFactors = FALSE)
# Add the labels of the words to the dataframe
df_word2vec_umap$word <- rownames(word2vec_embedding)
colnames(df_word2vec_umap) <- c("UMAP1", "UMAP2", "word")
df_word2vec_umap$technique <- 'Word2Vec'
cat(paste0('Our Word2Vec embedding reduced to 2 dimensions:', '\n'))
str(df_word2vec_umap)


gc()


# https://cjbarrie.github.io/CTA-ED/exercise-6-unsupervised-learning-word-embedding.html
df_glove_umap <- as.data.frame(glove_umap_50, stringsAsFactors = FALSE)
df_glove_umap <- as.data.frame(glove_umap[["layout"]])
df_glove_umap$word <- rownames(df_glove_umap)
colnames(df_glove_umap) <- c("UMAP1", "UMAP2", "word")

library(ggplot2)
ggplot(df_glove_umap) +
  geom_point(aes(x = UMAP1, y = UMAP2), colour = 'blue', size = 0.05) +
  ggplot2::annotate("rect", xmin = -3, xmax = -2, ymin = 5, ymax = 7,alpha = .2) +
  labs(title = "GloVe word embedding in 2D using UMAP")


# Plot the shaded part of the GloVe word embedding with labels
ggplot(df_glove_umap[df_glove_umap$UMAP1 < -2.5 & df_glove_umap$UMAP1 > -3 & df_glove_umap$UMAP2 > 5 & df_glove_umap$UMAP2 < 6.5,]) +
  geom_point(aes(x = UMAP1, y = UMAP2), colour = 'blue', size = 2) +
  geom_text(aes(UMAP1, UMAP2, label = word), size = 2.5, vjust=-1, hjust=0) +
  labs(title = "GloVe word embedding in 2D using UMAP - partial view") +
  theme(plot.title = element_text(hjust = .5, size = 14))


# Plot the word embedding of words that are related for the GloVe model
word <- glove_embedding["endeffector",, drop = FALSE]
cos_sim = sim2(x = glove_embedding, y = word, method = "cosine", norm = "l2")
select <- data.frame(rownames(as.data.frame(head(sort(cos_sim[,1], decreasing = TRUE), 25))))
colnames(select) <- "word"
selected_words <- df_glove_umap %>% 
  inner_join(y=select, by= "word")

#The ggplot visual for GloVe
ggplot(selected_words, aes(x = UMAP1, y = UMAP2)) + 
  geom_point(show.legend = FALSE) + 
  geom_text(aes(UMAP1, UMAP2, label = word), show.legend = FALSE, size = 2.5, vjust=-1.5, hjust=0) +
  labs(title = "GloVe word embedding of words related to 'economy'") +
  theme(plot.title = element_text(hjust = .5, size = 14))


#The ggplot visual for GloVe
ggplot(selected_words, aes(x = UMAP1, y = UMAP2)) + 
  geom_point(show.legend = FALSE) + 
  geom_text(aes(UMAP1, UMAP2, label = word), show.legend = FALSE, size = 2.5, vjust=-1.5, hjust=0) +
  labs(title = "GloVe word embedding of words related to 'endeffector'") +
  theme(plot.title = element_text(hjust = .5, size = 14))





# Plot the bottom part of the GloVe word embedding with labels
ggplot(df_glove_umap[df_glove_umap$UMAP1 > 0 & df_glove_umap$UMAP1 < 0.2 & df_glove_umap$UMAP2 > -5 & df_glove_umap$UMAP2 < -4.5,]) +
  geom_point(aes(x = UMAP1, y = UMAP2), colour = 'blue', size = 2) +
  geom_text(aes(UMAP1, UMAP2, label = word), size = 2.5, vjust=-1, hjust=0) +
  labs(title = "GloVe word embedding in 2D using UMAP - partial view") +
  theme(plot.title = element_text(hjust = .5, size = 14))



gc()


000000000000000000000000 Bunun yerine Google_news kullanacağım 00000000000000000000
# Put results in a dataframe for ggplot, starting with Word2Vec
df_word2vec_umap <- as.data.frame(word2vec_umap, stringsAsFactors = FALSE)
# Add the labels of the words to the dataframe
df_word2vec_umap$word <- rownames(word2vec_embedding)
colnames(df_word2vec_umap) <- c("UMAP1", "UMAP2", "word")
df_word2vec_umap$technique <- 'Word2Vec'
cat(paste0('Our Word2Vec embedding reduced to 2 dimensions:', '\n'))
str(df_word2vec_umap)
000000000000000000000000

# Do the same for the GloVe embeddings
df_glove_umap <- as.data.frame(glove_umap, stringsAsFactors = FALSE)


# Add the labels of the words to the dataframe
df_glove_umap$word <- rownames(glove_embedding)
colnames(df_glove_umap) <- c("UMAP1", "UMAP2", "word")
df_glove_umap$technique <- 'GloVe'
cat(paste0('\n', 'Our GloVe embedding reduced to 2 dimensions:', '\n'))
str(df_glove_umap)

# Combine the datasets
df_umap <- bind_rows(df_word2vec_umap, df_glove_umap)







000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000 kilitlenirse denemeyi kaydetmemeik için aşağı yazdım 000000000000000000000000

00000000000000000000000000000000000000000000000000000








