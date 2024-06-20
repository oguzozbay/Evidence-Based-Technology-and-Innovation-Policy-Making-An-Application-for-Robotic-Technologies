# Notes for thesis (02/04/2024) START
# 1. Articles unrelated to robotics will be discarded first.
# 2. The text will be lemmatized and incorrect lemmas will be corrected (e.g. center to center)
# 3. Then a preliminary STM will be performed (topic words obtained with STM will be examined and used to evaluate those that can be n-grams).
# 4. N-grams (collocations) will be determined using the library (text2vec) package.
# 5. I will also  extract the hyphenated words and frequencies in the TI and AB columns (those are possibly good n-grams)
# 6. I will save these ngrams to excel.
# 7. N-grams will be determined manually with the help of the code (df_pref_ext) that finds word pairs that start or end with a selected word and by examining the STM topic words.
# Notes for thesis (02/04/2024) END





library(fastmatch)
library(dplyr)
# load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_6_2_Naive_Bayes_Lemma_untidy_ready_to_use_in_naive_bayes_analysis.RData")
# I imported cleaned text data

Naive_Bayes_Lemma_untidy # Ready to Naive Bayes Analysis text (NOTE: I deleted other environment data)
#(i.e. AB, TI and keywords are united, lemmatized, lemmas are corrected, and then lemmas are untidied)
# C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_6_2_Naive_Bayes_Lemma_untidy_ready_to_use_in_naive_bayes_analysis.RData
M_for_STM <- Naive_Bayes_Lemma_untidy # for ease of reading
rm(Naive_Bayes_Lemma_untidy)

# load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_7_5_WOS_numbers_to_exclude.RData")
WOS_to_exclude # robotics un-related WOS numbers


# I will extract text data of articles in which "Research Area = robotic"
library(fastmatch)
compare_rows <- M_for_STM$UT %fin% WOS_to_exclude$UT  # the %fin% function comes from the `fastmatch` package
M_for_STM <- M_for_STM[which(compare_rows == FALSE),]

library(tidytext)
# text_df %>% unnest_tokens(word, text) # word= sütuna verilecek ad, text tokenize edilecek sütun
lemmas <- M_for_STM %>% unnest_tokens(lemmas_of_text, Untidy_of_AB_TI_key_lemmas)
singular_lemmas <- lemmas %>% group_by(lemmas_of_text) %>% summarise(repeats = n())
rm(lemmas)


# openxlsx::write.xlsx(x = singular_lemmas, file = "STM_text_lemmas_31_aug.xlsx")
STM_text_lemmas_31_aug_Macro # I manually corrected excel file and renamed (STM_text_lemmas_31_aug_Macro.xlsx)

# openxlsx::write.xlsx(x = M_for_STM, file = "STM_full_text_31_aug.xlsx")
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_1_0_data_preperation.RData")


M_for_STM$key_oguz <- 1:nrow(M_for_STM) # adding keys for untidying
M_for_STM$key_oguz <- as.numeric(M_for_STM$key_oguz)
M_for_STM <- M_for_STM %>% relocate(key_oguz, .before = UT)

lemmas <- M_for_STM %>% unnest_tokens(lemmas_of_text, Untidy_of_AB_TI_key_lemmas)

# df1$entry_column <- df2$value_to_take[match(df1$colum_to_match, df2$column_to_match)]
# NOTE: Because all tokens are included, there are no NA. (ie. Replacement list includes not changed tokens)
# load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_2_0_data_preperation.R.RData")

lemmas$corrected_token <- STM_text_lemmas_31_aug_Macro$to[match(lemmas$lemmas_of_text,
                                                                STM_text_lemmas_31_aug_Macro$from)]

library(stringr)
tidy_M_for_STM <- lemmas %>% group_by(key_oguz, UT) %>% summarize(key_TI_AB = str_c(corrected_token, collapse = " ")) %>% ungroup()

tidy_M_for_STM$key_TI_AB <- gsub(pattern= "OGUZOZBAYDELETE", replacement = " ", tidy_M_for_STM$key_TI_AB)
tidy_M_for_STM$key_TI_AB <- tidy_M_for_STM$key_TI_AB %>% stringr::str_squish()

# I deleted all environment except tidy_M_for_STM

# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_3_0_data_preperation.RData")
# load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W1_A_WOS_united_raw_data_Environment.RData")

library(dplyr)
M_final_STM <- left_join(tidy_M_for_STM, M, "UT")
rm(M)


M_final_STM <- M_final_STM %>% select(UT, key_TI_AB, AU, AF, AB, ID, DE, TI, CR, PY, Z9, UT, SO, DT, SC)

# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_3_0_data_preperation.RData")


M_final_STM$PY_corrected <- M_final_STM$PY # there are NA PYs. I will correct them (all of them are early acces. I think :)
M_final_STM$PY_corrected[is.na(M_final_STM$PY_corrected)] <- 2022 


library(stm)
system.time({
  processed <- textProcessor(M_final_STM$key_TI_AB, metadata = M_final_STM, wordLengths = c(2, Inf), stem = FALSE,
                             removepunctuation = FALSE) # metadata = data frame olacak!
}) # elapsed:

# I will create a STM model
system.time({
  out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 3)
})
# lower.thresh = minimum numbers of documents to be exist.
# Removing 71631 of 106019 terms (97906 of 16256101 tokens) due to frequency 
# Your corpus now has 211585 documents, 34388 terms and 16158195 tokens.

docs <- out$documents
vocab <- out$vocab
meta <- out$meta


system.time({
  STM_Eylul_2022 <- stm(documents = out$documents, vocab = out$vocab, K = 0, seed = 321,
                  prevalence = ~ PY_corrected,
                  max.em.its = 200, data = out$meta,
                  init.type = "Spectral", verbose = FALSE)    # NOTE: Roberts (2014) says that: "Spectral" reveals better results.
}) # elapsed


# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_3_0_data_preperation.RData")
labelTopics(STM_Eylul_2022, topics = NULL, n = 7, frexweight = 0.5) # To see topic words

findThoughts(STM_Eylul_2022, texts = M_final_STM$TI,
             n = 15, topics = 1:72) # It gives the 2 most prominent documents for Topic 3.


# load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_3_0_data_preperation.RData")


# collocation analysis (n-grams)
library(text2vec)
# preprocessor = function(x) {gsub("[^[:alnum:]\\s]", replacement = " ", tolower(x))} # to discard anything that is not a letter or number
sample_ind = 1:211585
# tokens = word_tokenizer(preprocessor(M_final_STM$key_TI_AB[sample_ind])) 
tokens = word_tokenizer(M_final_STM$key_TI_AB[sample_ind]) # 211585 rowa are tokenized 

it = itoken(tokens, ids = M_final_STM$UT[sample_ind]) # it= An input itoken or itoken_parallel iterator
system.time(v <- create_vocabulary(it))

v = prune_vocabulary(v, term_count_min = 10) # throws out very frequent and very infrequent terms.

# Attention!...: When pmi_min = 5, it does not capture the "human robot" in below code
model = Collocations$new(collocation_count_min = 25, pmi_min = 5, sep = "=") # model= A Collocation model object (orijinal 5 ve 5 idi)
# model = Collocations$new(vocabulary = NULL, collocation_count_min = 50, pmi_min = 5, gensim_min = 0, lfmd_min = -Inf, llr_min = 0, sep = "_") # help page definiton

model$fit(it, n_iter = 100)
model$collocation_stat # to see

it2 = model$transform(it)
v2 = create_vocabulary(it2)
v2 = prune_vocabulary(v2, term_count_min = 5) # In v2 there are single words and underscores
# check what phrases model has learned
setdiff(v2$term, v$term) #  setdiff= which elements of a vector or data frame X are not existent in a vector or data frame Y.

v2 <- within(v2,{underline = as.numeric(grepl("=", fixed = FALSE, v2$term))}) # I wrote how many "=" exist in the column. If it is equal to 0, it is a single word.
word_pairs <- v2 %>% filter(underline == 1)

# openxlsx::write.xlsx(x = word_pairs, file = "word_pairs.xlsx")
# openxlsx::write.xlsx(x = v2, file = "all_words_and_word_pairs.xlsx")

# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_3_0_data_preperation.RData")

# I extracted the n_grams above.
# While examining the n_grams, I thought I would also extract the hyphenated words.
# For this, I will extract the hyphenated words separately in the TI and AB columns.

library(stringr)
prefix_extraction <- str_extract_all(M_final_STM$TI, "([^\\s]+)-([^\\s]+)")
df_pref_ext <- data.frame(matrix(unlist(prefix_extraction), 
                                 nrow = sum(sapply(prefix_extraction, length)),
                                 byrow = TRUE), stringsAsFactors=FALSE)


colnames(df_pref_ext)[1] <- "term" # okunur hale gelsin diye
df_pref_ext  <- df_pref_ext  %>% group_by(term) %>% summarise(number = n()) %>% arrange(desc(number)) # counting
df_pref_ext <- df_pref_ext %>% filter(number > 10)

# openxlsx::write.xlsx(x = df_pref_ext, file = "hyphened_words_in_TI_oct_2022.xlsx")


library(stringr)
prefix_extraction <- str_extract_all(M_final_STM$AB, "([^\\s]+)-([^\\s]+)")
df_pref_ext <- data.frame(matrix(unlist(prefix_extraction), 
                                 nrow = sum(sapply(prefix_extraction, length)),
                                 byrow = TRUE), stringsAsFactors=FALSE)

colnames(df_pref_ext)[1] <- "term" # for a better readability (for being understandable)
df_pref_ext  <- df_pref_ext  %>% group_by(term) %>% summarise(number = n()) %>% arrange(desc(number)) # counting
df_pref_ext <- df_pref_ext %>% filter(number > 15)

# openxlsx::write.xlsx(x = df_pref_ext, file = "hyphened_words_in_AB_oct_2022.xlsx")


# I find words starting with multi:
library(stringr)
prefix_extraction <- str_extract_all(M_final_STM$key_TI_AB, "\\bmulti\\w+")
df_pref_ext <- data.frame(matrix(unlist(prefix_extraction), 
                                 nrow = sum(sapply(prefix_extraction, length)),
                                 byrow = TRUE), stringsAsFactors=FALSE)

colnames(df_pref_ext)[1] <- "term" # for readability
df_pref_ext  <- df_pref_ext  %>% group_by(term) %>% summarise(number = n()) %>% arrange(desc(number)) # counting
df_pref_ext <- df_pref_ext %>% filter(number > 1)

# openxlsx::write.xlsx(x = df_pref_ext, file = "starts_with_multi_united_word_oct_2022.xlsx")


# find words preceded by multi
library(stringr)
prefix_extraction <- str_extract_all(M_final_STM$key_TI_AB, "(\\b(multi))\\s([^\\s]+)")
df_pref_ext <- data.frame(matrix(unlist(prefix_extraction), 
                                 nrow = sum(sapply(prefix_extraction, length)),
                                 byrow = TRUE), stringsAsFactors=FALSE)

colnames(df_pref_ext)[1] <- "term" # okunur hale gelsin diye
df_pref_ext  <- df_pref_ext  %>% group_by(term) %>% summarise(number = n()) %>% arrange(desc(number)) # counting
df_pref_ext <- df_pref_ext %>% filter(number > 5)


# openxlsx::write.xlsx(x = df_pref_ext, file = "starts_with_multi_as_a_separate__word_oct_2022.xlsx")


library(stringr)
prefix_extraction <- str_extract_all(M_final_STM$key_TI_AB, "(\\b(non))\\s([^\\s]+)")
df_pref_ext <- data.frame(matrix(unlist(prefix_extraction), 
                                 nrow = sum(sapply(prefix_extraction, length)),
                                 byrow = TRUE), stringsAsFactors=FALSE)

colnames(df_pref_ext)[1] <- "term" 
df_pref_ext  <- df_pref_ext  %>% group_by(term) %>% summarise(number = n()) %>% arrange(desc(number))
df_pref_ext <- df_pref_ext %>% filter(number > 5)
# openxlsx::write.xlsx(x = df_pref_ext, file = "starts_with_non_as_a_separate__word_oct_2022.xlsx")
 
 
 
# I removed the ngrams.
# I'll change them.
 
# load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_3_0_data_preperation.RData")

 
 
 
 
 
 

