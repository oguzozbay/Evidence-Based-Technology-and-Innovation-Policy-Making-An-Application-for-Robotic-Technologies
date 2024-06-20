# This is the code for final STM model 08_03_2024 # WAS (This is the code for final STM model 07_03_2024)
# I created an STM model, determined Ngrams and updated the STM model. 
# I determined new NGrams in the updated STM model. Using these Ngrams, I created a new STM model. 
# In this way, I renewed the STM model five or six times (I don't remember the number right now). # WAS (I last created an STM model on 07 March 2024 (STM_patent_Final_V1).
# Now I am creating STM_patent_K100, STM_patent_K96, STM_patent_K92 and STM_patent_K90 (08_03_2024)
# Then I will select one of these models.


# In summary: I had to create a lot of STM models, even though I always thought it would be the last time.
# The Chinese text was poorly translated into English, I identified new Ngrams while examining the topic words of the STM model models.
# I didn't like the previous STM model either, so I created list_6, hoping this time would be the last one.
# for example, I turned the word "ware people" into => robot 
# (I examined the original patent texts, I checked a few patents by translating the original Chinese texts
# into English with Google Gemini and ChatGPT).



setwd("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024")
library(dplyr)
library(quanteda)
library(quanteda.textstats)
library(stm)
library(stringr)
library(stringi)
library(tidytext)


# Last time I ran the following code and I got a STM model for patent with K=100.
# C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM 2005_ 2023_K100_new_NGrams_STEP2_29_02_2024.R

# I defined some new N-Grams.
# And I also think that K=94 or K=104 may be beter than K=100.
# In conclusion, I will apply new N-Grams and then I will run 3 STM models (K=96, K=100i K=104) 01/03/2024


# load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_new_NGrams_STEP2_Ready_to_run_STM_01_03_2024.RData")

ls()
rm(BURADA_KALDIM, BURADA_KALDIM2, Patent_docvars, out, word_freqs_of_DFM)


patents_cluster_in_text$STM_text_Cluster # is the latest text data in which new_n_grams_after_K100, list_1 and list_2 was previously applied.
# I renamed $STM_text_Cluster in order to use same codes.

# I will import final N-Grams and replacement list
library(readxl)
list_3 <- read_excel("new n_grams after K100 new patents model_STEP3.xlsx", 
                     sheet = "NGram_STEP3", col_types = c("skip","text", "text", "skip"))



read_me_UPDATED_2_patents_cluster_in_text <- "this is patents from 01.012005 - 31.12.2023. The text is cleaned leamatized and Ngrams are applied. 
 IPC code clusters are inserted into the patent text The text is ready for STM. 
 The last thing to be completed is removal of delete_tokens_list. 
 But I applied new N-Grams patents_cluster_in_text$New_NGrams. 
 Now I will apply new N-Grams and corrections from list_1 and list_2. Now I will apply list_3"


list_3$from <- list_3$from %>% stringr::str_squish()
list_3$to <- list_3$to %>% stringr::str_squish()

# Replacement of N-Grmas
# Function for replacement (NOTE THAT, there are no special characters in new_n_grams_after_K100)
library(stringi)
replace_words <- function(text, word_from, word_to) {
  for (i in seq_along(word_from)) {
    pattern <- paste0("\\b", word_from[i], "\\b")
    text <- stri_replace_all_regex(text, pattern, word_to[i])
  }
  return(text)
}

patents_cluster_in_text$STM_text_Cluster # Latest text

# create a new column for corrections
patents_cluster_in_text$New_NGrams <- ""

# Apply the function to the $STM_text_Cluster column and get $New_NGrams 
system.time({
  patents_cluster_in_text$New_NGrams <- replace_words(patents_cluster_in_text$STM_text_Cluster,
                                                        list_3$from,
                                                        list_3$to)
  
  patents_cluster_in_text$New_NGrams <- patents_cluster_in_text$New_NGrams %>% stringr::str_squish()
  
  
  # STEP-2 I will run same code again
  patents_cluster_in_text$New_NGrams <- replace_words(patents_cluster_in_text$New_NGrams,
                                                      list_3$from,
                                                      list_3$to)
  
  patents_cluster_in_text$New_NGrams <- patents_cluster_in_text$New_NGrams %>% stringr::str_squish()
  
  
  # STEP-3 I will run same code again
  patents_cluster_in_text$New_NGrams <- replace_words(patents_cluster_in_text$New_NGrams,
                                                      list_3$from,
                                                      list_3$to)
  
  patents_cluster_in_text$New_NGrams <- patents_cluster_in_text$New_NGrams %>% stringr::str_squish()
  
  
  
  # save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM K_94_100_104_new_NGrams_STEP3_01_03_2024.RData")
  
}) # elapsed 947 (01/03/2024 16:00)

# load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM K_94_100_104_new_NGrams_STEP3_01_03_2024.RData")

############################################
# checking replacements
sil <- patents_cluster_in_text %>% select (New_NGrams, STM_text_Cluster)
# Comparing original and replaced text
sil$check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(sil[,1], '\\s'), strsplit(sil[,2], '\\s'))
sil$check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(sil[,2], '\\s'), strsplit(sil[,1], '\\s'))
sil_filtered <- sil %>% filter(check_1 != "" | check_2 != "")
############################################
############################################

rm(sil, sil_filtered)

# STEP  (dfm) ----
# Now I will create document feature matrix and then delete stop words.

# OGUZ_LOOK_HERE
patents_cluster_in_text$STM_text_Cluster <- patents_cluster_in_text$New_NGrams # In order to use previous code.
patents_cluster_in_text$New_NGrams <- NULL

library(tidytext)
tidy_Patent <- patents_cluster_in_text %>% unnest_tokens(output= word, input= STM_text_Cluster, token = "words")
# tidy_Patent %>% count(word, sort = TRUE)

# create document feature matrix (library(tidytext))
Patent_dfm <- tidy_Patent %>% count(key, word) %>% cast_dfm(key, word, n) # cast_dfm(data, document, term, value, ...)
# Document-feature matrix of: 342,565 documents, 52,692 features (99.91% sparse) and 0 docvars.

# I keep only words occurring at least 4 times and at least in 4 documents (library(quanteda))
library(quanteda)
Patent_dfm_trim_1 <- dfm_trim(Patent_dfm, min_termfreq = 4, min_docfreq = 4, termfreq_type = "count", docfreq_type = "count")
# Document-feature matrix of: 342,565 documents, 22,747 features (99.79% sparse) and 0 docvars.

# Now I will define single character tokens (i.e. a, b, c, ... etc.)
# Then I will add these to my stop-word list (NOTE: stop-word has already been removed.
# "my stop-word" means here some additional stopwords. 
tokens <- tidy_Patent %>% count(word, sort = TRUE)
tokens$n_char <- 0
tokens$n_char <-  nchar(tokens$word)
single_characters <- tokens %>% filter(n_char == 1)
single_characters <- single_characters$word
delete_tokens_list <- single_characters

# Add additional stop_words to delete_tokens_list
delete_tokens_list <- c(single_characters, 
                        "aa", "bb", "cc", "dd", "ee", "ff", "gg", "hh", "ii", "jj", "kk", "ll", "mm", "ul0002",
                        "nn", "oo", "pp", "qq", "rr", "ss", "tt", "uu", "vv", "ww", "xx", "yy", "zz",
                        "can_be", "be_use_for", "plurality_of", "so_that", "which_be", "such_as", "sf", "li", "s0", 
                        "thereof", "still", "said", "whose", "whole","ul0002","constitution",
                        "theta1", "theta2","theta3","theta4", "theta5", "theta6", "hss", "th", "sub", 
                        "vii", "viii", "vi", "iv", "xi", "xii", "xiii","my1", "mx1", "fx1", "fy1", "fz1",
                        "fx2", "fy2", "fz2", "mx2", "my2", "mz2", "sub") # NOTE: "constitution:" is a heading in patent abstracts.


# Removal of delete_tokens_list from dfm
Patent_dfm_trim_2 <- dfm_remove(Patent_dfm_trim_1, delete_tokens_list, verbose = TRUE) # removed 67 features
# Document-feature matrix of: 342,565 documents, 22,680 features (99.79% sparse) and 0 docvars

# Let me check the resultant dfm
library(quanteda.textstats)
word_freqs_of_DFM <-as.data.frame(quanteda.textstats::textstat_frequency(Patent_dfm_trim_2))
# Every thing seem OK.
Patent_dfm <- Patent_dfm_trim_2
rm(Patent_dfm_trim_2, Patent_dfm_trim_1)



# I will create "out" for STM
#> STM Vignette says that: "Note that when using quanteda dfm directly there may be higher memory use 
#> (because the texts and metadata are stored twice). You can convert from quanteda’s format directly 
#> to our native format using the quanteda function convert."
colnames(patents_cluster_in_text)
# [1] "key" "application_id" "year" "Country" "IPC" "title_corrected" "STM_text_Cluster"
Patent_docvars <- patents_cluster_in_text %>% select(key, year) # I will not use cluster because I inserted cluster into the text
#> You can convert from quanteda’s format directly to STM's native format using the quanteda function convert.
out <- convert(Patent_dfm, to = 'stm', docvars = Patent_docvars)
ls()
rm(tidy_Patent, single_characters, tokens, delete_tokens_list, Patent_dfm)

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_K94_100_104_Ready_to_run_STM_01_03_2024.RData")



#
###
#####
#######
######### STM RUN (01/03/2024)
#######
#####
###
#

# NOW I WILL RUN FINAL three different STM models (I HOPE SO - 01/03/2024 16:30)
# load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_K94_100_104_Ready_to_run_STM_01_03_2024.RData")

library(stm)
system.time({
  set.seed(2023)
  STM_patents_K96 <- stm(documents= out$documents,
                                     vocab= out$vocab,
                                     K=96,
                                     prevalence = ~s(year),
                                     data = out$meta,
                                     init.type = "Spectral", # # NOT: Roberts (2014), "Spectral" daha iyi sonuç veriyor diyor.
                                     max.em.its = 500,
                                     seed = 2023)
  # save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_K96_100_104_STM_02_2024.RData")
  
  set.seed(2023)
  STM_patents_K100 <- stm(documents= out$documents,
                                     vocab= out$vocab,
                                     K=100,
                                     prevalence = ~s(year),
                                     data = out$meta,
                                     init.type = "Spectral", # # NOT: Roberts (2014), "Spectral" daha iyi sonuç veriyor diyor.
                                     max.em.its = 500,
                                     seed = 2023)
  # save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_K96_100_104_STM_02_2024V2.RData")
  
  set.seed(2023)
  STM_patents_K104 <- stm(documents= out$documents,
                                     vocab= out$vocab,
                                     K=104,
                                     prevalence = ~s(year),
                                     data = out$meta,
                                     init.type = "Spectral", # # NOT: Roberts (2014), "Spectral" daha iyi sonuç veriyor diyor.
                                     max.em.its = 500,
                                     seed = 2023)
  # save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_K96_100_104_STM_02_2024V3.RData")
  
  
}) # elapsed 18,2 hours 01/03/2024




#########
# Label topics
ls()
STM_patents_K96
STM_patents_K100
STM_patents_K104

# I reviewed above STM models. K=100 seem good.
# I defined new N-Grams while I was reviewing topic words.
# I fill make new Ngrams.

# Delete all environment.
# rm(list = ls())


# Import final text.
# load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_K94_100_104_Ready_to_run_STM_01_03_2024.RData")


read_me_UPDATED_3_patents_cluster_in_text <- "this is patents from 01.012005 - 31.12.2023. The text is cleaned leamatized and Ngrams are applied. 
 IPC code clusters are inserted into the patent text The text is ready for STM. 
 The last thing to be completed is removal of delete_tokens_list. 
 But I applied new N-Grams patents_cluster_in_text$New_NGrams. 
 Now I will apply new N-Grams and corrections from list_1 and list_2. Now I will apply list_3
 Now I will apply list_4"


# Import laters N-Grmas and reopacements list
library(readxl)
list_4 <- read_excel("new n_grams after K100 new patents model_STEP4.xlsx", 
                     sheet = "new_Ngrams", col_types = c("skip","text", "text", "skip"))

list_4$from <- list_4$from %>% stringr::str_squish()
list_4$to <- list_4$to %>% stringr::str_squish()

patents_cluster_in_text$STM_text_Cluster # Latest up-to-date text. 
# I renamed "list_3" applied text as "STM_text_Cluster" in order to use same codes (see : OGUZ_LOOK_HERE)



# Apply the function to the $STM_text_Cluster column and get $New_NGrams 
# create a new column for corrections
patents_cluster_in_text$New_NGrams <- ""

# Apply the function to the $STM_text_Cluster column and get $New_NGrams 
system.time({
  patents_cluster_in_text$New_NGrams <- replace_words(patents_cluster_in_text$STM_text_Cluster,
                                                      list_4$from,
                                                      list_4$to)
  
  patents_cluster_in_text$New_NGrams <- patents_cluster_in_text$New_NGrams %>% stringr::str_squish()
  
  
  # STEP-2 I will run same code again
  patents_cluster_in_text$New_NGrams <- replace_words(patents_cluster_in_text$New_NGrams,
                                                      list_4$from,
                                                      list_4$to)
  
  patents_cluster_in_text$New_NGrams <- patents_cluster_in_text$New_NGrams %>% stringr::str_squish()
  
  
  # STEP-3 I will run same code again
  patents_cluster_in_text$New_NGrams <- replace_words(patents_cluster_in_text$New_NGrams,
                                                      list_4$from,
                                                      list_4$to)
  
  patents_cluster_in_text$New_NGrams <- patents_cluster_in_text$New_NGrams %>% stringr::str_squish()
  
  
  
  # save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_04_March_2024_V1.RData")
  
}) # elapsed 1583 (04/03/2024 16:00)



############################################
# checking replacements
sil <- patents_cluster_in_text %>% select (New_NGrams, STM_text_Cluster)
# Comparing original and replaced text
sil$check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(sil[,1], '\\s'), strsplit(sil[,2], '\\s'))
sil$check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(sil[,2], '\\s'), strsplit(sil[,1], '\\s'))
sil_filtered <- sil %>% filter(check_1 != "" | check_2 != "")
############################################
############################################

rm(sil, sil_filtered)

# STEP  (dfm) ----
# Now I will create document feature matrix and then delete stop words.

patents_cluster_in_text$STM_text_Cluster <- patents_cluster_in_text$New_NGrams # In order to use previous code.
patents_cluster_in_text$New_NGrams <- NULL

library(tidytext)
tidy_Patent <- patents_cluster_in_text %>% unnest_tokens(output= word, input= STM_text_Cluster, token = "words")
# tidy_Patent %>% count(word, sort = TRUE)

# create document feature matrix (library(tidytext))
Patent_dfm <- tidy_Patent %>% count(key, word) %>% cast_dfm(key, word, n) # cast_dfm(data, document, term, value, ...)
Patent_dfm # Document-feature matrix of: 342,565 documents, 52,752 features (99.91% sparse) and 0 docvars.

# I keep only words occurring at least 4 times and at least in 4 documents (library(quanteda))
library(quanteda)
Patent_dfm_trim_1 <- dfm_trim(Patent_dfm, min_termfreq = 4, min_docfreq = 4, termfreq_type = "count", docfreq_type = "count")
Patent_dfm_trim_1 # Document-feature matrix of: 342,565 documents, 22,819 features (99.79% sparse) and 0 docvars.

# Now I will define single character tokens (i.e. a, b, c, ... etc.)
# Then I will add these to my stop-word list (NOTE: stop-word has already been removed.
# "my stop-word" means here some additional stopwords. 
tokens <- tidy_Patent %>% count(word, sort = TRUE)
tokens$n_char <- 0
tokens$n_char <-  nchar(tokens$word)
single_characters <- tokens %>% filter(n_char == 1)
single_characters <- single_characters$word
delete_tokens_list <- single_characters

# Add additional stop_words to delete_tokens_list
delete_tokens_list <- c(single_characters, 
                        "aa", "bb", "cc", "dd", "ee", "ff", "gg", "hh", "ii", "jj", "kk", "ll", "mm", "ul0002",
                        "nn", "oo", "pp", "qq", "rr", "ss", "tt", "uu", "vv", "ww", "xx", "yy", "zz",
                        "can_be", "be_use_for", "plurality_of", "so_that", "which_be", "such_as", "sf", "li", "s0", 
                        "thereof", "still", "said", "whose", "whole","ul0002","constitution",
                        "theta1", "theta2","theta3","theta4", "theta5", "theta6", "hss", "th", "sub", 
                        "vii", "viii", "vi", "iv", "xi", "xii", "xiii","my1", "mx1", "fx1", "fy1", "fz1",
                        "fx2", "fy2", "fz2", "mx2", "my2", "mz2", "sub") # NOTE: "constitution:" is a heading in patent abstracts.


# Removal of delete_tokens_list from dfm
Patent_dfm_trim_2 <- dfm_remove(Patent_dfm_trim_1, delete_tokens_list, verbose = TRUE) # removed 67 features
Patent_dfm_trim_2 # Document-feature matrix of: 342,565 documents, 22,752 features (99.79% sparse) and 0 docvars

# Let me check the resultant dfm
library(quanteda.textstats)
word_freqs_of_DFM <-as.data.frame(quanteda.textstats::textstat_frequency(Patent_dfm_trim_2))
# Every thing seem OK.
Patent_dfm <- Patent_dfm_trim_2
rm(Patent_dfm_trim_2, Patent_dfm_trim_1)



# I will create "out" for STM
#> STM Vignette says that: "Note that when using quanteda dfm directly there may be higher memory use 
#> (because the texts and metadata are stored twice). You can convert from quanteda’s format directly 
#> to our native format using the quanteda function convert."
colnames(patents_cluster_in_text)
# [1] "key" "application_id" "year" "Country" "IPC" "title_corrected" "STM_text_Cluster"
Patent_docvars <- patents_cluster_in_text %>% select(key, year) # I will not use cluster because I inserted cluster into the text
#> You can convert from quanteda’s format directly to STM's native format using the quanteda function convert.
out <- convert(Patent_dfm, to = 'stm', docvars = Patent_docvars)
ls()
rm(tidy_Patent, single_characters, tokens, delete_tokens_list, Patent_dfm)

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_04_March_2024_V2.RData")
# rm(list = ls())

# NOW I WILL RUN FINAL STM for K=100 (I HOPE SO - 01/03/2024 16:30)
# load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_04_March_2024_V2.RData")

library(stm)
system.time({
  set.seed(2023)
  STM_patent_Final <- stm(documents= out$documents,
                         vocab= out$vocab,
                         K= 100,
                         prevalence = ~s(year),
                         data = out$meta,
                         init.type = "Spectral", # # NOT: Roberts (2014), "Spectral" daha iyi sonuç veriyor diyor.
                         max.em.its = 500,
                         seed = 2023)
}) # elapsed 26918 hours 04/03/2024

BURADA_KALDIM <- "STM final model K=100 04/03/2024"

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_Model_March_2024.RData")

# load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_Model_March_2024.RData") # To load STM model


#
####
######
########
########## # Unfortunately, There are additional Ngrams to replace.06/03/2024
########
######
####
#


# I will import Latest text.
load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_04_March_2024_V1.RData")

patents_cluster_in_text$New_NGrams # list_4 is changed in this column.
patents_cluster_in_text$STM_text_Cluster <- patents_cluster_in_text$New_NGrams # In order to use previous code.
patents_cluster_in_text$New_NGrams <- NULL

patents_cluster_in_text$STM_text_Cluster # Now Latest up-to-date text. 

# import list_5
library(readxl)
list_5 <- read_excel("new n_grams after K100 new patents model_STEP5.xlsx", 
                     sheet = "NGRams_5", col_types = c("skip", "text", "text", "skip"))


list_5$from <- list_5$from %>% stringr::str_squish()
list_5$to <- list_5$to %>% stringr::str_squish()

# Replace "oguzozbaydelete" with an empty space in the column list_5$to
list_5$to <- gsub("oguzozbaydelete", " ", list_5$to)
list_5$to <- list_5$to %>% stringr::str_squish()


# Apply the function to the $STM_text_Cluster column and get $New_NGrams 
# create a new column for corrections
patents_cluster_in_text$New_NGrams <- ""

# Apply the function to the $STM_text_Cluster column and get $New_NGrams 
system.time({
  patents_cluster_in_text$New_NGrams <- replace_words(patents_cluster_in_text$STM_text_Cluster,
                                                      list_5$from,
                                                      list_5$to)
  
  patents_cluster_in_text$New_NGrams <- patents_cluster_in_text$New_NGrams %>% stringr::str_squish()
  
  
  # STEP-2 I will run same code again
  patents_cluster_in_text$New_NGrams <- replace_words(patents_cluster_in_text$New_NGrams,
                                                      list_5$from,
                                                      list_5$to)
  
  patents_cluster_in_text$New_NGrams <- patents_cluster_in_text$New_NGrams %>% stringr::str_squish()
  
  
  # STEP-3 I will run same code again
  patents_cluster_in_text$New_NGrams <- replace_words(patents_cluster_in_text$New_NGrams,
                                                      list_5$from,
                                                      list_5$to)
  
  patents_cluster_in_text$New_NGrams <- patents_cluster_in_text$New_NGrams %>% stringr::str_squish()
  
  
  # STEP-4 I will run same code again
  patents_cluster_in_text$New_NGrams <- replace_words(patents_cluster_in_text$New_NGrams,
                                                      list_5$from,
                                                      list_5$to)
  
  patents_cluster_in_text$New_NGrams <- patents_cluster_in_text$New_NGrams %>% stringr::str_squish()
  
  save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_Model_March_2024_text_UPDATE.RData")
  
}) # elapsed  7500 (06/03/2024)


############################################
# checking replacements
sil <- patents_cluster_in_text %>% select (New_NGrams, STM_text_Cluster)
# Comparing original and replaced text
sil$check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(sil[,1], '\\s'), strsplit(sil[,2], '\\s'))
sil$check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(sil[,2], '\\s'), strsplit(sil[,1], '\\s'))
sil_filtered <- sil %>% filter(check_1 != "" | check_2 != "")
############################################
############################################
rm(sil, sil_filtered)

# NOW I will sun STM
# First I will create dtm
load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_Model_March_2024_text_UPDATE.RData")
patents_cluster_in_text$STM_text_Cluster <- patents_cluster_in_text$New_NGrams # In order to use previous code.
patents_cluster_in_text$New_NGrams <- NULL


# A few corrections:
patents_cluster_in_text$STM_text_Cluster <- gsub("\\bsocietyindustrial\\b", 
                                                 "society industrial", patents_cluster_in_text$STM_text_Cluster, perl = TRUE)

patents_cluster_in_text$STM_text_Cluster <- gsub("\\bsocietyin\\b", 
                                                 "society", patents_cluster_in_text$STM_text_Cluster, perl = TRUE)


library(tidytext)
tidy_Patent <- patents_cluster_in_text %>% unnest_tokens(output= word, input= STM_text_Cluster, token = "words")
# tidy_Patent %>% count(word, sort = TRUE)

# create document feature matrix (library(tidytext))
Patent_dfm <- tidy_Patent %>% count(key, word) %>% cast_dfm(key, word, n) # cast_dfm(data, document, term, value, ...)
Patent_dfm # Document-feature matrix of: 342,565 documents, 52,687 features (99.91% sparse) and 0 docvars.

# I keep only words occurring at least 4 times and at least in 4 documents (library(quanteda))
library(quanteda)
Patent_dfm_trim_1 <- dfm_trim(Patent_dfm, min_termfreq = 4, min_docfreq = 4, termfreq_type = "count", docfreq_type = "count")
Patent_dfm_trim_1 # Document-feature matrix of: 342,565 documents, 22,848 features (99.79% sparse) and 0 docvars..

# Now I will define single character tokens (i.e. a, b, c, ... etc.)
# Then I will add these to my stop-word list (NOTE: stop-word has already been removed.
# "my stop-word" means here some additional stopwords. 
tokens <- tidy_Patent %>% count(word, sort = TRUE)
tokens$n_char <- 0
tokens$n_char <-  nchar(tokens$word)
single_characters <- tokens %>% filter(n_char == 1)
single_characters <- single_characters$word
delete_tokens_list <- single_characters

# Add additional stop_words to delete_tokens_list
delete_tokens_list <- c(single_characters, 
                        "aa", "bb", "cc", "dd", "ee", "ff", "gg", "hh", "ii", "jj", "kk", "ll", "mm", "ul0002",
                        "nn", "oo", "pp", "qq", "rr", "ss", "tt", "uu", "vv", "ww", "xx", "yy", "zz",
                        "can_be", "be_use_for", "plurality_of", "so_that", "which_be", "such_as", "sf", "li", "s0", 
                        "thereof", "still", "said", "whose", "whole","ul0002","constitution",
                        "theta1", "theta2","theta3","theta4", "theta5", "theta6", "hss", "th", "sub", 
                        "vii", "viii", "vi", "iv", "xi", "xii", "xiii","my1", "mx1", "fx1", "fy1", "fz1",
                        "fx2", "fy2", "fz2", "mx2", "my2", "mz2", "sub", "aforementione") # NOTE: "constitution:" is a heading in patent abstracts.


# Removal of delete_tokens_list from dfm
Patent_dfm_trim_2 <- dfm_remove(Patent_dfm_trim_1, delete_tokens_list, verbose = TRUE) # removed 68 features
Patent_dfm_trim_2 # Document-feature matrix of: 342,565 documents, 22,780 features (99.79% sparse) and 0 docvars

# Let me check the resultant dfm
library(quanteda.textstats)
word_freqs_of_DFM <-as.data.frame(quanteda.textstats::textstat_frequency(Patent_dfm_trim_2))
# Every thing seem OK.
Patent_dfm <- Patent_dfm_trim_2
rm(Patent_dfm_trim_2, Patent_dfm_trim_1)



# I will create "out" for STM
#> STM Vignette says that: "Note that when using quanteda dfm directly there may be higher memory use 
#> (because the texts and metadata are stored twice). You can convert from quanteda’s format directly 
#> to our native format using the quanteda function convert."
colnames(patents_cluster_in_text)
# [1] "key" "application_id" "year" "Country" "IPC" "title_corrected" "STM_text_Cluster"
Patent_docvars <- patents_cluster_in_text %>% select(key, year) # I will not use cluster because I inserted cluster into the text
#> You can convert from quanteda’s format directly to STM's native format using the quanteda function convert.
out <- convert(Patent_dfm, to = 'stm', docvars = Patent_docvars)
ls()
rm(tidy_Patent, single_characters, tokens, delete_tokens_list, Patent_dfm)

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_Model_March_2024_text_UPDATE_ready_to_STM.RData")
# rm(list = ls())

# NOW I WILL RUN FINAL STM for K=100 (I HOPE SO - 06/03/2024 21:54)
# load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_Model_March_2024_text_UPDATE_ready_to_STM.RData")

library(stm)
system.time({
  set.seed(2023)
  STM_patent_Final_V1 <- stm(documents= out$documents,
                          vocab= out$vocab,
                          K= 100,
                          prevalence = ~s(year),
                          data = out$meta,
                          init.type = "Spectral", # # NOT: Roberts (2014), "Spectral" is better.
                          max.em.its = 500,
                          seed = 2023)
}) # elapsed 5,5 hours 07/03/2024

BURADA_KALDIM <- "burada kaldim 06/03/2023"

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_Model_March_2024_UPDATE_V1.RData")

# load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_Model_March_2024_UPDATE_V1.RData") # To load STM model



#
####
######
########
########## # Unfortunately, There are additional Ngrams to replace.08/03/2024
########## # Additionally K=100 seems to be reduced 08/03/2024
########
######
####
#


load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_Model_March_2024_text_UPDATE_ready_to_STM.RData")

ls()
rm(out, Patent_docvars, word_freqs_of_DFM)


library(readxl)
list_6 <- read_excel("new n_grams after K100 new patents model_STEP6.xlsx", 
                     col_types = c("skip", "text", "text","skip"))



# Replace "oguzozbaydelete" with an empty space in the column list_5$to
list_6$to <- gsub("oguzozbaydelete", " ", list_6$to)
list_6$to <- list_6$to %>% stringr::str_squish()

list_6$from <- list_6$from %>% stringr::str_squish()
list_6$to <- list_6$to %>% stringr::str_squish()



# Apply the function to the $STM_text_Cluster column and get $New_NGrams 
# create a new column for corrections
patents_cluster_in_text$New_NGrams <- ""

# Apply the function to the $STM_text_Cluster column and get $New_NGrams 
system.time({
  patents_cluster_in_text$New_NGrams <- replace_words(patents_cluster_in_text$STM_text_Cluster,
                                                      list_6$from,
                                                      list_6$to)
  
  patents_cluster_in_text$New_NGrams <- patents_cluster_in_text$New_NGrams %>% stringr::str_squish()
  
  
  # STEP-2 I will run same code again
  patents_cluster_in_text$New_NGrams <- replace_words(patents_cluster_in_text$New_NGrams,
                                                      list_6$from,
                                                      list_6$to)
  
  patents_cluster_in_text$New_NGrams <- patents_cluster_in_text$New_NGrams %>% stringr::str_squish()
  
  
  # STEP-3 I will run same code again
  patents_cluster_in_text$New_NGrams <- replace_words(patents_cluster_in_text$New_NGrams,
                                                      list_6$from,
                                                      list_6$to)
  
  patents_cluster_in_text$New_NGrams <- patents_cluster_in_text$New_NGrams %>% stringr::str_squish()

  # save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_text_UPDATE_2.RData")
  
}) # elapsed 572 (08/03/2024)

patents_cluster_in_text$STM_text_Cluster <- patents_cluster_in_text$New_NGrams # In order to use previous code.
patents_cluster_in_text$New_NGrams <- NULL
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_text_UPDATE_2.RData")

# Delete all enviroment.
# rm(list = ls())

load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_text_UPDATE_2.RData")


# "robot_comprise	= robot BUNU AYRI YAP" ve dtm öncesi yap
patents_cluster_in_text$STM_text_Cluster <- gsub("\\brobot_comprise\\b", 
                                                 "robot", patents_cluster_in_text$STM_text_Cluster, perl = TRUE)

library(tidytext)
tidy_Patent <- patents_cluster_in_text %>% unnest_tokens(output= word, input= STM_text_Cluster, token = "words")
# tidy_Patent %>% count(word, sort = TRUE)

# create document feature matrix (library(tidytext))
Patent_dfm <- tidy_Patent %>% count(key, word) %>% cast_dfm(key, word, n) # cast_dfm(data, document, term, value, ...)
Patent_dfm # Document-feature matrix of: 342,565 documents, 52,663 features (99.91% sparse) and 0 docvars.

# I keep only words occurring at least 4 times and at least in 4 documents (library(quanteda))
library(quanteda)
Patent_dfm_trim_1 <- dfm_trim(Patent_dfm, min_termfreq = 4, min_docfreq = 4, termfreq_type = "count", docfreq_type = "count")
Patent_dfm_trim_1 # Document-feature matrix of: 342,565 documents, 22,853 features (99.79% sparse) and 0 docvars.

# Now I will define single character tokens (i.e. a, b, c, ... etc.)
# Then I will add these to my stop-word list (NOTE: stop-word has already been removed.
# "my stop-word" means here some additional stopwords. 
tokens <- tidy_Patent %>% count(word, sort = TRUE)
tokens$n_char <- 0
tokens$n_char <-  nchar(tokens$word)
single_characters <- tokens %>% filter(n_char == 1)
single_characters <- single_characters$word
delete_tokens_list <- single_characters

# Add additional stop_words to delete_tokens_list
delete_tokens_list <- c(single_characters, 
                        "aa", "bb", "cc", "dd", "ee", "ff", "gg", "hh", "ii", "jj", "kk", "ll", "mm", "ul0002",
                        "nn", "oo", "pp", "qq", "rr", "ss", "tt", "uu", "vv", "ww", "xx", "yy", "zz",
                        "can_be", "be_use_for", "plurality_of", "so_that", "which_be", "such_as", "sf", "li", "s0", 
                        "thereof", "still", "said", "whose", "whole","ul0002","constitution",
                        "theta1", "theta2","theta3","theta4", "theta5", "theta6", "hss", "th", "sub", 
                        "vii", "viii", "vi", "iv", "xi", "xii", "xiii","my1", "mx1", "fx1", "fy1", "fz1",
                        "fx2", "fy2", "fz2", "mx2", "my2", "mz2", "sub", "aforementione",
                        "accord_to", "utility_model", "device_comprise", "method_comprise", "system_comprise", 
                        "mechanism_comprise", "correspond_to", "equip_with") # NOTE: "constitution:" is a heading in patent abstracts.

# Removal of delete_tokens_list from dfm
Patent_dfm_trim_2 <- dfm_remove(Patent_dfm_trim_1, delete_tokens_list, verbose = TRUE) # removed 76 features
Patent_dfm_trim_2 # Document-feature matrix of: 342,565 documents, 22,777 features (99.80% sparse) and 0 docvars.

# Let me check the resultant dfm
library(quanteda.textstats)
word_freqs_of_DFM <-as.data.frame(quanteda.textstats::textstat_frequency(Patent_dfm_trim_2))
# Every thing seem OK.
Patent_dfm <- Patent_dfm_trim_2
rm(Patent_dfm_trim_2, Patent_dfm_trim_1)



# I will create "out" for STM
#> STM Vignette says that: "Note that when using quanteda dfm directly there may be higher memory use 
#> (because the texts and metadata are stored twice). You can convert from quanteda’s format directly 
#> to our native format using the quanteda function convert."
colnames(patents_cluster_in_text)
# [1] "key" "application_id" "year" "Country" "IPC" "title_corrected" "STM_text_Cluster"
Patent_docvars <- patents_cluster_in_text %>% select(key, year) # I will not use cluster because I inserted cluster into the text
#> You can convert from quanteda’s format directly to STM's native format using the quanteda function convert.
out <- convert(Patent_dfm, to = 'stm', docvars = Patent_docvars)
ls()
rm(tidy_Patent, single_characters, tokens, delete_tokens_list, Patent_dfm)


# NOW I WILL RUN FINAL STM for different K values (08/03/2024 13:37)
library(stm)
system.time({
  set.seed(2023)
  STM_patent_K100 <- stm(documents= out$documents,
                             vocab= out$vocab,
                             K= 100,
                             prevalence = ~s(year),
                             data = out$meta,
                             init.type = "Spectral", # # NOT: Roberts (2014), "Spectral" is better.
                             max.em.its = 500,
                             seed = 2023)
}) # elapsed ??? 08/03/2024

BURADA_KALDIM_1 <- "burada kaldim 08/03/2023"
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_08_03_2024_V1.RData")

library(stm)
system.time({
  set.seed(2023)
  STM_patent_K96 <- stm(documents= out$documents,
                         vocab= out$vocab,
                         K= 96,
                         prevalence = ~s(year),
                         data = out$meta,
                         init.type = "Spectral", # # NOT: Roberts (2014), "Spectral" is better.
                         max.em.its = 500,
                         seed = 2023)
}) # elapsed 6 hours 08/03/2024

BURADA_KALDIM_2 <- "burada kaldim 08/03/2023"
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_08_03_2024_V2.RData")


library(stm)
system.time({
  set.seed(2023)
  STM_patent_K92 <- stm(documents= out$documents,
                         vocab= out$vocab,
                         K= 92,
                         prevalence = ~s(year),
                         data = out$meta,
                         init.type = "Spectral", # # NOT: Roberts (2014), "Spectral" is better.
                         max.em.its = 500,
                         seed = 2023)
}) # elapsed 4 hours 08/03/2024

BURADA_KALDIM_3 <- "burada kaldim 08/03/2023"
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_08_03_2024_V3.RData")



library(stm)
system.time({
  set.seed(2023)
  STM_patent_K90 <- stm(documents= out$documents,
                        vocab= out$vocab,
                        K= 90,
                        prevalence = ~s(year),
                        data = out$meta,
                        init.type = "Spectral", # # NOT: Roberts (2014), "Spectral" is better.
                        max.em.its = 500,
                        seed = 2023)
}) # elapsed 4,2 hours 12/03/2024

BURADA_KALDIM_4 <- "burada kaldim 08/03/2023"
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_08_03_2024_V4.RData")


### I will continue for more K values
# NOW I WILL RUN FINAL STM for different K values (12/03/2024 16:28)
library(stm)
system.time({
  set.seed(2023)
  STM_patent_K97 <- stm(documents= out$documents,
                         vocab= out$vocab,
                         K= 97,
                         prevalence = ~s(year),
                         data = out$meta,
                         init.type = "Spectral", # # NOT: Roberts (2014), "Spectral" is better.
                         max.em.its = 500,
                         seed = 2023)
}) # elapsed 18000 13/03/2024

BURADA_KALDIM_5 <- "burada kaldim 08/03/2023"
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_08_03_2024_V5.RData")



library(stm)
system.time({
  set.seed(2023)
  STM_patent_K98 <- stm(documents= out$documents,
                        vocab= out$vocab,
                        K= 98,
                        prevalence = ~s(year),
                        data = out$meta,
                        init.type = "Spectral", # # NOT: Roberts (2014), "Spectral" is better.
                        max.em.its = 500,
                        seed = 2023)
}) # elapsed 17000 13/03/2024

BURADA_KALDIM_6 <- "burada kaldim 08/03/2023"
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_08_03_2024_V6.RData")



library(stm)
system.time({
  set.seed(2023)
  STM_patent_K99 <- stm(documents= out$documents,
                        vocab= out$vocab,
                        K= 99,
                        prevalence = ~s(year),
                        data = out$meta,
                        init.type = "Spectral", # # NOT: Roberts (2014), "Spectral" is better.
                        max.em.its = 500,
                        seed = 2023)
}) # elapsed 5.2 hours08/03/2024

BURADA_KALDIM_7 <- "burada kaldim 08/03/2023"
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_08_03_2024_V7.RData")
load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_08_03_2024_V7.RData")


# Now I will review topic words and select the better STM

ls()


labelTopics(STM_patent_K100, topics=NULL, n=8)
labelTopics(STM_patent_K96, topics=NULL, n=8)
labelTopics(STM_patent_K92, topics=NULL, n=8)
labelTopics(STM_patent_K90, topics=NULL, n=8)
labelTopics(STM_patent_K97, topics=NULL, n=8)
labelTopics(STM_patent_K98, topics=NULL, n=8)
labelTopics(STM_patent_K99, topics=NULL, n=8)

# I copied and pasted each output (i.e. labelTopics(...) result) in to Notepad++, the I copied and pasted on to excel (By doing it this way, it is possible to paste it into Excel.).
# I compared topic word an I selected K98 as the final STM

labelTopics(STM_patent_K98, topics=NULL, n=8)
