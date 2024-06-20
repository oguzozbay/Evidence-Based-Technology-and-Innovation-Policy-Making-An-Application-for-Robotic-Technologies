
# This code is extracted from below file.
# C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM K_94_100_104_new_NGrams_STEP3_01_03_2024.R
# In the above file lots of STM trials are conducted (with n-gram updates, i.e. adding new n-grams after reviewing topic words after each STM).
# This code is the final run




setwd("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024")
library(dplyr)
library(quanteda)
library(quanteda.textstats)
library(stm)
library(stringr)
library(stringi)
library(tidytext)



load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_text_UPDATE_2.RData") # Ready to use data for STM model

# A simple text correction step which I did before Document Term Matix (dtm)
patents_cluster_in_text$STM_text_Cluster <- gsub("\\brobot_comprise\\b", "robot",
                                                 patents_cluster_in_text$STM_text_Cluster, perl = TRUE)

# Now I will create dtm
library(tidytext)
tidy_Patent <- patents_cluster_in_text %>% unnest_tokens(output= word, input= STM_text_Cluster, token = "words")
# tidy_Patent %>% count(word, sort = TRUE)

# create document feature matrix (library(tidytext))
Patent_dfm <- tidy_Patent %>% count(key, word) %>% cast_dfm(key, word, n) # cast_dfm(data, document, term, value, ...)
Patent_dfm # Document-feature matrix of: 342,565 documents, 52,663 features (99.91% sparse) and 0 docvars.

# I will keep only words occurring at least 4 times and at least in 4 documents (library(quanteda))
library(quanteda)
Patent_dfm_trim_1 <- dfm_trim(Patent_dfm, min_termfreq = 4, min_docfreq = 4, termfreq_type = "count", docfreq_type = "count")
Patent_dfm_trim_1 # Document-feature matrix of: 342,565 documents, 22,853 features (99.79% sparse) and 0 docvars.

# Now I will define and check single character tokens (i.e. a, b, c, ... etc.)
# Then I will add these tokens into my stop-word list (NOTE: common stop-words has already been removed from the text)
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
# NOTE: I run various STM with different K values. In this code I give only the final version (i.e. K=98)
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

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_08_03_2024_V6.RData")

# I skipped other STM trials
# ...
# I saved all STM model into below file. 
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_08_03_2024_V7.RData")

# Now I will load STM models file and then save final STM model into a new file.
# load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_08_03_2024_V7.RData")
# I deleted all environment except for below
# "Patent_docvars", "STM_patent_K98" "out", "patents_cluster_in_text", "read_me_UPDATED_5_patents_cluster_in_text"

# Below is the file for final STM model (STM_patent_K98) 14/03/2024
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_Code_K98_for_thesis_14_03_2024.RData")

load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent_STM_Final_Code_K98_for_thesis_14_03_2024.RData")

# In order to see topic words:
labelTopics(STM_patent_K98, topics=NULL, n=8)



















