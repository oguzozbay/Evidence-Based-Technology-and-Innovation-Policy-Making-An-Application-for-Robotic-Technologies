# Notes for thesis (02/04/2024) START
# I am making nGram replacement as explained below.
# After this process is completed, I will extract Ngram again.
# This code also includes combining words such as "this paper" etc. with "multi rotor".
# Notes for thesis (02/04/2024) END

library(dplyr)
library(stringi)
library(stringr)


# First, I upload the following:   load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_4_5_STM_NGram_replaced_topics.RData")
# I get the original text form the above environment, then I deleted all environment except for M_final_STM 
# colnames(M_final_STM)
# M_final_STM <- M_final_STM %>% select (UT, PY_corrected, key_TI_AB )



# Now it is enough to upload the following. I saved the following for this purpose.
load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_2_0_data_preperation_Ngram_replacement_1_CORRECTION.RData")
# In summary, it is enough to start from loop 1 below.
# N_gram_list_1, N_gram_list_2 and N_gram_list_3 are currently available in Environment.
# Likewise, Old, New, Old_2, New_2 and Old_3 and New_3 are also available in the Environment.

N_gram_list_1 #  I had imported the excel below. It's already available in Environment.
# library(readxl)
# N_gram_list_1 <- read_excel("N_Grams_separated words extracted excels Oct 2022/1_N_Ggrams_final_multi_over_pre_etc_all_together_26_oct.xlsx", 
#                            sheet = "all_united_123_final_in_order", 
#                            col_types = c("skip", "skip", "text", 
#                                          "text", "skip", "skip"))

N_gram_list_1$from <- N_gram_list_1$from %>% stringr::str_squish()
N_gram_list_1$to <- N_gram_list_1$to %>% stringr::str_squish()



N_gram_list_2  #  I had imported the excel below. It's already available in Environment.
# library(readxl)
# N_gram_list_2 <- read_excel("word_pairs_step2_text2vec_Ngrams - unite_1.xlsx", 
#                            sheet = "step2_unite_Ngram_1", col_types = c("skip", 
#                                                                         "text", "text", "skip"))
N_gram_list_2$from <- N_gram_list_2$from %>% stringr::str_squish()
N_gram_list_2$to <- N_gram_list_2$to %>% stringr::str_squish()



N_gram_list_3  #  I had imported the excel below. It's already available in Environment.
# library(readxl)
# N_gram_list_3 <- read_excel("word_pairs_step3_text2vec_Ngrams_unite_1_HYPEN_in_from_column.xlsx", 
#                            sheet = "NGram_replace_list_16_Nov_2022", 
#                            col_types = c("skip", "skip", "skip", 
#                                          "skip", "text", "text", "skip", "skip"))
N_gram_list_3$from <- N_gram_list_3$from %>% stringr::str_squish()
N_gram_list_3$to <- N_gram_list_3$to %>% stringr::str_squish()


# I have imported 3 separate replacement lists above. 
# One is spelling errors, incorrect lemmatizations, "multi etc." combinations.
# The second is the concatenation of n-grams such as "this_paper" etc.
# The third is again terms like this_paper etc. (I replaced terms like this_paper etc. in two stages: First, I identified and changed some of them, then I re-extracted n-Grams and replaced new determined ones.
# And I'm also replacing the n-grams I determined in the third round. (I determined these using text2vec after loop 2, i.e. after changing N_gram_list_2).
# Now replace the following lists (i.e. N_gram_list_1, N_gram_list_2 and N_gram_list_3)

Old <- N_gram_list_1$from
New <- N_gram_list_1$to

Old_2 <- N_gram_list_2$from
New_2 <- N_gram_list_2$to

Old_3 <- N_gram_list_3$from
New_3 <- N_gram_list_3$to

# I am saving the environment. There is only the original text and replacement lists:
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_2_0_data_preperation_Ngram_replacement_1_CORRECTION.RData")


# loop 1.1 ----
M_final_STM$key_TI_AB_ngram_1 <- "" 
system.time({
  M_final_STM$key_TI_AB_ngram_1 <- stri_replace_all_regex(M_final_STM$key_TI_AB,
                                                                       paste0("\\b", Old, "\\b"), 
                                                                       paste0(" ", New, " "), vectorize_all = FALSE)
}) # elapsed: 

M_final_STM$key_TI_AB_ngram_1 <- gsub(pattern= "OGUZOZBAYDELETE", replacement = " ", M_final_STM$key_TI_AB_ngram_1)
M_final_STM$key_TI_AB_ngram_1 <- str_squish(M_final_STM$key_TI_AB_ngram_1)
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_2_0_data_preperation_Ngram_replacement_1_CORRECTION_1.RData")


# loop 1.2
M_final_STM$key_TI_AB_ngram_2 <- ""
system.time({
  M_final_STM$key_TI_AB_ngram_2 <- stri_replace_all_regex(M_final_STM$key_TI_AB_ngram_1,
                                                          paste0("\\b", Old, "\\b"), 
                                                          paste0(" ", New, " "), vectorize_all = FALSE)
}) # elapsed


M_final_STM$key_TI_AB_ngram_2 <- str_squish(M_final_STM$key_TI_AB_ngram_2)
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_2_0_data_preperation_Ngram_replacement_1_CORRECTION_2.RData")


# loop 1.3
M_final_STM$key_TI_AB_ngram_3 <- ""
system.time({
  M_final_STM$key_TI_AB_ngram_3 <- stri_replace_all_regex(M_final_STM$key_TI_AB_ngram_2,
                                                          paste0("\\b", Old, "\\b"), 
                                                          paste0(" ", New, " "), vectorize_all = FALSE)
}) # elapsed 

M_final_STM$key_TI_AB_ngram_3 <- str_squish(M_final_STM$key_TI_AB_ngram_3)
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_2_0_data_preperation_Ngram_replacement_1_CORRECTION_3.RData")


# Second loop ---
# loop 2.1
M_final_STM$key_TI_AB_ngram_4 <- "" 
system.time({
  M_final_STM$key_TI_AB_ngram_4 <- stri_replace_all_regex(M_final_STM$key_TI_AB_ngram_3,
                                                          paste0("\\b", Old_2, "\\b"), 
                                                          paste0(" ", New_2, " "), vectorize_all = FALSE)
}) # elapsed

M_final_STM$key_TI_AB_ngram_4 <- gsub(pattern= "OGUZOZBAYDELETE", replacement = " ", M_final_STM$key_TI_AB_ngram_4)
M_final_STM$key_TI_AB_ngram_4 <- str_squish(M_final_STM$key_TI_AB_ngram_4)
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_2_0_data_preperation_Ngram_replacement_1_CORRECTION_4.RData")

# loop 2.2
M_final_STM$key_TI_AB_ngram_5 <- ""
system.time({
  M_final_STM$key_TI_AB_ngram_5 <- stri_replace_all_regex(M_final_STM$key_TI_AB_ngram_4,
                                                          paste0("\\b", Old_2, "\\b"), 
                                                          paste0(" ", New_2, " "), vectorize_all = FALSE)
}) # elapsed

M_final_STM$key_TI_AB_ngram_5 <- gsub(pattern= "OGUZOZBAYDELETE", replacement = " ", M_final_STM$key_TI_AB_ngram_5)
M_final_STM$key_TI_AB_ngram_5 <- str_squish(M_final_STM$key_TI_AB_ngram_5)
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_2_0_data_preperation_Ngram_replacement_1_CORRECTION_5.RData")


# loop 2.3
M_final_STM$key_TI_AB_ngram_6 <- ""
system.time({
  M_final_STM$key_TI_AB_ngram_6 <- stri_replace_all_regex(M_final_STM$key_TI_AB_ngram_5,
                                                          paste0("\\b", Old_2, "\\b"), 
                                                          paste0(" ", New_2, " "), vectorize_all = FALSE)
}) # elapsed 
M_final_STM$key_TI_AB_ngram_6 <- gsub(pattern= "OGUZOZBAYDELETE", replacement = " ", M_final_STM$key_TI_AB_ngram_6)
M_final_STM$key_TI_AB_ngram_6 <- str_squish(M_final_STM$key_TI_AB_ngram_6)
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_2_0_data_preperation_Ngram_replacement_1_CORRECTION_6.RData")



# Üçüncü çevrim------------------
# loop 3.1
M_final_STM$key_TI_AB_ngram_7 <- "" 
system.time({
  M_final_STM$key_TI_AB_ngram_7 <- stri_replace_all_regex(M_final_STM$key_TI_AB_ngram_6,
                                                          paste0("\\b", Old_3, "\\b"), 
                                                          paste0(" ", New_3, " "), vectorize_all = FALSE)
}) # elapsed
M_final_STM$key_TI_AB_ngram_7 <- gsub(pattern= "OGUZOZBAYDELETE", replacement = " ", M_final_STM$key_TI_AB_ngram_7)
M_final_STM$key_TI_AB_ngram_7 <- str_squish(M_final_STM$key_TI_AB_ngram_7)
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_2_0_data_preperation_Ngram_replacement_1_CORRECTION_7.RData")


# loop 3.2
M_final_STM$key_TI_AB_ngram_8 <- ""
system.time({
  M_final_STM$key_TI_AB_ngram_8 <- stri_replace_all_regex(M_final_STM$key_TI_AB_ngram_7,
                                                          paste0("\\b", Old_3, "\\b"), 
                                                          paste0(" ", New_3, " "), vectorize_all = FALSE)
}) # elapsed

M_final_STM$key_TI_AB_ngram_8 <- gsub(pattern= "OGUZOZBAYDELETE", replacement = " ", M_final_STM$key_TI_AB_ngram_8)
M_final_STM$key_TI_AB_ngram_8 <- str_squish(M_final_STM$key_TI_AB_ngram_8)
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_2_0_data_preperation_Ngram_replacement_1_CORRECTION_8.RData")


# loop 3.3
M_final_STM$key_TI_AB_ngram_9 <- ""
system.time({
  M_final_STM$key_TI_AB_ngram_9 <- stri_replace_all_regex(M_final_STM$key_TI_AB_ngram_8,
                                                          paste0("\\b", Old_3, "\\b"), 
                                                          paste0(" ", New_3, " "), vectorize_all = FALSE)
}) # elapsed 

M_final_STM$key_TI_AB_ngram_9 <- gsub(pattern= "OGUZOZBAYDELETE", replacement = " ", M_final_STM$key_TI_AB_ngram_9)
M_final_STM$key_TI_AB_ngram_9 <- str_squish(M_final_STM$key_TI_AB_ngram_9)
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_2_0_data_preperation_Ngram_replacement_1_CORRECTION_9.RData")


#### Replacement Check

colnames(M_final_STM)
# Now I will check the replacements
M_final_STM$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                      strsplit(M_final_STM$key_TI_AB_ngram_9, '\\s'), strsplit(M_final_STM$key_TI_AB_ngram_6, '\\s'))

M_final_STM$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                      strsplit(M_final_STM$key_TI_AB_ngram_6, '\\s'), strsplit(M_final_STM$key_TI_AB_ngram_3, '\\s'))

M_final_STM$replace_check_3 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                      strsplit(M_final_STM$key_TI_AB_ngram_3, '\\s'), strsplit(M_final_STM$key_TI_AB, '\\s'))


M_final_STM$replace_check_4 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                      strsplit(M_final_STM$key_TI_AB_ngram_9, '\\s'), strsplit(M_final_STM$key_TI_AB, '\\s'))

M_final_STM$replace_check_1 <- NULL
M_final_STM$replace_check_2 <- NULL
M_final_STM$replace_check_3 <- NULL






# Now I will extract ngram again.
# currently stop words are not removed
# collocation analysis (n-grams)
library(text2vec)
library(dplyr)

sample_ind = 1:211585
tokens = word_tokenizer(M_final_STM$key_TI_AB_ngram_9[sample_ind]) # tokenized 211585 rows
it = itoken(tokens, ids = M_final_STM$UT[sample_ind]) # it= An input itoken or itoken_parallel iterator
system.time(v <- create_vocabulary(it))
v = prune_vocabulary(v, term_count_min = 10) # throws out very frequent and very infrequent terms.

# Attention!...: When pmi_min = 5, it does not capture the "human robot" in below code
model = Collocations$new(collocation_count_min = 15, pmi_min = 5, sep = "=") # model= A Collocation model object (orijinal 5 ve 5 idi)

# model = Collocations$new(vocabulary = NULL, collocation_count_min = 50, pmi_min = 5, gensim_min = 0, lfmd_min = -Inf, llr_min = 0, sep = "_") # help page definiton
model$fit(it, n_iter = 100)
model$collocation_stat #

it2 = model$transform(it)
v2 = create_vocabulary(it2)
v2 = prune_vocabulary(v2, term_count_min = 5) # In v2 there are single words and underscores
# check what phrases model has learned
setdiff(v2$term, v$term) #  setdiff= which elements of a vector or data frame X are not existent in a vector or data frame Y.
v2 <- within(v2,{underline = as.numeric(grepl("=", fixed = FALSE, v2$term))}) # I wrote how many "=" exist in the column. If it is equal to 0, it is a single word.
word_pairs <- v2 %>% filter(underline == 1)

# openxlsx::write.xlsx(x = word_pairs, file = "word_pairs_step4.xlsx")
# openxlsx::write.xlsx(x = v2, file = "all_words_and_word_pairs_step4.xlsx"


rm(tokens) 
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/M_ngram_step3_after_correction.RData")
