
# I will lemmatize the reference titles that I corrected.
# After correcting the Lemmas, I will find the Ngrams (maybe not). I gave up on NGram (yes, I gave up later because it takes too much time)

library(dplyr)
library(spacyr)

setwd("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited")
# Import final version of corrected cited titles
# load("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/WOS cited titles corrected final 20_09_2023.RData")


# Singularize cited titles
colnames(WOS_Cited_tit_20_09_2023) # [1] "wos_id" "full_original" "correction_to_full_list_STEP3"
titles_singular <- WOS_Cited_tit_20_09_2023 %>% distinct(correction_to_full_list_STEP3, .keep_all = TRUE) # I will remove duplicated titles for data reduction.
titles_singular <- titles_singular %>% select (correction_to_full_list_STEP3) # select only corrected text

rm(WOS_Cited_tit_20_09_2023) # I delete for memory usage reduction.
# save.image("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/cited titles lemmatization and Ngrams 20_09_2023_ENVIRONMENT.RData")

# Now I will lemmatize and then correct necessary lemmas.
library(spacyr)
spacy_install()
spacy_initialize()

# Will just tokenize so lemma= FALSE
system.time({
  titles_lemma <- spacy_parse(titles_singular$correction_to_full_list_STEP3,
                              multithread = TRUE, 
                              nounphrase = FALSE, 
                              lemma= TRUE)
}) # elapsed 2166 20/09/2023
spacy_finalize()
# save.image("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/cited titles lemmatization and Ngrams 20_09_2023_ENVIRONMENT.RData")

# load("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/cited titles lemmatization and Ngrams 20_09_2023_ENVIRONMENT.RData")
# load("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/WOS cited titles corrected final 20_09_2023.RData")

# Group by token and calculate frequency
lemma_freq_df <- titles_lemma %>%
  group_by(token, lemma) %>%
  tally(sort = TRUE)
# openxlsx::write.xlsx(x = lemma_freq_df, file = "lemma_versus_token_of_cited_titles.xlsx")


# I will import manually corrected lemmas (a 32858 row of excel data for token -lemma correction)
cited_ref_lemma_replace_list <- read_excel("lemma_versus_token_of_cited_titles_final.xlsx",
                                           sheet = "corrected_tokens", col_types = c("skip", "text", "text", "text", "skip", "skip"))


titles_lemma <- as.data.frame(titles_lemma)
cited_ref_lemma_replace_list <- as.data.frame(cited_ref_lemma_replace_list)

# Checking lemma replace list which was prepared manually
# Group by token and calculate frequency
# sil <- cited_ref_lemma_replace_list
# sil <- sil %>%
#   group_by(token, lemma_original) %>%
#  tally(sort = TRUE)
# rm(sil)


# STEP-2 Replacement of corrected tokens according to excel data STEP-2 ----------
# I will create a new column in the titles_lemma data frame, named corrected_version.
# If "titles_lemma$token" and "titles_lemma$lemma"  match the "cited_ref_lemma_replace_list$token" and "cited_ref_lemma_replace_list$lemma_original", 
# Then I will replace the corresponding value in the "titles_lemma$corrected_version" with the "cited_ref_lemma_replace_list$lemma_corrected_excel" value

# 1. Create the corrected_version column
titles_lemma$corrected_version <- titles_lemma$lemma

# 2. Update the corrected_version values based on the conditions
titles_lemma <- titles_lemma %>%
  left_join(cited_ref_lemma_replace_list, by = c("token", "lemma" = "lemma_original")) %>%
  mutate(corrected_version = ifelse(is.na(lemma_corrected_excel), corrected_version, lemma_corrected_excel))
# If lemma_corrected is NA (which means there was no match in cited_ref_lemma_replace_list), keep the original value of corrected_version.
# If lemma_corrected is not NA (indicating a match), replace the value in corrected_version with the value from lemma_corrected_excel

titles_lemma <- titles_lemma %>% select(-lemma_corrected_excel)


# Prepare data for untidy lemmatized text -----
untidy_lemma <- titles_lemma %>% select(doc_id, corrected_version)
untidy_lemma$row_no <- untidy_lemma[,1]
untidy_lemma <- untidy_lemma[, c(1,3,2)] # moving last column after 1st column
untidy_lemma[,2] <- gsub(pattern= "text", replacement = "", untidy_lemma[,2])
untidy_lemma[,2] <- as.numeric(untidy_lemma[,2])
untidy_lemma$doc_id <- NULL
untidy_lemma <- as.data.frame(untidy_lemma)
# Group by and summarizse
column_index <- 1 # i.e. row_no column
untidy_X <- untidy_lemma %>%
  group_by(untidy_lemma[[column_index]]) %>% 
  summarise(united_text = paste0(corrected_version, collapse = ' '))
names(untidy_X) <- c("row_no", "untidy_corrected_lemma")

# Check the result
untidy_X$original <- titles_singular$correction_to_full_list_STEP3
untidy_X <- untidy_X[, c(1,3,2)] # reordering columns
rm(untidy_lemma)
untidy_cited_ref_lemma <- untidy_X
rm(untidy_X)


# STEP-3 I took some notes when making correction in excel now I will corrected those in Notepad++
# Transferring lemmatized text to Notepad++ 
# write.table(untidy_cited_ref_lemma, file="1_7_milyon_lemmatized.txt", row.names=FALSE, col.names=FALSE, quote=FALSE, sep="\t")
# I corrected above txt file (ie. the corrected lemmatized titles) manually according to notes taken during lemmatize correction excel process.

# Importing manually corrected in notepad++ text in to R -----------

# When I making corrections I made manual NGram replacements (total 31 Ng.
# Now I changed my mind. I will not Use NGrams.
# In below code I extracted those NGrams, then I will replace _ (underscores) to space in Notepad++
delete_under_score <- titles_lemma
manual_Ngrams <- titles_lemma %>%
  group_by(token, corrected_version) %>%
  tally(sort = TRUE)
manual_Ngrams <- manual_Ngrams %>%
  filter(grepl("_", corrected_version))
manual_Ngrams <- manual_Ngrams %>% filter(n>2)
rm(delete_under_score)
# openxlsx::write.xlsx(x = manual_Ngrams, file = "manual_Ngrams_of_cited_titles.xlsx")
# Using above excel I replaced Ngrams to original forms. Note that deg_of_freedom replaced to "degree of freedom"
# And I saved it as "1_7_milyon_lemmatized_corrected_manually_UPDATED.txt"


manually_corrected_lemmatize_text <- read.table("1_7_milyon_lemmatized_corrected_manually_UPDATED.txt", header=FALSE, stringsAsFactors=FALSE, sep="\t") # düzeltiğim metin
# NOTE: 1_7_milyon_lemmatized_corrected_manually_UPDATED_2_dof.txt is the updated verion of above file.
manually_corrected_lemmatize_text$V1 <- NULL
untidy_cited_ref_lemma$corrected_lemmatized_26_sep <- manually_corrected_lemmatize_text$V2
rm(manually_corrected_lemmatize_text)


Notepad_corrected_lemma_26_sep <- untidy_cited_ref_lemma # renaming for prevent confusion
rm(untidy_cited_ref_lemma)

colnames(Notepad_corrected_lemma_26_sep) # [1] "row_no" "original" "untidy_corrected_lemma" "corrected_lemmatized_26_sep"
# original ==
colnames(WOS_Cited_tit_20_09_2023) # [1] "wos_id" "full_original" "correction_to_full_list_STEP3"

# NOTE: Notepad_corrected_lemma_26_sep$original == WOS_Cited_tit_20_09_2023$correction_to_full_list_STEP3
# I will check it to be sure:
A <- Notepad_corrected_lemma_26_sep %>% select(original)
B <- WOS_Cited_tit_20_09_2023 %>% select (correction_to_full_list_STEP3)
B <- B %>% distinct(correction_to_full_list_STEP3, .keep_all = TRUE)
compare_A_B <- A
compare_A_B$correction_to_full_list_STEP3 <- B$correction_to_full_list_STEP3

all_same <- all(compare_A_B$original == compare_A_B$correction_to_full_list_STEP3)
all_same # [1] TRUE
rm(A, B, compare_A_B, all_same)


##################### CHECING NOTEPAD MANUAL CORRECTIONS - START ####################################################
sil <- Notepad_corrected_lemma_26_sep %>% select (untidy_corrected_lemma, corrected_lemmatized_26_sep)
# Comparing original and replaced text
sil$check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                      strsplit(sil$untidy_corrected_lemma, '\\s'), strsplit(sil$corrected_lemmatized_26_sep, '\\s'))
sil$check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                      strsplit(sil$corrected_lemmatized_26_sep, '\\s'), strsplit(sil$untidy_corrected_lemma, '\\s'))
sil_filtered <- sil %>% filter(check_1 != "" | check_2 != "")
rm(sil, sil_filtered)
##################### CHECING NOTEPAD MANUAL CORRECTIONS - END ####################################################



# I will delete unnecessary files:
# Notepad_corrected_lemma_26_sep = mostly required
# WOS_Cited_tit_20_09_2023 = mostly required
ls() 
# "cited_ref_lemma_replace_list" "column_index" "manual_Ngrams" 
# "Notepad_corrected_lemma_26_sep" "sil_filtered" "WOS_Cited_tit_20_09_2023" 
# save.image("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/WOS cited titles corrected lemmatize final 26_09_2023.RData")



# STEP-4 Inserting corrections in WOS data (replacing corrected cited titles with old WOS titles)
deneme_5_million <- WOS_Cited_tit_20_09_2023 # 5 million titles

colnames (Notepad_corrected_lemma_26_sep) # [1] "row_no" "original" "untidy_corrected_lemma" "corrected_lemmatized_26_sep"
# $untidy_corrected_lemma = excel lemma düzeltme listesi uygulanmış hali
# $corrected_lemmatized_26_sep ise untidy_corrected_lemma'nın notepad ile manuel düzeletilmiş hali
# $original ise "WOS_Cited_tit_20_09_2023$correction_to_full_list_STEP3" ile eşleşen sütun

correction_titles_1_7_million <- Notepad_corrected_lemma_26_sep %>% select (original, corrected_lemmatized_26_sep) 
# correction_titles_1_7_million = 1,7 million lemmatized titles which are corrected manually in Notepad++
colnames(correction_titles_1_7_million) <- c("correction_to_full_list_STEP3", "corrected_lemmatized_26_sep")
# I rename "original" to its real name to prevent confusion ("original" matches with WOS_Cited_tit_20_09_2023$correction_to_full_list_STEP3 )
# So for prevent confusion I renamed "original" to "correction_to_full_list_STEP3")

colnames(deneme_5_million) # [1] "wos_id" "full_original" "correction_to_full_list_STEP3" 
colnames(correction_titles_1_7_million) # [1] "original" "corrected_lemmatized_26_sep"

correction_titles_1_7_million$corrected_lemmatized_26_sep <- tolower(correction_titles_1_7_million$corrected_lemmatized_26_sep)
correction_titles_1_7_million$corrected_lemmatized_26_sep <- correction_titles_1_7_million$corrected_lemmatized_26_sep %>% stringr::str_squish()


# Left join on correction_to_full_list_STEP3 column
deneme_5_million <- left_join(deneme_5_million, 
                              correction_titles_1_7_million, 
                              by = "correction_to_full_list_STEP3")

# Fill in the new column based on the conditions
deneme_5_million$final_text <- ifelse(
  is.na(deneme_5_million$corrected_lemmatized_26_sep), 
  "OGUZ_OZBAY_CHECK_THIS_TITLE", # in order to check if some titles corrected wrong!
  deneme_5_million$corrected_lemmatized_26_sep)
# There is no "OGUZ_OZBAY_CHECK_THIS_TITLE", so the replacement is ok.

all_same_2 <- all(deneme_5_million$corrected_lemmatized_26_sep == deneme_5_million$final_text)
all_same_2 # [1] TRUE


# Drop the final_text column because it is equal to corrected_lemmatized_26_sep
# This means that I did not make any error when preparing replacement list.
deneme_5_million <- select(deneme_5_million, -final_text)

# Removal of OGUZOZBAYDELETE's
deneme_5_million$corrected_lemmatized_26_sep <- gsub("oguzozbaydelete", " ", deneme_5_million$corrected_lemmatized_26_sep)
deneme_5_million$corrected_lemmatized_26_sep <- deneme_5_million$corrected_lemmatized_26_sep %>% stringr::str_squish()
colnames(deneme_5_million)
# "wos_id" "full_original" "correction_to_full_list_STEP3" "corrected_lemmatized_26_sep" 
colnames(deneme_5_million) <- c("wos_id", "full_original", "tokens_corrected", "lemma_corrected_26_Sep") 
colnames(deneme_5_million)
# "wos_id"                 "full_original"          "tokens_corrected"       "lemma_corrected_26_Sep"



# To see empty or NA or only whitespace rows
filtered_data <- deneme_5_million %>% 
  filter(is.na(lemma_corrected_26_Sep) | 
           lemma_corrected_26_Sep == "" | 
           grepl("^\\s*$", lemma_corrected_26_Sep))

# Removal of empty or NA or only whitespace rows
dim(deneme_5_million) # 5090002
deneme_5_million <- deneme_5_million %>% 
  filter(!(is.na(lemma_corrected_26_Sep) | 
             lemma_corrected_26_Sep == "" | 
             grepl("^\\s*$", lemma_corrected_26_Sep)))
dim(deneme_5_million) # 5090000




############################################
############################################
# checking replacements
colnames(deneme_5_million) # [1] "wos_id" "full_original" "tokens_corrected" "lemma_corrected_26_Sep"
sil <- deneme_5_million %>% select (tokens_corrected, lemma_corrected_26_Sep)

# Comparing original and replaced text
sil$check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(sil$tokens_corrected, '\\s'), strsplit(sil$lemma_corrected_26_Sep, '\\s'))
sil$check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(sil$lemma_corrected_26_Sep, '\\s'), strsplit(sil$tokens_corrected, '\\s'))
sil_filtered <- sil %>% filter(check_1 != "" | check_2 != "")
############################################
############################################

colnames(deneme_5_million)
# [1] "wos_id"                 "full_original"          "tokens_corrected"       "lemma_corrected_26_Sep"
colnames(deneme_5_million) <- c("wos_id", "semi_original", "tokens_corrected", "lemma_corrected")

WOS_Cited_Titles_Final <- deneme_5_million
rm(deneme_5_million)
rm(WOS_Cited_tit_20_09_2023)
rm(correction_titles_1_7_million)
ls()

#> "cited_ref_lemma_replace_list" = excel data for lemma correction
#> "Notepad_corrected_lemma_26_sep" = I made an final correction in Notepad++ manually after applying above excel replacements.
#> "manual_Ngrams" = 33 Ngrams which I converted to regular words. I did this in "Notepad_corrected_lemma_26_sep" text
#> "WOS_Cited_Titles_Final" = Final version of cited titles references.



gc()

# I made some additional corrections which are given below
# non linear => nonlinear
# neurofuzzy => neurofuzzy
# degree of freedom => dof


WOS_Cited_Titles_Final$lemma_corrected <- gsub("degree of freedom", 
                                               "dof", 
                                               WOS_Cited_Titles_Final$lemma_corrected, 
                                               fixed = TRUE)

WOS_Cited_Titles_Final$tokens_corrected <- gsub("degree of freedom", 
                                                "dof", 
                                                WOS_Cited_Titles_Final$tokens_corrected, 
                                                fixed = TRUE)

WOS_Cited_Titles_Final$lemma_corrected <- gsub("\\bfpgas\\b", 
                                               "fpga", 
                                               WOS_Cited_Titles_Final$lemma_corrected)

WOS_Cited_Titles_Final$tokens_corrected <- gsub("\\bfpgas\\b", 
                                                "fpga", 
                                                WOS_Cited_Titles_Final$tokens_corrected)


# save.image("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/WOS_Cited_Titles_Final_27_09_2023.RData")
# I take WOS_Cited_Titles_Final$lemma_corrected and separate some words starting with "micro", "multi", "bio", "semi", "nano",  etc. manually in notepad++
# I will use this text for word embedding training.

lemmatize_text_for_embedding_sepatated <- read.table("1_7_milyon_lemmatized_corrected_micro_nano_seperation.txt",
                                                                   header=FALSE, stringsAsFactors=FALSE, sep="\t")

lemmatized_final_text <- read.table("1_7_milyon_lemmatized_corrected_manually_UPDATED_2_dof.txt",
                                       header=FALSE, stringsAsFactors=FALSE, sep="\t")


lemmatize_text_for_embedding_sepatated$V2 <- lemmatize_text_for_embedding_sepatated$V2 %>% stringr::str_squish()
lemmatized_final_text$V2 <- lemmatized_final_text$V2 %>% stringr::str_squish()

extract_separeted_text <- lemmatize_text_for_embedding_sepatated %>% select(V2)
colnames(extract_separeted_text) <- "separeted_words"
extract_separeted_text$lemma_corrected <- lemmatized_final_text$V2
extract_separeted_text <- extract_separeted_text %>% filter(separeted_words != lemma_corrected)

rm(lemmatize_text_for_embedding_sepatated, lemmatized_final_text)

# STEP-(?) I will create a data frame for word embedding. ----
#-1 It will consist of corrected text an 
#-2 It will consist of lemmatized text and lemmatize_text_for_embedding_sepatated

#1 Corrected text for word embedding ----
WOS_Cited_Titles_Final$tokens_corrected <- WOS_Cited_Titles_Final$tokens_corrected %>% stringr::str_squish()
WOS_Cited_Titles_Final$lemma_corrected <- WOS_Cited_Titles_Final$lemma_corrected %>% stringr::str_squish()

corrected_tokens_of_cited_title_text_for_word_embedding <- WOS_Cited_Titles_Final %>% select(tokens_corrected)
corrected_tokens_of_cited_title_text_for_word_embedding <- corrected_tokens_of_cited_title_text_for_word_embedding %>% 
  distinct(tokens_corrected, .keep_all = TRUE)


#2 Lemmatized text for word embedding ----
word_embedding_text_WOS_cited_ti_lemma <- WOS_Cited_Titles_Final %>% select(lemma_corrected)
word_embedding_text_WOS_cited_ti_lemma <- word_embedding_text_WOS_cited_ti_lemma %>% 
  distinct(lemma_corrected, .keep_all = TRUE)


# Convert both vectors to data frames in order to combine
df1 <- data.frame(lemma_corrected = word_embedding_text_WOS_cited_ti_lemma$lemma_corrected)
df2 <- data.frame(lemma_corrected = extract_separeted_text$separeted_words)

# Combine the two data frames
combined_df <- rbind(df1, df2)

lemmatized_cited_title_text_for_word_embedding <- combined_df
rm(df1, df2, word_embedding_text_WOS_cited_ti_lemma, extract_separeted_text, combined_df)


##################### extracting separated words ####################################################
sil <- extract_separeted_text
# Comparing original and replaced text
sil$check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                      strsplit(sil$lemma_corrected, '\\s'), strsplit(sil$separeted_words, '\\s'))
sil$check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                      strsplit(sil$separeted_words, '\\s'), strsplit(sil$lemma_corrected, '\\s'))
sil_filtered <- sil %>% filter(check_1 != "" | check_2 != "")

sil_filtered <- sil_filtered %>%
  group_by(check_1, check_2) %>%
  tally(sort = TRUE)

rm(sil)
sil_filtered <- sil_filtered %>% distinct(check_1, .keep_all = TRUE) # I will remov
# openxlsx::write.xlsx(x = sil_filtered, file = "list of separeted WOS cited titles lemmatize text.xlsx")
rm(sil_filtered, extract_separeted_text)
##################### extracting separated words - END ####################################################

# I will save word embedding data separately:
# save(lemmatized_cited_title_text_for_word_embedding, file = "lemmatized_cited_title_text_for_word_embedding.Rdata")
# save(corrected_tokens_of_cited_title_text_for_word_embedding, file = "corrected_tokens_of_cited_title_text_for_word_embedding.Rdata")
rm(lemmatized_cited_title_text_for_word_embedding, corrected_tokens_of_cited_title_text_for_word_embedding)


read_me <- "WOS_Cited_Titles_Final includes final corrected versions of 5 milyon cited titles.
 WOS_Cited_Titles_Final$lemma_corrected is final version of lemmatized text.
 WOS_Cited_Titles_Final$tokens_corrected is the final version of tokenized text i.e. (words are corected)"

WOS_Cited_Titles_Final$tokens_corrected <- tolower(WOS_Cited_Titles_Final$tokens_corrected)
WOS_Cited_Titles_Final$lemma_corrected <- tolower(WOS_Cited_Titles_Final$lemma_corrected)

# save.image("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/WOS_Cited_Titles_Final_27_09_2023.RData")
gc()
