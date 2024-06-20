
# I have a data set that has undergone a step of text correction. I will importing it.
# The data was previously tried to be cleaned to only include English titles using various language recognition functions of R.
# Afterwards, it was tokenized and some corrections were made manually
# NOTE: "manual" means, using R, the words to be corrected were determined with human effort.
# Data to be loaded:
# load("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/cited_reference_titles_data_original_and_step1_corerections_September_2023.RData")


# REFERENCE CODES: 
# titile cited title refenece title text cleaning new V3 29_Aug_2023.R
# lemmatized text correction September 2023 notepad txt  STEP_1.R
# "C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited"


library(dplyr)
library(stringr)


setwd("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited")
# Singularize cited titles
titles_singular <- cited_ref_titles %>% distinct(corrected_STEP1, .keep_all = TRUE) # I will remove duplicated titles for data reduction.

library(spacyr)
spacy_install()
spacy_initialize()

# Will just tokenize so lemma= FALSE
system.time({
  titles_lemma <- spacy_parse(titles_singular$corrected_STEP1 , multithread = TRUE, nounphrase = FALSE, lemma= FALSE) #
}) # elapsed 3542.36 (07/09/2023)
spacy_finalize()

# save.image("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/cited_titles_text_preperation_STEP2_september_2023_ENVIRONMENT.RData")

# load("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/cited_titles_text_preperation_STEP2_september_2023_ENVIRONMENT.RData")

tokens_freq <- titles_lemma %>% 
  group_by(token) %>%
  summarise(freq = n()) %>% 
  arrange(desc(freq))

# openxlsx::write.xlsx(x = tokens_freq, file = "tokens_freq_for_STEP2.xlsx") 
# sil <- titles_singular %>% select(corrected_STEP1)
# write.table(sil, file="1_7_milyon_title_STEP_2.txt", row.names=FALSE, col.names=FALSE, quote=FALSE, sep="\t")
# rm(sil)



# I will replace tokens according to tokens_freq_for_STEP2_edited.xlsx
# First prepare tokens for untidy operation (i.e. preparing for converions of tokenized titles to original form)
tokens_original <- titles_lemma %>% select(doc_id, token)
tokens_original$row_no <- tokens_original[,1]
tokens_original <- tokens_original[, c(1,3,2)] # moving last column after 1st column
tokens_original[,2] <- gsub(pattern= "text", replacement = "", tokens_original[,2])
tokens_original[,2] <- as.numeric(tokens_original[,2])
tokens_original$doc_id <- NULL
tokens_original <- as.data.frame(tokens_original)


#> The Excel file named "tokens_freq_for_STEP2_edited.xlsx" contains a list of incorrect tokens 7486 misspelled tokens. 
#> I replace there tokens 
#> Then I will transfer the text to Notepad++ and made manual corrections and some NGram replacements based on the "NGrams" I use for WOS STM.

library(dplyr)

# Create the new column as a copy of token column
tokens_original <- tokens_original %>% mutate(corrected_token = token)


# I manually corrected tokens manually (there are over than 200.000 tokes, so I edited all of the tokens).
# Now I will import tokens.
library(readxl)
tokens_replacements_STEP2 <- read_excel("tokens_freq_for_STEP2_edited.xlsx", sheet = "tokens_replacements_STEP2",
                                         col_types = c("skip", "text", "text"))


colnames(tokens_replacements_STEP2) # "if_token"    "token_to_be"
tokens_replacements_STEP2$token_to_be <- tokens_replacements_STEP2$token_to_be %>% stringr::str_squish()
tokens_replacements_STEP2$if_token <- tokens_replacements_STEP2$if_token %>% stringr::str_squish()


# Correct the tokens based on the condition
tokens_original <- tokens_original %>%
  left_join(tokens_replacements_STEP2, by = c("token" = "if_token")) %>%
  mutate(corrected_token = ifelse(!is.na(token_to_be), token_to_be, corrected_token)) %>%
  select(-token_to_be)


# I will unite tokens (untidy) to titles
colnames(tokens_original) # [1] "row_no"          "token"           "corrected_token"
# Group by and summarizse
column_index <- 1 # i.e. row_no column
untidy_X <- tokens_original %>%
  group_by(tokens_original[[column_index]]) %>% 
  summarise(united_text = paste0(corrected_token, collapse = ' '))
names(untidy_X) <- c("row_no", "untidy_of_corrections_text")

# Check the result
untidy_X$original <- titles_singular$corrected_STEP1
untidy_X <- untidy_X[, c(1,3,2)] # reordering columns
untidy_X$original <- NULL
# untidy_X <- na.omit(untidy_X)
# untidy_X$untidy_of_lemmatized_text <- tolower(untidy_X$untidy_of_lemmatized_text )



sil <- untidy_X
sil$row_no <- NULL
sil <- as.data.frame(sil)
getwd() # [1] "C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited"

# Transferring corrected text (corrected according to tokens_freq_for_STEP2_edited.xlsx) to Notepad++ 
# write.table(sil, file="1_7_milyon_title_STEP_3.txt", row.names=FALSE, col.names=FALSE, quote=FALSE, sep="\t")

# I corrected cited titles manually in notepad++
cited_titles_corrected_step3 <- read.table("1_7_milyon_title_STEP_3.txt", header=FALSE, stringsAsFactors=FALSE, sep="\t") # The text I corrected
titles_singular$Notepad_Corrected_3 <- cited_titles_corrected_step3$V1 # (I added corrected notepad++ text  to titles_singular)
rm(cited_titles_corrected_step3)
gc()


# Making corrections (replacing corrected cited titles)
deneme_5_million <- cited_ref_titles # 5 million titles
correction_titles_1_7_million <- titles_singular %>% select (corrected_STEP1, Notepad_Corrected_3) # 1,7 million titles corrected
colnames(correction_titles_1_7_million) <- c("corrected_STEP1", "new_title_STEP3")
correction_titles_1_7_million$new_title_STEP3 <- correction_titles_1_7_million$new_title_STEP3 %>% stringr::str_squish()



dim(deneme_5_million)
dim(correction_titles_1_7_million)


colnames(deneme_5_million) # "wos_id"          "full_original"   "corrected_STEP1"  
colnames(correction_titles_1_7_million) # "corrected_STEP1" "new_title_STEP3" 


library(dplyr)

# Left join on corrected_STEP1 column
deneme_5_million <- left_join(deneme_5_million, 
                              correction_titles_1_7_million, 
                              by = "corrected_STEP1")

# Fill in the new column based on the conditions
deneme_5_million$correction_to_full_list_STEP3 <- ifelse(
  is.na(deneme_5_million$new_title_STEP3), 
  "OGUZ_OZBAY_CHECK_THIS_TITLE", # in order to check if some titles corrected wrong!
  deneme_5_million$new_title_STEP3)

# (Optional) Drop the new_title_STEP3 column if you don't need it anymore
deneme_5_million <- select(deneme_5_million, -new_title_STEP3)
# Removal of OGUZOZBAYDELETE's
deneme_5_million$correction_to_full_list_STEP3 <- gsub("OGUZOZBAYDELETE", " ", deneme_5_million$correction_to_full_list_STEP3)
deneme_5_million$correction_to_full_list_STEP3 <- deneme_5_million$correction_to_full_list_STEP3 %>% stringr::str_squish()

# To see empty or NA or only whitespace rows
filtered_data <- deneme_5_million %>% 
  filter(is.na(correction_to_full_list_STEP3) | 
           correction_to_full_list_STEP3 == "" | 
           grepl("^\\s*$", correction_to_full_list_STEP3))

# Removal of empty or NA or only whitespace rows
dim(deneme_5_million) # 5090045
deneme_5_million <- deneme_5_million %>% 
  filter(!(is.na(correction_to_full_list_STEP3) | 
             correction_to_full_list_STEP3 == "" | 
             grepl("^\\s*$", correction_to_full_list_STEP3)))
dim(deneme_5_million) # 5090002

############################################
############################################
# checking replacements
colnames(deneme_5_million) # 1] "wos_id" "full_original"  "corrected_STEP1"  "correction_to_full_list_STEP3"
sil <- deneme_5_million %>% select (corrected_STEP1, correction_to_full_list_STEP3)

# Comparing original and replaced text
sil$check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(sil$corrected_STEP1, '\\s'), strsplit(sil$correction_to_full_list_STEP3, '\\s'))
sil$check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(sil$correction_to_full_list_STEP3, '\\s'), strsplit(sil$corrected_STEP1, '\\s'))
sil_filtered <- sil %>% filter(check_1 != "" | check_2 != "")
############################################
############################################

# I will delete all environment except for deneme_5_million

WOS_Cited_tit_20_09_2023 <- deneme_5_million
WOS_Cited_tit_20_09_2023$corrected_STEP1 <- NULL
rm(deneme_5_million)

# save.image("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/WOS cited titles corrected final 20_09_2023.RData")


