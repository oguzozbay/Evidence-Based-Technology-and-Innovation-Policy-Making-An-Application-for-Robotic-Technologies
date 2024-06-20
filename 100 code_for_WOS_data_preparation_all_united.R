# This code is the combination of below R codes (I copied and pasted them one after another):

# C:/Users/ozbay/OneDrive - XXXX/R tez/R codes for thesis 02 Nisan 2024/code_no_100_Data_Prepare W_1_A_WOS_data_unite.R
# C:/Users/ozbay/OneDrive - XXXX/R tez/R codes for thesis 02 Nisan 2024/code_no_110_Data_Prepare W_1_B_tokenization_for_spelling_control.R
# C:/Users/ozbay/OneDrive - XXXX/R tez/R codes for thesis 02 Nisan 2024/code_no_120_Data_Prepare W_2_word_corrections.R
# C:/Users/ozbay/OneDrive - XXXX/R tez/R codes for thesis 02 Nisan 2024/code_no_130_Data_Prepare W_3_token_corrections.R
# C:/Users/ozbay/OneDrive - XXXX/R tez/R codes for thesis 02 Nisan 2024/code_no_140_Data_Prepare W_4_0_et_al_delete_list.R
# C:/Users/ozbay/OneDrive - XXXX/R tez/R codes for thesis 02 Nisan 2024/code_no_150_Data_Prepare W_4_1_et_al_deleting.R

#
##
####
####_____
####_____
####_____
####
##
#


# NOTE: All data used for corrections are given in the following data: token_correction_lists_for_thesis_22_04_2024.RData
# load("C:/Users/ozbay/OneDrive - XXXX/R tez/R codes for thesis 02 Nisan 2024/WOS data preparation/token_correction_lists_for_thesis_22_04_2024.RData")


########################## file 1: ----
# W_1_A_WOS_data_unite.
# I will unite WOS data.
# Below I gave two WOS data which I will unite

# load("C:/Users/ozbay/OneDrive - XXXX/WoS data Mart 2022/WoS_data_2005_01_Mart_2022_Research_area_Robot.RData")
# load("C:/Users/ozbay/OneDrive - XXXX/WoS data Mart 2022/WoS_data_2005_01_Mart_2022_Topic_Robot_United.RData")

#* Because R imports WOS data with the name of "M", I will import first part, then I will save M as M_SC
#* Then I will delete M and then import second part of WOS data (which will be import with the name of M)
#* I will will save M as M_TU, then I will unite M_SC and M_TU

load("C:/Users/ozbay/OneDrive - XXXX/WoS data Mart 2022/WoS_data_2005_01_Mart_2022_Research_area_Robot.RData")
# NOTE FOR THESIS: 100 WoS_data_2005_01_Mart_2022_Research_area_Robot.RData # is given for reproduction
M_SC <- M # First part of WOS data
rm(M) # M is deleted

load("C:/Users/ozbay/OneDrive - XXXX/WoS data Mart 2022/WoS_data_2005_01_Mart_2022_Topic_Robot_United.RData")
# NOTE FOR THESIS: 100 WoS_data_2005_01_Mart_2022_Topic_Robot_United # is given for reproduction
M_TU <- M # Second part of WOS data
rm(M) # M is deleted

library(bibliometrix)
library(dplyr)
M <- mergeDbSources(M_SC, M_TU, remove.duplicated = FALSE) # 302933 satır

M <- M %>% relocate(UT, .before = AU) # moving WOS numbers to first columnm
M <- M %>% relocate(AB, .before = AU)
M <- M %>% relocate(TI, .before = AB)

M <- M %>% distinct(UT, .keep_all = TRUE) # deletion of duplicated WOS numbers / 263731 row remained
# NOT-2 .keep_all = FALSE => removes other columns 

M$AB <- M$AB %>% stringr::str_squish() # removal of unnecessary spaces.
M$TI <- M$TI %>% stringr::str_squish()

# Removal of NA valued rows of a specified column
M <- subset(M, !is.na(AB)) # 258660 remains
M <- subset(M, !is.na(UT)) # 258660 remains; no NA's.
M <- subset(M, !is.na(TI)) # 258660 remains; no NA's.

M$AB <- tolower(M$AB) # Conversion to lowercase
M$TI <- tolower(M$TI)

M # WOS data is united.
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W1_WOS_united_raw_data_Environment.RData")
# NOTE: I renamed file W1_WOS_united_raw_data_Environment.RData as follows: W1_A_WOS_united_raw_data_Environment.RData (note for thesis 02_04_2024)
# I checked the results (i.e. R Environment) by running the code again (note for thesis 02_04_2024).



#
##
####
####_____
####_____
####_____
####
##
#



########################## file 2: ----
# W_1_B_tokenization_for_spelling_control
library(dplyr)
library(tidytext)
library(tidyr)
library(spacyr)

# NOTE: I renamed file W1_WOS_united_raw_data_Environment.RData as follows: W1_A_WOS_united_raw_data_Environment.RData (note for thesisi 02_04_2024)
# load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W1_A_WOS_united_raw_data_Environment.RData") # United WOS data


# STEP-1 Tokenization of abstracts
#> I will tokenize Abstracts (AB column) and then I will check spellings manually in excel.
#> The manual control process in Excel will be done as follows:
#> 1. Tokens will be copied as two columns and sorted by number of characters in descending order.
#> 2. Using the "spell check" tool in Excel, the tokens in the second column will be checked and errors will be corrected (Excel gives suggestions)
#> 3. Manual control as much as seven or six character words is possible with reasonable human effort.
#> 4. Rows that are unequal in two columns will be filtered out so that the result is the Token Correction  list.

#> I WILL TOKKENIZE TWO DIFFERENT WAYS.
#> I used spcyr in the trials. 
#> I thought I would use tidytext because it takes a very short time (sapcy takes 1 hour),
#> but I suspected that the tonekization results would be different.
#> Let me use two methods ans see what happens.


library(tidytext)
# text_df %>% unnest_tokens(word, text) # word= sütuna verilecek ad, text tokenize edilecek sütun
M_AB_tokens <- M %>% select(UT, AB) %>% unnest_tokens(token, AB, strip_punct = FALSE)
rownames(M_AB_tokens) <- NULL # for decreasing the data size
singular_tokens_AB <- M_AB_tokens %>% group_by(token) %>% summarise(repeats = n())


library(spacyr)
spacy_install()
spacy_initialize()
system.time({
  M_AB_tokens_spcyr <- spacy_parse(M$AB, multithread = TRUE, nounphrase = FALSE, lemma= FALSE) #
}) 
singular_tokens_spcyr  <- M_AB_tokens_spcyr %>% group_by(token) %>% summarise(repeats = n())

#> NOTE: spacyr and tidytext generate different tokens. 
#> Since I will be using spacyr for lemmatization, I preferred spacyr tokens. 
#> I will save token table as excel file and check and correct tokens as described above.
#> 
#> Now I will delete unnecessary files
rm(M_AB_tokens)
rm(singular_tokens_AB)

#> NOTE: This code is for checking and clearing the R codes I used for thesis work. 
#> I saved the tokens with the below name and made the corrections manually befaore this code..
#> openxlsx::write.xlsx(x = singular_tokens, file = "new_WOS_singular_tokens.xlsx")


# STEP-2 Tokenization of titles
library(spacyr)
library(dplyr)
spacy_install()
spacy_initialize()

system.time({
  M_TI_tokens <- spacy_parse(M$TI, multithread = TRUE, nounphrase = FALSE, lemma= FALSE)
}) # elapsed 544.50

singular_tokens_TI <- M_TI_tokens %>% group_by(token) %>% summarise(repeats = n())

#> NOTE: This code is for checking and clearing the R codes I used for thesis work. 
#> I saved the tokens with the below name and made the corrections manually befaore this code..
#> openxlsx::write.xlsx(x = singular_tokens, file = ""tokens_TI.xlsx"")


# STEP-3 Tokenization of keywords
# Now I will tokenize the keywords column. Then I will save it as excel and create a correction list for miss spelled tokens.
# DE / Author Keywords
# ID / Keywords Plus
M$DE <- tolower(M$DE)
M$ID <- tolower(M$ID)
keywords <- M %>% select(UT, DE, ID)
rownames(keywords) <- NULL # data size reductiion
keywords$DE[is.na(keywords$DE)] <- "OGUZ_OZBAY_NA" # I converted NA's to "OGUZ_OZBAY_NA" 
keywords$ID[is.na(keywords$ID)] <- "OGUZ_OZBAY_NA"


library(tidyr)
# Below, I combined the ID and DE columns and entered to a new column called "united_keywords".
# Then I tokenized united_keywords
keywords <- unite(keywords, united_keywords, ID, DE, sep = "; ", remove = FALSE, na.rm = FALSE)
system.time({
  M_keyword_tokens <- spacy_parse(keywords$united_keywords, multithread = TRUE, nounphrase = FALSE, lemma= FALSE) #
}) # elapsed 384

#> Actually it's better to tokenize with tidy_text (since it's much, much faster). 
#> However, since I used spacyr before, I continue that way. 
#> By the way, tidy_text and spacyr tokenization results are different (for punctuation)
singular_keywords_tokens <- M_keyword_tokens %>% group_by(token) %>% summarise(repeats = n())


# Data size reduction 
# I will delete unnecessary columns for reducing data size
colnames(M_AB_tokens_spcyr)
M_AB_tokens_spcyr$sentence_id <- NULL 
M_AB_tokens_spcyr$token_id <- NULL 
M_AB_tokens_spcyr$pos <- NULL 
M_AB_tokens_spcyr$entity <- NULL 

colnames(M_TI_tokens)
M_TI_tokens$sentence_id <- NULL 
M_TI_tokens$token_id <- NULL 
M_TI_tokens$pos <- NULL 
M_TI_tokens$entity <- NULL 

colnames(M_keyword_tokens)
M_keyword_tokens$sentence_id <- NULL 
M_keyword_tokens$token_id <- NULL 
M_keyword_tokens$pos <- NULL 
M_keyword_tokens$entity <- NULL 

# EXPLANATIONS for ENVIRONMENT ---
M # Original M, just united (any correction do not exist)
M_AB_tokens_spcyr # all tokens of M$AB. 
M_TI_tokens # all tokens of M$TI. 
singular_tokens_spcyr # Singular tokens of M$AB. I will use this cor creating for token correction list of ABI.
singular_tokens_TI # Singular tokens of M$TI. I will use this cor creating for token correction list of TI.
singular_keywords_tokens # Singular tokens of keywords_united. I will use this cor creating for token correction list of united_keywords.

# NOTE: At this point I did not add keywords_united to M



#
##
####
####_____
####_____
####_____
####
##
#

########################## file 3: ----
# W_2_word_corrections
library(stringi)
library(dplyr)
library(stringr)

# load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W1_A_WOS_united_raw_data_Environment.RData") # 256660 row WOS data (original text)

# STEP-1 Correction of abstract (AB) ---
# now i will change the wrong abstracts
# I have a excel table for correction  some abstracts (AB): AB_replace_list.xlsx
AB_replace_list # replacements for AB's according to WOS numbers
M$AB_correct <- AB_replace_list$AB_corrected[match(M$UT, AB_replace_list$UT)]
library(dplyr)
M <- M %>% relocate(AB_correct, .after = AB)
M$AB_1 <- ifelse(is.na(M$AB_correct), M$AB, M$AB_correct) # if M$AB_1 is NA, then NA is changed to "M$AB" value (i.e. original value)
M <- M %>% relocate(AB_1, .after = AB_correct)

M$AB <- M$AB_1
M$AB_correct <- NULL
M$AB_1 <- NULL
# Now M$AB is updated and unnecessary columns are deleted 

# STEP-2 Correction (ie. replacement) of titles (TI) ---
# now i will change the wrong titles
# I have a excel table for correction (replacement) TI (titles): TI_replace_list.xlsx.xlsx

# Caution: This code converts mismatches to NA. Therefore, it is necessary to replace the NA ones with the original abstract.
TI_replace_list # replacements for Titles according to WOS numbers
M$TI_1 <- TI_replace_list$replacement_TI[match(M$UT, TI_replace_list$WOS_Number)]
M <- M %>% relocate(TI_1, .after = TI) # for the ease of checking
M$TI_1 <- ifelse(is.na(M$TI_1), M$TI, M$TI_1) # if M$TI_1 is NA, then NA is changed to "M$TI" value (i.e. original value)
M$TI <- M$TI_1
M$TI_1 <- NULL
# Now M$TI is updated and unnecessary columns are deleted 

# STEP-3  Now I will correct the misspelled words in AB ---
# I identified these misspelled words while manually checking for tokens. 
# correction in the token list was not possible because they were spitted words (e.g. "al gorithm"	to "algorithm")
# I have to separate correction list in to two (AB_word_correction_list.xlsx):
#> 1) for replacing according to word boundaries, and
#> 2) for replacing words that are unsuitable to use regex (e.g / is not usable in regex with a easy way)

word_replace_list_regex # 1) for replacing according to word boundaries
word_replace_list_no_regex # 2) for replacing word unsuitable to use regex

# STEP-3.1
word_replace_list_regex # AB_word_correction_list.xlsx => sheet1
# NOTE: R does not accept certain characters as word boundaries. For example, those that start/include with "[" or "(", etc.
Old_1 <- word_replace_list_regex[["from"]]
New_1 <- word_replace_list_regex[["to"]]

library(stringi)
system.time({
  M$AB_2 <- stringi::stri_replace_all_regex(
    M[["AB"]], "\\b"%s+%Old_1%s+%"\\b", New_1, vectorize_all = FALSE)
}) # elapsed 396

M <- M %>% relocate(AB_2, .after = AB) # for the ease of checking


# Now I will check the replacements
M$AB_replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                               strsplit(M$AB, '\\s'), strsplit(M$AB_2, '\\s'))

M$AB_replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                               strsplit(M$AB_2, '\\s'), strsplit(M$AB, '\\s'))

M <- M %>% relocate(AB_replace_check_1, .after = AB_2) # for the ease of checking
M <- M %>% relocate(AB_replace_check_2, .after = AB_replace_check_1) # for the ease of checking
# all replacements are done!

colnames(M)
M$AB <- M$AB_2
M$AB_2 <- NULL # unnecessary columns are deleted
M$AB_replace_check_1 <- NULL
M$AB_replace_check_2 <- NULL


# STEP-3.1
word_replace_list_no_regex # 2) for replacing word unsuitable to use regex (AB_word_correction_list.xlsx => sheet2)

word_replace <- function(w, word_old, word_new) {
  w <- gsub(pattern = paste0(word_old), replacement = word_new, w, fixed = TRUE) # = w içinde bu işlemi yap ve w olarak yaz.
  # CAUTION: This code ignores word boundaries!
  return(w)
}

replace_data <- word_replace_list_no_regex
replace_all_words <- function(w) {
  for (ii in c(1:nrow(replace_data))) {
    w <- word_replace(w, replace_data$from[ii], replace_data$to[ii])
  }
  return(w)
}

system.time(
  M$AB_3[1:258660] <- sapply(M$AB [1:258660], replace_all_words)
) # elapsed 215

M <- M %>% relocate(AB_3, .after = AB) # for the ease of checking

M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$AB, '\\s'), strsplit(M$AB_3, '\\s'))


M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$AB_3, '\\s'), strsplit(M$AB, '\\s'))

M <- M %>% relocate(replace_check_1, .after = AB_3) # for the ease of checking
M <- M %>% relocate(replace_check_2, .after = replace_check_1) # for the ease of checking

# All replacements for AB is completed.
colnames(M)
M$AB <- M$AB_3
M$AB_3 <- NULL
M$replace_check_1 <- NULL
M$replace_check_2 <- NULL



# STEP-4 Now I will correct the misspelled words in TI ---
# I identified these misspelled words while manually checking for tokens. 
# TI_word_correciton_lists.xlsx

replace_data <- TI_word_correciton_lists
replace_all_words <- function(w) {
  for (ii in c(1:nrow(replace_data))) {
    w <- word_replace(w, replace_data$from[ii], replace_data$to[ii])
  }
  return(w)
}

system.time(
  M$TI_1[1:258660] <- sapply(M$TI [1:258660], replace_all_words)
) # elapsed 188

M <- M %>% relocate(TI_1, .after = TI) # for the ease of checking
M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$TI, '\\s'), strsplit(M$TI_1, '\\s'))


M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$TI_1, '\\s'), strsplit(M$TI, '\\s'))

M <- M %>% relocate(replace_check_1, .after = TI_1) # for the ease of checking
M <- M %>% relocate(replace_check_2, .after = replace_check_1) # for the ease of checking

colnames(M)
M$TI <- M$TI_1
M$TI_1 <- NULL
M$replace_check_1 <- NULL
M$replace_check_2 <- NULL
# At this point, all miss spelled words in TI is changed (AB was changed too).


####___________________________________
# NOTE: spacyr tokenizes "5g" as "5 g". For this reason I converted 5g to 5_g in TI column.
# Similarly I converted 3g and 4g to 3_g and 4_g.
# I did this manually.
####___________________________________



# STEP-5 Now it is time to clean "OGUZOZBAYDELETE" in AB and TI columns to "" ---
# STEP-5-1 First "OGUZOZBAYDELETE" in AB column to ""
Old_2 <- "OGUZOZBAYDELETE"
New_2 <- " "
system.time({
  M$AB_2 <- stringi::stri_replace_all_regex(
    M[["AB"]], "\\b"%s+%Old_2%s+%"\\b", New_2, vectorize_all = FALSE)
}) # elapsed 215

M <- M %>% relocate(AB_2, .after = AB)

M$AB_replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                               strsplit(M$AB, '\\s'), strsplit(M$AB_2, '\\s'))

M$AB_replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                               strsplit(M$AB_2, '\\s'), strsplit(M$AB, '\\s'))

M <- M %>% relocate(AB_replace_check_1, .after = AB_2) # for the ease of checking
M <- M %>% relocate(AB_replace_check_2, .after = AB_replace_check_1) # for the ease of checking

colnames(M)
M$AB <- M$AB_2
M$AB_2 <- NULL
M$AB_replace_check_1 <- NULL
M$AB_replace_check_2 <- NULL
library(stringr)
M$AB <- M$AB %>% stringr::str_squish()


# STEP-5-2 Now I'm changing "OGUZOZBAYDELETE" to " " in TI column
system.time({
  M$TI_1 <- stringi::stri_replace_all_regex(
    M[["TI"]], "\\b"%s+%Old_2%s+%"\\b", New_2, vectorize_all = FALSE)
})

M <- M %>% relocate(TI_1, .after = TI) # for the ease of checking
M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$TI, '\\s'), strsplit(M$TI_1, '\\s'))

M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$TI_1, '\\s'), strsplit(M$TI, '\\s'))

M <- M %>% relocate(replace_check_1, .after = TI_1) # for the ease of checking
M <- M %>% relocate(replace_check_2, .after = replace_check_1) # for the ease of checking

colnames(M)
M$TI <- M$TI_1
M$TI_1 <- NULL
M$replace_check_1 <- NULL
M$replace_check_2 <- NULL
M$TI <- M$TI %>% stringr::str_squish()


# At this point, all word corrections are completed in the AB and TI columns of WoS data, and OGUZOZBAYDELETE's were deleted
# Mote that "word correction" means miss spelled words that are not a single token.
# Next step is doing same work for keywords.
# Then next process will be the correction of tokens by use of excel correction tables (ie. AB_tokens_corrections_for_STM_and_word_embedding.xlsx)


# STEP-6 Now I will create a united_keywords column ---
# Then I correct miss spelled keywords
# I identified these misspelled words while manually checking for tokens. 
# keywords_word_correction_list.xlsx

# DE / Author Keywords
# ID / Keywords Plus
M$DE <- tolower(M$DE)
M$ID <- tolower(M$ID)

M$DE[is.na(M$DE)] <- "OGUZ_OZBAY_NA" # I converted NA's to "OGUZ_OZBAY_NA" 
M$ID[is.na(M$ID)] <- "OGUZ_OZBAY_NA"

library(tidyr)
# Below, I combined the ID and DE columns and entered to a new column called "united_keywords".
M <- unite(M, united_keywords, ID, DE, sep = "; ", remove = FALSE, na.rm = FALSE)


keyword_word_correction_regex # 1) for replacing according to word boundaries
keyword_word_correction_noregex # 2) for replacing word unsuitable to use regex

# STEP-6.1 ---
keyword_word_correction_regex # keywords_word_correction_list.xlsx => sheet1
# NOTE: R does not accept certain characters as word boundaries. For example, those that start/include with "[" or "(", etc.
Old_3 <- keyword_word_correction_regex[["from"]]
New_3 <- keyword_word_correction_regex[["to"]]

library(stringi)
system.time({
  M$united_keywords_1 <- stringi::stri_replace_all_regex(
    M[["united_keywords"]], "\\b"%s+%Old_3%s+%"\\b", New_3, vectorize_all = FALSE)
}) # elapsed 396

M <- M %>% relocate(united_keywords_1, .after = united_keywords) # for the ease of checking


# Now I will check the replacements
M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$united_keywords, '\\s'), strsplit(M$united_keywords_1, '\\s'))

M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$united_keywords_1, '\\s'), strsplit(M$united_keywords, '\\s'))

M <- M %>% relocate(replace_check_1, .after = united_keywords) # for the ease of checking
M <- M %>% relocate(replace_check_2, .after = replace_check_1) # for the ease of checking
# all replacements are done!

colnames(M)
M$united_keywords <- M$united_keywords_1
M$united_keywords_1 <- NULL # unnecessary columns are deleted
M$replace_check_1 <- NULL
M$replace_check_2 <- NULL


# STEP-6.2 ---
keyword_word_correction_noregex # 2) for replacing word unsuitable to use regex (# keywords_word_correction_list.xlsx => sheet2)

word_replace <- function(w, word_old, word_new) {
  w <- gsub(pattern = paste0(word_old), replacement = word_new, w, fixed = TRUE) # = w içinde bu işlemi yap ve w olarak yaz.
  # CAUTION: This code ignores word boundaries!
  return(w)
}

replace_data <- keyword_word_correction_noregex
replace_all_words <- function(w) {
  for (ii in c(1:nrow(replace_data))) {
    w <- word_replace(w, replace_data$from[ii], replace_data$to[ii])
  }
  return(w)
}


system.time(
  M$united_keywords_2[1:258660] <- sapply(M$united_keywords [1:258660], replace_all_words)
)

M <- M %>% relocate(united_keywords_2, .after = united_keywords) # for the ease of checking

M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$united_keywords, '\\s'), strsplit(M$united_keywords_2, '\\s'))

M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$united_keywords_2, '\\s'), strsplit(M$united_keywords, '\\s'))

M <- M %>% relocate(replace_check_1, .after = united_keywords_2) # for the ease of checking
M <- M %>% relocate(replace_check_2, .after = replace_check_1) # for the ease of checking

# All replacements for united_keywords is completed.
colnames(M)
M$united_keywords <- M$united_keywords_2
M$united_keywords_2 <- NULL
M$replace_check_1 <- NULL
M$replace_check_2 <- NULL

# A manual replacement is required
M$united_keywords[M$UT == "WOS:000589836300030"]
M$united_keywords[M$UT == "WOS:000589836300030"] <- "surgery; future; mode; da vinci system; gastric;cancer; robotic gastrectomy"


# STEP-6-3 Now it is time to clean "OGUZ_OZBAY_NA; OGUZ_OZBAY_NA" in  to "OGUZ_OZBAY_NA" ---
Old_4 <- "OGUZ_OZBAY_NA; OGUZ_OZBAY_NA"
New_4 <- "OGUZ_OZBAY_NA"
system.time({
  M$united_keywords_2 <- stringi::stri_replace_all_regex(
    M[["united_keywords"]], "\\b"%s+%Old_4%s+%"\\b", New_4, vectorize_all = FALSE)
})

M <- M %>% relocate(united_keywords_2, .after = united_keywords)

M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$united_keywords, '\\s'), strsplit(M$united_keywords_2, '\\s'))

M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$united_keywords_2, '\\s'), strsplit(M$united_keywords, '\\s'))

M <- M %>% relocate(replace_check_1, .after = united_keywords) # for the ease of checking
M <- M %>% relocate(replace_check_2, .after = replace_check_1) # for the ease of checking

colnames(M)
M$united_keywords <- M$united_keywords_2
M$united_keywords_2 <- NULL
M$replace_check_1 <- NULL
M$replace_check_2 <- NULL
library(stringr)
M$united_keywords <- M$united_keywords %>% stringr::str_squish()


# STEP-6-4 Now it is time to clean "; OGUZ_OZBAY_NA" in  to "" ---
Old_5 <- "; OGUZ_OZBAY_NA"
New_5 <- ""
system.time({
  M$united_keywords_2 <- stringi::stri_replace_all_regex(
    M[["united_keywords"]], "\\b"%s+%Old_5%s+%"\\b", New_5, vectorize_all = FALSE)
})

M <- M %>% relocate(united_keywords_2, .after = united_keywords)

M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$united_keywords, '\\s'), strsplit(M$united_keywords_2, '\\s'))

M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$united_keywords_2, '\\s'), strsplit(M$united_keywords, '\\s'))

M <- M %>% relocate(replace_check_1, .after = united_keywords) # for the ease of checking
M <- M %>% relocate(replace_check_2, .after = replace_check_1) # for the ease of checking

colnames(M)
M$united_keywords <- M$united_keywords_2
M$united_keywords_2 <- NULL
M$replace_check_1 <- NULL
M$replace_check_2 <- NULL
library(stringr)
M$united_keywords <- M$united_keywords %>% stringr::str_squish()



# STEP-6-5 Now it is time to clean "OGUZ_OZBAY_NA; "" ---
M$united_keywords_2 <- gsub(pattern= "OGUZ_OZBAY_NA; ", replacement = "", M$united_keywords)

M <- M %>% relocate(united_keywords_2, .after = united_keywords)

M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$united_keywords, '\\s'), strsplit(M$united_keywords_2, '\\s'))

M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$united_keywords_2, '\\s'), strsplit(M$united_keywords, '\\s'))

M <- M %>% relocate(replace_check_1, .after = united_keywords) # for the ease of checking
M <- M %>% relocate(replace_check_2, .after = replace_check_1) # for the ease of checking

colnames(M)
M$united_keywords <- M$united_keywords_2
M$united_keywords_2 <- NULL
M$replace_check_1 <- NULL
M$replace_check_2 <- NULL
library(stringr)
M$united_keywords <- M$united_keywords %>% stringr::str_squish()


# There ";;" in M$united_keywords. And also some word;word. I will correct these ";;" and ";" to "; " 

M$united_keywords <- gsub(pattern= ";;", replacement = "; ", M$united_keywords)
M$united_keywords <- gsub(pattern= ";", replacement = "; ", M$united_keywords)
M$united_keywords <- M$united_keywords %>% stringr::str_squish()

# At this point, all word corrections are completed in the AB, TI and united_keywords columns of WoS data, and OGUZOZBAYDELETE's were deleted
# Note that "word correction" means miss spelled words that are not a single token.
# Next step will be the correction of tokens by use of excel correction tables (ie. AB_tokens_corrections_for_STM_and_word_embedding.xlsx)


# EXPLANATION FOR ENVIRONMENT
# Word corrections of AB, TI and united_keywords columns of M completed. 
# Note that "word correction" means miss spelled words that are not a single token (e.g. "trasnform tion" => "transformation")
# In the next step, I will tokenize these corrected AB, TI and united_keywords columns.

library(stringi)
library(dplyr)
library(stringr)

# load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W1_A_WOS_united_raw_data_Environment.RData")

# STEP-1 Correction of abstract (AB) ---
# now i will change the wrong abstracts
# I have a excel table for correction  some abstracts (AB): AB_replace_list.xlsx
AB_replace_list # replacements for AB's according to WOS numbers
M$AB_correct <- AB_replace_list$AB_corrected[match(M$UT, AB_replace_list$UT)]
library(dplyr)
M <- M %>% relocate(AB_correct, .after = AB)
M$AB_1 <- ifelse(is.na(M$AB_correct), M$AB, M$AB_correct) # if M$AB_1 is NA, then NA is changed to "M$AB" value (i.e. original value)
M <- M %>% relocate(AB_1, .after = AB_correct)

M$AB <- M$AB_1
M$AB_correct <- NULL
M$AB_1 <- NULL
# Now M$AB is updated and unnecessary columns are deleted 

# STEP-2 Correction (ie. replacement) of titles (TI) ---
# now i will change the wrong titles
# I have a excel table for correction (replacement) TI (titles): TI_replace_list.xlsx.xlsx

# Caution: This code converts mismatches to NA. Therefore, it is necessary to replace the NA ones with the original abstract.
TI_replace_list # replacements for Titles according to WOS numbers
M$TI_1 <- TI_replace_list$replacement_TI[match(M$UT, TI_replace_list$WOS_Number)]
M <- M %>% relocate(TI_1, .after = TI) # for the ease of checking
M$TI_1 <- ifelse(is.na(M$TI_1), M$TI, M$TI_1) # if M$TI_1 is NA, then NA is changed to "M$TI" value (i.e. original value)
M$TI <- M$TI_1
M$TI_1 <- NULL
# Now M$TI is updated and unnecessary columns are deleted 


# STEP-3  Now I will correct the misspelled words in AB ---
# I identified these misspelled words while manually checking for tokens. 
# correction in the token list was not possible because they were spitted words (e.g. "al gorithm"	to "algorithm")
# I have to separate correction list in to two (AB_word_correction_list.xlsx):
#> 1) for replacing according to word boundaries, and
#> 2) for replacing words that are unsuitable to use regex (e.g / is not usable in regex with a easy way)

word_replace_list_regex # 1) for replacing according to word boundaries
word_replace_list_no_regex # 2) for replacing word unsuitable to use regex

# STEP-3.1
word_replace_list_regex # AB_word_correction_list.xlsx => sheet1
# NOTE: R does not accept certain characters as word boundaries. For example, those that start/include with "[" or "(", etc.
Old_1 <- word_replace_list_regex[["from"]]
New_1 <- word_replace_list_regex[["to"]]

library(stringi)
system.time({
  M$AB_2 <- stringi::stri_replace_all_regex(
    M[["AB"]], "\\b"%s+%Old_1%s+%"\\b", New_1, vectorize_all = FALSE)
}) # elapsed 396

M <- M %>% relocate(AB_2, .after = AB) # for the ease of checking


# Now I will check the replacements
M$AB_replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                               strsplit(M$AB, '\\s'), strsplit(M$AB_2, '\\s'))

M$AB_replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                               strsplit(M$AB_2, '\\s'), strsplit(M$AB, '\\s'))

M <- M %>% relocate(AB_replace_check_1, .after = AB_2) # for the ease of checking
M <- M %>% relocate(AB_replace_check_2, .after = AB_replace_check_1) # for the ease of checking
# all replacements are done!

colnames(M)
M$AB <- M$AB_2
M$AB_2 <- NULL # unnecessary columns are deleted
M$AB_replace_check_1 <- NULL
M$AB_replace_check_2 <- NULL


# STEP-3.1
word_replace_list_no_regex # 2) for replacing word unsuitable to use regex (AB_word_correction_list.xlsx => sheet2)

word_replace <- function(w, word_old, word_new) {
  w <- gsub(pattern = paste0(word_old), replacement = word_new, w, fixed = TRUE) # = w içinde bu işlemi yap ve w olarak yaz.
  # CAUTION: This code ignores word boundaries!
  return(w)
}

replace_data <- word_replace_list_no_regex
replace_all_words <- function(w) {
  for (ii in c(1:nrow(replace_data))) {
    w <- word_replace(w, replace_data$from[ii], replace_data$to[ii])
  }
  return(w)
}

system.time(
  M$AB_3[1:258660] <- sapply(M$AB [1:258660], replace_all_words)
) # elapsed 215

M <- M %>% relocate(AB_3, .after = AB) # for the ease of checking

M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$AB, '\\s'), strsplit(M$AB_3, '\\s'))


M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$AB_3, '\\s'), strsplit(M$AB, '\\s'))

M <- M %>% relocate(replace_check_1, .after = AB_3) # for the ease of checking
M <- M %>% relocate(replace_check_2, .after = replace_check_1) # for the ease of checking

# All replacements for AB is completed.
colnames(M)
M$AB <- M$AB_3
M$AB_3 <- NULL
M$replace_check_1 <- NULL
M$replace_check_2 <- NULL

# STEP-4 Now I will correct the misspelled words in TI ---
# I identified these misspelled words while manually checking for tokens. 
# TI_word_correciton_lists.xlsx

replace_data <- TI_word_correciton_lists
replace_all_words <- function(w) {
  for (ii in c(1:nrow(replace_data))) {
    w <- word_replace(w, replace_data$from[ii], replace_data$to[ii])
  }
  return(w)
}

system.time(
  M$TI_1[1:258660] <- sapply(M$TI [1:258660], replace_all_words)
) # elapsed 188

M <- M %>% relocate(TI_1, .after = TI) # for the ease of checking
M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$TI, '\\s'), strsplit(M$TI_1, '\\s'))


M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$TI_1, '\\s'), strsplit(M$TI, '\\s'))

M <- M %>% relocate(replace_check_1, .after = TI_1) # for the ease of checking
M <- M %>% relocate(replace_check_2, .after = replace_check_1) # for the ease of checking

colnames(M)
M$TI <- M$TI_1
M$TI_1 <- NULL
M$replace_check_1 <- NULL
M$replace_check_2 <- NULL
# At this point, all miss spelled words in TI is changed (AB was changed too).


######________________________
# NOTE: spacyr tokenizes "5g" as "5 g". For this reason I converted 5g to 5_g in TI column.
# Similarly I converted 3g and 4g to 3_g and 4_g.
# I did this manually.
######________________________



# STEP-5 Now it is time to clean "OGUZOZBAYDELETE" in AB and TI columns to "" ---
# STEP-5-1 First "OGUZOZBAYDELETE" in AB column to ""
Old_2 <- "OGUZOZBAYDELETE"
New_2 <- " "
system.time({
  M$AB_2 <- stringi::stri_replace_all_regex(
    M[["AB"]], "\\b"%s+%Old_2%s+%"\\b", New_2, vectorize_all = FALSE)
}) # elapsed 215

M <- M %>% relocate(AB_2, .after = AB)

M$AB_replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                               strsplit(M$AB, '\\s'), strsplit(M$AB_2, '\\s'))

M$AB_replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                               strsplit(M$AB_2, '\\s'), strsplit(M$AB, '\\s'))

M <- M %>% relocate(AB_replace_check_1, .after = AB_2) # for the ease of checking
M <- M %>% relocate(AB_replace_check_2, .after = AB_replace_check_1) # for the ease of checking

colnames(M)
M$AB <- M$AB_2
M$AB_2 <- NULL
M$AB_replace_check_1 <- NULL
M$AB_replace_check_2 <- NULL
library(stringr)
M$AB <- M$AB %>% stringr::str_squish()



# STEP-5-2 Now I'm changing "OGUZOZBAYDELETE" to " " in TI column
system.time({
  M$TI_1 <- stringi::stri_replace_all_regex(
    M[["TI"]], "\\b"%s+%Old_2%s+%"\\b", New_2, vectorize_all = FALSE)
})

M <- M %>% relocate(TI_1, .after = TI) # for the ease of checking
M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$TI, '\\s'), strsplit(M$TI_1, '\\s'))

M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$TI_1, '\\s'), strsplit(M$TI, '\\s'))

M <- M %>% relocate(replace_check_1, .after = TI_1) # for the ease of checking
M <- M %>% relocate(replace_check_2, .after = replace_check_1) # for the ease of checking

colnames(M)
M$TI <- M$TI_1
M$TI_1 <- NULL
M$replace_check_1 <- NULL
M$replace_check_2 <- NULL
M$TI <- M$TI %>% stringr::str_squish()


# At this point, all word corrections are completed in the AB and TI columns of WoS data, and OGUZOZBAYDELETE's were deleted
# Mote that "word correction" means miss spelled words that are not a single token.
# Next step is doing same work for keywords.
# Then next process will be the correction of tokens by use of excel correction tables (ie. AB_tokens_corrections_for_STM_and_word_embedding.xlsx)

# STEP-6 Now I will create a united_keywords column ---
# Then I correct miss spelled keywords
# I identified these misspelled words while manually checking for tokens. 
# keywords_word_correction_list.xlsx

# DE / Author Keywords
# ID / Keywords Plus
M$DE <- tolower(M$DE)
M$ID <- tolower(M$ID)

M$DE[is.na(M$DE)] <- "OGUZ_OZBAY_NA" # I converted NA's to "OGUZ_OZBAY_NA" 
M$ID[is.na(M$ID)] <- "OGUZ_OZBAY_NA"

library(tidyr)
# Below, I combined the ID and DE columns and entered to a new column called "united_keywords".
M <- unite(M, united_keywords, ID, DE, sep = "; ", remove = FALSE, na.rm = FALSE)


keyword_word_correction_regex # 1) for replacing according to word boundaries
keyword_word_correction_noregex # 2) for replacing word unsuitable to use regex

# STEP-6.1 ---
keyword_word_correction_regex # keywords_word_correction_list.xlsx => sheet1
# NOTE: R does not accept certain characters as word boundaries. For example, those that start/include with "[" or "(", etc.
Old_3 <- keyword_word_correction_regex[["from"]]
New_3 <- keyword_word_correction_regex[["to"]]

library(stringi)
system.time({
  M$united_keywords_1 <- stringi::stri_replace_all_regex(
    M[["united_keywords"]], "\\b"%s+%Old_3%s+%"\\b", New_3, vectorize_all = FALSE)
}) # elapsed 396

M <- M %>% relocate(united_keywords_1, .after = united_keywords) # for the ease of checking

# Now I will check the replacements
M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                               strsplit(M$united_keywords, '\\s'), strsplit(M$united_keywords_1, '\\s'))

M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                               strsplit(M$united_keywords_1, '\\s'), strsplit(M$united_keywords, '\\s'))

M <- M %>% relocate(replace_check_1, .after = united_keywords) # for the ease of checking
M <- M %>% relocate(replace_check_2, .after = replace_check_1) # for the ease of checking
# all replacements are done!

colnames(M)
M$united_keywords <- M$united_keywords_1
M$united_keywords_1 <- NULL # unnecessary columns are deleted
M$replace_check_1 <- NULL
M$replace_check_2 <- NULL


# STEP-6.2 ---
keyword_word_correction_noregex # 2) for replacing word unsuitable to use regex (# keywords_word_correction_list.xlsx => sheet2)

word_replace <- function(w, word_old, word_new) {
  w <- gsub(pattern = paste0(word_old), replacement = word_new, w, fixed = TRUE) # = w içinde bu işlemi yap ve w olarak yaz.
  # CAUTION: This code ignores word boundaries!
  return(w)
}

replace_data <- keyword_word_correction_noregex
replace_all_words <- function(w) {
  for (ii in c(1:nrow(replace_data))) {
    w <- word_replace(w, replace_data$from[ii], replace_data$to[ii])
  }
  return(w)
}


system.time(
  M$united_keywords_2[1:258660] <- sapply(M$united_keywords [1:258660], replace_all_words)
)

M <- M %>% relocate(united_keywords_2, .after = united_keywords) # for the ease of checking

M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$united_keywords, '\\s'), strsplit(M$united_keywords_2, '\\s'))

M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$united_keywords_2, '\\s'), strsplit(M$united_keywords, '\\s'))

M <- M %>% relocate(replace_check_1, .after = united_keywords_2) # for the ease of checking
M <- M %>% relocate(replace_check_2, .after = replace_check_1) # for the ease of checking

# All replacements for united_keywords is completed.
colnames(M)
M$united_keywords <- M$united_keywords_2
M$united_keywords_2 <- NULL
M$replace_check_1 <- NULL
M$replace_check_2 <- NULL

# A manual replacement is required
M$united_keywords[M$UT == "WOS:000589836300030"]
M$united_keywords[M$UT == "WOS:000589836300030"] <- "surgery; future; mode; da vinci system; gastric;cancer; robotic gastrectomy"


# STEP-6-3 Now it is time to clean "OGUZ_OZBAY_NA; OGUZ_OZBAY_NA" in  to "OGUZ_OZBAY_NA" ---
Old_4 <- "OGUZ_OZBAY_NA; OGUZ_OZBAY_NA"
New_4 <- "OGUZ_OZBAY_NA"
system.time({
  M$united_keywords_2 <- stringi::stri_replace_all_regex(
    M[["united_keywords"]], "\\b"%s+%Old_4%s+%"\\b", New_4, vectorize_all = FALSE)
})

M <- M %>% relocate(united_keywords_2, .after = united_keywords)

M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                               strsplit(M$united_keywords, '\\s'), strsplit(M$united_keywords_2, '\\s'))

M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                               strsplit(M$united_keywords_2, '\\s'), strsplit(M$united_keywords, '\\s'))

M <- M %>% relocate(replace_check_1, .after = united_keywords) # for the ease of checking
M <- M %>% relocate(replace_check_2, .after = replace_check_1) # for the ease of checking

colnames(M)
M$united_keywords <- M$united_keywords_2
M$united_keywords_2 <- NULL
M$replace_check_1 <- NULL
M$replace_check_2 <- NULL
library(stringr)
M$united_keywords <- M$united_keywords %>% stringr::str_squish()


# STEP-6-4 Now it is time to clean "; OGUZ_OZBAY_NA" in  to "" ---
Old_5 <- "; OGUZ_OZBAY_NA"
New_5 <- ""
system.time({
  M$united_keywords_2 <- stringi::stri_replace_all_regex(
    M[["united_keywords"]], "\\b"%s+%Old_5%s+%"\\b", New_5, vectorize_all = FALSE)
})

M <- M %>% relocate(united_keywords_2, .after = united_keywords)

M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$united_keywords, '\\s'), strsplit(M$united_keywords_2, '\\s'))

M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$united_keywords_2, '\\s'), strsplit(M$united_keywords, '\\s'))

M <- M %>% relocate(replace_check_1, .after = united_keywords) # for the ease of checking
M <- M %>% relocate(replace_check_2, .after = replace_check_1) # for the ease of checking

colnames(M)
M$united_keywords <- M$united_keywords_2
M$united_keywords_2 <- NULL
M$replace_check_1 <- NULL
M$replace_check_2 <- NULL
library(stringr)
M$united_keywords <- M$united_keywords %>% stringr::str_squish()



# STEP-6-5 Now it is time to clean "OGUZ_OZBAY_NA; "" ---
M$united_keywords_2 <- gsub(pattern= "OGUZ_OZBAY_NA; ", replacement = "", M$united_keywords)

M <- M %>% relocate(united_keywords_2, .after = united_keywords)

M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$united_keywords, '\\s'), strsplit(M$united_keywords_2, '\\s'))

M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$united_keywords_2, '\\s'), strsplit(M$united_keywords, '\\s'))

M <- M %>% relocate(replace_check_1, .after = united_keywords) # for the ease of checking
M <- M %>% relocate(replace_check_2, .after = replace_check_1) # for the ease of checking

colnames(M)
M$united_keywords <- M$united_keywords_2
M$united_keywords_2 <- NULL
M$replace_check_1 <- NULL
M$replace_check_2 <- NULL
library(stringr)
M$united_keywords <- M$united_keywords %>% stringr::str_squish()


# There ";;" in M$united_keywords. And also some word;word. I will correct these ";;" and ";" to "; " 

M$united_keywords <- gsub(pattern= ";;", replacement = "; ", M$united_keywords)
M$united_keywords <- gsub(pattern= ";", replacement = "; ", M$united_keywords)
M$united_keywords <- M$united_keywords %>% stringr::str_squish()

# At this point, all word corrections are completed in the AB, TI and united_keywords columns of WoS data, and OGUZOZBAYDELETE's were deleted
# Note that "word correction" means miss spelled words that are not a single token.
# Next step will be the correction of tokens by use of excel correction tables (ie. AB_tokens_corrections_for_STM_and_word_embedding.xlsx)

# EXPLANATION FOR ENVIRONMENT
# Word corrections of AB, TI and united_keywords columns of M completed. 
# Note that "word correction" means miss spelled words that are not a single token (e.g. "trasnform tion" => "transformation")
# In the next step, I will tokenize these corrected AB, TI and united_keywords columns.



#
##
####
####_____
####_____
####_____
####
##
#





########################## file 4: ----
# W_3_token_corrections
# load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W2_word_corrections_Environment.R.RData")
# I deleted all environment date except for M
# At this point, all word corrections are completed in the AB, TI and united_keywords columns of M, and OGUZOZBAYDELETE's were deleted.
# I will tokenize AB, TI and united_keywords columns, then I will correct tokens.


#> STEP-1 Tokenization of word corrections completed AB, TI and united_keywords
library(spacyr)
spacy_install()
spacy_initialize()

system.time({
  M_AB_tokens_2 <- spacy_parse(M$AB, multithread = TRUE, nounphrase = FALSE, lemma= FALSE) #
}) 

system.time({
  M_TI_tokens_2 <- spacy_parse(M$TI, multithread = TRUE, nounphrase = FALSE, lemma= FALSE) #
}) 


system.time({
  M_keyword_tokens_2 <- spacy_parse(M$united_keywords, multithread = TRUE, nounphrase = FALSE, lemma= FALSE) #
}) 

spacy_finalize()


# Data size reduction - START
M_AB_tokens_2$sentence_id <- NULL
M_AB_tokens_2$token_id <- NULL
M_AB_tokens_2$pos <- NULL
M_AB_tokens_2$entity <- NULL

M_TI_tokens_2$sentence_id <- NULL
M_TI_tokens_2$token_id <- NULL
M_TI_tokens_2$pos <- NULL
M_TI_tokens_2$entity <- NULL

M_keyword_tokens_2$sentence_id <- NULL
M_keyword_tokens_2$token_id <- NULL
M_keyword_tokens_2$pos <- NULL
M_keyword_tokens_2$entity <- NULL
# Data size reduction - END

# At this point I got tokenized forms of word corrections completed AB, TI and united_keywords.


#> STEP-2 Correction of tokens
#> As you remember I had tokenized original AB, TI and united_keyword and created token correction excel list.
#> By using these token correction list, I will correct miss spelled tokens.

# Token Correction lists:
AB_tokens_corrections_list # AB_tokens_corrections_list.xlsx
TI_token_correction_list # TI_token_correction_list.xlsx"
keywords_token_correction_list # keywords_token_correction_list.xlsx


#> STEP-2 Correction of tokens of AB --
M_AB_tokens_2$corrected_token <- AB_tokens_corrections_list$to_for_STM[match(M_AB_tokens_2$token,
                                                                             AB_tokens_corrections_list$from)]
M_AB_tokens_2$corrected_token_for_word_embed <- AB_tokens_corrections_list$to_for_word_embedding[match(M_AB_tokens_2$token,
                                                                                                       AB_tokens_corrections_list$from)]
M_AB_tokens_2 %>% filter_all(any_vars(is.na(.)))

#> I checked if is there any NA values. Yes there was.Then I updated AB_tokens_corrections_list
NA_check <- M_AB_tokens_2 %>% filter_all(any_vars(is.na(.)))
openxlsx::write.xlsx(x = NA_check, file = "AB_token_NA_check.xlsx") # I used this file to update "AB_tokens_corrections_list"
AB_tokens_corrections_list$from[227131] <- "" # Last row is "" in excel. How ever R reads it as NA. I replaced NA to ""
AB_tokens_corrections_list$to_for_STM[227131] <- "" # Last row is "" in excel. How ever R reads it as NA
AB_tokens_corrections_list$to_for_word_embedding[227131] <- "" # Last row is "" in excel. How ever R reads it as NA


# Recorrection of tokens with updated AB_tokens_corrections_list
M_AB_tokens_2$corrected_token <- AB_tokens_corrections_list$to_for_STM[match(M_AB_tokens_2$token,
                                                                             AB_tokens_corrections_list$from)]
M_AB_tokens_2$corrected_token_for_word_embed <- AB_tokens_corrections_list$to_for_word_embedding[match(M_AB_tokens_2$token,
                                                                                                       AB_tokens_corrections_list$from)]

# I will correct remaining NA values (there are still some NA values)

M_AB_tokens_2$corrected_token_1 <- ifelse(is.na(M_AB_tokens_2$corrected_token),
                                          M_AB_tokens_2$token, M_AB_tokens_2$corrected_token)

M_AB_tokens_2$corrected_token_for_word_embed_1 <- ifelse(is.na(M_AB_tokens_2$corrected_token_for_word_embed), 
                                                         M_AB_tokens_2$token , M_AB_tokens_2$corrected_token_for_word_embed)

M_AB_tokens_2 %>% filter_all(any_vars(is.na(.))) # OK. There a no NA in corrected_token_1 and corrected_token_for_word_embed_1

colnames(M_AB_tokens_2)

M_AB_tokens_2$corrected_token <- M_AB_tokens_2$corrected_token_1
M_AB_tokens_2$corrected_token_for_word_embed <- M_AB_tokens_2$corrected_token_for_word_embed_1
M_AB_tokens_2$corrected_token_1 <- NULL
M_AB_tokens_2$corrected_token_for_word_embed_1 <- NULL


#> STEP-3 I will untidy corrected_token column of M_AB_tokens_2 ---
M_AB_tokens_2$WOS_no <- gsub(pattern= "text", replacement = "", M_AB_tokens_2$doc_id)
M_AB_tokens_2 <- M_AB_tokens_2 %>% relocate(WOS_no, .before = doc_id) # for ease of evaluation
M_AB_tokens_2$WOS_no <- as.numeric(M_AB_tokens_2$WOS_no)
M_AB_untidy <- M_AB_tokens_2 %>% group_by(WOS_no) %>% summarise(text = paste0(corrected_token, collapse = ' ')) # untidy of "corrected_token"

M_AB_untidy$UT <- M$UT
M_AB_untidy$AB <- M$AB
M_AB_untidy$AB <- M_AB_untidy$text
M_AB_untidy$WOS_no <- NULL
M_AB_untidy$text <- NULL
M_AB_untidy$AB_untidy <- M_AB_untidy$AB
M_AB_untidy$AB <- NULL

M_AB_untidy # All corrections doen (word an token corrections done)

#> STEP-4 I will untidy corrected_token_for_word_embed column of M_AB_tokens_2 ---
M_AB_untidy_word_emb <- M_AB_tokens_2 %>% group_by(WOS_no) %>% 
  summarise(text = paste0(corrected_token_for_word_embed, collapse = ' ')) # untidy of "corrected_token_for_word_embed"

M_AB_untidy_word_emb$UT <- M$UT
M_AB_untidy_word_emb$AB <- M$AB # inserting original AB for checking AB and UT and text (untidied)
M_AB_untidy_word_emb$AB <- M_AB_untidy_word_emb$text
M_AB_untidy_word_emb$WOS_no <- NULL
M_AB_untidy_word_emb$text <- NULL
M_AB_untidy_word_emb$AB_for_word_emb <- M_AB_untidy_word_emb$AB
M_AB_untidy_word_emb$AB <- NULL
M_AB_untidy_word_emb # AB for word embedding. All corrections done (word an token corrections dene)


#> STEP-5 Correction of tokens of TI --
TI_token_correction_list # TI_token_correction_list.xlsx"
M_TI_tokens_2$corrected_token <- TI_token_correction_list$to[match(M_TI_tokens_2$token, TI_token_correction_list$from)]

M_TI_tokens_2 %>% filter_all(any_vars(is.na(.)))
#> I checked if is there any NA values. Yes there was.Then I updated AB_tokens_corrections_list
NA_check_TI_tokens <- M_TI_tokens_2 %>% filter_all(any_vars(is.na(.)))
openxlsx::write.xlsx(x = NA_check_TI_tokens, file = "NA_check_TI_tokens.xlsx") # I used this file to update "AB_tokens_corrections_list"

TI_token_correction_list$from[59190] <- "" # Last row is "" in excel. How ever R reads it as NA. I replaced NA to ""
TI_token_correction_list$to[59190] <- "" # Last row is "" in excel. How ever R reads it as NA

# Recorrection of tokens with updated AB_tokens_corrections_list
M_TI_tokens_2$corrected_token <- TI_token_correction_list$to[match(M_TI_tokens_2$token, TI_token_correction_list$from)]

# I will correct remaining NA values (there are still some NA values)
M_TI_tokens_2$corrected_token_1 <- ifelse(is.na(M_TI_tokens_2$corrected_token),
                                          M_TI_tokens_2$token, M_TI_tokens_2$corrected_token)

M_TI_tokens_2 %>% filter_all(any_vars(is.na(.))) # OK. There a no NA in corrected_token_1 and corrected_token_for_word_embed_1

colnames(M_TI_tokens_2)

M_TI_tokens_2$corrected_token <- M_TI_tokens_2$corrected_token_1
M_TI_tokens_2$corrected_token_1 <- NULL


#> STEP-5.1 I will untidy corrected_token column of M_TI_tokens_2 ---
M_TI_tokens_2$WOS_no <- gsub(pattern= "text", replacement = "", M_TI_tokens_2$doc_id)
M_TI_tokens_2 <- M_TI_tokens_2 %>% relocate(WOS_no, .before = doc_id) # for ease of evaluation
M_TI_tokens_2$WOS_no <- as.numeric(M_TI_tokens_2$WOS_no)
M_TI_untidy <- M_TI_tokens_2 %>% group_by(WOS_no) %>% summarise(TI_untidy = paste0(corrected_token, collapse = ' ')) # untidy of "corrected_token"
M_TI_untidy$UT <- M$UT # for checkig UT 
M_TI_untidy$TI <- M$TI  # for checkig UT 
M_TI_untidy$WOS_no <- NULL

library(dplyr)
M_TI_untidy <- M_TI_untidy %>% relocate(UT, .before = TI_untidy)


#> STEP-6 Correction of tokens of keywords --
keywords_token_correction_list # keywords_token_correction_list.xlsx"
M_keyword_tokens_2$corrected_token <- keywords_token_correction_list$to[match(M_keyword_tokens_2$token, keywords_token_correction_list$from)]
M_keyword_tokens_2 %>% filter_all(any_vars(is.na(.))) # there are 3 NA's

# I will correct NA values
M_keyword_tokens_2$corrected_token_1 <- ifelse(is.na(M_keyword_tokens_2$corrected_token),
                                               M_keyword_tokens_2$token, M_keyword_tokens_2$corrected_token)

M_keyword_tokens_2 %>% filter_all(any_vars(is.na(.))) # OK. There a no NA in corrected_token_1 and corrected_token_for_word_embed_1

colnames(M_keyword_tokens_2)

M_keyword_tokens_2$corrected_token <- M_keyword_tokens_2$corrected_token_1
M_keyword_tokens_2$corrected_token_1 <- NULL


#> STEP-6.1 I will untidy corrected_token column of M_keyword_tokens_2 --
M_keyword_tokens_2$WOS_no <- gsub(pattern= "text", replacement = "", M_keyword_tokens_2$doc_id)
M_keyword_tokens_2 <- M_keyword_tokens_2 %>% relocate(WOS_no, .before = doc_id) # for ease of evaluation
M_keyword_tokens_2$WOS_no <- as.numeric(M_keyword_tokens_2$WOS_no)
M_keywords_untidy <- M_keyword_tokens_2 %>% group_by(WOS_no) %>% summarise(keywords_untidy = paste0(corrected_token, collapse = ' ')) # untidy of "corrected_token"

M_keywords_untidy$united_keywords <- M$united_keywords # for checking
M_keywords_untidy$UT <- M$UT # for checking UT versus united_keywords and keywords_untidy

library(dplyr)
M_keywords_untidy <- M_keywords_untidy %>% relocate(UT, .before = keywords_untidy)
M_keywords_untidy$WOS_no <- NULL
M_keywords_untidy$united_keywords <- NULL

# At this point ALL miss spell corrections of AB, TI, and keywords are completed (ie. all tokens are corrected).


M$AB_untidy <- M_AB_untidy$AB_untidy
M$TI_untidy <- M_TI_untidy$TI_untidy
M$keywords_untidy <- M_keywords_untidy$keywords_untidy

library(dplyr)
M <- M %>% relocate(AB_untidy, .after = AB)
M <- M %>% relocate(TI_untidy, .after = TI)
M <- M %>% relocate(keywords_untidy, .after = united_keywords)


colnames(M)

# EXPLANATIONA FOR ENVIRONMENT
# ENVIRONMENT = load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W3_token_corrections_Environment.RData")
# Token corrections of AB, TI and united_keywords columns of M completed. 

M$AB_untidy # word and token corrections completed text
M$TI_untidy # word and token corrections completed text
M$keywords_untidy # word and token corrections comleted text

# ADDITIONAL A correction STEP---
# Spacy tokenized most of the "5g" as "5 g". tidytext doesn't do that.
# It would be better to tokenize with tidytext. There is nothing to do after this stage.
# However I made am manual correction for 5g and 3 g. (i.e environment is OK)


#
##
####
####_____
####_____
####_____
####
##
#



########################## file 5: ----
# W_4_0_et_al_delete_list
# load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W3_token_corrections_Environment.RData")

library(dplyr)
library(stringr)

# I will specify the names of the authors before the term "et al".
# Below codes may be used to create et_al_delete_list.xlsx

search_term <- str_extract_all(M$AB_untidy, "([^\\s]+)\\s(\\b(et al)\\b)")
system.time({
  ends_with <- data.frame(matrix(unlist(search_term), 
                                 nrow = sum(sapply(search_term, length)),
                                 byrow = TRUE), stringsAsFactors=FALSE)
  colnames(ends_with)[1] <- "term" # for the ease of readingiye
  ends_with  <- ends_with  %>% group_by(term) %>% summarise(number = n()) %>% arrange(desc(number)) # counting the number
  ends_with <- ends_with %>% filter(number > 0)
  print(sum(ends_with$number))
})
openxlsx::write.xlsx(x = ends_with, file = "et_al.xlsx")

search_term <- str_extract_all(M$AB_untidy, "([^\\s]+)\\s((, et al)\\b)")
system.time({
  ends_with <- data.frame(matrix(unlist(search_term), 
                                 nrow = sum(sapply(search_term, length)),
                                 byrow = TRUE), stringsAsFactors=FALSE)
  colnames(ends_with)[1] <- "term" # for the ease of readingiye
  ends_with  <- ends_with  %>% group_by(term) %>% summarise(number = n()) %>% arrange(desc(number)) # counting the number
  ends_with <- ends_with %>% filter(number > 0)
  print(sum(ends_with$number))
})
openxlsx::write.xlsx(x = ends_with, file = "et_al_2.xlsx")



#
##
####
####_____
####_____
####_____
####
##
#


########################## file 6: ----
# W_4_1_et_al_deleting
# load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W3_token_corrections_Environment.RData")

library(dplyr)
M <- M %>% select(UT, AB_untidy)
rownames(M) <- NULL


# STEP-1.1
et_al_delete_list # List for deleting (replacing) et al neighbored words
# NOTE: R does not accept certain characters as word boundaries. For example, those that start/include with "[" or "(", etc.
Old_1 <- et_al_delete_list[["from"]]
New_1 <- et_al_delete_list[["to"]]

library(stringi)
system.time({
  M$AB_untidy_et_al_removed <- stringi::stri_replace_all_regex(
    M$AB_untidy, "\\b"%s+%Old_1%s+%"\\b", New_1, vectorize_all = FALSE)
}) # elapsed



# Now I will check the replacements
M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$AB_untidy, '\\s'), strsplit(M$AB_untidy_et_al_removed, '\\s'))

M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$AB_untidy_et_al_removed, '\\s'), strsplit(M$AB_untidy, '\\s'))

M <- M %>% relocate(replace_check_1, .after = AB_untidy_et_al_removed) # for the ease of checking
M <- M %>% relocate(replace_check_2, .after = replace_check_1) # for the ease of checking
# all replacements are done!
M$replace_check_1 <- NULL
M$replace_check_2 <- NULL

M$AB_untidy_et_al_removed # All words and tokens are corrected and et al is deleted data 
