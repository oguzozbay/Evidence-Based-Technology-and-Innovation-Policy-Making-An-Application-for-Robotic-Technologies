memory.limit()
memory.limit(size = NA)
memory.limit(size=9999999999999)
gc()

setwd("C:/Users/ozbay/OneDrive - XXXX/robot_WOS")

library(dplyr)
library(stringr)

# I will use for naive bayes for determining robot related and unrelated abstracts.
# First I will clean punctuation and OGUZOZBAYDELETE's in M.


# load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/AB_untidy_et_al_and_tailgrams_deleted_text_input.RData")
# load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W3_token_corrections_Environment.RData")
# I deleted all data in environment except:

AB_untidy_et_al_and_tailgrams_deleted_text_input # tail-grams deleted abstract
M_TI_untidy # words and tokens corrected titles
M_keywords_untidy # words and tokens corrected keywords

M_naive_bayes <- M_TI_untidy
M_naive_bayes$TI <- NULL
M_naive_bayes$AB_tails_deleted <- AB_untidy_et_al_and_tailgrams_deleted_text_input$AB_untidy_et_al_and_tailgrams_removed
M_naive_bayes$keywords_untidy <- M_keywords_untidy$keywords_untidy


# I will unite keywords, titles and abstracts to a single cell (column named = united_key_TI_AB)
library(tidyr)
colnames(M_naive_bayes)
M_naive_bayes <- unite(M_naive_bayes, united_key_TI_AB, keywords_untidy, TI_untidy, AB_tails_deleted, sep = "; ", remove = FALSE, na.rm = FALSE)
M_naive_bayes$key_oguz <- 1:nrow(M_naive_bayes) # this key will be required for freezing (protecting) WOS numbers order
M_naive_bayes <- M_naive_bayes %>% relocate(key_oguz, .before = UT)


# Now I will tokenize M_naive_bayes$united_key_TI_AB
library(tidytext)
# text_df %>% unnest_tokens(word, text) # word= name for new column, text= column to tokenize
tokenized <- M_naive_bayes %>% unnest_tokens(token, united_key_TI_AB, strip_punct = FALSE)
singular_tokens <- tokenized %>% group_by(token) %>% summarise(repeats = n())

# openxlsx::write.xlsx(x = singular_tokens, file = "singular_tokens_AB_TI_keys_united_for_naive_bayes.xlsx") 
token_delete_list_AB_TI_keys # token delete list is created using above excel file
# token_delete_list_AB_TI_keys$to[is.na(token_delete_list_AB_TI_keys$to)] <- ""
token_delete_list_AB_TI_keys$to <- ""


tokenized$deleted_token <- token_delete_list_AB_TI_keys$to[match(tokenized$token, token_delete_list_AB_TI_keys$from)]
# I will correct NA values
tokenized$token_corrected <- ifelse(is.na(tokenized$deleted_token), tokenized$token, tokenized$deleted_token)

tokenized$token <- tokenized$token_corrected
tokenized$token_corrected <- NULL
tokenized$deleted_token <- NULL


# I will untidy tokenized text
library(tidyverse)
library(tidytext)
AB_TI_key_untidied <- tokenized %>%
  group_by(key_oguz, UT, TI_untidy, AB_tails_deleted, keywords_untidy) %>%
  summarize(key_TI_AB_untidy = str_c(token, collapse = " ")) %>%
  ungroup()


# NOW I will delete "oguz_ozbay_na", "oguzozbaydelete" and "oguzozbaydeleteabstract"
# NOTE that, "oguzozbaydeleteabstract" HAVE TO BE REPLACED BEFORE "oguzozbaydelete"
AB_TI_key_untidied$key_TI_AB_untidy <- gsub(pattern = "oguzozbaydeleteabstract", 
                                                 replacement = "", AB_TI_key_untidied$key_TI_AB_untidy)
AB_TI_key_untidied$key_TI_AB_untidy <- gsub(pattern = "oguzozbaydelete", 
                                                 replacement = "", AB_TI_key_untidied$key_TI_AB_untidy)
AB_TI_key_untidied$key_TI_AB_untidy <- gsub(pattern = "oguz_ozbay_na", 
                                                 replacement = "", AB_TI_key_untidied$key_TI_AB_untidy)

Naive_Bayes_Text <- AB_TI_key_untidied %>% select (UT, key_TI_AB_untidy)
Naive_Bayes_Text$key_TI_AB_untidy <- Naive_Bayes_Text$key_TI_AB_untidy %>% stringr::str_squish()

# Now I will delete all environment except for Naive_Bayes_Text
Naive_Bayes_Text # united AB, TI and keywords ready to lemmatize


# Now I will lemmatize "Naive_Bayes_Text"
library(spacyr)
spacy_install()
spacy_initialize()

system.time({
  Naive_Bayes_Lemmatized <- spacy_parse(Naive_Bayes_Text$key_TI_AB_untidy, multithread = TRUE, nounphrase = FALSE, lemma= TRUE)
}) 

# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_6_0_data_for_naive_bayes_Environment.RData")

# I will import correction list for lemmas.
# I created this list manually in excel.
lemma_corrections_of_AB_words_and_tokens_replaced #lemma_corrections_of_AB_words_and_tokens_replaced.xlsx
lemma_correction_list <- lemma_corrections_of_AB_words_and_tokens_replaced # for ease of reading I renamed lemma correction list
rm(lemma_corrections_of_AB_words_and_tokens_replaced)


Naive_Bayes_Lemmatized$sentence_id <- NULL
Naive_Bayes_Lemmatized$token_id <- NULL
Naive_Bayes_Lemmatized$pos <- NULL
Naive_Bayes_Lemmatized$entity <- NULL


# I will replace lemmas according to lemma correction list
Naive_Bayes_Lemmatized$corrected_lemma <- lemma_correction_list$to[match(Naive_Bayes_Lemmatized$lemma,
                                                                                         lemma_correction_list$from)]
Naive_Bayes_Lemmatized$lemma_final <- ifelse(is.na(Naive_Bayes_Lemmatized$corrected_lemma),
                                             Naive_Bayes_Lemmatized$lemma, Naive_Bayes_Lemmatized$corrected_lemma)

colnames(Naive_Bayes_Lemmatized)
Naive_Bayes_Lemmatized$WOS_no <- gsub(pattern= "text", replacement = "", Naive_Bayes_Lemmatized$doc_id)
Naive_Bayes_Lemmatized <- Naive_Bayes_Lemmatized %>% relocate(WOS_no, .before = doc_id) # for ease of evaluation
Naive_Bayes_Lemmatized$WOS_no <- as.numeric(Naive_Bayes_Lemmatized$WOS_no)

Naive_Bayes_Lemma_untidy <- Naive_Bayes_Lemmatized %>% group_by(WOS_no) %>% summarise(Untidy_of_AB_TI_key_lemmas = paste0(lemma_final, collapse = ' ')) # untidy of "lemma_final"


library(stringr)
Naive_Bayes_Lemma_untidy$Untidy_of_AB_TI_key_lemmas <- Naive_Bayes_Lemma_untidy$Untidy_of_AB_TI_key_lemmas %>% stringr::str_squish()

# ENVIRONMENT EXPLANATIONS -------------------------------------
Naive_Bayes_Text # united AB, TI and keywords ready to lemmatize
Naive_Bayes_Lemma_untidy # lemmatized and untidyed text

# I will make a fast manual check if WOS numbers are correct by comparing "AB_TI_key_lemmatized" and "original text"
Naive_Bayes_Lemma_untidy$UT <- Naive_Bayes_Text$UT
Naive_Bayes_Lemma_untidy$semi_original_text <- Naive_Bayes_Text$key_TI_AB_untidy
Naive_Bayes_Lemma_untidy$Untidy_of_AB_TI_key_lemmas <- tolower(Naive_Bayes_Lemma_untidy$Untidy_of_AB_TI_key_lemmas) # to warranty there are no lower cases

# It seem OK.

Naive_Bayes_Lemma_untidy$WOS_no <- NULL
Naive_Bayes_Lemma_untidy$semi_original_text <- NULL
Naive_Bayes_Lemma_untidy <- Naive_Bayes_Lemma_untidy %>% relocate(UT, .before = Untidy_of_AB_TI_key_lemmas)

# FINAL DATA:
Naive_Bayes_Lemma_untidy # Ready to Naive Bayes Analysis text 
#(i.e. AB, TI and keywords are united, lemmatized, lemmas are corrected, and then lemmas are untidied)
# C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_6_2_Naive_Bayes_Lemma_untidy_ready_to_use_in_naive_bayes_analysis.RData

# -------------------------------------------------------------------------------------------------------------------------------------



# DATA preperation for Naive Bayes
robotics_relation_data # 27553 WOS numbers' robotics relation data ("WOS_robot_related_or_not_Aug_2022.xlsx")
# I will extract Untidy_of_AB_TI_key_lemmas of 27553 paper. 
# First I will delete  robot" terms.
# Then I will remove stopwords

library("fastmatch")
compare_rows <- Naive_Bayes_Lemma_untidy$UT %fin% robotics_relation_data$UT  # the %fin% function comes from the `fastmatch` package
Data_for_Naive_Bayes_Model <- Naive_Bayes_Lemma_untidy[which(compare_rows == TRUE),]
# I will insert YES or NO data
Data_for_Naive_Bayes_Model$robot_relation <- robotics_relation_data$relation[match(Data_for_Naive_Bayes_Model$UT, 
                                                                                   robotics_relation_data$UT)]

# Now I will extract all words that include "robot"
search_term <- str_extract_all(Data_for_Naive_Bayes_Model$Untidy_of_AB_TI_key_lemmas, "[^\\s]*robot[^\\s]*")
extacted_search_terms <- data.frame(matrix(unlist(search_term), 
                                 nrow = sum(sapply(search_term, length)),
                                 byrow = TRUE), stringsAsFactors=FALSE)
rm(search_term)
colnames(extacted_search_terms )[1] <- "term" # to make readable
extacted_search_terms  <- extacted_search_terms  %>% group_by(term) %>% 
  summarise(number = n()) %>% arrange(desc(number)) # counting

extacted_search_terms # replacement list (ie. robot terms)
extacted_search_terms$to <- " "
extacted_search_terms <- rename(extacted_search_terms, from = term)


# NOW I will delete robot's

Old_robot <- extacted_search_terms$from
New_robot <- extacted_search_terms$to
Data_for_Naive_Bayes_Model$robot_deleted <- ""
library(stringi)
Data_for_Naive_Bayes_Model$robot_deleted <- stri_replace_all_regex(Data_for_Naive_Bayes_Model$Untidy_of_AB_TI_key_lemmas,
                                                                     "\\b"%s+%Old_robot%s+%"\\b", 
                                                                     New_robot, vectorize_all = FALSE) # DO NOT DELTE for example s. and .s

Data_for_Naive_Bayes_Model$robot_deleted <- str_squish(Data_for_Naive_Bayes_Model$robot_deleted) # deletion of unnecessary spaces

Data_for_Naive_Bayes_Model$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                                     strsplit(Data_for_Naive_Bayes_Model$Untidy_of_AB_TI_key_lemmas, '\\s'), strsplit(Data_for_Naive_Bayes_Model$robot_deleted, '\\s'))

Data_for_Naive_Bayes_Model$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                                     strsplit(Data_for_Naive_Bayes_Model$robot_deleted, '\\s'), strsplit(Data_for_Naive_Bayes_Model$Untidy_of_AB_TI_key_lemmas, '\\s'))


# I have forgotten to replace (-) ---------------------
# Now I will replace it
Data_for_Naive_Bayes_Model$robot_deleted <- gsub(pattern= "-", replacement = " ", Data_for_Naive_Bayes_Model$robot_deleted)

Data_for_Naive_Bayes_Model$robot_deleted <- str_squish(Data_for_Naive_Bayes_Model$robot_deleted) # remove unnecessary white spaces

Data_for_Naive_Bayes_Model$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(Data_for_Naive_Bayes_Model$Untidy_of_AB_TI_key_lemmas, '\\s'), strsplit(Data_for_Naive_Bayes_Model$robot_deleted, '\\s'))

Data_for_Naive_Bayes_Model$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(Data_for_Naive_Bayes_Model$robot_deleted, '\\s'), strsplit(Data_for_Naive_Bayes_Model$Untidy_of_AB_TI_key_lemmas, '\\s'))


Data_for_Naive_Bayes_Model$replace_check_1 <- NULL
Data_for_Naive_Bayes_Model$replace_check_2 <- NULL

# There are some dots between words. I will extract them
dot_seach <- str_extract_all(Data_for_Naive_Bayes_Model$robot_deleted, "\\b[^\\s]*\\D\\.\\D[^\\s]*\\b") # no digit
dot_extacted_search <- data.frame(matrix(unlist(dot_seach), 
                                         nrow = sum(sapply(dot_seach, length)),
                                         byrow = TRUE), stringsAsFactors=FALSE)

colnames(dot_extacted_search )[1] <- "term" # to make readable
dot_extacted_search  <- dot_extacted_search  %>% group_by(term) %>% 
  summarise(number = n()) %>% arrange(desc(number)) # counting
# openxlsx::write.xlsx(x = dot_extacted_search, file = "dot_extacted_search.xlsx") # I will edit manuallly this excel file


# replacement of dotted words and single characters (new CODE) - START -----------------

Data_for_Naive_Bayes_Model$robot_deleted_1 <- ""

dot_replace_list # dot_extacted_search.xlsx (manually edited excel file)
colnames(dot_replace_list)
dot_replace_list$from_regex <- gsub(pattern = "\\.", replacement = "\\\\.", dot_replace_list$from)
# Because will use REGEX, I have to change (.) to (\\.)

Old <- dot_replace_list$from_regex
New <- dot_replace_list$to
library(stringi)


# WOS:000582558200050 (there was an error in replacing and I solved it. Not a necessary line. Delete this later. 18/08/2022)
system.time({
  Data_for_Naive_Bayes_Model$robot_deleted_1 <- stri_replace_all_regex(Data_for_Naive_Bayes_Model$robot_deleted,
                                                                       paste0("[^\\.]\\b", Old, "\\b"), paste0(" ", New, " "), vectorize_all = FALSE)
}) # elapsed 100

Data_for_Naive_Bayes_Model$robot_deleted_1 <- str_squish(Data_for_Naive_Bayes_Model$robot_deleted_1)



Data_for_Naive_Bayes_Model$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                                     strsplit(Data_for_Naive_Bayes_Model$robot_deleted, '\\s'),
                                                     strsplit(Data_for_Naive_Bayes_Model$robot_deleted_1, '\\s'))

Data_for_Naive_Bayes_Model$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                                     strsplit(Data_for_Naive_Bayes_Model$robot_deleted_1, '\\s'),
                                                     strsplit(Data_for_Naive_Bayes_Model$robot_deleted, '\\s'))
Data_for_Naive_Bayes_Model$replace_check_1 <- NULL
Data_for_Naive_Bayes_Model$replace_check_2 <- NULL

# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_6_5_Naive_Bayes_text_process_Environment.RData")
# 18/08/2022


# Now I will replace singe characters. (Below code deletes for example "s" but do not delete ".s" or "s."
# However below code can not be used for deleting words such as "pa.s"
# Because in REGEX "pa.s" P + A + ANY_CHARACTER + S (such as "pass")
delete_list_single_character
Old_1 <- delete_list_single_character$from
New_1 <- delete_list_single_character$to
Data_for_Naive_Bayes_Model$robot_deleted_2 <- ""
library(stringi)
Data_for_Naive_Bayes_Model$robot_deleted_2 <- stri_replace_all_regex(Data_for_Naive_Bayes_Model$robot_deleted_1,
                                                                    "[^\\.]\\b"%s+%Old_1%s+%"(?!\\.)\\b", 
                                                                    paste0(" ", New_1, " "), vectorize_all = FALSE) # Does not delete, for example, s. and .s

Data_for_Naive_Bayes_Model$robot_deleted_2 <- str_squish(Data_for_Naive_Bayes_Model$robot_deleted_2) # deletion of unnecessary spaces

Data_for_Naive_Bayes_Model$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                                     strsplit(Data_for_Naive_Bayes_Model$robot_deleted_1, '\\s'),
                                                     strsplit(Data_for_Naive_Bayes_Model$robot_deleted_2, '\\s'))

Data_for_Naive_Bayes_Model$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                                     strsplit(Data_for_Naive_Bayes_Model$robot_deleted_2, '\\s'),
                                                     strsplit(Data_for_Naive_Bayes_Model$robot_deleted_1, '\\s'))


# 18/08/2022
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_6_5_Naive_Bayes_text_process_Environment.RData")




Data_for_Naive_Bayes_Model$replace_check_1 <- NULL
Data_for_Naive_Bayes_Model$replace_check_2 <- NULL
Data_for_Naive_Bayes_Model$robot_deleted <- Data_for_Naive_Bayes_Model$robot_deleted_2
Data_for_Naive_Bayes_Model$robot_deleted_2 <- NULL
Data_for_Naive_Bayes_Model$robot_deleted_1 <- NULL

# EXPLANATIONS FOR ENVIRONMENT:
Data_for_Naive_Bayes_Model$robot_deleted # lemmatized text of (AB+TI+keywords) then "*robot*", dotted words (eg."environment.this") and singe characters (eg. a, b, c ...) deleted.
# replacement of dotted words and single characters (new CODE) - END ------------------



# Now I will delete stopwords (STOPWORDS REMOVAL) -----
library(tidytext)
library(tidyverse)
stopwords_for_naive_bayes # "stopwords_for_naive_bayes.xlsx"
# NOTE that I created this stopwords list manually by basically use of stopwords-smart.xlsx and stopwords-snowball.
# This list do not include words such as "over" (because of over-actuated term).
stopwords_to_remove <- stopwords_for_naive_bayes$from
stopwords_removal <- Data_for_Naive_Bayes_Model
stopwords_removal$key <- 1:nrow(stopwords_removal) # This will be used for untidy process
stopwords_removal <- stopwords_removal %>% unnest_tokens(tokenized, robot_deleted, strip_punct = FALSE)
stopwords_removal <- stopwords_removal[!(stopwords_removal$tokenized %in% stopwords_to_remove),]
stopwords_removal <- stopwords_removal %>%
  group_by(key, UT, Untidy_of_AB_TI_key_lemmas, robot_relation) %>%
  summarise(stopwords_deleted_text = paste0(tokenized, collapse = ' ')) %>% ungroup()

colnames(stopwords_removal)
stopwords_removal$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "),
                                            strsplit(stopwords_removal$Untidy_of_AB_TI_key_lemmas, '\\s'),
                                            strsplit(stopwords_removal$stopwords_deleted_text, '\\s'))

stopwords_removal$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "),
                                            strsplit(stopwords_removal$stopwords_deleted_text, '\\s'),
                                            strsplit(stopwords_removal$Untidy_of_AB_TI_key_lemmas, '\\s'))

# there are some (') remained in  replace_check_2
stopwords_removal$stopwords_deleted_text <- gsub(pattern= "'", replacement = " ", 
                                                 stopwords_removal$stopwords_deleted_text)
stopwords_removal$stopwords_deleted_text <- str_squish(stopwords_removal$stopwords_deleted_text)

stopwords_removal$replace_check_1 <- NULL
stopwords_removal$replace_check_2 <- NULL

Data_for_Naive_Bayes_Model$UT_check <- stopwords_removal$UT
Data_for_Naive_Bayes_Model$robot_deleted_no_stopword <- stopwords_removal$stopwords_deleted_text


final_data_naive_bayes <- Data_for_Naive_Bayes_Model

colnames(final_data_naive_bayes)
final_data_naive_bayes$robot_deleted <- NULL
final_data_naive_bayes$UT_check <- NULL
final_data_naive_bayes$Untidy_of_AB_TI_key_lemmas <- NULL
final_data_naive_bayes # ( 27545 obs.) ready-to-use processed data for Naive Bayes Model creation (teaching) (ie. "*robot*", Stopwords and single characters deleted)


# There are some WOS numbers that I have to remove because of several reasons (eg. too long, too short, meaningless, etc.)
# I will chech if there are any of WOS numbers to be deleted, an I will delete existing ones.
WOS_delete_list # list of WOS numbers to be deleted ("delete_abstract.xlsx")
WOS_delete_list <- WOS_delete_list$UT
final_data_naive_bayes <- final_data_naive_bayes[!final_data_naive_bayes$UT %in% WOS_delete_list, ]

final_data_naive_bayes # ( 27566 obs.) ready-to-use processed data for Naive Bayes Model Creation (ie. "*robot*", Stopwords and single characters deleted)

# 18/08/2022
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_6_5_1_Naive_Bayes_text_process_Environment.RData")
