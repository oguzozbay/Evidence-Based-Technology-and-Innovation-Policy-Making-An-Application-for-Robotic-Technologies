# Explanations for thesis (02 April 2024) - START
# Below code is for generating final_data_naive_bayes
# colnames(final_data_naive_bayes)
# [1] "UT" "robot_relation" "robot_deleted_no_stopword"
# UT is  WOS number, robot_relation is YES or NO, and robot_deleted_no_stopword is the text in which titi and abstract is united
# See also explanations at the end
# Explanations for thesis (02 April 2024) - END

library(dplyr)
library("fastmatch")
library(stringr)
library(stringi)

setwd("C:/Users/ozbay/OneDrive - XXXX/robot_WOS")

# I realized 125 WoS number which are not related to robotics in December 2022.
# Now I will update Naive Bayes Model
# load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_6_2_Naive_Bayes_Lemma_untidy_ready_to_use_in_naive_bayes_analysis.RData")


Naive_Bayes_Lemma_untidy # Ready to Naive Bayes Analysis text (I deleted other two Environment data)
#(i.e. AB, TI and keywords are united, lemmatized, lemmas are corrected, and then lemmas are untidied)
# -------------------------------------------------------------------------------------------------------------------------------------


# DATA preparation for Naive Bayes
# library(readxl)
# WOS_robot_related_or_not_updated_December_2022 <- read_excel("WOS_robot_related_or_not_updated_December_2022.xlsx", 
#                                                             sheet = "robot_relation_update_dec_2022")


robotics_relation_data <- WOS_robot_related_or_not_updated_December_2022 # 30329 WOS numbers' robotics relation data
# I will extract Untidy_of_AB_TI_key_lemmas of 30329 paper. 
# First I will delete  robot" terms.
# Then I will remove stopwords
rm(WOS_robot_related_or_not_updated_December_2022) # I remove unnecesssary data


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
system.time({
Data_for_Naive_Bayes_Model$robot_deleted <- stri_replace_all_regex(Data_for_Naive_Bayes_Model$Untidy_of_AB_TI_key_lemmas,
                                                                     "\\b"%s+%Old_robot%s+%"\\b", 
                                                                     New_robot, vectorize_all = FALSE) # This code DO NOT DELTE, for example, s. and .s
}) # elapsed 14.69

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
# openxlsx::write.xlsx(x = dot_extacted_search, file = "dot_extacted_search_update_december_2022.xlsx") # I will edit manually this excel file


Data_for_Naive_Bayes_Model$robot_deleted_1 <- ""

# library(readxl)
# dot_replace_list <- read_excel("dot_extacted_search_update_december_2022.xlsx", 
#                                sheet = "edit_updated")

dot_replace_list # "dot_extacted_search_update_december_2022.xlsx" (manually edited and updated excel file in december 2022)
colnames(dot_replace_list)
# Because will use REGEX, I have to change (.) to (\\.)
dot_replace_list$from_regex <- gsub(pattern = "\\.", replacement = "\\\\.", dot_replace_list$from)


Old <- dot_replace_list$from_regex
New <- dot_replace_list$to
library(stringi)

system.time({
  Data_for_Naive_Bayes_Model$robot_deleted_1 <- stri_replace_all_regex(Data_for_Naive_Bayes_Model$robot_deleted,
                                                                       paste0("[^\\.]\\b", Old, "\\b"), paste0(" ", New, " "), vectorize_all = FALSE)
}) # elapsed

Data_for_Naive_Bayes_Model$robot_deleted_1 <- str_squish(Data_for_Naive_Bayes_Model$robot_deleted_1)



Data_for_Naive_Bayes_Model$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                                     strsplit(Data_for_Naive_Bayes_Model$robot_deleted, '\\s'),
                                                     strsplit(Data_for_Naive_Bayes_Model$robot_deleted_1, '\\s'))

Data_for_Naive_Bayes_Model$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                                     strsplit(Data_for_Naive_Bayes_Model$robot_deleted_1, '\\s'),
                                                     strsplit(Data_for_Naive_Bayes_Model$robot_deleted, '\\s'))
Data_for_Naive_Bayes_Model$replace_check_1 <- NULL
Data_for_Naive_Bayes_Model$replace_check_2 <- NULL

# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_8_0_Naive_Bayes_updated_text_process_Environment_update_december_2022.RData")
# 12/12/2022


# Now I will replace singe characters. (Below code deletes for example "s" but do not delete ".s" or "s."
# However below code can not be used for deleting words such as "pa.s"
# Because in REGEX "pa.s" P + A + ANY_CHARACTER + S (such as "pass")

Old_1 <- letters # [a-z]
New_1 <- rep(c(" "), each = 26)
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


# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_8_0_Naive_Bayes_updated_text_process_Environment_update_december_2022.RData")
# 12/12/2022



Data_for_Naive_Bayes_Model$replace_check_1 <- NULL
Data_for_Naive_Bayes_Model$replace_check_2 <- NULL
Data_for_Naive_Bayes_Model$robot_deleted <- Data_for_Naive_Bayes_Model$robot_deleted_2
Data_for_Naive_Bayes_Model$robot_deleted_2 <- NULL
Data_for_Naive_Bayes_Model$robot_deleted_1 <- NULL

# EXPLANATIONS FOR ENVIRONMENT:
Data_for_Naive_Bayes_Model$robot_deleted # Updated (i.e 125 new articles added) lemmatized text of (AB+TI+keywords), 
# then "*robot*", dotted words (eg."environment.this") and singe characters (eg. a, b, c ...) deleted.


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
final_data_naive_bayes # ( 30321 obs.) ready-to-use processed data for Naive Bayes Model creation (teaching) (ie. "*robot*", Stopwords and single characters deleted)


# There are some WOS numbers that I have to remove because of several reasons (eg. too long, too short, meaningless, etc.)
# I will chech if there are any of WOS numbers to be deleted, an I will delete existing ones.
WOS_delete_list # list of WOS numbers to be deleted ("delete_abstract.xlsx")
WOS_delete_list <- WOS_delete_list$UT
final_data_naive_bayes <- final_data_naive_bayes[!final_data_naive_bayes$UT %in% WOS_delete_list, ]

final_data_naive_bayes # ( 30312 obs.) ready-to-use processed data for Naive Bayes Model Creation (ie. "*robot*", Stopwords and single characters deleted)

# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_8_0_Naive_Bayes_updated_text_process_Environment_update_december_2022.RData")
# 12/12/2022


#
###
##### # 02 April 2024 (I added below lines while I am editing the code for my thesis)
###
#

# load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_8_0_Naive_Bayes_updated_text_process_Environment_update_december_2022.RData")
colnames(final_data_naive_bayes)
# > colnames(final_data_naive_bayes)
# [1] "UT"                        "robot_relation"            "robot_deleted_no_stopword"
final_data_naive_bayes[1:3, 1:2]
final_data_naive_bayes[1:3, 1:2]
# UT robot_relation
# 1 WOS:000232003500130            YES
# 2 WOS:000232003500158             NO
# 3 WOS:000232003500184             NO

final_data_naive_bayes[1,3]
final_data_naive_bayes[1,3]
# [1] "home healthcare monitoring system evolutionary hybrid control architecture pet autonomous pet base interactive home healthcare system use entertain pet home healthcare monitoring system new idea feature research project report paper structure feature control system home healthcare system present hybrid evolutionary control architecture virtue ai base behavior base control architecture propose design architecture endow ability learn adaptation quick reactive speed rationality ability achieve give task design home healthcare system make sure pet reliably accomplish vital physiology datum collection datum transmission healthcare task strong situation adaptability low cost high robustness system can apply home master suffer chronic disease handicap master fragile elderly master"



