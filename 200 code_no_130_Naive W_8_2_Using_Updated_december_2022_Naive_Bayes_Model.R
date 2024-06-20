library(dplyr)
library("fastmatch")
library(stringr)
library(stringi)

load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_8_1_Naive_Bayes_MODEL_creation_Environment_updated_december_2022.RData")

# I will remove all data in environment except for Navive_B_text_model_30312_K_fold

Navive_B_text_model_30312_K_fold # Naive Bayess Classification text model which was updated in 13 December 2022

# processed text: stop-words removed, single characters removed and also lemmatized

# STEP-1: I will define abstracts which do not include "robot" in AB, TI and keywords.
# I had two WOS raw data files. 
# One is "TOPIC = robot" (ie. at least AB, TI or keywords include robot)
# Second is "RESEARCH AREA = robotics", which may not include "*robot*" in AB, TI or keywords.
# I will use both data. Because some robots may be deleted in first data (i.e: TOPIC = robot) during tail gram deletion etc. 

# load("C:/Users/ozbay/OneDrive - XXXX/WoS data Mart 2022/WoS_data_2005_01_Mart_2022_Research_area_Robot.RData")

# Below is for getting "Naive_Bayes_Lemma_untidy"
# load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_6_2_Naive_Bayes_Lemma_untidy_ready_to_use_in_naive_bayes_analysis.RData")
Naive_Bayes_Lemma_untidy # (AB, TI and keywords are united, lemmatized, lemmas corrected, and then untidied)
# Naive_Bayes_Lemma_untidy includes all WOS corpus (ie. 258660 articles)


library(dplyr)
M # 2005 - 2022 publication year, 175724 articles from WOS which are in robotics RESEARCH AREA
M <- M %>% select(UT)
M_SC_robot <- M # SC is abbreviation of WOS for Research Area
rownames(M_SC_robot) <- NULL
rm(M)

# 13/12/2022
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_8_2_Using_Naive_Bayes_Model_Environment_updated_december_2022.RData")


# load("C:/Users/ozbay/OneDrive - XXXX/WoS data Mart 2022/WoS_data_2005_01_Mart_2022_Topic_Robot_United.RData")
M # 2005 - 2022 publication year, 127209 articles from WOS whose Topic is "robot" (i.e. abstract title or keyword includes robot)
M <- M %>% select(UT)
M_Topic_robot <- M 
rownames(M_Topic_robot) <- NULL
rm(M)


# I will extract text data of articles in which "topic = robot"
library("fastmatch")
compare_rows_topic <- Naive_Bayes_Lemma_untidy$UT %fin% M_Topic_robot$UT  # the %fin% function comes from the `fastmatch` package
Topic_Robot_WOS_Numbers <- Naive_Bayes_Lemma_untidy[which(compare_rows_topic == TRUE),]

# Now I will find text that do not include "robot"
Topic_Robot_WOS_Numbers <- within(Topic_Robot_WOS_Numbers,{robot_exists = as.numeric(grepl("robot", fixed = FALSE, 
                                                                                           Topic_Robot_WOS_Numbers$Untidy_of_AB_TI_key_lemmas))})
# if "robot" exists, Topic_Robot_WOS_Numbers$robot_exists = 1)

NO_robot_Topic_Robot_WOS_Numbers <- Topic_Robot_WOS_Numbers %>% filter(robot_exists <1)
NO_robot_Topic_Robot_WOS_Numbers$robot_exists <- NULL
# Articles in robot topic and which do not include "robot"



# I will extract text data of articles in which "Research Area = robotic"
library("fastmatch")
compare_rows <- Naive_Bayes_Lemma_untidy$UT %fin% M_SC_robot$UT  # the %fin% function comes from the `fastmatch` package
SC_Robotics_WOS_Numbers <- Naive_Bayes_Lemma_untidy[which(compare_rows == TRUE),]
# Now I will find text that do not include "robot"
SC_Robotics_WOS_Numbers <- within(SC_Robotics_WOS_Numbers,{robot_exists = as.numeric(grepl("robot", fixed = FALSE, 
                                                                        SC_Robotics_WOS_Numbers$Untidy_of_AB_TI_key_lemmas))})
# if "robot" exists, SC_Robotics_WOS_Numbers$robot_exists = 1)

NO_robot_SC_Robotics_WOS_Numbers <- SC_Robotics_WOS_Numbers %>% filter(robot_exists <1)
NO_robot_SC_Robotics_WOS_Numbers$robot_exists <- NULL
# Articles in Resarch Area= robotics and which do not include "robot"


# I will also take data from TOPIC = robot but on only ID (Keywords Plus) includes robot.
# load("C:/Users/ozbay/OneDrive - XXXX/WoS data Mart 2022/WoS_data_2005_01_Mart_2022_Topic_Robot_United.RData")
M <- M %>% select(UT, AB, TI, DE, ID)
cols <- c("AB", "TI", "ID", "DE")
M [cols] <- lapply(M [cols], tolower)
rownames(M) <- NULL
M <- within(M,{robot_exists_AB = as.numeric(grepl("robot", fixed = FALSE, M$AB))}) # if "robot" exists, M$robot_exists_AB = 1)
M <- within(M,{robot_exists_TI = as.numeric(grepl("robot", fixed = FALSE, M$TI))})
M <- within(M,{robot_exists_DE = as.numeric(grepl("robot", fixed = FALSE, M$DE))})
M <- within(M,{robot_exists_ID = as.numeric(grepl("robot", fixed = FALSE, M$ID))})
M$sum <- rowSums(M[,c(6:9)])
M_ID_O <- M %>% filter(sum < 2)
M_ID_O <- M_ID_O %>% filter(robot_exists_ID == 1) # Articles in which, only ID includes "robot"
rm(M)

# 13/12/2022
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_8_2_Using_Naive_Bayes_Model_Environment_updated_december_2022.RData")

x <- NO_robot_SC_Robotics_WOS_Numbers %>% select(UT)
y <- M_ID_O %>% select(UT)
z <- rbind(x,y)
rm(x,y)  
y1 <- NO_robot_Topic_Robot_WOS_Numbers %>% select(UT)
Final_WOS_numbers_no_robot_include <- rbind(z,y1) # 92204 obs.
rm(z,y1) 

Final_WOS_numbers_no_robot_include <- Final_WOS_numbers_no_robot_include %>% distinct(UT, .keep_all = TRUE) # removal of duplicated WOS
# 91986 obs.



# I will extract text data of above defined articles using their WOS numbers.
library("fastmatch")
compare_rows_no_robot <- Naive_Bayes_Lemma_untidy$UT %fin% Final_WOS_numbers_no_robot_include$UT 
# the %fin% function comes from the `fastmatch` package

Unknown_robotic_relation_articles <- Naive_Bayes_Lemma_untidy[which(compare_rows_no_robot == TRUE),]
Unknown_robotic_relation_articles # Articles which I will find if are they  related to robotics or not.


# Now I will extract all words that include "robot".
# Those robots come from ID (Keywords Plus) that includes robot (ie. M_ID_O).
library(stringr)
search_term <- str_extract_all(Unknown_robotic_relation_articles$Untidy_of_AB_TI_key_lemmas, "[^\\s]*robot[^\\s]*")
extacted_search_terms <- data.frame(matrix(unlist(search_term), 
                                           nrow = sum(sapply(search_term, length)),
                                           byrow = TRUE), stringsAsFactors=FALSE)
rm(search_term)
colnames(extacted_search_terms )[1] <- "term" # to make readable
extacted_search_terms  <- extacted_search_terms  %>% group_by(term) %>% 
  summarise(number = n()) %>% arrange(desc(number)) # counting

extacted_search_terms$to <- " "
extacted_search_terms <- rename(extacted_search_terms, from = term)

# 13/12/2022
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_8_2_Using_Naive_Bayes_Model_Environment_updated_december_2022.RData")

Unknown_robotic_relation_articles$Untidy_of_AB_TI_key_lemmas
# NOW I will delete robot's

Old_robot <- extacted_search_terms$from
New_robot <- extacted_search_terms$to
Unknown_robotic_relation_articles$robot_deleted <- ""
library(stringi)
Unknown_robotic_relation_articles$robot_deleted <- stri_replace_all_regex(Unknown_robotic_relation_articles$Untidy_of_AB_TI_key_lemmas,
                                                                   "\\b"%s+%Old_robot%s+%"\\b", 
                                                                   New_robot, vectorize_all = FALSE) # this code does not delete, for example, s. and .s

Unknown_robotic_relation_articles$robot_deleted <- str_squish(Unknown_robotic_relation_articles$robot_deleted) # deletion of unnecessary spaces

Unknown_robotic_relation_articles$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                                     strsplit(Unknown_robotic_relation_articles$Untidy_of_AB_TI_key_lemmas, '\\s'),
                                                     strsplit(Unknown_robotic_relation_articles$robot_deleted, '\\s'))

Unknown_robotic_relation_articles$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                                     strsplit(Unknown_robotic_relation_articles$robot_deleted, '\\s'),
                                                     strsplit(Unknown_robotic_relation_articles$Untidy_of_AB_TI_key_lemmas, '\\s'))

# I have forgotten to replace (-)
Unknown_robotic_relation_articles$robot_deleted <- gsub(pattern= "-", replacement = " ", Unknown_robotic_relation_articles$robot_deleted)
Unknown_robotic_relation_articles$robot_deleted <- str_squish(Unknown_robotic_relation_articles$robot_deleted) # remove unnecessary white spaces

Unknown_robotic_relation_articles$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                                     strsplit(Unknown_robotic_relation_articles$Untidy_of_AB_TI_key_lemmas, '\\s'), 
                                                     strsplit(Unknown_robotic_relation_articles$robot_deleted, '\\s'))

Unknown_robotic_relation_articles$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                                     strsplit(Unknown_robotic_relation_articles$robot_deleted, '\\s'),
                                                     strsplit(Unknown_robotic_relation_articles$Untidy_of_AB_TI_key_lemmas, '\\s'))


Unknown_robotic_relation_articles$replace_check_1 <- NULL
Unknown_robotic_relation_articles$replace_check_2 <- NULL



# There are some dots between words. I will extract them -----
dot_seach <- str_extract_all(Unknown_robotic_relation_articles$robot_deleted, "\\b[^\\s]*\\D\\.\\D[^\\s]*\\b") # no digit
dot_extacted_search <- data.frame(matrix(unlist(dot_seach), 
                                         nrow = sum(sapply(dot_seach, length)),
                                         byrow = TRUE), stringsAsFactors=FALSE)

colnames(dot_extacted_search )[1] <- "term" # to make readable
dot_extacted_search  <- dot_extacted_search  %>% group_by(term) %>% 
  summarise(number = n()) %>% arrange(desc(number)) # counting

# openxlsx::write.xlsx(x = dot_extacted_search, file = "dot_extacted_search_no_robot_corpus_updated_December_2022.xlsx")
# I will replace some dotted word after manually editing above excel

# library(readxl)
# dot_replace_list_No_robot_included_Corpus <- read_excel("dot_extacted_search_no_robot_corpus_updated_December_2022.xlsx", sheet = "edit")

dot_replace_list_No_robot_included_Corpus # replacement list for dotted words (dot_extacted_search_no_robot_corpus_updated_December_2022.xlsx)
# Remember that since )  will use REGEX, I have to change (.) to (\\.)

dot_replace_list_No_robot_included_Corpus$to[is.na(dot_replace_list_No_robot_included_Corpus$to)] <- ""


dot_replace_list_No_robot_included_Corpus$from_regex <- gsub(pattern = "\\.", replacement = "\\\\.", dot_replace_list_No_robot_included_Corpus$from)

Old <- dot_replace_list_No_robot_included_Corpus$from_regex
New <- dot_replace_list_No_robot_included_Corpus$to
library(stringi)
system.time({
  Unknown_robotic_relation_articles$robot_deleted_1 <- stri_replace_all_regex(Unknown_robotic_relation_articles$robot_deleted,
                                                                       paste0("[^\\.]\\b", Old, "\\b"), paste0(" ", New, " "), 
                                                                       vectorize_all = FALSE)
}) # elapsed 2341.23  approx. 




Unknown_robotic_relation_articles$robot_deleted_1 <- str_squish(Unknown_robotic_relation_articles$robot_deleted_1)



Unknown_robotic_relation_articles$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                                     strsplit(Unknown_robotic_relation_articles$robot_deleted, '\\s'),
                                                     strsplit(Unknown_robotic_relation_articles$robot_deleted_1, '\\s'))

Unknown_robotic_relation_articles$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                                     strsplit(Unknown_robotic_relation_articles$robot_deleted_1, '\\s'),
                                                     strsplit(Unknown_robotic_relation_articles$robot_deleted, '\\s'))

Unknown_robotic_relation_articles$replace_check_1 <- NULL
Unknown_robotic_relation_articles$replace_check_2 <- NULL


# Now I will replace singe characters. (Below code deletes for example "s" but do not delete ".s" or "s." -----
# However below code CAN NOT be used for deleting words such as "pa.s"
# Because in REGEX "pa.s" P + A + ANY_CHARACTER + S (such as "pass")
Old_1 <- letters # [a-z]
New_1 <- rep(c(" "), each = 26)
Unknown_robotic_relation_articles$robot_deleted_2 <- ""
library(stringi)
Unknown_robotic_relation_articles$robot_deleted_2 <- stri_replace_all_regex(Unknown_robotic_relation_articles$robot_deleted_1,
                                                                     "[^\\.]\\b"%s+%Old_1%s+%"(?!\\.)\\b", 
                                                                     paste0(" ", New_1, " "), vectorize_all = FALSE) # Does not delete, for example, s. and .s

Unknown_robotic_relation_articles$robot_deleted_2 <- str_squish(Unknown_robotic_relation_articles$robot_deleted_2) # deletion of unnecessary spaces

Unknown_robotic_relation_articles$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                                     strsplit(Unknown_robotic_relation_articles$robot_deleted_1, '\\s'),
                                                     strsplit(Unknown_robotic_relation_articles$robot_deleted_2, '\\s'))

Unknown_robotic_relation_articles$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                                     strsplit(Unknown_robotic_relation_articles$robot_deleted_2, '\\s'),
                                                     strsplit(Unknown_robotic_relation_articles$robot_deleted_1, '\\s'))

# 13/12/2022
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_8_2_Using_Naive_Bayes_Model_Environment_updated_december_2022.RData")

Unknown_robotic_relation_articles$replace_check_1 <- NULL
Unknown_robotic_relation_articles$replace_check_2 <- NULL
Unknown_robotic_relation_articles$robot_deleted <- Unknown_robotic_relation_articles$robot_deleted_2
Unknown_robotic_relation_articles$robot_deleted_2 <- NULL
Unknown_robotic_relation_articles$robot_deleted_1 <- NULL


# Now I will delete stopwords (STOPWORDS REMOVAL) -----
library(tidytext)
library(tidyverse)
stopwords_for_naive_bayes # "stopwords_for_naive_bayes.xlsx"
# NOTE that I created this stopwords list manually by basically use of stopwords-smart.xlsx and stopwords-snowball.
# This list do not include words such as "over" (because of over-actuated term).
stopwords_to_remove <- stopwords_for_naive_bayes$from
stopwords_removal <- Unknown_robotic_relation_articles
stopwords_removal$key <- 1:nrow(stopwords_removal) # This will be used for untidy process
stopwords_removal <- stopwords_removal %>% unnest_tokens(tokenized, robot_deleted, strip_punct = FALSE)
stopwords_removal <- stopwords_removal[!(stopwords_removal$tokenized %in% stopwords_to_remove),]
colnames(stopwords_removal)
stopwords_removal <- stopwords_removal %>%
  group_by(key, UT, Untidy_of_AB_TI_key_lemmas) %>%
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

Unknown_robotic_relation_articles$UT_check <- stopwords_removal$UT
Unknown_robotic_relation_articles$robot_deleted_no_stopword <- stopwords_removal$stopwords_deleted_text

final_data_Unknown_robotic_relation <- Unknown_robotic_relation_articles

colnames(final_data_Unknown_robotic_relation)
final_data_Unknown_robotic_relation$robot_deleted <- NULL
final_data_Unknown_robotic_relation$UT_check <- NULL
final_data_Unknown_robotic_relation$Untidy_of_AB_TI_key_lemmas <- NULL
final_data_Unknown_robotic_relation # ( 91986 obs.) ready-to-use processed data for Naive Bayes Model Classification (i.e. "*robot*", Stopwords and single characters deleted)


# There are some WOS numbers that I have to remove because of several reasons (eg. too long, too short, meaningless, etc.)
# I will check if there are any of WOS numbers to be deleted, and I will delete existing ones.
# library(readxl)
# WOS_delete_list <- read_excel("delete_abstract.xlsx")
WOS_delete_list # list of WOS numbers to be deleted ("delete_abstract.xlsx")
WOS_delete_list <- WOS_delete_list$UT
final_data_Unknown_robotic_relation <- final_data_Unknown_robotic_relation[!final_data_Unknown_robotic_relation$UT %in% WOS_delete_list, ]

final_data_Unknown_robotic_relation # ( 91868 obs.) ready-to-use processed data for Naive Bayes Classification  (ie. "*robot*", Stopwords and single characters deleted)
# Using Naive Bayer Classifier Model on final_data_Unknown_robotic_relation, I will define articles are related to robotics or not  

# 13/12/2022 - SON burada kaldım.
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_8_2_Using_Naive_Bayes_Model_Environment_updated_december_2022.RData")


# 13/12/2022 (below environment includes only final_data_Unknown_robotic_relation data table)
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_8_3_Unknown_robotic_relation_91886_articles_updated_december_2022.RData")

00000000000000000000000000000000000
# at this point I deleted environment.
# if you want do see environment = 
# ("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_8_2_Using_Naive_Bayes_Model_Environment_updated_december_2022.RData")
00000000000000000000000000000000000


#
#
#
#
#

#> To summarize what I've done so far: 
#> 1- I cleaned the texts of the articles that do not contain the "*robot*" (they were already cleaned but a few more cleanings needed), 
#> 2- lemmatized, 
#> 3- deleted stop-words, 
#> 4- deleted single-letter words. 
#> 
#> NOTE: I have done the same operations for articles that I know if they are on robotics or not. 
#> Then have I created a Naive Bayes Classifier Model using that data.
#> I had 2 different naive Bayes Models. I compared results of these two models: approx 2600 results were different. 
#> I checked these 2600 results manually.
#> Then I create a new Naive Bayes Classifier model by using updated results (I added 2600 results to robotic relation known articles)
#> Then in december 2022 I realized an error. I had included 125 articles which contain "aerosol robotic network" to "not robotic related" list.
#> I corrected this error
#> Finally, using new Naive Bayes Classifier, I find robotic relations of articles, which do not include "robot".
#> 


# USING NAIVE BAYES MODEL -------------------------------
# Now I will use Naive Bayes Model


# load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_8_1_Naive_Bayes_MODEL_creation_Environment_updated_december_2022.RData")
# load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_8_3_Unknown_robotic_relation_91886_articles_updated_december_2022.RData")
rm(Corpus)


final_data_Unknown_robotic_relation # this is data to determine if robot related or not.


final_data_naive_bayes # 30312 WOS numbers which's robotics relation is known.
final_data_naive_bayes$robot_deleted_no_stopword <- NULL # un-necessary column
M_relation_known <- final_data_naive_bayes # for ease of reading
rm(final_data_naive_bayes)

M_relation_NOT_known <- final_data_Unknown_robotic_relation # for ease of reading
rm(final_data_Unknown_robotic_relation)

# I will remove relation known rows from M_relation_NOT_known

M_relation_NOT_known$robot_relation <- "unknown"

M_relation_NOT_known$robot_relation <- M_relation_known$robot_relation[match(M_relation_NOT_known$UT, M_relation_known$UT)]

M_relation_NOT_known <- M_relation_NOT_known %>% filter(is.na(robot_relation)) # I selected NA (unknowns)

M_relation_NOT_known$robot_relation <- NULL

library(quanteda)
Corpus_to_predict <- corpus(M_relation_NOT_known, docid_field = "UT",  text_field = "robot_deleted_no_stopword", unique_docnames = TRUE)
summary(Corpus_to_predict, 5)

install.packages("remotes")
remotes::install_github("quanteda/quanteda.classifiers")
library(quanteda)
library("quanteda.textmodels")
library("quanteda.classifiers")

dfmat_teaching <- Corpus_to_predict %>% tokens %>% dfm()

# I will use Navive_B_text_model_30187_K_fold model and find articles' robotics relation ----
Navive_B_text_model_30312_K_fold # k_fold Naive Bayes Model
model_results_k_fold <- predict(Navive_B_text_model_30312_K_fold, newdata = dfmat_teaching, 
                                type = c("class", "probability", "logposterior"),
                                force = TRUE) # FORCE TRUE yapacaksın ki farklı terimler nedeniyle hata vermesin

model_results_k_fold_dataframe <- as.data.frame(model_results_k_fold)
library(tidyverse) 
model_results_k_fold_dataframe <- model_results_k_fold_dataframe %>% rownames_to_column(var = "UT")

# Below I make the model results written in the column named "model_results"
M_relation_NOT_known$model_results_of_k_fold <- model_results_k_fold_dataframe$model_results_k_fold[match(M_relation_NOT_known$UT, model_results_k_fold_dataframe$UT)]


# 13/12/2022
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_8_4_Naive_Bayes_Results_updated_december_2022.RData")


robot_NOT_related_NBC <- model_results_k_fold_dataframe %>% filter(model_results_k_fold == "NO")
robot_NOT_related_knowns <- M_relation_known %>% filter(robot_relation == "NO")


# library(readxl)
# delete_abstract <- read_excel("delete_abstract.xlsx")
robot_to_be_deleted_abstracts <- delete_abstract # read_excel("delete_abstract.xlsx")

# df <- rename(df, newName = oldName)
robot_NOT_related_NBC <- rename(robot_NOT_related_NBC, relation = model_results_k_fold)
robot_NOT_related_knowns <- rename(robot_NOT_related_knowns, relation = robot_relation)
robot_to_be_deleted_abstracts <- rename(robot_to_be_deleted_abstracts, relation = AB_corrected)


WOS_to_exclude <- rbind(robot_NOT_related_NBC, robot_NOT_related_knowns, robot_to_be_deleted_abstracts) #47075 obs.
WOS_to_exclude <- WOS_to_exclude %>% distinct(UT, .keep_all = TRUE) # removing duplicates

# 13/12/2022
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_8_4_Naive_Bayes_Results_updated_december_2022.RData")


# I deleted all except below
WOS_to_exclude_update_december_2022 <- WOS_to_exclude
rm(WOS_to_exclude)
# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_8_5_WOS_numbers_to_exclude_updated_december_2022.RData")

# Note for thesis (02/04/2024)
# I guess the following process is just to see and check the result. There are 128 wos numbers in total. I checked the data where the articles' relationship with robotics is stated as YES/NO, and the following WOS numbers were added to that list.
load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_8_5_WOS_numbers_to_exclude_updated_december_2022.RData")
load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/W_7_5_WOS_numbers_to_exclude.RData")


NOT_robot_related_wos_addition <- WOS_to_exclude_update_december_2022[which(!WOS_to_exclude_update_december_2022$UT %in% WOS_to_exclude$UT),]
# openxlsx::write.xlsx(x = NOT_robot_related_wos_addition, file = "new_WOS_not_related_robotic_updated_december_2022.xlsx")








