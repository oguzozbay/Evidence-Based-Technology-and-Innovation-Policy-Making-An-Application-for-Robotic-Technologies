# I started at this and then gave up halfway through.
# But I shared this code to keep track of Environment.
# I used this but then there was no need to complete it.
# I did not save the Environment after the place where REFERENCE_123 is written below.
# Why did this happen: I lemmatized it without doing "text correction". I thought this wouldn't work without correcting the text.
# Also, I later realized that in Kemal Sami's data, all the data in part2 (I think) is duplicated.
# As a result, the file below contains only partially corrected and extracted cited ref data.
# "C:/Users/ozbay/OneDrive - XXXX/R common/WoS Cited/cited_reference_titles_data_original_and_step1_corerections_September_2023.RData"
# I will continue working on this data with another R code.
# NOTE: The file mentioned above, namely: "C:/Users .../cited_reference_ ... _September_2023.RData" was produced partially using the code on this page.


getwd()
setwd("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited")
# load("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/titles_cleaned.RData")

library(quanteda)
library(dplyr)

#################################################################################################
library(tm)
# stop_words_of_tm_package <- as.data.frame(stopwords("english"))
# openxlsx::write.xlsx(x = stop_words_of_tm_package, file = "stop_words_of_tm_package.xlsx")
# Then I selected stopwords manually and saved as stop_words_of_tm_package.xlsx
#################################################################################################
library(readxl)
stopwords_for_titles <- read_excel("stop_words_of_tm_package.xlsx", 
                                    sheet = "stopwords_for_titles", col_types = c("skip", "text"))


# I want a.i. to be replaced to ai
# NOTE: I have to do below before tokenization. Because "a.i." is tokenized as "a.i ."
titles_cleaned$cited_title <- gsub(pattern= "(^|\\s)([[:alpha:]])\\.([[:alpha:]])\\.($|\\s)", 
                                   replacement = "\\1\\2\\3\\4", titles_cleaned$cited_title)
#> "(^|\\s)([[:alpha:]])\\.([[:alpha:]])\\.($|\\s)"
#> (^|\\s) matches either the start of a line (^) or a space (\\s). The | symbol stands for OR.
#> ([[:alpha:]])\\. matches a single alphabetical character followed by a period. [:alpha:] is a class representing all alphabetic characters.
#> ([[:alpha:]])\\. is the same pattern repeated, so this whole section matches two single alphabetical characters separated by a period, like "a.i.".
#> ($|\\s) matches either the end of a line ($) or a space (\\s).
#> The replacement is "\\1\\2\\3\\4". This uses the numbered capturing groups from the pattern (in parentheses) to form the replacement. 
#> \\1 refers to the first capturing group (^|\\s), i.e., the space or start of the line before the abbreviation.
#> \\2 refers to the second capturing group ([[:alpha:]]), i.e., the first letter of the abbreviation.
#> \\3 refers to the third capturing group ([[:alpha:]]), i.e., the second letter of the abbreviation.
#> \\4 refers to the fourth capturing group ($|\\s), i.e., the space or end of the line after the abbreviation.
#> In effect, this gsub() call is looking for abbreviations of the form "a.i." that are either at the start of a line, 
#> at the end of a line, or surrounded by spaces, and it's removing the periods from those abbreviations while leaving everything else the same.

# 3 letter abbreviations.
# gsub(pattern= "(^|\\s)([[:alpha:]])\\.([[:alpha:]])\\.([[:alpha:]])\\.($|\\s)",  replacement = "\\1\\2\\3\\4\\5", "test u.s.a. usa mit m.i.t.") # for test 
titles_cleaned$cited_title <- gsub(pattern= "(^|\\s)([[:alpha:]])\\.([[:alpha:]])\\.([[:alpha:]])\\.($|\\s)",  
                                   replacement = "\\1\\2\\3\\4\\5", titles_cleaned$cited_title)

# Some manual corrections (also given as an example code):
titles_cleaned$cited_title[titles_cleaned$key == 3308328] <- "ai's white guy problem"
# gsub(pattern= "a.i.'s", replacement = "ai", "a.i.'s white guy problem") # test code
titles_cleaned$cited_title_test <- gsub(pattern= "\\bu.s.-", replacement = "us ", titles_cleaned$cited_title)

titles_cleaned$cited_title_test <- gsub(" u.s.: ", " us ", titles_cleaned$cited_title_test, fixed = TRUE) # using fixed pattern
titles_cleaned$cited_title_test <- gsub(" (u.s.a.).", " usa ", titles_cleaned$cited_title_test, fixed = TRUE) # using fixed pattern
titles_cleaned$cited_title_test <- gsub(" imuu.s. army ", " imu us army ", titles_cleaned$cited_title_test, fixed = TRUE) # using fixed pattern


# Before removal of apostrophe stopword I do some ' cleaning
titles_cleaned$cited_title_test <- gsub("` s ", " ", titles_cleaned$cited_title_test, fixed = TRUE) # using fixed pattern
titles_cleaned$cited_title_test <- gsub("'s ", " ", titles_cleaned$cited_title_test, fixed = TRUE) # using fixed pattern


# I will remove stopwords with aphostrophe
stopwords_apostrophe <- read_excel("stopwords_apostrophed.xlsx")
library(quanteda)
# Tokenize
tokens_1 <- tokens(titles_cleaned$cited_title_test)
# Remove stopwords
tokens_1_No_apos_Stop <- tokens_select(tokens_1, pattern = stopwords_apostrophe$apostrophed_words, selection = "remove")
# Convert tokenized text back to string format
titles_No_apos_Stop <- sapply(tokens_1_No_apos_Stop, function(x) paste(as.character(x), collapse = " "))
# Add the cleaned titles back to the data frame
titles_cleaned$cited_title_test_2 <- titles_No_apos_Stop
gc()

# Now I will make punctuation cleaning ----
titles_cleaned$clean <- titles_cleaned$cited_title_test_2
titles_cleaned$cited_title_test <- NULL
titles_cleaned$cited_title_test_2 <- NULL
gc()
ls()
rm(tokens_1, tokens_1_No_apos_Stop, titles_No_apos_Stop)



titles_cleaned$clean <- gsub(pattern= "\"",  replacement = " ", titles_cleaned$clean)
titles_cleaned$clean <- gsub(pattern= "''",  replacement = " ", titles_cleaned$clean)
titles_cleaned$clean <- gsub(pattern= "' '",  replacement = " ", titles_cleaned$clean)
titles_cleaned$clean <- gsub(pattern= "(?<=[[:alpha:]])'s\\b",  replacement = " ", titles_cleaned$clean, perl=TRUE)# removal of 's (eg. word's to word) # may be remaind end of string
titles_cleaned$clean <- gsub(pattern= "(?<=\\d)'s\\b",  replacement = " ", titles_cleaned$clean, perl=TRUE)  # removal of 's (eg. 1980's to 1980)
titles_cleaned$clean <- gsub(pattern= "'",  replacement = " ", titles_cleaned$clean)

titles_cleaned$clean <- gsub(pattern= " ` s ",  replacement = " ", titles_cleaned$clean, fixed = TRUE)
titles_cleaned$clean <- gsub(pattern= "`",  replacement = " ", titles_cleaned$clean)
titles_cleaned$clean <- titles_cleaned$clean %>% stringr::str_squish()

titles_cleaned$clean <- gsub(pattern= "´ s",  replacement = " ", titles_cleaned$clean)
titles_cleaned$clean <- gsub(pattern= "(?<=[[:alpha:]])'s\\b",  replacement = " ", titles_cleaned$clean, perl=TRUE) # removing possessive 's

titles_cleaned$clean <- gsub(pattern= "\\b2 d\\b",  replacement = "2d", titles_cleaned$clean, perl=TRUE)
titles_cleaned$clean <- gsub(pattern= "\\b2-d\\b",  replacement = "2d", titles_cleaned$clean, perl=TRUE)
titles_cleaned$clean <- gsub(pattern= "\\b2-dimension\\b",  replacement = "2d", titles_cleaned$clean, perl=TRUE)
titles_cleaned$clean <- gsub(pattern= "\\b2-dimensional\\b",  replacement = "2d", titles_cleaned$clean, perl=TRUE)
titles_cleaned$clean <- gsub(pattern= "\\b2 dimension\\b",  replacement = "2d", titles_cleaned$clean, perl=TRUE)
titles_cleaned$clean <- gsub(pattern= "\\b2 dimensional\\b",  replacement = "2d", titles_cleaned$clean, perl=TRUE)

titles_cleaned$clean <- gsub(pattern= "\\b3 d\\b",  replacement = "3d", titles_cleaned$clean, perl=TRUE)
titles_cleaned$clean <- gsub(pattern= "\\b3-d\\b",  replacement = "3d", titles_cleaned$clean, perl=TRUE)
titles_cleaned$clean <- gsub(pattern= "\\b3-dimension\\b",  replacement = "3d", titles_cleaned$clean, perl=TRUE)
titles_cleaned$clean <- gsub(pattern= "\\b3-dimensional\\b",  replacement = "3d", titles_cleaned$clean, perl=TRUE)
titles_cleaned$clean <- gsub(pattern= "\\b3 dimension\\b",  replacement = "3d", titles_cleaned$clean, perl=TRUE)
titles_cleaned$clean <- gsub(pattern= "\\b3 dimensional\\b",  replacement = "3d", titles_cleaned$clean, perl=TRUE)

# An example of forrection defined by chance.
titles_cleaned$clean[titles_cleaned$key == 5015659] <- "industry_4_0 readinessin manufacturing company compass2.0 a renewed framework and solution for industry_4_0 maturity assessment"

titles_cleaned$clean <- gsub(pattern= "\\bindustrie 4.0\\b",  replacement = "industry_4_0", titles_cleaned$clean, perl=TRUE)
titles_cleaned$clean <- gsub(pattern= "\\bindustry 4.0\\b",  replacement = "industry_4_0", titles_cleaned$clean, perl=TRUE)
titles_cleaned$clean <- gsub(pattern= "oftheindustry",  replacement = "of the industry", titles_cleaned$clean, fixed = TRUE)
titles_cleaned$clean <- gsub(pattern= "industry 4 . o",  replacement = "industry_4_0", titles_cleaned$clean, fixed = TRUE)
titles_cleaned$clean <- gsub(pattern= "industry4.0",  replacement = "industry_4_0", titles_cleaned$clean, fixed = TRUE)
titles_cleaned$clean <- gsub(pattern= "industry 4 . 0",  replacement = "industry_4_0", titles_cleaned$clean, fixed = TRUE)


# Remove all non-alphanumeric characters except underscore (_) and space
titles_cleaned$clean <- gsub(pattern= "[^[:alnum:]_[:space:]]+",  replacement = " ", titles_cleaned$clean)
# Remove digits except for the ones touching a letter
titles_cleaned$clean <- gsub(pattern= "\\b[[:digit:]]+\\b",  replacement = " ", titles_cleaned$clean) # removal of whole digits (e.g. A321 will stay)
#  \\b[[:digit:]]+\\b matches one or more digit characters ([[:digit:]]+) that form a complete word (\\b...\\b).
titles_cleaned$clean <- titles_cleaned$clean %>% stringr::str_squish()

# save.image("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/titile cited title refenece title text cleaning new V3 29_Aug_20_ENVIRONMENT.RData")

# At this point I will lemmatize:
# load("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/titile cited title refenece title text cleaning new V3 29_Aug_20_ENVIRONMENT.RData")

# I will remove duplication of titles for memory efficiency.
titles_singular <- titles_cleaned %>% distinct(clean, .keep_all = TRUE) # I will remove duplicated titles for corrections.

library(spacyr)
spacy_install()
spacy_initialize()

system.time({
  titles_lemma <- spacy_parse(titles_singular$clean , multithread = TRUE, nounphrase = FALSE, lemma= TRUE) #
}) # elapsed 2227.85

spacy_finalize()
# save.image("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/titile cited title refenece title text cleaning new V3 29_Aug_20_ENVIRONMENT.RData")

load("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/titile cited title refenece title text cleaning new V3 29_Aug_20_ENVIRONMENT.RData")
# YUKARIDA BURYA REFERANS VERDİM
# REFERANS_123
