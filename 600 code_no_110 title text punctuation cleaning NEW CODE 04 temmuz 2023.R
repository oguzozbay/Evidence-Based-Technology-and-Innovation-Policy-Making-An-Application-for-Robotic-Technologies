load("D:/OneDrive - XXXX/R ortak/WoS Cited/titles_cleaned.RData")

setwd("D:/OneDrive - XXXX/R ortak/WoS Cited")

library(tm)
# stop_words_of_tm_package <- as.data.frame(stopwords("english"))
# openxlsx::write.xlsx(x = stop_words_of_tm_package, file = "stop_words_of_tm_package.xlsx")
# Then I selected stopwords manually and saved as stop_words_of_tm_package.xlsx


library(readxl)
stop_words_for_titles <- read_excel("stop_words_of_tm_package.xlsx", 
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
# gsub(pattern= "(^|\\s)([[:alpha:]])\\.([[:alpha:]])\\.([[:alpha:]])\\.($|\\s)",  replacement = "\\1\\2\\3\\4\\5", "u.s.a.")
titles_cleaned$cited_title <- gsub(pattern= "(^|\\s)([[:alpha:]])\\.([[:alpha:]])\\.([[:alpha:]])\\.($|\\s)",  
                                   replacement = "\\1\\2\\3\\4\\5", titles_cleaned$cited_title)

# Some manual corrections:
titles_cleaned$cited_title[titles_cleaned$key == 3308328] <- "ai's white guy problem"
titles_cleaned$cited_title_test <- gsub(pattern= "\\bu.s.-", replacement = "us ", titles_cleaned$cited_title)





3308328
gsub(pattern= "a.i.'s", replacement = "ai", "a.i.'s white guy problem")


# I will remove duplications of titles for corrections.
titles_singular <- titles_cleaned %>% distinct(cited_title, .keep_all = TRUE) # I will remove duplicated titles for corrections.

# I will add OGUZOZBAY to every row of titles_singular$cited_title. 
# Because I will remove stopwords later and I dont want any row to be deleted.
# i.e. if all elements of a title is a stopword, it will still exist as OGUZOZBAYSTOPWORD
titles_singular$cited_title_original <- titles_singular$cited_title # in order to keep original form of data
titles_singular$cited_title <- paste(titles_singular$cited_title, "OGUZOZBAYSTOPWORD")

library(quanteda)
tokens_of_titles <- tokens(titles_singular$cited_title)
# I will remove my stopwords.
tokens_of_titles_NoStop <- tokens_select(tokens_of_titles, pattern = stop_words_for_titles$stopwords, selection = "remove")
dataframe_tokens_of_titles_NoStop <- sapply(tokens_of_titles_NoStop, function(x) paste(as.character(x), collapse = " "))
dataframe_tokens_of_titles_NoStop <- as.data.frame(dataframe_tokens_of_titles_NoStop)
colnames(dataframe_tokens_of_titles_NoStop)[1] <- "NoStopword"
titles_singular$NoStopword <- dataframe_tokens_of_titles_NoStop$NoStopword



# use gsub() to replace "OGUZOZBAYSTOPWORD" with " "
titles_singular$NoStopword <- gsub("OGUZOZBAYSTOPWORD", " ", titles_singular$NoStopword)

rm(tokens_of_titles)
rm(tokens_of_titles_NoStop)
rm(dataframe_tokens_of_titles_NoStop)

titles_singular$cited_title <- titles_singular$cited_title_original
titles_singular$cited_title_original <- NULL


# Now I will clean some punctuation.
titles_singular$NoStopword <- gsub(pattern= "\"",  replacement = " ", titles_singular$NoStopword)
titles_singular$NoStopword <- gsub(pattern= "''",  replacement = " ", titles_singular$NoStopword)
titles_singular$NoStopword <- gsub(pattern= "(?<=[[:alpha:]])'s\\b",  replacement = " ", titles_singular$NoStopword, perl=TRUE)# removal of 's (eg. word's to word)
titles_singular$NoStopword <- gsub(pattern= "(?<=\\d)'s\\b",  replacement = " ", titles_singular$NoStopword, perl=TRUE)  # removal of 's (eg. 1980's to 1980)

titles_singular$NoStopword <- gsub(pattern= "'",  replacement = " ", titles_singular$NoStopword)
titles_singular$NoStopword <- gsub(pattern= " ` s ",  replacement = " ", titles_singular$NoStopword, fixed = TRUE)
titles_singular$NoStopword <- gsub(pattern= "`",  replacement = " ", titles_singular$NoStopword)
titles_singular$NoStopword <- titles_singular$NoStopword %>% stringr::str_squish()
titles_singular$NoStopword <- gsub(pattern= "´ s",  replacement = " ", titles_singular$NoStopword)
titles_singular$NoStopword <- gsub(pattern= "(?<=[[:alpha:]])'s\\b",  replacement = " ", titles_singular$NoStopword, perl=TRUE) # removing possessive 's from the NoStopword column.

titles_singular$NoStopword_1 <- gsub(pattern= "\\b2 d\\b",  replacement = "2d", titles_singular$NoStopword, perl=TRUE)
titles_singular$NoStopword_1 <- gsub(pattern= "\\b2-d\\b",  replacement = "2d", titles_singular$NoStopword_1, perl=TRUE)
titles_singular$NoStopword_1 <- gsub(pattern= "\\b2-dimension\\b",  replacement = "2d", titles_singular$NoStopword_1, perl=TRUE)
titles_singular$NoStopword_1 <- gsub(pattern= "\\b2-dimensional\\b",  replacement = "2d", titles_singular$NoStopword_1, perl=TRUE)
titles_singular$NoStopword_1 <- gsub(pattern= "\\b2 dimension\\b",  replacement = "2d", titles_singular$NoStopword_1, perl=TRUE)
titles_singular$NoStopword_1 <- gsub(pattern= "\\b2 dimensional\\b",  replacement = "2d", titles_singular$NoStopword_1, perl=TRUE)

titles_singular$NoStopword_1 <- gsub(pattern= "\\b3 d\\b",  replacement = "3d", titles_singular$NoStopword_1, perl=TRUE)
titles_singular$NoStopword_1 <- gsub(pattern= "\\b3-d\\b",  replacement = "3d", titles_singular$NoStopword_1, perl=TRUE)
titles_singular$NoStopword_1 <- gsub(pattern= "\\b3-dimension\\b",  replacement = "3d", titles_singular$NoStopword_1, perl=TRUE)
titles_singular$NoStopword_1 <- gsub(pattern= "\\b3-dimensional\\b",  replacement = "3d", titles_singular$NoStopword_1, perl=TRUE)
titles_singular$NoStopword_1 <- gsub(pattern= "\\b3 dimension\\b",  replacement = "3d", titles_singular$NoStopword_1, perl=TRUE)
titles_singular$NoStopword_1 <- gsub(pattern= "\\b3 dimensional\\b",  replacement = "3d", titles_singular$NoStopword_1, perl=TRUE)

titles_singular$NoStopword_1 <- gsub(pattern= "\\bindustrie 4.0\\b",  replacement = "industry four_zero", titles_singular$NoStopword_1, perl=TRUE)
titles_singular$NoStopword_1 <- gsub(pattern= "\\bindustry 4.0\\b",  replacement = "industry four_zero", titles_singular$NoStopword_1, perl=TRUE)
titles_singular$NoStopword_1 <- gsub(pattern= "oftheindustry",  replacement = "of the industry", titles_singular$NoStopword_1, fixed = TRUE)
titles_singular$NoStopword_1 <- gsub(pattern= "industry 4 . o",  replacement = "industry four_zero", titles_singular$NoStopword_1, fixed = TRUE)
titles_singular$NoStopword_1 <- gsub(pattern= "industry4.0",  replacement = "industry four_zero", titles_singular$NoStopword_1, fixed = TRUE)
titles_singular$NoStopword_1 <- gsub(pattern= "industry 4 . 0",  replacement = "industry four_zero", titles_singular$NoStopword_1, fixed = TRUE)



# Remove all non-alphanumeric characters except underscore (_) and space
titles_singular$NoStopword_1 <- gsub(pattern= "[^[:alnum:]_[:space:]]+",  replacement = " ", titles_singular$NoStopword_1)
titles_singular$NoStopword_1 <- titles_singular$NoStopword_1 %>% stringr::str_squish()

# Remove digits except for the ones touching a letter
titles_singular$NoStopword_1 <- gsub(pattern= "\\b[[:digit:]]+\\b",  replacement = " ", titles_singular$NoStopword_1) # removal of whole digits (e.g. A321 will stay)
#  \\b[[:digit:]]+\\b matches one or more digit characters ([[:digit:]]+) that form a complete word (\\b...\\b).
titles_singular$NoStopword_1 <- titles_singular$NoStopword_1 %>% stringr::str_squish()

# If you want to remove all numeric digits, use below:
# titles_singular$NoStopword_1 <- gsub(pattern= "[[:digit:]]+",  replacement = " ", titles_singular$NoStopword_1) 

# save.image("D:/OneDrive - XXXX/R ortak/WoS Cited/title text punctuation cleaning NEW CODE 04 temmuz 2023_ENVIRONMENT.RData")

