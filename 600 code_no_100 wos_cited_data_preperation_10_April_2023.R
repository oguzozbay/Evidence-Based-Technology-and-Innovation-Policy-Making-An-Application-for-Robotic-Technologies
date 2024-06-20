library(stringr)
library(dplyr)
library(cld2)

getwd()
setwd("D:/OneDrive - XXXX/R ortak/WoS Cited")

# Use UTF-8 encoding

part_1 = read.csv('bs_wos_cited_references_kemal_sami_original_part1.csv', encoding='utf-8')
part_2 = read.csv('WOS_cited_references_06_02_2023_Original__part2.csv', encoding='utf-8')


colnames(part_1)
#[1] "wos_id"       "cited_title"  "cited_author" "cited_work"   "year"         "page"         "times_cited"  "volume"
colnames(part_2)
# [1] "X."           "id"           "article_id"   "cited_author" "cited_title"  "cited_work"   "doc_id"       "page"        
# [9] "times_cited"  "volume"       "wos_id"       "year" 


part_2$X. <- NULL
part_2$id <- NULL
part_2$article_id <- NULL
part_1$doc_id <- NA

united <- rbind(part_1, part_2)
nrow(united) # [1] 6249534

united <- subset(united, cited_title != "")
nrow(united) # [1] 5827301

united <- united[!is.na(united$cited_title),]
nrow(united) # [1] 5827301

# library(stringr)
united$cited_title <-united$cited_title %>% stringr::str_squish()

# remove rows where cited_title has only one character
united <- united[nchar(trimws(united$cited_title)) > 1,] # [1] 5827228

# remove rows where cited_title has only one word
united <- united[sapply(strsplit(united$cited_title, " "), length) > 1,]
nrow(united) # [1] 5807823

# to lower
united$cited_title <- tolower(united$cited_title)

# Now I will define English articles
# Firstly I will add a column, named key
library(dplyr)
# add a column named "key" with the row number as its value
united <- united %>% mutate(key = row_number())
united <- united %>% relocate(key, .before = wos_id)

colnames(united)
library(cld2)
united$language_detect <- detect_language(united$cited_title, lang_code = FALSE) # definition of languages

detected_langs <- sort(table(united$language_detect), decreasing = TRUE)
detected_langs <- as.data.frame(detected_langs)

options(max.print=1000)
nrow(united) # [1] 5807823
united %>% filter(language_detect == "CHINESE") # I few deletion of rows
united <- subset(united, language_detect != "CHINESE") # I few deletion of rows
nrow(united) # [1] 5746309

united %>% filter(language_detect == "KOREAN")
united <- subset(united, language_detect != "KOREAN") # I few deletion of rows


manuel_check_of_language <- united %>% filter(language_detect != "ENGLISH")
# openxlsx::write.xlsx(x = manuel_check_of_language, file = "manuel_check_of_language.xlsx") 
# I checked and defined English titles in manuel_check_of_language.xlsx and renamed it manually defined english titles.xlsx


library(readxl)
manually_defined_english_titles <- read_excel("manually defined english titles.xlsx", 
                                              sheet = "english_titles", col_types = c("numeric", 
                                                                                      "text", "text", "text", "text", "numeric", 
                                                                                      "text", "numeric", "text", "text", 
                                                                                      "text"))
colnames(united)
united_Eng_cdl2 <- subset(united, language_detect == "ENGLISH") # I selected ENGLISH titles.
nrow(united_Eng_cdl2) # [1] 5731413

united_English <- rbind(united_Eng_cdl2, manually_defined_english_titles)
nrow(united_English) # [1] 5733538

united_English$cited_title <-united_English$cited_title %>% stringr::str_squish()

rm(united)
rm(united_Eng_cdl2)



# title frequences 
library(dplyr)
title_freq <- sort(table(united_English$cited_title), decreasing = TRUE)
title_freq <- as.data.frame(title_freq)
# title_freq_1 <- title_freq %>% filter(Freq==1)



spell_check_PART_1 <- title_freq[1:1000000,]
spell_check_PART_2 <- title_freq[1000001:1771954,]

# openxlsx::write.xlsx(x = spell_check_PART_1, file = "spell_check_PART_1.xlsx") 
# openxlsx::write.xlsx(x = spell_check_PART_2, file = "spell_check_PART_2.xlsx") 



library(readxl)
spell_check_part_2_data_HYPENED_words <- read_excel("spell_check_part_2_data_HYPENED_words.xlsx")
spell_check_part_2_data_HYPENED_words <- sort(table(spell_check_part_2_data_HYPENED_words$term), decreasing = TRUE)
spell_check_part_2_data_HYPENED_words <- as.data.frame(spell_check_part_2_data_HYPENED_words)
openxlsx::write.xlsx(x = spell_check_part_2_data_HYPENED_words, file = "HYPENED_words_correction_list.xlsx")


# I manually created a hypened word correction list in excel.
library(readxl)
HYPENED_words_correction_list <- read_excel("HYPENED_words_correction_list_1_and_2_final.xlsx", 
                                            sheet = "correction_list_final")

HYPENED_words_correction_list$to <-HYPENED_words_correction_list$to %>% stringr::str_squish()


# save.image("D:/OneDrive - XXXX/R ortak/WoS Cited/wos_cited_data_preperation_10_April_2023_Environment.RData") # 12/04/2023 saat 15:42

#       load("D:/OneDrive - XXXX/R ortak/WoS Cited/wos_cited_data_preperation_10_April_2023_Environment.RData")


# Aşağıdaki kodun aslı ve açıklamaları: D:\OneDrive - XXXX\R_örnekler_final\değiştirme YENİ April 2023 - SON TAMAM


HYPENED_words_correction_list$to <-HYPENED_words_correction_list$to %>% stringr::str_squish()
correction_list <- HYPENED_words_correction_list
correction_list$from <- str_replace_all(correction_list$from, "([\\?\\.\\*\\(\\)\\{\\}\\[\\|])", "\\\\\\1") # bu anlayana kadar çatladım. Örneğin ?'yi \? yapıyor.

# Define the function for replacement
replace_words <- function(text, word_from, word_to) {
  # Loop through the correction list
  for (i in seq_along(word_from)) {
    # Use regex to match whole words, including at the beginning and end of a string
    pattern <- paste0("(^|[[:space:]]+)", word_from[i], "($|[[:space:]]+)")
    # print(pattern)
    # Replace matched words with the corresponding correction
    text <- str_replace_all(text, pattern, paste0(" ", word_to[i], " "))
  }
  return(text)
}


colnames(title_freq) #[1] "Var1" "Freq"



# Apply the function to the 'abstract' column in the dataframe
system.time({
title_freq$corrected_Var1 <- replace_words(title_freq$Var1, correction_list$from, correction_list$to)
title_freq$corrected_Var1 <- title_freq$corrected_Var1 %>% stringr::str_squish()
}) #elapsed 163898(45 hours)

# save.image("D:/OneDrive - XXXX/R ortak/WoS Cited/wos_cited_data_preperation_10_April_2023_Environment.RData") # save 1

# STEP-2 (Eğer ki değiştirme listesinde özel karakterler varsa bir tur daha değiştirme yapılacak)
system.time({
    title_freq$corrected_Var1 <- replace_words(title_freq$corrected_Var1, correction_list$from, correction_list$to)
    title_freq$corrected_Var1 <- title_freq$corrected_Var1 %>% stringr::str_squish()
}) # elapsed 20370.61 / ANLAMADIM BU nden çok çok daha kısa sürüyor (Altı Saat? Yani bulmak ayrı değiştirmek ayrı vakt mi alıyor)

# save.image("D:/OneDrive - XXXX/R ortak/WoS Cited/wos_cited_data_preperation_10_April_2023_Environment.RData") # save 2



# Split the original and corrected abstracts into individual words
title_freq$Var1_splitted <- str_split(title_freq$Var1, "\\s+")
title_freq$corrected_Var1_splitted <- str_split(title_freq$corrected_Var1, "\\s+")

# Find the differences between the original and corrected abstracts at word level
title_freq$word_changes <- mapply(function(original, corrected) setdiff(original, corrected),
                          title_freq$Var1_splitted, title_freq$corrected_Var1_splitted)


title_freq$word_changes_reverse <- mapply(function(original, corrected) setdiff(original, corrected),
                                  title_freq$corrected_Var1_splitted, title_freq$Var1_splitted)
# Convert the result to a character vector
title_freq$word_changes_reverse <- sapply(title_freq$word_changes_reverse, function(x) paste(x, collapse = " "))

# View the dataframe with word changes
head(title_freq)

replaced_rows <- title_freq %>% select (word_changes, word_changes_reverse) %>% filter (word_changes != "" | word_changes_reverse != "")
replaced_rows_title_freq_rev <- title_freq %>% select (word_changes_reverse) %>% filter (word_changes_reverse != "")

# I checked if replacements are done. It is OK

colnames(title_freq)
#[1] "Var1"                    "Freq"                    "corrected_Var1"          "Var1_splitted"           "corrected_Var1_splitted"
#[6] "word_changes"            "word_changes_reverse"  

# Now I will remove unnecessary columns.
title_freq <- title_freq %>% select(Var1, Freq, corrected_Var1)
colnames(title_freq) # [1] "Var1"           "Freq"           "corrected_Var1" 
names(title_freq)[3] <- "title"

#
##
###
####
##### # Now I will make new and more detailed language check of titles.
####
###
##
#

# Now I will makea new and more detailed language check


# STEP-1 Language detection with textcat
library(textcat)

names(TC_byte_profiles) # available languages in TC_byte_profiles
my.texcat.profiles <- TC_byte_profiles[names(TC_byte_profiles) %in% c("english",
                                                                      "french", 
                                                                      "spanish",
                                                                      "german",
                                                                      "italian",
                                                                      "turkish",
                                                                      "russian-iso8859_5",
                                                                      "russian-koi8_r",
                                                                      "russian-windows1251",
                                                                      "finnish",
                                                                      "polish")]

# Add a new column to title_freq for the language detected by textcat
title_freq$textcat <- NA
system.time({
title_freq$textcat <- textcat(title_freq$title, p = my.texcat.profiles)
}) # elapsed 2104.4

# save.image("D:/OneDrive - XXXX/R ortak/WoS Cited/wos_cited_data_preperation_10_April_2023_Environment.RData")


# STEP-2 Language detection with cld2
detach("package:cld3") # I turned of cld2 package becuse I will use cld3 in the below line
# Add a new column to title_freq for the language detected by cld2
library(cld2)
title_freq$cld2 <- NA
system.time({
title_freq$cld2 <- tolower(detect_language(title_freq$title, lang_code = FALSE))
}) # elapsed 78.49 
detach("package:cld2") # I turned of cld2 package becuse I will use cld3 in the below line. Because cld2 and cld3 both has a function named detect_language


# save.image("D:/OneDrive - XXXX/R ortak/WoS Cited/wos_cited_data_preperation_10_April_2023_Environment.RData")


# STEP-3 Language detection with cld3
library(cld3)
title_freq$cld3 <- NA
system.time({
title_freq$cld3 <- tolower(detect_language(title_freq$title))
detach("package:cld3") # because cld2 has function named detect_language()
}) # elapsed 327.32

# save.image("D:/OneDrive - XXXX/R ortak/WoS Cited/wos_cited_data_preperation_10_April_2023_Environment.RData")

# STEP-4 Language detection with cld3
library(fastText)
# https://fasttext.cc/docs/en/language-identification.html
# We distribute two versions of the models:
# lid.176.bin, which is faster and slightly more accurate, but has a file size of 126MB
# lid.176.ftz, which is the compressed version of the model, with a file size of 917kB.
file_pretrained = "D:/OneDrive - XXXX/R ortak/WoS Cited/lid.176.bin" #  I downloaded.
system.time({
dtbl_out = language_identification(input_obj = title_freq$title,
                                   pre_trained_language_model_path = file_pretrained,
                                   k = 1,
                                   th = 0.0,
                                   verbose = TRUE)
})
title_freq$fastText <- dtbl_out$iso_lang_1


# save.image("D:/OneDrive - XXXX/R ortak/WoS Cited/wos_cited_data_preperation_10_April_2023_Environment.RData")

colnames(title_freq)
filter_language_1 <- title_freq %>% filter(textcat!= "english" & cld3 != "en" & fastText != "en")
openxlsx::write.xlsx(x = filter_language_1, file = "filter_language_1.xlsx")


filter_language_2 <- title_freq %>% filter(cld3 != "en")
openxlsx::write.xlsx(x = filter_language_2, file = "filter_language_2.xlsx")


filter_language_3 <- title_freq %>% filter(cld2 != "english")
openxlsx::write.xlsx(x = filter_language_3, file = "filter_language_3.xlsx")

filter_language_4 <- title_freq %>% filter(textcat != "english")
openxlsx::write.xlsx(x = filter_language_4, file = "filter_language_4.xlsx")
# openxlsx::write.xlsx(x = filter_language_4, file = "filter_language_4.xlsx")

filter_language_tur <- title_freq %>% filter(fastText == "tr")


# Now I will use readability metrics to define miss-spelled titles.
# Load the required packages
library(quanteda.textstats)
library(dplyr)

# Take a random sample of 1000 rows from the title_freq data frame
# set.seed(123) # Set a seed for reproducibility
# sample_title_freq <- title_freq %>% sample_n(1000)


# Calculate the readability metrics for the sample_title_freq data frame
my.measure <- c("Dickes.Steiwer", "ELF", "Flesch", "FORCAST", "Linsear.Write", "RIX", "Traenkle.Bailer", "meanWordSyllables")
# readability_metrics <- textstat_readability(sample_title_freq$title, measure = "all")
readability_metrics <- textstat_readability(title_freq$title, measure = my.measure)
title_freq <- cbind(title_freq, readability_metrics)

#> Dickes-Steiwer Score: A measure of the complexity of a text based on the average number of syllables per word 
#> and the average number of words per sentence.
#> 
#> European Languange Framework (ELF): A metric developed by the European Union to assess the language proficiency 
#> of non-native speakers of European languages.
#> 
#> Flesch Reading Ease: A formula for calculating the ease of reading in a text, based on the average number 
#> of syllables per word and the average number of words per sentence.
#> 
#> FORCAST Readability Formula: A formula for estimating the difficulty of a text, based on the average number
#>  of sentences per paragraph and the average number of syllables per word.
#>  
#> Linsear Write Formula: A formula for estimating the reading level of a text, based on the average number of 
#> syllables per word and the number of simple and complex words in the text.
#> 
#> Readability Index (RIX): A formula for estimating the difficulty of a text, based on the ratio of long words to 
#> short words and the average number of sentences per 100 words.
#> 
#> Traenkle-Bailer Index: A formula for estimating the reading level of a text, based on the average number 
#> of syllables per word and the percentage of difficult words in the text.
#> 
#> Mean Word Syllables: The average number of syllables per word in a text.



sorted_df <- title_freq[order(title_freq$Dickes.Steiwer),]
colnames(sorted_df)
sorted_df <- sorted_df %>% select(title, Freq, Dickes.Steiwer)
sorted_df <- sorted_df[1:3000,]
sorted_df$is_it_English <- sorted_df$title # for ease of excel manual check copy-paste operation
sorted_df <- sorted_df %>% relocate(is_it_English, .before = Freq) # <- koymayı unutma

openxlsx::write.xlsx(x = sorted_df, file = "sorted_df_Dickes_Steiwer_3000.xlsx")



sorted_df_ELF <- title_freq[order(desc(title_freq$ELF)),]
colnames(sorted_df_ELF)
sorted_df_ELF <- sorted_df_ELF %>% select(title, Freq, ELF)
sorted_df_ELF <- sorted_df_ELF[1:1000,]
sorted_df_ELF$is_it_English <- sorted_df_ELF$title # for ease of excel manual check copy-paste operation
sorted_df_ELF <- sorted_df_ELF %>% relocate(is_it_English, .before = Freq) # <- koymayı unutma

openxlsx::write.xlsx(x = sorted_df_ELF, file = "sorted_df_ELF_1000.xlsx")

# NOT: ELF'i yapıştırıp yinelenenleri silince 300 tane kadar kaldı. Onlara çok hızlı baktım. Hatalı bir şey görmedim.


# save.image("D:/OneDrive - XXXX/R ortak/WoS Cited/wos_cited_data_preperation_10_April_2023_Environment.RData") # 20 Nisan 2023

# load("D:/OneDrive - XXXX/R ortak/WoS Cited/wos_cited_data_preperation_10_April_2023_Environment.RData")

#
##
###
####
#####
# I will replace titles in title_freq according to title_replacement_list
# Note that title_freq includes all available titles. However no duplication is allowed in this data.
library(readxl)
title_replacement_list <- read_excel("filter_language_all_edit.xlsx", sheet = "replace_list")

# merge title_freq & title_replacement_list by the "from" column (i.e. add "to" column with corresponding values)
merged_df <- merge(title_freq, title_replacement_list, by.x = "title", by.y = "from", all.x = TRUE)
# replace the "title" values with the corresponding "to" values
merged_df$title <- ifelse(!is.na(merged_df$to), merged_df$to, merged_df$title)

merged_df <- merged_df %>% select(Var1, title) # Note that Var1 is original text and, title is corrected text. 

title_main_replacement_data <- merged_df
rm(merged_df)
colnames(title_main_replacement_data)
colnames(title_main_replacement_data)[1] <- "from"
colnames(title_main_replacement_data)[2] <- "to"


# I see some misspelled words
title_main_replacement_data$to <- gsub("\\bscaleindependant\\b", "scale independant", title_main_replacement_data$to)
title_main_replacement_data$to <- gsub("\\baspossible\\b", "as possible", title_main_replacement_data$to)
title_main_replacement_data$to <- gsub("\\bal soundd etection- aldebaran\\b", "al sound detection - aldebara", title_main_replacement_data$to)
title_main_replacement_data$to <- gsub("\\balphahelical\\b", "alpha helical", title_main_replacement_data$to)
title_main_replacement_data$to <- gsub("\\batfordance\\b", "affordance", title_main_replacement_data$to)
title_main_replacement_data$to <- gsub("\\bautonmous\\b", "autonomous", title_main_replacement_data$to)
title_main_replacement_data$to <- gsub("\\bdirectonal\\b", "directional", title_main_replacement_data$to)
title_main_replacement_data$to <- gsub("\\bdynamicfusion\\b", "dynamic fusion", title_main_replacement_data$to)
title_main_replacement_data$to <- gsub("\\bfiberoptic\\b", "fiber optic", title_main_replacement_data$to)
title_main_replacement_data$to <- gsub("\\bfundmentals\\b", "fundamentals", title_main_replacement_data$to)
title_main_replacement_data$to <- gsub("\\benglighte- ment-history\\b", "enlightenment history", title_main_replacement_data$to)
title_main_replacement_data$to <- gsub("\\bominidirectional\\b", "omnidirectional", title_main_replacement_data$to)

#
##
###
####
#####
# Now I finished corrections in title_freq data frame.
# title$Var1 is the original versions of titles. This row is duplicateds removed version of united_English$cited_title



# I will replace titles in united_English according to title_main_replacement_data
# Note that title_main_replacement_data includes all available titles. However no duplication is allowed in this data.

# merge united_English & title_replacement_list by the "from" column (i.e. add "to" column with corresponding values)
merge_titles_full <- merge(united_English, title_main_replacement_data, by.x = "cited_title", by.y = "from", all.x = TRUE)
merge_titles_full <- subset(merge_titles_full, to != "OGUZOZBAYDELETE") # delete predefined rows


# replace the "cited_title" values with the corresponding "to" values
merge_titles_full$cited_title <- ifelse(!is.na(merge_titles_full$to), merge_titles_full$to, merge_titles_full$cited_title) # In fact there are no NA.
# There is not any NA because title_main_replacement_data includes all available titles. Below line is not necessary. I may use directly to column.
merge_titles_full <- merge_titles_full %>% select(key, wos_id, cited_title) 

titles_cleaned <- merge_titles_full # For ease of understanding name of data frame
rm(merge_titles_full)

# save.image("D:/OneDrive - XXXX/R ortak/WoS Cited/wos_cited_data_preperation_10_April_2023_Environment.RData") # 24 Nisan 2023

save(titles_cleaned, file= "D:/OneDrive - XXXX/R ortak/WoS Cited/titles_cleaned.RData")
