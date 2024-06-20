

# I run STM model for K=100
# I ran the last part of this code on the workstation and it took 5 days.
# I would run the STM model after "STEP- STM PROCESS ----" on the workstation.

# It is required to make an new STM analysis for patents because of missing patents in my raw patent data.
# While analyzing STM results, I realized that I had incompletely downloaded the patents. The years 2007, 2009, 2012, 2014 were fully missing.
# Thereupon, I downloaded both the missing parts and the period between 2021-2023.
# i.e. I downloaded the missing years' patents and additionally those "between 2021 and 2023" in January 2024.
# Additionally, the numbers of patents for 2013 and 2015 were more than I had, so I downloaded them too.
# Downloaded patents are in the following folder: 
# "C:\Users\ozbay\OneDrive - XXXX\R partner\patent STM 13 july 2023\missing_patents"

library(dplyr)
library(stringr)


# Import latest patent data, which is now 343409 rows (Approximately 1.5 times larger than before)
# load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/all patents from 2005 to end of 2023 ready for STM 05_01_2024.RData")
setwd("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024")

patents_05_01_2024 <- as.data.frame(patents_05_01_2024)


# Apply additional NGrams which I defined after STM trials
library(readxl)
additional_Ngrams_after_STM <- read_excel("NEW ie second STM for Patent STM additional Ngrams_Nov_2023.xlsx", 
                                          sheet = "N_Grams_Now_2023", 
                                          col_types = c("skip", "text", "text", "numeric", "skip"))

# Removal of possible unnecessary spaces
additional_Ngrams_after_STM <- additional_Ngrams_after_STM %>%
  mutate(across(everything(), ~str_squish(.)))

# Replace Ngrams
system.time({
  patterns <- paste0("\\b", additional_Ngrams_after_STM$from, "\\b")
  replacements <- additional_Ngrams_after_STM$to
  
  # Use a single str_replace_all call with vectors for pattern and replacement
  patents_05_01_2024$STM_text_new_Ngrams <- str_replace_all(patents_05_01_2024$STM_text, 
                                                           setNames(replacements, patterns))
}) # elapsed 


############################################
############################################
# checking replacements
colnames(patents_05_01_2024)
sil <- patents_05_01_2024 %>% select (STM_text_new_Ngrams, STM_text)
# Comparing original and replaced text
sil$check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(sil[,1], '\\s'), strsplit(sil[,2], '\\s'))
sil$check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(sil[,2], '\\s'), strsplit(sil[,1], '\\s'))
sil_filtered <- sil %>% filter(check_1 != "" | check_2 != "")
############################################
############################################

# I will do a second loop:
system.time({
  # Use a single str_replace_all call with vectors for pattern and replacement
  patents_05_01_2024$STM_text_new_Ngrams_Loop2 <- str_replace_all(patents_05_01_2024$STM_text_new_Ngrams, 
                                                                 setNames(replacements, patterns))
}) # elapsed 705.98

############################################
############################################
# checking replacements
colnames(patents_05_01_2024)
sil <- patents_05_01_2024 %>% select (STM_text_new_Ngrams, STM_text_new_Ngrams_Loop2)
# Comparing original and replaced text
sil$check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(sil[,1], '\\s'), strsplit(sil[,2], '\\s'))
sil$check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(sil[,2], '\\s'), strsplit(sil[,1], '\\s'))
sil_filtered <- sil %>% filter(check_1 != "" | check_2 != "")
############################################
############################################


# I will do a third loop:
system.time({
  # Use a single str_replace_all call with vectors for pattern and replacement
  patents_05_01_2024$STM_text_new_Ngrams_Loop3 <- str_replace_all(patents_05_01_2024$STM_text_new_Ngrams_Loop2, 
                                                                 setNames(replacements, patterns))
}) # elapsed 705.98 # No 


############################################
############################################
# checking replacements
colnames(patents_05_01_2024)
sil <- patents_05_01_2024 %>% select (STM_text_new_Ngrams_Loop3, STM_text_new_Ngrams_Loop2)
# Comparing original and replaced text
sil$check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(sil[,1], '\\s'), strsplit(sil[,2], '\\s'))
sil$check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(sil[,2], '\\s'), strsplit(sil[,1], '\\s'))
sil_filtered <- sil %>% filter(check_1 != "" | check_2 != "")
dim(sil_filtered) # [1] 0 4
############################################
############################################

# delete un-necessary columns
patents_05_01_2024$STM_text <- patents_05_01_2024$STM_text_new_Ngrams_Loop3
patents_05_01_2024$STM_text_new_Ngrams_Loop3 <- NULL
patents_05_01_2024$STM_text_new_Ngrams_Loop2 <- NULL
patents_05_01_2024$STM_text_new_Ngrams <- NULL

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/all patents from 2005 to end of 2023 ready for STM 05_01_2024_V2.RData")


# I will make manual text corrections and additional NGrams.
# I will extract Ngrams then select them manually:
library(tidytext)
library(dplyr)
# Extracting 3-grams
three_grams <- patents_05_01_2024 %>%
  unnest_tokens(gram, STM_text, token = "ngrams", n = 3) %>%
  count(gram, sort = TRUE)


# Extracting 2-grams
two_grams <- patents_05_01_2024 %>%
  unnest_tokens(gram, STM_text, token = "ngrams", n = 2) %>%
  count(gram, sort = TRUE)

# Filtering n-grams with frequencies greater than 500
filtered_three_grams <- three_grams %>% filter(n > 100)
# openxlsx::write.xlsx(x = filtered_three_grams, file = "filtered_three_grams.xlsx")

filtered_two_grams <- two_grams %>% filter(n > 100)
# openxlsx::write.xlsx(x = filtered_two_grams, file = "filtered_two_grams.xlsx")

patents_05_01_2024_FOR_MANUAL_CORRECTION <- patents_05_01_2024 %>% select(key, STM_text)
write.table(patents_05_01_2024_FOR_MANUAL_CORRECTION, file="patents_05_01_2024_FOR_MANUAL_CORRECTION.txt", row.names=FALSE, col.names=FALSE, quote=FALSE, sep="\t")

# By use of Notepad++ I made made below corrections:
# Note that some of replacement may not written blow (e.g. I corrected the spelling errors I noticed by chance.)
# REPLACEMENTS ARE AS FOLLOWS:
# "machine_people" = "robot" # I checek from patents and realized that the machine_people is wrong translation from Chinese to English
# "thick_bamboo" = "It is wrongly translated by WIPO I think: with the appropriate technical terms such as "barrel" or "cylindrical section.""
# section thick_bamboo = "section_thick_bamboo"
# pile up neatly = pile_neatly
# pile neatly = pile_neatly (freq= 980)
# "pile up neatly" = ? paletizing?
# p problem solve provide = DELETED
# p problem solve	6448	= DELETED
# utility_model relate_to technical_field	5894	= DELETED
# problem solve provide	4769	= DELETED
# problem solve improve	146	= DELETED
# problem solve solve_problem	136	= DELETED
# various field such_as	26	= DELETED
# relate_to field = DELETED
# relate_to technical_field = DELETED
# particularly relate_to_field = relate_to_field
# two end = two_end
# thereof= deleted
# thereto= deleted
# one end	36333	1	one_end
# low end	16474	1	low_end
# simple structure	14569	1	simple_structure
# output end	12694	1	output_end
# efficiency improve	11596	1	efficiency_improve
# fixedly install	10979	1	fixedly_install
# belong technical_field	10690	1	technical_field
# bottom end	8682	1	bottom_end
# belong field	4563	1	DELETED
# relate_to field	4499	1	DELETED
# input end	3889	1	input_end
# field robot	2284	1	robot
# field medical	387	1	medical
# field automatic	330	1	automatic
# various field	42	1	DELETED
# s = DELETED (more than 30000)
# potential safety hazard = safety_hazard (1436)
# safety hazard = safety_hazard (64)
# automatic guide vehicle = automatic_guide_vehicle (1115)
# automate guide vehicle = automatic_guide_vehicle (446)
# widely apply field = DELETED (136)
# widely apply various = DELETED (43)
# can_be widely apply = DELETED (660)
# can_be well = DELETED (2026)
# can_be well= DELETED (227)
# abdoman = abdomen (148)
# inputte = inputted (2172)
# tensione = tensioning
# airtightness = airtight
# field relate_to = DELETED
# u turn = u_turn (40)
# backr = backrest
# hand wheel & handwheel = hand_wheel
# pre position = preposition

# I manually corrected "patent_text_for_manual_correction.txt" and re-named it as follows:
library(readxl)
missing_patents_manuel_edited <- read.table("patents_05_01_2024_FOR_MANUAL_CORRECTION_edited.txt", header=FALSE, stringsAsFactors=FALSE, sep="\t") # düzeltiğim metin
missing_patents_manuel_edited$V2 <- tolower(missing_patents_manuel_edited$V2)
missing_patents_manuel_edited$V2 <- missing_patents_manuel_edited$V2 %>% stringr::str_squish()


library(dplyr)
# Assuming patents_05_01_2024 and missing_patents_manuel_edited ready to use:
# Join the data frames
merge_corrections <- left_join(patents_05_01_2024, missing_patents_manuel_edited, by = c("key" = "V1"))

# Remove rows where V2 is exactly "oguzozbaydelete"
merge_corrections <- merge_corrections[merge_corrections$V2 != "oguzozbaydelete", ]


############################################
############################################
# checking replacements
colnames(merge_corrections)
sil <- merge_corrections %>% select (STM_text, V2)
# Comparing original and replaced text
sil$check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(sil[,1], '\\s'), strsplit(sil[,2], '\\s'))
sil$check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(sil[,2], '\\s'), strsplit(sil[,1], '\\s'))
sil_filtered <- sil %>% filter(check_1 != "" | check_2 != "")
############################################
############################################

merge_corrections$STM_text <- merge_corrections$V2
merge_corrections$V2 <- NULL

colnames(merge_corrections)
colnames(patents_05_01_2024)


patents_05_01_2024 <- merge_corrections
rm(merge_corrections, missing_patents_manuel_edited)

patents_06_01_2024 <- patents_05_01_2024
patents_06_01_2024_Readme <- "this is patents from 01.012005 - 31.12.2023. The text is cleaned leamatized and Ngram are applied. The text is almost ready for STM. I said almost becasue in previous STMs, I did some additional text corrections and Ngrams, which I will do for this text too (05.01.2024). Next day in 06.01.2024 I did additional Ngrams and text corrections by use of Notedap++. I extracted NGrams by use of tidytext unnest_tokens(). Some of the correcions are given in the following R code: C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent STM 2005_2023 patents.R). As a result: patents_06_01_2024 is still almost ready for STM. Tha last thing to be completed is remocal of numbers and removal of duplicated patents"
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/all patents from 2005 to end of 2023 ready for STM 05_01_2024_V3.RData")

load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/all patents from 2005 to end of 2023 ready for STM 05_01_2024_V3.RData")

# I will delete tokens (words) from the strings in the patents_06_01_2024$STM_text column that consist only of numbers:
patents_06_01_2024$STM_text <- gsub("\\b\\d+\\b", "", patents_06_01_2024$STM_text)
patents_06_01_2024$STM_text <- patents_06_01_2024$STM_text %>% stringr::str_squish()


# To find the frequencies of tokens in patents_06_01_2024$STM_text that include any numeric character 
# and then to find the frequencies of tokens that include any non-alphanumeric characters, 
library(tidytext)
library(dplyr)
library(stringr)

# Tokenizing the text
tokens <- patents_06_01_2024 %>%
  unnest_tokens(word, STM_text)

# Filtering and counting tokens that include any numeric character
tokens_with_numbers <- tokens %>%
  filter(str_detect(word, "\\d")) %>%
  count(word, sort = TRUE)


# Filtering and counting tokens that include any non-alphanumeric character
tokens_with_nonalphanum <- tokens %>%
  filter(str_detect(word, "[^a-zA-Z0-9]")) %>%
  count(word, sort = TRUE)


# Filtering and counting tokens that include any non-alphanumeric character except for _
tokens_with_nonalphanum_except_underscore <- tokens %>%
  filter(str_detect(word, "[^a-zA-Z0-9_]")) %>%
  count(word, sort = TRUE)


openxlsx::write.xlsx(x = tokens_with_numbers, file = "tokens_with_numbers.xlsx")

patents_06_01_2024$STM_text <- gsub("\\bdeﬁne\\b", "define", patents_06_01_2024$STM_text)
patents_06_01_2024$STM_text <- gsub("\\befﬁciency\\b", "efficiency", patents_06_01_2024$STM_text)
patents_06_01_2024$STM_text <- gsub("\\bﬁnite\\b", "finite", patents_06_01_2024$STM_text)
patents_06_01_2024$STM_text <- gsub("\\bﬂexibility\\b", "flexibility", patents_06_01_2024$STM_text)
patents_06_01_2024$STM_text <- gsub("\\bli fi\\b", "lifi", patents_06_01_2024$STM_text)
patents_06_01_2024$STM_text <- gsub("\\bwi fi\\b", "wifi", patents_06_01_2024$STM_text)
patents_06_01_2024$STM_text <- gsub("\\bjudgement\\b", "judgment", patents_06_01_2024$STM_text)

patents_06_01_2024$STM_text <- gsub("\\bli li d ul0002\\b", "", patents_06_01_2024$STM_text)
patents_06_01_2024$STM_text <- gsub("\\bli d ul0002\\b", "", patents_06_01_2024$STM_text)
patents_06_01_2024$STM_text <- gsub("\\bnum ul d ul0002 list li d ul0002\\b", "", patents_06_01_2024$STM_text)

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/all patents from 2005 to end of 2023 ready for STM 05_01_2024_V4.RData")

# load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/all patents from 2005 to end of 2023 ready for STM 05_01_2024_V4.RData")

patents_06_01_2024$STM_text <- gsub("\\blist li d ul0001\\b", "", patents_06_01_2024$STM_text)
patents_06_01_2024$STM_text <- gsub("\\bnum ul d ul0002\\b", "", patents_06_01_2024$STM_text)
patents_06_01_2024$STM_text <- gsub("\\bul d ul0001\\b", "", patents_06_01_2024$STM_text)
patents_06_01_2024$STM_text <- gsub("\\babstract lang en p num\\b", "", patents_06_01_2024$STM_text)
patents_06_01_2024$STM_text <- gsub("\\bli d ul0001 0001 num 0000\\b", "", patents_06_01_2024$STM_text)
patents_06_01_2024$STM_text <- gsub("\\bli d ul0001 num\\b", "", patents_06_01_2024$STM_text)
patents_06_01_2024$STM_text <- gsub("\\bd ul0001 num\\b", "", patents_06_01_2024$STM_text)
patents_06_01_2024$STM_text <- gsub("\\blight weight\\b", "lightweight", patents_06_01_2024$STM_text)
patents_06_01_2024$STM_text <- gsub("\\blightweight\\b", "light_weight", patents_06_01_2024$STM_text)
patents_06_01_2024 <- patents_06_01_2024 <- subset(patents_06_01_2024, key != "10145260") # non-English text


setwd("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024")
# I will apply correction to tokens which include numbers: (list of tokens_with_numbers_replace_list.xlsx)
library(readxl)
tokens_with_numbers_replace_list <- read_excel("tokens_with_numbers_replace_list.xlsx",
                                               sheet = "replacements", col_types = c("text", "text", "skip"))

# NGrams_of_PATENTS_for_Final_STM_Aug_2023$from <- NGrams_of_PATENTS_for_Final_STM_Aug_2023$from %>% stringr::str_squish()
tokens_with_numbers_replace_list$from <- tokens_with_numbers_replace_list$from %>% stringr::str_squish()
tokens_with_numbers_replace_list$to <- tokens_with_numbers_replace_list$to %>% stringr::str_squish()

# Replacement of tokens_with_numbers_replace_list
# Function for replacement (NOTE THAT, there are no special characters in NGrams_of_PATENTS_for_Final_STM_Aug_2023)
library(stringi)
replace_words <- function(text, word_from, word_to) {
  for (i in seq_along(word_from)) {
    pattern <- paste0("\\b", word_from[i], "\\b")
    text <- stri_replace_all_regex(text, pattern, word_to[i])
  }
  return(text)
}

patents_06_01_2024$tokens_with_numbers <- patents_06_01_2024$STM_text # create a column for corrections

# Apply the function to the 'tokens_with_numbers' column to the patents_06_01_2024 dataframe
system.time({
  patents_06_01_2024$tokens_with_numbers <- replace_words(patents_06_01_2024$tokens_with_numbers, 
                                                          tokens_with_numbers_replace_list$from,
                                                          tokens_with_numbers_replace_list$to)
  
  patents_06_01_2024$tokens_with_numbers <- patents_06_01_2024$tokens_with_numbers %>% stringr::str_squish()
  # save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/all patents from 2005 to end of 2023 ready for STM 05_01_2024_V5.RData")
  
  # STEP-2 I will run same code again
  patents_06_01_2024$tokens_with_numbers <- replace_words(patents_06_01_2024$tokens_with_numbers, 
                                                          tokens_with_numbers_replace_list$from,
                                                          tokens_with_numbers_replace_list$to)
  
  patents_06_01_2024$tokens_with_numbers <- patents_06_01_2024$tokens_with_numbers %>% stringr::str_squish()
  # save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/all patents from 2005 to end of 2023 ready for STM 05_01_2024_V5.RData")
  
  # STEP-3 I will run same code again
  patents_06_01_2024$tokens_with_numbers <- replace_words(patents_06_01_2024$tokens_with_numbers, 
                                                          tokens_with_numbers_replace_list$from,
                                                          tokens_with_numbers_replace_list$to)
  
  patents_06_01_2024$tokens_with_numbers <- patents_06_01_2024$tokens_with_numbers %>% stringr::str_squish()
  # save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/all patents from 2005 to end of 2023 ready for STM 05_01_2024_V5.RData")
}) # elapsed 2904.42 07/01/2024


############################################
############################################
# checking replacements
colnames(patents_06_01_2024)
sil <- patents_06_01_2024 %>% select (tokens_with_numbers, STM_text)
# Comparing original and replaced text
sil$check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(sil[,1], '\\s'), strsplit(sil[,2], '\\s'))
sil$check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(sil[,2], '\\s'), strsplit(sil[,1], '\\s'))
sil_filtered <- sil %>% filter(check_1 != "" | check_2 != "")
############################################
############################################

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/all patents from 2005 to end of 2023 ready for STM 05_01_2024_V5.RData")


patents_06_01_2024$STM_text <- patents_06_01_2024$tokens_with_numbers
patents_06_01_2024$tokens_with_numbers <- NULL


patents_06_01_2024$STM_text <- tolower(patents_06_01_2024$STM_text)
dim(patents_06_01_2024) # [1] 343408      7

# Remove duplicated patents (in previous trials I see that it is better way remove duplication when use findthoughs function of STM package,
# additionally some patents are applied many countries.)
# Note that I did this removal previously. This removal of duplication is on the final version of the patents text.
patents_06_01_2024 <- patents_06_01_2024[!duplicated(patents_06_01_2024$STM_text), ]
dim(patents_06_01_2024) # [1] 342565      7

patents_06_01_2024$STM_text <- str_replace_all(patents_06_01_2024$STM_text, "\\boguzozbaydelete\\b", "")
patents_06_01_2024$STM_text <- patents_06_01_2024$STM_text %>% stringr::str_squish()


patents_08_01_2024 <- patents_06_01_2024

patents_08_01_2024_Readme <- "this is patents from 01.012005 - 31.12.2023. The text is cleaned leamatized and Ngram are applied. The text is ready for STM.  The last thing to be completed is removal of delete_tokens_list"

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/all patents from 2005 to end of 2023 ready for STM 05_01_2024_V6.RData")

# Cluster information will be added and STM will be performed. Everything is ready.
# burada kaldım 08/01/2023 saat 00:30

# Finally clusters are ready. 08/02/2023 (It was running in anpther computer)
# I derived it with the R code there: D:/OneDrive - XXXX/R ortak/patent IPC clustering/missing patents patent clustering 03 Jan 2024.R

# I will import patent data ans Cluster data
# load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/all patents from 2005 to end of 2023 ready for STM 05_01_2024_V6.RData") # for casper
# load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/K13 Cluster data of all patents from 2005 to end of 2023.RData") # for casper
# load("D:/OneDrive - XXXX/R_patent_2024/all patents from 2005 to end of 2023 ready for STM 05_01_2024_V6.RData") # for PC
# load("D:/OneDrive - XXXX/R_patent_2024/K13 Cluster data of all patents from 2005 to end of 2023.RData") # for PC



patents_08_01_2024_Readme # [1] "this is patents from 01.012005 - 31.12.2023. The text is cleaned lemmatized and Ngram are applied. The text is ready for STM.  The last thing to be completed is removal of delete_tokens_list"
df_original # K13 cluster data (13 centers)


# Attention, the following situation happened:
# While analyzing the cluster, I did not delete the duplicate patents in the old patents, that is, "STM patent 13 temmuz_ENVIRONMENT.RData".
# I delete these (i.e. duplicate patents) when doing STM because it distorts the result and also makes it harder to analyze with fingthoughs.
# Also, logically, I can delete them because these are applications for the same patent in different countries.
# As a result, the df_original here is 352957 lines, unless I took a wrong note.
# But the "all patents from 2005 to end of 2023 ready for STM 05_01_2024_V6.RData" data to be used for stm is 342565 lines. But since I defined the key, there is no problem.


library(dplyr)
# left_join(): keeps all rows from the first (left) data frame and adds the columns from the second (right)
patents_08_01_2024 <- patents_08_01_2024 %>%
  left_join(df_original[, c("key", "cluster", "IPC_LEVEL_1")], by = "key") #  "select all rows but only the key, cluster and IPC_LEVEL_1 columns from df_original"


# adding IPC-clusters in to the text
# Now I will add text as "ipc_cluster_" to cluster values then combine it to STM text: 
library(dplyr)
patents_08_01_2024 <- patents_08_01_2024 %>%
    mutate(cluster_text = paste0('ipc_cluster_', cluster))

# Now I will add this "cluster text" in to "STM_text" column in a new column named STM_text_Cluster
patents_08_01_2024 <- patents_08_01_2024 %>%
    mutate(STM_text_Cluster = paste0(cluster_text, ' ', STM_text))
  

patents_cluster_in_text <- patents_08_01_2024
patents_cluster_in_text$STM_text <- NULL
patents_cluster_in_text$cluster <- NULL
patents_cluster_in_text$IPC_LEVEL_1 <- NULL
patents_cluster_in_text$cluster_text <- NULL



patents_cluster_in_text # Şu anda STM model için hazır veri. kümeler metnin içine altıdı.
# Diğer tüm environemti sildim 08/02/2024
patents_cluster_in_text_Readme <- "this is patents from 01.012005 - 31.12.2023. The text is cleaned leamatized and Ngrams are applied. IPC code clusters are inserted into the patent text The text is ready for STM.  The last thing to be completed is removal of delete_tokens_list"
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/all patents 2005_2023 ipc cluster in the text ready for STM 08_02_2024.RData")

load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/all patents 2005_2023 ipc cluster in the text ready for STM 08_02_2024.RData")

# STEP  (dfm) ----
# Now I will create document feature matrix and then delete stop words.
library(tidytext)
tidy_Patent <- patents_cluster_in_text %>% unnest_tokens(output= word, input= STM_text_Cluster, token = "words")
tidy_Patent %>% count(word, sort = TRUE)
  
# create document feature matrix (library(tidytext))
Patent_dfm <- tidy_Patent %>% count(key, word) %>% cast_dfm(key, word, n) # cast_dfm(data, document, term, value, ...)
# Document-feature matrix of: 342,565 documents, 52,566 features (99.91% sparse) and 0 docvars.

# I keep only words occurring at least 4 times and at least in 3 documents (library(quanteda))
library(quanteda)
Patent_dfm_trim_1 <- dfm_trim(Patent_dfm, min_termfreq = 4, min_docfreq = 3, termfreq_type = "count", docfreq_type = "count")
# Document-feature matrix of: 342,565 documents, 24,270 features (99.80% sparse) and 0 docvars.
  
# Now I will define single character tokens (i.e. a, b, c, ... etc.)
# Then I will add these to my stop-word list (NOTE: stop-word has already bean removed. "my stop-word" means here some additional stopwords. 
tokens <- tidy_Patent %>% count(word, sort = TRUE)
tokens$n_char <- 0
tokens$n_char <-  nchar(tokens$word)
single_characters <- tokens %>% filter(n_char == 1)
single_characters <- single_characters$word
delete_tokens_list <- single_characters
  
# Add stop_words to delete_tokens_list
delete_tokens_list <- c(single_characters, 
                          "aa", "bb", "cc", "dd", "ee", "ff", "gg", "hh", "ii", "jj", "kk", "ll", "mm", "ul0002",
                          "nn", "oo", "pp", "qq", "rr", "ss", "tt", "uu", "vv", "ww", "xx", "yy", "zz",
                          "can_be", "be_use_for", "plurality_of", "so_that", "which_be", "such_as", "sf", "li", "s0", 
                          "thereof", "still", "said", "whose", "whole","ul0002","constitution") # NOTE: "constitution:" is a heading in patent abstracts.
  
  
# Removal of stop-words (i.e delete_tokens_list) from dfm
Patent_dfm_trim_2 <- dfm_remove(Patent_dfm_trim_1, delete_tokens_list, verbose = TRUE) # removed 56 features
# Document-feature matrix of: 342,565 documents, 24,214 features (99.80% sparse) and 0 docvars.

# Let me check the resultant dfm
library(quanteda.textstats)
word_freqs_of_DFM <-as.data.frame(quanteda.textstats::textstat_frequency(Patent_dfm_trim_2))
# Every thing seem OK.
Patent_dfm <- Patent_dfm_trim_2
rm(Patent_dfm_trim_2, Patent_dfm_trim_1)
  
  
# I will create "out" for STM
#> STM Vignette says that: "Note that when using quanteda dfm directly there may be higher memory use 
#> (because the texts and metadata are stored twice). You can convert from quanteda’s format directly 
#> to our native format using the quanteda function convert."
colnames(patents_cluster_in_text)
# [1] "key" "application_id" "year" "Country" "IPC" "title_corrected" "STM_text_Cluster"
Patent_docvars <- patents_cluster_in_text %>% select(key, year) # I willl not use cluster because I inserted cluster into the text
#> You can convert from quanteda’s format directly to our native format using the quanteda function convert.
out <- convert(Patent_dfm, to = 'stm', docvars = Patent_docvars)
ls()
rm(tidy_Patent, single_characters, tokens, delete_tokens_list)
  
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/all patents 2005_2023 ipc cluster in the text ready for STM 08_02_2024_V2.RData")
# At this point I am ready to Start STM process. 08/02/2024

#
##
####
######
########
########### STM - K_Results i.e Number of topics
########
######
####
##
#
  
  
 # STEP- STM PROCESS ----
setwd("D:/oguz_ozbay/patent 2024 K hesaplama") # UME work station
load("D:/oguz_ozbay/patent 2024 K hesaplama/all patents 2005_2023 ipc cluster in the text ready for STM 08_02_2024_V2.RData") # UME work station / 09/02/2024


  
  # STEP- (suitable numbers of topics should be defined)
  library(stm)
  library(furrr)
  library(dplyr)
  plan(multisession, workers = 24) # UME work station / 09/02/2024
  # plan(multisession)
  # nbrOfWorkers()  ## == availableCores()
  # plan(multisession, workers = 3) # https://juliasilge.com/blog/evaluating-stm/
  set.seed(2023)

  # The data I use is the most current patent data, in which I added the missing patents and the newly added patents between 2021-2023.# 09/02/2023  
  system.time({
    many_models_patent_K_85_125 <-tibble(K = c(85,90,95,100,105,110,115,120,125)) %>% # was= c(70,75,80,85,90,94) in which I used old patent data
      mutate(topic_model = future_map(
        K, ~stm(out$documents, out$vocab, data = out$meta, K = ., 
                prevalence = ~s(year), 
                init.type = 'Spectral', max.em.its=100, seed = 2023, verbose = FALSE),
        .options=furrr_options(seed = TRUE)
      ))
  }) # elapsed 46488 / 12.02.2024
  
  plan(sequential) ## Explicitly close multisession workers by switching plan
  
# save.image("D:/oguz_ozbay/patent 2024 K hesaplama/all patents 2005_2023 number of topics_K_result_09_02_2024.RData")

system.time({heldout <- make.heldout(Patent_dfm)})
  
library(purrr)
  k_result_85_125 <- many_models_patent_K_85_125 %>%
    mutate(exclusivity = map(topic_model, exclusivity),
           semantic_coherence = map(topic_model, semanticCoherence, Patent_dfm),
           eval_heldout = map(topic_model, eval.heldout, heldout$missing),
           residual = map(topic_model, checkResiduals, Patent_dfm),
           bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
           lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
           lbound = bound + lfact,
           iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))
  
  
# save.image("D:/oguz_ozbay/patent 2024 K hesaplama/all patents 2005_2023 number of topics_K_result_09_02_2024.RData")

# In order to import K results:
# load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/all patents 2005_2023 number of topics_K_result_09_02_2024.RData") # for laptop (casper)

# plot the result
library(ggplot2)
library(tidyverse)
k_result_85_125 %>%
    transmute(K,
              `Lower bound` = lbound,
              Residuals = map_dbl(residual, "dispersion"),
              `Semantic coherence` = map_dbl(semantic_coherence, mean),
              `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
    gather(Metric, Value, -K) %>%
    ggplot(aes(K, Value, color = Metric)) +
    geom_line(linewidth = 1.5, alpha = 0.7, show.legend = T) +
    facet_wrap(~Metric, scales = "free_y") +
    labs(x = "K (number of topics)",
         y = NULL,
         title = "Patents Model diagnostics by number of topics",
         subtitle = "12/02/2024 Higher 'Held-out likelihood' and 'Sematic Coherence' indicate ideal number of topics.
Change of Held-out and Lower bound is almost linear. A good number of topics may be 100.")


#
##
####
######
########
########### STM - K=100
########
######
####
##
#


# STM model of patents:
# I think 100 is the good number or topics.
# Now I will run STM for K= 100 topics. 12/02/2023
# Remove all environment. Len iöport STM data then import patent STM data

# load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/all patents 2005_2023 ipc cluster in the text ready for STM 08_02_2024_V2.RData") # for laptop (casper)
# load("D:/oguz_ozbay/patent 2024 K hesaplama/all patents 2005_2023 ipc cluster in the text ready for STM 08_02_2024_V2.RData") # Work station


library(tidytext)
library(quanteda)
library(stm)
library(furrr)
library(ggplot2)
library(purrr)
library(tidyr)


library(stm)
system.time({
  set.seed(2023)
  mod.out_patent <- selectModel(documents= out$documents,
                                vocab= out$vocab,
                                K=100, 
                                prevalence = ~s(year), 
                                data = out$meta,
                                init.type = "LDA",
                                max.em.its = 250, # 500 iken kapanmıştı bilgisayar
                                runs= 20, # 50'den düşürdüm iken kapanmıştı bilgisayar (yaklaşık %15'ini alıyormuş yani model sayısı)
                                seed = 2023)
  
}) # elapsed ? = 416796.02 / 17.02.2024 (5 days)



BURADA_KALDIM_STM_model_with_K_100 <- "burada kaldim 12 subat 2024"

# save.image("D:/oguz_ozbay/patent STM model K_100 12_02_2024/Patent STM 2005_2023 models for K_100.RData") # Work station
load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent STM 2005_2023 models for K_100.RData") # STM models
setwd("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024")

# Now I will qualitatively select better model by reviewing the plots

plotModels(
  models= mod.out_patent,
  xlab = "Semantic Coherence",
  ylab = "Exclusivity",
  #     c(NA,2,NA,4, NA,6, NA,8, NA,10),
  # pch = c(1:10),
  legend.position = "bottomleft"
) # model 1 ... 10


plotModels(
  models= mod.out_patent,
  xlab = "Semantic Coherence",
  ylab = "Exclusivity",
  pch = c(1,NA,NA,NA),
  legend.position = "bottomleft"
) # model 1

plotModels(
  models= mod.out_patent,
  xlab = "Semantic Coherence",
  ylab = "Exclusivity",
  pch = c(NA,2,NA,NA),
  legend.position = "bottomleft"
) # model 2

plotModels(
  models= mod.out_patent,
  xlab = "Semantic Coherence",
  ylab = "Exclusivity",
  pch = c(1,2,NA,NA),
  legend.position = "bottomleft"
) # model 1 vs 2


plotModels(
  models= mod.out_patent,
  xlab = "Semantic Coherence",
  ylab = "Exclusivity",
  pch = c(1,NA,3,NA),
  legend.position = "bottomleft"
) # model  1 vs 3 => 


plotModels(
  models= mod.out_patent,
  xlab = "Semantic Coherence",
  ylab = "Exclusivity",
  pch = c(1,NA,NA,4),
  legend.position = "bottomleft"
) # model  1 vs 4 



plotModels(
  models= mod.out_patent,
  xlab = "Semantic Coherence",
  ylab = "Exclusivity",
  pch = c(NA,2,NA,4),
  legend.position = "bottomleft"
) # model 1 to 4 => 


plotModels(
  models= mod.out_patent,
  xlab = "Semantic Coherence",
  ylab = "Exclusivity",
  pch = c(NA,NA,3,4),
  legend.position = "bottomleft"
) # model => 



#> Model 1 >= Model 2 (> : seems better than)
#> Model 1 ?= Model 4 
#> Model 3 >= Model 1
#> Model 3 >= Model 4
#> Model 4 >= Model 2
# Although there are no significant differences between the outputs of the models, the third model seems more appropriate.

STM_new_patents_K100_M3 <- mod.out_patent$runout[[3]]
read_me_STM_new_patents_K100_M3 <- "It took 5 days to create four models in workstation. I seleceted 3rd model. Infact there are no significant differences between the exclusivity vs sematic Coherence plots of the models."
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/Patent STM 2005_2023_final_model K_100.RData")





