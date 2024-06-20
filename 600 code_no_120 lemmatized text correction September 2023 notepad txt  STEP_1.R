# I will make some text corrections. I will use spacyr tokens.
# Then I will untidy text and then re-lemmatize and then correct lemmas by using WOS lemma correction list.

##################################################################################
############### MAIN CODE FOR USIND AS A BASEMENT - START ########################
tokens_original <- titles_lemma %>% select(doc_id, token, lemma)
tokens_original$row_no <- tokens_original[,1]
tokens_original <- tokens_original[, c(1,4,2,3)] # moving last column after 1st column
tokens_original[,2] <- gsub(pattern= "text", replacement = "", tokens_original[,2])
tokens_original[,2] <- as.numeric(tokens_original[,2])
tokens_original$doc_id <- NULL

# Group by and summarizse
column_index <- 1 # i.e. row_no column
untidy_X <- tokens_original %>%
  group_by(tokens_original[[column_index]]) %>% 
  summarise(united_text = paste0(lemma, collapse = ' '))
names(untidy_X) <- c("row_no", "untidy_of_lemmatized_text")

# Check the result
untidy_X$original <- titles_singular$clean
untidy_X <- untidy_X[, c(1,3,2)] # reordering columns
untidy_X$original <- NULL
# untidy_X <- na.omit(untidy_X)
untidy_X$untidy_of_lemmatized_text <- tolower(untidy_X$untidy_of_lemmatized_text )
############### MAIN CODE FOR USIND AS A BASEMENT - END ########################
##################################################################################


#> In the data below, there's text that has been lemmatized using spacyr. 
#> I'm not using the lemmatized part for now. I will try to correct the erroneous tokens using the tokens.
load("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/titile cited title refenece title text cleaning new V3 29_Aug_20_ENVIRONMENT.RData")

tokens_original <- titles_lemma %>% select(doc_id, token, lemma)
tokens_original$row_no <- tokens_original[,1]
tokens_original <- tokens_original[, c(1,4,2,3)] # moving last column after 1st column
tokens_original[,2] <- gsub(pattern= "text", replacement = "", tokens_original[,2])
tokens_original[,2] <- as.numeric(tokens_original[,2])
tokens_original$doc_id <- NULL
tokens_original <- as.data.frame(tokens_original)



#> The Excel file named "token_correction_test_data_september_2023" contains a list of incorrect tokens that I started to 
#> manually correct, and it has approximately 3,000 misspelled tokens. I corrected these and made the text "tidy" before making 
#> it "untidy." I then transferred the text to Notepad++ and made manual corrections and some NGram replacements based on the "NGrams" I use for WOS STM.
#> The mentioned Notepad++ version consists of 1.7 million unique cited reference titles.
#> Therefore, the corrections I made within Notepad++ are partial (I looked up to 70,000 lines and corrected 
#> any misspelled words I encountered throughout the text) and took 3 days. 
#> I extracted all the changes made to the original text (I took the difference between the original and the corrected). 
#> So it can't be considered as "from" and "to." However, it shows what has changed.


library(dplyr)

# Create the new column as a copy of token column
tokens_original <- tokens_original %>% mutate(corrected_token = token)

token_correction_test_data <- read_excel("token_correction_test_data_september_2023.xlsx")
token_correction_test_data$if_token <- token_correction_test_data$if_token %>% stringr::str_squish()
token_correction_test_data$token_to_be <- token_correction_test_data$token_to_be %>% stringr::str_squish()
colnames(token_correction_test_data) # [1] "if_token"    "token_to_be"

# Correct the tokens based on the condition
tokens_original <- tokens_original %>%
  left_join(token_correction_test_data, by = c("token" = "if_token")) %>%
  mutate(corrected_token = ifelse(!is.na(token_to_be), token_to_be, corrected_token)) %>%
  select(-token_to_be)


# Yukarıdakini untidy edeceksin
# Sonra tokenize edeceksin ve bu defa tokenleri değiştireceksin

colnames(tokens_original)
# Group by and summarizse
column_index <- 1 # i.e. row_no column
untidy_X <- tokens_original %>%
  group_by(tokens_original[[column_index]]) %>% 
  summarise(united_text = paste0(corrected_token, collapse = ' '))
names(untidy_X) <- c("row_no", "untidy_of_corrections_text")

# Check the result
untidy_X$original <- titles_singular$clean
untidy_X <- untidy_X[, c(1,3,2)] # reordering columns
untidy_X$original <- NULL
# untidy_X <- na.omit(untidy_X)
# untidy_X$untidy_of_lemmatized_text <- tolower(untidy_X$untidy_of_lemmatized_text )


sil <- untidy_X
sil$row_no <- NULL
sil <- as.data.frame(sil)
getwd()

# Transferring corrected text (corrected according to token_correction_test_data_september_2023.xlsx) to Notepad++ 
# write.table(sil, file="1_7_milyon_title.txt", row.names=FALSE, col.names=FALSE, quote=FALSE, sep="\t")

# importing the text which is manually corrected in Notepad++ 
sil_edited <- read.table("1_7_milyon_title_edit.txt", header=FALSE, stringsAsFactors=FALSE, sep="\t") # The text I corrected
colnames(sil_edited) <- c("corrected_in_Notepad_Step_1")

# Placing the corrected text next to the original.
sil$corrected_Notepad <- sil_edited$corrected_in_Notepad_Step_1
# Comparing original and replaced text
sil <- sil %>% select(original, corrected_Notepad)
sil$check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "),
                      strsplit(sil$original, '\\s'), strsplit(sil$corrected_Notepad, '\\s'))

sil$check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                      strsplit(sil$corrected_Notepad, '\\s'), strsplit(sil$original, '\\s'))

sil_filtered <- sil %>% filter(check_1 != "" | check_2 != "")
edited_tokens <- sil_filtered

# Singularizing replacements
edited_tokens <- edited_tokens %>%
  group_by(edited_tokens[[3]], edited_tokens[[4]]) %>%
  summarise(freq = n())

colnames(edited_tokens) <- c("original", "corrected", "freq")

getwd()
# openxlsx::write.xlsx(x = edited_tokens, file = "corrected_tokens_group_by_list_for_in_cited_references_titles_STEP_1.xlsx") 



untidy_X$manual_corrected_STEP1 <- sil_edited$corrected_in_Notepad_Step_1


untidy_X$is_equal <- ifelse(untidy_X[,2] == untidy_X[,4], 1, 0)
untidy_X <- as.data.frame(untidy_X)
colnames(untidy_X) <- c("row_no", "original", "untidy_of_corrections_text", "manual_corrected_STEP1", "is_equal")


gc()

equals <- untidy_X %>% filter(is_equal==1)
equals_NOT <- untidy_X %>% filter(is_equal==0)
colnames(untidy_X)
colnames(untidy_X)[5] <- "is_equal"



titles_cleaned # 5,5 milyon title
colnames(untidy_X)
CORRECTION_data_STEP1 <- untidy_X %>% select(row_no, original, manual_corrected_STEP1)
colnames(CORRECTION_data_STEP1)[3] <- "title_to_be"
colnames(CORRECTION_data_STEP1)[2] <- "if_title"

CORRECTION_data_STEP1$clean_CHECK <- titles_singular$clean
CORRECTION_data_STEP1$is_equal <- ifelse(CORRECTION_data_STEP1[,2] == CORRECTION_data_STEP1[,4], 1, 0)
# Tamam konrol ettim (kafam karışmıştı emin olmak için kontol ettim)
CORRECTION_data_STEP1$is_equal <- NULL
CORRECTION_data_STEP1$clean_CHECK <- NULL

deneme <- titles_cleaned
colnames(deneme) # [1] "key"         "wos_id"      "cited_title" "clean"     
colnames(CORRECTION_data_STEP1) # [1] "row_no"      "if_title"    "title_to_be"

deneme <- deneme %>%
  left_join(CORRECTION_data_STEP1, by = c("clean" = "if_title")) %>%
  mutate(title_correction_STEP1 = ifelse(!is.na(title_to_be), title_to_be, title_correction_STEP1)) %>%
  select(-title_to_be)

#> left_join(CORRECTION_data_STEP1, by = c("clean" = "if_title"))
#> joins the `deneme` data frame with another data frame named CORRECTION_data_STEP1.
#> The joining based on the `clean` column in the `deneme` data frame and the `if_title` column in the `CORRECTION_data_STEP1` data frame.
#> The result of the join will include all rows from `deneme` and the matching rows from `CORRECTION_data_STEP1` based on the joining criteria. 
#> If there's no match for a particular row in `deneme`, the corresponding columns from `CORRECTION_data_STEP1` will be filled with `NA` values for that row.
#>  mutate creates or updates a column named `title_correction_STEP1` in the `deneme` data frame. 
#>  The new values for this column are determined by an `ifelse` function:
#>  if title_to_be has a non-NA, then the value of `title_correction_STEP1` for that row is set to the corresponding value from `title_to_be`.
#>  If the condition is `FALSE` (i.e., `title_to_be` has an `NA` value), then the value of `title_correction_STEP1` remains unchanged for that row.
#>  select(-title_to_be): line removes the `title_to_be` column from the resulting data frame. 

# WARNING: I realized that some duplicated  wos_id - cited_title combinations. ----
# As I see part_2 of Kemal_Sami cited title data includues duplicated values.
# I will clean duplications:
#> I want to retain only one unique occurrence of each `wos_id` - `cited_title` combination in the "deneme" data frame and remove any duplicates.
#> Filtering the data frame to retain only the unique rows based on `wos_id` and `cited_title`:
library(dplyr)
filtered_deneme <- deneme %>%
  group_by(wos_id, cited_title) %>%
  slice(1) %>%
  ungroup()

#> group_by(wos_id, cited_title) groups the data by both `wos_id` and `cited_title`.
#> slice(1) retains only the first occurrence of each group.
#> ungroup() removes the grouping, so I am back to a standard data frame.
#> filtered_deneme will contain only unique combinations of `wos_id` and `cited_title`, with duplicates removed.

gc()


cited_ref_titles <- filtered_deneme
colnames(cited_ref_titles)
cited_ref_titles <- cited_ref_titles %>% select(wos_id, cited_title, title_correction_STEP1)



cited_ref_titles <- cited_ref_titles %>% filter(title_correction_STEP1 != "OGUZOZBAYDELETE")
library(dplyr)
library(stringr)

cited_ref_titles <- cited_ref_titles %>% 
  mutate(title_correction_STEP1 = str_replace_all(title_correction_STEP1, "OGUZOZBAYDELETE", " "))
cited_ref_titles <- cited_ref_titles %>% 
  mutate(title_correction_STEP1 = str_replace_all(title_correction_STEP1, "OGUZOZBAYBURADAKALDIM", " "))

cited_ref_titles$title_correction_STEP1 <- cited_ref_titles$title_correction_STEP1 %>% stringr::str_squish()
colnames(cited_ref_titles)

colnames(cited_ref_titles)[2] <- "full_original"
colnames(cited_ref_titles)[3] <- "corrected_STEP1"

cited_ref_titles$corrected_STEP1 <- tolower(cited_ref_titles$corrected_STEP1)

# save.image("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/cited_reference_titles_data_original_and_step1_corerections_September_2023.RData")





