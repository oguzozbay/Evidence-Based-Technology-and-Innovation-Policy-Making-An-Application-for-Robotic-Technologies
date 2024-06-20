##############################################################################
##############################################################################
##### 0000000000000000 _ to extract words containing -  000000000000000 ###### 
##############################################################################
library(dplyr)
library(stringr)
library(tidyr)

# First I calculate all the frequencies

# Tokenize the text into words and count their frequencies
all_word_freq <- patents_cluster_in_text %>%
  mutate(STM_text_Cluster = str_split(STM_text_Cluster, "\\s+")) %>%
  unnest(STM_text_Cluster) %>%
  count(word = STM_text_Cluster) %>%
  arrange(desc(n))

# Now, I can use the all_word_freq data frame to filter out specified word patterns
specified_word_pattern <- "_disc"  # Change this pattern as needed

# Filter out words matching the specified pattern
specified_word_freq <- all_word_freq %>%
  filter(str_detect(word, specified_word_pattern))

# Print the resulting table
print(specified_word_freq)

specified_word_pattern <- "^disk"  # Word pattern for words starting with "disk"
specified_word_pattern <- "disk$"  # Word pattern for words ending with "disk"

all_word_freq %>% filter(str_detect(word, "^disk"))




##############################################################################
##############################################################################
#####            to extract words phrases ending with a word            ###### 
##############################################################################

# ENDS WITH (function) -----
library(stringr)
library(stringi)
library(dplyr)

# Define the function
extract_prefixes <- function(n, your_word, input_column) {
  # Create the part of the regular expression for n words
  word_pattern <- paste0(rep("([^\\s]+)", n), collapse = "\\s")
  # Reverse the word we're looking for (if not reversed, computation time increases)
  your_word_reversed <- stri_reverse(your_word)
  # Combine the pattern for the reversed "your_word" with the word pattern
  pattern <- paste0("(\\b", your_word_reversed, "\\b)\\s", word_pattern)
  # Reverse the text in the input column (if the text not reversed, computation time increases)
  reversed_text <- stri_reverse(input_column)
  # Apply the regular expression to the reversed text
  prefix_extraction_reversed <- str_extract_all(reversed_text, pattern)
  # Reverse the matched sequences back to their original order
  prefix_extraction <- lapply(prefix_extraction_reversed, stri_reverse)
  df_pref_ext <- data.frame(matrix(unlist(prefix_extraction), 
                                   nrow = sum(sapply(prefix_extraction, length)),
                                   byrow = TRUE), stringsAsFactors=FALSE)
  colnames(df_pref_ext)[1] <- "term"
  df_pref_ext  <- df_pref_ext  %>% group_by(term) %>% summarise(number = n()) %>% arrange(desc(number))
  
  return(df_pref_ext)
}

df_pref_ext <- extract_prefixes(n= 4, your_word= "markov", input_column= deneme_patent$lemma_correction)

your_word <- "markov"
n <- 4
input_column <- deneme_patent$lemma_correction
result <- extract_prefixes(n, your_word, input_column)



##############################################################################
##############################################################################
#####            to extract words phrases starting with a word          ###### 
##############################################################################

# STARTS WITH (function) -----
library(stringr)
library(dplyr)

# Define the function
extract_suffixes <- function(n, your_word, input_column) {
  # Create the part of the regular expression for n words
  word_pattern <- paste0(rep("([^\\s]+)", n), collapse = "\\s")
  # Combine the pattern for "your_word" with the word pattern
  pattern <- paste0("(\\b(", your_word, ")\\b)\\s", word_pattern)
  suffix_extraction <- str_extract_all(input_column, pattern)
  suffix_extraction <- suffix_extraction[lapply(suffix_extraction,length) > 0]
  df_suff_ext <- data.frame(matrix(unlist(suffix_extraction), 
                                   nrow = sum(sapply(suffix_extraction, length)),
                                   byrow = TRUE), stringsAsFactors=FALSE)
  colnames(df_suff_ext)[1] <- "term"
  df_suff_ext  <- df_suff_ext  %>% group_by(term) %>% summarise(number = n()) %>% arrange(desc(number))
  
  return(df_suff_ext)
}


your_word <- "markov"
n <- 3
input_column <- deneme_patent$lemma_correction
result <- extract_suffixes(n, your_word, input_column)

result <- extract_suffixes(n= 3, your_word = "markov" , input_column= deneme_patent$lemma_correction)


##############################################################################
##############################################################################
#####            to extract words phrases between words                 ###### 
##############################################################################

# BETWEEN (function) -----
library(stringr)
library(dplyr)

# Define the function
extract_between <- function(n_before, n_after, your_word, input_column) {
  # Create the part of the regular expression for n words before and after
  word_pattern_before <- paste0(rep("(\\b[^\\s]+)", n_before), collapse = "\\s")
  word_pattern_after <- paste0(rep("([^\\s]+)", n_after), collapse = "\\s")
  # Combine the word patterns with "\\s(\\b(your_word)\\b)\\s"
  pattern <- paste0(word_pattern_before, "\\s(\\b(", your_word, ")\\b)\\s", word_pattern_after)
  
  betwee_n_word_extraction <- str_extract_all(input_column, pattern)
  df_between_ext <- data.frame(matrix(unlist(betwee_n_word_extraction), 
                                      nrow = sum(sapply(betwee_n_word_extraction, length)),
                                      byrow = TRUE), stringsAsFactors=FALSE)
  colnames(df_between_ext)[1] <- "term"
  df_between_ext  <- df_between_ext  %>% group_by(term) %>% summarise(number = n()) %>% arrange(desc(number))
  
  return(df_between_ext)
}

your_word <- "markov"
n_before <- 2
n_after <- 3
input_column <- deneme_patent$lemma_correction
result <- extract_between(n_before, n_after, your_word, input_column)




##############################################################################
##############################################################################
#####            Drawing two-character words containing numbers         ###### 
##############################################################################

library(stringr)
library(dplyr)

# Pattern for extracting two-character terms with one letter and one number
pattern <- "(?<!\\S)(?:(?:[a-zA-Z][0-9])|(?:[0-9][a-zA-Z]))(?!\\S)"
# Extract terms
n_char_mixed_terms <- str_extract_all(STM_text_final_version$STM_text, pattern)
# Create data frame
df_n_char_ext <- data.frame(matrix(unlist(n_char_mixed_terms), 
                                   nrow = sum(sapply(n_char_mixed_terms, length)),
                                   byrow = TRUE), stringsAsFactors=FALSE)
colnames(df_n_char_ext)[1] <- "term"
# Group and count
a_n_pref_ext  <- df_n_char_ext  %>% group_by(term) %>% summarise(number = n()) %>% arrange(desc(number))
openxlsx::write.xlsx(x = a_n_pref_ext, file = "2_letter_mixed_terms_eg_2d.xlsx")
getwd()
##### Drawing two-character words containing numbers END 






########################## START
##########################
# The following code extracts the words and their frequencies, one of the sctripts which used  STM trials to evaluate N-Gram and topic words. START
library(dplyr)
library(stringr)
library(stringi)
# C:/Users/ozbay/OneDrive - XXXX/R_örnekler_final/function for starts wih ends with kelime arama nGram arama kelime çifti arama extractionUME.R

your_word <- "position"

result_start <- extract_suffixes(n= 1, your_word, input_column= patents_cluster_in_text$STM_text_Cluster)
result_end <- extract_prefixes(n= 1, your_word, input_column= patents_cluster_in_text$STM_text_Cluster)

# Extracting word parts (i.e robot* or *robot)
library(tidytext)
library(tidyr)
# Tokenize the text into words and count their frequencies
all_word_freq <- patents_cluster_in_text %>%
  mutate(STM_text_Cluster = str_split(STM_text_Cluster, "\\s+")) %>%
  unnest(STM_text_Cluster) %>%
  count(word = STM_text_Cluster) %>%
  arrange(desc(n))

# Now, you can use the all_word_freq data frame to filter out specified word patterns
specified_word_pattern <- "^transpl"  # Change this pattern as needed

# Filter out words matching the specified pattern
specified_word_freq <- all_word_freq %>%
  filter(str_detect(word, specified_word_pattern))

# Print the resulting table
print(specified_word_freq)

specified_word_pattern <- "^disk"  # Word pattern for words starting with "disk"
specified_word_pattern <- "disk$"  # Word pattern for words ending with "disk"

all_word_freq %>% filter(str_detect(word, "^overcon"))
all_word_freq %>% filter(str_detect(word, "disk$"))

all_word_freq %>% filter(str_detect(word, "_belt$"))
all_word_freq %>% filter(str_detect(word, "^demons"))
# Above are the scripts extracting the words and their frequencies, which I used after each STM trials to evaluate N-Gram and topic words.
########################## END




