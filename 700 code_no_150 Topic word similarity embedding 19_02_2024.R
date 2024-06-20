
# I used this word embedding only when calculating the similarity of keywords of WOS articles at the end of the thesis.
# The difference of this word embed is that it also includes the new n-grams that I used when creating the final patent STM model.
# However, since UMAP operations would take a long time, I did not re-UMAP.
# To summarize, I updated the word embedding that I used for WMD (i.e. novelty determination) using the STM text containing the new N-Grams that I determined in the patent STM, as in this code. I used the word embedding, which is the output of this code, for keyword similarity of WOS articles.




# The following is just the missing patents data (new patents I added later):
load("C:/Users/ozbay/OneDrive - XXXX/R ortak/patent STM 13 temmuz 2023/missing_patents/missing_patents_V5.RData")
load("C:/Users/ozbay/OneDrive - XXXX/R_patent_2024/all patents from 2005 to end of 2023 ready for STM 05_01_2024_V6.RData") # for casper


missing_patents_05_01_2024 <- missing_patents_05_01_2024 %>% select(TI_AB, lemma_corrected_in_Notepad)
patents_08_01_2024 <- patents_08_01_2024 %>% select(STM_text)

# Define the list of words to delete
# Update the list of words to delete to include "d" to "y"
delete_list <- c(letters[1:26], # Includes "a" to "z"
                 "aa", "bb", "cc", "dd", "ee", "ff", "gg", "hh", "ii", "jj", "kk", "ll", "mm",
                 "nn", "oo", "pp", "qq", "rr", "ss", "tt", "uu", "vv", "ww", "xx", "yy", "zz",
                 "can_be", "be_use_for", "plurality_of", "so_that", "which_be", "such_as", "sf", "li", "s0", 
                 "thereof", "still", "said", "whose", "whole", "ul0002", "constitution")

# Create the regular expression pattern to match any of the words in the delete list
# \\b represents a word boundary, ensuring exact matches
pattern <- paste0("\\b(", paste(delete_list, collapse = "|"), ")\\b")

# Use gsub to remove the words from the STM_text column
patents_08_01_2024$deleted <- gsub(pattern, "", patents_08_01_2024$STM_text)

# save.image("C:/Users/ozbay/OneDrive - XXXX/R topic words similarity/glove text for updating missing pattents 19_02_2024.RData")

sil <- patents_08_01_2024 %>% select (STM_text, deleted)
# Comparing original and replaced text
sil$check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(sil[,1], '\\s'), strsplit(sil[,2], '\\s'))
sil$check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), strsplit(sil[,2], '\\s'), strsplit(sil[,1], '\\s'))
sil_filtered <- sil %>% filter(check_1 != "" | check_2 != "")
############################################

# kontrol ettim sonra sil'i sildim
00000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000

load("C:/Users/ozbay/OneDrive - XXXX/R topic words similarity/glove text for updating missing pattents 19_02_2024.RData")

patents_08_01_2024$STM_text <- patents_08_01_2024$deleted
patents_08_01_2024$deleted <- NULL
patents_2005_2023_STM_text_for_embedding <- patents_08_01_2024
rm(patents_08_01_2024)

patents_2005_2023_STM_text_for_embedding # final version of patent STM text (i.e. 2005-2023 patents STM text)
missing_patents_05_01_2024 # text only missing patents (newly added patents). Original and lemmatized version. I am not sure to use which one.


# Now I will import old version (The version that I used in Words Mover's Distance)
# Look below code for the creation pf old code:
# "C:\Users\ozbay\OneDrive - XXXX\R_TIK_6_GloVe\update of word embedding using wikipedia abstracts and corrected cited titles and cited title lemmas 02 October 2023.R"

# Because I updated patents data I prepared final version of patent STM text and a text only missing patents (newly added patents)
# I had created word embedding using My_Robotics_Corpus (it includes 107 pdf robotic boos and WOS and Patent and Cited title texts.)
# I made lots of corrections in cited title text, and I downloaded Wikipedia abstracts.
# I will combine all these data and create an new word embedding.



# I will combine my previous robotic Corpus with Wikipedia and Newly added patents and patent final STM text
load("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic_big_corpus_final_aug_2023.RData")
# remove all except "My_Robotics_Corpus"; patents_2005_2023_STM_text_for_embedding & missing_patents_05_01_2024
ls() # [1] "My_Robotics_Corpus" "missing_patents_05_01_2024" "patents_2005_2023_STM_text_for_embedding"


load("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/untidy_lemmatized_wikipedia_full.Rdata")
load("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/wikipedia_abstracts.RData")
load("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/corrected_tokens_of_cited_title_text_for_word_embedding.Rdata")
load("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/lemmatized_cited_title_text_for_word_embedding.Rdata")


ls()
rm(part5)
ls()
# "corrected_tokens_of_cited_title_text_for_word_embedding" => cited titles için düzletme işlemleri yapılmış metin
# "lemmatized_cited_title_text_for_word_embedding" => cited titles için düzletme işlemleri yapılmış lemmatize metin
# "My_Robotics_Corpus" => Daha önce hazırladığım word embedding metni ayrıntı için şuna bak=> "C:\Users\ozbay\OneDrive - XXXX\R_TIK_6_GloVe\preparing word embedding of robotic corpus UME temmuz 2023.R"
# "wikipedia_abstracts_df" => wikipedia abstracts =>   "C:\Users\ozbay\OneDrive - XXXX\R ortak\WoS Cited\wikipedia text manipulation.R"
# "untidy_lemmatized_wikipedia_full"  => wikipedia abstracts =>    "C:\Users\ozbay\OneDrive - XXXX\R ortak\WoS Cited\untidy of lemmatized wikipedia texts.R"                 

# Yukarıdaki kısım için çizelge:
# file name of the text	                                                    version to be used	
#                                                                           original	lemmatize
# cited_ref_titles_part_1_UME_07_temmuz_2023.xlsx                               x         x
# cited_ref_titles_part_2_UME_07_temmuz_2023.xlsx	                              x       	x
# corpus_of_handook_books_and_articles.xlsx	                                    x	        x
# M_AB_untidy_specially_prepared_for_word_embedding_UME_07_temmuz_2023.xlsx	    x       	-
# robotic_patetns_AB_and_TI_corrected_text_UME_07_temmuz_2023.xlsx	            x         x
# STM_used_text_UME_07_temmuz_2023.xlsx	(Bu WOS olsa gerek)                     x       	-
# WoS_original_AB_and_TI_UME_07_temmuz_2023.xlsx	                              x       	x

# Yukarıdaki açıklamalardan yola çıkarak:
# Aşağıdakilerin tamamını dahil edeceğim. Yani yeni eklenen patentlerin Orijinal ve lemmatize halini ekleyeceğim ve Patent STM metnini ekleyeceğim.
# Bu durumda eski patentlerin STM text'i iki kere kullanılmış olacak, tek sıkıntı bu
# patents_2005_2023_STM_text_for_embedding # final version of patent STM text (i.e. 2005-2023 patents STM text)
# missing_patents_05_01_2024 # text only missing patents (newly added patents). Original and lemmatized version. I am not sure to use which one.


# Create a new dataframe with one column named "text"
# This column contains all rows from TI_AB followed by all rows from lemma_corrected_in_Notepad
missing_patents_single_column_05_01_2024 <- data.frame(
  text = c(missing_patents_05_01_2024$TI_AB, missing_patents_05_01_2024$lemma_corrected_in_Notepad)
)
rm(missing_patents_05_01_2024)

# Rename all dataframes column as "text"
colnames(patents_2005_2023_STM_text_for_embedding) <- "text"
colnames(corrected_tokens_of_cited_title_text_for_word_embedding) <- "text"
colnames(lemmatized_cited_title_text_for_word_embedding) <- "text"
colnames(untidy_lemmatized_wikipedia_full) <- "text"
colnames(wikipedia_abstracts_df) <- "text"



Corpus_wiki_and_new_patents <- bind_rows(corrected_tokens_of_cited_title_text_for_word_embedding, # WAS: My_Robotics_Corpus_UPDATED_wiki
                                             lemmatized_cited_title_text_for_word_embedding,
                                             My_Robotics_Corpus,
                                             untidy_lemmatized_wikipedia_full,
                                             wikipedia_abstracts_df,
                                             missing_patents_single_column_05_01_2024,
                                             patents_2005_2023_STM_text_for_embedding)


setwd("C:/Users/ozbay/OneDrive - XXXX/R topic words similarity")
save(Corpus_wiki_and_new_patents, file = "Corpus_wiki_and_new_patents_20_02_2024.Rdata")
rm(corrected_tokens_of_cited_title_text_for_word_embedding,
   lemmatized_cited_title_text_for_word_embedding,
   My_Robotics_Corpus,
   untidy_lemmatized_wikipedia_full,
   wikipedia_abstracts_df,
   missing_patents_single_column_05_01_2024,
   patents_2005_2023_STM_text_for_embedding)


##################################
# Training a GloVe word embedding
# https://medium.com/cmotions/nlp-with-r-part-2-training-word-embedding-models-and-visualize-results-ae444043e234
##################################

load("C:/Users/ozbay/OneDrive - XXXX/R topic words similarity/Corpus_wiki_and_new_patents_20_02_2024.Rdata")
library(text2vec)
# We need to tokenize our already tokenized set as input for text2vec
it <- itoken(Corpus_wiki_and_new_patents$text, # WAS: My_Robotics_Corpus_UPDATED_wiki
             tokenizer = space_tokenizer,
             progressbar = TRUE)

# create a vocabulary out of the tokenset (stopword removal and bi-grams are optional)
vocab <- create_vocabulary(it) # use uni-grams

# text2vec has the option to prune the vocabulary of low-frequent words
vocab <- prune_vocabulary(vocab, term_count_min = 15) # I increased from 5 to 15. 03/10/2023 and keep as same 20/02/2023
dim(vocab) # [1] 712616 / WAS: 709898 (Yeni patentleri eklemeden önceki durum)
sum(vocab$term_count) # [1] 1444259377 / WAS 1356386957
# 712616 words and 1,444,259,377 tokens

# Constructing term-co-occurence matrix (TCM).
vectorizer <- vocab_vectorizer(vocab)
# Create a term-co-occurence: I will use a skipgram window of 5 (symmetrical)
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)


# save.image("C:/Users/ozbay/OneDrive - XXXX/R topic words similarity/Corpus_wiki_and_new_patents_20_02_2024_V2_tcm_created.RData") # Glove embedding çıktıktan sonra silersin
load("C:/Users/ozbay/OneDrive - XXXX/R topic words similarity/Corpus_wiki_and_new_patents_20_02_2024_V2_tcm_created_delete_when_embedding_created.RData")

# x_max= 100 in the original paper.
# Pennington, J., Socher, R., & Manning, C. D. (2014, October). Glove: Global vectors for word representation. In Proceedings of the 2014 conference on empirical methods in natural language processing (EMNLP) (pp. 1532-1543).
# For all our experiments,xmax=100, α=3/4 ... with initial learning rate of 0.05.
# ... We run 50 iterations for vectors smaller than 300 dimensions, and 100 iterations otherwise (Pennington, et al. 2014).

# set up the embedding matrix and fit model
glove_model <- GloVe$new(rank = 300, x_max = 100, learning_rate = 0.04)

system.time({
  glove_embedding = glove_model$fit_transform(tcm, n_iter = 200, n_threads = 8)
}) # elapsed: 67301.80 = 19 hours


#> The GloVe model learns two sets of word vectors: main and context. 
#> Best practice is to combine both the main word vectors and the context word vectors into one matrix.
# combine main embedding and context embeddings (sum) into one matrix
dim(glove_embedding) # 712616    300
dim(glove_model$components) # [1]    300 712616
#> While both of word-vectors matrices can be used as result it usually better (idea from GloVe paper) to average or take a sum of main and context vector:
glove_embedding = glove_embedding + t(glove_model$components) # the transpose of the context matrix

# save(glove_embedding, file = "glove_embedding_wiki_xmax_100_new_patents_included.Rdata") # 22/02/2024

burada_kaldim <- "BURADA KALDIM!..."
getwd()




000000000000000000000000000
#> Let’s find out how well GloVe is doing on our robotic texts.
word <- glove_embedding["control", , drop = FALSE] # I wonder if glove_embedding_YEDEK gives good results?
cos_sim = sim2(x = glove_embedding, y = word, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)

word <- glove_embedding["vinci", , drop = FALSE]
cos_sim = sim2(x = glove_embedding, y = word, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)


#> Let’s find out how well GloVe is doing on our robotic texts.
word <- glove_embedding["paper", , drop = FALSE] # I wonder if glove_embedding_YEDEK gives good results?
cos_sim = sim2(x = glove_embedding, y = word, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)

word <- glove_embedding["nano", , drop = FALSE]
cos_sim = sim2(x = glove_embedding, y = word, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)


word <- glove_embedding["humanoid", , drop = FALSE]
cos_sim = sim2(x = glove_embedding, y = word, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)

word <- glove_embedding["haptic", , drop = FALSE]
cos_sim = sim2(x = glove_embedding, y = word, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)

word <- glove_embedding["effector", , drop = FALSE]
cos_sim = sim2(x = glove_embedding, y = word, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)

word <- glove_embedding["manned", , drop = FALSE]
cos_sim = sim2(x = glove_embedding, y = word, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)


