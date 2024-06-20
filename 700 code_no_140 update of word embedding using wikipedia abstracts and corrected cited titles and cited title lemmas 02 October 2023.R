# I had created word embedding using My_Robotics_Corpus (it includes 107 pdf robotic boos and WOS and Patend and Cited title texts.)
# I made lots of corrections in cited title text, and I downloaded wikipdia abstracts.
# I will combine all these data and create an new word embedding.



# I will combine my previous robotic Corpus with wikipedia
load("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic_big_corpus_final_aug_2023.RData")
# remove all except "My_Robotics_Corpus"


load("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/untidy_lemmatized_wikipedia_full.Rdata")
load("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/wikipedia_abstracts.RData")
load("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/corrected_tokens_of_cited_title_text_for_word_embedding.Rdata")
load("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/lemmatized_cited_title_text_for_word_embedding.Rdata")


ls()
rm(part5)
ls()
# "corrected_tokens_of_cited_title_text_for_word_embedding" => cited titles için düzletme işlemleri yapılmış metin
# "lemmatized_cited_title_text_for_word_embedding" => cited titles için düzletme işlemleri yapılmış temmatize metin
# "My_Robotics_Corpus" => Dah önce hazırladığım word embedding metni ayrıntı için şuna bak=> "C:\Users\ozbay\OneDrive - XXXX\R_TIK_6_GloVe\preparing word embedding of robotic corpus UME temmuz 2023.R"
# "wikipedia_abstracts_df" => wikipedia abstracts =>   "C:\Users\ozbay\OneDrive - XXXX\R ortak\WoS Cited\wikipedia text manipulation.R"
# "untidy_lemmatized_wikipedia_full"  => wikipedia abstracts =>    "C:\Users\ozbay\OneDrive - XXXX\R ortak\WoS Cited\untidy of lemmatized wikipedia texts.R"                 



colnames(corrected_tokens_of_cited_title_text_for_word_embedding) <- "text"
colnames(lemmatized_cited_title_text_for_word_embedding) <- "text"
colnames(untidy_lemmatized_wikipedia_full) <- "text"
colnames(wikipedia_abstracts_df) <- "text"


My_Robotics_Corpus_UPDATED_wiki <- bind_rows(corrected_tokens_of_cited_title_text_for_word_embedding,
                                              lemmatized_cited_title_text_for_word_embedding,
                                              My_Robotics_Corpus,
                                              untidy_lemmatized_wikipedia_full,
                                              wikipedia_abstracts_df)


setwd("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe")
save(My_Robotics_Corpus_UPDATED_wiki, file = "My_Robotics_Corpus_UPDATED_wiki.Rdata")
rm(corrected_tokens_of_cited_title_text_for_word_embedding,
   lemmatized_cited_title_text_for_word_embedding,
   My_Robotics_Corpus,
   untidy_lemmatized_wikipedia_full,
   wikipedia_abstracts_df)


##################################
# Training a GloVe word embedding
# https://medium.com/cmotions/nlp-with-r-part-2-training-word-embedding-models-and-visualize-results-ae444043e234
##################################

load("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/My_Robotics_Corpus_UPDATED_wiki.Rdata")
library(text2vec)
# We need to tokenize our already tokenized set as input for text2vec
it <- itoken(My_Robotics_Corpus_UPDATED_wiki$text, 
             tokenizer = space_tokenizer,
             # BENCE GEREK YOK ids = Corpus_Lemmatized_new$ID,
             progressbar = TRUE)

# create a vocabulary out of the tokenset (stopword removal and bi-grams are optional)
vocab <- create_vocabulary(it) # use uni-grams

# text2vec has the option to prune the vocabulary of low-frequent words
vocab <- prune_vocabulary(vocab, term_count_min = 15) # I increased from 5 to 15. 03/10/2023
dim(vocab) # [1] 709898      3
sum(vocab$term_count) # [1] 1356386957
# 709898 words and 1.356.386.957 tokens

# Constructing term-co-occurence matrix (TCM).
vectorizer <- vocab_vectorizer(vocab)
# Create a term-co-occurence: I will use a skipgram window of 5 (symmetrical)
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)


000000000000000000000000
# tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)

# Warning message:
#  In eval(formal.args[[as.character(substitute(arg))]], envir = sys.frame(sysP)) :
#  restarting interrupted promise evaluation
000000000000000000000000


# save.image("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic_big_corpus_final_aug_2023.RData") # 14/08/2023
# load("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/robotic_big_corpus_final_aug_2023.RData")

# x_max= 100 in the original paper.
# Pennington, J., Socher, R., & Manning, C. D. (2014, October). Glove: Global vectors for word representation. In Proceedings of the 2014 conference on empirical methods in natural language processing (EMNLP) (pp. 1532-1543).
# For all our experiments,xmax=100, α=3/4 ... with initial learning rate of 0.05.
# ... We run 50 iterations for vectors smaller than 300 dimensions, and 100 iterations otherwise (Pennington, et al. 2014).

# set up the embedding matrix and fit model
glove_model <- GloVe$new(rank = 300, x_max = 100, learning_rate = 0.04)

system.time({
  glove_embedding = glove_model$fit_transform(tcm, n_iter = 200, n_threads = 8)
}) # elapsed: 65346.48 



#> The GloVe model learns two sets of word vectors: main and context. 
#> Best practice is to combine both the main word vectors and the context word vectors into one matrix.
# combine main embedding and context embeddings (sum) into one matrix
dim(glove_embedding) # [1] 709898    300
dim(glove_model$components) # [1]    300 709898
#> While both of word-vectors matrices can be used as result it usually better (idea from GloVe paper) to average or take a sum of main and context vector:
glove_embedding = glove_embedding + t(glove_model$components) # the transpose of the context matrix

save(glove_embedding, file = "glove_embedding_wiki_xmax_100.Rdata")

load("C:/Users/ozbay/OneDrive - XXXX/R_TIK_6_GloVe/glove_embedding_wiki_xmax_100.Rdata")

dim(glove_embedding)



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







