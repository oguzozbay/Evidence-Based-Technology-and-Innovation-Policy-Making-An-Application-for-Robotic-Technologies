# I try different K numbers and find the most suitable K.
# Then I will choose the most suitable model for the most suitable K (I did it with another code => "C:\Users\ozbay\OneDrive - XXXX\R_May_2023\STM_with_clusters_May_2023_91_Topics.R")

# stm (version 1.3.6)
# https://www.rdocumentation.org/packages/stm/versions/1.3.6
memory.limit()
memory.limit(size = NA)
memory.limit(size=9999999999999)
gc()

# https://juliasilge.com/blog/evaluating-stm/
# https://rstudio-pubs-static.s3.amazonaws.com/867806_df2a1b9db561438a84eda5d3e0a9f3ca.html
# https://warin.ca/shiny/stm/#section-understand
# http://thomaselliott.me/pdfs/earl/topic_modeling.html
# https://github.com/cbail/textasdata
# https://www.youtube.com/watch?v=IUAHUEy1V0Q
# https://github.com/dondealban/learning-stm
# https://francescocaberlin.blog/2019/06/26/messing-around-with-stm-part-iiia-model-selection/
# LDAvis paketine aktarmak için:
# https://rdrr.io/cran/stm/man/toLDAvis.html

library(dplyr)
library(tidytext)
library(quanteda)
library(stm)
library(furrr)
library(ggplot2)
library(purrr)
library(tidyr)


getwd()
setwd("C:/Users/ozbay/OneDrive - XXXX/R_May_2023")

# I will import STM data.
load("C:/Users/ozbay/OneDrive - XXXX/R_March_2023/M_24_02_2023.RData")
colnames(M)
# [1] "UT"                  "STM_text_16_02_2023" "publication_year"  
# UT is WoS numbers of each document.
# STM_text_16_02_2023 is union of abstract, title and keywords of each article.
# STM_text_16_02_2023 is paritally corrected (i.e spell checked), cleaned, lemmatized and NGrammed text.
# User defined stopwords are also removed in M$STM_text_16_02_2023 
# see "STM 2023 05 February.R" for detail.



# I will import WoS numbers and Cluster numbers (there are total 9 clusters)
load("C:/Users/ozbay/OneDrive - XXXX/R_deneme_feb_2023/WoS_per_Cluster_No_April_2023.RData")
# See for clustering details => C:\Users\ozbay\OneDrive - XXXX\R_deneme_feb_2023\Publication_clustering_April_2023_final.R
k9_df # A Data frame includes WoS and Cluster numbers (there are total 9 clusters)
k9_df$Cluster <- as.factor(k9_df$Cluster)

M <- M %>% right_join(k9_df)
M$united_SC_WC <- NULL

# Now I will tokenize WoS abstracts (library(tidytext))
tidy_M <- M %>% unnest_tokens(output= word, input= STM_text_16_02_2023, token = "words")
tidy_M %>% count(word, sort = TRUE)


# Input for STM may be a dfm from quanteda.
# Roberts, M. E., Stewart, B. M., & Tingley, D. (2019). Stm: An R package for structural topic models. Journal of Statistical Software, 91, 1-40.

# STM vignette:
#> stm(documents,vocab,K, prevalence = NULL, ...)
#> documents: The document term matrix to be modeled. These can be supplied in the native stm format, 
#> a sparse term count matrix with one row per document and one column per term, or a quanteda dfm (document-feature matrix) object.
#> When using the sparse matrix or quanteda format this will include the vocabulary and, for quanteda, optionally the metadata.
#> If using the native list format, the object must be a list of with each element corresponding to a document. 
#> Each document is represented as an integer matrix with two rows, and columns equal to the number of unique vocabulary words in the document. 
#> The first row contains the 1-indexed vocabulary entry and the second row contains the number of times that term appears. 
#> This is similar to the format in the lda package except that (following R convention) the vocabulary is indexed from one.
#> Corpora can be imported using the reader function and manipulated using the prepDocuments. Raw texts can be ingested using textProcessor. 
#> Note that when using quanteda dfm directly there may be higher memory use (because the texts and metadata are stored twice). 
#> You can convert from quanteda’s format directly to our native format using the quanteda function convert.


# I will create document feature matrix (library(tidytext))
M_dfm <- tidy_M %>% count(UT, word) %>% cast_dfm(UT, word, n) # cast_dfm(data, document, term, value, ...)
M_dfm # Document-feature matrix of: 211,585 documents, 112,845 features (99.94% sparse) and 0 docvars


# keep only words occurring at least 4 times and at least in 3 documents (library(quanteda))
M_dfm_trim_1 <- dfm_trim(M_dfm, min_termfreq = 4, min_docfreq = 3, termfreq_type = "count", docfreq_type = "count")
M_dfm_trim_1 # Document-feature matrix of: 211,585 documents, 41,954 features (99.83% sparse) and 0 docvars

# Now I will remove single character tokens (i.e. a, b, c, ... etc.)
tokens <- tidy_M %>% count(word, sort = TRUE)
tokens$n_char <- 0
tokens$n_char <-  nchar(tokens$word)
single_characters <- tokens %>% filter(n_char == 1)
single_characters <- list(single_characters$word)
rm(tokens)

M_dfm_trim_2 <- dfm_remove(M_dfm_trim_1, single_characters, verbose = TRUE) # https://stackoverflow.com/questions/55832844/remove-words-from-a-dtm
head(M_dfm_trim_1) # Document-feature matrix of: 6 documents, 41,954 features (99.83% sparse) and 0 docvars.
head(M_dfm_trim_2) # Document-feature matrix of: 6 documents, 41,921 features (99.83% sparse) and 0 docvars.
rm(M_dfm_trim_1)
rm(M_dfm)
rm(tidy_M)

#> (STM Vignette) Note that when using quanteda dfm directly there may be higher memory use (because the texts and metadata are stored twice).
#> You can convert from quanteda’s format directly to our native format using the quanteda function convert.

colnames(M)
M_docvars <- M %>% select(publication_year, Cluster)

#> You can convert from quanteda’s format directly to our native format using the quanteda function convert.
out <- convert(M_dfm_trim_2, to = 'stm', docvars = M_docvars)

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_May_2023/STM_with_clusters_May_2023_Environment.RData") # 27/04/2023


# https://stackoverflow.com/questions/65762781/structural-topic-model-stm-error-unreliable-value-future-none-unexpect
# https://stackoverflow.com/questions/75521357/ensure-reproducibility-across-purrrmap-and-furrrfuture-map
# https://juliasilge.com/blog/evaluating-stm/


library(stm)
library(furrr)
plan(multisession, workers = 8)
# plan(multisession)
# nbrOfWorkers()  ## == availableCores()
# plan(multisession, workers = 3) # https://juliasilge.com/blog/evaluating-stm/
set.seed(2023)

system.time({
  many_models <-tibble(K = c(70,75,80,85,90,95,100,105,110,115)) %>%
    mutate(topic_model = future_map(
      K, ~stm(out$documents, out$vocab, data = out$meta, K = ., 
              prevalence = ~s(publication_year) + Cluster, 
              init.type = 'Spectral', max.em.its=100, seed = 2023, verbose = FALSE),
      .options=furrr_options(seed = TRUE)
      ))
}) # elapsed =  approx 29 hours (01/05/2023)

plan(sequential) ## Explicitly close multisession workers by switching plan
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_May_2023/STM_with_clusters_May_2023_Environment.RData") # 01/05/2023
# https://juliasilge.com/blog/evaluating-stm/
#> ... Now that we’ve fit all these topic models with different numbers of topics, 
#> we can explore how many topics are appropriate/good/“best”. 
#> The code below to find k_result is similar to stm’s own searchK() function, 
#> but it allows you to evaluate models trained on a sparse matrix (or a quanteda dfm) 
#> instead of only stm’s corpus data structure, as well as to dig into the model diagnostics yourself in detail. 
#> Some of these functions were not originally flexible enough to take a sparse matrix or dfm as input, 
#> so I’d like to send huge thanks to Brandon Stewart, stm’s developer, for adding this functionality.

system.time({
  heldout <- make.heldout(M_dfm_trim_2)
}) # elapsed 206.53 

# REFERENCE: https://www.rdocumentation.org/packages/stm/versions/1.3.6
# exclusivity(model, M = 10, frexw = 0.7) # Calculate an exclusivity metric for an STM model.
# model: the STM object
# M: the number of top words to consider per topic
# frexw: the frex weight


# REFERENCE for below code: https://juliasilge.com/blog/evaluating-stm/
library(purrr)
k_result <- many_models %>%
  # The map functions transform their input by applying a function to each element and returning a vector the same length as the input.
  # map(), map_if() and map_at() always return a list.
  # map(.x, .f, ...)
  # .x: A list or atomic vector.
  # .f: A function, formula, or atomic vector.
  
  mutate(exclusivity = map(topic_model, exclusivity),
         # https://rdrr.io/cran/stm/man/semanticCoherence.html
         # exclusivity(model, M = 10, frexw = 0.7)
         # model: the STM object
         # M: the number of top words to consider per topic
         # frexw: the frex weight
         
         semantic_coherence = map(topic_model, semanticCoherence, M_dfm_trim_2),
         # https://rdrr.io/cran/stm/man/semanticCoherence.html
         # semanticCoherence(model, documents, M = 10)
         # Arguments: 
         # model: the STM object
         # documents: the STM formatted documents (see stm for format).
         # M: the number of top words to consider per topic
         
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, M_dfm_trim_2),
         # SELECT "checkResiduals" AND PRESS F2 =>
         # function (stmobj, documents, tol = 0.01) 
         
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

# elapsed approx I forget to save. But as I remember it has taken 5-6 hours. 01/05/2023

# save.image("C:/Users/ozbay/OneDrive - XXXX/R_May_2023/STM_with_clusters_May_2023_Environment.RData") # 01/05/2023


#> We’re evaluating things like the residuals, the semantic coherence of the topics, the likelihood for held-out datasets, and more.
#> We can make some diagnostic plots using these quantities to understand how the models are performing at various numbers of topics.
#> The following code makes a diagnostic plot similar to one that comes built in to the stm package.


# library(tidyr) & library(purrr)

library(dplyr)
library(quanteda)
library(ggplot2)
library(purrr)
library(tidyr)



k_result %>%
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
       title = "Model diagnostics by number of topics",
       subtitle = "02/05/2023 Higher 'Held-out likelihood' and 'Sematic Coherence', and lower 'Residuals' indicate an ideal number of topics. \nIn the three graphs (Held-out, Lower bound, and Residuals), the changes in the number of topics are close to linear. \nHowever, the change of Sematic Coherence versus number of topics fluctuates when the number of topics is around 90.")



# Optimal results would demonstrate relatively high semantic coherence, and a high held-out likelihood...
# https://juliasilge.com/blog/evaluating-stm/
#> Semantic coherence is maximized when the most probable words in a given topic frequently co-occur together, 
#> and it’s a metric that correlates well with human judgment of topic quality. 
#> Having high semantic coherence is relatively easy, though, if you only have a few topics dominated by very common words, 
#> so you want to look at both semantic coherence and exclusivity of words to topics. 
#> It’s a tradeoff. Read more about semantic coherence in the original paper about it (given below)
#> Mimno, D., Wallach, H., Talley, E., Leenders, M., & McCallum, A. (2011, July).
#> Optimizing semantic coherence in topic models. In Proceedings of the 2011 conference on empirical methods in natural language processing (pp. 262-272).


library(stm)
library(furrr)
plan(multisession, workers = 8)
# plan(multisession)
# nbrOfWorkers()  ## == availableCores()
# plan(multisession, workers = 3) # https://juliasilge.com/blog/evaluating-stm/
set.seed(2023)

system.time({
  many_models_88_97 <-tibble(K = c(88,89,90,91,92,93,94,95,96,97)) %>%
    mutate(topic_model = future_map(
      K, ~stm(out$documents, out$vocab, data = out$meta, K = ., 
              prevalence = ~s(publication_year) + Cluster, 
              init.type = 'Spectral', max.em.its=100, seed = 2023, verbose = FALSE),
      .options=furrr_options(seed = TRUE)
    ))
}) # elapsed =  approx 20 hours (03/05/2023)
plan(sequential) ## Explicitly close multisession workers by switching plan


# KAYNAK (BOOK VERSION) https://bookdown.org/Maxine/tidy-text-mining/tuning-number-of-topics.html
library(purrr)
k_result_2 <- many_models_88_97 %>%
  # The map functions transform their input by applying a function to each element and returning a vector the same length as the input.
  # map(), map_if() and map_at() always return a list.
  # map(.x, .f, ...)
  # .x: A list or atomic vector.
  # .f: A function, formula, or atomic vector.
  
  mutate(exclusivity = map(topic_model, exclusivity),
         # https://rdrr.io/cran/stm/man/semanticCoherence.html
         # exclusivity(model, M = 10, frexw = 0.7)
         # model: the STM object
         # M: the number of top words to consider per topic
         # frexw: the frex weight
         
         semantic_coherence = map(topic_model, semanticCoherence, M_dfm_trim_2),
         # https://rdrr.io/cran/stm/man/semanticCoherence.html
         # semanticCoherence(model, documents, M = 10)
         # Arguments: 
         # model: the STM object
         # documents: the STM formatted documents (see stm for format).
         # M: the number of top words to consider per topic
         
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, M_dfm_trim_2),
         # SELECT "checkResiduals" AND PRESS F2 =>
         # function (stmobj, documents, tol = 0.01) 
         
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))



# save.image("C:/Users/ozbay/OneDrive - XXXX/R_May_2023/STM_with_clusters_May_2023_Environment.RData") # 02/05/2023

# library(tidyr) & library(purrr)


k_result_2 %>%
   transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(linewidth = 1.5, alpha = 0.7, show.legend = TRUE) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(88, 97, by = 1)) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics",
       subtitle = "03/05/2023 According to the graphics, it seems that 91 or 93 topics would be appropriate. \nHeld-out likelihood increases as the number of topics increases, let's not take it into consideration too much. \nThe same is true for lower bound, where 91 and 91 are relatively close. Residuals 91 and 93 are almost equal.\nIn this context, it seems more reasonable to choose 93 topics with a higher Semantic coherence.")


# number of topics is 91



system.time({
set.seed(2023)
mod.out <- selectModel(documents= out$documents,
                       vocab= out$vocab,
                       K=91, 
                       prevalence = ~s(publication_year) + Cluster, 
                       data = out$meta,
                       init.type = "LDA",
                       max.em.its = 500,
                       runs=100,
                       seed = 2023)

})

plotModels(mod.out)

# selected<-mod.out$runout[[1]]







