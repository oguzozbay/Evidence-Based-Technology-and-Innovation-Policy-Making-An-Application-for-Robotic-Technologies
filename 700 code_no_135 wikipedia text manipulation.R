# I will import wikipedia abstracts text and then I will prepare the text for word embedding.
# I will not do word embedding. I will manipulate the text and make it ready for word embedding operations



# Source page :
#  https://databus.dbpedia.org/dbpedia/collections/dbpedia-snapshot-2022-12
# File name : Long abstracts from Wikipedia articles (2022.12)
# Path for download
# https://databus.dbpedia.org/dbpedia/text/long-abstracts/2022.12.01/long-abstracts_lang=en.ttl.bzip2

# Below is the information of this text data.
# This text data is the latest releases of core data from en.wikipedia.org (as of 15 September 2023)
# Text: a long version of DBpedia abstracts (the text before the table of contents)
# web page is given below:
# https://databus.dbpedia.org/dbpedia/collections/dbpedia-snapshot-2022-12
# file name is given below:
# Long abstracts from Wikipedia articles (2022.12)
# Path for download is as follows:
# https://databus.dbpedia.org/dbpedia/text/long-abstracts/2022.12.01/long-abstracts_lang=en.ttl.bzip2
# The date of download is 15 September 2023

library(tidyverse)
library(data.table)
library(stringi)
library(stringr)

setwd("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited")

wikipedia_abstracts <- read_lines("wikipedia_long_abstracts_2022.txt") # first

# wikipedia_abstracts <- read.table("wikipedia_long_abstracts_2022.txt", header=FALSE, stringsAsFactors=FALSE, sep="\t") # new

titles <- wikipedia_abstracts %>%
  str_extract(pattern = '(?<=<http://dbpedia.org/resource/).*(?=> <http://dbpedia.org/ontology/abstract>)') # %>% na.omit()


abstract <- wikipedia_abstracts %>%
  str_extract(pattern = '(?<=<http://dbpedia.org/ontology/abstract> ").*(?="@en)') # %>% na.omit()

titles <- tibble(titles = titles)
abstract <- tibble(titles = abstract)

wikipedia_abstracts_df <- tibble(title = titles, wiki_abstract = abstract)

colnames(wikipedia_abstracts_df)
na_rows_title <- wikipedia_abstracts_df %>% filter(is.na(title))
na_rows_wiki_abstract <- wikipedia_abstracts_df %>% filter(is.na(wiki_abstract))

rm(abstract, na_rows_title, na_rows_wiki_abstract, titles, wikipedia_abstracts)
# save.image("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/wikipedia_abstracts.RData")

load("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/wikipedia_abstracts.RData")

# there is a problem in col names which I did not understand.
sil <- wikipedia_abstracts_df$title
sil2 <- wikipedia_abstracts_df$wiki_abstract
colnames(sil2) <- "abstract"
sil$abstract <- sil2$abstract
wikipedia_abstracts_df <- sil # there was a problem in col names which I did not understand.
rm(sil, sil2)

colnames(wikipedia_abstracts_df)

# replace _ to space in title column.
wikipedia_abstracts_df$titles <- gsub("_", " ", wikipedia_abstracts_df$titles)


# save.image("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/wikipedia_abstracts.RData")

library(data.table)
setDT(wikipedia_abstracts_df)
wikipedia_abstracts_df[, abstract := paste(titles, abstract)]

wikipedia_abstracts_df <- as.data.frame(wikipedia_abstracts_df)


library(stringi)
wikipedia_abstracts_df$abstract_ASCII  <- stri_trans_general(wikipedia_abstracts_df$abstract, "Latin-ASCII")


library(data.table)
setDT(wikipedia_abstracts_df)
subset_dt <- wikipedia_abstracts_df[abstract != abstract_ASCII]
rm(subset_dt)

wikipedia_abstracts_df <- as.data.frame(wikipedia_abstracts_df)
wikipedia_abstracts_df$abstract <- NULL
wikipedia_abstracts_df$abstract_ASCII <- tolower(wikipedia_abstracts_df$abstract_ASCII)



library(data.table)
library(stringi)

# Ensure the data is in data.table format
setDT(wikipedia_abstracts_df)

# Use stringi's stri_replace_all_regex to replace punctuations
wikipedia_abstracts_df[, abstract_ASCII := stri_replace_all_regex(abstract_ASCII, "[!\"#$%&'()*+,-./:;<=>?@\\[\\]^`{|}~]", " ")]


library(stringr)
wikipedia_abstracts_df[, abstract_ASCII := str_squish(abstract_ASCII)]


# I will delete words in the abstract_ASCII column that don't contain at least one letter from a to z.
library(data.table)
wikipedia_abstracts_df[, titles := NULL]

wikipedia_abstracts_df[, abstract_ASCII := sapply(strsplit(abstract_ASCII, " "), 
                                                  function(x) paste(x[grepl("[a-z]", x)], collapse = " "))]



# save.image("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/wikipedia_abstracts.RData")


library(data.table)
wikipedia_abstracts_df[, abstract_ASCII := gsub("\\\\", "", abstract_ASCII)]
wikipedia_abstracts_df[, abstract_ASCII := str_squish(abstract_ASCII)]
wikipedia_abstracts_df <- as.data.frame(wikipedia_abstracts_df)




# Calculate the rows for each part
n_rows <- nrow(wikipedia_abstracts_df)
rows_per_part <- ceiling(n_rows / 5)

# Split the data into parts
start1 <- 1; end1 <- start1 + rows_per_part - 1
part1 <- wikipedia_abstracts_df[start1:end1, "abstract_ASCII", drop=FALSE]

start2 <- end1 + 1; end2 <- start2 + rows_per_part - 1
part2 <- wikipedia_abstracts_df[start2:end2, "abstract_ASCII", drop=FALSE]

start3 <- end2 + 1; end3 <- start3 + rows_per_part - 1
part3 <- wikipedia_abstracts_df[start3:end3, "abstract_ASCII", drop=FALSE]

start4 <- end3 + 1; end4 <- start4 + rows_per_part - 1
part4 <- wikipedia_abstracts_df[start4:end4, "abstract_ASCII", drop=FALSE]

start5 <- end4 + 1; end5 <- n_rows
part5 <- wikipedia_abstracts_df[start5:end5, "abstract_ASCII", drop=FALSE]


gc()

library(spacyr)
spacy_initialize()



gc()
# Lemmatizing part1
system.time({
  lemma_part1 <- spacy_parse(part1$abstract_ASCII, multithread = TRUE, nounphrase = FALSE, lemma= TRUE)
})

a1 <- "burada kaldım 1"
gc()
# save.image("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/wikipedia_abstracts.RData")

# Lemmatizing part2
system.time({
  lemma_part2 <- spacy_parse(part2$abstract_ASCII, multithread = TRUE, nounphrase = FALSE, lemma= TRUE)
})

a2 <- "burada kaldım 2"
gc()
# save.image("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/wikipedia_abstracts.RData")

# Lemmatizing part3
system.time({
  lemma_part3 <- spacy_parse(part3$abstract_ASCII, multithread = TRUE, nounphrase = FALSE, lemma= TRUE)
})

a3 <- "burada kaldım 3"
gc()
# save.image("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/wikipedia_abstracts.RData")


# Lemmatizing part4
system.time({
  lemma_part4 <- spacy_parse(part4$abstract_ASCII, multithread = TRUE, nounphrase = FALSE, lemma= TRUE)
})


a4 <- "burada kaldım 4"
gc()
# save.image("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/wikipedia_abstracts.RData")


# because of data size my pc was not continued after this point. So a will remove unnecessary files.
# save(lemma_part1, file = "wikipedia_lemma_for_embedding_part_1.Rdata")
# save(lemma_part2, file = "wikipedia_lemma_for_embedding_part_2.Rdata")
# save(lemma_part3, file = "wikipedia_lemma_for_embedding_part_3.Rdata")
# save(lemma_part4, file = "wikipedia_lemma_for_embedding_part_4.Rdata")

rm(part1, part2, part3, part4, lemma_part1, lemma_part2, lemma_part3, 
   lemma_part4, a1, a2, a3, end1, end2, end3, end4, end5, start1, start2, start3, start4, start5)

# save.image("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/wikipedia_abstracts.RData")
load("C:/Users/ozbay/OneDrive - XXXX/R ortak/WoS Cited/wikipedia_abstracts.RData")


library(spacyr)
spacy_initialize()

# Lemmatizing part5
system.time({
  lemma_part5 <- spacy_parse(part5$abstract_ASCII, multithread = TRUE, nounphrase = FALSE, lemma= TRUE)
}) # elapsed 10666.09  (10 - 12.000 arasında değişti yukarıdakiler)

a5 <- "burada kaldım 5"
gc()
# save(lemma_part5, file = "wikipedia_lemma_for_embedding_part_5.Rdata")
