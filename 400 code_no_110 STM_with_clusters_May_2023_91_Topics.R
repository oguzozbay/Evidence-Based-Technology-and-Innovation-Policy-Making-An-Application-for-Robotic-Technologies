# I will choose the most appropriate model for K= 91 topics and determine the final STM model.

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

# library(dplyr)
# library(tidytext)
# library(tidyr)
# library(quanteda)
# library(stm)
# library(furrr)
# library(ggplot2)
# library(purrr)
# library(ggplot2)



getwd()
setwd("C:/Users/ozbay/OneDrive - XXXX/R_May_2023")

# load("C:/Users/ozbay/OneDrive - XXXX/R_May_2023/STM_with_clusters_May_2023_91_Topics_Environment.RData")


# library(tidytext)
# library(quanteda)
# library(stm)
# library(furrr)
# library(ggplot2)
# library(purrr)
# library(tidyr)


library(stm)
system.time({
set.seed(2023)
mod.out <- selectModel(documents= out$documents,
                       vocab= out$vocab,
                       K=91, 
                       prevalence = ~s(publication_year) + Cluster, 
                       data = out$meta,
                       init.type = "LDA",
                       max.em.its = 250,
                       runs=50,
                       seed = 2023)

}) # elapsed 660636 = 183 hours / 22/05/2023 
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_May_2023/STM_with_clusters_May_2023_91_Topics_Environment.RData")


# Now I will analyse models.
# plotting models

plotModels(
  models= mod.out,
  xlab = "Semantic Coherence",
  ylab = "Exclusivity",
  #     c(NA,2,NA,4, NA,6, NA,8, NA,10),
  pch = c(1:10),
  legend.position = "bottomleft"
) # model 1 ... 10


plotModels(
  models= mod.out,
  xlab = "Semantic Coherence",
  ylab = "Exclusivity",
  pch = c(NA,2,NA,NA,NA,NA,NA,NA,NA,NA),
  legend.position = "bottomleft"
) # model 2


plotModels(
  models= mod.out,
  xlab = "Semantic Coherence",
  ylab = "Exclusivity",
  #     c(NA,2,NA,4, NA,6, NA,8, NA,10),
  pch = c(NA,2,NA,NA,5,NA,NA,NA,NA,10),
  legend.position = "bottomleft"
) # model 2 vs 5 vs 10

plotModels(
  models= mod.out,
  xlab = "Semantic Coherence",
  ylab = "Exclusivity",
  #     c(NA,2,NA,4, NA,6, NA,8, NA,10),
  pch = c(NA,2,NA,NA,NA,NA,NA,8,NA,NA),
  legend.position = "bottomleft"
) # model 2 vs 8.

plotModels(
  models= mod.out,
  xlab = "Semantic Coherence",
  ylab = "Exclusivity",
  #     c(NA,2,NA,4, NA,6, NA,8, NA,10),
  pch = c(1,2,NA,NA,NA,NA,NA,NA,NA,NA),
  legend.position = "bottomleft"
) # model 1 vs 2


plotModels(
  models= mod.out,
  xlab = "Semantic Coherence",
  ylab = "Exclusivity",
  #     c(NA,2,NA,4, NA,6, NA,8, NA,10),
  pch = c(1,NA,NA,NA,NA,NA,NA,8,NA,NA),
  legend.position = "bottomleft"
) # model model 1 vs 8



plotModels(
  models= mod.out,
  xlab = "Semantic Coherence",
  ylab = "Exclusivity",
  #     c(NA,2,NA,4, NA,6, NA,8, NA,10),
  pch = c(NA,2,NA,4,NA,NA,NA,NA,NA,NA),
  legend.position = "bottomleft"
) # model model 2 vs 4



plotModels(
  models= mod.out,
  xlab = "Semantic Coherence",
  ylab = "Exclusivity",
  #     c(NA,2,NA,4, NA,6, NA,8, NA,10),
  pch = c(NA,2,3,NA,NA,NA,NA,NA,NA,NA),
  legend.position = "bottomleft"
) # model model 2 vs 3


plotModels(
  models= mod.out,
  xlab = "Semantic Coherence",
  ylab = "Exclusivity",
  #     c(NA,2,NA,4, NA,6, NA,8, NA,10),
  pch = c(NA,2,NA,NA,5,NA,NA,NA,NA,NA),
  legend.position = "bottomleft"
) # model model 2 vs 5


plotModels(
  models= mod.out,
  xlab = "Semantic Coherence",
  ylab = "Exclusivity",
  #     c(NA,2,NA,4, NA,6, NA,8, NA,10),
  pch = c(NA,2,NA,NA,NA,6,NA,NA,NA,NA),
  legend.position = "bottomleft"
) # model model 2 vs 6


plotModels(
  models= mod.out,
  xlab = "Semantic Coherence",
  ylab = "Exclusivity",
  #     c(NA,2,NA,4, NA,6, NA,8, NA,10),
  pch = c(NA,2,NA,NA,NA,NA,7,NA,NA,NA),
  legend.position = "bottomleft"
) # model model 2 vs 7



plotModels(
  models= mod.out,
  xlab = "Semantic Coherence",
  ylab = "Exclusivity",
  #     c(NA,2,NA,4, NA,6, NA,8, NA,10),
  pch = c(NA,2,NA,NA,NA,NA,NA,8,NA,NA),
  legend.position = "bottomleft"
) # model model 2 vs 8


plotModels(
  models= mod.out,
  xlab = "Semantic Coherence",
  ylab = "Exclusivity",
  #     c(NA,2,NA,4, NA,6, NA,8, NA,10),
  pch = c(NA,2,NA,NA,NA,NA,NA,NA,9,NA),
  legend.position = "bottomleft"
) # model model 2 vs 9


plotModels(
  models= mod.out,
  xlab = "Semantic Coherence",
  ylab = "Exclusivity",
  #     c(NA,2,NA,4, NA,6, NA,8, NA,10),
  pch = c(NA,2,NA,NA,NA,NA,NA,NA,NA,10),
  legend.position = "bottomleft"
) # model model 2 vs 10


plotModels(
  models= mod.out,
  xlab = "Semantic Coherence",
  ylab = "Exclusivity",
  #     c(NA,2,NA,4, NA,6, NA,8, NA,10),
  pch = c(NA,2,NA,NA,5, NA,NA,8,9,NA),
  legend.position = "bottomleft"
) # model model 2 vs 5 vs 8 vs 9


plotModels(
  models= mod.out,
  xlab = "Semantic Coherence",
  ylab = "Exclusivity",
  #     c(NA,2,NA,4, NA,6, NA,8, NA,10),
  pch = c(NA,2,NA,NA,5, NA,NA,8,9,NA),
  legend.position = "bottomleft"
) # model model 2 vs 5 vs 8 vs 9


plotModels(
  models= mod.out,
  xlab = "Semantic Coherence",
  ylab = "Exclusivity",
  #     c(NA,2,NA,4, NA,6, NA,8, NA,10),
  pch = c(NA,NA,NA,NA,5, NA,NA,8,NA,NA),
  legend.position = "bottomleft"
) # model model 5 vs 8



# Below code to get subsets of the model.
# I didn't save it in the environment because it took up a lot of space.
model_6_10 <- mod.out # to get subsets of the model.
for(i in 1:5){
  model_6_10$runout[[1]] <- NULL
  model_6_10$semcoh[[1]] <- NULL
  model_6_10$exclusivity[[1]] <- NULL
  model_6_10$sparsity[[1]] <- NULL
  Sys.sleep(1)
  print(i)
}
plotModels(model_6_10, pch = c(6:10), labels = 6:10, legend.position = "bottomleft")

model_1_5 <- mod.out # to get subsets of the model.
for(i in 10:6){
  model_1_5$runout[[i]] <- NULL
  model_1_5$semcoh[[i]] <- NULL
  model_1_5$exclusivity[[i]] <- NULL
  model_1_5$sparsity[[i]] <- NULL
  Sys.sleep(1)
  print(i)
}
plotModels(model_1_5, pch = c(1:5), labels = 1:5, legend.position = "bottomleft")

# As a result, model number 2 seems more appropriate. I chose it.

STM_robot <- mod.out$runout[[2]]

# rm(mod.out)
# save.image("C:/Users/ozbay/OneDrive - XXXX/R_May_2023/STM_Model_May_2023.RData")




