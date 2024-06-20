
#> En son STM 05 December isimli R dosyası ile topicleri bulmuştum.
#> topic'lerin kelimelerine bakarak manulel olarak yeni NGramlar buldum.
#> Şimdi bu son bulduğum NGramlar ile son STM'yi yapacağım.
#> Kolaylık sağlasın diye orada kullanıdığım M'i çekiyorum.
#> Salt M olarak kaydedeceğim.
memory.limit()
memory.limit(size = NA)
memory.limit(size=9999999999999)
gc()


load("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/STM_07_December_2022.RData")

#> M hariş tüm environmenti sildimorum.

M$STM_no_stop_text <- NULL # stopwords silinmiş sütunu da sildim.
# Çünkü en yukarıda yazdığım yeni NGramları değiştirirmeden önce stopwordlu metinde işlem yapmam gerekiyor.

# NOT:
M$STM_text # Kelime işlmeleri yapılmış, anan metin içerisinde NGramlar belirlenmiş ve değiştirilmiş metin.
# M$STM_text içindekiğ stop worde'leri temizleyip STM yaptım en son. Bu STM sonunda bulduğum topic kelimeleri ile 
# yeni Ngraölar buldum. Şimdi bu yeni NGramları düzelteceğim.



# DİKKAT: Metinde (.), (,) ve (;) noktalama işaretleri kalmış.
# Önce onları temizleceğim.

# yedek <- M
yedek$publication_year <- NULL # for data size reduction
yedek$TI <- NULL # for data size reduction
yedek$AB <- NULL # for data size reduction



M$STM_text <- gsub(pattern= "\\.", replacement = "OGUZOZBAYNOKTA", M$STM_text) # CAUTION: use "\\." for replaing "."
library(stringr)
M$STM_text <- str_squish(M$STM_text)


sil <- M %>% select (UT, STM_text, control)

# dot replacement data frame:
library(readxl)
dot_replacements <- read_excel("correction_list_for_nGrams_12_december_2022_after_STM.xlsx", 
                               sheet = "dot_replecements", col_types = c("skip", 
                                                                         "text", "text", "skip", "skip", "skip"))

# DİKKAT: NOKTALAMA İÇERMEYEN KELİEM DEĞİŞİKLİĞİ İÇİN AŞAĞIDAKİ KULLANILACAK:
library(stringi)
Old <- dot_replacements$from
New <- dot_replacements$to

M$control <- "" 
system.time({
  M$control <- stri_replace_all_regex(M$STM_tex,
                                                paste0("\\b", Old, "\\b"), 
                                                paste0(" ", New, " "), vectorize_all = FALSE)
})

M$control <- str_squish(M$control)


# Değişiklik kontrolü
colnames(M)
# Now I will check the replacements
M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                      strsplit(M$control, '\\s'), strsplit(M$STM_text, '\\s')) # BU GÖSTERİYOR FARKI

M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                                      strsplit(M$STM_text, '\\s'), strsplit(M$control, '\\s'))


# Now I am deleting all OGUZOZBAYNOKTA
M$control <- gsub(pattern= "OGUZOZBAYNOKTA", replacement = " ", M$control) # I used sama column (ie. M$control)
library(stringr)
M$control <- str_squish(M$control)

# Değişiklik kontrolü
colnames(M)
# Now I will check the replacements
M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$control, '\\s'), strsplit(M$STM_text, '\\s')) # BU GÖSTERİYOR FARKI

M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$STM_text, '\\s'), strsplit(M$control, '\\s'))

# I will check replaced cells
sil_2 <- M %>% select(replace_check_1)
library(tidyverse)
sil_2$replace_check_1[sil_2$replace_check_1 == ""] <- NA
sil_2 <- sil_2 %>% filter_all(any_vars(!is.na(.))) # I checked sil_2 and make a correction list

library(readxl)
library(readxl)
dot_replacement_correction <- read_excel("correction_list_for_nGrams_12_december_2022_after_STM.xlsx", 
                                         sheet = "dot_replecements_correction", 
                                         col_types = c("text", "text", "skip", 
                                                       "skip", "skip"))

# DİKKAT: NOKTALAMA İÇERMEYEN KELİEM DEĞİŞİKLİĞİ İÇİN AŞAĞIDAKİ KULLANILACAK:
library(stringi)
Old_1 <- dot_replacement_correction$from
New_1 <- dot_replacement_correction$to

M$control_1 <- "" 
system.time({
  M$control_1 <- stri_replace_all_regex(M$control,
                                      paste0("\\b", Old_1, "\\b"), 
                                      paste0(" ", New_1, " "), vectorize_all = FALSE)
  M$control_1 <- str_squish(M$control_1)
})

# Değişiklik kontrolü
colnames(M)
# Now I will check the replacements
M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$control_1, '\\s'), strsplit(M$control, '\\s')) # BU GÖSTERİYOR FARKI

M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$control, '\\s'), strsplit(M$control_1, '\\s'))


sil <- M %>% select (UT, STM_text, control, control_1)
sil <- M %>% select (UT, STM_text, control_1)
sil <- M %>% select (control_1)

# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/M_05_Feb_2023_Environment.RData")

# Now I will delete (,) and (;)


M$control_1 <- gsub(pattern= ",", replacement = " ", M$control_1)
library(stringr)
M$control_1 <- str_squish(M$control_1)

M$control_1 <- gsub(pattern= ";", replacement = " ", M$control_1)
library(stringr)
M$control_1 <- str_squish(M$control_1)


M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$control_1, '\\s'), strsplit(M$STM_text, '\\s')) # BU GÖSTERİYOR FARKI

M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$STM_text, '\\s'), strsplit(M$control_1, '\\s'))


M$replace_check_1 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(M$control_1, '\\s'), strsplit(yedek$STM_text, '\\s')) # BU GÖSTERİYOR FARKI

M$replace_check_2 <- mapply(function(x, y) paste0(setdiff(x, y), collapse = " "), 
                            strsplit(yedek$STM_text, '\\s'), strsplit(M$control_1, '\\s'))



M$STM_text_05_02_2023 <- M$control_1
M$control <- NULL
M$replace_check_1 <- NULL
M$replace_check_2 <- NULL
M$control_1 <- NULL

M$key_TI_AB_CORRECTED_d # DİKKAT BUNDA BU KODDAKİ DÜZELTMELER YAPILMAMIŞ DURUMDA!
M$STM_text # STM yaptığım metin. Bu metinde ilk parti Ngramlar değiştirlmişti.
M$STM_text_05_02_2023 # STM yaptığım metinde hataları düzelttim.



# save.image("C:/Users/ozbay/OneDrive - XXXX/robot_WOS/M_05_Feb_2023_Environment.RData")














