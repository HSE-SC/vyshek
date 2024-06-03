

library(dplyr)
library(data.table)
library(openalexR)
library(stringr)
library(wordcloud2)
library(tidyr)

df <- oa_request(
    query_url = paste0('https://api.openalex.org/works?filter=display_name.search:pushkin&select=concepts,abstract_inverted_index'),
    count_only = TRUE,
    verbose = TRUE
)

#we get data tables from Python script here https://colab.research.google.com/drive/1G700vM4lq1c51k-ZddsZwHIc2FluVijY#scrollTo=Bqy7UMcPrMs4
pushkin_df <- read.csv("Выше квартилей/pushkin_df.csv")
pushkin_df <- pushkin_df %>%
  filter(Abstract != "") %>%
  select(OpenAlex.ID,OpenAlex.Concepts,Abstract)
mus_char <- as.character(pushkin_df$Abstract)

#library(RWeka)
#bi_tokens <- NGramTokenizer(mus_char, Weka_control(min=2,max=2))
library(quanteda)
library(stopwords)

punct <- str_split_1("[\\.,…:;=+-−–_•©×!?\"\'’“°”«»&${}^<>/%\\()]",pattern="")
stops <- c('article','chapter','study','paper','paragraph','studies','data',
           'на','и','в','а','не','о','статья','acid','en','el','dans','le',
           'interview','abstract','array','span','radiation','electron',
           'x-ray','solar','particle','chlorophyll','photosynthetic','energy',
           'partum','postpartum','considers','differential','diabetic',
           'submissions','expert','administered','rate','reviewers','mass',
           'permissionexport','citationadd','citationadd','membrane','google',
           'more.copy','synaptic','online','carbonic','amino','acids',
           'bicarbonate','boron','toolbar','issue','search','share')
tokens <- tokens(mus_char)
tokens <- tokens_select(tokens,c(stopwords(source = "stopwords-iso"),punct,stops),selection = 'remove', padding = TRUE)

bi_tokens <- tokens_ngrams(tokens,n=2L)

bi_tokens_unique <- dfm(bi_tokens)
bi_tokens_unique <- dfm_trim(bi_tokens_unique,min_termfreq = 10,termfreq_type = "count")
freq <- as.data.frame(featfreq(bi_tokens_unique))
freq$token = row.names(freq)
freq <- freq[,c(2,1)]
rownames(freq) <- NULL
freq <- top_n(freq,1000)
freq$token <- str_replace_all(freq$token,"_"," ")

write.csv2(freq,'frequency.csv')
freq <- read.csv2("Выше квартилей/pushkin_ru.csv")
freq <- freq %>% filter(SUM.of.featfreq < 600)
freq[39,1] <- "Медный всадник"
freq <- freq %>% filter(!str_detect(russian_token,'век'))
freq <- freq %>% filter(!str_detect(russian_token,'еврей'))
freq <- freq %>% filter(!str_detect(russian_token,'афроамерик'))
freq$SUM.of.featfreq <- log(freq$SUM.of.featfreq,1.001)

#figPath == path to image (contour black on white)
wc2 <- wordcloud2(freq, size = 0.2, widgetsize = c(1000,1000), color = 'black',#shape = "star")
                  figPath = "/Users/Евгений Гайбарян/Pictures/pushkin88.jpg")
wc2
