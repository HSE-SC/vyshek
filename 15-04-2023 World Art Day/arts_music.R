###визуализации к посту https://t.me/HQhse/230

library(dplyr)
library(data.table)
library(openalexR)
library(stringr)
library(wordcloud2)

###здесь список ID концептов, по которым будем брать статьи.
##Полный список во вложении, в столбце N.

music_list <- c('https://openalex.org/C112813540',
                'https://openalex.org/C147120446',
                'https://openalex.org/C114611597',
                'https://openalex.org/C123079801',
                'https://openalex.org/C530479602')

df_total <- list()
for (i in 1:length(music_list)) {
  df <- oa_request(
    query_url = paste0('https://api.openalex.org/works?filter=from_publication_date:2018-01-01,concepts.id:',music_list[i],'&select=concepts,abstract_inverted_index'),
    count_only = FALSE,
    verbose = TRUE
  )
  df_total <- append(df_total,df)
  print(i)
}

pushkin_df <- as.data.frame(do.call(rbind,df))

music_df <- read.csv("Выше квартилей/Art day/music.csv")
music_df <- music_df %>%
  filter(Abstract != "") %>%
  select(OpenAlex.ID,OpenAlex.Concepts,Abstract)
mus_char <- as.character(music_df$Abstract)

#library(RWeka)
#bi_tokens <- NGramTokenizer(mus_char, Weka_control(min=2,max=2))
library(quanteda)
library(stopwords)

punct <- str_split_1("[\\.,:;=+-−_•×!?\"\'’“«»&${}^<>/%\\()]",pattern="")
stops <- c('article','chapter','study','paper','paragraph','studies','data',
           'на','и','в','а','не','статья','acid','en','el','dans','le',
           'interview','abstract','array','span','radiation','electron',
           'x-ray','solar','particle','chlorophyll','photosynthetic','energy',
           'partum','postpartum','considers','differential','diabetic',
           'submissions','expert','administered','rate','reviewers','mass')
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

#figPath = system.file("/Users/Malburg/Pictures/violin.png",package = "wordcloud2")
wc1 <- wordcloud2(freq, size = 1, widgetsize = c(800,800), color = '#42271c',#shape = "star")
                  figPath = "/Users/Евгений Гайбарян/Pictures/Violin.jpg")
wc2 <- wordcloud2(freq, size = 1, widgetsize = c(800,800), color = '#b35a48',#shape = "star")
                  figPath = "/Users/Евгений Гайбарян/Pictures/61-612417_open-book-icon-symbol-vector-silhouette-of-open.jpg") 
wc3 <- wordcloud2(freq, size = 1, widgetsize = c(800,800), color = '#7c8f7c',#shape = "star")
                  figPath = "/Users/Евгений Гайбарян/Pictures/3d810031b5edcc4a434d7fce9bb76f44_t.jpeg") 
wc4 <- wordcloud2(freq, size = 1, widgetsize = c(800,800), color = '#ba944d',#shape = "star")
                  figPath = "/Users/Евгений Гайбарян/Pictures/palette.jpg") 
wc5 <- wordcloud2(freq, size = 1, widgetsize = c(800,800), color = '#323f38',#shape = "star")
                  figPath = "/Users/Евгений Гайбарян/Pictures/png-clipart-computer-icons-symbol-column-ancient-greek-temple-graphics-symbol-miscellaneous-building.jpg") 
wc6 <- wordcloud2(freq, size = 1, widgetsize = c(800,800), color = '#36341b',#shape = "star")
                  figPath = "/Users/Евгений Гайбарян/Pictures/istockphoto-1317753965-612x612.jpg") 
wc7 <- wordcloud2(freq, size = 1, widgetsize = c(800,800), color = '#e3bd72',#shape = "star")
                  figPath = "/Users/Евгений Гайбарян/Pictures/17075.jpg") 
wc8 <- wordcloud2(freq, size = 1, widgetsize = c(800,800), color = '#92996f',#shape = "star")
                  figPath = "/Users/Евгений Гайбарян/Pictures/kisspng-flamenco-dancer-silhouette-spanish-dancer-5b373b541c31b4.5627014315303463241155.jpg")
