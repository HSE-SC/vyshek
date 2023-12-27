###визуализации к посту https://t.me/HQhse/202

library(openalexR)
library(jsonlite)
library(dplyr)
library(stringr)
library(plotly)

concepts <- oa_request(
  query_url = 'https://api.openalex.org/concepts',
  count_only = FALSE,
  verbose = TRUE
)

df_concepts <- as.data.frame(do.call(rbind,concepts))
df_concepts <- df_concepts[,c(1:7)]
df_concepts <- apply(df_concepts,2,as.character)
write.csv2(df_concepts,'OA concepts dataframe.csv')

search_list <- c('https://openalex.org/C2777688943', 
                 'https://openalex.org/C2778047097', 
                 'https://openalex.org/C2778983918', 
                 'https://openalex.org/C2992299759', 
                 'https://openalex.org/C2778868352', 
                 'https://openalex.org/C57520943', 
                 'https://openalex.org/C2986619947', 
                 'https://openalex.org/C2776639814', 
                 'https://openalex.org/C2994533747', 
                 'https://openalex.org/C157150851', 
                 'https://openalex.org/C2984776528', 
                 'https://openalex.org/C2991854812', 
                 'https://openalex.org/C2777789624', 
                 'https://openalex.org/C2983427547', 
                 'https://openalex.org/C1102183', 
                 'https://openalex.org/C2775982641', 
                 'https://openalex.org/C2777967091', 
                 'https://openalex.org/C3020004979', 
                 'https://openalex.org/C2992628942', 
                 'https://openalex.org/C2776828027', 
                 'https://openalex.org/C161968269', 
                 'https://openalex.org/C2780977596', 
                 'https://openalex.org/C2779563681', 
                 'https://openalex.org/C2778965832', 
                 'https://openalex.org/C2778720315', 
                 'https://openalex.org/C2776762731', 
                 'https://openalex.org/C2993271050', 
                 'https://openalex.org/C193641492', 
                 'https://openalex.org/C2779566919', 
                 'https://openalex.org/C3018047383', 
                 'https://openalex.org/C2779500161', 
                 'https://openalex.org/C2779555700', 
                 'https://openalex.org/C2778462305', 
                 'https://openalex.org/C2780733421', 
                 'https://openalex.org/C2780777687', 
                 'https://openalex.org/C2779080126', 
                 'https://openalex.org/C2778555616', 
                 'https://openalex.org/C2781212760', 
                 'https://openalex.org/C2993152109', 
                 'https://openalex.org/C2781048585', 
                 'https://openalex.org/C2909871498', 
                 'https://openalex.org/C2778789064', 
                 'https://openalex.org/C2776740804', 
                 'https://openalex.org/C2993565178', 
                 'https://openalex.org/C2777748543', 
                 'https://openalex.org/C84848861', 
                 'https://openalex.org/C2910429700', 
                 'https://openalex.org/C523274147', 
                 'https://openalex.org/C2780990159', 
                 'https://openalex.org/C2775977693', 
                 'https://openalex.org/C522210863'
)

#search_words <- c('woman','women','female','feminism','feminist','sexism')

df_c <- list()
total_df_c <- list()

for (i in 1:length(search_list)) {
  df_c <- mapply(c,oa_request(
    query_url = paste0('https://api.openalex.org/works?filter=concepts.id:',search_list[i],'&group_by=publication_year'),
    count_only = FALSE,
    verbose = TRUE
  ),list(search_list[i]))
  total_df_c <- append(total_df_c,df_c)
}

#df_final <- as.data.frame(matrix(total_df,ncol=4,byrow=TRUE))
#colnames(df_final) <- c('key','key_display_name','count','search_word')
#df_final <- df_final[,-2]
#df_final$count <- as.numeric(df_final$count)
#df_final$key <- as.numeric(df_final$key)

#df_final <- reshape(df_final,idvar='key',timevar='search_word',direction='wide')
#df_final <- df_final %>% arrange(key)

#====================CONCEPTS RU==========================

df_final_c <- as.data.frame(matrix(total_df_c,ncol=4,byrow=TRUE))
colnames(df_final_c) <- c('key','key_display_name','count','concept_id')
df_final_c <- df_final_c[,-2]
df_final_c$count <- as.numeric(df_final_c$count)
df_final_c$key <- as.numeric(df_final_c$key)

#df_final_c <- reshape(df_final_c,idvar='concept_id',timevar='key',direction='wide')
#df_final_c <- df_final_c[,order(names(df_final_c))]

#df_final_c <- df_final_c[,c(1,34:45)]
df_final_c <- as_tibble(df_final_c)
women_concepts <- read.csv2('OA women concepts.csv')
df_final_c$concept_id <- as.character(df_final_c$concept_id)
df_final_c <- left_join(df_final_c,women_concepts,by=c('concept_id'='id'))
#df_final_c <- df_final_c[,c(1,15,16,2:13)]
#df_final_c[4:15] <- sapply(df_final_c[4:15],as.numeric)
df_final_c <- df_final_c[1:6]

df_final_c <- df_final_c %>%
  group_by(display_name) %>%
  filter(sum(count)>10) %>%
  ungroup()
df_final_c <- df_final_c %>%
  filter(key>2012) %>% arrange(key)
df_final_c$key <- as.factor(df_final_c$key)

#====================CONCEPTS ALL==========================

df_final_all <- as.data.frame(matrix(total_df_c,ncol=4,byrow=TRUE))
colnames(df_final_all) <- c('key','key_display_name','count','concept_id')
df_final_all <- df_final_all[,-2]
df_final_all$count <- as.numeric(df_final_all$count)
df_final_all$key <- as.numeric(df_final_all$key)

#df_final_all <- reshape(df_final_all,idvar='concept_id',timevar='key',direction='wide')
#df_final_all <- df_final_all[,order(names(df_final_all))]

#df_final_all <- df_final_all[,c(1,34:45)]
df_final_all <- as_tibble(df_final_all)
women_concepts <- read.csv2('OA women concepts.csv')
df_final_all$concept_id <- as.character(df_final_all$concept_id)
df_final_all <- left_join(df_final_all,women_concepts,by=c('concept_id'='id'))
#df_final_all <- df_final_all[,c(1,15,16,2:13)]
#df_final_all[4:15] <- sapply(df_final_all[4:15],as.numeric)
df_final_all <- df_final_all[1:6]

#df_final_all <- df_final_all %>%
#  group_by(display_name) %>%
#  filter(sum(count)>10) %>%
#  ungroup()
df_final_all <- df_final_all %>%
  filter(key %in% (2013:2023)) %>%
  filter(display_name %in% df_final_c$display_name) %>%
      arrange(key)
df_final_all$key <- as.factor(df_final_all$key)

#plots <- lapply(vars, function(var) {
#  plot_ly(economics, x = ~date, y = as.formula(paste0("~", var))) %>%
#    add_lines(name = var)
#})

fig <- plot_ly()
fig <- fig %>% 
  add_trace(data='df_final_c',
            x=df_final_c$key,
            y=df_final_c$count,
            color=df_final_c$display_name,
            type='scatter',
            mode='lines+markers+text',
            textposition='middle left',
            marker=list(symbol='diamond'),
            visible=TRUE,
            showlegend=TRUE) %>%
  add_trace(data='df_final_all',
            x=df_final_all$key,
            y=df_final_all$count,
            color=df_final_all$display_name,
            type='scatter',
            mode='lines+markers+text',
            textposition='middle left',
            marker=list(symbol='diamond'),
            visible=FALSE,
            showlegend=FALSE) %>%
  layout(title = 'Количество статей по различным тематикам (concepts),\nиндексированных БД OpenAlex',
         xaxis = list(title = 'Год'),
         yaxis = list(title = 'Количество статей'),
         legend = list(title = list(text = 'Название тематики')),
         updatemenus = list(
           list(
             active = 0,
             type = "buttons",
             y = 0.8,
             buttons = list(
               
               list(method = "restyle",
                    args = list(list(visible=c('TRUE','FALSE'))),
                    args2 = list(list(visible=c('FALSE','TRUE'))),
                    label = "Россия"),
               
               list(method = "restyle",
                    args = list(list(visible=c('FALSE','TRUE'))),
                    args2 = list(list(visible=c('TRUE','FALSE'))),
                    label = "Мир")))
))
fig

rsconnect::rpubsUpload('Тематики публикаций, связанных с женскими исследованиями',
                       '8march.html',
                       'OA women concepts.csv')
