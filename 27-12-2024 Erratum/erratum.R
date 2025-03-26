###визуализации к посту https://t.me/HQhse/479

library(dplyr)
library(data.table)
library(tidyr)
library(purrr)
library(stringr)
library(openalexR)
library(plotly)
library(jsonlite)

dt <- oa2df(oa_request("https://api.openalex.org/works?filter=type:erratum,from_publication_date:2019-01-01,to_publication_date:2023-12-31"),entity='works')

sources_df <- sources2df(oa_request("https://api.openalex.org/sources?select=id,country_code,topics"))
#sources_df <- toJSON(sources_df)
#write_json(sources_df,"OpenAlex_sources_24_12_2024.json")

dt_full <- left_join(dt,sources_df,by=c("so_id"="id"))
dt_view <- dt_full %>% mutate(topic_fin = map(topics.y, possibly( ~ filter(., name == "field"),otherwise=NULL))) 
dt_view <- dt_view %>% mutate(topic_fin = map(topic_fin, possibly( ~ select(., -c('i','count')),otherwise=NULL))) 
dt_view <- dt_view %>% mutate(topic_fin = map(topic_fin, possibly( ~ distinct(.),otherwise=NULL))) 
dt <- dt_view %>% unnest_longer(topic_fin,keep_empty = TRUE)
dt_jour_pub <- dt %>% group_by(so,host_organization,country_code,topic_fin) %>% 
  summarize(n=n())

write.csv2(dt_jour_pub,"data_for_erratum.csv")

publishers <- oa_request("https://api.openalex.org/works?filter=from_publication_date:2019-01-01,to_publication_date:2023-12-31&group_by=primary_location.source.host_organization")
publishers <- bind_rows(publishers)

topics <- oa_request("https://api.openalex.org/works?filter=from_publication_date:2019-01-01,to_publication_date:2023-12-31&group_by=topics.field.id")
topics <- bind_rows(topics)

dt_jour_pub <- left_join(dt_jour_pub,publishers,by=c('host_organization'='key_display_name'))

fig <- plot_ly(data = dt_jour_pub %>% distinct_at(vars(-topic_fin) group_by(host_organization,count) %>% 
                 summarize(n = sum(n)) %>% filter(n > 1000) %>% filter(is.na(host_organization)==FALSE) 
               %>% filter(host_organization!='Elsevier BV')
                 ,
               x = ~count,
               y = ~n,
               #color = ~country_code,
               text = ~host_organization,
               type = 'scatter',
               mode = 'markers',
               marker = list(size = ~n/count*300, opacity = 0.5))
fig <- fig %>% layout(title = list(text = 'Соотношение erratum и общего числа публикаций за 2019-2023 гг.\nв журналах различных издателей'),
                      xaxis = list(title = 'Общее количество публикаций'),
                      yaxis = list(title = 'Количество erratum'),
                      margin = list(l = 50, r = 50,
                                    b = 50, t = 60,
                                    pad = 20))
fig

dt_jour_topics <- dt %>% group_by(so,host_organization,country_code,topic_fin) %>% 
  summarize(n=n())
dt_jour_topics <- dt_jour_topics %>% unnest_wider(topic_fin)
dt_jour_topics <- left_join(dt_jour_topics,topics,by=c('display_name'='key_display_name'))
dt_jour_topics <- dt_jour_topics %>% filter(count!=0)

fig <- plot_ly(data = dt_jour_topics[,6:9] %>% distinct() %>% group_by(display_name,count) %>% 
                                                    summarize(n = sum(n)) #%>% filter(n > 1000) %>% filter(is.na(host_organization)==FALSE) 
                                                  #%>% filter(host_organization!='Elsevier BV')
                                                  ,
                                                  x = ~count,
                                                  y = ~n,
                                                  #color = ~country_code,
                                                  text = ~display_name,
                                                  textposition = 'center',
                                                  type = 'scatter',
                                                  mode = 'markers+text',
                                                  marker = list(size = ~n/count*5000, opacity = 0.5))
fig <- fig %>% layout(title = list(text = 'Соотношение erratum и общего числа публикаций за 2019-2023 гг.\nв различных областях науки'),
                                     xaxis = list(title = 'Общее количество публикаций'),
                                     yaxis = list(title = 'Количество erratum'),
                                     margin = list(l = 50, r = 50,
                                                   b = 50, t = 60,
                                                   pad = 20))
fig
