###визуализации к посту https://t.me/HQhse/264

library(openalexR)
library(dplyr)
library(plotly)

esp <- oa_request("https://api.openalex.org/works?filter=display_name.search:esperanto",
                  count_only = FALSE,
                  verbose = TRUE)

esp_df <- as.data.frame(do.call(rbind,esp))
esp_year <- unlist(esp_df$publication_year,recursive = FALSE)
esp_year <- substr(esp_year, 1, 4)

esp_filtered <- esp_df %>%
  mutate(type = case_when(type == 'journal-article' ~ 'article',
                          type == 'article' ~ 'article',
                          type == 'book-chapter' ~ 'book',
                          type == 'book' ~ 'book')) %>%
  mutate(publication_year = esp_year) %>%
  filter(type %in% c('book','article')) %>%
  group_by(type,publication_year) %>%
  summarize(n=n()) %>%
  pivot_wider(names_from = type,
              values_from = n)


fig <- plot_ly(data = esp_filtered,
               type = 'bar',
               x = ~publication_year,
               y = ~article,
               name = 'Статьи',
               marker = list(color='mediumseagreen'))
fig <- fig %>% add_trace(y = ~book, 
                         name = 'Книги', 
                         marker = list(color = 'goldenrod'))
fig <- fig %>% layout(xaxis = list(title = 'Год'),
                      yaxis = list(title = 'Количество публикаций'),
                      barmode = 'stack',
                      title = '\nКоличество книг и статей об эсперанто, индексируемых в OpenAlex')
fig
