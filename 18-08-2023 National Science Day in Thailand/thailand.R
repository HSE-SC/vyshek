###визуализация к посту https://t.me/HQhse/265

library(openalexR)
library(dplyr)
library(plotly)
library(tidyr)
library(data.table)

thai <- read.csv2("Выше квартилей/Thai.csv")

fig <- plot_ly(height=800)
fig <- fig %>% add_pie(data = thai,
                       labels = ~Research.Area,
                       values = ~Documents.all,
                       textinfo = 'value+percent',
                       hovertemplate = paste('%{label}',
                                             '\n%{value}',
                                             '\n%{percent}',
                                             '<extra></extra>'),
                       domain = list(row = 0, column = 0),
                       hole = 0.5,
                       marker = list(colors = c("#440154","#414487","#2a788e",
                                                "#22a884","#7ad151","#fde725"),
                                     line = list(color = '#FFFFFF', width = 1)),
                       insidetextorientation='horizontal',
                       title = paste0("Всего:\n",sum(thai$Documents.all)))
fig <- fig %>% add_pie(data = thai,
                       labels = ~Research.Area,
                       values = ~Documents.global,
                       textinfo = 'value+percent',
                       hovertemplate = paste('%{label}',
                                             '\n%{value}',
                                             '\n%{percent}',
                                             '<extra></extra>'),
                       domain = list(row = 0, column = 1),
                       hole = 0.5,
                       marker = list(colors = c("#440154","#414487","#2a788e",
                                                "#22a884","#7ad151","#fde725"),
                                     line = list(color = '#FFFFFF', width = 1)),
                       insidetextorientation='horizontal',
                       title = paste0("В коллаборации с Россией:\n",sum(thai$Documents.global)))
fig <- fig %>% layout(title = "Количество публикаций с аффилиацией таиландских организаций\n2018-2022 гг.",
                      margin = list(t = 100, pad = 50),
                      showlegend = T,
                      legend = list(orientation="h"),
                      grid = list(rows=1, columns=2),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig
