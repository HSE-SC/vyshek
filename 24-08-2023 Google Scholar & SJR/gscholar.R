library(dplyr)
library(plotly)

dt <- read.csv2("scholar.csv")
#dt <- dt %>% replace(is.na(.), 5)

fig <- plot_ly(data = dt,
               x = ~SJR,
               y = ~h5.index,
               text = ~Publication,
               type = 'scatter',
               mode = 'markers',
               marker = list(size = 10),
               color = ~as.factor(РЦНИ.уровень),
               colors = 'Spectral')
fig <- fig %>% layout(title = list(text = '<b>Соотношение h-index (Google Scholar) и SJR (Scimago JR)</b>',font = list(size = 22), pad = list(t = 100, b = 100)),
                      margin = list(t = 50, pad = 50),
                      legend = list(title = list(text = '<b>Уровень РЦНИ</b>')))
fig
