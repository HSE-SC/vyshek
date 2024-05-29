###визуализации к посту https://t.me/HQhse/249

library(openalexR)
library(dplyr)
library(readxl)
library(plotly)
library(data.table)
library(tidyr)

#getting russian libraries
inst <- oa_request(
  query_url = "https://api.openalex.org/institutions?filter=display_name.search:library,country_code:RU"
)

inst <- as.data.frame(do.call(rbind,inst))
inst_list <- inst[1]
inst_list$id <- str_remove_all(inst_list$id,"https://openalex.org/")

for (i in 1:8){
  dfl <- oa_request(
    query_url = paste0("https://api.openalex.org/works?filter=institutions.id:",inst_list[i,1])
  )
  lib_works <- append(lib_works,dfl)
}

lib_works <- as.data.frame(do.call(rbind,lib_works))


#getting data from mincult

mc <- read_xlsx("Выше квартилей/Библиотеки.xlsx",
                sheet = "Свод по регионам")
mc_reg <- mc[!(mc$Регион %like% "федеральный округ"),]
mc_reg <- mc_reg[-1,]
mc_fo <- mc[mc$Регион %like% "федеральный округ",]

fig <- plot_ly(labels = ~Регион,  legendgroup = ~Регион,
       textposition = 'inside',textinfo = 'label',insidetextorientation='radial') %>%
  add_pie(data = mc_fo, name = "",
          domain = list(row = 0, column = 0), 
          values = ~`Общее число библиотек и библиотек-филиалов`,
          title = "Общее количество библиотек по федеральным округам\n ")%>%
  add_pie(data = mc_reg, name = "", 
          domain = list(row = 0, column = 1), 
          values = ~`Общее число библиотек и библиотек-филиалов`,
          title = "Общее количество библиотек по регионам\n ")%>%
  add_pie(data = mc_fo, name = "", 
          domain = list(row = 1, column = 0), 
          values = ~`Состоит документов на конец отчетного года`*1000,
          title = "Общее количество документов по федеральным округам\n ")%>%
  add_pie(data = mc_reg, name = "", 
          domain = list(row = 1, column = 1), 
          values = ~`Состоит документов на конец отчетного года`*1000,
          title = "Общее количество документов по регионам\n ")%>%
  layout(title = "Библиотеки России. Статистика Министерства культуры, 2022 г.\n ", showlegend = F,
         grid=list(rows=2, columns=2),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig


#getting library science

ls_works <- oa_request(
  query_url = "https://api.openalex.org/works?filter=concepts.id:C161191863,institutions.country_code:RU",
  verbose = TRUE
)

is_works <- oa_request(
  query_url = "https://api.openalex.org/works?filter=concepts.id:C95831776,institutions.country_code:RU",
  verbose = TRUE
)

ls_works <- append(ls_works,is_works)

ls_works <- as.data.frame(do.call(rbind,lapply(ls_works, function(x) x[match(names(ls_works[[1]]), names(x))])))
ls_w <- unnest(ls_works,concepts)
ls_w <- ls_w %>%
  mutate(concepts_id = lapply(ls_w$concepts,"[[","id"),
         concepts_name = lapply(ls_w$concepts,"[[","display_name"),
         concepts_score = lapply(ls_w$concepts,"[[","score")) %>%
  filter(concepts_id == "https://openalex.org/C161191863"|concepts_id == "https://openalex.org/C95831776",
         concepts_score > 0.69,
         publication_year >2000)

is_w <- unnest(ls_works,concepts)
is_w <- is_w %>%
  mutate(concepts_id = lapply(is_w$concepts,"[[","id"),
         concepts_name = lapply(is_w$concepts,"[[","display_name"),
         concepts_score = lapply(is_w$concepts,"[[","score")) %>%
  filter(concepts_id == "https://openalex.org/C95831776",
         concepts_score > 0.5,
         publication_year >2000)

df_lib <- as.data.frame(bind_rows(ls_w,is_w))

df_lib <- unnest(df_lib,authorships)
df_lib <- df_lib %>%
  mutate(author = lapply(df_lib$authorships,"[[","author"))
df_lib <- df_lib %>% 
  mutate(auth_id = lapply(df_lib$author,"[[","id"))

df_lib <- df_lib %>% 
  mutate(institution = lapply(df_lib$authorships,"[[","institutions"))
df_lib <- unnest(df_lib,institution)
df_lib <- df_lib %>%
  mutate(org_id = lapply(df_lib$institution,"[[","id"),
         org_name = lapply(df_lib$institution,"[[","display_name"),
         org_country = lapply(df_lib$institution,"[[","country_code"))

by_inst <- df_lib %>%
  filter(org_country == 'RU') %>%
  group_by(org_name) %>%
  summarize(n=n()) %>%
  arrange(n) %>%
  filter(n>1)
by_inst$org_name <- paste0(by_inst$org_name," ")
by_inst[24,1] <- "State Public Scientific Technological Library of Siberian Branch of RAS"

fig2 <- plot_ly(data = by_inst, x = by_inst$n, y = by_inst$org_name, 
                type = 'bar', orientation = 'h',
                text = by_inst$n) %>%
  layout(yaxis = list(categoryorder = "total ascending"),
         title = "Публикации российских организаций в области библиотечно-информационного дела")
fig2
