###визуализации к посту https://t.me/HQhse/245

library(openalexR)
library(jsonlite)
library(dplyr)
library(plyr)
library(data.table)
library(stringr)
library(tidyr)
library(plotly)

keywords <- c("museum","gallery","park")

df <- data.frame()
museums <- data.frame()

for (i in 1:length(keywords)) {
df <- oa_request(
  query_url = paste0("https://api.openalex.org/institutions?filter=display_name.search:",keywords[i],",country_code:RU")
)
museums <- append(museums,df)
}

mus <- as.data.frame(do.call(rbind,museums))
mus <- mus[-14,]
mus[15,4:ncol(mus)] <- c(NA,mus[15,4:(ncol(mus)-1)])

#====================================================

orgs <- mus[,c(1,3)]
orgs$id <- str_remove_all(orgs$id,"https://openalex.org/")

works <- data.frame()

for (i in 1:nrow(orgs)) {
  df <- oa_request(
    query_url = paste0("https://api.openalex.org/works?filter=institutions.id:",orgs[i,1])
  )
  works <- append(works,df)
  print(i)
}

#If different number of columns USE THIS CHARM
works_df <- as.data.frame(do.call(rbind,lapply(works, function(x) x[match(names(works[[1]]), names(x))])))

conc <- unnest(works_df,concepts)
conc <- conc %>%
  mutate(concepts_id = lapply(conc$concepts,"[[","id"),
         concepts_name = lapply(conc$concepts,"[[","display_name"),
         concepts_score = lapply(conc$concepts,"[[","score")) %>%
  filter(concepts_score > 0.5)

concept_count <- conc %>%
  dplyr::group_by(concepts_name) %>%
  dplyr::summarize(n = n())

author <- unnest(works_df,authorships)
author <- author %>%
  mutate(author_info = lapply(author$authorships,"[[","author"),
         inst_info = lapply(author$authorships,"[[","institutions"))
author <- unnest(author,inst_info)

author <- author %>%
  mutate(author_id = lapply(author$author_info,"[[","id"),
         author_name = lapply(author$author_info,"[[","display_name"),
         institution_id = lapply(author$inst_info,"[[","id"),
         institution_name = lapply(author$inst_info,"[[","display_name"),
         institution_country = lapply(author$inst_info,"[[","country_code")) %>%
  filter(institution_country == "RU") %>%
  filter(institution_id %in% mus$id) %>%
  distinct(id,institution_id,.keep_all = TRUE) 

joined <- left_join(conc,author[,c(1,35:38)],by="id",multiple="all")
joined <- joined %>%
  group_by(concepts_name,institution_name) %>%
  dplyr::summarize(n = n())
joined$concepts_name <- as.character(joined$concepts_name)
joined$institution_name <- as.character(joined$institution_name)

df <- read.csv2("joined1.csv")

df <- joined %>%
  filter(n>5)
df <- as.data.frame(df)
df$institution_name <- as.character(df$institution_name)
mus$display_name <- as.character(mus$display_name)

total_sums <- df %>%
  group_by(institution_name) %>% 
  summarise(total = sum(n)) %>%
  mutate(parent = "")
total_sums <- total_sums[,c(1,3,2)]
df$institution_name <- as.character(df$institution_name)
colnames(total_sums) <- colnames(df)
df <- bind_rows(total_sums,df)

#================================

fig <- plot_ly(
  type='sunburst',
  labels=df %>% filter(institution_name == mus[1,3]) %>% select(concepts_name),
  parents=df %>% filter(institution_name == mus[1,3]) %>% select(institution_name),
  values=df %>% filter(institution_name == mus[1,3]) %>% select(n),
  branchvalues = 'total',
  domain=list(column=1),
  maxdepth=2,
  insidetextorientation='radial'
)
fig
