library(dplyr)
library(tibble)
library(data.table)
library(tidyr)
library(magrittr)
library(stringr)
library(stringi)
library(rvest) 
library(readxl)
library(jsonlite)
library(reshape2)
library(plotly)
library(ggplot2)
library(forcats)

#df <- read_html("https://elibrary.ru/contents.asp?titleid=7648")
#df_t <- df %>% html_elements(".collapsed") %>% html_children()
#df_t <- html_attr(html_nodes(df,"a"), "href")
#df <- df[[34]][["X2"]]
#  html_elements("tbody") %>%
#  html_children(tbody) %>%
#  as.data.frame()


#dt <- read_html("https://elibrary.ru/contents.asp?titleid=7648")
#dt_issues <- tibble(
#  text = html_elements(dt,"a") %>% html_text(),
#  link = html_elements(dt,"a") %>% html_attr("href")
#)
#dt_issues <- dt_issues %>% filter(str_detect(link,"contents.asp")) %>%
#  mutate(text = str_replace_all(text,"№",'')) %>%
#  mutate(text = as.numeric(str_trim(text)))
#View(dt_issues %>% filter(str_detect(text,"№ 1"))) %>% mutate(index = row_number(text)))

journals <- read_xlsx("RAS_journals_2024_28_10_2024.xlsx")
journals <- journals %>% mutate(elibrary_id = str_remove_all(elibrary_id,'https.*='))
journals <- journals %>% mutate(elibrary_id = paste0('https://elibrary.ru/contents.asp?titleid=',elibrary_id))

journals_data <- list()
scraplinks <- function(url){
  # Create an html document from the url
  webpage <- xml2::read_html(url)
  # Extract the URLs
  url_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  # Extract the link text
  link_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  return(tibble(link = link_, url = url_))
}

for (i in 141:141) {
  url = as.character(journals[i,4])
  
  #getting list of issues with links
  scrap_is <- scraplinks(url)
  scrap_is <- scrap_is %>% filter(str_detect(url,"contents"))
  scrap_is <- scrap_is %>% filter(str_detect(link,"№"))
  
  #getting issue number and year
  for (j in 1:nrow(scrap_is)){
    scrap_year <- read_html(paste0("https://www.elibrary.ru/",scrap_is[j,2])) %>%
      html_text() 
    scrap_is[j,3] <- str_extract(scrap_year, "Номер:.*")
    scrap_is[j,4] <- str_extract(scrap_year, "Год:.*")
    colnames(scrap_is) <- c('link','url','num','year')
    #print(scrap_is[j,])
    if(str_detect(scrap_is[j,4],'2019') == TRUE) break else next
  }
  
  #trim 2020-2024
  scrap_is <- scrap_is %>% filter(str_detect(year,'2019')==FALSE & !is.na(year))
  
  #getting list of articles with links
  for (k in 1:nrow(scrap_is)) {
    scrap_ar <- scraplinks(paste0("https://www.elibrary.ru/",scrap_is[k,2]))
    scrap_ar <- scrap_ar %>% filter(str_detect(url,"item.asp"))
    scrap_is[k,5] <- read_html(paste0("https://www.elibrary.ru/",scrap_ar[1,2])) %>% 
      html_text() %>% str_extract("Дата размещения:.*")
    Sys.sleep(sample(3:9, 1))
  }
  journals_data[[i]] <- scrap_is
  print(paste0(journals[i,1],' обработан, ждем 2 минуты...'))
  Sys.sleep(120)
}

for (l in 1:nrow(journals)) {
  names(journals_data)[l] <- as.character(journals[l,1])
}

dt <- melt(journals_data)
#View(dt %>% filter(is.na(...5)==TRUE))
dt <- dt %>% mutate(num = str_remove(num,'Номер[:punct:][:blank:]'))
dt <- dt %>% mutate(num = case_when(
  str_detect(num,'\\(') == TRUE ~ substring(str_extract(num,'\\(.*\\)'),2, nchar(str_extract(num,'\\(.*\\)'))-1),
  .default = num))
dt[3283:3293,3] <- c(1:11)
dt <- dt %>% mutate(year = str_remove(year,'Год[:punct:][:blank:]'))
dt <- dt %>% mutate(...5 = str_remove(...5,'Дата размещения[:punct:]'))
#dt <- read.csv2("elib_raw_works.csv")

dt <- dtt
dt$num <- as.numeric(dt$num)
dt[4048:4085,3] <- c(1:6,1:7,1:7,1:7,3,1:7,7,1,6)
dt[4175:4185,3] <- c(1:11)
dt[553:579,3] <- c(1:3,1:6,1:6,1:6,1:6)
dt[2587,3] <- c(9)
dt <- dt[-387,]
dt[447,3] <- c(5)
dt[942,3] <- c(7)
dt[1747,3] <- c(2)
dt[1824,3] <- c(11)
dt[1834,3] <- c(10)
dt[2358,3] <- c(2)
dt[2361,3] <- c(3)
dt[2366,3] <- c(5)
dt[3037,3] <- c(7)
dt[3930:3931,3] <- c(4,6)
dt[4132,3] <- c(8)
dt[4670,3] <- c(2)
  dt <- dt %>% mutate(num=if_else(num>90, as.numeric(url), num, missing = as.numeric(url)))
dt <- dt %>% group_by(L1) %>% mutate(issues = max(num)) %>% ungroup()
dt$...5 <- as.Date(dt$...5,"%d.%m.%Y")
dt <- dt %>% mutate(date = yday(...5))
dt <- dt %>% mutate(month = format(...5,"%m-%d"))
dt <- dt %>% mutate(month = factor(month,levels=c("янв","фев","мар","апр","май","июн",
                                                     "июл","авг","сен","окт","ноя","дек")))
dt <- dt %>% mutate(volume = str_match(link,"Т\\.\\s*(.*?)\\s*№") %>% { .[,2] })

# basic plot --------------------------------------------------------------

fig <- plot_ly(data = dt %>% filter(issues == '1'|issues == '2'),
               type = 'histogram',
               color = ~as.factor(year),
               colors = 'viridis',
               x = ~month,
               visible = T)
fig <- fig %>% add_histogram(data = dt %>% filter(issues == '4'|issues == '5'),
                             color = ~as.factor(year),
                             colors = 'viridis',
                             x = ~month,
                             visible = F)
fig <- fig %>% add_histogram(data = dt %>% filter(issues == '6'|issues == '7'),
                             color = ~as.factor(year),
                             colors = 'viridis',
                             x = ~month,
                             visible = F)
fig <- fig %>% add_histogram(data = dt %>% filter(issues == '8'),
                             color = ~as.factor(year),
                             colors = 'viridis',
                             x = ~month,
                             visible = F)
fig <- fig %>% add_histogram(data = dt %>% filter(issues == '12'),
                             color = ~as.factor(year),
                             colors = 'viridis',
                             x = ~month,
                             visible = F)
fig <- fig %>% layout(title = list(text = 'Распределение публикации выпусков журналов РАН на сайте eLibrary\nв 2020-2024 гг.'),
                      xaxis = list(title = 'Месяц'),
                      margin = list(l = 50, r = 50,
                                    b = 50, t = 60,
                                    pad = 20),
                      showlegend = TRUE,
                      updatemenus = list(
                        list(
                          y = 0.8,
                          buttons = list(
                            list(method = "restyle",
                                 args = list("visible", c(rep(T,5),rep(F,5),rep(F,5),rep(F,5),rep(F,5))),
                                 label = "1-2 выпуска"),
                            list(method = "restyle",
                                 args = list("visible", c(rep(F,5),rep(T,5),rep(F,5),rep(F,5),rep(F,5))),
                                 label = "4-5 выпусков"),
                            list(method = "restyle",
                                 args = list("visible", c(rep(F,5),rep(F,5),rep(T,5),rep(F,5),rep(F,5))),
                                 label = "6-7 выпусков"),
                            list(method = "restyle",
                                 args = list("visible", c(rep(F,5),rep(F,5),rep(F,5),rep(T,5),rep(F,5))),
                                 label = "8 выпусков"),
                            list(method = "restyle",
                                 args = list("visible", c(rep(F,5),rep(F,5),rep(F,5),rep(F,5),rep(T,5))),
                                 label = "12 выпусков"))
                          )
                        )
)
fig

# basic plot mean -----------------------------------------------------------

dt <- left_join(dt,journals[,c(1,5)],by=c("L1"="publication_title"))
dt <- dt %>% filter(issues<per_year) %>% group_by(L1,year) %>% 
  mutate(num=row_number()) %>% 
  rbind(dt %>% filter(issues>=per_year))

dt_means <- dt %>% group_by(per_year,num) %>% 
  summarize(mean_date = round(mean(yday(...6))))
dt_means <- dt_means %>% mutate(mean_date = as.Date(mean_date))

fig1 <- plot_ly(dt_means,
                x = ~mean_date,
                y = ~num,
                color = ~as.factor(per_year),
                type = 'bar')
fig1 <- fig1 %>% layout(xaxis = list(tickformat = "%d-%m"))
fig1


# ----------------------------
# dt_wide <- dt %>% filter(year==2020) %>% 
#   group_by(num,per_year) %>% summarize(min_date=min(as.Date(...6)),
#                                      max_date=max(as.Date(...6)))
# dt_wide <- dt_wide %>% mutate(deadline = case_when(
#   per_year == 1 ~ '2020-12-31',
#   per_year == 4 & num == 1 ~ '2020-03-31',
#   per_year == 4 & num == 2 ~ '2020-06-30',
#   per_year == 4 & num == 3 ~ '2020-09-30',
#   per_year == 4 & num == 4 ~ '2020-12-31',
#   per_year == 6 & num == 1 ~ '2020-02-28',
#   per_year == 6 & num == 2 ~ '2020-04-30',
#   per_year == 6 & num == 3 ~ '2020-06-30',
#   per_year == 6 & num == 4 ~ '2020-08-31',
#   per_year == 6 & num == 5 ~ '2020-10-31',
#   per_year == 6 & num == 6 ~ '2020-12-31',
#   per_year == 12 & num == 1 ~ '2020-01-31',
#   per_year == 12 & num == 2 ~ '2020-02-28',
#   per_year == 12 & num == 3 ~ '2020-03-31',
#   per_year == 12 & num == 4 ~ '2020-04-30',
#   per_year == 12 & num == 5 ~ '2020-05-31',
#   per_year == 12 & num == 6 ~ '2020-06-30',
#   per_year == 12 & num == 7 ~ '2020-07-31',
#   per_year == 12 & num == 8 ~ '2020-08-31',
#   per_year == 12 & num == 9 ~ '2020-09-30',
#   per_year == 12 & num == 10 ~ '2020-10-31',
#   per_year == 12 & num == 11 ~ '2020-11-30',
#   per_year == 12 & num == 12 ~ '2020-12-31'
# ))

dt_benchmark <- dt %>% group_by(year,num,per_year) %>% 
  summarize(min_date=min(as.Date(...6)),
            max_date=max(as.Date(...6)))
dt_benchmark <- dt_benchmark %>% mutate(deadline = case_when(
  per_year == 1 ~ paste0(year,'-12-31'),
  per_year == 4 & num == 1 ~ paste0(year,'-03-31'),
  per_year == 4 & num == 2 ~ paste0(year,'-06-30'),
  per_year == 4 & num == 3 ~ paste0(year,'-09-30'),
  per_year == 4 & num == 4 ~ paste0(year,'-12-31'),
  per_year == 6 & num == 1 ~ paste0(year,'-02-28'),
  per_year == 6 & num == 2 ~ paste0(year,'-04-30'),
  per_year == 6 & num == 3 ~ paste0(year,'-06-30'),
  per_year == 6 & num == 4 ~ paste0(year,'-08-31'),
  per_year == 6 & num == 5 ~ paste0(year,'-10-31'),
  per_year == 6 & num == 6 ~ paste0(year,'-12-31'),
  per_year == 12 & num == 1 ~ paste0(year,'-01-31'),
  per_year == 12 & num == 2 ~ paste0(year,'-02-28'),
  per_year == 12 & num == 3 ~ paste0(year,'-03-31'),
  per_year == 12 & num == 4 ~ paste0(year,'-04-30'),
  per_year == 12 & num == 5 ~ paste0(year,'-05-31'),
  per_year == 12 & num == 6 ~ paste0(year,'-06-30'),
  per_year == 12 & num == 7 ~ paste0(year,'-07-31'),
  per_year == 12 & num == 8 ~ paste0(year,'-08-31'),
  per_year == 12 & num == 9 ~ paste0(year,'-09-30'),
  per_year == 12 & num == 10 ~ paste0(year,'-10-31'),
  per_year == 12 & num == 11 ~ paste0(year,'-11-30'),
  per_year == 12 & num == 12 ~ paste0(year,'-12-31')
))
#dt_benchmark <- left_join(dt_benchmark,dt_wide,
#                          by = c("num","per_year"),
#                          suffix = c(".total",".2020"))

df_tidy <- dt_benchmark %>% rename_with(~c('year','issue','per_year','start','end','deadline')) %>% 
  mutate(issue = as.factor(issue), per_year = as.factor(per_year)) %>%
  mutate(start = as.Date(start), end = as.Date(end), deadline = as.Date(deadline)) %>%
  gather(key=date_type, value=date, -year, -issue, -per_year, -deadline)

deadlines <- df_tidy %>% as_tibble(rownames = ~per_year)

p <- ggplot(df_tidy) +
  geom_line(mapping=aes(x=fct_rev(fct_inorder(issue)), y=date, color=issue), size=10) +
  facet_wrap(~ year, scales = "free_y") +
  #geom_segment(x=as.numeric(df_tidy$per_year)-.45, y=df_tidy$deadline,
  #             xend=as.numeric(df_tidy$per_year)+.45, yend=df_tidy$deadline, colour="black", linetype="dashed") +
  geom_hline(data=deadlines, aes(yintercept=deadline, colour="black", linetype="dashed")) +
  scale_y_date(date_breaks = "1 month", date_labels =  "%d %b") +
  #  ylim(c(as.Date('1970-01-01',"%d %b"),as.Date('1970-12-31',"%d %b"))) +
  coord_flip() 

ggplotly(p) %>%
  add_trace(split = ~year,
            legendgroup = "year") %>%
  layout(title = list(text = 'Распределение публикации выпусков журналов РАН на сайте eLibrary\nв 2020-2024 гг.'),
         xaxis = list(title = 'Месяц'),
         margin = list(l = 50, r = 50,
                       b = 50, t = 60,
                       pad = 20),
         showlegend = TRUE
  )


#ggplot(dt_wide, aes(x = Date, y = Value, fill = Location, group = interaction(Date, Location))) +
#  geom_boxplot() +
#  scale_x_date(date_labels = "%b %Y")


#RSCI FETCH HERE
#preparing table of urls
datas <- data.frame()
datas <- rbind(datas,c('10-06-2024','16-06-2024'))
datas[1,1] <- as.Date(datas[1,1],format="%d-%m-%Y")
datas[1,2] <- as.Date(datas[1,2],format="%d-%m-%Y")
datas <- datas %>% mutate_all(.,as.numeric) %>% mutate_all(.,as.Date)
for (i in 1:31) {
  datas <- rbind(datas,c(datas[i,1]+7,datas[i,2]+7))
}
datas <- datas %>% mutate(short = paste0("https://rcsi.science/activity/zhurnalnaya-platforma/publikatsii-s-",format(X.10.06.2024.,'%d-%m'),"-po-",format(X.16.06.2024.,'%d-%m')),
                          long = paste0("https://rcsi.science/activity/zhurnalnaya-platforma/publikatsii-s-",format(X.10.06.2024.,'%d-%m-%y'),"-po-",format(X.16.06.2024.,'%d-%m-%y')),
                          mean_date = X.10.06.2024. + 3)

#scraping the data
rsci_raw <- data.frame()
for (i in 1:nrow(datas)) {
  rsci_temp <- scraplinks(datas[i,3])
  rsci_temp <- rsci_temp %>% filter(str_detect(link,"Публикации") | str_detect(link,"202?"))
  rsci_temp <- cbind(rsci_temp$link[1],datas[i,5],rsci_temp[-1,])
  rsci_raw <- rbind(rsci_raw,rsci_temp)
  print(i)
}

#regexing as hell
rsci_raw <- rsci_raw %>% mutate(year = str_extract(link,"[:digit:]{4}"),
                                volume = str_trim(str_remove_all(str_extract(link,"Т.*,|Т.*$"),"[:punct:]|[:alpha:]")),
                                issue = str_extract(str_extract(link,"№.*"),"[:digit:]"),
                                issn = str_remove(url,'https://journals.rcsi.science/')) %>%
  mutate(issn = str_remove(issn,'/issue/.*'))

rsci_raw <- rsci_raw %>% mutate(rsci_link = str_extract(url,'https://journals.rcsi.science/.*/'))
rsci_raw <- rsci_raw %>% mutate(rsci_link = str_remove(rsci_link,"/issue/view/"))

# repairing issn ----------------------------------------------------------
rsci_raw <- rsci_raw %>% mutate(issn=case_when(
  issn=="vszgmu" ~ '2618-7116',
  issn=="kazanmedj" ~ '0368-4814',
  issn=="turner" ~ '2309-3994',
  issn=="pediatr" ~ '2079-7850',
  issn=="PharmForm" ~ '2713-153X',
  issn=="ecolgenet" ~ '1811-0932',
  issn=="cardar" ~ '2782-4284',
  issn=="PMJ" ~ '0136-1449',
  issn=="uroved" ~ '2225-9074',
  issn=="vramn" ~ '0869-6047',
  issn=="clinpractice" ~ '2220-3095',
  issn=="RCF" ~ '1683-4100',
  issn=="jowd" ~ '1684-0461',
  issn=="transj" ~ '2782-3733',
  issn=="MAJ" ~ '1608-4101',
  issn=="pavlovj" ~ '0204-3475',
  issn=="RFD" ~ '2072-1668',
  issn=="DD" ~ '2712-8490',
  issn=="ov" ~ '1998-7102',
  issn=="RMMArep" ~ '2713-2315',
  issn=="raj" ~ '2686-682X',
  issn=="EDGCC" ~ '2218-4422',
  .default=issn
))

# ----------------------------------------------------------

rsci_raw <- left_join(rsci_raw,journals[,c(1,13)],by=c("rsci_link"="title_url"))
rsci_raw <- left_join(rsci_raw,dt[,c(7,11)],by=c("publication_title"="L1"))
rsci_raw <- unique(rsci_raw)
rsci_plot <- rsci_raw %>% filter(!is.na(publication_title))
rsci_plot$issue <- as.numeric(rsci_plot$issue)
rsci_plot$year <- as.numeric(rsci_plot$year)


elib_rsci <- full_join(dt,rsci_plot,by=c("L1"="publication_title",
                                         "num"="issue",
                                         "volume"="volume",
                                         "year"="year"))

fig <- plot_ly(elib_rsci, color = I("gray80"))
fig <- fig %>% add_segments(x = ~...6, xend = ~`datas[i, 5]`, y = ~num, yend = ~num, showlegend = FALSE)
fig <- fig %>% add_markers(x = ~...6, y = ~num, name = "elib", color = I("pink"))
fig <- fig %>% add_markers(x = ~`datas[i, 5]`, y = ~num, name = "rcsi", color = I("blue"))
fig




rsci_plot %>% rownames_to_column('rn') %>% 
  filter(!str_detect(issn,'[:digit:]-[:digit:]')) %>% 
  select(rn,issn,rsci_link) %>% 
  distinct(issn,rsci_link)
dt <- dt %>% filter(issues<per_year) %>% group_by(L1,year) %>% 
  mutate(num=row_number()) %>% 
  rbind(dt %>% filter(issues>=per_year))








fig <- plot_ly(data = rsci_raw %>% group_by(`datas[i, 5]`,issue,issues) %>% summarize(n=n()),
               x = ~`datas[i, 5]`,
               y = ~issues,
               color = ~issue,
               type = 'bar')
fig
  

p <- ggplot(rsci_raw %>% filter(is.na(issues)==FALSE)) +
  geom_line(mapping=aes(x=fct_rev(fct_inorder(issue)), y=`datas[i, 5]`, color=issue), size=10) +
  #geom_hline(yintercept=df_tidy$deadline, colour="black", linetype="dashed") +
  facet_wrap(~ issues, scales = "free_y") +
  #geom_segment(x=as.numeric(df_tidy$per_year)-.45, y=df_tidy$deadline,
  #             xend=as.numeric(df_tidy$per_year)+.45, yend=df_tidy$deadline, colour="black", linetype="dashed") +
  scale_y_date(date_breaks = "1 month", date_labels =  "%d %b") +
  #  ylim(c(as.Date('1970-01-01',"%d %b"),as.Date('1970-12-31',"%d %b"))) +
  coord_flip() 



