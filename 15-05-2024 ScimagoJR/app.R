#SJR quartiles update - annual 

library(dplyr)
library(data.table)
library(readxl)
library(stringr)
library(tidyr)
library(plotly)
library(shiny)
library(shinydashboard)
library(DT)
library(networkD3)
library(htmlwidgets)

sjr23 <- read.csv2("scimagojr 2023.csv")
sjr22 <- read.csv2("scimagojr 2022.csv")
sjr21 <- read.csv2("scimagojr 2021.csv")

sjr23_ru <- sjr23 %>% filter(Country == "Russian Federation")
sjr22_ru <- sjr22 %>% filter(Country == "Russian Federation")
sjr21_ru <- sjr21 %>% filter(Country == "Russian Federation")

df_for_sankey <- full_join(sjr21_ru,sjr22_ru,by='Sourceid')
df_for_sankey <- full_join(df_for_sankey,sjr23_ru,by='Sourceid')
df_for_sankey <- df_for_sankey[,c(2,43,7,27,47)]
df_for_sankey <- df_for_sankey %>%
  group_by(SJR.Best.Quartile.x,SJR.Best.Quartile.y,SJR.Best.Quartile) %>%
  summarize(Freq=n())
df_for_sankey <- df_for_sankey %>%
  replace(is.na(.),'NA') %>%
  mutate(SJR.Best.Quartile.x = recode(SJR.Best.Quartile.x, 'NA' = 'не индексируется', '-' = 'без квартиля'),
         SJR.Best.Quartile.y = recode(SJR.Best.Quartile.y, 'NA' = 'не индексируется', '-' = 'без квартиля')) %>%
  mutate(SJR.Best.Quartile = case_when(SJR.Best.Quartile.y == 'не индексируется' & SJR.Best.Quartile == 'NA' ~ 'не индексируется',
                                       SJR.Best.Quartile.y != 'не индексируется' & SJR.Best.Quartile == 'NA' ~ 'индексация прекращена',
                                       SJR.Best.Quartile == '-' ~ 'без квартиля',
                                       .default = as.character(SJR.Best.Quartile))) %>%
  mutate(SJR.Best.Quartile.x = factor(SJR.Best.Quartile.x, levels = c("Q1","Q2","Q3","Q4","без квартиля","не индексируется")),
         SJR.Best.Quartile.y = factor(SJR.Best.Quartile.y, levels = c("Q1","Q2","Q3","Q4","без квартиля","не индексируется")),
         SJR.Best.Quartile = factor(SJR.Best.Quartile, levels = c("Q1","Q2","Q3","Q4","без квартиля","индексация прекращена","не индексируется")))
df_for_sankey$SJR.Best.Quartile.y <- lapply(df_for_sankey$SJR.Best.Quartile.y,function(x) paste0(x," "))
df_for_sankey$SJR.Best.Quartile <- lapply(df_for_sankey$SJR.Best.Quartile,function(x) paste0(x,"  "))
df_for_sankey <- df_for_sankey %>%
  arrange(SJR.Best.Quartile.x,SJR.Best.Quartile.y,SJR.Best.Quartile)

node_names <- unique(c(as.character(df_for_sankey$SJR.Best.Quartile.x), as.character(df_for_sankey$SJR.Best.Quartile.y), as.character(df_for_sankey$SJR.Best.Quartile)))
#node_names <- node_names[c(2:5,1,6,8:11,7,12)]
nodes <- data.frame(
  id = 0:(length(node_names)-1),
  name = node_names
)

##### migrating on networkD3

links <- as.data.frame(list(
  source = append(match(df_for_sankey$SJR.Best.Quartile.x, node_names) - 1, match(df_for_sankey$SJR.Best.Quartile.y, node_names) - 1),
  target = append(match(df_for_sankey$SJR.Best.Quartile.y, node_names) - 1, match(df_for_sankey$SJR.Best.Quartile, node_names) - 1),
  value = df_for_sankey$Freq))
  
links <- links %>%
  mutate(s_char = rbindlist(list(df_for_sankey[1], df_for_sankey[2])),
         t_char = rbindlist(list(df_for_sankey[2], df_for_sankey[3])))

nodes <- as.data.frame(list(
  name = node_names,
  group = seq_along(node_names)-1))

sjr_dt <- full_join(sjr21_ru,sjr22_ru,by='Sourceid')
sjr_dt <- full_join(sjr_dt,sjr23_ru,by='Sourceid')
#sjr_dt <- sjr_dt[,c(2,7,27,43,45,47,58,61)]
sjr_dt <- sjr_dt[,c(2,3,5,7,18,21,23,25,27,38,41,43,45,47,58,61)]
sjr_dt <- sjr_dt %>%
  mutate(
    Name = case_when(
      Title.x != 0 ~ Title.x,
      is.na(Title.x)&is.na(Title.y) ~ Title,
      is.na(Title.x) ~ Title.y),
    ISSN = case_when(
      Issn.x > 0 ~ Issn.x,
      is.na(Issn.x)&is.na(Issn.y) ~ Issn,
      is.na(Issn.x) ~ Issn.y),
    Publisher = case_when(
      Publisher.x > 0 ~ Publisher.x,
      is.na(Publisher.x)&is.na(Publisher.y) ~ Publisher,
      is.na(Publisher.x) ~ Publisher.y),
    Areas = case_when(
      Areas.x > 0 ~ Areas.x,
      is.na(Areas.x)&is.na(Areas.y) ~ Areas,
      is.na(Areas.x) ~ Areas.y)
    )
sjr_dt <- sjr_dt[,c(1,17:18,15,4,9,14,16)]

colnames(sjr_dt) <- c('ID','Name','ISSN','Publisher','Best Q 2021','Best Q 2022','Best Q 2023','Areas')
sjr_dt <- sjr_dt %>%
replace(is.na(.),'NA') %>%
  mutate(`Best Q 2021` = recode(`Best Q 2021`, 'NA' = 'не индексируется', '-' = 'без квартиля'),
         `Best Q 2022` = recode(`Best Q 2022`, 'NA' = 'не индексируется', '-' = 'без квартиля')) %>%
  mutate(`Best Q 2023` = case_when(`Best Q 2022` == 'не индексируется' & `Best Q 2023` == 'NA' ~ 'не индексируется',
                                       `Best Q 2022` != 'не индексируется' & `Best Q 2023` == 'NA' ~ 'индексация прекращена',
                                       `Best Q 2023` == '-' ~ 'без квартиля',
                                       .default = as.character(`Best Q 2023`))) %>%
  mutate(Publisher = gsub('&amp;', '&', Publisher))
sjr_dt$ID <- as.character(sjr_dt$ID)

my_color <- 'd3.scaleOrdinal() .domain(nodes[2]) .range(["rgb(164, 207, 99)", "rgb(232, 213, 89)", "rgb(251, 163,83)", "rgb(221,90,78)", "palevioletred", "slategray",
              "rgb(164, 207, 99)", "rgb(232, 213, 89)", "rgb(251, 163,83)", "rgb(221,90,78)", "palevioletred", "slategray", "black"])'

ui <- dashboardPage(title = "Изменения в квартилях российских журналов",
                    ############# Header
                    dashboardHeader(title = 'SJR 2021-2023'),
                    ############# Sidebar
                    dashboardSidebar(disable = TRUE),
                    ############# Dashboard body
                    dashboardBody(
                      # Boxes need to be put in a row (or column)
                      fluidRow(
                        column(width = 5, 
                          box(width = NULL, collapsible = F,
                              title = 'Изменения в квартилях российских журналов',
                              sankeyNetworkOutput("plot"),
                              status = "primary"),
                          box(width = NULL, collapsible = F,
                              p("График создан на основе данных ", tags$a(href="https://www.scimagojr.com/journalrank.php", "Scimago Journal & Country Rank")),
                              p("Расчеты выполнены Наукометрическим центром ВШЭ."))),
                        column(width = 7,
                          box(width = NULL, collapsible = F,
                              dataTableOutput("view"),
                              status = "primary",
                              #headerPanel("Список журналов"),
                              #solidHeader = T,
                              br()
                              #DT::dataTableOutput("table"),
                          ))
                      )
                    )
)

server <- function(input, output, session) {
  output$plot <- renderSankeyNetwork({
    sankeyNetwork(Links = links, Nodes = nodes,
                Source = "source", Target = "target",
                Value = "value", NodeID = "name",
                fontSize= 12, nodeWidth = 30,
                iterations = 1, colourScale=my_color)
    
#    clickFun <- 
#      'function() { 
#      d3.selectAll(".node").on("mousedown.drag", null);
#      d3.selectAll(".node").on("click",function(d) { alert(d.name); })
#   }'
#    onRender(san, clickFun)
  })
  
  output$view <- DT::renderDataTable({
#    get.data(input$obs)
    DT::datatable(sjr_dt,filter='top')
  })
}

shinyApp(ui, server)
