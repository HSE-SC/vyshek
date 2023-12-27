###визуализации к посту https://t.me/HQhse/257

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

df <- read.csv2("jif_quartiles.csv")

df_for_sankey <- df %>%
  distinct(year,Source.Title,country,Best.Quartile) %>%
  pivot_wider(names_from = year, values_from = Best.Quartile) %>%
  group_by(`2021`,`2022`) %>%
  summarize(Freq=n()) %>%
  replace(is.na(.),'без квартиля')
df_for_sankey$`2021` <- lapply(df_for_sankey$`2021`,function(x) paste0(x,": "))
df_for_sankey$`2022` <- lapply(df_for_sankey$`2022`,function(x) paste0(x,":  "))


node_names <- unique(c(as.character(df_for_sankey$`2021`), as.character(df_for_sankey$`2022`)))
node_names <- node_names[c(1:8,10,9)]
nodes <- data.frame(
  id = 0:(length(node_names)-1),
  name = node_names
)

##### migrating on networkD3

links <- as.data.frame(list(
  source = match(df_for_sankey$`2021`, node_names) - 1,
  target = match(df_for_sankey$`2022`, node_names) - 1,
  value = df_for_sankey$Freq))

links <- links %>%
  mutate(s_char = df_for_sankey[1],
         t_char = df_for_sankey[2])

nodes <- as.data.frame(list(
  name = node_names,
  group = seq_along(node_names)-1))


my_color <- 'd3.scaleOrdinal() .domain(nodes[2]) .range(["rgb(164, 207, 99)", "rgb(232, 213, 89)", "rgb(251, 163,83)", "rgb(221,90,78)", "palevioletred",
              "rgb(164, 207, 99)", "rgb(232, 213, 89)", "rgb(251, 163,83)", "rgb(221,90,78)", "palevioletred"])'

#===========histo for JIFs

jif <- read.csv2("JIF.csv")
jif22 <- jif %>% filter(year=="2022")
#jif22 <- jif22 %>% mutate(Collection = ifelse(Collection == "", "Н/Д", Collection))
jif22 <- jif22 %>% filter(Collection != "")

#===========go Shiny

ui <- dashboardPage(title = "JCR 2023: изменения в квартилях",
                    ############# Header
                    dashboardHeader(title = 'JCR 2023'),
                    ############# Sidebar
                    dashboardSidebar(disable = TRUE),
                    ############# Dashboard body
                    dashboardBody(
                      # Boxes need to be put in a row (or column)
                      fluidRow(
                        column(width = 6, 
                               box(width = NULL, collapsible = F,
                                   title = 'Изменения в квартилях журналов, индексирующихся в Web of Science',
                                   sankeyNetworkOutput("sankey"),
                                   status = "primary"),
                               box(width = NULL, collapsible = F,
                                   p("График создан на основе данных ", tags$a(href="http://jcr.clarivate.com", "Clarivate Journal Citation Reports")),
                                   p("Расчеты выполнены Наукометрическим центром ВШЭ."))),
                        column(width = 6,
                               box(width = NULL, collapsible = F,
                                   title = 'Распределение импакт-фактора в зависимости от коллекции',
                                   plotlyOutput("boxplot"),
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
  output$sankey <- renderSankeyNetwork({
    sankeyNetwork(Links = links, Nodes = nodes,
                  Source = "source", Target = "target",
                  Value = "value", NodeID = "name",
                  fontSize= 12, nodeWidth = 30,
                  iterations = 0, colourScale=my_color)
  })
  
  output$boxplot <- renderPlotly({
    plot_ly(jif22,y = ~jif, color = ~Collection, type = "box") %>% 
      layout(yaxis = list(type = "log", title = "log 10 (JIF)"))
  })
}

shinyApp(ui, server)
