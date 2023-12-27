###визуализации к посту https://t.me/HQhse/200

library(openalexR)
library(dplyr)
library(stringr)
library(jsonlite)
library(tidyr)
library(plotly)
library(RColorBrewer)
library(viridisLite)
library(data.table)
library(shiny)
library(shinydashboard)
library(DT)

count_by_concept_date <- oa_request(
  query_url = 'https://api.openalex.org/works?filter=from_publication_date:2018-01-01&group_by=concept.id',
  count_only = TRUE,
  verbose = TRUE
)

concepts <- as.data.frame(do.call(rbind,count_by_concept_date))
concepts$key <- as.character(concepts$key)
concepts$key <- str_replace_all(concepts$key,"C","c")

concept_list <- read.csv2("OpenAlex concepts in use (17 August 2022).csv")

concepts <- left_join(concepts,concept_list,by = c("key" = "openalex_id"))
concepts$parent_display_names <- str_remove_all(concepts$parent_display_names,",.*")
concepts <- concepts %>% filter(if_any(everything(), ~ grepl('ngineering',.)))
concepts$count <- as.numeric(concepts$count)

fig1 <- plot_ly()
fig1 <- fig1 %>% add_trace(
  type='sunburst',
  #  ids=concepts$count,
  labels=concepts$display_name,
  parents=concepts$parent_display_names,
  values=concepts$count,
  domain=list(column=0),
  maxdepth=2,
  insidetextorientation='radial'
)

#==================================================

count_by_concept_date_RU <- oa_request(
  query_url = 'https://api.openalex.org/works?filter=authorships.institutions.country_code:RU,from_publication_date:2018-01-01&group_by=concept.id',
  count_only = TRUE,
  verbose = TRUE
)

concepts_RU <- as.data.frame(do.call(rbind,count_by_concept_date_RU))
concepts_RU$key <- as.character(concepts_RU$key)
concepts_RU$key <- str_replace_all(concepts_RU$key,"C","c")

concepts_RU <- left_join(concepts_RU,concept_list,by = c("key" = "openalex_id"))
concepts_RU$parent_display_names <- str_remove_all(concepts_RU$parent_display_names,",.*")
concepts_RU <- concepts_RU %>% filter(if_any(everything(), ~ grepl('ngineering',.)))
concepts_RU <- concepts_RU[-14,]
concepts_RU$count <- as.numeric(concepts_RU$count)
concepts_RU <- inner_join(concepts_RU,concepts,by='display_name')
concepts_RU <- concepts_RU[1:9]

fig2 <- plot_ly()
fig2 <- fig2 %>% add_trace(
  type='sunburst',
  #  ids=concepts_RU$count,
  labels=concepts_RU$display_name,
  parents=concepts_RU$parent_display_names,
  values=concepts_RU$count,
  domain=list(column=1),
  maxdepth=2,
  insidetextorientation='radial'
)

#fig <- subplot(fig1, fig2, ncols=2) %>% 
#  layout(title = 'Side By Side Subplots')
#fig

#========================================================

inst_all <- read.csv2("https://raw.githubusercontent.com/Malbvrg/wed2023/main/oa_inst_part1.csv")
inst_all <- inst_all %>% filter(count>500,country_code=='RU',type=='education') %>%
  arrange(desc(count))
#inst_all$lat <- as.numeric(inst_all$lat)
#inst_all$lon <- as.numeric(inst_all$lon)
#inst_all <- left_join(inst_all,countries,by=c('country_code'='key'))
inst_all <- inst_all[,c(3,9,8)]
colnames(inst_all) <- c('Университет','Город','Число публикаций в области инженерных наук')

hist <- plot_ly()
hist <- hist %>% add_trace(
  type='bar',
  orientation='h',
  x=inst_all$`Число публикаций в области инженерных наук`,
  y=reorder(inst_all$Университет,inst_all$`Число публикаций в области инженерных наук`),
  text=inst_all$`Число публикаций в области инженерных наук`
)
hist


#========================================================

ui <- dashboardPage(title = "World Engineering Day 2023",
  ############# Header
  dashboardHeader(title = 'WED 2023'),
  ############# Sidebar
  dashboardSidebar(disable = TRUE),
#    sidebarMenu(
#      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
#    )
#  ),
  
  ############# Dashboard body
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(title='Количество публикаций в области инженерных наук в мире', 
          plotlyOutput("fig"), width=6, height=500),
      box(title='Количество публикаций в области инженерных наук в России',
          plotlyOutput("figru"), width=6, height=500)
    ),
    fluidRow(
      box(title='Количество публикаций российских университетов в области инженерных наук',
          dataTableOutput("inst_all"), width=6, height=500),
      box(title='Количество публикаций российских университетов в области инженерных наук',
          plotlyOutput("hist"), width=6, height=500)
    )
  )
)


#######
####### Server
#######

server <- function(input, output, session) {
  
  output$fig <- renderPlotly({
    
    fig1 <- plot_ly()
    fig1 <- fig1 %>% add_trace(
      type='sunburst',
      #  ids=concepts$count,
      labels=concepts$display_name,
      parents=concepts$parent_display_names,
      values=concepts$count,
      domain=list(column=0),
      maxdepth=2,
      insidetextorientation='radial'
    )
  })
  
  output$figru <- renderPlotly({
    fig2 <- plot_ly()
    fig2 <- fig2 %>% add_trace(
      type='sunburst',
      #  ids=concepts_RU$count,
      labels=concepts_RU$display_name,
      parents=concepts_RU$parent_display_names,
      values=concepts_RU$count,
      domain=list(column=1),
      maxdepth=2,
      insidetextorientation='radial'
    )
  })

  click_name <- reactiveVal()
  
    output$inst_all <- DT::renderDataTable({
    DT::datatable( 
      if(is.null(click_name())) 
        inst_all    # Not filtered
      else 
        inst_all %>% filter( Город==click_name())
    )
  })
    
    output$hist <- renderPlotly({
      hist <- plot_ly()
      hist <- hist %>% add_trace(
        type='bar',
        orientation='h',
        x=inst_all$`Число публикаций в области инженерных наук`,
        y=reorder(inst_all$Университет,inst_all$`Число публикаций в области инженерных наук`),
        text=inst_all$`Число публикаций в области инженерных наук`
      )
    })
}


shinyApp(ui, server)
