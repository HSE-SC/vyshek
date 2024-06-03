###визуализации к посту https://t.me/HQhse/254

library(dplyr)
library(data.table)
library(stringr)
library(plotly)
library(tidyr)
library(shiny)

dt <- fread("NPI_total.csv")
areas <- read.csv2("NPI_fields.csv")

dt_distinct_1 <- dt %>%
  filter(List == "NPI1") %>%
  select(-1,-4)
dtd_long_1 <- gather(dt_distinct_1, area, value, 3:86)

dt_distinct_2 <- dt %>%
  filter(List == "NPI2") %>%
  select(-1,-4)
dtd_long_2 <- gather(dt_distinct_2, area, value, 3:86)

dtd_long <- left_join(dtd_long_1, dtd_long_2, 
                      by = c("University", "University_abbr", "area"))
colnames(dtd_long) <- c("University","University_abbr","area","NPI1","NPI2")

dtd_long <- bind_cols(dtd_long,dtd_long$NPI1+dtd_long$NPI2*3)
colnames(dtd_long) <- c("University","University_abbr","area","NPI1","NPI2","NPI_combine")
dtd_long <- left_join(dtd_long,areas,by=c("area"="narrow.area"))
dtd_long <- dtd_long %>%
  dplyr::group_by(wide.area) %>%
  mutate(NPI1_wide=sum(NPI1),
         NPI2_wide=sum(NPI2),
         NPI_combine_wide=sum(NPI_combine)) %>%
  ungroup()
#npi all unis & 
dtd_long <- dtd_long %>%
  dplyr::group_by(area) %>%
  mutate(NPI1_area=sum(NPI1),
         NPI2_area=sum(NPI2),
         NPI_combine_area=sum(NPI_combine)) %>%
  ungroup()

#-Yale,-Tsinghua
dtd_long <- dtd_long %>%
  filter(!(University %in% c("Yale University","Tsinghua University")))

#=========HERE WE'RE GOING TO SHINE=========

ui <- fluidPage(
  
  # Application title
  titlePanel("Публикации российских университетов в журналах,\n
             представленных в 1 и 2 уровнях норвежского списка NPI"),
  titlePanel(tags$a("Norwegian Publication Indicator", href="https://npi.hkdir.no/")),
  
  # Remove scrollbar with custom CSS
  
  tags$head(
    tags$style(HTML('.shiny-split-layout>div {overflow: hidden;}')),
  ),
  
  # Sidebar with a slider input for number of bins
#  splitLayout(
#    cellWidths = c("25%", "75%"),
  fluidPage(
    fluidRow(
      sidebarPanel(
        width = 3,
        selectInput(
          inputId = "university",
          label = "Университет:",
          choices = sort(c(unique(dtd_long$University))),
          size = 10,selectize = FALSE,
          selected = "Высшая школа экономики"
        ),
        radioButtons(
          inputId = "wide.area",
          label = "Укрупненное направление:",
          choices = c(unique(dtd_long$wide.area)),
          #        size = 10,selectize = FALSE,
          #        selected = c(unique(dtd_long$wide.area))
        )
      ),
      div(id='Hide',
          column(
            width = 9,
            plotlyOutput("bar")
          )
      )
    ),
    fluidRow(
      sidebarPanel(
        width = 3,
        selectInput(
          inputId = "area",
          label = "Предметная область:",
          choices = sort(c(unique(dtd_long$area))),
          size = 10,selectize = FALSE,
          selected = "Anaesthesia, Emergency and Intensive Care"
        )
        # actionButton("remove", "Remove data table(s)!"),br(),br(),br(),
      ),
      div(id='Hide',
          column(
            width = 9,
            plotlyOutput("barx")
          )
      ),
      hr(),
      print("Расчеты и графики выполнены Наукометрическим центром НИУ ВШЭ")
        )
      )
    )

server <- function(input, output, session) {
  
  df <- reactive({
    filter(dtd_long,
           University==input$university,
           wide.area==input$wide.area
    )
    #    unique(dtd_long[dtd_long$University==input$university,])
  })
  
  output$bar <- renderPlotly({
    
    plot_ly(df(), type = "bar") %>% 
      add_trace(x = ~area,
                y = ~NPI1, 
                name = "NPI1",
                marker = list(color = 'steelblue')) %>% 
      add_trace(x = ~area,
                y = ~NPI2*3,
                hovertext = ~NPI2,
                name = "NPI2",
                hovertemplate = "%{x}, %{y}\nКоличество статей: %{hovertext}",
                marker = list(color = 'lightsteelblue')) %>% 
      layout(xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Количество статей"), 
             barmode = "stack") %>% 
      add_annotations(data = df(),
                      x = ~area,
                      y = ~NPI_combine + NPI_combine/10 + 10,
                      text = ~NPI_combine,
                      showarrow = FALSE,
                      font = list(color = '#264E86',
                                  family = 'arial',
                                  size = 14))
  })
  
  
  dn <- reactive({
    filter(dtd_long,
           area==input$area)
    #    unique(dtd_long[dtd_long$University==input$university,])
  })
  
  output$barx <- renderPlotly({
    
    plot_ly(dn(), type = "bar") %>% 
      add_trace(x = ~University_abbr,
                y = ~NPI1, 
                name = "NPI1",
                marker = list(color = 'steelblue')) %>% 
      add_trace(x = ~University_abbr,
                y = ~NPI2*3,
                hovertext = ~NPI2,
                name = "NPI2",
                hovertemplate = "%{x}, %{y}\nКоличество статей: %{hovertext}",
                marker = list(color = 'lightsteelblue')) %>% 
      layout(xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Количество статей"), 
             barmode = "stack") %>% 
      add_annotations(data = dn(),
                      x = ~University_abbr,
                      y = ~NPI_combine + NPI_combine/10 + 10,
                      text = ~NPI_combine,
                      showarrow = FALSE,
                      font = list(color = '#264E86',
                                  family = 'arial',
                                  size = 14))
  })
  
  
}

shinyApp(ui, server)
