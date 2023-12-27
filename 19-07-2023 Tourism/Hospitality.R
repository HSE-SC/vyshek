###визуализации к посту https://t.me/HQhse/263

library(openalexR)
library(dplyr)
library(stringr)
library(jsonlite)
library(tidyr)
library(plotly)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(RColorBrewer)
library(viridisLite)
library(data.table)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(sf)
library(rworldmap)

codes <- read.csv("ISO-3166-Countries-with-Regional-Codes_master_all_all.csv")

#worldmap
count_by <- oa_request(
  query_url = 'https://api.openalex.org/works?filter=from_publication_date:2018-01-01,concept.id:https://openalex.org/c18918823&group_by=authorships.institutions.country_code',
  count_only = TRUE,
  verbose = TRUE
)

works_all <- oa_request(
  query_url = 'https://api.openalex.org/works?filter=from_publication_date:2018-01-01,concept.id:https://openalex.org/c18918823&select=id,authorships',
  count_only = FALSE,
  verbose = TRUE
)
works_dt <- lapply(works_all, unlist, use.names=TRUE)
works_df <- as.data.frame(do.call(rbind,works_dt))
colnames(works_df) <- paste0(colnames(works_df),"_",seq_along(works_df))
inst_all <- works_df %>% 
  mutate_all(~ replace(., !str_detect(., "https://openalex.org/I"), NA_character_) %>% 
                                      {.[order(.)]})
inst <- unlist(inst_all)
inst_all <- na.omit(inst) %>% unique()
inst_all <- as.data.frame(inst_all)
write.csv2(inst_all,"inst_all.csv")

for (i in 1:nrow(inst_all)) {
  works_by_inst <- oa_request(
    query_url = paste0('https://api.openalex.org/works?filter=authorships.institutions.id:',inst_all[i,1],',from_publication_date:2018-01-01,concept.id:https://openalex.org/c18918823'),
    count_only = TRUE,
    verbose = FALSE
  )
  inst_all[i,2] <- works_by_inst$count
  print(i)
}


countries_all <- oa_request(
  query_url = 'https://api.openalex.org/works?filter=from_publication_date:2018-01-01&group_by=authorships.institutions.country_code',
  count_only = TRUE,
  verbose = TRUE
)

countries_total <- as.data.frame(do.call(rbind,countries_all))
countries_total$key <- as.character(countries_total$key)

countries <- as.data.frame(do.call(rbind,count_by))
countries$key <- as.character(countries$key)
countries <- left_join(countries,codes,by = c("key" = "alpha.2"))
countries <- left_join(countries,countries_total,by = "key")
countries$count.x <- as.numeric(countries$count.x)
countries$count.y <- str_replace_all(countries$count.y,"NULL","0")
countries$count.y <- as.numeric(countries$count.y)
countries <- countries %>%
  mutate(tourism_share = count.x/count.y*100)

inst_hosp <- read.csv2('inst_hospitality.csv')
inst_all <- read.csv2("inst_all.csv")
inst_hospitality <- left_join(inst_hosp,inst_all,by=c("inst_all"="id"))
inst_hosp <- inst_hospitality %>% filter(V2>50,type=='education')
inst_hosp$lat <- as.numeric(inst_hosp$lat)
inst_hosp$lon <- as.numeric(inst_hosp$lon)

inst_hosp <- left_join(inst_hosp,countries,by=c('country_code'='key'))

ca_counties <- ne_countries(scale = "medium", returnclass = "sf")
ca_states <- ne_states(returnclass = "sf")
ca_states <- ca_states[,c(7,9,13,37,38,42,43,84)]

russia <- ca_counties[[64]][[184]]
crimea <- ca_states[[8]][[1693]]
sevastopol <- ca_states[[8]][[2270]]
ukraine <- ca_states %>% filter(geonunit == 'Ukraine')
ukraine <- ukraine$geometry
unn <- st_union(ukraine)[[1]]
#inst_all[3979,15] <- 'Russian Federation'
#inst_all[4016,15] <- 'Russian Federation'

ca_counties[[64]][[225]] <- NA
ca_counties[[64]][[225]] <- unn

ca_counties[[64]][[184]] <- append(ca_counties[[64]][[184]],crimea)
ca_counties[[64]][[184]] <- append(ca_counties[[64]][[184]],sevastopol)

parcels <- countries

parcels <- st_as_sf(left_join(parcels, ca_counties[,c('iso_a2')], by = c("key"="iso_a2" )))
parcels <- parcels %>% filter(name!='NA') %>% filter(count.y>count.x)

parcels_df <- inst_hosp
parcels_df <- parcels_df %>% 
  select(display_name,name,city,V2,lat,lon) %>% arrange(desc(V2))
colnames(parcels_df) <- c('Название','Страна','Город',
                          'Количество работ в области туризма','lat','lon')

fig <- parcels_df 
fig <- fig %>%
  plot_ly(
    lat = ~lat,
    lon = ~lon,
    marker = list(color = "rgb(230,30,60)"),
    type = 'scattermapbox',
    size = ~`Количество работ в области туризма`,
    text = paste0(parcels_df$Название,"\n",parcels_df$Страна,"\nКоличество публикаций: ",parcels_df$`Количество работ в области туризма`),
    hoverinfo = 'text')
fig <- fig %>%
  layout(
    title = "Количество публикаций в области туризма и гостеприимства, 2018-2023 гг.",
    mapbox = list(
    style = 'white-bg',
#      zoom =2.5,
#      center = list(lon = -88, lat = 34)
    layers = list(list(
      below = 'traces',
      sourcetype = "raster",
      opacity = 0.75,
      source = list("https://basemap.nationalmap.gov/arcgis/rest/services/USGSImageryOnly/MapServer/tile/{z}/{y}/{x}")
)))) 

fig


#====================================================================================================

ui <- dashboardPage(
#  skin = 'green',
  dashboardHeader(),
  dashboardSidebar(
    disable = TRUE
    ),
  dashboardBody(
            fluidRow(
              box(
                width = 12, collapsible = T,
                title = 'Количество публикаций в области туризма и гостеприимства, 2018-2023 гг.',
                solidHeader = T, status = 'primary',
                leafletOutput('parcels_map')
              )
            ),
            fluidRow(
              box(
                width = 12, collapsible = T,
                title = 'Данные по стране',
                solidHeader = T, status = 'primary',
                dataTableOutput('parcels_table')
              )
            )
  )
)

#====================================================================================================

server <- function(input, output, session) {
  #===== Инженерные науки =====#
  # Map of Census block groups
  output$parcels_map <- renderLeaflet({
    bins <- c(1, 100, 500, 1000, 5000, 10000, 16000)
    #    bins <- quantile(parcels$count.x)
    pal <- colorBin("Blues", domain = parcels$count.x, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>
      Публикаций: %g<br/>",
      parcels$name, parcels$count.x
    ) %>% lapply(htmltools::HTML)
    
    leaflet(parcels) %>%
      #      setView(-119, 37.9, 6) %>%
      addTiles() %>%
      addPolygons(
        layerId = ~name,
        fillColor = ~pal(count.x),
        weight = 2,
        opacity = 1,
        color = 'black',
        dashArray = '2',
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(color = "darkred", weight = 2,
                                            bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "4px 8px"),
          textsize = "15px",
          direction = 'auto')) %>%
      addLegend(pal = pal, values = ~count.x, opacity = 0.7, title = "Количество публикаций",
                position = "bottomleft")
  })
  
  click_name <- reactiveVal()
  
  observeEvent(input$parcels_map_shape_click, {
    # Capture the info of the clicked polygon
    if(!is.null(click_name()) && click_name() == input$parcels_map_shape_click$id)
      click_name(NULL)     # Reset filter
    else
      click_name(input$parcels_map_shape_click$id)
  })
  
  # Parcels data table
  output$parcels_table <- DT::renderDataTable({
    DT::datatable( 
      if(is.null(click_name())) 
        parcels_df    # Not filtered
      else 
        parcels_df %>% filter( Страна==click_name())
    )
  })
}

shinyApp(ui, server)
