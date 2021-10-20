---
title: "Tendências de pesquisa por 'Covid' e 'Covid Sergipe' no Google, no Brasil, a partir de março de 2020"
output: github_document:
    orientation: columns
    navbar:
      - { title: "Café com Dados", href: "http://www.cafecomdados.com", align: right }
    logo: logo.png
    favicon: favicon.png
    css: dashpnad.css
    
---
  
  ```{r setup, include=FALSE}

require(gtrendsR)
require(magrittr)
require(dplyr)
require(xts)
require(tbl2xts)
require(dygraphs)
require(rgdal)
require(RColorBrewer)
require(leaflet)
require(wordcloud)
require(flexdashboard)

```

```{r global, include=FALSE}

S1 <- readOGR("Mapa\\.", "BRUFE250GC_SIR", 
              stringsAsFactors=FALSE,
              encoding="UTF-8", 
              use_iconv = TRUE, 
              verbose = FALSE)

```

Column {.sidebar}
-----------------------------------------------------------------------
  
  
  [Google Trends](https://pt.wikipedia.org/wiki/Google_Trends) é uma ferramenta do Google que mostra os mais populares termos buscados em um passado recente. Esta página do Café com Dados mostra como foi a busca pelo termo "Covid Sergipe" no primeiro gráfico, e "Covid" para o mapa do Brasil e para a nuvem de palavras. Os hits são indicadores dimensionados num intervalo de 0 a 100 com base na proporção de um tópico em relação a todas as pesquisas em todos os tópicos, para um determinado local (aqui no caso, Brasil), em determinado período de tempo (aqui definido como desde março de 2020).

Column {data-width=500}
-----------------------------------------------------------------------
  
### Interesse ao longo do tempo por "Covid Sergipe"
  
  ```{r}

# ler os dados

time <- paste0("2020-03-01"," ", Sys.Date())


covid <- gtrends("Covid",
                 geo = c("BR"),
                 time = time)


covid <- gtrends("Covid",
                   geo = c("BR"),
                   time = time)
                   
covid_SE <- gtrends("Covid Sergipe",
                   geo = c("BR"),
                   time = time)                 

covid_SE$interest_over_time %>% as_tibble() %>% 
  select(date, hits) %>% 
  tbl_xts() %>% 
  dygraph(ylab = "Hits") %>% 
  dyRangeSelector()

```

### Interesse por estado por "Covid"

```{r}

S1$hits <- 
  S1 %>% data.frame() %>% as_tibble() %>% 
  left_join(x = ., 
            covid$interest_by_region %>% 
              select(NM_ESTADO = location, hits) %>% as_tibble() %>% 
              mutate(NM_ESTADO = iconv(NM_ESTADO, from = "utf-8", to = "iso8859-1") %>% 
                       gsub("Federal District", "Distrito Federal", x = .) %>% 
                       gsub("State of ", "", x = .) %>% toupper()), 
            by = "NM_ESTADO") %>% pull(hits)

npal <- colorNumeric("RdYlBu", S1$hits, reverse = TRUE)

labels <- paste0("<strong>Estado: </strong>", 
                      S1$NM_ESTADO, 
                      "<br><strong>Hits: </strong>", 
                      S1$hits)

leaflet(data = S1) %>% 
  addTiles() %>% 
  addPolygons(fill = TRUE, 
              stroke = FALSE,
              color = ~npal(hits),
              smoothFactor = .2, 
              fillOpacity = .8,
              highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = FALSE),
              label = paste(S1$NM_ESTADO),
              popup = labels) %>% 
  addLegend(pal = npal,
            values = ~hits,
            opacity = 1,
            title="Hits covid")

```

Column {data-width=500}
-----------------------------------------------------------------------
  
### Pesquisas relacionadas à "Covid"
  
  ```{r}

set.seed(1234)

related <- 
  covid$related_queries %>% 
  filter(related_queries == "top") %>% 
  select(subject, value) %>% as_tibble() %>% 
  mutate(value = iconv(value, from = "utf-8", to = "iso8859-1"))

wordcloud(words = related %>% pull(value), freq = related %>% pull(subject) %>% as.numeric,
          colors = brewer.pal(6, "Dark2"), random.order = FALSE)

#d3wordcloud(words = related %>% pull(value), freq = related %>% pull(subject),
#            colors = brewer.pal(6, "Dark2"),
#            spiral = "rectangular")

```

