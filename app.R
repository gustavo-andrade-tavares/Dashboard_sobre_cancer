library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)

#dados ajustados com as variáveis traduzidas para visualização no mapa 
dados_cancer <- read_rds("Dados_cancer.rds")

#dados ajustados para visualização em gráficos
dados_graficos <- read_rds("dados_canceres_grafico.rds")

continentes <- c("All","Africa", "Americas", "Asia", "Europe", "Oceania")

escolhas <- c("Mortes por câncer devido ao álcool(%)",
              "Tabagismo masculino (%)", "Tabagismo feminino (%)", "Cânceres atribuíveis a infecções (%)",
              "Obesidade masculina (%)", "Obesidade feminina (%)", "Câncer de pele Melanoma (por 100 mil)",
              "Amamentação aos 12 meses (%)", "Média de nascimentos por mulher", "Poluição do ar exterior (micro*g/m^3)",
              "Poluição do ar interno (%)", "Câncer de pulmão, homens (por 100 mil)", "Câncer de pulmão, mulheres (por 100 mil)",
              "Pessoas indígenas (% do total da pop.)", "Sobreviventes de câncer (por 100 mil)", "Anos vividos com deficiência devido ao câncer",
              "Vacinação contra hepatite B (% crianças, 3 doses)", "Disponibilidade de radioterapia (por 1000 pacientes)",
              "Mortes não tratadas com dor", "Custo de doenças atribuíveis ao fumo (US$ bilhões)", "Câncer cervical (por 100 mil)",
              "HIV (%)", "UICC (organizações de câncer)")

palhetas <- c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdYlBu", "RdYlGn", "Spectral")

ui <- dashboardPage(
  dashboardHeader(title = "Cancer Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Mapa interativo", tabName = "map", icon = icon("globe")),
      menuItem("Gráficos", tabName = "graf", icon = icon("th")),
      menuItem("Estatísticas", tabName = "estat", icon = icon("signal"))),
    collapsed = TRUE
  ),
  dashboardBody(
    tabItems(
      #first tab content
      tabItem(tabName = "map",
              column(width = 9, 
                     box(width = NULL, solidHeader = TRUE, 
                         leafletOutput("mapa_principal", height = 500))),
              column(width = 3, 
                     box(width = NULL, status = "warning", solidHeader = TRUE, 
                         selectInput("var", "Escolha a variável", escolhas),
                         p(class = "text-muted",
                           paste("Nota: Ao clicar sobre um país, é possível",
                                 "visualizar o valor exato."))),
                     box(width = NULL, status = "warning", solidHeader = TRUE,
                         selectInput("pal", "Escolha a paleta de cores", palhetas)))),
      #second tab content
      tabItem(tabName = "graf",
              fluidRow(
                h2(" 1) Tabagismo masculino vs Câncer de pulmão"),
                column(width = 9,
                       box(width = NULL, solidHeader = TRUE,
                           plotlyOutput("plot1", height = 500))),
                column(width = 3,
                       box(width = NULL, status = "warning", solidHeader = TRUE,
                           selectInput("cont1", "Escolha o continente", continentes)))),
              
              fluidRow(
                h2(" 2) Tabagismo feminino vs Câncer de pulmão"),
                column(width = 9,
                       box(width = NULL, solidHeader = TRUE,
                           plotlyOutput("plot2", height = 500))),
                column(width = 3,
                       box(width = NULL, status = "warning", solidHeader = TRUE,
                           selectInput("cont2", "Escolha o continente", continentes)))
              ),
              
              fluidRow(
                h2(" 3) Cânceres atribuiveis a infecções"),
                column(width = 9,
                       box(width = NULL, solidHeader = TRUE,
                           plotlyOutput("plot3", height = 500))),
                column(width = 3,
                       box(width = NULL, status = "warning", solidHeader = TRUE,
                           selectInput("cont3", "Escolha o continente", continentes)))
              ),
              
              fluidRow(
                h2(" 4) Câncer cervical"),
                column(width = 9,
                       box(width = NULL, solidHeader = TRUE,
                           plotlyOutput("plot4", height = 500))),
                column(width = 3,
                       box(width = NULL, status = "warning", solidHeader = TRUE,
                           selectInput("cont4", "Escolha o continente", continentes)))
              ),
              
              fluidRow(
                h2(" 5) Sobreviventes de câncer"),
                column(width = 9,
                       box(width = NULL, solidHeader = TRUE,
                           plotlyOutput("plot5", height = 500))),
                column(width = 3,
                       box(width = NULL, status = "warning", solidHeader = TRUE,
                           selectInput("cont5", "Escolha o continente", continentes)))
              ),
              
              fluidRow(
                h2(" 6) Câncer de pele"),
                column(width = 9,
                       box(width = NULL, solidHeader = TRUE,
                           plotlyOutput("plot6", height = 500))),
                column(width = 3,
                       box(width = NULL, status = "warning", solidHeader = TRUE,
                           selectInput("cont6", "Escolha o continente", continentes)))
              ),
              
              fluidRow(
                h2(" 7) Disponibilidade de radioterapia"),
                column(width = 9,
                       box(width = NULL, solidHeader = TRUE,
                           plotlyOutput("plot7", height = 500))),
                column(width = 3,
                       box(width = NULL, status = "warning", solidHeader = TRUE,
                           selectInput("cont7", "Escolha o continente", continentes)))
              ),
              
              fluidRow(
                h2(" 8) Organizações de câncer"),
                column(width = 9,
                       box(width = NULL, solidHeader = TRUE,
                           plotlyOutput("plot8", height = 500))),
                column(width = 3,
                       box(width = NULL, status = "warning", solidHeader = TRUE,
                           selectInput("cont8", "Escolha o continente", continentes)))
              )
              
              
              
              
      ),
      
      #third tab content
      tabItem(tabName = "estat",
              fluidRow(
                h2(" 1) Mortes por câncer devido ao álcool"),
                column(width = 9,
                       box(width = NULL,
                           uiOutput("tabela1")))
              ),
              
              fluidRow(
                h2(" 2) Cânceres atribuíveis a infecções"),
                column(width = 9,
                       box(width = NULL,
                           uiOutput("tabela2")))
              ),
              
              fluidRow(
                h2(" 3) Câncer de pele Melanoma"),
                column(width = 9,
                       box(width = NULL,
                           uiOutput("tabela3")))
              ),
              
              fluidRow(
                h2(" 4) Câncer de pulmão, homens"),
                column(width = 9,
                       box(width = NULL,
                           uiOutput("tabela4")))
              ),
              
              fluidRow(
                h2(" 5) Câncer de pulmão, mulheres"),
                column(width = 9,
                       box(width = NULL,
                           uiOutput("tabela5")))
              ),
              
              fluidRow(
                h2(" 6) Sobreviventes de câncer"),
                column(width = 9,
                       box(width = NULL,
                           uiOutput("tabela6")))
              )
              
              
              
      )
      
    )
  )
)

server <- function(input, output) {
  
  
  output$mapa_principal <- renderLeaflet({
    variavel <- input$var
    palhet <- input$pal
    
    colorData <- dados_cancer[[variavel]]
    pal <- colorBin(palhet, colorData)
    
    dados_cancer %>% leaflet() %>% addTiles() %>% addPolygons(
      fillColor = ~pal(colorData),
      label = ~country_or_territory,
      smoothFactor = 0.5,
      fillOpacity = 0.5,
      weight = 0.5,
      opacity = 0.8,
      stroke = T,
      color = "black",
      highlightOptions = highlightOptions(
        color = "black",
        weight = 2,
        bringToFront = TRUE
      ),
      #mostra o país e a variável selecionada
      popup = ~ paste0(
        sep = " ",
        "<b>", country_or_territory, "<b><br>",
        if(variavel == "Mortes por câncer devido ao álcool(%)"){
          paste0("Proporção de mortes por câncer devido ao álcool em homens com 15 anos ou mais (2016):","<br>", `Mortes por câncer devido ao álcool(%)` ,"%")
        } 
        else if(variavel == "Tabagismo masculino (%)"){
          paste0("Prevalência de tabagismo masculino:","<br>", `Tabagismo masculino (%)` ,"%")
        } 
        else if(variavel == "Tabagismo feminino (%)"){
          paste0("Prevalência de tabagismo feminino:","<br>", `Tabagismo feminino (%)`,"%")
        } 
        else if(variavel == "Cânceres atribuíveis a infecções (%)"){
          paste0("Proporção de cânceres atribuíveis a infecções:","<br>", `Cânceres atribuíveis a infecções (%)`, "%")
        } 
        else if(variavel == "Obesidade masculina (%)"){
          paste0("Prevalência de obesidade masculina (2016):","<br>", `Obesidade masculina (%)`, "%")
        } 
        else if(variavel == "Obesidade feminina (%)"){
          paste0("Prevalência de obesidade feminina (2016):","<br>", `Obesidade feminina (%)`, "%")
        } 
        else if(variavel == "Câncer de pele Melanoma (por 100 mil)"){
          paste0("Incidência de câncer de pele Melanoma, ambos os sexos (2018):","<br>", `Câncer de pele Melanoma (por 100 mil)`)
        } 
        else if(variavel == "Amamentação aos 12 meses (%)"){
          paste0("Amamentação aos 12 meses:","<br>", `Amamentação aos 12 meses (%)` , "%")
        } 
        else if(variavel == "Média de nascimentos por mulher"){
          paste0("Média de nascimentos por mulher (2010-2015):","<br>", `Média de nascimentos por mulher`)
        } 
        else if(variavel == "Poluição do ar exterior (micro*g/m^3)"){
          paste0("Poluição do ar exterior (2017):","<br>", `Poluição do ar exterior (micro*g/m^3)` )
        } 
        else if(variavel == "Poluição do ar interno (%)"){
          paste0("Poluição do ar interno, proporção da população que usa combustíveis sólidos (2017):","<br>", `Poluição do ar interno (%)`, "%")
        } 
        else if(variavel == "Câncer de pulmão, homens (por 100 mil)"){
          paste0("Taxa de incidência de câncer de pulmão, homens (2018): ","<br>", `Câncer de pulmão, homens (por 100 mil)` )
        } 
        else if(variavel == "Câncer de pulmão, mulheres (por 100 mil)"){
          paste0("Taxa de incidência de câncer de pulmão, mulheres (2018): ","<br>", `Câncer de pulmão, mulheres (por 100 mil)`)
        } 
        else if(variavel == "Pessoas indígenas (% do total da pop.)"){
          paste0("Povos indígenas como porcentagem da população total:","<br>", `Pessoas indígenas (% do total da pop.)` , "%")
        } 
        else if(variavel == "Sobreviventes de câncer (por 100 mil)"){
          paste0("Sobreviventes de câncer nos últimos cinco anos, ambos os sexos (2018): ","<br>", `Sobreviventes de câncer (por 100 mil)`)
        } 
        else if(variavel == "Anos vividos com deficiência devido ao câncer"){
          paste0("Anos vividos com deficiência devido ao câncer, ambos os sexos, todas as idades (2017): ","<br>", `Anos vividos com deficiência devido ao câncer` )
        } 
        else if(variavel == "Vacinação contra hepatite B (% crianças, 3 doses)"){
          paste0("Vacinação contra hepatite B (cobertura):","<br>", `Vacinação contra hepatite B (% crianças, 3 doses)`, "%")
        } 
        else if(variavel == "Disponibilidade de radioterapia (por 1000 pacientes)"){
          paste0("Disponibilidade de radioterapia, número de máquinas de radioterapia por 1000 pacientes com câncer:","<br>", `Disponibilidade de radioterapia (por 1000 pacientes)` )
        } 
        else if(variavel == "Mortes não tratadas com dor"){
          paste0("Mortes não tratadas com dor (2016): ","<br>", `Mortes não tratadas com dor`)
        } 
        else if(variavel == "Custo de doenças atribuíveis ao fumo (US$ bilhões)"){
          paste0("Custo de doenças atribuíveis ao fumo: ","<br>", `Custo de doenças atribuíveis ao fumo (US$ bilhões)` )
        } 
        else if(variavel == "Câncer cervical (por 100 mil)"){
          paste0("Taxas de incidência de câncer cervical (2018): ","<br>", `Câncer cervical (por 100 mil)`)
        } 
        else if(variavel == "HIV (%)"){
          paste0("Prevalência de HIV, ambos os sexos (2017): ","<br>", `HIV (%)` , "%")
        } 
        else {
          paste0("UICC (organizações de câncer): ","<br>", `UICC (organizações de câncer)`)
        }
      )) %>%
      addLegend("bottomright",
                title = variavel,
                pal = pal,
                values = ~colorData)
  })
  
  
  
  output$plot1 <- renderPlotly({
    cont <- input$cont1
    
    if(cont == "All" ){
      p <- dados_graficos %>%
        filter(continent != "NA") %>% 
        ggplot(aes(x = `Tabagismo masculino (%)`, y = `Câncer de pulmão, homens (por 100 mil)`, color = continent))+
        geom_point()+
        geom_smooth(method = "lm", se = FALSE)+
        labs(title = "Tabagismo masculino vs Câncer de pulmão", caption = "Elaborado pelos autores.")
      
      fig <- ggplotly(p)
      
      fig
    } else {
      p <- dados_graficos %>%
        filter(continent == cont) %>% 
        ggplot(aes(x = `Tabagismo masculino (%)`, y = `Câncer de pulmão, homens (por 100 mil)`, color = continent))+
        geom_point()+
        geom_smooth(method = "lm", se = FALSE)+
        labs(title = "Tabagismo masculino vs Câncer de pulmão", caption = "Elaborado pelos autores.")
      
      fig <- ggplotly(p)
      
      fig
    }
    
    
  })
  
  
  output$plot2 <- renderPlotly({
    cont <- input$cont2
    
    if(cont == "All" ){
      p <- dados_graficos %>%
        filter(continent != "NA") %>% 
        ggplot(aes(x = `Tabagismo feminino (%)`, y = `Câncer de pulmão, mulheres (por 100 mil)`, color = continent))+
        geom_point()+
        geom_smooth(method = "lm", se = FALSE)+
        labs(title = "Tabagismo feminino vs Câncer de pulmão", caption = "Elaborado pelos autores.")
      
      fig <- ggplotly(p)
      
      fig
    } else {
      p <- dados_graficos %>%
        filter(continent == cont) %>% 
        ggplot(aes(x = `Tabagismo feminino (%)`, y = `Câncer de pulmão, mulheres (por 100 mil)`, color = continent))+
        geom_point()+
        geom_smooth(method = "lm", se = FALSE)+
        labs(title = "Tabagismo feminino vs Câncer de pulmão", caption = "Elaborado pelos autores.")
      
      fig <- ggplotly(p)
      
      fig
    }
    
  })
  
  output$plot3 <- renderPlotly({
    cont <- input$cont3
    
    if(cont == "All" ){
      p <- dados_graficos %>% 
        filter(continent != "NA") %>%
        ggplot(aes(x = `Cânceres atribuíveis a infecções (%)`))+
        geom_histogram(aes(fill = continent))+
        labs(title = "Proporção de cânceres atribuíveis a infecções", y = "Quantidade de países", caption = "Elaborado pelos autores.")
      
      fig <- ggplotly(p)
      
      fig
    } else {
      p <- dados_graficos %>% 
        filter(continent == cont) %>%
        ggplot(aes(x = `Cânceres atribuíveis a infecções (%)`))+
        geom_histogram(aes(fill = continent))+
        labs(title = "Proporção de cânceres atribuíveis a infecções", y = "Quantidade de países", caption = "Elaborado pelos autores.")
      
      fig <- ggplotly(p)
      
      fig
    }
    
  })
  
  output$plot4 <- renderPlotly({
    cont <- input$cont4
    
    if(cont == "All" ){
      p <- dados_graficos %>% 
        filter(continent != "NA") %>%
        ggplot(aes(x = `Câncer cervical (por 100 mil)`))+
        geom_histogram(aes(fill = continent))+
        labs(title = "Taxas de incidência de câncer cervical", y = "Quantidade de países", caption = "Elaborado pelos autores.")
      
      fig <- ggplotly(p)
      
      fig
    } else {
      p <- dados_graficos %>% 
        filter(continent == cont) %>%
        ggplot(aes(x = `Câncer cervical (por 100 mil)`))+
        geom_histogram(aes(fill = continent))+
        labs(title = "Taxas de incidência de câncer cervical", y = "Quantidade de países", caption = "Elaborado pelos autores.")
      
      fig <- ggplotly(p)
      
      fig
    }
    
  })
  
  output$plot5 <- renderPlotly({
    cont <- input$cont5
    
    if(cont == "All" ){
      p <- dados_graficos %>% 
        filter(continent != "NA") %>% 
        ggplot(aes(x = `Sobreviventes de câncer (por 100 mil)`))+
        geom_histogram(aes(fill = continent))+
        labs(title = "Sobreviventes de câncer nos últimos cinco anos", y = "Quantidade de países", caption = "Elaborado pelos autores.")
      
      fig <- ggplotly(p)
      
      fig
    } else {
      p <- dados_graficos %>% 
        filter(continent == cont) %>% 
        ggplot(aes(x = `Sobreviventes de câncer (por 100 mil)`))+
        geom_histogram(aes(fill = continent))+
        labs(title = "Sobreviventes de câncer nos últimos cinco anos", y = "Quantidade de países", caption = "Elaborado pelos autores.")
      
      fig <- ggplotly(p)
      
      fig
    }
    
  })
  
  output$plot6 <- renderPlotly({
    cont <- input$cont6
    
    if(cont == "All" ){
      p <- dados_graficos %>% 
        filter(continent != "NA") %>% 
        ggplot(aes(x = `Câncer de pele Melanoma (por 100 mil)`))+
        geom_histogram(aes(fill = continent))+
        labs(title = "Incidência de câncer de pele Melanoma", y = "Quantidade de países", caption = "Elaborado pelos autores.")
      
      fig <- ggplotly(p)
      
      fig
    } else {
      p <- dados_graficos %>% 
        filter(continent == cont) %>% 
        ggplot(aes(x = `Câncer de pele Melanoma (por 100 mil)`))+
        geom_histogram(aes(fill = continent))+
        labs(title = "Incidência de câncer de pele Melanoma", y = "Quantidade de países", caption = "Elaborado pelos autores.")
      
      fig <- ggplotly(p)
      
      fig
    }
    
  })
  
  output$plot7 <- renderPlotly({
    cont <- input$cont7
    
    if(cont == "All" ){
      p <- dados_graficos %>% 
        filter(continent != "NA") %>% 
        ggplot(aes(x = continent, y = `Disponibilidade de radioterapia (por 1000 pacientes)`))+
        geom_boxplot(aes(fill = continent))+
        labs(title = "Disponibilidade de radioterapia", x = "continente", caption = "Elaborado pelos autores.")
      
      fig <- ggplotly(p)
      
      fig
    } else {
      p <- dados_graficos %>% 
        filter(continent == cont) %>% 
        ggplot(aes(x = continent, y = `Disponibilidade de radioterapia (por 1000 pacientes)`))+
        geom_boxplot(aes(fill = continent))+
        labs(title = "Disponibilidade de radioterapia", x = "continente", caption = "Elaborado pelos autores.")
      
      fig <- ggplotly(p)
      
      fig
    }
    
  })
  
  output$plot8 <- renderPlotly({
    cont <- input$cont8
    
    if(cont == "All" ){
      p <- dados_graficos %>% 
        filter(continent != "NA") %>% 
        ggplot(aes(x = `UICC (organizações de câncer)`))+
        geom_histogram(aes(fill = continent))+
        labs(title = "Quantidade de organizações de câncer por país", y = "Quantidade de países", caption = "Elaborado pelos autores.")
      
      fig <- ggplotly(p)
      
      fig
    } else {
      p <- dados_graficos %>% 
        filter(continent == cont) %>% 
        ggplot(aes(x = `UICC (organizações de câncer)`))+
        geom_histogram(aes(fill = continent))+
        labs(title = "Quantidade de organizações de câncer por país", y = "Quantidade de países", caption = "Elaborado pelos autores.")
      
      fig <- ggplotly(p)
      
      fig
    }
    
  })
  
  output$tabela1 <- renderUI({
    tags$table(class = "table",
               tags$thead(tags$tr(
                 tags$th("Continente"),
                 tags$th("min"),
                 tags$th("Q1"),
                 tags$th("Q2"),
                 tags$th("Q3"),
                 tags$th("média"),
                 tags$th("max")
               )),
               tags$tbody(tags$tr(
                 tags$td("Asia"),
                 tags$td(round(0.300, 2)),
                 tags$td(round(1.425, 2)),
                 tags$td(round(3.200, 2)),
                 tags$td(round(6.375, 2)),
                 tags$td(round(3.936, 2)),
                 tags$td(round(10.500, 2))
               ),
               tags$tr(
                 tags$td("Africa"),
                 tags$td(round(0.200, 2)),
                 tags$td(round(1.975, 2)),
                 tags$td(round(4.250, 2)),
                 tags$td(round(7.250, 2)),
                 tags$td(round(4.698, 2)),
                 tags$td(round(10.600, 2))
               ),
               tags$tr(
                 tags$td("Americas"),
                 tags$td(round(2.300, 2)),
                 tags$td(round(3.800, 2)),
                 tags$td(round(4.550, 2)),
                 tags$td(round(5.350, 2)),
                 tags$td(round(4.661, 2)),
                 tags$td(round(8.700, 2))
               ),
               tags$tr(
                 tags$td("Europe"),
                 tags$td(round(4.900, 2)),
                 tags$td(round(6.850, 2)),
                 tags$td(round(8.100, 2)),
                 tags$td(round(9.250, 2)),
                 tags$td(round(8.166, 2)),
                 tags$td(round(11.800, 2))
               ),
               tags$tr(
                 tags$td("Oceania"),
                 tags$td(round(2.900, 2)),
                 tags$td(round(3.175, 2)),
                 tags$td(round(4.100, 2)),
                 tags$td(round(6.000, 2)),
                 tags$td(round(4.617, 2)),
                 tags$td(round(7.100, 2))
               )
               
               ))
  })
  
  output$tabela2 <- renderUI({
    tags$table(class = "table",
               tags$thead(tags$tr(
                 tags$th("Continente"),
                 tags$th("min"),
                 tags$th("Q1"),
                 tags$th("Q2"),
                 tags$th("Q3"),
                 tags$th("média"),
                 tags$th("max")
               )),
               tags$tbody(tags$tr(
                 tags$td("Asia"),
                 tags$td(round(5.40, 2)),
                 tags$td(round(11.22, 2)),
                 tags$td(round(16.10, 2)),
                 tags$td(round(20.60, 2)),
                 tags$td(round(16.73, 2)),
                 tags$td(round(49.80, 2))
               ),
               tags$tr(
                 tags$td("Africa"),
                 tags$td(round(10.70, 2)),
                 tags$td(round(23.70, 2)),
                 tags$td(round(30.40, 2)),
                 tags$td(round(36.00, 2)),
                 tags$td(round(30.59, 2)),
                 tags$td(round(52.50, 2))
               ),
               tags$tr(
                 tags$td("Americas"),
                 tags$td(round(3.90, 2)),
                 tags$td(round(11.00, 2)),
                 tags$td(round(18.40, 2)),
                 tags$td(round(21.40, 2)),
                 tags$td(round(17.29, 2)),
                 tags$td(round(33.60, 2))
               ),
               tags$tr(
                 tags$td("Europe"),
                 tags$td(round(3.600, 2)),
                 tags$td(round(4.400, 2)),
                 tags$td(round(7.200, 2)),
                 tags$td(round(9.600, 2)),
                 tags$td(round(7.451, 2)),
                 tags$td(round(13.400, 2))
               ),
               tags$tr(
                 tags$td("Oceania"),
                 tags$td(round(3.700, 2)),
                 tags$td(round(5.80, 2)),
                 tags$td(round(21.20, 2)),
                 tags$td(round(22.700, 2)),
                 tags$td(round(15.16, 2)),
                 tags$td(round(24.20, 2))
               )
               
               ))
  })
  
  output$tabela3 <- renderUI({
    tags$table(class = "table",
               tags$thead(tags$tr(
                 tags$th("Continente"),
                 tags$th("min"),
                 tags$th("Q1"),
                 tags$th("Q2"),
                 tags$th("Q3"),
                 tags$th("média"),
                 tags$th("max")
               )),
               tags$tbody(tags$tr(
                 tags$td("Asia"),
                 tags$td(round(0.400, 2)),
                 tags$td(round(0.3225, 2)),
                 tags$td(round(0.4750, 2)),
                 tags$td(round(0.7450, 2)),
                 tags$td(round(0.9076, 2)),
                 tags$td(round(8.3000, 2))
               ),
               tags$tr(
                 tags$td("Africa"),
                 tags$td(round(0.230, 2)),
                 tags$td(round(0.570 , 2)),
                 tags$td(round(0.920, 2)),
                 tags$td(round(1.450, 2)),
                 tags$td(round(1.044, 2)),
                 tags$td(round(2.800, 2))
               ),
               tags$tr(
                 tags$td("Americas"),
                 tags$td(round(0.0800, 2)),
                 tags$td(round(0.7225, 2)),
                 tags$td(round(2.0000, 2)),
                 tags$td(round(2.7250, 2)),
                 tags$td(round(2.5071, 2)),
                 tags$td(round(12.7000, 2))
               ),
               tags$tr(
                 tags$td("Europe"),
                 tags$td(round(1.70, 2)),
                 tags$td(round(5.55, 2)),
                 tags$td(round(10.10, 2)),
                 tags$td(round(16.40, 2)),
                 tags$td(round(12.02, 2)),
                 tags$td(round(29.60, 2))
               ),
               tags$tr(
                 tags$td("Oceania"),
                 tags$td(round(0.49, 2)),
                 tags$td(round(0.88, 2)),
                 tags$td(round(3.60 , 2)),
                 tags$td(round(20.30, 2)),
                 tags$td(round(11.44, 2)),
                 tags$td(round(33.60, 2))
               )
               
               ))
  })
  
  output$tabela4 <- renderUI({
    tags$table(class = "table",
               tags$thead(tags$tr(
                 tags$th("Continente"),
                 tags$th("min"),
                 tags$th("Q1"),
                 tags$th("Q2"),
                 tags$th("Q3"),
                 tags$th("média"),
                 tags$th("max")
               )),
               tags$tbody(tags$tr(
                 tags$td("Asia"),
                 tags$td(round(3.90, 2)),
                 tags$td(round(10.60, 2)),
                 tags$td(round(19.45, 2)),
                 tags$td(round(32.83, 2)),
                 tags$td(round(23.34, 2)),
                 tags$td(round(70.60, 2))
               ),
               tags$tr(
                 tags$td("Africa"),
                 tags$td(round(0.850, 2)),
                 tags$td(round(2.975 , 2)),
                 tags$td(round(3.700, 2)),
                 tags$td(round(5.650, 2)),
                 tags$td(round(6.204, 2)),
                 tags$td(round(31.900, 2))
               ),
               tags$tr(
                 tags$td("Americas"),
                 tags$td(round(3.90, 2)),
                 tags$td(round(7.70, 2)),
                 tags$td(round(14.60, 2)),
                 tags$td(round(21.10, 2)),
                 tags$td(round(16.61, 2)),
                 tags$td(round(46.20, 2))
               ),
               tags$tr(
                 tags$td("Europe"),
                 tags$td(round(16.90, 2)),
                 tags$td(round(36.40, 2)),
                 tags$td(round(46.60, 2)),
                 tags$td(round(52.40, 2)),
                 tags$td(round(45.45, 2)),
                 tags$td(round(77.40, 2))
               ),
               tags$tr(
                 tags$td("Oceania"),
                 tags$td(round(8.0, 2)),
                 tags$td(round(12.7, 2)),
                 tags$td(round(15.7 , 2)),
                 tags$td(round(26.7, 2)),
                 tags$td(round(23.2, 2)),
                 tags$td(round(59.9, 2))
               )
               
               ))
  })
  
  output$tabela5 <- renderUI({
    tags$table(class = "table",
               tags$thead(tags$tr(
                 tags$th("Continente"),
                 tags$th("min"),
                 tags$th("Q1"),
                 tags$th("Q2"),
                 tags$th("Q3"),
                 tags$th("média"),
                 tags$th("max")
               )),
               tags$tbody(tags$tr(
                 tags$td("Asia"),
                 tags$td(round(1.500, 2)),
                 tags$td(round(4.175, 2)),
                 tags$td(round(6.150, 2)),
                 tags$td(round(9.525, 2)),
                 tags$td(round(8.076, 2)),
                 tags$td(round(26.600, 2))
               ),
               tags$tr(
                 tags$td("Africa"),
                 tags$td(round(0.040, 2)),
                 tags$td(round(1.275 , 2)),
                 tags$td(round(1.700, 2)),
                 tags$td(round(2.675, 2)),
                 tags$td(round(2.122, 2)),
                 tags$td(round(9.700, 2))
               ),
               tags$tr(
                 tags$td("Americas"),
                 tags$td(round(2.300, 2)),
                 tags$td(round(4.800, 2)),
                 tags$td(round(7.300, 2)),
                 tags$td(round(10.500, 2)),
                 tags$td(round(9.245, 2)),
                 tags$td(round(30.800 , 2))
               ),
               tags$tr(
                 tags$td("Europe"),
                 tags$td(round(5.70, 2)),
                 tags$td(round(12.95, 2)),
                 tags$td(round(18.10, 2)),
                 tags$td(round(27.75, 2)),
                 tags$td(round(19.56, 2)),
                 tags$td(round(41.40, 2))
               ),
               tags$tr(
                 tags$td("Oceania"),
                 tags$td(round(1.80, 2)),
                 tags$td(round(4.35, 2)),
                 tags$td(round(8.90 , 2)),
                 tags$td(round(24.80, 2)),
                 tags$td(round(13.63, 2)),
                 tags$td(round(26.40, 2))
               )
               
               ))
  })
  
  output$tabela6 <- renderUI({
    tags$table(class = "table",
               tags$thead(tags$tr(
                 tags$th("Continente"),
                 tags$th("min"),
                 tags$th("Q1"),
                 tags$th("Q2"),
                 tags$th("Q3"),
                 tags$th("média"),
                 tags$th("max")
               )),
               tags$tbody(tags$tr(
                 tags$td("Asia"),
                 tags$td(round(115.4, 2)),
                 tags$td(round(199.2, 2)),
                 tags$td(round(274.1, 2)),
                 tags$td(round(353.2, 2)),
                 tags$td(round(319.1, 2)),
                 tags$td(round(880.7, 2))
               ),
               tags$tr(
                 tags$td("Africa"),
                 tags$td(round(89.1, 2)),
                 tags$td(round(155.0 , 2)),
                 tags$td(round(191.2, 2)),
                 tags$td(round(240.7, 2)),
                 tags$td(round(203.7, 2)),
                 tags$td(round(436.2, 2))
               ),
               tags$tr(
                 tags$td("Americas"),
                 tags$td(round(207.3, 2)),
                 tags$td(round(290.3, 2)),
                 tags$td(round(418.3, 2)),
                 tags$td(round(512.1, 2)),
                 tags$td(round(461.5, 2)),
                 tags$td(round(1195.7 , 2))
               ),
               tags$tr(
                 tags$td("Europe"),
                 tags$td(round(401.1, 2)),
                 tags$td(round(705.4, 2)),
                 tags$td(round(803.3, 2)),
                 tags$td(round(993.0, 2)),
                 tags$td(round(823.5, 2)),
                 tags$td(round(1240.5, 2))
               ),
               tags$tr(
                 tags$td("Oceania"),
                 tags$td(round(199.7, 2)),
                 tags$td(round(275.1, 2)),
                 tags$td(round(395.0 , 2)),
                 tags$td(round(1302.2, 2)),
                 tags$td(round(799.9, 2)),
                 tags$td(round(1849.8, 2))
               )
               
               ))
  })
  
}

shinyApp(ui, server)