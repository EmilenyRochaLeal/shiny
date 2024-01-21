library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(readxl)
library(dplyr)
library(ggplot2)
library(maps)
dados_abertos_IC <- read_excel("/home/mambee/Aluna/aplicantion/app/dados_abertos_IC.xlsx")

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = 'Iniciação Científica 2023.1', titleWidth = 260),
  
  dashboardSidebar(
    width = 260,
    
    sidebarMenu(
      id = 'sidebar',
      menuItem("Iniciação científica", tabName = "dashboard", icon = icon("")),
      menuItem("Saiba mais", tabName = "sobre", icon = icon("graduation-cap")),
      menuItem("Acesse o código", href = "https://github.com/beatrizmilz/QualisCAPES",  icon = icon("github-square"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "dashboard",
        "Essa página foi elaborada com a intenção de visualizar dados sobre pesquisa científica nas universidades",
        br(), br(),
        "É possivel visualizar as 10 instituições que mais tiveram pesquisa científica em 2023.1",
        br(), br(),
        "É possível consultar a quantidade por estado baseada na área",
        br(), br(),
        fluidRow(
          box(
            selectizeInput(
              inputId = "areas",
              label = "Selecione a área da pesquisa",
              choices = sort(unique(dados_abertos_IC$...12)),
              multiple = TRUE,
              selected = FALSE
            ), 
          ),
          br(), br(),
              plotOutput("universidades"),
          br(), br(), br(), br(),br(), br(),
            plotOutput("iniciacao_por_estado"),
        ),
        br(), br(),
      ),
      tabItem(tabName = "sobre",
              h2("Sobre o projeto"),
              "- Essa página foi elaborada com a intenção de facilitar a visualização dos dados armazenados na planilha de relátorio do ano letivo 2023.1. Disponivel em : ",
              tags$a(href = "https://dados.gov.br/home", "Portal de dados abertos"),
              br(), br(), 
              "- Desenvolvido por",
              tags$a(href = "http://lattes.cnpq.br/2417460661242056", "Emileny Leal"),
              br(), br()
              )
    )
  )
)
server <- function(input, output) {
  
  dados_filtrados <- reactive({
    dados_abertos_IC %>% filter(...12 %in% input$areas)
  })
  
  output$iniciacao_por_estado <- renderPlot({
    ggplot(dados_filtrados(), aes(x = ...21)) + 
      geom_bar(color = "black", fill = "seagreen") + 
      labs(title = "Quantidade de Iniciação cientifica por estado", 
           x = "Estados",
           y = "Quantidade") +
      theme_minimal()
  })
  output$universidades <- renderPlot({
    top_universidades <- dados_filtrados() %>%
      group_by(...19) %>%
      summarise(Quantidade = n()) %>%
      top_n(10, Quantidade)
    
    ggplot(top_universidades, aes(x = Quantidade, y = reorder(...19, Quantidade))) + 
      geom_bar(stat = "identity", color = "black", fill = "seagreen") +
      labs(title = "Top 10 Instituições",
           x = "Quantidade",
           y = "Instituições") +
      theme_minimal()
  })
  
  
}
shinyApp(ui = ui, server = server)
