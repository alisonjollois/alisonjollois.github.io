library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

evol_globale = txhousing %>%
  group_by(year) %>%
  summarise(volume = sum(volume, na.rm = T))

shinyApp(
  ui = dashboardPage(
    dashboardHeader(
      title = "Texas Housing Dashboard",
      titleWidth = 300
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Vue globale", tabName = "vue", icon = icon("dashboard")),
        menuItem("TOPs", tabName = "top", icon = icon("list-ol")),
        menuItem("Données", icon = icon("database"), href = "https://www.recenter.tamu.edu/"),
        menuItem("Liste des icônes", icon = icon("font-awesome"), href = "http://fontawesome.io/icons/")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          "vue",
          box(
            title = "Evolution du volume de ventes",
            footer = "en US$",
            status = "info",
            solidHeader = TRUE,
            width = 8,
            plotOutput("evolution")
          ),
          box(
            width = 4,
            selectInput("ville", "Ville choisie", 
                        choices = c("Toutes les villes", unique(txhousing$city)))
          ),
          infoBox(
            title = "Progression",
            value = textOutput("progression"),
            subtitle = "Entre 2000 et 2015",
            icon = icon("line-chart"),
            fill = TRUE,
            color = "light-blue",
            width = 4
          ),
          valueBox(
            value = textOutput("volume"),
            subtitle = "Volume totale des ventes (en milliards)",
            icon = icon("usd"),
            color = "green",
            width = 4
          ),
          tabBox(
            title = "Informations",
            width = 4,
            tabPanel(title = "Prix médian", tableOutput("info_prix")),
            tabPanel(title = "Nombre", tableOutput("info_nombre"))
          )
        ),
        tabItem(
          "top",
          box(title = "Ville", width = 4, "TOP des meilleures villes"),
          box(title = "Année", width = 4, "TOP des meilleurs années"),
          box(title = "Mois", width = 4, "TOP des meilleurs mois")
        )
      )
    ),
    title = "Texas Housing",
    skin = "red"
  ),
  server = function(input, output) {
    donnees <- reactive({
      if (input$ville == "Toutes les villes") {
        evol = evol_globale
      } else {
        evol = txhousing %>%
          filter(city == input$ville) %>%
          group_by(year) %>%
          summarise(volume = sum(volume, na.rm = T))
      }
      evol
    })
    
    output$evolution <- renderPlot({
      ggplot(donnees(), aes(year, volume)) +
        geom_line() +
        theme_minimal() +
        labs(x = "", y = "Volume des ventes")
    })
    
    output$progression <- renderText({
      evol = donnees()
      paste(round(tail(evol$volume, 1) / head(evol$volume, 1) * 100), "%")
    })
    output$volume <- renderText({
      round(sum(evol_globale$volume, na.rm = T) / 1e+9, 1)
    })
    
    output$info_prix <- renderTable({
      data.frame(
        Statistique = c("Minimum", "Médiane", "Maximum"),
        Valeur = c(
          min(txhousing$median, na.rm = T),
          median(txhousing$median, na.rm = T),
          max(txhousing$median, na.rm = T)
        )
      )
    })
    output$info_nombre <- renderTable({
      data.frame(
        Statistique = c("Minimum", "Médiane", "Maximum"),
        Valeur = c(
          min(txhousing$sales, na.rm = T),
          median(txhousing$sales, na.rm = T),
          max(txhousing$sales, na.rm = T)
        )
      )
    })
  }
)
