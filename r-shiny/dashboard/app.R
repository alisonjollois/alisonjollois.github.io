library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

evol_globale = txhousing %>%
  group_by(year) %>%
  summarise(volume = sum(volume, na.rm = T))

tri <- function(don, ordre) {
  if (ordre) {
    don = don %>%
      arrange(Volume)
  } else {
    don = don %>%
      arrange(desc(Volume))
  }
  don
}

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
            plotOutput("evolution", height = 250),
            checkboxInput("lm", "Droite d'ajustement")
          ),
          box(
            width = 4,
            selectInput("ville", "Ville choisie", 
                        choices = c("Toutes les villes", unique(txhousing$city)))
          ),
          infoBoxOutput("progression"),
          valueBoxOutput("volume"),
          box(
            title = "Evolution en base 100",
            footer = "Base 100 en 2000",
            width = 8,
            plotOutput("base100", height = 250)
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
          box(
            width = 12,
            fluidRow(
              column(
                width = 6,
                radioButtons("taille", "Taille des TOPS", c(3, 5, 10), inline = TRUE)
              ),
              column(
                width = 6,
                checkboxInput("ordre", "TOP inversé")
              )
            )
          )
          ,
          box(title = "Ville", width = 4, tableOutput("top_ville")),
          box(title = "Année", width = 4, tableOutput("top_annee")),
          box(title = "Mois", width = 4, tableOutput("top_mois"))
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
      evol %>%
        mutate(volume = ifelse(volume == 0, NA, volume))
    })
    
    output$evolution <- renderPlot({
      g = ggplot(donnees(), aes(year, volume / 1e+9)) +
        geom_line(lwd = 1) +
        theme_minimal() +
        labs(x = "", y = "Volume des ventes")
      if (input$lm) {
        g = g + geom_smooth(method = "lm", se = FALSE)
      }
      g
    })
    
    output$progression <- renderInfoBox({
      evol = donnees()
      prog = tail(evol$volume, 1) / head(evol$volume, 1) - 1
      
      infoBox(
        title = "Progression",
        value = ifelse(is.na(prog), "Non calculable", paste(round(prog * 100), "%")),
        subtitle = "Entre 2000 et 2015",
        icon = icon(ifelse(is.na(prog), "warning", "line-chart")),
        fill = FALSE,
        color = ifelse(is.na(prog), "red", ifelse(prog > 0, "green", "orange")),
        width = 4
      )
    })
    output$volume <- renderValueBox({
      evol = donnees()
      val = round(sum(evol$volume, na.rm = T) / 1e+9, 1)
      valueBox(
        value = val,
        subtitle = "Volume totale des ventes (en milliards)",
        icon = icon("usd"),
        color = ifelse(val < 2, "red", ifelse(val < 50, "light-blue", "green")),
        width = 4
      )
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
    
    output$top_ville <- renderTable({
      don = txhousing %>%
        group_by(city) %>%
        summarise(Volume = sum(volume, na.rm = T) / 1e+9) %>%
        rename(Ville = city) 
      tri(don, input$ordre) %>%
        head(as.numeric(input$taille))
    }, digits = 1)
    output$top_annee <- renderTable({
      don = txhousing %>%
        group_by(year) %>%
        summarise(Volume = sum(volume, na.rm = T) / 1e+9) %>%
        rename(`Année` = year) 
      tri(don, input$ordre) %>%
        head(as.numeric(input$taille))
    }, digits = 1)
    output$top_mois <- renderTable({
      don = txhousing %>%
        mutate(month_name = month.name[month]) %>%
        group_by(month_name) %>%
        summarise(Volume = sum(volume, na.rm = T) / 1e+9) %>%
        rename(Mois = month_name) 
      tri(don, input$ordre) %>%
        head(as.numeric(input$taille))
    }, digits = 1)
    
    output$base100 <- renderPlot({
      evol = donnees() %>%
        mutate(volume = volume / volume[1] * 100)
      g = ggplot(evol, aes(year, volume)) +
        geom_line(lwd = 1)  +
        theme_minimal() +
        labs(x = "", y = "Evolution des ventes")
      if (input$ville != "Toutes les villes") {
        g = g + 
          geom_line(data = evol_globale %>% mutate(volume = volume / volume[1] * 100), 
                    aes(y = volume), col = "gray", lwd = 1, lty = 2)
      }
      g
    })
  }
)