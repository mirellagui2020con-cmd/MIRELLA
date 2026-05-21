#################################################
# UNCP - FACULTAD DE ECONOMÍA
# =============================================
# *** Econometría I
# *** Dashboard Económico Mundial
# *** Alumna: Guillermo Quispe Mirella
# =============================================

# =========================================================
# LIMPIAR ENTORNO
# =========================================================

rm(list = ls())

# =========================================================
# INSTALAR PAQUETES (SOLO LA PRIMERA VEZ)
# =========================================================

install.packages("shiny")
install.packages("shinydashboard")
install.packages("plotly")
install.packages("tidyverse")
install.packages("gapminder")
install.packages("DT")
install.packages("scales")

# =========================================================
# LIBRERÍAS
# =========================================================

library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(gapminder)
library(DT)
library(scales)

# =========================================================
# BASE DE DATOS
# =========================================================

data <- gapminder %>%
  
  filter(year >= 1960) %>%
  
  mutate(
    
    # PIB TOTAL
    gdp_total = gdpPercap * pop,
    
    # POBLACIÓN EN MILLONES
    pop_millions = pop / 1000000,
    
    # ÍNDICE ECONÓMICO
    economic_index = (gdpPercap * lifeExp)/1000,
    
    # CONTINENTE
    continent = as.factor(continent),
    
    # NIVEL ECONÓMICO
    income_level = case_when(
      gdpPercap < 2000 ~ "Bajo",
      gdpPercap < 10000 ~ "Medio",
      TRUE ~ "Alto"
    )
  )

# =========================================================
# UI
# =========================================================

ui <- dashboardPage(
  
  skin = "black",
  
  # =======================================================
  # HEADER
  # =======================================================
  
  dashboardHeader(
    title = "Dashboard Económico"
  ),
  
  # =======================================================
  # SIDEBAR
  # =======================================================
  
  dashboardSidebar(
    
    width = 260,
    
    sidebarMenu(
      
      menuItem(
        "Principal",
        tabName = "dashboard",
        icon = icon("chart-line")
      )
    ),
    
    br(),
    
    h4(
      "Filtros Económicos",
      style = "
      color:#F8EDEB;
      text-align:center;
      "
    ),
    
    br(),
    
    # CONTINENTE
    
    selectInput(
      "continent",
      "Selecciona Continente:",
      choices = levels(data$continent),
      selected = "Europe"
    ),
    
    # PAÍS
    
    selectInput(
      "country",
      "Selecciona País:",
      choices = sort(unique(data$country)),
      selected = "Peru"
    ),
    
    # AÑO
    
    sliderInput(
      "year",
      "Selecciona Año:",
      min = 1962,
      max = 2007,
      value = 2007,
      step = 5,
      animate = TRUE,
      sep = ""
    ),
    
    # ESCALA
    
    checkboxInput(
      "log_scale",
      "Usar Escala Logarítmica en PIB",
      value = FALSE
    )
  ),
  
  # =======================================================
  # BODY
  # =======================================================
  
  dashboardBody(
    
    tags$head(
      
      tags$style(HTML("
      
      /* ==================================================
      FONDO GENERAL
      ================================================== */
      
      .content-wrapper, .right-side {
        background-color: #FFF8FB;
      }
      
      /* ==================================================
      SIDEBAR PASTEL OSCURO
      ================================================== */
      
      .skin-black .main-sidebar {
        background-color: #4A4E69;
      }
      
      .skin-black .main-header .logo {
        background-color: #9A8C98;
        color: white;
        font-weight: bold;
      }
      
      .skin-black .main-header .navbar {
        background-color: #C9ADA7;
      }
      
      .skin-black .sidebar-menu > li.active > a {
        background-color: #F2D7D9;
        color: #4A4E69;
      }
      
      .skin-black .sidebar-menu > li > a:hover {
        background-color: #F6E7E7;
        color: #4A4E69;
      }
      
      /* ==================================================
      CAJAS
      ================================================== */
      
      .small-box {
        border-radius: 18px;
      }
      
      .box {
        border-radius: 18px;
        border-top: none;
      }
      
      /* ==================================================
      TITULOS
      ================================================== */
      
      h1, h2, h3 {
        color: #6D597A;
        font-weight: bold;
      }
      
      /* ==================================================
      TABS
      ================================================== */
      
      .nav-tabs-custom>.nav-tabs>li.active {
        border-top-color: #A0C4FF;
      }
      
      "))
    ),
    
    tabItems(
      
      # ===================================================
      # TAB PRINCIPAL
      # ===================================================
      
      tabItem(
        
        tabName = "dashboard",
        
        # =================================================
        # CAJAS SUPERIORES
        # =================================================
        
        fluidRow(
          
          valueBoxOutput("box1", width = 3),
          
          valueBoxOutput("box2", width = 3),
          
          valueBoxOutput("box3", width = 3),
          
          valueBoxOutput("box4", width = 3)
        ),
        
        # =================================================
        # FILA 1
        # =================================================
        
        fluidRow(
          
          # ===============================================
          # SCATTER
          # ===============================================
          
          box(
            
            width = 7,
            
            title = "Relación Salud vs Economía",
            
            status = "primary",
            
            solidHeader = TRUE,
            
            plotlyOutput("plot_scatter", height = 450)
          ),
          
          # ===============================================
          # LÍNEA
          # ===============================================
          
          box(
            
            width = 5,
            
            title = "Tendencia Histórica",
            
            status = "info",
            
            solidHeader = TRUE,
            
            plotlyOutput("plot_line", height = 450)
          )
        ),
        
        # =================================================
        # FILA 2
        # =================================================
        
        fluidRow(
          
          box(
            
            width = 12,
            
            title = "Tabla Resumen",
            
            status = "success",
            
            solidHeader = TRUE,
            
            dataTableOutput("table_data")
          )
        )
      )
    )
  )
)

# =========================================================
# SERVER
# =========================================================

server <- function(input, output) {
  
  # =======================================================
  # DATOS FILTRADOS
  # =======================================================
  
  filtered_data <- reactive({
    
    data %>%
      
      filter(
        continent == input$continent,
        year == input$year
      )
  })
  
  # =======================================================
  # VALUE BOX 1
  # =======================================================
  
  output$box1 <- renderValueBox({
    
    valueBox(
      
      value = input$year,
      
      subtitle = "Año Analizado",
      
      icon = icon("calendar"),
      
      color = "light-blue"
    )
  })
  
  # =======================================================
  # VALUE BOX 2
  # =======================================================
  
  output$box2 <- renderValueBox({
    
    vida <- round(
      mean(filtered_data()$lifeExp),
      0
    )
    
    valueBox(
      
      paste0(vida, " años"),
      
      "Esperanza de Vida",
      
      icon = icon("heartbeat"),
      
      color = "green"
    )
  })
  
  # =======================================================
  # VALUE BOX 3
  # =======================================================
  
  output$box3 <- renderValueBox({
    
    pib <- round(
      mean(filtered_data()$gdpPercap),
      0
    )
    
    valueBox(
      
      paste0("$", comma(pib)),
      
      "PIB per cápita",
      
      icon = icon("dollar-sign"),
      
      color = "yellow"
    )
  })
  
  # =======================================================
  # VALUE BOX 4
  # =======================================================
  
  output$box4 <- renderValueBox({
    
    poblacion <- round(
      sum(filtered_data()$pop_millions),
      0
    )
    
    valueBox(
      
      paste0(comma(poblacion), " M"),
      
      "Población Total",
      
      icon = icon("users"),
      
      color = "purple"
    )
  })
  
  # =======================================================
  # SCATTER ECONÓMICO
  # =======================================================
  
  output$plot_scatter <- renderPlotly({
    
    gg <- ggplot(
      
      filtered_data(),
      
      aes(
        x = gdpPercap,
        y = lifeExp,
        size = pop_millions,
        color = continent,
        
        text = paste(
          "País:", country,
          "<br>PIB per cápita:", round(gdpPercap,0),
          "<br>Esperanza de Vida:", round(lifeExp,1),
          "<br>Población:", round(pop_millions,1)," M"
        )
      )
    ) +
      
      geom_point(alpha = 0.75) +
      
      scale_color_manual(values = c(
        "#F4A9A8",
        "#A0C4FF",
        "#CDB4DB",
        "#B8F2E6",
        "#FFD6A5"
      )) +
      
      labs(
        x = "PIB per cápita",
        y = "Esperanza de Vida"
      ) +
      
      theme_minimal(base_size = 14) +
      
      theme(
        
        plot.background = element_rect(
          fill = "white",
          color = NA
        ),
        
        panel.background = element_rect(
          fill = "white"
        ),
        
        legend.position = "right"
      )
    
    # ESCALA LOGARÍTMICA
    
    if(input$log_scale){
      
      gg <- gg + scale_x_log10()
    }
    
    ggplotly(gg, tooltip = "text")
  })
  
  # =======================================================
  # GRÁFICO DE LÍNEA
  # =======================================================
  
  output$plot_line <- renderPlotly({
    
    plot_data <- data %>%
      
      filter(continent == input$continent) %>%
      
      group_by(year) %>%
      
      summarise(
        lifeExp = mean(lifeExp)
      )
    
    gg <- ggplot(
      
      plot_data,
      
      aes(
        x = year,
        y = lifeExp,
        
        text = paste(
          "Año:", year,
          "<br>Esperanza de Vida:",
          round(lifeExp,1)
        )
      )
    ) +
      
      geom_line(
        color = "#A0C4FF",
        size = 2
      ) +
      
      geom_point(
        color = "#FFAFCC",
        size = 3
      ) +
      
      labs(
        x = "Año",
        y = "Esperanza de Vida"
      ) +
      
      theme_minimal(base_size = 14) +
      
      theme(
        
        plot.background = element_rect(
          fill = "white",
          color = NA
        ),
        
        panel.background = element_rect(
          fill = "white"
        )
      )
    
    ggplotly(gg, tooltip = "text")
  })
  
  # =======================================================
  # TABLA RESUMEN
  # =======================================================
  
  output$table_data <- renderDataTable({
    
    filtered_data() %>%
      
      select(
        country,
        continent,
        gdpPercap,
        lifeExp,
        pop_millions
      ) %>%
      
      arrange(desc(gdpPercap))
  })
}

# =========================================================
# EJECUTAR APP
# =========================================================

shinyApp(ui, server)