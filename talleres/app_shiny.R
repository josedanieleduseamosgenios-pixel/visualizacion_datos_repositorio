# =====================================================================
# US States by the Year of Joining the Union -- version profesional (Shiny)
# Dashboard interactivo con datos REALES (Censo de EE.UU. 2020).
#
# Como correrlo:
#   install.packages(c("shiny", "bslib", "plotly", "dplyr", "DT"))
#   shiny::runApp("app_shiny.R")
#
# Estructura del archivo (para que sea facil de seguir):
#   1. Librerias
#   2. Datos (constantes + construccion del data frame)
#   3. Paleta de colores y etiquetas
#   4. Funciones para construir cada grafico
#   5. Interfaz de usuario (UI)
#   6. Logica del servidor (server)
#   7. Lanzar la app
# =====================================================================


# ---------------------------------------------------------------------
# 1. Librerias
# ---------------------------------------------------------------------
library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(DT)


# ---------------------------------------------------------------------
# 2. Datos reales (Censo de EE.UU. 2020)
#    Poblacion, superficie y reparto de escanos: Censo 2020 / apportionment
#    2023-2033 (435 escanos en total). Ano de ingreso a la Union: hecho
#    historico.
# ---------------------------------------------------------------------
states_raw <- data.frame(
  state = c(
    "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
    "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho",
    "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
    "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
    "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",
    "New Hampshire", "New Jersey", "New Mexico", "New York",
    "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon",
    "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
    "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
    "West Virginia", "Wisconsin", "Wyoming"
  ),
  abbr = c(
    "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID",
    "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS",
    "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK",
    "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV",
    "WI", "WY"
  ),
  statehood_year = c(
    1819, 1959, 1912, 1836, 1850, 1876, 1788, 1787, 1845, 1788, 1959, 1890,
    1818, 1816, 1846, 1861, 1792, 1812, 1820, 1788, 1788, 1837, 1858, 1817,
    1821, 1889, 1867, 1864, 1788, 1787, 1912, 1788, 1789, 1889, 1803, 1907,
    1859, 1787, 1790, 1788, 1889, 1796, 1845, 1896, 1791, 1788, 1889, 1863,
    1848, 1890
  ),
  population = c(
    5024279, 733391, 7151502, 3011524, 39538223, 5773714, 3605944, 989948,
    21538187, 10711908, 1455271, 1839106, 12812508, 6785528, 3190369,
    2937880, 4505836, 4657757, 1362359, 6177224, 7029917, 10077331,
    5706494, 2961279, 6154913, 1084225, 1961504, 3104614, 1377529,
    9288994, 2117522, 20201249, 10439388, 779094, 11799448, 3959353,
    4237256, 13002700, 1097379, 5118425, 886667, 6910840, 29145505,
    3271616, 643077, 8631393, 7705281, 1793716, 5893718, 576851
  ),
  land_sqmi = c(
    50645, 570641, 113594, 52035, 155779, 103642, 4842, 1949, 53625, 57513,
    6423, 82643, 55519, 35826, 55857, 81759, 39486, 43204, 30843, 9707,
    7800, 56539, 79627, 46923, 68742, 145546, 76824, 109781, 8953, 7354,
    121298, 47126, 48618, 69001, 40861, 68595, 95988, 44743, 1034, 30061,
    75811, 41235, 261232, 82170, 9217, 39490, 66456, 24038, 54158, 97093
  ),
  water_sqmi = c(
    1775, 94743, 396, 1143, 7916, 452, 701, 540, 12133, 1912, 4509, 926,
    2395, 593, 416, 520, 921, 9174, 4537, 2699, 2754, 40175, 7309, 1508,
    965, 1494, 524, 791, 397, 1368, 292, 7429, 5201, 1698, 3965, 1304,
    2391, 1312, 511, 1960, 1305, 909, 7365, 2727, 400, 3285, 4842, 192,
    11339, 720
  ),
  house_seats = c(
    7, 1, 9, 4, 52, 8, 5, 1, 28, 14, 2, 2, 17, 9, 4, 4, 6, 6, 2, 8, 9, 13,
    8, 4, 8, 2, 3, 4, 2, 12, 3, 26, 14, 1, 15, 5, 6, 17, 2, 7, 1, 9, 38, 4,
    1, 11, 10, 2, 8, 1
  ),
  stringsAsFactors = FALSE
)

# Regiones oficiales de la Oficina del Censo de EE.UU. -- ayudan a agrupar
# y a entender el mapa ademas de la linea de tiempo de ingreso a la Union.
region_lookup <- list(
  "Noreste"     = c("CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA"),
  "Medio Oeste" = c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD"),
  "Sur"         = c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "WV", "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX"),
  "Oeste"       = c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", "AK", "CA", "HI", "OR", "WA")
)
abbr_to_region <- setNames(
  rep(names(region_lookup), lengths(region_lookup)),
  unlist(region_lookup)
)

DATA_SOURCE_NOTE <- paste(
  "Fuente: U.S. Census Bureau (Censo 2020) para poblacion, area y reparto",
  "de escanos 2023-2033. Ano de ingreso a la Union: hecho historico."
)


# ---------------------------------------------------------------------
# 3. Paleta de colores y etiquetas
# ---------------------------------------------------------------------

# Bins de epoca de ingreso a la Union (para el coloreado categorico del mapa)
PERIOD_BREAKS <- c(1786, 1790, 1820, 1850, 1875, 1900, 1960)
PERIOD_LABELS <- c(
  "Antes de 1790", "1790-1820", "1820-1850",
  "1850-1875", "1875-1900", "Despues de 1900"
)

# Paleta calida y secuencial: claro = ingreso temprano, oscuro = ingreso tardio
PERIOD_COLORS <- c("#FFF3B0", "#FFD166", "#F4A261", "#E76F51", "#C1121F", "#6A040F")

# Colores para el modo "colorear por region"
REGION_COLORS <- c(
  "Noreste" = "#118AB2", "Medio Oeste" = "#06A77D",
  "Sur" = "#E76F51", "Oeste" = "#7B2CBF"
)

HIGHLIGHT_COLOR   <- "#118AB2"  # borde de estados seleccionados (contrasta con la paleta calida)
ACCENT_COLOR      <- "#118AB2"  # color de acento en donas y barras
CONTINUOUS_SCALE  <- "Viridis"  # escala perceptualmente uniforme y apta para daltonismo

METRIC_LABELS <- c(population = "Poblacion", area_sqmi = "Superficie", house_seats = "Escanos")
METRIC_UNITS  <- c(population = "hab.", area_sqmi = "mi2", house_seats = "escanos")


# ---------------------------------------------------------------------
# Construccion final del data frame (deriva columnas a partir de los datos
# crudos de la seccion 2, usando la paleta y las regiones de la seccion 3)
# ---------------------------------------------------------------------
states_data <- states_raw %>%
  mutate(
    area_sqmi = land_sqmi + water_sqmi,
    water_pct = round(water_sqmi / area_sqmi * 100, 1),
    region    = abbr_to_region[abbr],
    period    = cut(statehood_year, breaks = PERIOD_BREAKS, labels = PERIOD_LABELS, right = TRUE)
  ) %>%
  select(state, abbr, statehood_year, population, area_sqmi, water_pct, house_seats, region, period)

TOTAL_POPULATION <- sum(states_data$population)
TOTAL_AREA       <- sum(states_data$area_sqmi)
TOTAL_SEATS      <- sum(states_data$house_seats)
REGION_CHOICES   <- sort(unique(states_data$region))

# Totales nacionales indexados por nombre de columna (usado en build_comparison)
TOTALS <- c(population = TOTAL_POPULATION, area_sqmi = TOTAL_AREA, house_seats = TOTAL_SEATS)


# ---------------------------------------------------------------------
# 4. Funciones para construir cada grafico
# ---------------------------------------------------------------------

#' Mapa coroplético de EE.UU., coloreado por epoca / region / una metrica.
build_map <- function(df, selected_abbrs, color_by = "period") {
  hover_text <- paste0(
    df$state, "<br>Ingreso: ", df$statehood_year,
    "<br>Poblacion: ", format(df$population, big.mark = ","),
    "<br>Superficie: ", format(df$area_sqmi, big.mark = ","), " mi2",
    "<br>Escanos: ", df$house_seats,
    "<br>Region: ", df$region
  )
  df$hover_text <- hover_text

  p <- plot_geo(locationmode = "USA-states")

  if (color_by == "period") {
    for (i in seq_along(PERIOD_LABELS)) {
      sub <- filter(df, period == PERIOD_LABELS[i])
      if (nrow(sub) == 0) next
      p <- p %>% add_trace(
        data = sub, z = 1, locations = ~abbr, text = ~hover_text,
        colors = colorRamp(c(PERIOD_COLORS[i], PERIOD_COLORS[i])),
        showscale = FALSE, name = PERIOD_LABELS[i],
        marker = list(line = list(color = "white", width = 0.6)),
        hovertemplate = "%{text}<extra></extra>"
      )
    }
  } else if (color_by == "region") {
    for (region_name in names(REGION_COLORS)) {
      sub <- filter(df, region == region_name)
      if (nrow(sub) == 0) next
      p <- p %>% add_trace(
        data = sub, z = 1, locations = ~abbr, text = ~hover_text,
        colors = colorRamp(c(REGION_COLORS[[region_name]], REGION_COLORS[[region_name]])),
        showscale = FALSE, name = region_name,
        marker = list(line = list(color = "white", width = 0.6)),
        hovertemplate = "%{text}<extra></extra>"
      )
    }
  } else {
    p <- p %>% add_trace(
      data = df, z = df[[color_by]], locations = ~abbr, text = ~hover_text,
      colorscale = CONTINUOUS_SCALE, showscale = TRUE, name = METRIC_LABELS[[color_by]],
      marker = list(line = list(color = "white", width = 0.6)),
      hovertemplate = "%{text}<extra></extra>"
    )
  }

  if (length(selected_abbrs) > 0) {
    sel <- filter(df, abbr %in% selected_abbrs)
    p <- p %>% add_trace(
      data = sel, z = 1, locations = ~abbr, text = ~state,
      # "colorscale" (a diferencia de "colors"/colorRamp) se pasa directo a
      # plotly.js sin pasar por la validacion de colores de R, asi que aqui
      # SI se puede usar la sintaxis rgba() para un relleno transparente.
      colorscale = list(list(0, "rgba(0,0,0,0)"), list(1, "rgba(0,0,0,0)")),
      showscale = FALSE, name = "Seleccionado",
      marker = list(line = list(color = HIGHLIGHT_COLOR, width = 4)),
      hovertemplate = "%{text} (seleccionado)<extra></extra>"
    )
  }

  p %>% layout(
    geo = list(scope = "usa", bgcolor = "rgba(0,0,0,0)"),
    paper_bgcolor = "rgba(0,0,0,0)",
    legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.12),
    transition = list(duration = 400)
  )
}

#' Dona que muestra el % de un total representado por los estados seleccionados.
build_donut <- function(value, total, label, color = ACCENT_COLOR) {
  pct <- if (total == 0) 0 else round(100 * value / total, 2)
  plot_ly(
    values = c(pct, 100 - pct), labels = c(label, ""),
    marker = list(colors = c(color, "#eef1f4")),
    type = "pie", hole = 0.78, textinfo = "none", hoverinfo = "skip", sort = FALSE
  ) %>%
    layout(
      showlegend = FALSE, paper_bgcolor = "rgba(0,0,0,0)",
      title = list(text = label, font = list(size = 13, color = "#5b6b76")),
      annotations = list(text = paste0(pct, "%"), x = 0.5, y = 0.5, font = list(size = 22), showarrow = FALSE),
      margin = list(l = 0, r = 0, t = 40, b = 0)
    )
}

#' Ranking horizontal de los top/bottom N estados segun una metrica.
build_ranking <- function(df, metric, top_n = 10, ascending = FALSE) {
  ordered <- df %>% arrange(if (ascending) .data[[metric]] else desc(.data[[metric]]))
  subset  <- head(ordered, top_n) %>% arrange(.data[[metric]])
  order_word <- if (ascending) "menor" else "mayor"

  plot_ly(
    data = subset, x = ~.data[[metric]], y = ~reorder(state, .data[[metric]]),
    type = "bar", orientation = "h", marker = list(color = ACCENT_COLOR),
    text = ~format(.data[[metric]], big.mark = ","), textposition = "outside"
  ) %>%
    layout(
      title = list(
        text = paste(top_n, "estados con", order_word, tolower(METRIC_LABELS[[metric]])),
        font = list(size = 14, color = "#5b6b76")
      ),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      yaxis = list(title = ""), xaxis = list(title = "", showticklabels = FALSE)
    )
}

#' Barras agrupadas comparando exactamente 2 estados (como % del total nacional).
build_comparison <- function(df, abbrs) {
  sel     <- df %>% filter(abbr %in% abbrs)
  metrics <- c("population", "area_sqmi", "house_seats")
  colors  <- c(HIGHLIGHT_COLOR, "#E76F51")

  p <- plot_ly()
  for (i in seq_along(abbrs)) {
    row <- sel %>% filter(abbr == abbrs[i])
    pct_values  <- sapply(metrics, function(m) 100 * row[[m]] / TOTALS[[m]])
    text_values <- sapply(metrics, function(m) paste(format(row[[m]], big.mark = ","), METRIC_UNITS[[m]]))
    p <- p %>% add_trace(
      x = METRIC_LABELS[metrics], y = pct_values, type = "bar", name = row$state,
      marker = list(color = colors[((i - 1) %% 2) + 1]),
      text = text_values, textposition = "outside"
    )
  }
  p %>% layout(
    barmode = "group",
    title = list(text = "Comparacion directa (% del total nacional)", font = list(size = 14, color = "#5b6b76")),
    yaxis = list(title = "% del total nacional"),
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    legend = list(orientation = "h", y = -0.15)
  )
}


# ---------------------------------------------------------------------
# 5. Interfaz de usuario (UI)
# ---------------------------------------------------------------------
help_text <- tagList(
  p("Este panel muestra los 50 estados de EE.UU. segun el ano en que se unieron
     a la Union, junto con su poblacion, superficie y representacion en el
     Congreso (todo con datos reales del Censo 2020)."),
  tags$ul(
    tags$li("Haz clic en un estado del mapa para seleccionarlo (puedes elegir varios)."),
    tags$li("Usa el buscador para encontrar un estado por nombre."),
    tags$li("Cambia el color del mapa: por epoca de ingreso, region o una metrica numerica."),
    tags$li("El deslizador filtra el rango de anos de ingreso a la Union."),
    tags$li("Si seleccionas exactamente 2 estados, aparece una comparacion lado a lado."),
    tags$li("El boton CSV descarga la tabla completa de datos.")
  ),
  tags$p(DATA_SOURCE_NOTE, style = "font-size:11px; color:#888;")
)

map_panel <- card(
  card_body(
    div(
      style = "text-align:center;",
      h2("US States", style = "margin-bottom:0; color:#3d4f5c; display:inline-block;"),
      actionLink("help_btn", "  Como se usa?", style = "margin-left:8px; font-size:14px;")
    ),
    h4("by the Year of Joining the Union", style = "text-align:center; color:#7a8a99;"),
    p("Datos reales del Censo de EE.UU. (2020) -- haz clic en un estado, buscalo o filtra por region o ano.",
      style = "text-align:center; font-size:13px; color:#7a8a99;"),

    layout_columns(
      col_widths = c(5, 3, 2, 2),
      textInput("search_box", NULL, placeholder = "Buscar estado..."),
      selectizeInput("region_filter", NULL, choices = REGION_CHOICES, multiple = TRUE,
                     options = list(placeholder = "Filtrar por region...")),
      actionButton("clear_btn", "Limpiar", class = "btn-outline-secondary w-100"),
      downloadButton("download_data", "CSV", class = "btn-outline-primary w-100")
    ),

    layout_columns(
      col_widths = c(8, 4),
      sliderInput("year_range", "Rango de anos de ingreso a la Union:",
                  min = min(states_data$statehood_year), max = max(states_data$statehood_year),
                  value = range(states_data$statehood_year), sep = ""),
      selectInput("map_metric", "Colorear mapa por:",
                  choices = c("Epoca de ingreso" = "period", "Region" = "region",
                              "Poblacion" = "population", "Superficie" = "area_sqmi",
                              "Escanos" = "house_seats"))
    ),

    plotlyOutput("us_map", height = "420px"),
    tags$p(DATA_SOURCE_NOTE, style = "font-size:10px; color:#999;")
  )
)

detail_panel <- card(
  card_body(
    h4("Porcentaje del total (estados seleccionados)", style = "text-align:center; color:#7a8a99;"),
    p("Que parte del total nacional representan los estados que elegiste.",
      style = "text-align:center; font-size:11px; color:#7a8a99;"),
    layout_columns(
      col_widths = c(4, 4, 4),
      plotlyOutput("donut_pop", height = "180px"),
      plotlyOutput("donut_area", height = "180px"),
      plotlyOutput("donut_seats", height = "180px")
    ),

    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(title = "Estados seleccionados", value = textOutput("kpi_n"), theme = "primary"),
      value_box(title = "Poblacion conjunta", value = textOutput("kpi_pop"), theme = "info"),
      value_box(title = "Escanos conjuntos", value = textOutput("kpi_seats"), theme = "secondary")
    ),

    uiOutput("comparison_section"),

    h4("Estados seleccionados", style = "text-align:center; margin-top:16px; color:#7a8a99;"),
    DTOutput("states_table"),

    h4("Ranking", style = "text-align:center; margin-top:20px; color:#7a8a99;"),
    layout_columns(
      col_widths = c(8, 4),
      radioButtons("ranking_metric", NULL,
                   choices = c("Poblacion" = "population", "Superficie" = "area_sqmi", "Escanos" = "house_seats"),
                   selected = "population", inline = TRUE),
      radioButtons("ranking_direction", NULL,
                   choices = c("Top 10 (mayor)" = "desc", "Ultimos 10 (menor)" = "asc"),
                   selected = "desc", inline = TRUE)
    ),
    plotlyOutput("ranking_chart", height = "340px")
  )
)

# page_fluid (con scroll normal) en vez de page_fillable: page_fillable
# fuerza TODO el contenido a caber en una sola pantalla sin scroll, y con
# el mapa + donas + tabla + ranking eso hacia que los elementos se
# encimaran unos sobre otros.
ui <- page_fluid(
  theme = bs_theme(bootswatch = "flatly"),
  tags$style(HTML("h3, h4 { color: #3d4f5c; } .muted { color: #7a8a99; font-size: 13px; } body { padding: 16px; }")),
  layout_columns(col_widths = c(6, 6), map_panel, detail_panel)
)


# ---------------------------------------------------------------------
# 6. Logica del servidor (server)
# ---------------------------------------------------------------------
server <- function(input, output, session) {

  # --- Estado reactivo: abreviaturas de los estados seleccionados ---
  selected_abbrs <- reactiveVal(c("VA"))

  observeEvent(input$help_btn, {
    showModal(modalDialog(
      title = "Como se usa este mapa?",
      help_text, easyClose = TRUE, size = "l", footer = modalButton("Cerrar")
    ))
  })

  observeEvent(event_data("plotly_click"), {
    click <- event_data("plotly_click")
    if (!is.null(click$location)) {
      current <- selected_abbrs()
      if (click$location %in% current) {
        selected_abbrs(setdiff(current, click$location))
      } else {
        selected_abbrs(union(current, click$location))
      }
    }
  })

  observeEvent(input$search_box, {
    if (nzchar(input$search_box)) {
      match <- states_data %>% filter(grepl(input$search_box, state, ignore.case = TRUE))
      selected_abbrs(match$abbr)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$region_filter, {
    if (length(input$region_filter) > 0) {
      match <- states_data %>% filter(region %in% input$region_filter)
      selected_abbrs(match$abbr)
    }
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  observeEvent(input$clear_btn, {
    selected_abbrs(character(0))
  })

  # --- Datos derivados de los filtros activos ---
  filtered_data <- reactive({
    states_data %>%
      filter(statehood_year >= input$year_range[1], statehood_year <= input$year_range[2])
  })

  selected_data <- reactive({
    filtered_data() %>% filter(abbr %in% selected_abbrs())
  })

  # --- Mapa principal ---
  output$us_map <- renderPlotly({
    build_map(states_data, selected_data()$abbr, color_by = input$map_metric)
  })

  # --- Donas de porcentaje del total ---
  output$donut_pop   <- renderPlotly({ build_donut(sum(selected_data()$population), TOTAL_POPULATION, "Poblacion") })
  output$donut_area  <- renderPlotly({ build_donut(sum(selected_data()$area_sqmi), TOTAL_AREA, "Superficie") })
  output$donut_seats <- renderPlotly({ build_donut(sum(selected_data()$house_seats), TOTAL_SEATS, "Escanos") })

  # --- Tarjetas KPI ---
  output$kpi_n     <- renderText({ nrow(selected_data()) })
  output$kpi_pop   <- renderText({ format(sum(selected_data()$population), big.mark = ",") })
  output$kpi_seats <- renderText({ sum(selected_data()$house_seats) })

  # --- Comparacion directa (solo cuando hay exactamente 2 estados) ---
  output$comparison_section <- renderUI({
    n <- nrow(selected_data())
    if (n == 2) {
      tagList(
        h4("Comparacion de los 2 estados seleccionados",
           style = "text-align:center; margin-top:12px; color:#7a8a99;"),
        plotlyOutput("comparison_chart", height = "320px")
      )
    } else if (n > 2) {
      div("Selecciona exactamente 2 estados para ver una comparacion directa entre ellos.",
          style = "text-align:center; font-size:12px; color:#7a8a99; margin-top:8px;")
    } else {
      NULL
    }
  })

  output$comparison_chart <- renderPlotly({
    req(nrow(selected_data()) == 2)
    build_comparison(states_data, selected_data()$abbr)
  })

  # --- Tabla de estados seleccionados ---
  output$states_table <- renderDT({
    selected_data() %>%
      select(Nombre = state, Ano = statehood_year, Poblacion = population,
             `Area (mi2)` = area_sqmi, `% Agua` = water_pct, Escanos = house_seats, Region = region)
  }, options = list(pageLength = 8), rownames = FALSE)

  # --- Ranking ---
  output$ranking_chart <- renderPlotly({
    build_ranking(states_data, input$ranking_metric, ascending = (input$ranking_direction == "asc"))
  })

  # --- Descarga de datos ---
  output$download_data <- downloadHandler(
    filename = function() "us_states_data.csv",
    content = function(file) write.csv(states_data, file, row.names = FALSE)
  )
}


# ---------------------------------------------------------------------
# 7. Lanzar la app
# ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)
