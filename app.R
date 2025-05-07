# biblioteki
library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(httr)
library(jsonlite)
library(stringr)
library(lubridate)
library(shinyjs)
library(shinyalert)
library(ggplot2)
library(data.table)
library(maps)
library(shinytoastr)
library(plotly)

choices_list = list()

#ekstrema odczytów
extremes <- c(
  pm25 = 1000,
  pm10 = 1500,
  co = 50000,
  no2 = 1000,
  so2 = 1500,
  o3 = 1000
)

ui <- page_sidebar(
  
  theme = bs_theme(preset = "journal"),
  title = 'AirWow',
  window_title = 'AirWow',
  
  # pasek użytkownika
  sidebar = sidebar(
    
    title = tagList(
      tags$h4(tags$b('PANEL UŻYTKOWNIKA')), 
      tags$hr()  
    ),
    
    useShinyjs(),
    
    passwordInput("access_key", tags$b("Wpisz swój klucz API:")),
    
    actionButton("confirm_key", "Zatwierdź", class = "btn-primary", style = "background-color: grey; color: white; border: none;"),
    
    actionButton("info_btn", "Jak uzyskać swój klucz API?", style = "background-color: transparent; border: none; color: orange; font-size: 16px;"),
    
    selectInput(
      inputId = 'multi_select',
      label = tags$b('LOKALIZACJE (max 5)'),
      choices = choices_list,
      multiple = T,
      selectize = T
    ),
    selectInput(
      inputId = 'dynamic_select',
      label = tags$b('TYP ANALIZY'),
      choices = c('Porównanie'),
      selected = 'Porównanie' 
    ),
    radioButtons(
      inputId = 'type_of_pollution',
      label = tags$b('RODZAJ ZANIECZYSZCZENIA'),
      choices = setNames(
        c('pm25', 'pm10', 'o3', 'co', 'no2', 'so2'),
        c('PM2.5', 'PM10', 'O₃', 'CO', 'NO₂', 'SO₂')
      ),
      selected = 'pm25'
    ),
    radioButtons(
      inputId = 'time_range',
      label = tags$b('ZAKRES CZASOWY'),
      choices = c('Rok', 'Miesiąc', 'Tydzień'),
      selected = 'Miesiąc'
    )
  ),
  div(
    
    useShinyjs(), 
    useToastr(), 
    
    # informacja o niewybraniu żadnej lokalizacji
    div(
      
      id = "no_selection_msg",
      style = "position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); 
             background-color: #fff3cd; color: #721c24; padding: 20px; border-radius: 10px; 
             box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1); z-index: 9999; display: none;",
      "Proszę wybrać lokalizację"
    ),
    
    # tryb porównywania lokalizacji
    conditionalPanel(
      
      condition = "input.dynamic_select == 'Porównanie'",
      
      card(
        tabsetPanel(
          
          type = "tabs",
          
          tabPanel("Analiza w czasie",
                   tags$br(),
                   tags$h4('Aktualne wartości odczytane z sensorów'),
                   card(tableOutput(outputId = 'live_values_dt'), height = '100%'),
                   card(plotlyOutput(outputId = 'comparision_plot'), height = '300%')
          ),
          
          tabPanel("Analiza wartości",
                   tags$br(),
                   tags$h4('Aktualne wartości odczytane z sensorów'),
                   card(tableOutput(outputId = 'live_values_dt_v2'), height = '100%'),
                   card(plotlyOutput(outputId = 'comparision_boxplot'), height = '300%')
          )
        ),
        
        height = '100%'  
      )
    ),
    
    # tryb przeglądu konretnej lokalizacji
    conditionalPanel(
      
      condition = "input.dynamic_select != 'Porównanie'",
      
      div(
        style = "display: flex; flex-direction: column; height: 100%;",
        
        div(
          style = "display: flex; height: 50%;",
          
          div(
            style = "width: 66%; height: 100%;",
            tags$br(),
            tags$h4('Podsumowanie'),
            card(tableOutput(outputId = 'stat_dt'), height = '80%')
          ),
          
          div(
            style = "width: 33%; height: 100%;",
            plotOutput(outputId = 'map')
          )
        ),
        
        div(
          style = "height: 50%;",
          card(plotlyOutput(outputId = 'norms_plot'), height = '150%')
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  entry_data <- reactiveVal()
  
  shinyjs::disable("confirm_key")
  shinyjs::disable("multi_select")
  shinyjs::disable("type_of_pollution")
  shinyjs::disable("time_range")
  shinyjs::disable("dynamic_select")
  
  # Sprawdzanie długości klucza i zmiana koloru przycisku
  observeEvent(input$access_key, {
    if (nchar(input$access_key) == 64) {
      shinyjs::enable("confirm_key")
      runjs("$('#confirm_key').css('background-color', 'blue').css('color', 'white');")
    } else {
      shinyjs::disable("confirm_key")
      runjs("$('#confirm_key').css('background-color', 'grey').css('color', 'white');")
    }
    updateActionButton(session, "confirm_key", label = "Zatwierdź")
    shinyjs::disable("multi_select")
    shinyjs::disable("type_of_pollution")
    shinyjs::disable("time_range")
    shinyjs::disable("dynamic_select")
  })
  
  observeEvent(input$info_btn, {
    showModal(modalDialog(
      title = "Jak uzyskać klucz API?",
      HTML("Swój klucz API, składający się z 64 znaków, możesz wygenerować zakładając konto na <a href='https://explore.openaq.org/register' target='_blank'>OpenAQ</a>."),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$confirm_key, {
    
    api_key = as.character(input$access_key)
    
    url_loc <- "https://api.openaq.org/v3/locations?limit=1000&page=1&order_by=id&sort_order=asc&mobile=false&countries_id=77"
    
    response <- GET(
      url_loc,
      add_headers(`X-API-Key` = api_key)  
    )
    
    if (status_code(response) == 200) {
      
      runjs("$('#confirm_key').css('background-color', 'green').css('color', 'white');")
      updateActionButton(session, "confirm_key", label = "Klucz poprawny")
      showNotification("Klucz poprawny!", type = "message", duration = 3)
      
      # Odblokowanie reszty sidebaru po poprawnym kluczu
      shinyjs::enable("multi_select")
      shinyjs::enable("type_of_pollution")
      shinyjs::enable("time_range")
      shinyjs::enable("dynamic_select")
      
      data <- content(response, as = "text", encoding = "UTF-8")
      locations <- fromJSON(data)$results
      # obrabianie pobranych danych
      
      # wybór lokalizacji
      locations <- locations %>% 
        filter(!is.na(locality))
      
      locations <- locations %>% 
        unnest(cols = c(country, provider, datetimeLast, coordinates, datetimeFirst), names_sep = "_")
      
      locations <- locations %>% 
        filter(timezone == 'Europe/Warsaw')
      
      locations$datetimeLast_local <- as.Date(ymd_hms(locations$datetimeLast_local, tz = "UTC", quiet = TRUE))
      
      locations$datetimeFirst_local <- as.Date(ymd_hms(locations$datetimeFirst_local, tz = "UTC", quiet = TRUE))
      
      # wyfiltrowanie tylko tych z aktualnymi odczytami
      locations <- locations %>% 
        filter(datetimeLast_local > with_tz(Sys.time(), tzone = "UTC") - 24*60*60)
      
      locations$name <- gsub(' ul. ', ', ul. ', locations$name)
      locations$name <- gsub(' Aleja ', ', Aleja ', locations$name)
      locations$name <- gsub(',,', ',', locations$name)
      
      # jeśli jest więcej niż jedna lokalizacja w danym mieście - dodanie ulicy
      locations$name_v2 <- sapply(strsplit(locations$name, ","), function(x) trimws(x[1]))
      
      locations$name_v3 <- ifelse(duplicated(locations$name_v2) | duplicated(locations$name_v2, fromLast = TRUE), 1, 0)
      
      locations$name <- ifelse(locations$name_v3 == 1, 
                               paste(locations$name_v2, " (", sapply(strsplit(locations$name, ","), function(x) trimws(x[2])), ")", sep=""), 
                               locations$name_v2)
      
      # usuwanie niepotrzbnych kolumn
      locations <- locations[,-c(24,25)]
      
      locations <- locations %>% 
        unnest(cols = c(sensors), names_sep = "_")
      locations <- locations %>% 
        unnest(cols = sensors_parameter, names_sep = "_")
      
      locations <- locations[,c(1,2,14,17,20,21,26,28)]
      
      locations <- locations %>% rename(locations_id = id)
      
      locations <- locations[locations$sensors_parameter_name != 'bc',]
      
      entry_data0 <- locations
      
      # dodanie potrzebnych kolumn
      entry_data0$value <- NA
      entry_data0$last_update <- NA
      
      entry_data0 <- entry_data0 %>%
        rename(Lokalizacja = name)
      
      entry_data(entry_data0)
      
      # dostepne zanieczyszczenia
      choices_list <- sort(unique(locations$name))
      updateSelectInput(
        session,
        "multi_select",
        choices = choices_list
      )
    } else {
      runjs("$('#confirm_key').css('background-color', 'red').css('color', 'white');")
      updateActionButton(session, "confirm_key", label = "Klucz niepoprawny")
      showNotification("Błąd! Klucz jest niepoprawny.", type = "error", duration = 3)
      
      
      # Zablokowanie reszty sidebaru w przypadku błędu
      shinyjs::disable("multi_select")
      shinyjs::disable("type_of_pollution")
      shinyjs::disable("time_range")
      shinyjs::disable("dynamic_select")
      
      cat("Błąd w zapytaniu: ", status_code(response), "\n")
      cat("Treść odpowiedzi: ", content(response, as = "text"), "\n")
    }
  })
  
  observe({
    
    if (length(input$multi_select) < 1) {
      # informacja o braku wybranych lokalizacji
      shinyjs::show("no_selection_msg")
      
      # akutalizacja opcji w wyborzu analizy
      updateSelectInput(
        session,
        "dynamic_select",
        choices = c('Porównanie')
      )
    } else {
      
      # usunięcie informacji o braku wybranych lokalizacji
      shinyjs::hide("no_selection_msg") 
      # akutalizacja opcji w wyborzu analizy
      updateSelectInput(
        session,
        "dynamic_select",
        choices = c('Porównanie', paste('Analiza lokacji:', input$multi_select))
      )
    }
    
    if (length(input$multi_select) > 5) {
      # informacja o przkeroczeniu maksymalnej ilości lokalizacji
      toastr_error("Możesz wybrać maksymalnie 5 lokalizacji.", position = "top-right")
      
      # akutalizacja opcji w wyborzu analizy
      updateSelectizeInput(session, "multi_select", selected = head(input$multi_select, 5))
      return(NULL)
    }
  })
  
  # pobranie aktualnych odczytów
  reactive_data_live <- reactive({
    
    req(input$multi_select)
    req(entry_data)
    entry_data <- entry_data()
    req(input$access_key)
    api_key = as.character(input$access_key)
    
    
    df <- entry_data[entry_data$Lokalizacja %in% input$multi_select,]
    choosen_ids <- unique(df$locations_id)
    
    for (i in choosen_ids) {
      url <- paste0("https://api.openaq.org/v3/locations/", i, "/latest?limit=20&page=1")
      response <- GET(
        url,
        add_headers(`X-API-Key` = api_key) 
      )
      
      if (status_code(response) == 200) {
        data <- content(response, as = "text", encoding = "UTF-8")
        parsed_data <- fromJSON(data)$results
      } else {
        cat("Błąd w zapytaniu: ", status_code(response), "\n")
        cat("Treść odpowiedzi: ", content(response, as = "text"), "\n")
      }
      
      if (length(parsed_data) == 0) {
        next
      }
      
      latest <- parsed_data %>% 
        unnest(cols = c(datetime, coordinates), names_sep = "_")
      latest$value <- ifelse(ymd_hms(latest$datetime_local, tz = "UTC", quiet = TRUE) > with_tz(Sys.time(), tzone = "UTC") - 24*60*60, latest$value, NA)
      latest <- latest %>% rename(last_update_new = datetime_local,
                                  sensors_id = sensorsId,
                                  locations_id = locationsId,
                                  value_new = value)
      df <- merge(df, latest[, c("sensors_id", "value_new", "last_update_new")], by = "sensors_id", all.x = TRUE)
      df$value <- ifelse(is.na(df$value), df$value_new, df$value)
      df$last_update <- ifelse(is.na(df$last_update), df$last_update_new, df$last_update) 
      df <- df[,1:10]
      
    }
    
    return(df)
  }) 
  
  # pobranie danych z wybranego ostatniego okresu czasowego (wartości są średnią dzienną)
  reactive_data_historic <- reactive({
    
    req(input$multi_select)
    req(input$type_of_pollution)
    req(input$time_range)
    req(entry_data)
    entry_data <- entry_data()
    req(input$access_key)
    api_key = as.character(input$access_key)
    
    df <- entry_data[entry_data$Lokalizacja %in% input$multi_select,]
    df <- df[df$sensors_parameter_name == input$type_of_pollution,]
    
    sensors_ids <- unique(df$sensors_id)
    
    df_list <- list()
    
    # zdefiniowanie jaki zakres pobrać
    today <- Sys.Date()
    diff <- switch (input$time_range,
                    'Rok' = 365,
                    'Miesiąc' = 30,
                    'Tydzień' = 7
    )
    since <- today - diff
    
    for (i in sensors_ids) {
      url <- paste0('https://api.openaq.org/v3/sensors/', i, '/days?date_to=', today, '&date_from=', since, '&limit=370&page=1')
      
      response <- GET(
        url,
        add_headers(`X-API-Key` = api_key) 
      )
      
      if (status_code(response) == 200) {
        data <- content(response, as = "text", encoding = "UTF-8")
        parsed_data <- fromJSON(data)$results
      } else {
        cat("Błąd w zapytaniu: ", status_code(response), "\n")
        cat("Treść odpowiedzi: ", content(response, as = "text"), "\n")
      }
      
      if (length(parsed_data) == 0) {
        next
      }
      
      parsed_data <- parsed_data[,c(1,4)]
      parsed_data <- parsed_data %>% 
        unnest(cols = period, names_sep = '_')
      parsed_data <- parsed_data %>% 
        unnest(cols = period_datetimeTo, names_sep = '_')
      parsed_data <- parsed_data[,c('value', 'period_datetimeTo_utc')]
      parsed_data <- parsed_data %>% 
        rename(date = period_datetimeTo_utc)
      parsed_data$date <- as.Date(ymd_hms(parsed_data$date, tz = "UTC", quiet = TRUE))
      parsed_data$sensors_id <- i
      
      df_list <- append(df_list, list(parsed_data))
    }
    
    # zlączenie wyników pętli
    big_df <- bind_rows(df_list)
    
    if (nrow(big_df) > 0) {
      big_df <- merge(big_df, df[, c("sensors_id", "Lokalizacja")], by = "sensors_id", all.x = TRUE)
    }
    
    return(big_df)
  })
  
  # wydobycie współrzędnych geograficznych analizowanej lokalizacji
  places_for_map <- reactive({
    
    req(input$dynamic_select)
    req(entry_data)
    entry_data <- entry_data()
    req(input$access_key)
    api_key = as.character(input$access_key)
    
    df <- entry_data[unique(entry_data$locations_id),]
    df <- entry_data[entry_data$Lokalizacja == gsub("Analiza lokacji: ", "", input$dynamic_select),]
    df <- df[,c(2,5,6)]
    df <- df %>%
      rename(dlugosc = coordinates_longitude,
             szerokosc = coordinates_latitude)
    
    return(df)
  })
  
  # pobranie danych z wszystkich sensorów z konkretnej lokalizacji z wybranego okresu czasowego (wartości są średnią dzienną)
  reactive_data_loc <- reactive({
    
    req(input$dynamic_select)
    req(entry_data)
    entry_data <- entry_data()
    req(input$access_key)
    api_key = as.character(input$access_key)
    
    if (input$dynamic_select == "Porównanie") {
      return(NULL)
    }
    
    dynamic_select_val <- gsub("Analiza lokacji: ", "", input$dynamic_select)
    df <- entry_data[entry_data$Lokalizacja == dynamic_select_val,]
    dynamic_ids <- unique(df$sensors_id)
    
    df_list <- list()
    
    # zdefiniowanie jaki zakres pobrać
    today <- Sys.Date()
    diff <- switch (input$time_range,
                    'Rok' = 365,
                    'Miesiąc' = 30,
                    'Tydzień' = 7
    )
    since <- today - diff
    
    for (i in dynamic_ids) {
      url <- paste0('https://api.openaq.org/v3/sensors/', i, '/days?date_to=', today, '&date_from=', since, '&limit=370&page=1')
      response <- GET(
        url,
        add_headers(`X-API-Key` = api_key) 
      )
      
      if (status_code(response) == 200) {
        data <- content(response, as = "text", encoding = "UTF-8")
        parsed_data <- fromJSON(data)$results
      } else {
        cat("Błąd w zapytaniu: ", status_code(response), "\n")
        cat("Treść odpowiedzi: ", content(response, as = "text"), "\n")
      }
      
      if (length(parsed_data) == 0) {
        next
      }
      
      parsed_data <- parsed_data[,c(1,4)]
      parsed_data <- parsed_data %>% 
        unnest(cols = period, names_sep = '_')
      parsed_data <- parsed_data %>% 
        unnest(cols = period_datetimeTo, names_sep = '_')
      parsed_data <- parsed_data[,c('value', 'period_datetimeTo_utc')]
      parsed_data <- parsed_data %>% 
        rename(date = period_datetimeTo_utc)
      parsed_data$date <- as.Date(ymd_hms(parsed_data$date, tz = "UTC", quiet = TRUE))
      parsed_data$sensors_id <- i
      
      df_list <- append(df_list, list(parsed_data))
    }
    
    # zlączenie wyników pętli
    big_df <- bind_rows(df_list)
    big_df <- merge(big_df, df[, c("sensors_id", "sensors_parameter_name")], by = "sensors_id", all.x = TRUE)
    
    return(big_df)
  })
  
  # tabela z aktualnymi odczytami nr1
  output$live_values_dt <- renderTable({
    
    req(reactive_data_live())
    
    live_data <- reactive_data_live()
    
    live_data <- live_data %>% 
      select(3, 4, 9) %>% 
      pivot_wider(names_from = sensors_parameter_name, values_from = value) 
    setDT(live_data)
    
    # Uzupełnienie kolumn o zanieczyszczenia nieobecne w danych i ustandaryzowanie kolejności
    missing_cols <- setdiff(c('Lokalizacja', 'pm25', 'pm10', 'o3', 'co', 'no2', 'so2'), colnames(live_data))
    live_data[, (missing_cols) := NA_real_]
    live_data <- live_data %>% select('Lokalizacja', 'pm25', 'pm10', 'o3', 'co', 'no2', 'so2')
    
    # Dodanie wiersza z normami dla konkretnych zanieczyszczeń
    live_data <- live_data %>% rbind(data.table(
      Lokalizacja = 'NORMY',
      pm25 = 15,
      pm10 = 45,
      o3 = 100,
      co = 4000,
      no2 = 25,
      so2 = 40
    ))
    
    # Zmiana nazw kolumn 
    colnames(live_data) <- sapply(colnames(live_data), function(x) {
      if (x == "Lokalizacja") {
        return(x)  
      } else {
        x <- toupper(x)  
        if (x == "PM25") {
          return("PM2.5") 
        } else if (x == "PM10") {
          return("PM10")  
        } else {
          gsub("([A-Z]+)([0-9]+)", "\\1<sub>\\2</sub>", x)  
        }
      }
    })
    
    live_data
  }, sanitize.text.function = function(x) x)  
  
  # tabela z aktualnymi odczytami nr2
  output$live_values_dt_v2 <- renderTable({
    
    req(reactive_data_live())
    
    live_data <- reactive_data_live()
    
    live_data <- live_data %>% 
      select(3, 4, 9) %>% 
      pivot_wider(names_from = sensors_parameter_name, values_from = value) 
    setDT(live_data)
    
    # Uzupełnienie kolumn o zanieczyszczenia nieobecne w danych i ustandaryzowanie kolejności
    missing_cols <- setdiff(c('Lokalizacja', 'pm25', 'pm10', 'o3', 'co', 'no2', 'so2'), colnames(live_data))
    live_data[, (missing_cols) := NA_real_]
    live_data <- live_data %>% select('Lokalizacja', 'pm25', 'pm10', 'o3', 'co', 'no2', 'so2')
    
    # Dodanie wiersza z normami dla konkretnych zanieczyszczeń
    live_data <- live_data %>% rbind(data.table(
      Lokalizacja = 'NORMY',
      pm25 = 15,
      pm10 = 45,
      o3 = 100,
      co = 4000,
      no2 = 25,
      so2 = 40
    ))
    
    # Zmiana nazw kolumn
    colnames(live_data) <- sapply(colnames(live_data), function(x) {
      if (x == "Lokalizacja") {
        return(x)  
      } else {
        x <- toupper(x)  
        if (x == "PM25") {
          return("PM2.5")  
        } else if (x == "PM10") {
          return("PM10") 
        } else {
          gsub("([A-Z]+)([0-9]+)", "\\1<sub>\\2</sub>", x) 
        }
      }
    })
    
    live_data
  }, sanitize.text.function = function(x) x)
  
  # wykres liniowy poziomu konkretnego zanieczyszczenia dla analizy porównawczej
  output$comparision_plot <- renderPlotly({
    
    req(reactive_data_historic())
    req(input$time_range)
    req(input$type_of_pollution)
    
    diff <- switch (input$time_range,
                    'Rok' = 365,
                    'Miesiąc' = 30,
                    'Tydzień' = 7)
    type <- input$type_of_pollution
    
    title_type <- toupper(type)
    title_type <- gsub("PM25", "PM2.5", title_type)
    title_type <- gsub("O3", "O₃", title_type)
    title_type <- gsub("NO2", "NO₂", title_type)
    title_type <- gsub("SO2", "SO₂", title_type)
    
    threshold <- switch(type,
                        "pm10" = 45,
                        "pm25" = 15,
                        "o3" = 100,
                        "co" = 4000,
                        "no2" = 25,
                        "so2" = 40,
                        NA)
    
    historic_data <- reactive_data_historic()
    
    if ("value" %in% names(historic_data)) {
      historic_data$value <- ifelse(historic_data$value < 0, NA_real_, historic_data$value)
      historic_data$value <- ifelse(historic_data$value > extremes[input$type_of_pollution], NA_real_, historic_data$value)
    }
    
    if (nrow(historic_data) == 0){
      #informuje, że dla wybranych lokalizacji nie ma danych dla tego zanieczyszczenia
      ggplot() +
        annotate("text", x = 1, y = 1, label = paste("Brak pomiarów dla", title_type), size = 13, color = "red") +
        theme_void()
    } else {
      #dopasowywuje etykiety osi x do wybranego zakresu czasowego
      date_range <- diff
      date_breaks_value <- ifelse(date_range > 300, "1 month", ifelse(date_range > 20, "3 days", "1 day"))
      
      p <- historic_data %>%
        ggplot(aes(x = date, y = value, color = as.factor(Lokalizacja), text = round(value, 2))) +
        geom_line()
      
      if (date_range < 300) {
        p <- p + geom_point(size = 1)
      }
      
      p <- p +
        scale_x_date(
          date_breaks = date_breaks_value, 
          date_labels = "%b %d", 
          limits = as.Date(c(Sys.Date() - switch(input$time_range,
                                                 'Rok' = 365,
                                                 'Miesiąc' = 30,
                                                 'Tydzień' = 7), 
                             Sys.Date() - 1))
        ) +
        labs(
          x = NULL, 
          y = "Wartość", 
          color = "" 
        ) +
        ggtitle(paste('Średnie dzienne odczyty', title_type, '- ostatni', tolower(input$time_range))) +
        theme_minimal() + 
        geom_hline(yintercept = threshold, color = "black", linetype = "dashed") 
      
      p_build <- ggplot_build(p)
      y_range <- p_build$layout$panel_params[[1]]$y.range
      ymin <- y_range[1]
      ymax <- y_range[2]
      
      annotate_y <- threshold + 0.04 * (ymax - ymin)
      
      p <- p + annotate( 
        "text", 
        x = max(historic_data$date),
        y = annotate_y,
        label = "               NORMA",
        hjust = 1,
        vjust = -0.5,
        size = 5,
        color = "black"
      ) +
        theme( 
          axis.title.x = element_blank(), 
          axis.text = element_text(size = 16), 
          axis.title.y = element_text(size = 14), 
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 17), 
          plot.title = element_text(hjust = 0, size = 15, face = 'bold')
        )
      
      p_plotly <- ggplotly(p, tooltip = 'text') %>%
        layout(legend = list(orientation = "h",
                             x = 0.5,
                             xanchor = "center",
                             y = -0.2,
                             yanchor = "top"))
      
      p_plotly
      
    }  
  })
  
  # wykres pudełkowy odczytów konkretnego zanieczyszczenia dla analizy porównawczej
  output$comparision_boxplot <- renderPlotly({
    
    req(reactive_data_historic())
    req(input$type_of_pollution)
    
    type <- input$type_of_pollution
    title_type <- toupper(type)
    title_type <- gsub("PM25", "PM2.5", title_type)
    title_type <- gsub("O3", "O₃", title_type)
    title_type <- gsub("NO2", "NO₂", title_type)
    title_type <- gsub("SO2", "SO₂", title_type)
    
    historic_data <- reactive_data_historic()
    
    if ("value" %in% names(historic_data)) {
      historic_data$value <- ifelse(historic_data$value < 0, NA_real_, historic_data$value)
      historic_data$value <- ifelse(historic_data$value > extremes[input$type_of_pollution], NA_real_, historic_data$value)
    }
    
    if (nrow(historic_data) == 0){
      #informuje, że dla wybranych lokalizacji nie ma danych dla tego zanieczyszczenia
      ggplot() +
        annotate("text", x = 1, y = 1, label = paste("Brak pomiarów dla", title_type), size = 13, color = "red") +
        theme_void()
    } else {
      p <- historic_data %>%
        ggplot(aes(x = as.factor(Lokalizacja), y = value, fill = as.factor(Lokalizacja), text = round(value,2))) +  
        geom_boxplot(color = "black") + 
        labs(
          x = NULL,
          y = "Wartość",
          fill = "" 
        ) +
        ggtitle(paste('Rozkład średnich dziennych odczytów',title_type ,'- ostatni', tolower(input$time_range))) +
        theme_minimal() + 
        theme(
          legend.position = "none",
          axis.title.x = element_blank(),
          axis.text = element_text(size = 18),
          axis.title.y = element_text(size = 14),  
          plot.title = element_text(hjust = 0, size = 15, face = 'bold')  
        )
      ggplotly(p, tooltip = 'text') %>% suppressWarnings()
    }
  })
  
  # mapa z analizowaną lokalizacją
  output$map <- renderPlot({
    
    req(places_for_map())
    
    places_for_map <- places_for_map()
    
    polska_map <- map_data("world") %>%
      subset(region == "Poland")
    
    ggplot() +
      geom_polygon(data = polska_map, aes(x = long, y = lat, group = group), fill = "gray", color = "black") +
      geom_point(data = places_for_map, aes(x = dlugosc, y = szerokosc), color = "red", size = 5) + 
      coord_fixed(ratio = 1.1) + 
      ggtitle(paste(places_for_map[1,1], 'na mapie Polski')) +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20, face = 'bold')
      )
  })
  
  # wykres liniowy poziomów wszystkich zanieczyszczeń w danej lokalizacji
  output$norms_plot <- renderPlotly({
    
    req(reactive_data_loc())
    req(input$time_range)
    
    diff <- switch (input$time_range,
                    'Rok' = 365,
                    'Miesiąc' = 30,
                    'Tydzień' = 7)
    
    df <- reactive_data_loc()
    
    df$value <- mapply(function(value, param) {
      if (value < 0 | value > extremes[param]) {
        return(NA_real_)
      } else {
        return(value)
      }
    }, df$value, df$sensors_parameter_name)
    
    # przypisanie norm
    norms <- c(pm10 = 45, pm25 = 15, o3 = 100, co = 4000, no2 = 25, so2 = 40)
    
    # dzielenie wartości przez odpowiednie normy dla ujednolicenia skali
    df <- df %>%
      mutate(
        normalized_value = case_when(
          sensors_parameter_name == "pm10" ~ value / norms["pm10"],
          sensors_parameter_name == "pm25" ~ value / norms["pm25"],
          sensors_parameter_name == "o3" ~ value / norms["o3"],
          sensors_parameter_name == "co" ~ value / norms["co"],
          sensors_parameter_name == "no2" ~ value / norms["no2"],
          sensors_parameter_name == "so2" ~ value / norms["so2"],
          TRUE ~ NA_real_
        ),
        # Zamiana nazw na wielkie litery i indeksy dolne tam, gdzie trzeba
        sensors_parameter_label = case_when(
          sensors_parameter_name == "pm10" ~ "PM10",
          sensors_parameter_name == "pm25" ~ "PM2.5",
          sensors_parameter_name == "o3" ~ "O₃",
          sensors_parameter_name == "co" ~ "CO",
          sensors_parameter_name == "no2" ~ "NO₂",
          sensors_parameter_name == "so2" ~ "SO₂",
          TRUE ~ sensors_parameter_name
        )
      )
    
    # Dopasowywanie etykiet osi x do wybranego zakresu czasowego
    date_range <- diff
    date_breaks_value <- ifelse(date_range > 300, "1 month", ifelse(date_range > 20, "3 days", "1 day"))
    
    p <- ggplot(df, aes(x = date, y = normalized_value, color = sensors_parameter_label, text = round(value, 2))) +
      geom_line()
    
    if (date_range < 300) {
      p <- p + geom_point(size = 1)
    }
    
    p <- p +
      scale_x_date(
        date_breaks = date_breaks_value, 
        date_labels = "%b %d", 
        limits = as.Date(c(Sys.Date() - switch(input$time_range,
                                               'Rok' = 365,
                                               'Miesiąc' = 30,
                                               'Tydzień' = 7), 
                           Sys.Date() - 1))
      ) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "black")
    
    p_build <- ggplot_build(p)
    y_range <- p_build$layout$panel_params[[1]]$y.range
    ymin <- y_range[1]
    ymax <- y_range[2]
    
    annotate_y <- 1 + 0.03 * (ymax - ymin)
    
    p <- p + annotate(
      "text", 
      x = max(df$date),
      y = annotate_y,
      label = "               NORMA",
      hjust = 0,
      vjust = -0.5,
      size = 5,
      color = "black"
    ) +
      labs(
        x = NULL,
        y = "Wartość w odniesieniu do normy",
        color = "Typ zanieczyszczenia"
      ) +
      ggtitle(paste('Zmiany zanieczyszczeń w czasie w porównaniu do norm - ostatni', tolower(input$time_range))) +
      theme_minimal() + 
      theme(
        axis.title.x = element_blank(),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(hjust = 0, size = 15, face = "bold")
      )
    
    p_plotly <- ggplotly(p, tooltip = 'text') %>%
      layout(legend = list(orientation = "h",
                           x = 0.5,
                           xanchor = "center",
                           y = -0.2,
                           yanchor = "top"))
    
    p_plotly
  })
  
  # tabela ze statystykami i podsumowaniem odczytów danej lokalizacji z wybranego zakresu czasowego
  output$stat_dt <- renderTable({
    
    req(reactive_data_loc())
    
    df <- reactive_data_loc()
    
    parameters <- tibble(Zanieczyszczenie = c('pm25', 'pm10', 'o3', 'co', 'no2', 'so2'))
    
    # określenie norm 
    norms <- tibble(
      Zanieczyszczenie = c('pm25', 'pm10', 'o3', 'co', 'no2', 'so2'),
      norma_gora = c(15, 45, 100, 4000, 25, 40)
    )
    
    
    df$value <- mapply(function(value, param) {
      if (value < 0 | value > extremes[param]) {
        return(NA_real_)
      } else {
        return(value)
      }
    }, df$value, df$sensors_parameter_name)
    
    df <- df %>% 
      rename(
        Zanieczyszczenie = sensors_parameter_name
      )
    
    dt <- df %>%
      group_by(Zanieczyszczenie) %>%
      summarise(
        min = min(value, na.rm = TRUE),
        max = max(value, na.rm = TRUE),
        avg = mean(value, na.rm = TRUE),
        median = median(value, na.rm = TRUE),
        sd = sd(value, na.rm = TRUE),
        zmiennosc = sd(value, na.rm = TRUE) / mean(value, na.rm = TRUE) * 100,
        przekroczenie_normy = sum(value > norms$norma_gora[match(Zanieczyszczenie, norms$Zanieczyszczenie)], na.rm = TRUE) 
      ) %>%
      right_join(parameters, by = "Zanieczyszczenie") %>%
      left_join(norms, by = "Zanieczyszczenie") %>%
      arrange(match(Zanieczyszczenie, parameters$Zanieczyszczenie)) %>% 
      rename(
        "Minimalna wartość" = min,
        "Maksymalna wartość" = max,
        "Średnia wartość" = avg,
        "Mediana" = median,
        "Odchylenie standardowe" = sd,
        "Współczynnik zmienności (%)" = zmiennosc,
        "Liczba dni z przekroczoną normą" = przekroczenie_normy,
        "Norma" = norma_gora
      ) %>% 
      select(Zanieczyszczenie, Norma, everything()) %>%
      mutate(
        Zanieczyszczenie = sapply(Zanieczyszczenie, function(x) {
          x <- toupper(x)
          if (x == "PM25") {
            return("PM2.5")
          } else if (x == "PM10") {
            return("PM10")
          } else {
            gsub("([A-Z]+)([0-9]+)", "\\1<sub>\\2</sub>", x)
          }
        })
      )
    
    dt
  }, sanitize.text.function = function(x) x)
}

shinyApp(ui, server)
