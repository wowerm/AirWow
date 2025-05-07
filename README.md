# AirWow – Panel Jakości Powietrza w R Shiny

**AirWow** to interaktywna aplikacja internetowa stworzona w R Shiny, która wizualizuje **aktualne i historyczne dane o jakości powietrza** z [OpenAQ API](https://docs.openaq.org/).
Użytkownicy mogą porównywać poziomy zanieczyszczeń w wybranych polskich miastach.

🌐 **Działająca wersja online**: [https://vv0vv3r.shinyapps.io/airwow/](https://vv0vv3r.shinyapps.io/airwow/)

## Funkcje

* Aktualne i historyczne dane o jakości powietrza
* Porównanie danych między różnymi polskimi miastami
* Interaktywne wykresy i filtry
* Dane z OpenAQ API

## Jak uruchomić lokalnie

1. Sklonuj repozytorium:

   ```bash
   git clone https://github.com/wowerm/AirWow.git
   cd AirWow
   ```

2. Zainstaluj wymagane biblioteki w R:

   ```R
   install.packages(c(
     "shiny", "bslib", "tidyverse", "DT", "httr", "jsonlite",
     "stringr", "lubridate", "shinyjs", "shinyalert",
     "ggplot2", "data.table", "maps", "shinytoastr", "plotly"
   ))
   ```

3. Uruchom aplikację:

   ```R
   shiny::runApp()
   ```

*Klucz API wprowadza się bezpośrednio w interfejsie aplikacji.*

## Licencja

Ten projekt jest udostępniony na licencji MIT.

## Autor

Aplikację stworzył **vv0vv3r**, 2025.
