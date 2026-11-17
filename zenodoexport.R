library(httr2)
library(jsonlite)
library(purrr)
library(tidyr)
library(dplyr)


# ═══════════════════════════════════════
# ANPASSUNG 1: Community-ID eintragen
# ═══════════════════════════════════════
COMMUNITY_ID <- "dhd"

# ═══════════════════════════════════════
# ANPASSUNG 2 (optional): Batch-Größe
# ═══════════════════════════════════════
# Größere Werte = weniger Requests, aber längere Antwortzeit
# Maximum bei Zenodo typisch 10.000, praktisch 100-1000 empfohlen
BATCH_SIZE <- 25 #größere Batch-Größe führte zum timeout

fetch_community <- function(community_id, size = BATCH_SIZE) {
  base_url <- "https://zenodo.org/api/records/"
  
  records <- list()
  url <- request(base_url) |>
    req_url_query(
      communities = community_id,  # ← wird übergeben
      size = size,
      page = 1
    )
  
  repeat {
    resp <- url |> req_perform() |> resp_body_json()
    hits <- resp$hits$hits
    if (length(hits) == 0) break
    
    records <- c(records, hits)
    
    next_url <- resp$links$`next`
    if (is.null(next_url)) break
    url <- request(next_url)
    
    # ═══════════════════════════════════════
    # ANPASSUNG 3 (optional): Wartezeit
    # ═══════════════════════════════════════
    # Erhöhen bei Rate-Limit-Fehlern (HTTP 429)
    Sys.sleep(0.5)
  }
  
  records
}

# ═══════════════════════════════════════
# Ausführung
# ═══════════════════════════════════════
records <- fetch_community(COMMUNITY_ID)

# ═══════════════════════════════════════
# ANPASSUNG 4: Extrahierte Felder
# ═══════════════════════════════════════
df <- map_dfr(records, function(r) {
  meta <- r$metadata
  
  # Hier weitere Felder hinzufügen nach Bedarf:
  # meta$publication_date, meta$description, meta$resource_type$type, etc.
  tibble(
    title = meta$title,
    creators = list(map_chr(meta$creators, "name", .default = NA)),
    keywords = list(meta$keywords %||% character(0)),
    recid = r$id
  )
})

# ═══════════════════════════════════════
# ANPASSUNG 5: Auswertungsstruktur
# ═══════════════════════════════════════
# df Typ vereinheitlichen
# df %>%
#    mutate(keywords = as.character(keywords)) %>%                                  # Sicherheitshalber
#  pivot_longer(cols = keywords, names_to = "var", values_to = "val")

# Beispiel: Keywords einzeln pro Zeile
df_exploded <- df |> unnest_longer(keywords)

# ═══════════════════════════════════════
# AUSWERTUNG: Übersicht
# ═══════════════════════════════════════
# Schnelle Übersicht
objects(df_exploded)           # oder ls()
str(df_exploded)    # prüft Existenz + Struktur

# ═══════════════════════════════════════
# AUSWERTUNG: Formatierte Tabellen
# ═══════════════════════════════════════

library(knitr)
kable(df_exploded[1:20, ])  # HTML/Markdown-Ausgabe

library(gt)
df_exploded |>
  slice_head(n = 50) |>
  gt() |>
  cols_width(keywords ~ px(200))

# ═══════════════════════════════════════
# AUSWERTUNG: Aggregierte Übersicht
# ═══════════════════════════════════════

# Häufigste Keywords
df_exploded |>
  count(keywords, sort = TRUE) |>
  head(20)

# Prolifikste Autoren
df |>
  unnest_longer(creators) |>
  count(creators, sort = TRUE) |>
  head(20)

# Records pro Jahr
df |>
  mutate(year = substr(meta$publication_date, 1, 4)) |>
  count(year)

### Speichern als csv
write.csv(df, "recordlist.csv", row.names = FALSE, dec = ",")
