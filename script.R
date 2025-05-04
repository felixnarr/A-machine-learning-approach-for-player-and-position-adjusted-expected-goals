# 1. Pakete laden
library(StatsBombR)
library(dplyr)
library(purrr)       # für map_df()
library(tidyr)       # für Datenbereinigung
library(caret)       # für Feature Encoding

# 2. Wettbewerbe & Spiele laden
Comp <- FreeCompetitions()
Matches <- FreeMatches(Comp)

#Nur La Liga-Daten filtern
la_liga <- Matches %>% 
  filter(competition.competition_name == "La Liga")

# 3. Alle Eventdaten aus La Liga laden
all_events <- map_df(1:nrow(la_liga), function(i) {
  tryCatch({
    df <- get.matchFree(la_liga[i, ])
    allclean(df)  # Spaltennamen vereinheitlichen
  }, error = function(e) {
    message(paste("Fehler bei Spiel", i, ":", e$message))
    NULL
  })
})

# 4. Nur Open-Play-Schüsse (ohne Elfmeter)
shots <- all_events %>%
  filter(type.name == "Shot",
         play_pattern.name == "Regular Play",
         shot.type.name != "Penalty")

# 5. Feature-Engineering & Vorverarbeitung
# Extrahiere X/Y-Koordinaten aus 'location'-Spalte
shots <- shots %>%
  filter(!is.na(location)) %>%
  mutate(
    x = map_dbl(location, 1),
    y = map_dbl(location, 2),
    goal = ifelse(shot.outcome.name == "Goal", 1, 0),
    first_time = ifelse(is.na(shot.first_time), FALSE, TRUE),
    body_part = shot.body_part.name
  )

# 6. Relevante Features auswählen
model_data <- shots %>%
  select(goal, x, y, first_time, body_part) %>%
  filter(complete.cases(.))  # entferne Zeilen mit NA

# 7. Encoding für maschinelles Lernen
install.packages("caret")
install.packages("recipes")
library(caret)



model_data$body_part <- as.factor(model_data$body_part)
model_data$first_time <- as.factor(model_data$first_time)

# Dummy-Variablen erstellen
dummies <- dummyVars(goal ~ ., data = model_data)
X <- predict(dummies, newdata = model_data)

# Endgültiger Modell-Datensatz
final_data <- data.frame(X)
final_data$goal <- model_data$goal


# Beispiel: Logistic Regression
model <- glm(goal ~ ., data = final_data, family = "binomial")
summary(model)

shots <- all_events %>%
  filter(type.name == "Shot",
         play_pattern.name == "Regular Play",
         shot.type.name != "Penalty")

# Anzahl Schüsse im Datensatz
nrow(shots)


shots %>%
  distinct(player.name) %>%
  filter(str_detect(player.name, "Messi"))


#MESSI
#-----------------------------------------------------------------------------

messi_shots <- shots %>%
  filter(player.name == "Lionel Andrés Messi Cuccittini")

# 1. Nur Schüsse von Messi
messi_shots <- shots %>%
  filter(player.name == "Lionel Andrés Messi Cuccittini")

# 2. Anzahl Tore
messi_goals <- sum(messi_shots$shot.outcome.name == "Goal")

# 3. Gesamtes Expected Goals (xG)
messi_xg <- sum(messi_shots$shot.statsbomb_xg, na.rm = TRUE)

# 4. Effizienz-Metrik: Over-/Underperformance
messi_effizienz <- messi_goals - messi_xg

# 5. Ausgabe
cat("📊 Lionel Messi – Abschlussanalyse:\n")
cat("• Anzahl Schüsse: ", nrow(messi_shots), "\n")
cat("• Tore: ", messi_goals, "\n")
cat("• xG: ", round(messi_xg, 2), "\n")
cat("• Tore – xG (Effizienz): ", round(messi_effizienz, 2), "\n")



# plot
library(ggplot2)

# Daten vorbereiten
messi_stats <- data.frame(
  Kategorie = c("Tore", "Expected Goals", "Tore - xG"),
  Wert = c(messi_goals, messi_xg, messi_effizienz)
)

# Balkendiagramm erstellen
ggplot(messi_stats, aes(x = Kategorie, y = Wert, fill = Kategorie)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = round(Wert, 2)), vjust = -0.5, size = 5) +
  labs(
    title = "Lionel Messi: Abschluss-Effizienz",
    y = "Wert",
    x = NULL
  ) +
  scale_fill_manual(values = c("#0072B2", "#56B4E9", "#009E73")) +
  theme_minimal(base_size = 14)


# INIESTA
# -----------------------------------------------------------------------------

library(dplyr)
library(ggplot2)

# 1. Nur Schüsse von Andrés Iniesta
iniesta_shots <- shots %>%
  filter(player.name == "Andrés Iniesta Luján")

# 2. Anzahl Tore
iniesta_goals <- sum(iniesta_shots$shot.outcome.name == "Goal")

# 3. Gesamtes Expected Goals (xG)
iniesta_xg <- sum(iniesta_shots$shot.statsbomb_xg, na.rm = TRUE)

# 4. Effizienz-Metrik: Over-/Underperformance
iniesta_effizienz <- iniesta_goals - iniesta_xg

# 5. Ausgabe
cat("📊 Andrés Iniesta – Abschlussanalyse:\n")
cat("• Anzahl Schüsse: ", nrow(iniesta_shots), "\n")
cat("• Tore: ", iniesta_goals, "\n")
cat("• xG: ", round(iniesta_xg, 2), "\n")
cat("• Tore – xG (Effizienz): ", round(iniesta_effizienz, 2), "\n")

# 6. Daten vorbereiten für Plot
iniesta_stats <- data.frame(
  Kategorie = c("Tore", "Expected Goals", "Tore - xG"),
  Wert = c(iniesta_goals, iniesta_xg, iniesta_effizienz)
)

# 7. Balkendiagramm erstellen
ggplot(iniesta_stats, aes(x = Kategorie, y = Wert, fill = Kategorie)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = round(Wert, 2)), vjust = -0.5, size = 5) +
  labs(
    title = "Andrés Iniesta: Abschluss-Effizienz",
    y = "Wert",
    x = NULL
  ) +
  scale_fill_manual(values = c("#0072B2", "#56B4E9", "#009E73")) +
  theme_minimal(base_size = 14)





#Positionsgruppen
#----------------------------------------------------------------------------------------------

# Positionsadaptiert
library(dplyr)
library(ggplot2)

# Positionale xG-Analyse mit Gruppierung in Abwehr, Mittelfeld und Angriff
position_groups <- shots %>%
  mutate(PositionGruppe = case_when(
    position.name %in% c("Goalkeeper", "Right Back", "Left Back", "Center Back", "Right Wing Back", "Left Wing Back") ~ "Abwehr",
    position.name %in% c("Defensive Midfield", "Central Midfield", "Left Midfield", "Right Midfield") ~ "Mittelfeld",
    position.name %in% c("Attacking Midfield", "Left Wing", "Right Wing", "Center Forward", "Second Striker") ~ "Angriff",
    TRUE ~ "Sonstige"
  )) %>%
  group_by(PositionGruppe) %>%
  summarise(
    Schuesse = n(),
    Tore = sum(shot.outcome.name == "Goal"),
    xG = sum(shot.statsbomb_xg, na.rm = TRUE),
    Effizienz = Tore - xG
  ) %>%
  arrange(desc(Schuesse))

# Plot
ggplot(position_groups, aes(x = reorder(PositionGruppe, -Effizienz), y = Effizienz, fill = PositionGruppe)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(Effizienz, 2)), vjust = -0.5, size = 5) +
  labs(
    title = "xG-Effizienz nach Positionsgruppe",
    x = "Positionsgruppe",
    y = "Tore – xG (Effizienz)"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14)

