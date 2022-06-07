library(magrittr)
library(tidyverse)
library(readr)


## Datensätze aneinanderreihen

# Ordner mit Datensätzen als wd festlegen
setwd('/Users/ansgarhillenkotter/Bachelorkolloquium_trade/input/Impfstoffhandel 1988-2021 Comtrade')

# Neuen, allumfassenden Datensatz "data" aus allen Daten erstellen
data <- list.files(path='/Users/ansgarhillenkotter/Bachelorkolloquium_trade/input/Impfstoffhandel 1988-2021 Comtrade') %>% 
  lapply(read_csv) %>% 
  bind_rows

# Working Directory wieder zum Ursprungsprojekt setzen
setwd('/Users/ansgarhillenkotter/Bachelorkolloquium_trade/input')

# Auswählen der relevanten Features und jeden 4ten Jahres (1989 - 2021)
data_1 <- data %>%
  select('Year', 'Trade Flow Code', 'Reporter', 'Reporter Code', 'Reporter ISO', 'Partner', 'Partner Code', 'Partner ISO', 'Trade Value (US$)') %>%
  filter(Year %in% seq(1989, 2021, 4)) %>%
  rename (Trade = `Trade Value (US$)`)

# Elimienieren der Trade Daten mit der gesamten Welt und Reduktion auf Export-Daten
data_1 <- data_1 %>%
  filter(`Partner Code` != 0 , `Trade Flow Code` == 2)

# Umwandeln der Handelsmengen in deren Log.
data_1 <- data_1 %>%
  mutate(
    log_trade = log(Trade)
  )

# Generate new variables Yit and Eit
data_1 <- data_1  %>%
  # Create Yit
  group_by(Reporter, Year)  %>%
  mutate(
    y = sum(Trade),
    log_y = log(y)
  )  %>%
  # Create Eit
  group_by(Partner, Year)  %>%
  mutate(
    e = sum(Trade),
    log_e = log(e)
  )

#Importieren des Datensatzes zu Distanzen
library(haven)
dtafile <- file.path(getwd(), "dist_cepii.dta")
dist_data_df <- read_dta(dtafile)

#Zuordnen der Distanzen
data_1 <- data_1%>%
  left_join(dist_data_df, by = c("Reporter ISO" = 'iso_o', 'Partner ISO' = 'iso_d'))%>%
  mutate(log_dist = log(dist))

########## Fehler bei ca. 600 ISO-Bezeignungen########################
missing <- data_2[is.na(data_2$dist),]
View(missing)

setdiff(data_1$`Reporter ISO`, dist_data_df$iso_o)
setdiff(data_1$`Partner ISO`, dist_data_df$iso_d)
######################################################################



data_1 <- data_1 %>%
  # Replicate total_e
  group_by(Reporter, Year) %>%
  mutate(total_e = sum(e)) %>%
  group_by(Year) %>%
  mutate(total_e = max(total_e)) %>%
  
  # Replicate rem_exp
  group_by(Reporter, Year) %>%
  mutate(
    remoteness_exp = sum(distcap *  total_e / e),
    log_remoteness_exp = log(remoteness_exp)
  ) %>%
  
  # Replicate total_y
  group_by(Partner, Year) %>%
  mutate(total_y = sum(y)) %>%
  group_by(Year) %>%
  mutate(total_y = max(total_y)) %>%
  
  # Replicate rem_imp
  group_by(Partner, Year) %>%
  mutate(
    remoteness_imp = sum(distcap / (y / total_y)),
    log_remoteness_imp = log(remoteness_imp)
  )

#Erstellen der Importer- und Exporter-fixed effects Variablen
data_1 <- data_1 %>%
  # This merges the columns exporter/importer with year
  mutate(
    reporter_year = paste0(Reporter, Year),
    partner_year = paste0(Partner, Year)
  )

#Ausfiltern des Länderinternen Handels
data_1 <- data_1 %>%
  filter(Reporter != Partner)
