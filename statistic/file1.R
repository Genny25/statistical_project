library("tidyverse")
require("tidyverse")
library("dplyr")

dati <- read.csv("dati_istat_filtrati.csv") # viene caricato il dataset
dati <- dati %>% select(Sesso, Tipo.di.laurea, Luogo.di.lavoro, Osservazione) # vengono selezionate solo le colonne che ci interessano

# vengono calcolate le frequenze assolute, relative e anche in forma cumulativa, suddise per categorie

# sesso
freq_sesso_cum <- dati %>%
  count(Sesso, name = "Frequenza_Assoluta") %>%
  mutate(
    Frequenza_Relativa_Percentuale = (Frequenza_Assoluta / sum(Frequenza_Assoluta)) * 100,
    Frequenza_Assoluta_Cumulata = cumsum(Frequenza_Assoluta),
    Frequenza_Relativa_Cumulata = cumsum(Frequenza_Relativa_Percentuale)
  )

print(freq_sesso_cum)

# tipo di laurea
freq_laurea_cum <- dati %>%
  count(`Tipo.di.laurea`, name = "Frequenza_Assoluta") %>%
  arrange(desc(`Tipo.di.laurea`)) %>%  # Ordine decrescente per cumulate
  mutate(
    Frequenza_Relativa_Percentuale = (Frequenza_Assoluta / sum(Frequenza_Assoluta)) * 100,
    Frequenza_Assoluta_Cumulata = cumsum(Frequenza_Assoluta),
    Frequenza_Relativa_Cumulata = cumsum(Frequenza_Relativa_Percentuale)
  )

print(freq_laurea_cum)

# luogo di lavoro
freq_luogo_cum <- dati %>%
  count(`Luogo.di.lavoro`, name = "Frequenza_Assoluta") %>%
  arrange(`Luogo.di.lavoro`) %>%  # Ordine alfabetico
  mutate(
    Frequenza_Relativa_Percentuale = (Frequenza_Assoluta / sum(Frequenza_Assoluta)) * 100,
    Frequenza_Assoluta_Cumulata = cumsum(Frequenza_Assoluta),
    Frequenza_Relativa_Cumulata = cumsum(Frequenza_Relativa_Percentuale)
  )

print(freq_luogo_cum)



