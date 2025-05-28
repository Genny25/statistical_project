library("tidyverse")
require("tidyverse")

# viene caricato il dataset
dati <- read.csv("dati_istat_filtrati.csv")

# frequenze assolute e relative e anche in forma cumulativa, suddise per categorie

freq_sesso_cum <- dati %>%
  count(Sesso, name = "Frequenza_Assoluta") %>%
  mutate(
    Frequenza_Relativa_Percentuale = (Frequenza_Assoluta / sum(Frequenza_Assoluta)) * 100,
    Frequenza_Assoluta_Cumulata = cumsum(Frequenza_Assoluta),
    Frequenza_Relativa_Cumulata = cumsum(Frequenza_Relativa_Percentuale)
  )

print(freq_sesso_cum)
#---------------------------------------------------------------------------------
freq_laurea_cum <- dati %>%
  count(`Tipo.di.laurea`, name = "Frequenza_Assoluta") %>%
  arrange(desc(`Tipo.di.laurea`)) %>%  # Ordine decrescente per cumulate
  mutate(
    Frequenza_Relativa_Percentuale = (Frequenza_Assoluta / sum(Frequenza_Assoluta)) * 100,
    Frequenza_Assoluta_Cumulata = cumsum(Frequenza_Assoluta),
    Frequenza_Relativa_Cumulata = cumsum(Frequenza_Relativa_Percentuale)
  )

print(freq_laurea_cum)
#---------------------------------------------------------------------------------
freq_luogo_cum <- dati %>%
  count(`Luogo.di.lavoro`, name = "Frequenza_Assoluta") %>%
  arrange(`Luogo.di.lavoro`) %>%  # Ordine alfabetico
  mutate(
    Frequenza_Relativa_Percentuale = (Frequenza_Assoluta / sum(Frequenza_Assoluta)) * 100,
    Frequenza_Assoluta_Cumulata = cumsum(Frequenza_Assoluta),
    Frequenza_Relativa_Cumulata = cumsum(Frequenza_Relativa_Percentuale)
  )

print(freq_luogo_cum)
#---------------------------------------------------------------------------------

