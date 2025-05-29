# inclusione librerie
library("tidyverse")
require("tidyverse")
library("dplyr")

# viene caricato il dataset
dati <- read.csv("dati_istat.csv")

# filtraggio dati
dati <- dati %>% select(Intersezione, TIME_PERIOD, Osservazione) 
dati <- dati %>% 
  filter(Intersezione != "Totale") 

# 1. Frequenze assolute per intersezione dell'incidente
freq_assolute <- aggregate(Osservazione ~ Intersezione, data=dati, sum)
colnames(freq_assolute) <- c("Intersezione", "Frequenza_Assoluta")
print(freq_assolute)

# 2. Frequenze relative per intersezione dell'incidente
totale <- sum(freq_assolute$Frequenza_Assoluta)
freq_assolute$Frequenza_Relativa <- freq_assolute$Frequenza_Assoluta / totale
print(freq_assolute)

# 3. Frequenze cumulate assolute
freq_assolute <- freq_assolute[order(-freq_assolute$Frequenza_Assoluta),]
freq_assolute$Frequenza_Cumulata_Assoluta <- cumsum(freq_assolute$Frequenza_Assoluta)
print(freq_assolute)

# 4. Frequenze cumulate relative
freq_assolute$Frequenza_Cumulata_Relativa <- freq_assolute$Frequenza_Cumulata_Assoluta / totale
print(freq_assolute)

# Visualizzazione grafica
barplot(freq_assolute$Frequenza_Assoluta, 
        names.arg = freq_assolute$Intersezione,
        main = "Frequenze assolute per intersezione incidenti",
        xlab = "Intersezione",
        ylab = "Numero di morti",
        col = "steelblue",
        las = 2) # Etichette verticali


media_generale <- mean(dati$Osservazione, na.rm = TRUE)
print(paste("Media campionaria generale:", round(media_generale, 2)))

mediana_generale <- median(dati$Osservazione, na.rm = TRUE)
print(paste("Mediana generale:", mediana_generale))

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
  }

find_mode(dati)

# Converti TIME_PERIOD in fattore per mantenere l'ordine originale
dati$TIME_PERIOD <- factor(dati$TIME_PERIOD, levels = unique(dati$TIME_PERIOD))

# Calcola il numero totale di morti per anno
morti_per_anno <- dati %>%
  group_by(TIME_PERIOD) %>%
  summarise(Totale_Morti = sum(Osservazione, na.rm = TRUE))

# Crea il grafico a linee con tutti gli anni visibili
ggplot(morti_per_anno, aes(x = TIME_PERIOD, y = Totale_Morti, group = 1)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(title = "Andamento del numero di morti per incidenti stradali",
       x = "Anno",
       y = "Numero di morti") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +  # Riduci dimensione testo
  scale_x_discrete(breaks = levels(morti_per_anno$TIME_PERIOD))  # Mostra tutti i valori

# Crea il grafico a torta
ggplot(freq_assolute, aes(x = "", y = Frequenza_Assoluta, fill = Intersezione)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Distribuzione dei morti per tipo di intersezione",
       fill = "Tipo di intersezione") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.position = "right") +
  geom_text(aes(label = paste0(round(Frequenza_Assoluta/sum(Frequenza_Assoluta)*100, 1), "%")), 
            position = position_stack(vjust = 0.5),
            size = 3)

#write.csv(freq_assolute, "tabella_frequeza.csv", row.names = FALSE)
