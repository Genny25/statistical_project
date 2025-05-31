# inclusione librerie
library("tidyverse")
require("tidyverse")
library("dplyr")
library(moments)

# viene caricato il dataset
dati <- read.csv("dati_istat.csv")

# filtraggio dati
dati <- dati %>% select(Intersezione, TIME_PERIOD, Osservazione) 
dati <- dati %>% 
  filter(Intersezione != "Totale") 

#-----------------------------------------------------------------------

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

#-----------------------------------------------------------------------

# 1. Grafico frequenza assolute
barplot(freq_assolute$Frequenza_Assoluta, 
        names.arg = freq_assolute$Intersezione,
        main = "Frequenze assolute per intersezione",
        xlab = "",
        ylab = "",
        col = "steelblue",
        las = 2) # Etichette verticali

# 2. Grafico frequenze relative
barplot(freq_assolute$Frequenza_Relativa, 
        names.arg = freq_assolute$Intersezione,
        main = "Frequenze Relative per Intersezione",
        xlab = "",
        ylab = "",
        col = "coral",
        las = 2,
        cex.names = 0.8)

# 3. Grafico frequenze cumulate assolute
barplot(freq_assolute$Frequenza_Cumulata_Assoluta, 
        names.arg = freq_assolute$Intersezione,
        main = "Frequenze Cumulate Assolute",
        xlab = "",
        ylab = "",
        col = "darkgreen",
        las = 2,
        cex.names = 0.8)

# 4. Grafico frequenze cumulate relative
barplot(freq_assolute$Frequenza_Cumulata_Relativa, 
        names.arg = freq_assolute$Intersezione,
        main = "Frequenze Cumulate Relative",
        xlab = "",
        ylab = "",
        col = "purple",
        las = 2,
        cex.names = 0.8)

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
  labs(title = "",
       fill = "Tipo di intersezione") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.position = "right") +
  geom_text(aes(label = paste0(round(Frequenza_Assoluta/sum(Frequenza_Assoluta)*100, 1), "%")), 
            position = position_stack(vjust = 0.5),
            size = 3)

#---------------------------------------------------------------------------

media_generale <- mean(dati$Osservazione, na.rm = TRUE)
print(paste("Media campionaria generale delle osservazioni:", round(media_generale, 2)))

mediana_generale <- median(dati$Osservazione, na.rm = TRUE)
print(paste("Mediana generale delle osservazioni:", mediana_generale))

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
  }

find_mode(dati)

#---------------------------------------------------------------------------

# Calcolo della varianza sulla variabile 'Osservazione'.
varianza_campionaria <- var(dati$Osservazione, na.rm = TRUE)
print(paste("Varianza campionaria delle osservazioni:", round(varianza_campionaria, 2)))

# Calcolo della deviazione standard sulla variabile 'Osservazione'.
dev_std_campionaria <- sd(dati$Osservazione, na.rm = TRUE)
print(paste("Deviazione standard campionaria delle osservazioni:", round(dev_std_campionaria, 2)))

# Calcolo dello scarto medio assoluto dalla media per 'Osservazione'.
media_oss <- mean(dati$Osservazione, na.rm = TRUE)
scarto_medio_assoluto <- mean(abs(dati$Osservazione - media_oss), na.rm = TRUE)
print(paste("Scarto medio assoluto (dalla media) delle osservazioni:", round(scarto_medio_assoluto, 2)))

# Calcolo del minimo, massimo e ampiezza del campo di variazione per 'Osservazione'.
min_oss <- min(dati$Osservazione, na.rm = TRUE)
max_oss <- max(dati$Osservazione, na.rm = TRUE)
ampiezza_variazione <- max_oss - min_oss 

print(paste("Valore minimo delle osservazioni:", min_oss))
print(paste("Valore massimo delle osservazioni:", max_oss))
print(paste("Ampiezza del campo di variazione delle osservazioni:", ampiezza_variazione))

# Calcolo del coefficiente di variazione per 'Osservazione'.
media_oss <- mean(dati$Osservazione, na.rm = TRUE)
dev_std_oss <- sd(dati$Osservazione, na.rm = TRUE)
coeff_variazione <- (dev_std_oss / abs(media_oss)) * 100 # abs() per media se potesse essere negativa
print(paste("Coefficiente di variazione delle osservazioni:", round(coeff_variazione, 2), "%"))

#--------------------------------------------------------------------------------------------------

# Calcolo dell'indice di asimmetria per 'Osservazione'.
indice_asimmetria <- skewness(dati$Osservazione, na.rm = TRUE)
print(paste("Indice di asimmetria (Skewness) delle osservazioni:", round(indice_asimmetria, 2)))

# Creazione del grafico
skewness_plot <- ggplot(dati, aes(x = Osservazione)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(color = "darkblue", linewidth = 1) +
  geom_vline(aes(xintercept = media_oss, color = "Media"), linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = mediana_generale, color = "Mediana"), linetype = "dotted", linewidth = 1) +
  labs(title = "Distribuzione del Numero di Osservazioni (Decessi)",
       subtitle = paste0("Skewness: ", round(indice_asimmetria, 2), 
                         " | Media: ", round(media_oss, 2), 
                         " | Mediana: ", round(mediana_generale, 2)),
       x = "Numero di Osservazioni (Decessi)",
       y = "Densità") +
  scale_color_manual(name = "Statistiche", values = c("Media" = "red", "Mediana" = "green")) +
  theme_minimal()

print(skewness_plot)

# Calcolo dell'indice di curtosi (eccesso di curtosi) per 'Osservazione'.
indice_curtosi <- kurtosis(dati$Osservazione, na.rm = TRUE)
print(paste("Indice di curtosi (eccesso di curtosi) delle osservazioni:", round(indice_curtosi, 2)))
# La curtosi "classica" sarebbe approssimativamente: round(indice_curtosi, 2) + 3

# Creazione del grafico per la curtosi
kurtosis_plot <- ggplot(dati, aes(x = Osservazione)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "lightcoral", color = "black", alpha = 0.7) +
  geom_density(color = "darkred", linewidth = 1) +
  geom_vline(aes(xintercept = media_oss, color = "Media"), linetype = "dashed", linewidth = 1) +
  labs(title = "Distribuzione del Numero di Osservazioni (Decessi)",
       subtitle = paste0("Eccesso di Curtosi: ", round(indice_curtosi, 2),
                         " | Media: ", round(media_oss, 2)),
       x = "Numero di Osservazioni (Decessi)",
       y = "Densità") +
  scale_color_manual(name = "Statistiche", values = c("Media" = "blue")) +
  theme_minimal()

print(kurtosis_plot)



#write.csv(freq_assolute, "tabella_frequeza.csv", row.names = FALSE)

