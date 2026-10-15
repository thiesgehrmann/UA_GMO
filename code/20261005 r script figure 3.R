# r script figure 3 emotions towards microbial factories
while(dev.cur() > 1) dev.off()
rm(list = ls())


install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggtern")
install.packages("haven")
install.packages("scales")
library(ggtern)
library(haven)
library(ggplot2)
library(dplyr)
library(scales)

install.packages("tidyr")
library(tidyr)
# load data
geg <- read_sav("../data/GMO questionnaire_October 30, 2024_12.42.sav")
head(geg)
geg_final <- subset(geg, !ResponseId %in% c("R_2b3EfkluOkBXN20", "R_8ercbb6HeOIPh1b", "R_2EA2yTY4Z0PJFaA", "R_5N3Qj23bfOGGTXr", "R_8Tq6MfIcPua7XWN", "R_8rx2Lvfd1ro1BQD", "R_801anLBACCO4RKN"))
head(geg_final)
View(geg_final) # exclusie van de 7 participanten


som_1_154 <- sum(geg_final$MicroFact_emotions_1_154, na.rm = TRUE)
som_2_154 <- sum(geg_final$MicroFact_emotions_2_154, na.rm = TRUE)
som_3_154 <- sum(geg_final$MicroFact_emotions_3_154, na.rm = TRUE)

som_1_155 <- sum(geg_final$MicroFact_emotions_1_155, na.rm = TRUE)
som_2_155 <- sum(geg_final$MicroFact_emotions_2_155, na.rm = TRUE)
som_3_155 <- sum(geg_final$MicroFact_emotions_3_155, na.rm = TRUE)

som_1_156 <- sum(geg_final$MicroFact_emotions_1_156, na.rm = TRUE)
som_2_156 <- sum(geg_final$MicroFact_emotions_2_156, na.rm = TRUE)
som_3_156 <- sum(geg_final$MicroFact_emotions_3_156, na.rm = TRUE)

som_1_157 <- sum(geg_final$MicroFact_emotions_1_157, na.rm = TRUE)
som_2_157 <- sum(geg_final$MicroFact_emotions_2_157, na.rm = TRUE)
som_3_157 <- sum(geg_final$MicroFact_emotions_3_157, na.rm = TRUE)

som_1_158 <- sum(geg_final$MicroFact_emotions_1_158, na.rm = TRUE)
som_2_158 <- sum(geg_final$MicroFact_emotions_2_158, na.rm = TRUE)
som_3_158 <- sum(geg_final$MicroFact_emotions_3_158, na.rm = TRUE)

som_1_159 <- sum(geg_final$MicroFact_emotions_1_159, na.rm = TRUE)
som_2_159 <- sum(geg_final$MicroFact_emotions_2_159, na.rm = TRUE)
som_3_159 <- sum(geg_final$MicroFact_emotions_3_159, na.rm = TRUE)

som_1_160 <- sum(geg_final$MicroFact_emotions_1_160, na.rm = TRUE)
som_2_160 <- sum(geg_final$MicroFact_emotions_2_160, na.rm = TRUE)
som_3_160 <- sum(geg_final$MicroFact_emotions_3_160, na.rm = TRUE)


tot154 <- sum(som_1_154 + som_2_154 + som_3_154)
tot155 <- sum(som_1_155 + som_2_155 + som_3_155)
tot156 <- sum(som_1_156 + som_2_156 + som_3_156)
tot157 <- sum(som_1_157 + som_2_157 + som_3_157)
tot158 <- sum(som_1_158 + som_2_158 + som_3_158)
tot159 <- sum(som_1_159 + som_2_159 + som_3_159)
tot160 <- sum(som_1_160 + som_2_160 + som_3_160)


ratio_1_154 <- som_1_154 / tot154
ratio_2_154 <- som_2_154 / tot154
ratio_3_154 <- som_3_154 / tot154

ratio_1_155 <- som_1_155 / tot155
ratio_2_155 <- som_2_155 / tot155
ratio_3_155 <- som_3_155 / tot155

ratio_1_156 <- som_1_156 / tot156
ratio_2_156 <- som_2_156 / tot156
ratio_3_156 <- som_3_156 / tot156

ratio_1_157 <- som_1_157 / tot157
ratio_2_157 <- som_2_157 / tot157
ratio_3_157 <- som_3_157 / tot157

ratio_1_158 <- som_1_158 / tot158
ratio_2_158 <- som_2_158 / tot158
ratio_3_158 <- som_3_158 / tot158

ratio_1_159 <- som_1_159 / tot159
ratio_2_159 <- som_2_159 / tot159
ratio_3_159 <- som_3_159 / tot159

ratio_1_160 <- som_1_160 / tot160
ratio_2_160 <- som_2_160 / tot160
ratio_3_160 <- som_3_160 / tot160


# create data frame
data <- data.frame(
  connected = c(ratio_1_154, ratio_1_155, ratio_1_156, ratio_1_157, ratio_1_158, ratio_1_159, ratio_1_160),
  unbothered = c(ratio_2_154, ratio_2_155, ratio_2_156, ratio_2_157, ratio_2_158, ratio_2_159, ratio_2_160),
  disconnected = c(ratio_3_154, ratio_3_155, ratio_3_156, ratio_3_157, ratio_3_158, ratio_3_159, ratio_3_160),
  Emotions = c("Curious", "Hopeful", "Disinterested", "Concerned", "Afraid", "Angry", "Excited")
)
# Data restructuring
data_long <- data %>%
  pivot_longer(
    cols = c('connected', 'unbothered', 'disconnected'),
    names_to = "Connection",
    values_to = "Percentage"
  )

############################################# adapt orden

connected_totals <- data_long %>%
  filter(data_long$Connection == "connected") %>%
  group_by(Emotions) %>%
  summarise(TotalConnected = sum(Percentage)) %>%
  arrange(desc(1-TotalConnected))  # Sorteer op "connected" (aflopend)
# Pas de volgorde van de Emotions factor aan op basis van de sortering
data_long$Emotions <- factor(data_long$Emotions, levels = connected_totals$Emotions)

######################################

# stacked bar plot
p <- ggplot(data_long, aes(y = Emotions, x = Percentage, fill = Connection)) +
  geom_bar(stat = "identity", position = "stack") +  # Gestapelde staven
  scale_x_continuous(labels = scales::percent) +  # Gebruik percent zonder de schaal te vermenigvuldigen
  scale_fill_brewer(palette = "Set3") +  # Set3 kleurenpalet
  labs(
    x = "Percentage of Participants",
    y = "Emotions",
    fill = "Legend",  # Titel van de legenda
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),         # Grotere as-tekst
    axis.title = element_text(size = 12, face = "bold"),  # Grotere as-titels
    legend.text = element_text(size = 10),      # Grotere legenda-tekst
    legend.title = element_text(size = 12, face = "bold") # Grotere legenda-titel
  )

# Plot graph
plot(p)

############################################################

# to make percentages in the grpah
response_counts <- data_long %>%
  group_by(Statement, Response) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(Statement, Response)


View(response_counts)

########################################################

# save as JPEG-bestand
ggsave("stacked_barplot_emotions.jpg", plot = p, width = 10, height = 8, dpi = 300)

