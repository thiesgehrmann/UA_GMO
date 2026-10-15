# figure 4: supportivenss towards microbial factories
library(ggplot2)
library(haven)
while(dev.cur() > 1) dev.off()
rm(list = ls())
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

# load data
geg <- read_sav("../data/GMO questionnaire_October 30, 2024_12.42.sav")
head(geg)
geg_final <- subset(geg, !ResponseId %in% c("R_2b3EfkluOkBXN20", "R_8ercbb6HeOIPh1b", "R_2EA2yTY4Z0PJFaA", "R_5N3Qj23bfOGGTXr", "R_8Tq6MfIcPua7XWN", "R_8rx2Lvfd1ro1BQD", "R_801anLBACCO4RKN"))
head(geg_final)
View(geg_final)

# creation of data frame
data <- data.frame(
  Statement = rep("To what extent do you support the development of microbial factories?", each = length(geg_final$MicroFact_Support)),
  Response = geg_final$MicroFact_Support
)
# remove NA rows
data <- na.omit(data)


# count numbers and percentages
data_summary <- data %>%
  count(Response) %>%
  mutate(Percentage = (n / sum(n)) * 100)


library(ggplot2)
library(viridis)


data_summary$Response <- factor(data_summary$Response, levels = 0:10)



# histogram, updated figure
ggplot(data, aes(x = Response)) +
  geom_histogram(
    binwidth = 1,
    boundary = -0.5,
    fill = "#8DD3C7",     # één vaste kleur (Set3-achtig)
    color = "black"
  ) +
  scale_x_continuous(
    breaks = 0:10,
    limits = c(-0.5, 10.5)
  ) +
  ggtitle("To what extent do you support the development of microbial factories") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 11),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    x = "0 = no support at all, 10 = full support",
    y = "Percentage of participants"
  )

