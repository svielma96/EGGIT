# Load required packages
install.packages('tidyverse')
install.packages('FSA')
library(tidyverse)
library(rstatix)
library(FSA)  # For Dunn’s post-hoc
library(ggplot2)
setwd("/Users/svielma/Library/CloudStorage/OneDrive-ITG/ITG/7.EDUCATION/2025 - BSC - DIOGO PINTO AND MARIJN WEYLER/BSc Thesis")

# Load your datasets (adjust the paths accordingly)
eggs <- read_delim("DataEggs.txt", delim = "\t")
area <- read_delim("DataArea.txt", delim = "\t") %>%
  mutate(Area = str_replace(Area, ",", ".") %>% as.numeric())
integrity <- read_delim("Integrity.txt", delim = "\t")

# 1. LOOSE EGGS
# Plot
ggplot(eggs, aes(x = Intensity, y = Eggs)) +
  geom_boxplot(fill = "lightblue") +
  geom_jitter(width = 0.1, alpha = 0.6) +
  theme_minimal() +
  ggtitle("Number of Loose Eggs per Rain Intensity")

# Kruskal-Wallis
kruskal.test(Eggs ~ Intensity, data = eggs)

# Post-hoc
dunnTest(Eggs ~ Intensity, data = eggs, method = "bonferroni")

# 2. EGG AREA
# Plot
ggplot(area, aes(x = Intensity, y = Area)) +
  geom_boxplot(fill = "blue") +
  geom_jitter(width = 0.1, alpha = 0.6) +
  theme_minimal() +
  ggtitle("Egg Area per Rain Intensity")

# Kruskal-Wallis
kruskal.test(Area ~ Intensity, data = area)

# Time Series Plot
area %>%
  group_by(Intensity, Time) %>%
  summarise(mean_area = mean(Area), sd = sd(Area), .groups = "drop") %>%
  ggplot(aes(x = Time, y = mean_area, group = Intensity, color = Intensity)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_area - sd, ymax = mean_area + sd), width = 0.2) +
  theme_minimal() +
  ggtitle("Egg Area Over Time by Rain Intensity") +
  ylab("Mean Area ± SD")

# 3. INTEGRITY SCORE
# Clean missing values
integrity_clean <- integrity %>% filter(!is.na(`Integrity Score`))

# Integrity Score Boxplot per Intensity
integrity_clean %>%
  group_by(Intensity, Replicate) %>%
  summarise(MeanScore = mean(`Integrity Score`), .groups = "drop") %>%
  ggplot(aes(x = Intensity, y = MeanScore)) +
  geom_boxplot(fill = "lightcoral") +
  geom_jitter(width = 0.1, alpha = 0.6) +
  theme_minimal() +
  ggtitle("Mean Integrity Score per Replicate")

# Kruskal-Wallis
integrity_mean <- integrity_clean %>%
  group_by(Intensity, Replicate) %>%
  summarise(MeanIntegrity = mean(`Integrity Score`), .groups = "drop")
kruskal.test(MeanIntegrity ~ Intensity, data = integrity_mean)

# Integrity Over Time Plot
integrity_clean %>%
  group_by(Intensity, Time) %>%
  summarise(mean_score = mean(`Integrity Score`, na.rm = TRUE),
            sd = sd(`Integrity Score`, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Time, y = mean_score, group = Intensity, color = Intensity)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_score - sd, ymax = mean_score + sd), width = 0.2) +
  theme_minimal() +
  ggtitle("Integrity Score Over Time by Rain Intensity") +
  ylab("Mean Integrity Score ± SD")

# 4. FRIEDMAN TEST FOR TEMPORAL CHANGES (WITH CLEANED DATA)
friedman_integrity_results <- integrity_clean %>%
  group_by(Intensity) %>%
  filter(n_distinct(Replicate) >= 2) %>%
  nest() %>%
  mutate(friedman = map(data, ~ .x %>%
                          pivot_wider(names_from = Time, values_from = `Integrity Score`) %>%
                          drop_na() %>%
                          column_to_rownames("Replicate") %>%
                          as.matrix() %>%
                          t() %>%
                          friedman.test() ))

friedman_integrity_results %>% select(Intensity, friedman)

# Plot for medium rain -- significant
medium_integrity <- integrity %>%
  filter(Intensity == "Medium", !is.na(`Integrity Score`))

# Plot
ggplot(medium_integrity, aes(x = Time, y = `Integrity Score`, group = Replicate, color = factor(Replicate))) +
  geom_line(alpha = 0.6, size = 1) +
  geom_point(size = 2) +
  stat_summary(aes(group = 1), fun = mean, geom = "line", color = "black", size = 1.2, linetype = "dashed") +
  stat_summary(aes(group = 1), fun.data = mean_sdl, geom = "errorbar", color = "black", width = 0.2) +
  theme_minimal() +
  labs(title = "Integrity Score Over Time (Medium Rain Intensity)",
       x = "Time Point",
       y = "Integrity Score",
       color = "Replicate") +
  theme(legend.position = "right")
