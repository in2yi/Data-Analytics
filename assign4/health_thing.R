# 1) Load libraries
library(tidyverse)
library(GGally)
library(ggcorrplot)

# 2) Read raw air‑pollution data & pick the single “most impactful” pollutant
air_raw <- read_csv("air_pollitant_data_2019.csv", col_types = cols(.default = col_guess()))

# Compute mean Value per pollutant
pollutant_means <- air_raw %>%
  group_by(Pollutant) %>%
  summarise(mean_val = mean(Value, na.rm = TRUE)) %>%
  arrange(desc(mean_val))

# Pick pollutant with highest average concentration
top_pollutant <- pollutant_means %>% slice(1) %>% pull(Pollutant)

# Keep only that pollutant’s county values
air <- air_raw %>%
  filter(Pollutant == top_pollutant) %>%
  select(State, County, air_value = Value)

# 3) Read health‑status data (percent → decimal)
health <- read_csv("health_data_2019.csv", col_types = cols(.default = col_character())) %>%
  mutate(
    health_rate = parse_number(Value) / 100
  ) %>%
  select(State, County, health_rate)

# 4) Read temperature data (skip “#” metadata and strip “ County” suffix)
temp <- read_csv("temperature_data_2019.csv",
                 comment = "#",
                 col_types = cols(.default = col_guess())) %>%
  rename(County = Name) %>%
  mutate(
    County = str_remove(County, " County$")
  ) %>%
  select(State, County, temp_value = Value)

# 5) Read UV‑exposure data (Value already numeric)
uv <- read_csv("UV_data_2019.csv", col_types = cols(.default = col_guess())) %>%
  select(State, County, uv_value = Value)

# 6) Merge all four by State + County
# keep State in the merged frame
df <- air %>%
  inner_join(health, by = c("State","County")) %>%
  inner_join(temp,  by = c("State","County")) %>%
  inner_join(uv,    by = c("State","County")) %>%
  select(State, County, air_value, health_rate, temp_value, uv_value)


# 8) Inspect the result
glimpse(df)










# 2) Pivot to long form for univariate plots
long_df <- df %>%
  pivot_longer(
    cols      = c(air_value, health_rate, temp_value, uv_value),
    names_to  = "variable",
    values_to = "value"
  )

# 3) Histograms

df2 <- df %>%
  mutate(
    log_air = log(air_value),
    log_uv  = log(uv_value)
  )

df2 <- df2 %>%
  mutate(across(
    c(log_air, log_uv, health_rate, temp_value),
    ~ (.-mean(.))/sd(.),
    .names = "z_{col}"
  ))


ggplot(long_df, aes(x = value)) +
  facet_wrap(~variable, scales = "free") +
  geom_histogram(bins = 30, color = "white") +
  labs(title = "Distribution of Each Variable", x = NULL, y = "Count") +
  theme_minimal()

# 4) Density plots
ggplot(long_df, aes(x = value)) +
  facet_wrap(~variable, scales = "free") +
  geom_density() +
  labs(title = "Density of Each Variable", x = NULL, y = "Density") +
  theme_minimal()

# 5) Boxplots (spotting outliers)
# Separate boxplots with free y‑scales
ggplot(long_df, aes(x = "", y = value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  labs(
    title = "Individual Boxplots of Each Variable",
    x     = NULL,
    y     = "Value"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    strip.text  = element_text(face = "bold")
  )


# 6) Pairwise scatterplots with LOESS & Pearson r
ggpairs(
  df %>% select(-County),
  lower = list(continuous = wrap("smooth_loess", se = FALSE)),
  upper = list(continuous = wrap("cor", size = 3)),
  title = "Scatterplots (LOESS) & Correlations"
)

# 7) Correlation heatmap
corr_mat <- cor(df %>% select(-County), use = "complete.obs")
ggcorrplot(
  corr_mat,
  lab   = TRUE,
  title = "Correlation Heatmap"
)










# -----------------------------------------------------
# Slide 2: Model Construction & Application
# -----------------------------------------------------

# 0) Install/load extra packages
if (!requireNamespace("caret", quietly=TRUE)) {
  install.packages("caret", repos="https://cloud.r-project.org")
}
library(tidyverse)
library(caret)

# ── A) Create the z‑scored dataset (df3) ─────────────────────────────

# 1) Starting from `df` (County, air_value, health_rate, temp_value, uv_value),
#    log‑transform the two skewed vars, then z‑score all four.
df3 <- df %>%
  mutate(
    log_air = log(air_value),
    log_uv  = log(uv_value)
  ) %>%
  mutate(across(
    c(log_air, log_uv, health_rate, temp_value),
    ~ (.-mean(.))/sd(.),
    .names = "z_{col}"
  ))

# Quick check
glimpse(df3)
# should show: County, air_value, health_rate, temp_value, uv_value,
#               log_air, log_uv, z_log_air, z_log_uv, z_health_rate, z_temp_value

# ── B) Build the PCA‐based air_index ───────────────────────────────────

# 2) Pivot the full air_raw into wide form, one column per pollutant
air_wide <- air_raw %>%
  select(State, County, Pollutant, Value) %>%
  pivot_wider(names_from = Pollutant, values_from = Value) %>%
  drop_na()

# 3) Run PCA (standardizing each pollutant)
poll_cols <- setdiff(names(air_wide), c("State","County"))
pca_res <- prcomp(air_wide[poll_cols], scale. = TRUE)

# 4) Extract PC1 as the “air_index”
air_index_df <- air_wide %>%
  mutate(air_index = pca_res$x[,1]) %>%
  select(State, County, air_index)

# Optional: check how much variance PC1 explains
summary(pca_res)

# ── C) Merge & model ─────────────────────────────────────────────────

# 5) Join air_index into your z‑scored data
model_df <- df3 %>%
  inner_join(air_index_df, by = c("State","County")) %>%
  select(health_rate, air_index, z_temp_value, z_log_uv)

# 6) Fit in‑sample regression
lm_fit <- lm(health_rate ~ air_index + z_temp_value + z_log_uv,
             data = model_df)
summary(lm_fit)

# 7) 5‑fold CV with caret
set.seed(123)
cv_fit <- train(
  health_rate ~ air_index + z_temp_value + z_log_uv,
  data      = model_df,
  method    = "lm",
  trControl = trainControl(method="cv", number=5)
)
print(cv_fit)












