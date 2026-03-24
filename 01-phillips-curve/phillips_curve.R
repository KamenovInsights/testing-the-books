# =============================================================================
# Testing the Books #1: The Phillips Curve
# =============================================================================
# Author:  Aleksandar Kamenov | Kamenov Insights
# Date:    March 2026
# Data:    U.S. Unemployment Rate (UNRATE) & CPI (CPIAUCSL) - FRED
# Blog:    https://medium.com/@kamenovinsights
# =============================================================================

# ── REQUIRED PACKAGES ─────────────────────────────────────────────────────────
# Install if needed:
# install.packages(c("fredr", "ggplot2", "dplyr", "zoo"))

library(fredr)
library(ggplot2)
library(dplyr)
library(zoo)

# ── 1. DATA DOWNLOAD ─────────────────────────────────────────────────────────

# NOTE: Replace with your own FRED API key
# Get a free key at: https://fred.stlouisfed.org/docs/api/api_key.html
fredr_set_key("YOUR_FRED_API_KEY")

# UNRATE   = Civilian Unemployment Rate (monthly, seasonally adjusted)
# CPIAUCSL = Consumer Price Index for All Urban Consumers (monthly, SA)
unemployment <- fredr(
  series_id         = "UNRATE",
  observation_start = as.Date("1960-01-01")
)

cpi <- fredr(
  series_id         = "CPIAUCSL",
  observation_start = as.Date("1960-01-01")
)

# ── 2. COMPUTE INFLATION VARIABLES ───────────────────────────────────────────

# Year-over-year inflation rate from CPI
cpi <- cpi %>%
  mutate(inflation = (value / lag(value, 12) - 1) * 100)

# Month-over-month change in inflation (expectations-augmented version)
# Proxy for unexpected inflation: π_t - π_t-1
cpi <- cpi %>%
  mutate(inflation_change = inflation - lag(inflation, 1))

# ── 3. MERGE DATASETS ────────────────────────────────────────────────────────

unemployment <- unemployment %>% rename(unemp_rate = value)
cpi          <- cpi %>% rename(cpi_value = value)

phillips <- inner_join(
  unemployment %>% select(date, unemp_rate),
  cpi %>% select(date, inflation, inflation_change),
  by = "date"
) %>%
  filter(!is.na(inflation), !is.na(inflation_change))

# ── 4. SHARED THEME ──────────────────────────────────────────────────────────

theme_phillips <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "grey40", size = 10),
    plot.caption     = element_text(colour = "grey50", size = 8),
    panel.grid.minor = element_blank()
  )

# ── 5. PLOT 1: ORIGINAL PHILLIPS CURVE ───────────────────────────────────────
# Inflation level vs. unemployment — the classic formulation

plot1 <- ggplot(phillips, aes(x = unemp_rate, y = inflation)) +
  geom_point(alpha = 0.3, colour = "steelblue") +
  geom_smooth(method = "lm", colour = "red", se = TRUE) +
  labs(
    title    = "The Original Phillips Curve (1961–2025)",
    subtitle = "YoY Inflation vs. Unemployment Rate",
    x        = "Unemployment Rate (%)",
    y        = "Inflation Rate (%)",
    caption  = "Source: FRED (UNRATE, CPIAUCSL) | Analysis: Kamenov Insights"
  ) +
  theme_phillips

print(plot1)
ggsave("plot1_original_phillips.png", plot = plot1, width = 10, height = 5, dpi = 300)

# ── 6. PLOT 2: EXPECTATIONS-AUGMENTED PHILLIPS CURVE ─────────────────────────
# Change in inflation vs. unemployment — the Friedman-Phelps formulation

plot2 <- ggplot(phillips, aes(x = unemp_rate, y = inflation_change)) +
  geom_point(alpha = 0.3, colour = "darkorange") +
  geom_smooth(method = "lm", colour = "red", se = TRUE) +
  labs(
    title    = "Expectations-Augmented Phillips Curve (1961–2025)",
    subtitle = "Change in Inflation vs. Unemployment Rate",
    x        = "Unemployment Rate (%)",
    y        = "Change in Inflation (pp)",
    caption  = "Source: FRED (UNRATE, CPIAUCSL) | Analysis: Kamenov Insights"
  ) +
  theme_phillips

print(plot2)
ggsave("plot2_augmented_phillips.png", plot = plot2, width = 10, height = 5, dpi = 300)

# ── 7. REGRESSION RESULTS ────────────────────────────────────────────────────

# Original Phillips Curve: inflation ~ unemployment
model1 <- lm(inflation ~ unemp_rate, data = phillips)
cat("\n=== Original Phillips Curve ===\n")
print(summary(model1))

# Expectations-Augmented: change in inflation ~ unemployment
model2 <- lm(inflation_change ~ unemp_rate, data = phillips)
cat("\n=== Expectations-Augmented Phillips Curve ===\n")
print(summary(model2))

# ── 8. PLOT 3: PHILLIPS CURVE BY DECADE ──────────────────────────────────────
# Colour-coded by decade to show how the relationship shifted over time

phillips <- phillips %>%
  mutate(decade = paste0(floor(as.numeric(format(date, "%Y")) / 10) * 10, "s"))

plot3 <- ggplot(phillips, aes(x = unemp_rate, y = inflation, colour = decade)) +
  geom_point(alpha = 0.5) +
  labs(
    title    = "Phillips Curve by Decade (1961–2025)",
    x        = "Unemployment Rate (%)",
    y        = "Inflation Rate (%)",
    colour   = "Decade",
    caption  = "Source: FRED (UNRATE, CPIAUCSL) | Analysis: Kamenov Insights"
  ) +
  theme_phillips

print(plot3)
ggsave("plot3_decades.png", plot = plot3, width = 10, height = 5, dpi = 300)

# ── 9. PLOT 4: SUB-PERIOD REGRESSIONS ────────────────────────────────────────
# Three eras with separate regression lines

phillips <- phillips %>%
  mutate(era = case_when(
    date < as.Date("1975-01-01") ~ "1961–1974",
    date < as.Date("2000-01-01") ~ "1975–1999",
    TRUE                         ~ "2000–2025"
  ))

plot4 <- ggplot(phillips, aes(x = unemp_rate, y = inflation, colour = era)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  labs(
    title    = "Phillips Curve Across Three Eras",
    x        = "Unemployment Rate (%)",
    y        = "Inflation Rate (%)",
    colour   = "Era",
    caption  = "Source: FRED (UNRATE, CPIAUCSL) | Analysis: Kamenov Insights"
  ) +
  theme_phillips

print(plot4)
ggsave("plot4_eras.png", plot = plot4, width = 10, height = 5, dpi = 300)

# ── 10. PLOT 5: ROLLING 10-YEAR SLOPE ────────────────────────────────────────
# Tracks the Phillips Curve coefficient over time using a 120-month window
# Below zero = Phillips Curve works | Above zero = relationship inverted

rolling_slope <- rollapply(
  1:nrow(phillips),
  width = 120,
  FUN = function(i) {
    subset <- phillips[i, ]
    coef(lm(inflation ~ unemp_rate, data = subset))[2]
  },
  align = "right"
)

rolling_df <- data.frame(
  date  = phillips$date[120:nrow(phillips)],
  slope = rolling_slope
)

plot5 <- ggplot(rolling_df, aes(x = date, y = slope)) +
  geom_line(colour = "steelblue", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
  labs(
    title    = "Rolling Phillips Curve Slope (10-Year Window)",
    subtitle = "Coefficient of unemployment on inflation over time",
    x        = "Date",
    y        = "Slope Coefficient",
    caption  = "Source: FRED (UNRATE, CPIAUCSL) | Analysis: Kamenov Insights"
  ) +
  theme_phillips

print(plot5)
ggsave("plot5_rolling_slope.png", plot = plot5, width = 10, height = 5, dpi = 300)

# =============================================================================
# END
# =============================================================================
