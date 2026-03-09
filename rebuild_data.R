#!/usr/bin/env Rscript
# Comprehensive data rebuild for Politicide Dashboard
# IMPROVED alignment methodology:
#   - 5-year rolling window for alignment classification
#   - Minimum 5 trades threshold (was 2)
#   - Arms category weighting (aircraft/ships > sensors/engines)
#   - 3-year rolling average for time series (no more flickering)
#   - Continuous alignment score preserved

library(dplyr)
library(tidyr)
library(jsonlite)
library(countrycode)

cat("=== Loading V-Dem data ===\n")
library(vdemdata)
vdem_raw <- vdemdata::vdem
cat("V-Dem loaded:", nrow(vdem_raw), "rows,", ncol(vdem_raw), "columns\n")
cat("Year range:", min(vdem_raw$year), "-", max(vdem_raw$year), "\n")

# ============================================================
# 1. Extract V-Dem variables - political violence & repression
# ============================================================
cat("\n=== Extracting V-Dem variables ===\n")

wanted_vars <- c(
  "country_name", "country_text_id", "year",
  "v2clkill",     # Freedom from political killings
  "v2cltort",     # Freedom from torture
  "v2csreprss",   # CSO repression
  "v2x_clphy",    # Physical violence index
  "v2x_clpol",    # Political civil liberties index
  "v2x_rule",     # Rule of law index
  "v2mecenefm",   # Government censorship—Media
  "v2meharjrn",   # Harassment of journalists
  "v2x_freexp_altinf", # Freedom of expression
  "v2clacjust",   # Social class equality civil liberties
  "e_regionpol"   # Region (political)
)

available <- wanted_vars[wanted_vars %in% names(vdem_raw)]
missing <- wanted_vars[!wanted_vars %in% names(vdem_raw)]
cat("Available:", paste(available, collapse=", "), "\n")
if (length(missing) > 0) cat("Missing:", paste(missing, collapse=", "), "\n")

vdem <- vdem_raw %>%
  select(all_of(available)) %>%
  rename(
    cname = country_name,
    iso3 = country_text_id,
    pkill = v2clkill
  ) %>%
  filter(year >= 1950)

cat("V-Dem filtered:", nrow(vdem), "rows\n")
cat("Countries:", length(unique(vdem$iso3)), "\n")

# ============================================================
# 2. Load and process SIPRI trades
# ============================================================
cat("\n=== Loading SIPRI trades ===\n")

sipri <- fromJSON("/Users/favstats/Dropbox/besides/projects/Politicide_Dashboard/data/sipri_all_trades.json")
cat("SIPRI trades loaded:", nrow(sipri), "rows\n")

# Define Western seller countries
western_sellers <- c(
  "Australia", "Austria", "Belgium", "Canada", "Switzerland", "Cyprus",
  "Germany", "West Germany", "Denmark", "Spain", "Finland", "France",
  "United Kingdom", "Greece", "Ireland", "Iceland", "Italy",
  "Netherlands", "Norway", "New Zealand", "Portugal", "Sweden",
  "United States", "Czechia", "Czech Republic", "Poland", "Hungary",
  "Romania", "Bulgaria", "Slovakia", "Slovenia", "Croatia",
  "Estonia", "Latvia", "Lithuania", "Japan", "South Korea",
  "Israel", "Turkey"
)

# Define Russia/Soviet/China sellers
russian_sellers <- c(
  "Russia", "Soviet Union", "USSR", "China", "North Korea"
)

# ============================================================
# 2b. Arms category weighting
# ============================================================
# Major platforms (aircraft, ships, armored vehicles) represent
# deeper strategic relationships than spare parts/sensors
category_weights <- c(
  "Aircraft" = 3,
  "Ships" = 3,
  "Armoured vehicles" = 3,
  "Air-defence systems" = 3,
  "Missiles" = 2,
  "Artillery" = 2,
  "Naval weapons" = 2,
  "Sensors" = 1,
  "Engines" = 1,
  "Satellites" = 1,
  "Other" = 1
)

# Classify seller alliance and add weights
sipri_classified <- sipri %>%
  mutate(
    alliance = case_when(
      seller %in% western_sellers ~ "Western",
      seller %in% russian_sellers ~ "Russia",
      TRUE ~ "Other"
    ),
    weight = ifelse(!is.na(category), category_weights[category], 1),
    weight = ifelse(is.na(weight), 1, weight)
  ) %>%
  filter(alliance %in% c("Western", "Russia"))

cat("Classified trades - Western:", sum(sipri_classified$alliance == "Western"),
    "Russia:", sum(sipri_classified$alliance == "Russia"), "\n")

# Get buyer ISO3 codes
sipri_classified <- sipri_classified %>%
  mutate(iso3 = countrycode(buyer, "country.name", "iso3c", warn = FALSE))

# Remove non-state actors (NA iso3)
sipri_states <- sipri_classified %>% filter(!is.na(iso3))
cat("State trades:", nrow(sipri_states), "\n")

# Extract year from deliveryYr (use it if available, otherwise orderYr)
sipri_states <- sipri_states %>%
  mutate(year = as.integer(coalesce(deliveryYr, orderYr))) %>%
  filter(!is.na(year), year >= 1950, year <= 2024)

cat("Trades with valid years:", nrow(sipri_states), "\n")
cat("Category weight distribution:\n")
print(table(sipri_states$category, sipri_states$weight))

# ============================================================
# 3. Build alignment with 5-YEAR ROLLING WINDOW + WEIGHTING
# ============================================================
cat("\n=== Building alignment with 5-year rolling window ===\n")

# First: compute WEIGHTED trade counts per buyer-year-alliance
trades_yearly <- sipri_states %>%
  group_by(iso3, year, alliance) %>%
  summarise(
    n_trades = n(),
    weighted_trades = sum(weight, na.rm = TRUE),
    .groups = "drop"
  )

# Create complete grid: every country × every year that appears in data
all_countries <- unique(trades_yearly$iso3)
all_years <- min(trades_yearly$year):max(trades_yearly$year)

# For each country-year, sum weighted trades across a 5-year window (year-2 to year+2)
# EFFICIENT: use self-join with offset years instead of rowwise()
cat("Computing 5-year rolling windows (efficient join method)...\n")

# Pivot to wide format: one row per (iso3, year) with Western and Russia columns
trades_wide <- trades_yearly %>%
  pivot_wider(
    id_cols = c(iso3, year),
    names_from = alliance,
    values_from = c(n_trades, weighted_trades),
    values_fill = 0
  )

# Ensure columns exist (in case some data has only one alliance)
if (!"n_trades_Western" %in% names(trades_wide)) trades_wide$n_trades_Western <- 0
if (!"n_trades_Russia" %in% names(trades_wide)) trades_wide$n_trades_Russia <- 0
if (!"weighted_trades_Western" %in% names(trades_wide)) trades_wide$weighted_trades_Western <- 0
if (!"weighted_trades_Russia" %in% names(trades_wide)) trades_wide$weighted_trades_Russia <- 0

# Create offset copies and self-join for 5-year window
# For each year Y, we want sum of trades in [Y-2, Y+2]
offsets <- -2:2

rolling_parts <- lapply(offsets, function(off) {
  trades_wide %>%
    mutate(target_year = year - off) %>%  # This row's data contributes to target_year
    select(iso3, target_year,
           n_w = n_trades_Western, n_r = n_trades_Russia,
           wt_w = weighted_trades_Western, wt_r = weighted_trades_Russia)
})

# Combine all offset copies and sum by (iso3, target_year)
rolling_alignment <- bind_rows(rolling_parts) %>%
  group_by(iso3, year = target_year) %>%
  summarise(
    w_west = sum(wt_w, na.rm = TRUE),
    w_russia = sum(wt_r, na.rm = TRUE),
    n_west_raw = sum(n_w, na.rm = TRUE),
    n_russia_raw = sum(n_r, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    w_total = w_west + w_russia,
    n_total_raw = n_west_raw + n_russia_raw,
    # Continuous alignment score: -1 = fully Russia, +1 = fully Western
    alignment = ifelse(w_total > 0, (w_west - w_russia) / w_total, NA_real_)
  ) %>%
  # Only keep rows where we have SOME trade data in the window
  filter(w_total > 0)

cat("Rolling alignment computed:", nrow(rolling_alignment), "country-years\n")

# ============================================================
# 4. Classify alignment - MINIMUM 5 RAW TRADES in window
# ============================================================
cat("\n=== Classifying alignment (min 5 trades, 70% threshold) ===\n")

# Use 70% weighted threshold (slightly relaxed from 75% since we now
# have 5-year windows with category weighting)
trades_classified <- rolling_alignment %>%
  mutate(
    west_ratio = ifelse(w_total > 0, w_west / w_total, NA),
    russia_ratio = ifelse(w_total > 0, w_russia / w_total, NA),
    affil = case_when(
      n_total_raw < 5 ~ NA_character_,  # Need at least 5 raw trades in 5yr window
      west_ratio >= 0.70 ~ "Western Countries",
      russia_ratio >= 0.70 ~ "(Soviet) Russia",
      TRUE ~ NA_character_  # Mixed / non-aligned
    ),
    # For the scatter plot: perc_all = the dominant alliance's share
    perc_all = case_when(
      affil == "Western Countries" ~ west_ratio,
      affil == "(Soviet) Russia" ~ russia_ratio,
      TRUE ~ NA_real_
    )
  )

cat("Classified - Western:", sum(trades_classified$affil == "Western Countries", na.rm=TRUE),
    "Russian:", sum(trades_classified$affil == "(Soviet) Russia", na.rm=TRUE),
    "Unclassified:", sum(is.na(trades_classified$affil)), "\n")
cat("Min 5 trades filter removed:", sum(rolling_alignment$n_total_raw < 5), "country-years\n")

# ============================================================
# 4b. Case study validation
# ============================================================
cat("\n=== Case study validation ===\n")

check_country <- function(iso, name) {
  sub <- trades_classified %>% filter(iso3 == iso) %>% arrange(year)
  if (nrow(sub) == 0) { cat(name, ": NO DATA\n"); return() }
  cat(name, ":\n")
  # Show alignment transitions
  sub_affil <- sub %>% filter(!is.na(affil)) %>% select(year, affil, alignment, n_total_raw)
  if (nrow(sub_affil) > 0) {
    switches <- sub_affil %>% mutate(prev = lag(affil)) %>% filter(affil != prev, !is.na(prev))
    if (nrow(switches) > 0) {
      for (i in 1:nrow(switches)) {
        cat(sprintf("  Switch in %d: %s -> %s (n=%d, score=%.2f)\n",
            switches$year[i], switches$prev[i], switches$affil[i],
            switches$n_total_raw[i], switches$alignment[i]))
      }
    } else {
      main_affil <- names(sort(table(sub_affil$affil), decreasing=TRUE))[1]
      cat(sprintf("  Stable %s (%d-%d, n_years=%d)\n", main_affil,
          min(sub_affil$year), max(sub_affil$year), nrow(sub_affil)))
    }
  }
}

check_country("IRN", "Iran")
check_country("EGY", "Egypt")
check_country("IND", "India")
check_country("IRQ", "Iraq")
check_country("SYR", "Syria")
check_country("CUB", "Cuba")

# ============================================================
# 5. Merge with V-Dem data
# ============================================================
cat("\n=== Merging V-Dem + SIPRI ===\n")

merged <- vdem %>%
  left_join(
    trades_classified %>% select(iso3, year, affil, perc_all, alignment,
                                  w_west, w_russia, w_total, n_total_raw),
    by = c("iso3", "year")
  )

# Exclude SELLER / bloc-leader countries from analysis
# These countries ARE the blocs - including them is circular
# They stay on the map (arms_map) but not in the scatter/gap/analysis data
seller_isos <- c(
  # Western sellers
  "AUS", "AUT", "BEL", "CAN", "CHE", "CYP", "DEU", "DNK",
  "ESP", "FIN", "FRA", "GBR", "GRC", "IRL", "ISL", "ITA",
  "NLD", "NOR", "NZL", "PRT", "SWE", "USA", "JPN", "KOR",
  "ISR", "TUR",
  # Russia/China sellers
  "RUS", "CHN", "PRK"
)
cat("Excluding", length(seller_isos), "seller/bloc-leader countries from analysis\n")

# Final classified dataset (only where we have alignment AND pkill, excluding sellers)
final_data <- merged %>%
  filter(!is.na(affil), !is.na(pkill), !iso3 %in% seller_isos) %>%
  mutate(
    cname = case_when(
      grepl("Democratic People", cname) ~ "North Korea",
      grepl("German Democratic", cname) ~ "East Germany",
      cname == "Iran (Islamic Republic of)" ~ "Iran",
      grepl("Lao People", cname) ~ "Laos",
      cname == "Republic of Korea" ~ "South Korea",
      grepl("Korea, Republic", cname) ~ "South Korea",
      cname == "Syrian Arab Republic" ~ "Syria",
      grepl("Taiwan", cname) ~ "Taiwan",
      grepl("Venezuela", cname) ~ "Venezuela",
      grepl("Yemen People", cname) ~ "Yemen (Socialist)",
      grepl("Viet Nam", cname) ~ "Vietnam",
      grepl("Republic of the Congo", cname) ~ "Congo (DRC)",
      grepl("Burma", cname) ~ "Myanmar",
      grepl("Timor", cname) ~ "Timor-Leste",
      TRUE ~ cname
    )
  )

cat("Final classified dataset:", nrow(final_data), "rows\n")
cat("Countries:", length(unique(final_data$cname)), "\n")
cat("Year range:", min(final_data$year), "-", max(final_data$year), "\n")
cat("Western:", sum(final_data$affil == "Western Countries"),
    "Russian:", sum(final_data$affil == "(Soviet) Russia"), "\n")
cat("Mean pkill Western:", round(mean(final_data$pkill[final_data$affil == "Western Countries"], na.rm=TRUE), 3), "\n")
cat("Mean pkill Russian:", round(mean(final_data$pkill[final_data$affil == "(Soviet) Russia"], na.rm=TRUE), 3), "\n")

# ============================================================
# 6. Export main datasets as JSON
# ============================================================
cat("\n=== Exporting JSON files ===\n")
data_dir <- "/Users/favstats/Dropbox/besides/projects/Politicide_Dashboard/data"

# 6a. Final data for scatter plot
final_export <- final_data %>%
  select(cname, iso3, year, pkill, affil, perc_all, alignment,
         w_west, w_russia, w_total, n_total_raw,
         any_of(c("v2cltort", "v2csreprss", "v2x_clphy", "v2x_clpol",
                   "v2x_rule", "v2mecenefm", "v2meharjrn", "v2x_freexp_altinf")))

write_json(final_export, file.path(data_dir, "final_data_combined.json"), auto_unbox = TRUE)
cat("Wrote final_data_combined.json:", nrow(final_export), "rows\n")

# 6b. Time series (aggregated by affil + year) WITH 3-YEAR ROLLING AVERAGE
ts_raw <- final_data %>%
  group_by(affil, year) %>%
  summarise(
    mean_pkill = round(mean(pkill, na.rm=TRUE), 3),
    median_pkill = round(median(pkill, na.rm=TRUE), 3),
    n = n(),
    mean_torture = round(mean(v2cltort, na.rm=TRUE), 3),
    mean_phys_violence = round(mean(v2x_clphy, na.rm=TRUE), 3),
    mean_civil_lib = round(mean(v2x_clpol, na.rm=TRUE), 3),
    mean_rule_of_law = round(mean(v2x_rule, na.rm=TRUE), 3),
    mean_free_expression = round(mean(v2x_freexp_altinf, na.rm=TRUE), 3),
    mean_censorship = round(mean(v2mecenefm, na.rm=TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(affil, year)

# Add 3-year rolling averages (smoothed versions)
time_series <- ts_raw %>%
  group_by(affil) %>%
  mutate(
    smooth_pkill = round(zoo::rollmean(mean_pkill, k = 3, fill = NA, align = "center"), 3),
    smooth_phys_violence = round(zoo::rollmean(mean_phys_violence, k = 3, fill = NA, align = "center"), 3),
    smooth_civil_lib = round(zoo::rollmean(mean_civil_lib, k = 3, fill = NA, align = "center"), 3),
    smooth_rule_of_law = round(zoo::rollmean(mean_rule_of_law, k = 3, fill = NA, align = "center"), 3),
    smooth_free_expression = round(zoo::rollmean(mean_free_expression, k = 3, fill = NA, align = "center"), 3),
    smooth_torture = round(zoo::rollmean(mean_torture, k = 3, fill = NA, align = "center"), 3)
  ) %>%
  ungroup()

write_json(time_series, file.path(data_dir, "time_series_combined.json"), auto_unbox = TRUE)
cat("Wrote time_series_combined.json:", nrow(time_series), "rows\n")

# 6c. V-Dem map data (all countries, all years, multiple variables)
vdem_map <- vdem %>%
  select(cname, iso3, year, pkill,
         any_of(c("v2cltort", "v2x_clphy", "v2x_clpol", "v2x_rule")))

write_json(vdem_map, file.path(data_dir, "vdem_map.json"), auto_unbox = TRUE)
cat("Wrote vdem_map.json:", nrow(vdem_map), "rows\n")

# 6d. Arms alignment map (using 5-year rolling window scores)
western_isos <- c("AUS", "AUT", "BEL", "CAN", "CHE", "CYP", "DEU", "DNK",
                   "ESP", "FIN", "FRA", "GBR", "GRC", "IRL", "ISL", "ITA",
                   "NLD", "NOR", "NZL", "PRT", "SWE", "USA", "JPN", "KOR",
                   "ISR", "TUR")

arms_map <- trades_classified %>%
  mutate(
    # Override for seller countries
    alignment = case_when(
      iso3 %in% western_isos ~ 1.0,
      iso3 == "RUS" ~ -1.0,
      iso3 == "CHN" ~ -0.8,
      iso3 == "PRK" ~ -0.9,
      TRUE ~ alignment
    )
  ) %>%
  select(iso3, year, alignment, w_west, w_russia, w_total, n_total_raw) %>%
  mutate(cname = countrycode(iso3, "iso3c", "country.name", warn = FALSE))

write_json(arms_map, file.path(data_dir, "arms_map_combined.json"), auto_unbox = TRUE)
cat("Wrote arms_map_combined.json:", nrow(arms_map), "rows\n")

# Quick check: Russia-aligned countries in the map
russia_heavy <- arms_map %>% filter(alignment < -0.5, year == 2000) %>% arrange(alignment)
cat("Sample Russia-aligned countries (year 2000):\n")
print(head(russia_heavy, 10))

# 6e. Summary stats
summary_stats <- list(
  total_country_years = nrow(final_data),
  year_range = c(min(final_data$year), max(final_data$year)),
  n_countries = length(unique(final_data$cname)),
  n_western = sum(final_data$affil == "Western Countries"),
  n_russian = sum(final_data$affil == "(Soviet) Russia"),
  mean_pkill_western = round(mean(final_data$pkill[final_data$affil == "Western Countries"], na.rm=TRUE), 2),
  mean_pkill_russian = round(mean(final_data$pkill[final_data$affil == "(Soviet) Russia"], na.rm=TRUE), 2),
  methodology = list(
    window = "5-year rolling (year-2 to year+2)",
    min_trades = 5,
    threshold = "70% weighted ratio",
    weighting = "Aircraft/Ships/Armored/AirDef: 3x, Missiles/Artillery/Naval: 2x, Sensors/Engines: 1x"
  ),
  variables = list(
    pkill = "Freedom from political killings (v2clkill)",
    torture = "Freedom from torture (v2cltort)",
    phys_violence = "Physical violence index (v2x_clphy)",
    civil_lib = "Political civil liberties (v2x_clpol)",
    rule_of_law = "Rule of law index (v2x_rule)",
    censorship = "Government censorship—Media (v2mecenefm)",
    journalist_harassment = "Harassment of journalists (v2meharjrn)",
    free_expression = "Freedom of expression (v2x_freexp_altinf)"
  )
)

write_json(summary_stats, file.path(data_dir, "summary_stats_combined.json"), auto_unbox = TRUE, pretty = TRUE)
cat("Wrote summary_stats_combined.json\n")

# 6f. Country summary
country_summary <- final_data %>%
  group_by(cname, iso3, affil) %>%
  summarise(
    n_years = n(),
    mean_pkill = round(mean(pkill, na.rm=TRUE), 3),
    mean_torture = round(mean(v2cltort, na.rm=TRUE), 3),
    mean_phys_violence = round(mean(v2x_clphy, na.rm=TRUE), 3),
    mean_civil_lib = round(mean(v2x_clpol, na.rm=TRUE), 3),
    year_min = min(year),
    year_max = max(year),
    .groups = "drop"
  ) %>%
  arrange(affil, mean_pkill)

write_json(country_summary, file.path(data_dir, "country_summary_combined.json"), auto_unbox = TRUE)
cat("Wrote country_summary_combined.json:", nrow(country_summary), "rows\n")

# ============================================================
# 7. Additional analysis datasets
# ============================================================
cat("\n=== Creating additional analysis datasets ===\n")

# 7a. Era comparison (Cold War vs Post-Cold War vs Recent)
era_data <- final_data %>%
  mutate(era = case_when(
    year <= 1991 ~ "Cold War (1950-1991)",
    year <= 2013 ~ "Post-Cold War (1992-2013)",
    TRUE ~ "Recent (2014-2024)"
  )) %>%
  group_by(era, affil) %>%
  summarise(
    n = n(),
    mean_pkill = round(mean(pkill, na.rm=TRUE), 3),
    median_pkill = round(median(pkill, na.rm=TRUE), 3),
    sd_pkill = round(sd(pkill, na.rm=TRUE), 3),
    mean_torture = round(mean(v2cltort, na.rm=TRUE), 3),
    mean_phys_violence = round(mean(v2x_clphy, na.rm=TRUE), 3),
    mean_civil_lib = round(mean(v2x_clpol, na.rm=TRUE), 3),
    mean_rule_of_law = round(mean(v2x_rule, na.rm=TRUE), 3),
    .groups = "drop"
  )

write_json(era_data, file.path(data_dir, "era_comparison.json"), auto_unbox = TRUE, pretty = TRUE)
cat("Wrote era_comparison.json\n")
print(era_data)

# 7b. Alignment switchers (countries that switched alignment)
switcher_data <- final_data %>%
  select(cname, iso3, year, affil, pkill) %>%
  group_by(cname) %>%
  mutate(
    prev_affil = lag(affil, order_by = year),
    switched = affil != prev_affil & !is.na(prev_affil)
  ) %>%
  filter(switched) %>%
  select(cname, iso3, year, from = prev_affil, to = affil, pkill) %>%
  ungroup()

write_json(switcher_data, file.path(data_dir, "alignment_switchers.json"), auto_unbox = TRUE)
cat("Wrote alignment_switchers.json:", nrow(switcher_data), "switches\n")

# 7c. Regional breakdown
region_names <- c(
  "1" = "Eastern Europe & Central Asia",
  "2" = "Latin America & Caribbean",
  "3" = "Middle East & North Africa",
  "4" = "Sub-Saharan Africa",
  "5" = "Western Europe & North America",
  "6" = "East Asia",
  "7" = "South-East Asia",
  "8" = "South Asia",
  "9" = "Pacific",
  "10" = "Caribbean"
)

regional_data <- final_data %>%
  mutate(region = region_names[as.character(e_regionpol)]) %>%
  filter(!is.na(region)) %>%
  group_by(region, affil) %>%
  summarise(
    n = n(),
    mean_pkill = round(mean(pkill, na.rm=TRUE), 3),
    mean_phys_violence = round(mean(v2x_clphy, na.rm=TRUE), 3),
    mean_civil_lib = round(mean(v2x_clpol, na.rm=TRUE), 3),
    .groups = "drop"
  )

write_json(regional_data, file.path(data_dir, "regional_breakdown.json"), auto_unbox = TRUE, pretty = TRUE)
cat("Wrote regional_breakdown.json:", nrow(regional_data), "rows\n")

# 7d. Top countries (most extreme cases)
top_countries <- final_data %>%
  group_by(cname, iso3, affil) %>%
  summarise(
    n_years = n(),
    mean_pkill = round(mean(pkill, na.rm=TRUE), 3),
    mean_torture = round(mean(v2cltort, na.rm=TRUE), 3),
    mean_phys_violence = round(mean(v2x_clphy, na.rm=TRUE), 3),
    .groups = "drop"
  ) %>%
  filter(n_years >= 3) %>%
  arrange(mean_pkill)

write_json(top_countries, file.path(data_dir, "top_countries.json"), auto_unbox = TRUE)
cat("Wrote top_countries.json:", nrow(top_countries), "rows\n")

# 7e. Multi-variable comparison
multi_var <- final_data %>%
  select(cname, iso3, year, affil,
         pkill, v2cltort, v2x_clphy, v2x_clpol, v2x_rule,
         v2mecenefm, v2meharjrn, v2x_freexp_altinf) %>%
  pivot_longer(
    cols = c(pkill, v2cltort, v2x_clphy, v2x_clpol, v2x_rule,
             v2mecenefm, v2meharjrn, v2x_freexp_altinf),
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  mutate(
    var_label = case_when(
      variable == "pkill" ~ "Political Killings",
      variable == "v2cltort" ~ "Torture",
      variable == "v2x_clphy" ~ "Physical Violence",
      variable == "v2x_clpol" ~ "Civil Liberties",
      variable == "v2x_rule" ~ "Rule of Law",
      variable == "v2mecenefm" ~ "Media Censorship",
      variable == "v2meharjrn" ~ "Journalist Harassment",
      variable == "v2x_freexp_altinf" ~ "Free Expression",
      TRUE ~ variable
    )
  ) %>%
  group_by(affil, variable, var_label) %>%
  summarise(
    mean_val = round(mean(value, na.rm=TRUE), 3),
    median_val = round(median(value, na.rm=TRUE), 3),
    sd_val = round(sd(value, na.rm=TRUE), 3),
    n = n(),
    .groups = "drop"
  )

write_json(multi_var, file.path(data_dir, "multi_variable_comparison.json"), auto_unbox = TRUE, pretty = TRUE)
cat("Wrote multi_variable_comparison.json\n")
print(multi_var %>% arrange(variable, affil))

# 7f. Decade-level trends
decade_trends <- final_data %>%
  mutate(decade = paste0(floor(year/10)*10, "s")) %>%
  group_by(decade, affil) %>%
  summarise(
    n = n(),
    mean_pkill = round(mean(pkill, na.rm=TRUE), 3),
    mean_torture = round(mean(v2cltort, na.rm=TRUE), 3),
    mean_phys_violence = round(mean(v2x_clphy, na.rm=TRUE), 3),
    mean_civil_lib = round(mean(v2x_clpol, na.rm=TRUE), 3),
    mean_rule_of_law = round(mean(v2x_rule, na.rm=TRUE), 3),
    mean_free_expression = round(mean(v2x_freexp_altinf, na.rm=TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(decade, affil)

write_json(decade_trends, file.path(data_dir, "decade_trends.json"), auto_unbox = TRUE, pretty = TRUE)
cat("Wrote decade_trends.json\n")
print(decade_trends)

# ============================================================
# 7g. Country alignment timeline (for case study explorer)
# ============================================================
cat("\n=== Creating country alignment timeline ===\n")

country_timeline <- trades_classified %>%
  filter(!is.na(alignment)) %>%
  mutate(cname = countrycode(iso3, "iso3c", "country.name", warn = FALSE)) %>%
  select(cname, iso3, year, alignment, affil, w_west, w_russia, n_total_raw) %>%
  arrange(iso3, year)

write_json(country_timeline, file.path(data_dir, "country_alignment_timeline.json"), auto_unbox = TRUE)
cat("Wrote country_alignment_timeline.json:", nrow(country_timeline), "rows\n")

cat("\n=== ALL DONE! ===\n")
cat("Total files written: 13\n")
cat("Dashboard data directory:", data_dir, "\n")
cat("\nMethodology summary:\n")
cat("  - 5-year rolling window (year-2 to year+2)\n")
cat("  - Minimum 5 raw trades in window to classify\n")
cat("  - Category weighting: Aircraft/Ships/Armor/AirDef=3x, Missiles/Artillery/Naval=2x, Sensors/Engines=1x\n")
cat("  - 70% weighted ratio threshold for binary classification\n")
cat("  - 3-year rolling average for time series smoothing\n")
