library(tidyverse)
library(lubridate)
library(plm)
library(caret)
library(xtable)
library(stargazer)
library(car)
library(lmtest)
library(pROC)
library(sandwich)
library(modelsummary)
library(fixest)
library(pscl)

# =============================
# 1. Preprocessing
# =============================

# ---------------------------
# 1-1. Load Data
# ---------------------------

folder_path <- "C:/Users/yonwo/OneDrive/바탕 화면/PH.D/dissertation/Project/IPSA_China_discourse/Finalized Data/Data" # Desktop
folder_path <- "C:/Users/User/OneDrive/바탕 화면/PH.D/dissertation/Project/IPSA_China_discourse/Finalized Data/Data" # Laptop
event_chn <- read.csv(file.path(folder_path, "Polecat2021_2024.txt"), sep = "\t")
nationalism <- read.csv(file.path(folder_path, "nationalism.txt"), sep = "\t")

# ---------------------------
# 1-2. Match by Date
# ---------------------------
common_dates <- intersect(nationalism$date, event_chn$Event.Date)
nationalism <- nationalism |> filter(date %in% common_dates)
event_chn <- event_chn |> filter(Event.Date %in% common_dates)

# ---------------------------
# 1-3. IV (Frame Counts from Nationalism Data)
# ---------------------------
nationalism_clean <- nationalism |> 
  mutate(date = as.Date(date),
         year = year(date),
         month = month(date)) |> 
  select(title, year, month, frame, countries_str) |> 
  separate_rows(countries_str, sep = ",") |> 
  filter(countries_str != "", countries_str != "中国")

country_frame_counts <- nationalism_clean |> 
  group_by(year, month, countries_str, frame) |> 
  summarise(count = n(), .groups = "drop") |> 
  group_by(year, month, countries_str) |> 
  mutate(total_count = sum(count)) |> 
  ungroup()

# ---------------------------
# 1-4. DV (Event Data Aggregation)
# ---------------------------
event_group <- event_chn |> 
  select(Event.Date, Event.Type, Event.Intensity, Quad.Code, Actor.Country, Recipient.Country) |> 
  mutate(Event.Date = as.Date(Event.Date),
         year = year(Event.Date),
         month = month(Event.Date)) |> 
  separate_rows(Recipient.Country, sep = ";") |> 
  mutate(Recipient.Country = str_trim(Recipient.Country)) |> 
  filter(!is.na(Recipient.Country),
         Recipient.Country != "",
         !Recipient.Country %in% c("None", "China", "Hong Kong SAR China")) |> 
  group_by(year, month, Recipient.Country, Quad.Code) |> 
  summarise(count = n(), .groups = "drop") |> 
  group_by(year, month, Recipient.Country) |> 
  mutate(total_count = sum(count)) |> 
  ungroup()

# ---------------------------
# 1-5. Country Name Matching
# ---------------------------
country_list <- read.csv(file.path(folder_path, "english_chinese_country.csv"))
country_frame_counts <- country_frame_counts |> 
  rename(countries_chn = countries_str) |> 
  left_join(country_list, by = c("countries_chn" = "Chinese")) |> 
  rename(countries_en = English)

# ---------------------------
# 1-6. IV: Pivot to Wide & Lagging
# ---------------------------
iv_data <- country_frame_counts |> 
  pivot_wider(names_from = frame, values_from = count, values_fill = 0) |> 
  rename(Country = countries_en)

iv_data_lagged <- iv_data |> 
  arrange(Country, year, month) |> 
  group_by(Country) |> 
  mutate(
    Anti_foreign_t1 = lag(`Anti foreign`, 1),
    Anti_foreign_t2 = lag(`Anti foreign`, 2),
    Anti_foreign_t3 = lag(`Anti foreign`, 3),
    National_pride_t1 = lag(`national pride`, 1),
    National_pride_t2 = lag(`national pride`, 2),
    National_pride_t3 = lag(`national pride`, 3),
    National_revival_t1 = lag(`national revival`, 1),
    National_revival_t2 = lag(`national revival`, 2),
    National_revival_t3 = lag(`national revival`, 3)
  ) |> 
  ungroup()

# ---------------------------
# 1-7. DV: Pivot to Wide
# ---------------------------
dv_data <- event_group |> 
  pivot_wider(names_from = Quad.Code, values_from = count, values_fill = 0) |> 
  rename(Country = Recipient.Country)

# ---------------------------
# 1-8. Merge IV and DV
# ---------------------------
matched_countries <- intersect(unique(iv_data_lagged$Country), unique(dv_data$Country))
final_data <- dv_data |> filter(Country %in% matched_countries) |> 
  left_join(iv_data_lagged |> filter(Country %in% matched_countries), 
            by = c("Country", "year", "month"))

final_data <- final_data %>%
  select(-countries_chn) %>% 
  mutate(across(everything(), ~replace_na(.x, 0)))

# write.csv(
#   final_data,
#   file = file.path(folder_path, "final_data.csv"),
#   row.names = FALSE,
#   fileEncoding = "UTF-8"
# )

# =============================
# 2. Model Fitting
# =============================

final_data <- read.csv(file.path(folder_path, "final_data.csv"), sep = ",", header = TRUE)

# View(final_data)

# range(final_data$MATERIAL.CONFLICT)
# range(final_data$Anti.foreign)

# ---------------------------
# 2-1. Preparing dataset
# ---------------------------

analysis_data <- final_data |> 
  select(Country, year, month, 
         MATERIAL_CONFLICT = `MATERIAL.CONFLICT`, 
         MATERIAL_COOPERATION = `MATERIAL.COOPERATION`,
         starts_with("Anti_foreign"),
         starts_with("National_pride"),
         starts_with("National_revival"))

analysis_data <- analysis_data |> 
  mutate(time = as.Date(paste0(year, "-", month, "-01")),
         year_month = factor(paste0(year, "-", sprintf("%02d", month))))

pdata <- pdata.frame(analysis_data, index = c("Country", "time"))


# ---------------------------
# 2-1. Multicollinearity Diagnostics for Lagged Predictors
# ---------------------------

# Check 
# extract lagged variables
lag_vars <- analysis_data[, c(
  "Anti_foreign_t1", "Anti_foreign_t2", "Anti_foreign_t3",
  "National_pride_t1", "National_pride_t2", "National_pride_t3",
  "National_revival_t1", "National_revival_t2", "National_revival_t3"
)]

# Omit NA
lag_vars <- na.omit(lag_vars)

# Correlation 
cor_matrix <- cor(lag_vars)
print(cor_matrix)


# ---------------------------
# 2-2. Panel Fixed Effects Models
# ---------------------------

# MATERIAL_CONFLICT: FE Models by Lag
fe_conflict_t1 <- plm(MATERIAL_CONFLICT ~ Anti_foreign_t1 + National_pride_t1 + National_revival_t1 + factor(year_month),
                      data = pdata, model = "within")
bptest(fe_conflict_t1)
coeftest(fe_conflict_t1, vcov = vcovHC(fe_conflict_t1, method = "arellano", type = "HC1"))

# MATERIAL_COOPERATION: FE Models by Lag
fe_coop_t1 <- plm(MATERIAL_COOPERATION ~ Anti_foreign_t1 + National_pride_t1 + National_revival_t1 + factor(year_month),
                  data = pdata, model = "within")
bptest(fe_coop_t1)
coeftest(fe_coop_t1, vcov = vcovHC(fe_coop_t1, method = "arellano", type = "HC1"))

# ---------------------------
# 2-3. Model Diagnostics (Linear Fixed Effect Model)
# ---------------------------

# Pooling vs Fixed Effects (F-test, Hausman test)

# t1 Conflict
pool_conflict_t1 <- plm(MATERIAL_CONFLICT ~ Anti_foreign_t1 + National_pride_t1 + National_revival_t1 + factor(year_month),
                        data = pdata, model = "pooling")
pFtest(fe_conflict_t1, pool_conflict_t1)
phtest(fe_conflict_t1,
       plm(MATERIAL_CONFLICT ~ Anti_foreign_t1 + National_pride_t1 + National_revival_t1 + factor(year_month),
           data = pdata, model = "random"))

# t1 Cooperation
pool_coop_t1 <- plm(MATERIAL_COOPERATION ~ Anti_foreign_t1 + National_pride_t1 + National_revival_t1 + factor(year_month),
                    data = pdata, model = "pooling")
pFtest(fe_coop_t1, pool_coop_t1)
phtest(fe_coop_t1,
       plm(MATERIAL_COOPERATION ~ Anti_foreign_t1 + National_pride_t1 + National_revival_t1 + factor(year_month),
           data = pdata, model = "random"))


# ---------------------------
# 2-4. Summary Table (t1 models)
# ---------------------------

models_t1 <- list(
  "Conflict t1" = fe_conflict_t1,
  "Cooperation t1" = fe_coop_t1
)

modelsummary(models_t1,
             vcov = "HC1",
             stars = TRUE,
             output = "latex",
             title = "Fixed Effects Models (t1 Lag Only)",
             coef_omit = "factor")  # omit time fixed effects from output


# ---------------------------
# 2-5. Zero-Inflated Negative Binomial Models
# ---------------------------

zinb_conflict <- zeroinfl(
  MATERIAL_CONFLICT ~ Anti_foreign_t1 + National_pride_t1 + National_revival_t1 |
    Anti_foreign_t1 + National_pride_t1 + National_revival_t1,
  data = analysis_data,
  dist = "negbin"
)
summary(zinb_conflict)

zinb_coop <- zeroinfl(
  MATERIAL_COOPERATION ~ Anti_foreign_t1 + National_pride_t1 + National_revival_t1 |
    Anti_foreign_t1 + National_pride_t1 + National_revival_t1,
  data = analysis_data,
  dist = "negbin"
)
summary(zinb_coop)


# ---------------------------
# 2-6. Model Diagnostics (Zero-Inflated Model vs Hurdle Model)
# ---------------------------

###### AIC

# Conflict
hurdle_conflict <- hurdle(
  MATERIAL_CONFLICT ~ Anti_foreign_t1 + National_pride_t1 + National_revival_t1 |
    Anti_foreign_t1 + National_pride_t1 + National_revival_t1,
  data = analysis_data,
  dist = "negbin"
)

AIC(zinb_conflict, hurdle_conflict)


# Cooperation
hurdle_coop <- hurdle(
  MATERIAL_COOPERATION ~ Anti_foreign_t1 + National_pride_t1 + National_revival_t1 |
    Anti_foreign_t1 + National_pride_t1 + National_revival_t1,
  data = analysis_data,
  dist = "negbin"
)

AIC(zinb_coop, hurdle_coop)


###### Residual Plot

# Identify high-residual observations
resids <- residuals(zinb_conflict, type = "pearson")

high_residual_idx <- which(abs(resids) > 3)
analysis_data[high_residual_idx, ]

# Residual diagnostics: ZINB model
resid_zinb_conflict <- residuals(zinb_conflict, type = "pearson")
fitted_zinb_conflict <- fitted(zinb_conflict)

# Residuals vs Fitted plot (full range)
plot(fitted_zinb_conflict, resid_zinb_conflict,
     xlab = "Fitted values", ylab = "Pearson Residuals",
     main = "ZINB: Residuals vs Fitted (Conflict)")
abline(h = 0, lty = 2, col = "gray")

# Residuals vs Fitted plot (zoomed in: fitted values 0–50)
plot(fitted_zinb_conflict, resid_zinb_conflict,
     xlab = "Fitted values", ylab = "Pearson Residuals",
     main = "ZINB: Residuals vs Fitted (Conflict, Zoomed)",
     xlim = c(0, 50))
abline(h = 0, lty = 2, col = "gray")

# Residual diagnostics: Hurdle model
resid_hurdle_coop <- residuals(hurdle_coop, type = "pearson")
fitted_hurdle_coop <- fitted(hurdle_coop)

# Residuals vs Fitted plot (full range)
plot(fitted_hurdle_coop, resid_hurdle_coop,
     xlab = "Fitted values", ylab = "Pearson Residuals",
     main = "Hurdle: Residuals vs Fitted (Coop)")
abline(h = 0, lty = 2, col = "gray")

# Residuals vs Fitted plot (zoomed in: fitted values 0–50)
plot(fitted_hurdle_coop, resid_hurdle_coop,
     xlab = "Fitted values", ylab = "Pearson Residuals",
     main = "Hurdle: Residuals vs Fitted (Coop, Zoomed)",
     xlim = c(0, 50))
abline(h = 0, lty = 2, col = "gray")


# Identify extreme fitted values in Hurdle model
fitted_vals <- fitted(hurdle_conflict_clean)
which(fitted_vals > 5000)
analysis_data_clean[which(fitted_vals > 5000), ]

