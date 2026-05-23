# Mateo Gudiño Alvarado
# The following code corresponds to the regressions part of my undergrad thesis 
# project under Dr. Horacio Larreguy Arbesú at ITAM



setwd("C:/Users/mateo/Dropbox/Microeconometría Aplicada/Código")
base = read.csv("C:/Users/mateo/Dropbox/Microeconometría Aplicada/Código/Final_Dataset.csv")

# NOTE: no relative paths are used as per requested for the class this was made for

library(tidyverse)
library(plm)
library(haven)
library(fixest)
library(lfe)
library(stargazer)
library(dplyr)
library(varhandle)
library(didimputation)
library(did)
library(ggplot2)
library(xtable)
#remotes::install_github("shuo-zhang-ucsb/did_multiplegt")
#install.packages("DIDmultiplegtDYN")
library(DIDmultiplegtDYN)
#library("DIDmultiplegt")
library(broom)

# Remove Always treated
base <- base %>%
  filter(!(Muns %in% Muns[Anio == 1990 & DTO_dummy == 1 ]))


# Treatment variable
base <- base %>%
  group_by(Muns) %>%
  mutate(
    first_treated = ifelse(any(DTO_dummy == 1), min(Anio[DTO_dummy == 1]), 0)
  )
base <- ungroup(base)

base <- base %>%
  group_by(Muns) %>%
  mutate(
    first_simple = ifelse(any(Simple == 1), min(Anio[Simple == 1]), 0)
  )
base <- ungroup(base)

base <- base %>%
  group_by(Muns) %>%
  mutate(
    first_multiple = ifelse(any(Multiple == 1), min(Anio[Multiple == 1]), 0)
  )

# Separar base para el Censo, bianual
# Separate biannual census
años_deseados = c(2010, 2012, 2014, 2016, 2018, 2020)
base_censo = base %>%
  filter(Anio %in% años_deseados)# %>% 

años_deseados_ganado_carreteras = c(2011:2017)
base_ganado_carreteras = base %>% 
  filter(Anio %in% años_deseados_ganado_carreteras)

años_deseados_crimenes = c(2011:2019)
base_crimenes = base %>% 
  filter(Anio %in% años_deseados_crimenes)


años_deseados_safetyonly = c(2001:2019)
base_safetyonly = base %>%
  filter(Anio %in% años_deseados_safetyonly)

base <- ungroup(base)




# The code proceeds as follows:
# - Run each variable's event study for 4 different specifications of the DiD:
#     * De Chaisemartin & D'Hoeltfoeuille (2020)
#     * De Chaisemartin & D'Hoeltfoeuille (2024)
#     * Callaway & Sant'Anna
#     * Borusyak, Jaravel & Speiss
# - Plot the 3 event studies in one single overlapping plot
# - Generate a table for the coefficients of the 3 regressions



### SAFETY EXPENDITURE

# Callaway
event_calSafetyAll <- att_gt(yname = "logSafetyAll_exp",
                             gname = "first_treated",
                             idname = "Muns",
                             tname = "Anio",
                             xformla = ~1,
                             data = base,
                             allow_unbalanced_panel = TRUE,
                             est_method = "reg",
                             control_group = c("notyettreated","nevertreated")
)

mw.dyn = aggte(event_calSafetyAll, type = "dynamic", na.rm = TRUE)

calSafetyAll = aggte(event_calSafetyAll, type = "simple", na.rm = TRUE)
ggdid(mw.dyn,
      title = "Event Study - Safety expenditure (All categories)",
      xlab = "Period",
      ylab = "Coefficient",
      xgap = 4)

# Borusyak

event_bor_safetyall = did_imputation(base,
                                     yname = "logSafetyAll_exp",
                                     gname =  "first_treated",
                                     tname = "Anio",
                                     idname = "Muns",
                                     first_stage = ~0 |
                                       Muns + Anio,
                                     horizon = TRUE,
                                     pretrends = -28:-1) # No sé por qué con TRUE explota la desviación estándar

bor_safetyall = did_imputation(base,
                               yname = "logSafetyAll_exp",
                               gname = "first_treated",
                               tname = "Anio",
                               idname = "Muns",
                               first_stage = ~0 | 
                                 Muns + Anio,
)

# Chaisemartin 2020

event_chaiseSafetyAll <- 
  DIDmultiplegtDYN::did_multiplegt_dyn(
    df = base,
    outcome = "logSafetyAll_exp",
    group = "Muns",
    time = "Anio",
    treatment = "DTO_dummy",
    effects = 14,
    placebo = 14
  )

event_chaiseSafetyAll$results
event_chaiseSafetyAll$plot

# Chaisemartin 2024

event_intSafetyAll <- 
  DIDmultiplegtDYN::did_multiplegt_dyn(
    df = base,
    outcome = "logSafetyAll_exp",
    group = "Muns",
    time = "Anio",
    treatment = "DTO_dummy_cont",
    effects = 14,
    placebo = 12
  )

event_intSafetyAll$results
event_intSafetyAll$plot




library(dplyr)
library(ggplot2)


# 1. Callaway & Sant’Anna
callaway_df <- data.frame(
  period = as.numeric(mw.dyn$egt),
  estimate = as.numeric(mw.dyn$att.egt),
  se = as.numeric(mw.dyn$se.egt),
  Method = "Callaway & Sant'Anna (2021)"
) %>%
  mutate(
    conf.low = as.numeric(estimate - 1.96 * se),
    conf.high = as.numeric(estimate + 1.96 * se)
  )

# 2. Borusyak et al.
borusyak_df <- event_bor_safetyall %>%
  mutate(
    Method = "Borusyak et al (2021)",
    period = as.numeric(term),
    estimate = as.numeric(estimate),
    se = as.numeric(std.error),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high)
  ) %>%
  select(Method, period, estimate, se, conf.low, conf.high) %>%
  filter(!is.na(estimate))  # Elimina periodos sin estimaciones

# 3. Chaisemartin & D’Haultfoeuille 2020
chaise2020_placebos <- as.data.frame(event_chaiseSafetyAll$results$Placebos) %>%
  mutate(
    period = -as.numeric(gsub("Placebo_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2020_effects <- as.data.frame(event_chaiseSafetyAll$results$Effects) %>%
  mutate(
    period = as.numeric(gsub("Effect_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2020_df <- bind_rows(chaise2020_placebos, chaise2020_effects) %>%
  mutate(Method = "de Chaisemartin (2020)")

# 4. Chaisemartin & D’Haultfoeuille 2024
chaise2024_placebos <- as.data.frame(event_intSafetyAll$results$Placebos) %>%
  mutate(
    period = -as.numeric(gsub("Placebo_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2024_effects <- as.data.frame(event_intSafetyAll$results$Effects) %>%
  mutate(
    period = as.numeric(gsub("Effect_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2024_df <- bind_rows(chaise2024_placebos, chaise2024_effects) %>%
  mutate(Method = " de Chaisemartin (2024)")

# Combinar todos los data frames
# Combine dataframes
event_studies_df <- bind_rows(callaway_df, borusyak_df, chaise2020_df, chaise2024_df)

# 5. Plot
p<-ggplot(event_studies_df, aes(x = period, y = estimate, color = Method, shape = Method)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.4) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(

           x = "Time to treatment",
       y = "Coefficient") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 13),       # Tamaño del texto de la leyenda
    legend.title = element_text(size = 15),      # Tamaño del título de la leyenda
    axis.title.x = element_text(size = 15),      # Tamaño del título del eje x
    axis.title.y = element_text(size = 15),      # Tamaño del título del eje y
    axis.text.x = element_text(size = 14),       # Tamaño de los valores del eje x
    axis.text.y = element_text(size = 14)        # Tamaño de los valores del eje y
  )
ggsave(
  filename = "C:/Users/mateo/Desktop/Figuras finales/event_study_safetyall.jpeg", 
  plot = p, 
  width = 12, 
  height = 7, 
  dpi = 300
)

# PLOT ATT

library(ggplot2)



# Para calSafetyAll
ATT_calSafetyAll <- calSafetyAll$overall.att
SE_calSafetyAll <- calSafetyAll$overall.se
CI_lower_calSafetyAll <- ATT_calSafetyAll - 1.96 * SE_calSafetyAll
CI_upper_calSafetyAll <- ATT_calSafetyAll + 1.96 * SE_calSafetyAll

# Para bor_safetyall
ATT_bor_safetyall <- bor_safetyall$estimate[bor_safetyall$term == "treat"]
SE_bor_safetyall <- bor_safetyall$std.error[bor_safetyall$term == "treat"]
CI_lower_bor_safetyall <- ATT_bor_safetyall - 1.96 * SE_bor_safetyall
CI_upper_bor_safetyall <- ATT_bor_safetyall + 1.96 * SE_bor_safetyall

# Para event_chaiseSafetyAll (averaged treatment effect)
ATT_event_chaiseSafetyAll <- event_chaiseSafetyAll$results$ATE[1, "Estimate"]
SE_event_chaiseSafetyAll <- event_chaiseSafetyAll$results$ATE[1, "SE"]
CI_lower_event_chaiseSafetyAll <- event_chaiseSafetyAll$results$ATE[1, "LB CI"]
CI_upper_event_chaiseSafetyAll <- event_chaiseSafetyAll$results$ATE[1, "UB CI"]

# Para event_intSafetyAll (averaged treatment effect)
ATT_event_intSafetyAll <- event_intSafetyAll$results$ATE[1, "Estimate"]
SE_event_intSafetyAll <- event_intSafetyAll$results$ATE[1, "SE"]
CI_lower_event_intSafetyAll <- ATT_event_intSafetyAll - 1.96 * SE_event_intSafetyAll
CI_upper_event_intSafetyAll <- ATT_event_intSafetyAll + 1.96 * SE_event_intSafetyAll

# Create Plottable dataframe
results_df <- data.frame(
  Method = c("Callaway & Sant'Anna (2021)", "Borusyak et al (2021)", "de Chaisemartin (2020)", "de Chaisemartin (2024)"),
  ATT = c(ATT_calSafetyAll, ATT_bor_safetyall, ATT_event_chaiseSafetyAll, ATT_event_intSafetyAll),
  SE = c(SE_calSafetyAll, SE_bor_safetyall, SE_event_chaiseSafetyAll, SE_event_intSafetyAll),
  CI_lower = c(CI_lower_calSafetyAll, CI_lower_bor_safetyall, CI_lower_event_chaiseSafetyAll, CI_lower_event_intSafetyAll),
  CI_upper = c(CI_upper_calSafetyAll, CI_upper_bor_safetyall, CI_upper_event_chaiseSafetyAll, CI_upper_event_intSafetyAll)
)

p<-ggplot(results_df, aes(y = Method, x = ATT, xmin = CI_lower, xmax = CI_upper)) +
  geom_pointrange() +
  geom_text(aes(label = round(ATT, 3), y = Method), vjust = -1, size = 5.5) +  # vjust controla la posición vertical
  theme_minimal() +
  geom_vline(xintercept = 0) +
  labs(
   # title = "ATT Estimates with 95% Confidence Intervals", 
    y = "Method", 
    x = "Average Treatment Effect (ATT)"
  ) +
  theme(
    axis.title.x = element_text(size = 15),      # Título del eje x
    axis.title.y = element_text(size = 15),      # Título del eje y
    axis.text.x  = element_text(size = 15),      # Etiquetas del eje x
    axis.text.y  = element_text(size = 15),      # Etiquetas del eje y
    plot.title   = element_text(size = 16, face = "bold")  # Título del gráfico
  )
ggsave(
  filename = "C:/Users/mateo/Desktop/Figuras finales/coefs_safetyall.jpeg", 
  plot = p, 
  width = 12, 
  height = 7, 
  dpi = 300
)





### SAFETY EXPENDITURE

# Callaway
event_calDummy_Safety <- att_gt(yname = "Dummy_Safety",
                                gname = "first_treated",
                                idname = "Muns",
                                tname = "Anio",
                                xformla = ~1,
                                data = base,
                                allow_unbalanced_panel = TRUE,
                                est_method = "reg",
                                control_group = c("notyettreated","nevertreated")
)

mw.dyn = aggte(event_calDummy_Safety, type = "dynamic", na.rm = TRUE)

calDummy_Safety = aggte(event_calDummy_Safety, type = "simple", na.rm = TRUE)
ggdid(mw.dyn,
      title = "Event Study - Safety expenditure (All categories)",
      xlab = "Period",
      ylab = "Coefficient",
      xgap = 4)

# Borusyak

event_bor_Dummy_Safety = did_imputation(base,
                                        yname = "Dummy_Safety",
                                        gname =  "first_treated",
                                        tname = "Anio",
                                        idname = "Muns",
                                        first_stage = ~0 |
                                          Muns + Anio,
                                        horizon = TRUE,
                                        pretrends = -28:-1) # No sé por qué con TRUE explota la desviación estándar

bor_Dummy_Safety = did_imputation(base,
                                  yname = "Dummy_Safety",
                                  gname = "first_treated",
                                  tname = "Anio",
                                  idname = "Muns",
                                  first_stage = ~0 | 
                                    Muns + Anio,
)

# Chaisemartin 2020

event_chaiseDummy_Safety <- 
  DIDmultiplegtDYN::did_multiplegt_dyn(
    df = base,
    outcome = "Dummy_Safety",
    group = "Muns",
    time = "Anio",
    treatment = "DTO_dummy",
    effects = 14,
    placebo = 14
  )

event_chaiseDummy_Safety$results
event_chaiseDummy_Safety$plot

# Chaisemartin 2024

event_intDummy_Safety <- 
  DIDmultiplegtDYN::did_multiplegt_dyn(
    df = base,
    outcome = "Dummy_Safety",
    group = "Muns",
    time = "Anio",
    treatment = "DTO_dummy_cont",
    effects = 14,
    placebo = 12
  )

event_intDummy_Safety$results
event_intDummy_Safety$plot


# Event Study Plots

library(dplyr)
library(ggplot2)


# 1. Callaway & Sant’Anna
callaway_df <- data.frame(
  period = as.numeric(mw.dyn$egt),
  estimate = as.numeric(mw.dyn$att.egt),
  se = as.numeric(mw.dyn$se.egt),
  Method = "Callaway & Sant'Anna (2021)"
) %>%
  mutate(
    conf.low = as.numeric(estimate - 1.96 * se),
    conf.high = as.numeric(estimate + 1.96 * se)
  )

# 2. Borusyak et al.
borusyak_df <- event_bor_Dummy_Safety %>%
  mutate(
    Method = "Borusyak et al (2021)",
    period = as.numeric(term),
    estimate = as.numeric(estimate),
    se = as.numeric(std.error),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high)
  ) %>%
  select(Method, period, estimate, se, conf.low, conf.high) %>%
  filter(!is.na(estimate))  # Elimina periodos sin estimaciones

# 3. Chaisemartin & D’Haultfoeuille 2020
chaise2020_placebos <- as.data.frame(event_chaiseDummy_Safety$results$Placebos) %>%
  mutate(
    period = -as.numeric(gsub("Placebo_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2020_effects <- as.data.frame(event_chaiseDummy_Safety$results$Effects) %>%
  mutate(
    period = as.numeric(gsub("Effect_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2020_df <- bind_rows(chaise2020_placebos, chaise2020_effects) %>%
  mutate(Method = "de Chaisemartin (2020)")

# 4. Chaisemartin & D’Haultfoeuille 2024
chaise2024_placebos <- as.data.frame(event_intDummy_Safety$results$Placebos) %>%
  mutate(
    period = -as.numeric(gsub("Placebo_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2024_effects <- as.data.frame(event_intDummy_Safety$results$Effects) %>%
  mutate(
    period = as.numeric(gsub("Effect_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2024_df <- bind_rows(chaise2024_placebos, chaise2024_effects) %>%
  mutate(Method = " de Chaisemartin (2024)")

# Combinar todos los data frames
event_studies_df <- bind_rows(callaway_df, borusyak_df, chaise2020_df, chaise2024_df)

# 5. Graficar
p<-ggplot(event_studies_df, aes(x = period, y = estimate, color = Method, shape = Method)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.4) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(
    #title = "Comparación de Event Studies",
    x = "Time to treatment",
    y = "Coefficient") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 13),       # Tamaño del texto de la leyenda
    legend.title = element_text(size = 15),      # Tamaño del título de la leyenda
    axis.title.x = element_text(size = 15),      # Tamaño del título del eje x
    axis.title.y = element_text(size = 15),      # Tamaño del título del eje y
    axis.text.x = element_text(size = 14),       # Tamaño de los valores del eje x
    axis.text.y = element_text(size = 14)        # Tamaño de los valores del eje y
  )
ggsave(
  filename = "C:/Users/mateo/Desktop/Figuras finales/event_study_Dummy_Safety.jpeg", 
  plot = p, 
  width = 12, 
  height = 7, 
  dpi = 300
)

# PLOT DEL AVERAGE TREATMENT EFFECT ON THE TREATED

library(ggplot2)

# Extraer los valores de ATT y su error estándar desde los objetos

# Para calDummy_Safety
ATT_calDummy_Safety <- calDummy_Safety$overall.att
SE_calDummy_Safety <- calDummy_Safety$overall.se
CI_lower_calDummy_Safety <- ATT_calDummy_Safety - 1.96 * SE_calDummy_Safety
CI_upper_calDummy_Safety <- ATT_calDummy_Safety + 1.96 * SE_calDummy_Safety

# Para bor_Dummy_Safety
ATT_bor_Dummy_Safety <- bor_Dummy_Safety$estimate[bor_Dummy_Safety$term == "treat"]
SE_bor_Dummy_Safety <- bor_Dummy_Safety$std.error[bor_Dummy_Safety$term == "treat"]
CI_lower_bor_Dummy_Safety <- ATT_bor_Dummy_Safety - 1.96 * SE_bor_Dummy_Safety
CI_upper_bor_Dummy_Safety <- ATT_bor_Dummy_Safety + 1.96 * SE_bor_Dummy_Safety

# Para event_chaiseDummy_Safety (averaged treatment effect)
ATT_event_chaiseDummy_Safety <- event_chaiseDummy_Safety$results$ATE[1, "Estimate"]
SE_event_chaiseDummy_Safety <- event_chaiseDummy_Safety$results$ATE[1, "SE"]
CI_lower_event_chaiseDummy_Safety <- event_chaiseDummy_Safety$results$ATE[1, "LB CI"]
CI_upper_event_chaiseDummy_Safety <- event_chaiseDummy_Safety$results$ATE[1, "UB CI"]

# Para event_intDummy_Safety (averaged treatment effect)
ATT_event_intDummy_Safety <- event_intDummy_Safety$results$ATE[1, "Estimate"]
SE_event_intDummy_Safety <- event_intDummy_Safety$results$ATE[1, "SE"]
CI_lower_event_intDummy_Safety <- ATT_event_intDummy_Safety - 1.96 * SE_event_intDummy_Safety
CI_upper_event_intDummy_Safety <- ATT_event_intDummy_Safety + 1.96 * SE_event_intDummy_Safety

# Crear el dataframe para graficar
results_df <- data.frame(
  Method = c("Callaway & Sant'Anna (2021)", "Borusyak et al (2021)", "de Chaisemartin (2020)", "de Chaisemartin (2024)"),
  ATT = c(ATT_calDummy_Safety, ATT_bor_Dummy_Safety, ATT_event_chaiseDummy_Safety, ATT_event_intDummy_Safety),
  SE = c(SE_calDummy_Safety, SE_bor_Dummy_Safety, SE_event_chaiseDummy_Safety, SE_event_intDummy_Safety),
  CI_lower = c(CI_lower_calDummy_Safety, CI_lower_bor_Dummy_Safety, CI_lower_event_chaiseDummy_Safety, CI_lower_event_intDummy_Safety),
  CI_upper = c(CI_upper_calDummy_Safety, CI_upper_bor_Dummy_Safety, CI_upper_event_chaiseDummy_Safety, CI_upper_event_intDummy_Safety)
)

p<-ggplot(results_df, aes(y = Method, x = ATT, xmin = CI_lower, xmax = CI_upper)) +
  geom_pointrange() +
  geom_text(aes(label = round(ATT, 3), y = Method), vjust = -1, size = 5.5) +  # vjust controla la posición vertical
  theme_minimal() +
  geom_vline(xintercept = 0) +
  labs(
    # title = "ATT Estimates with 95% Confidence Intervals", 
    y = "Method", 
    x = "Average Treatment Effect (ATT)"
  ) +
  theme(
    axis.title.x = element_text(size = 15),      # Título del eje x
    axis.title.y = element_text(size = 15),      # Título del eje y
    axis.text.x  = element_text(size = 15),      # Etiquetas del eje x
    axis.text.y  = element_text(size = 15),      # Etiquetas del eje y
    plot.title   = element_text(size = 16, face = "bold")  # Título del gráfico
  )
ggsave(
  filename = "C:/Users/mateo/Desktop/Figuras finales/coefs_Dummy_Safety.jpeg", 
  plot = p, 
  width = 12, 
  height = 7, 
  dpi = 300
)








### POLICE MISSING INFORMATION

# Callaway
event_calDummy_Ing <- att_gt(yname = "Dummy_Ing",
                             gname = "first_treated",
                             idname = "Muns",
                             tname = "Anio",
                             xformla = ~1,
                             data = base_censo,
                             allow_unbalanced_panel = TRUE,
                             est_method = "reg",
                             control_group = c("notyettreated","nevertreated")
)

mw.dyn = aggte(event_calDummy_Ing, type = "dynamic", na.rm = TRUE)

calDummy_Ing = aggte(event_calDummy_Ing, type = "simple", na.rm = TRUE)
ggdid(mw.dyn,
      title = "Event Study - Safety expenditure (All categories)",
      xlab = "Period",
      ylab = "Coefficient",
      xgap = 4)

# Borusyak

event_bor_Dummy_Ing = did_imputation(base_censo,
                                     yname = "Dummy_Ing",
                                     gname =  "first_treated",
                                     tname = "Anio",
                                     idname = "Muns",
                                     first_stage = ~0 |
                                       Muns + Anio,
                                     horizon = TRUE,
                                     pretrends = -8:-1) # No sé por qué con TRUE explota la desviación estándar
# mutate(type = "Borusyak") |> 
#  select(type, coef = estimate, se = std.error, conf.low, conf.high)
bor_Dummy_Ing = did_imputation(base_censo,
                               yname = "Dummy_Ing",
                               gname = "first_treated",
                               tname = "Anio",
                               idname = "Muns",
                               first_stage = ~0 | 
                                 Muns + Anio,
)

# Chaisemartin 2020

event_chaiseDummy_Ing <- 
  DIDmultiplegtDYN::did_multiplegt_dyn(
    df = base_censo,
    outcome = "Dummy_Ing",
    group = "Muns",
    time = "Anio",
    treatment = "DTO_dummy",
    effects = 8,
    placebo = 8
  )

event_chaiseDummy_Ing$results
event_chaiseDummy_Ing$plot

# Chaisemartin 2024

event_intDummy_Ing <- 
  DIDmultiplegtDYN::did_multiplegt_dyn(
    df = base_censo,
    outcome = "Dummy_Ing",
    group = "Muns",
    time = "Anio",
    treatment = "DTO_dummy_cont",
    effects = 8,
    placebo = 8
  )

event_intDummy_Ing$results
event_intDummy_Ing$plot


# PLOTS DEL EVENT STUDY

library(dplyr)
library(ggplot2)


# 1. Callaway & Sant’Anna
callaway_df <- data.frame(
  period = as.numeric(mw.dyn$egt),
  estimate = as.numeric(mw.dyn$att.egt),
  se = as.numeric(mw.dyn$se.egt),
  Method = "Callaway & Sant'Anna (2021)"
) %>%
  mutate(
    conf.low = as.numeric(estimate - 1.96 * se),
    conf.high = as.numeric(estimate + 1.96 * se)
  )

# 2. Borusyak et al.
borusyak_df <- event_bor_Dummy_Ing %>%
  mutate(
    Method = "Borusyak et al (2021)",
    period = as.numeric(term),
    estimate = as.numeric(estimate),
    se = as.numeric(std.error),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high)
  ) %>%
  select(Method, period, estimate, se, conf.low, conf.high) %>%
  filter(!is.na(estimate))  # Elimina periodos sin estimaciones

# 3. Chaisemartin & D’Haultfoeuille 2020
chaise2020_placebos <- as.data.frame(event_chaiseDummy_Ing$results$Placebos) %>%
  mutate(
    period = -as.numeric(gsub("Placebo_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2020_effects <- as.data.frame(event_chaiseDummy_Ing$results$Effects) %>%
  mutate(
    period = as.numeric(gsub("Effect_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2020_df <- bind_rows(chaise2020_placebos, chaise2020_effects) %>%
  mutate(Method = "de Chaisemartin (2020)")

# 4. Chaisemartin & D’Haultfoeuille 2024
chaise2024_placebos <- as.data.frame(event_intDummy_Ing$results$Placebos) %>%
  mutate(
    period = -as.numeric(gsub("Placebo_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2024_effects <- as.data.frame(event_intDummy_Ing$results$Effects) %>%
  mutate(
    period = as.numeric(gsub("Effect_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2024_df <- bind_rows(chaise2024_placebos, chaise2024_effects) %>%
  mutate(Method = " de Chaisemartin (2024)")

# Combinar todos los data frames
event_studies_df <- bind_rows(callaway_df, borusyak_df, chaise2020_df, chaise2024_df)

# 5. Graficar
p<-ggplot(event_studies_df, aes(x = period, y = estimate, color = Method, shape = Method)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.4) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(
    #title = "Comparación de Event Studies",
    x = "Time to treatment",
    y = "Coefficient") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 13),       # Tamaño del texto de la leyenda
    legend.title = element_text(size = 15),      # Tamaño del título de la leyenda
    axis.title.x = element_text(size = 15),      # Tamaño del título del eje x
    axis.title.y = element_text(size = 15),      # Tamaño del título del eje y
    axis.text.x = element_text(size = 14),       # Tamaño de los valores del eje x
    axis.text.y = element_text(size = 14)        # Tamaño de los valores del eje y
  )
ggsave(
  filename = "C:/Users/mateo/Desktop/Figuras finales/event_study_Dummy_Ing.jpeg", 
  plot = p, 
  width = 12, 
  height = 7, 
  dpi = 300
)

# PLOT DEL AVERAGE TREATMENT EFFECT ON THE TREATED

library(ggplot2)

# Extraer los valores de ATT y su error estándar desde los objetos

# Para calDummy_Ing
ATT_calDummy_Ing <- calDummy_Ing$overall.att
SE_calDummy_Ing <- calDummy_Ing$overall.se
CI_lower_calDummy_Ing <- ATT_calDummy_Ing - 1.96 * SE_calDummy_Ing
CI_upper_calDummy_Ing <- ATT_calDummy_Ing + 1.96 * SE_calDummy_Ing

# Para bor_Dummy_Ing
ATT_bor_Dummy_Ing <- bor_Dummy_Ing$estimate[bor_Dummy_Ing$term == "treat"]
SE_bor_Dummy_Ing <- bor_Dummy_Ing$std.error[bor_Dummy_Ing$term == "treat"]
CI_lower_bor_Dummy_Ing <- ATT_bor_Dummy_Ing - 1.96 * SE_bor_Dummy_Ing
CI_upper_bor_Dummy_Ing <- ATT_bor_Dummy_Ing + 1.96 * SE_bor_Dummy_Ing

# Para event_chaiseDummy_Ing (averaged treatment effect)
ATT_event_chaiseDummy_Ing <- event_chaiseDummy_Ing$results$ATE[1, "Estimate"]
SE_event_chaiseDummy_Ing <- event_chaiseDummy_Ing$results$ATE[1, "SE"]
CI_lower_event_chaiseDummy_Ing <- event_chaiseDummy_Ing$results$ATE[1, "LB CI"]
CI_upper_event_chaiseDummy_Ing <- event_chaiseDummy_Ing$results$ATE[1, "UB CI"]

# Para event_intDummy_Ing (averaged treatment effect)
ATT_event_intDummy_Ing <- event_intDummy_Ing$results$ATE[1, "Estimate"]
SE_event_intDummy_Ing <- event_intDummy_Ing$results$ATE[1, "SE"]
CI_lower_event_intDummy_Ing <- ATT_event_intDummy_Ing - 1.96 * SE_event_intDummy_Ing
CI_upper_event_intDummy_Ing <- ATT_event_intDummy_Ing + 1.96 * SE_event_intDummy_Ing

# Crear el dataframe para graficar
results_df <- data.frame(
  Method = c("Callaway & Sant'Anna (2021)", "Borusyak et al (2021)", "de Chaisemartin (2020)", "de Chaisemartin (2024)"),
  ATT = c(ATT_calDummy_Ing, ATT_bor_Dummy_Ing, ATT_event_chaiseDummy_Ing, ATT_event_intDummy_Ing),
  SE = c(SE_calDummy_Ing, SE_bor_Dummy_Ing, SE_event_chaiseDummy_Ing, SE_event_intDummy_Ing),
  CI_lower = c(CI_lower_calDummy_Ing, CI_lower_bor_Dummy_Ing, CI_lower_event_chaiseDummy_Ing, CI_lower_event_intDummy_Ing),
  CI_upper = c(CI_upper_calDummy_Ing, CI_upper_bor_Dummy_Ing, CI_upper_event_chaiseDummy_Ing, CI_upper_event_intDummy_Ing)
)

p<-ggplot(results_df, aes(y = Method, x = ATT, xmin = CI_lower, xmax = CI_upper)) +
  geom_pointrange() +
  geom_text(aes(label = round(ATT, 3), y = Method), vjust = -1, size = 5.5) +  # vjust controla la posición vertical
  theme_minimal() +
  geom_vline(xintercept = 0) +
  labs(
    # title = "ATT Estimates with 95% Confidence Intervals", 
    y = "Method", 
    x = "Average Treatment Effect (ATT)"
  ) +
  theme(
    axis.title.x = element_text(size = 15),      # Título del eje x
    axis.title.y = element_text(size = 15),      # Título del eje y
    axis.text.x  = element_text(size = 15),      # Etiquetas del eje x
    axis.text.y  = element_text(size = 15),      # Etiquetas del eje y
    plot.title   = element_text(size = 16, face = "bold")  # Título del gráfico
  )
ggsave(
  filename = "C:/Users/mateo/Desktop/Figuras finales/coefs_Dummy_Ing.jpeg", 
  plot = p, 
  width = 12, 
  height = 7, 
  dpi = 300
)













### SAFETY EXPENDITURE FOR PUBLIC SERVANTS


# Callaway
event_calFuncionarios <- att_gt(yname = "logSafetyFuncionarios_exp",
                                gname = "first_treated",
                                idname = "Muns",
                                tname = "Anio",
                                xformla = ~1,
                                data = base,
                                allow_unbalanced_panel = TRUE,
                                est_method = "reg",
                                control_group = c("notyettreated","nevertreated")
)

mw.dyn = aggte(event_calFuncionarios, type = "dynamic", na.rm = TRUE)

calFuncionarios = aggte(event_calFuncionarios, type = "simple", na.rm = TRUE)
ggdid(mw.dyn,
      title = "Event Study - Safety expenditure for public servants",
      xlab = "Period",
      ylab = "Coefficient",
      xgap = 4)

# Borusyak

event_bor_Funcionarios = did_imputation(base,
                                        yname = "logSafetyFuncionarios_exp",
                                        gname =  "first_treated",
                                        tname = "Anio",
                                        idname = "Muns",
                                        first_stage = ~0 |
                                          Muns + Anio,
                                        horizon = TRUE,
                                        pretrends = TRUE) # No sé por qué con TRUE explota la desviación estándar
# mutate(type = "Borusyak") |> 
#  select(type, coef = estimate, se = std.error, conf.low, conf.high)
bor_Funcionarios = did_imputation(base,
                                  yname = "logSafetyFuncionarios_exp",
                                  gname = "first_treated",
                                  tname = "Anio",
                                  idname = "Muns",
                                  first_stage = ~0 | 
                                    Muns + Anio,
)

# Chaisemartin 2020

event_chaiseFuncionarios <- 
  DIDmultiplegtDYN::did_multiplegt_dyn(
    df = base,
    outcome = "logSafetyFuncionarios_exp",
    group = "Muns",
    time = "Anio",
    treatment = "DTO_dummy",
    effects = 14,
    placebo = 7
  )

event_chaiseFuncionarios$results
event_chaiseFuncionarios$plot

# Chaisemartin 2024

event_intFuncionarios <- 
  DIDmultiplegtDYN::did_multiplegt_dyn(
    df = base,
    outcome = "logSafetyFuncionarios_exp",
    group = "Muns",
    time = "Anio",
    treatment = "DTO_dummy_cont",
    effects = 14,
    placebo = 6
  )

event_intFuncionarios$results
event_intFuncionarios$plot




# PLOTS DEL EVENT STUDY

library(dplyr)
library(ggplot2)


# 1. Callaway & Sant’Anna
callaway_df <- data.frame(
  period = as.numeric(mw.dyn$egt),
  estimate = as.numeric(mw.dyn$att.egt),
  se = as.numeric(mw.dyn$se.egt),
  Method = "Callaway & Sant'Anna (2021)"
) %>%
  mutate(
    conf.low = as.numeric(estimate - 1.96 * se),
    conf.high = as.numeric(estimate + 1.96 * se)
  )

# 2. Borusyak et al.
borusyak_df <- event_bor_Funcionarios %>%
  mutate(
    Method = "Borusyak et al (2021)",
    period = as.numeric(term),
    estimate = as.numeric(estimate),
    se = as.numeric(std.error),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high)
  ) %>%
  select(Method, period, estimate, se, conf.low, conf.high) %>%
  filter(!is.na(estimate))  # Elimina periodos sin estimaciones

# 3. Chaisemartin & D’Haultfoeuille 2020
chaise2020_placebos <- as.data.frame(event_chaiseFuncionarios$results$Placebos) %>%
  mutate(
    period = -as.numeric(gsub("Placebo_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2020_effects <- as.data.frame(event_chaiseFuncionarios$results$Effects) %>%
  mutate(
    period = as.numeric(gsub("Effect_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2020_df <- bind_rows(chaise2020_placebos, chaise2020_effects) %>%
  mutate(Method = "de Chaisemartin (2020)")

# 4. Chaisemartin & D’Haultfoeuille 2024
chaise2024_placebos <- as.data.frame(event_intFuncionarios$results$Placebos) %>%
  mutate(
    period = -as.numeric(gsub("Placebo_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2024_effects <- as.data.frame(event_intFuncionarios$results$Effects) %>%
  mutate(
    period = as.numeric(gsub("Effect_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2024_df <- bind_rows(chaise2024_placebos, chaise2024_effects) %>%
  mutate(Method = " de Chaisemartin (2024)")

# Combinar todos los data frames
event_studies_df <- bind_rows(callaway_df, borusyak_df, chaise2020_df, chaise2024_df)

# 5. Graficar
p<-ggplot(event_studies_df, aes(x = period, y = estimate, color = Method, shape = Method)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.4) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(
    #title = "Comparación de Event Studies",
    x = "Time to treatment",
    y = "Coefficient") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 13),       # Tamaño del texto de la leyenda
    legend.title = element_text(size = 15),      # Tamaño del título de la leyenda
    axis.title.x = element_text(size = 15),      # Tamaño del título del eje x
    axis.title.y = element_text(size = 15),      # Tamaño del título del eje y
    axis.text.x = element_text(size = 14),       # Tamaño de los valores del eje x
    axis.text.y = element_text(size = 14)        # Tamaño de los valores del eje y
  )
ggsave(
  filename = "C:/Users/mateo/Desktop/Figuras finales/event_study_funcionarios.jpeg", 
  plot = p, 
  width = 12, 
  height = 7, 
  dpi = 300
)

# PLOT DEL AVERAGE TREATMENT EFFECT ON THE TREATED

library(ggplot2)

# Extraer los valores de ATT y su error estándar desde los objetos

# Para calFuncionarios
ATT_calFuncionarios <- calFuncionarios$overall.att
SE_calFuncionarios <- calFuncionarios$overall.se
CI_lower_calFuncionarios <- ATT_calFuncionarios - 1.96 * SE_calFuncionarios
CI_upper_calFuncionarios <- ATT_calFuncionarios + 1.96 * SE_calFuncionarios

# Para bor_Funcionarios
ATT_bor_Funcionarios <- bor_Funcionarios$estimate[bor_Funcionarios$term == "treat"]
SE_bor_Funcionarios <- bor_Funcionarios$std.error[bor_Funcionarios$term == "treat"]
CI_lower_bor_Funcionarios <- ATT_bor_Funcionarios - 1.96 * SE_bor_Funcionarios
CI_upper_bor_Funcionarios <- ATT_bor_Funcionarios + 1.96 * SE_bor_Funcionarios

# Para event_chaiseFuncionarios (averaged treatment effect)
ATT_event_chaiseFuncionarios <- event_chaiseFuncionarios$results$ATE[1, "Estimate"]
SE_event_chaiseFuncionarios <- event_chaiseFuncionarios$results$ATE[1, "SE"]
CI_lower_event_chaiseFuncionarios <- event_chaiseFuncionarios$results$ATE[1, "LB CI"]
CI_upper_event_chaiseFuncionarios <- event_chaiseFuncionarios$results$ATE[1, "UB CI"]

# Para event_intFuncionarios (averaged treatment effect)
ATT_event_intFuncionarios <- event_intFuncionarios$results$ATE[1, "Estimate"]
SE_event_intFuncionarios <- event_intFuncionarios$results$ATE[1, "SE"]
CI_lower_event_intFuncionarios <- ATT_event_intFuncionarios - 1.96 * SE_event_intFuncionarios
CI_upper_event_intFuncionarios <- ATT_event_intFuncionarios + 1.96 * SE_event_intFuncionarios

# Crear el dataframe para graficar
results_df <- data.frame(
  Method = c("Callaway & Sant'Anna (2021)", "Borusyak et al (2021)", "de Chaisemartin (2020)", "de Chaisemartin (2024)"),
  ATT = c(ATT_calFuncionarios, ATT_bor_Funcionarios, ATT_event_chaiseFuncionarios, ATT_event_intFuncionarios),
  SE = c(SE_calFuncionarios, SE_bor_Funcionarios, SE_event_chaiseFuncionarios, SE_event_intFuncionarios),
  CI_lower = c(CI_lower_calFuncionarios, CI_lower_bor_Funcionarios, CI_lower_event_chaiseFuncionarios, CI_lower_event_intFuncionarios),
  CI_upper = c(CI_upper_calFuncionarios, CI_upper_bor_Funcionarios, CI_upper_event_chaiseFuncionarios, CI_upper_event_intFuncionarios)
)

p<-ggplot(results_df, aes(y = Method, x = ATT, xmin = CI_lower, xmax = CI_upper)) +
  geom_pointrange() +
  geom_text(aes(label = round(ATT, 3), y = Method), vjust = -1, size = 5.5) +  # vjust controla la posición vertical
  theme_minimal() +
  geom_vline(xintercept = 0) +
  labs(
    # title = "ATT Estimates with 95% Confidence Intervals", 
    y = "Method", 
    x = "Average Treatment Effect (ATT)"
  ) +
  theme(
    axis.title.x = element_text(size = 15),      # Título del eje x
    axis.title.y = element_text(size = 15),      # Título del eje y
    axis.text.x  = element_text(size = 15),      # Etiquetas del eje x
    axis.text.y  = element_text(size = 15),      # Etiquetas del eje y
    plot.title   = element_text(size = 16, face = "bold")  # Título del gráfico
  )
ggsave(
  filename = "C:/Users/mateo/Desktop/Figuras finales/coefs_Funcionarios.jpeg", 
  plot = p, 
  width = 12, 
  height = 7, 
  dpi = 300
)




### MEAN GROSS MUNICIPAL POLICE INCOME

# Callaway
event_calloging <- att_gt(yname = "loging",
                          gname = "first_treated",
                          idname = "Muns",
                          tname = "Anio",
                          xformla = ~1,
                          data = base_censo,
                          allow_unbalanced_panel = TRUE,
                          est_method = "reg",
                          control_group = c("notyettreated","nevertreated")
)

mw.dyn = aggte(event_calloging, type = "dynamic", na.rm = TRUE)

calloging = aggte(event_calloging, type = "simple", na.rm = TRUE)
ggdid(mw.dyn,
      title = "Event Study - Safety expenditure for public servants",
      xlab = "Period",
      ylab = "Coefficient",
      xgap = 4)

# Borusyak

event_bor_loging = did_imputation(base_censo,
                                  yname = "loging",
                                  gname =  "first_treated",
                                  tname = "Anio",
                                  idname = "Muns",
                                  first_stage = ~0 |
                                    Muns + Anio,
                                  horizon = TRUE,
                                  pretrends = TRUE) # No sé por qué con TRUE explota la desviación estándar
# mutate(type = "Borusyak") |> 
#  select(type, coef = estimate, se = std.error, conf.low, conf.high)
bor_loging = did_imputation(base_censo,
                            yname = "loging",
                            gname = "first_treated",
                            tname = "Anio",
                            idname = "Muns",
                            first_stage = ~0 | 
                              Muns + Anio,
)

# Chaisemartin 2020

event_chaiseloging <- 
  DIDmultiplegtDYN::did_multiplegt_dyn(
    df = base_censo,
    outcome = "loging",
    group = "Muns",
    time = "Anio",
    treatment = "DTO_dummy",
    effects = 4,
    placebo = 4
  )

event_chaiseloging$results
event_chaiseloging$plot

# Chaisemartin 2024

event_intloging <- 
  DIDmultiplegtDYN::did_multiplegt_dyn(
    df = base_censo,
    outcome = "loging",
    group = "Muns",
    time = "Anio",
    treatment = "DTO_dummy_cont",
    effects = 4,
    placebo = 4
  )

event_intloging$results
event_intloging$plot




# PLOTS DEL EVENT STUDY

library(dplyr)
library(ggplot2)


# 1. Callaway & Sant’Anna
callaway_df <- data.frame(
  period = as.numeric(mw.dyn$egt),
  estimate = as.numeric(mw.dyn$att.egt),
  se = as.numeric(mw.dyn$se.egt),
  Method = "Callaway & Sant'Anna (2021)"
) %>%
  mutate(
    conf.low = as.numeric(estimate - 1.96 * se),
    conf.high = as.numeric(estimate + 1.96 * se)
  )

# 2. Borusyak et al.
borusyak_df <- event_bor_loging %>%
  mutate(
    Method = "Borusyak et al (2021)",
    period = as.numeric(term),
    estimate = as.numeric(estimate),
    se = as.numeric(std.error),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high)
  ) %>%
  select(Method, period, estimate, se, conf.low, conf.high) %>%
  filter(!is.na(estimate))  # Elimina periodos sin estimaciones

# 3. Chaisemartin & D’Haultfoeuille 2020
chaise2020_placebos <- as.data.frame(event_chaiseloging$results$Placebos) %>%
  mutate(
    period = -as.numeric(gsub("Placebo_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2020_effects <- as.data.frame(event_chaiseloging$results$Effects) %>%
  mutate(
    period = as.numeric(gsub("Effect_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2020_df <- bind_rows(chaise2020_placebos, chaise2020_effects) %>%
  mutate(Method = "de Chaisemartin (2020)")

# 4. Chaisemartin & D’Haultfoeuille 2024
chaise2024_placebos <- as.data.frame(event_intloging$results$Placebos) %>%
  mutate(
    period = -as.numeric(gsub("Placebo_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2024_effects <- as.data.frame(event_intloging$results$Effects) %>%
  mutate(
    period = as.numeric(gsub("Effect_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2024_df <- bind_rows(chaise2024_placebos, chaise2024_effects) %>%
  mutate(Method = " de Chaisemartin (2024)")

# Combinar todos los data frames
event_studies_df <- bind_rows(callaway_df, borusyak_df, chaise2020_df, chaise2024_df)

# 5. Graficar
p<-ggplot(event_studies_df, aes(x = period, y = estimate, color = Method, shape = Method)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.4) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(
    #title = "Comparación de Event Studies",
    x = "Time to treatment",
    y = "Coefficient") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 13),       # Tamaño del texto de la leyenda
    legend.title = element_text(size = 15),      # Tamaño del título de la leyenda
    axis.title.x = element_text(size = 15),      # Tamaño del título del eje x
    axis.title.y = element_text(size = 15),      # Tamaño del título del eje y
    axis.text.x = element_text(size = 14),       # Tamaño de los valores del eje x
    axis.text.y = element_text(size = 14)        # Tamaño de los valores del eje y
  )
ggsave(
  filename = "C:/Users/mateo/Desktop/Figuras finales/event_study_loging.jpeg", 
  plot = p, 
  width = 12, 
  height = 7, 
  dpi = 300
)

# PLOT DEL AVERAGE TREATMENT EFFECT ON THE TREATED

library(ggplot2)

# Extraer los valores de ATT y su error estándar desde los objetos

# Para calloging
ATT_calloging <- calloging$overall.att
SE_calloging <- calloging$overall.se
CI_lower_calloging <- ATT_calloging - 1.96 * SE_calloging
CI_upper_calloging <- ATT_calloging + 1.96 * SE_calloging

# Para bor_loging
ATT_bor_loging <- bor_loging$estimate[bor_loging$term == "treat"]
SE_bor_loging <- bor_loging$std.error[bor_loging$term == "treat"]
CI_lower_bor_loging <- ATT_bor_loging - 1.96 * SE_bor_loging
CI_upper_bor_loging <- ATT_bor_loging + 1.96 * SE_bor_loging

# Para event_chaiseloging (averaged treatment effect)
ATT_event_chaiseloging <- event_chaiseloging$results$ATE[1, "Estimate"]
SE_event_chaiseloging <- event_chaiseloging$results$ATE[1, "SE"]
CI_lower_event_chaiseloging <- event_chaiseloging$results$ATE[1, "LB CI"]
CI_upper_event_chaiseloging <- event_chaiseloging$results$ATE[1, "UB CI"]

# Para event_intloging (averaged treatment effect)
ATT_event_intloging <- event_intloging$results$ATE[1, "Estimate"]
SE_event_intloging <- event_intloging$results$ATE[1, "SE"]
CI_lower_event_intloging <- ATT_event_intloging - 1.96 * SE_event_intloging
CI_upper_event_intloging <- ATT_event_intloging + 1.96 * SE_event_intloging

# Crear el dataframe para graficar
results_df <- data.frame(
  Method = c("Callaway & Sant'Anna (2021)", "Borusyak et al (2021)", "de Chaisemartin (2020)", "de Chaisemartin (2024)"),
  ATT = c(ATT_calloging, ATT_bor_loging, ATT_event_chaiseloging, ATT_event_intloging),
  SE = c(SE_calloging, SE_bor_loging, SE_event_chaiseloging, SE_event_intloging),
  CI_lower = c(CI_lower_calloging, CI_lower_bor_loging, CI_lower_event_chaiseloging, CI_lower_event_intloging),
  CI_upper = c(CI_upper_calloging, CI_upper_bor_loging, CI_upper_event_chaiseloging, CI_upper_event_intloging)
)

p<-ggplot(results_df, aes(y = Method, x = ATT, xmin = CI_lower, xmax = CI_upper)) +
  geom_pointrange() +
  geom_text(aes(label = round(ATT, 3), y = Method), vjust = -1, size = 5.5) +  # vjust controla la posición vertical
  theme_minimal() +
  geom_vline(xintercept = 0) +
  labs(
    # title = "ATT Estimates with 95% Confidence Intervals", 
    y = "Method", 
    x = "Average Treatment Effect (ATT)"
  ) +
  theme(
    axis.title.x = element_text(size = 15),      # Título del eje x
    axis.title.y = element_text(size = 15),      # Título del eje y
    axis.text.x  = element_text(size = 15),      # Etiquetas del eje x
    axis.text.y  = element_text(size = 15),      # Etiquetas del eje y
    plot.title   = element_text(size = 16, face = "bold")  # Título del gráfico
  )
ggsave(
  filename = "C:/Users/mateo/Desktop/Figuras finales/coefs_loging.jpeg", 
  plot = p, 
  width = 12, 
  height = 7, 
  dpi = 300
)








### MEAN GROSS MUNICIPAL POLICE INCOME VARIANCE

# Callaway
event_callogvaring <- att_gt(yname = "logvaring",
                             gname = "first_treated",
                             idname = "Muns",
                             tname = "Anio",
                             xformla = ~1,
                             data = base_censo,
                             allow_unbalanced_panel = TRUE,
                             est_method = "reg",
                             control_group = c("notyettreated","nevertreated")
)

mw.dyn = aggte(event_callogvaring, type = "dynamic", na.rm = TRUE)

callogvaring = aggte(event_callogvaring, type = "simple", na.rm = TRUE)
ggdid(mw.dyn,
      title = "Event Study - Safety expenditure for public servants",
      xlab = "Period",
      ylab = "Coefficient",
      xgap = 4)

# Borusyak

event_bor_logvaring = did_imputation(base_censo,
                                     yname = "logvaring",
                                     gname =  "first_treated",
                                     tname = "Anio",
                                     idname = "Muns",
                                     first_stage = ~0 |
                                       Muns + Anio,
                                     horizon = TRUE,
                                     pretrends = TRUE) # No sé por qué con TRUE explota la desviación estándar
# mutate(type = "Borusyak") |> 
#  select(type, coef = estimate, se = std.error, conf.low, conf.high)
bor_logvaring = did_imputation(base_censo,
                               yname = "logvaring",
                               gname = "first_treated",
                               tname = "Anio",
                               idname = "Muns",
                               first_stage = ~0 | 
                                 Muns + Anio,
)

# Chaisemartin 2020

event_chaiselogvaring <- 
  DIDmultiplegtDYN::did_multiplegt_dyn(
    df = base_censo,
    outcome = "logvaring",
    group = "Muns",
    time = "Anio",
    treatment = "DTO_dummy",
    effects = 4,
    placebo = 4
  )

event_chaiselogvaring$results
event_chaiselogvaring$plot

# Chaisemartin 2024

event_intlogvaring <- 
  DIDmultiplegtDYN::did_multiplegt_dyn(
    df = base_censo,
    outcome = "logvaring",
    group = "Muns",
    time = "Anio",
    treatment = "DTO_dummy_cont",
    effects = 4,
    placebo = 4
  )

event_intlogvaring$results
event_intlogvaring$plot




# PLOTS DEL EVENT STUDY

library(dplyr)
library(ggplot2)


# 1. Callaway & Sant’Anna
callaway_df <- data.frame(
  period = as.numeric(mw.dyn$egt),
  estimate = as.numeric(mw.dyn$att.egt),
  se = as.numeric(mw.dyn$se.egt),
  Method = "Callaway & Sant'Anna (2021)"
) %>%
  mutate(
    conf.low = as.numeric(estimate - 1.96 * se),
    conf.high = as.numeric(estimate + 1.96 * se)
  )

# 2. Borusyak et al.
borusyak_df <- event_bor_logvaring %>%
  mutate(
    Method = "Borusyak et al (2021)",
    period = as.numeric(term),
    estimate = as.numeric(estimate),
    se = as.numeric(std.error),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high)
  ) %>%
  select(Method, period, estimate, se, conf.low, conf.high) %>%
  filter(!is.na(estimate))  # Elimina periodos sin estimaciones

# 3. Chaisemartin & D’Haultfoeuille 2020
chaise2020_placebos <- as.data.frame(event_chaiselogvaring$results$Placebos) %>%
  mutate(
    period = -as.numeric(gsub("Placebo_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2020_effects <- as.data.frame(event_chaiselogvaring$results$Effects) %>%
  mutate(
    period = as.numeric(gsub("Effect_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2020_df <- bind_rows(chaise2020_placebos, chaise2020_effects) %>%
  mutate(Method = "de Chaisemartin (2020)")

# 4. Chaisemartin & D’Haultfoeuille 2024
chaise2024_placebos <- as.data.frame(event_intlogvaring$results$Placebos) %>%
  mutate(
    period = -as.numeric(gsub("Placebo_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2024_effects <- as.data.frame(event_intlogvaring$results$Effects) %>%
  mutate(
    period = as.numeric(gsub("Effect_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2024_df <- bind_rows(chaise2024_placebos, chaise2024_effects) %>%
  mutate(Method = " de Chaisemartin (2024)")

# Combinar todos los data frames
event_studies_df <- bind_rows(callaway_df, borusyak_df, chaise2020_df, chaise2024_df)

# 5. Graficar
p<-ggplot(event_studies_df, aes(x = period, y = estimate, color = Method, shape = Method)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.4) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(
    #title = "Comparación de Event Studies",
    x = "Time to treatment",
    y = "Coefficient") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 13),       # Tamaño del texto de la leyenda
    legend.title = element_text(size = 15),      # Tamaño del título de la leyenda
    axis.title.x = element_text(size = 15),      # Tamaño del título del eje x
    axis.title.y = element_text(size = 15),      # Tamaño del título del eje y
    axis.text.x = element_text(size = 14),       # Tamaño de los valores del eje x
    axis.text.y = element_text(size = 14)        # Tamaño de los valores del eje y
  )
ggsave(
  filename = "C:/Users/mateo/Desktop/Figuras finales/event_study_logvaring.jpeg", 
  plot = p, 
  width = 12, 
  height = 7, 
  dpi = 300
)

# PLOT DEL AVERAGE TREATMENT EFFECT ON THE TREATED

library(ggplot2)

# Extraer los valores de ATT y su error estándar desde los objetos

# Para callogvaring
ATT_callogvaring <- callogvaring$overall.att
SE_callogvaring <- callogvaring$overall.se
CI_lower_callogvaring <- ATT_callogvaring - 1.96 * SE_callogvaring
CI_upper_callogvaring <- ATT_callogvaring + 1.96 * SE_callogvaring

# Para bor_logvaring
ATT_bor_logvaring <- bor_logvaring$estimate[bor_logvaring$term == "treat"]
SE_bor_logvaring <- bor_logvaring$std.error[bor_logvaring$term == "treat"]
CI_lower_bor_logvaring <- ATT_bor_logvaring - 1.96 * SE_bor_logvaring
CI_upper_bor_logvaring <- ATT_bor_logvaring + 1.96 * SE_bor_logvaring

# Para event_chaiselogvaring (averaged treatment effect)
ATT_event_chaiselogvaring <- event_chaiselogvaring$results$ATE[1, "Estimate"]
SE_event_chaiselogvaring <- event_chaiselogvaring$results$ATE[1, "SE"]
CI_lower_event_chaiselogvaring <- event_chaiselogvaring$results$ATE[1, "LB CI"]
CI_upper_event_chaiselogvaring <- event_chaiselogvaring$results$ATE[1, "UB CI"]

# Para event_intlogvaring (averaged treatment effect)
ATT_event_intlogvaring <- event_intlogvaring$results$ATE[1, "Estimate"]
SE_event_intlogvaring <- event_intlogvaring$results$ATE[1, "SE"]
CI_lower_event_intlogvaring <- ATT_event_intlogvaring - 1.96 * SE_event_intlogvaring
CI_upper_event_intlogvaring <- ATT_event_intlogvaring + 1.96 * SE_event_intlogvaring

# Crear el dataframe para graficar
results_df <- data.frame(
  Method = c("Callaway & Sant'Anna (2021)", "Borusyak et al (2021)", "de Chaisemartin (2020)", "de Chaisemartin (2024)"),
  ATT = c(ATT_callogvaring, ATT_bor_logvaring, ATT_event_chaiselogvaring, ATT_event_intlogvaring),
  SE = c(SE_callogvaring, SE_bor_logvaring, SE_event_chaiselogvaring, SE_event_intlogvaring),
  CI_lower = c(CI_lower_callogvaring, CI_lower_bor_logvaring, CI_lower_event_chaiselogvaring, CI_lower_event_intlogvaring),
  CI_upper = c(CI_upper_callogvaring, CI_upper_bor_logvaring, CI_upper_event_chaiselogvaring, CI_upper_event_intlogvaring)
)

p<-ggplot(results_df, aes(y = Method, x = ATT, xmin = CI_lower, xmax = CI_upper)) +
  geom_pointrange() +
  geom_text(aes(label = round(ATT, 3), y = Method), vjust = -1, size = 5.5) +  # vjust controla la posición vertical
  theme_minimal() +
  geom_vline(xintercept = 0) +
  labs(
    # title = "ATT Estimates with 95% Confidence Intervals", 
    y = "Method", 
    x = "Average Treatment Effect (ATT)"
  ) +
  theme(
    axis.title.x = element_text(size = 15),      # Título del eje x
    axis.title.y = element_text(size = 15),      # Título del eje y
    axis.text.x  = element_text(size = 15),      # Etiquetas del eje x
    axis.text.y  = element_text(size = 15),      # Etiquetas del eje y
    plot.title   = element_text(size = 16, face = "bold")  # Título del gráfico
  )
ggsave(
  filename = "C:/Users/mateo/Desktop/Figuras finales/coefs_logvaring.jpeg", 
  plot = p, 
  width = 12, 
  height = 7, 
  dpi = 300
)








### MEAN GROSS MUNICIPAL POLICE INCOME IQ RANGE

# Callaway
event_callogquarting <- att_gt(yname = "logquarting",
                               gname = "first_treated",
                               idname = "Muns",
                               tname = "Anio",
                               xformla = ~1,
                               data = base_censo,
                               allow_unbalanced_panel = TRUE,
                               est_method = "reg",
                               control_group = c("notyettreated","nevertreated")
)

mw.dyn = aggte(event_callogquarting, type = "dynamic", na.rm = TRUE)

callogquarting = aggte(event_callogquarting, type = "simple", na.rm = TRUE)
ggdid(mw.dyn,
      title = "Event Study - Safety expenditure for public servants",
      xlab = "Period",
      ylab = "Coefficient",
      xgap = 4)

# Borusyak

event_bor_logquarting = did_imputation(base_censo,
                                       yname = "logquarting",
                                       gname =  "first_treated",
                                       tname = "Anio",
                                       idname = "Muns",
                                       first_stage = ~0 |
                                         Muns + Anio,
                                       horizon = TRUE,
                                       pretrends = TRUE) # No sé por qué con TRUE explota la desviación estándar
# mutate(type = "Borusyak") |> 
#  select(type, coef = estimate, se = std.error, conf.low, conf.high)
bor_logquarting = did_imputation(base_censo,
                                 yname = "logquarting",
                                 gname = "first_treated",
                                 tname = "Anio",
                                 idname = "Muns",
                                 first_stage = ~0 | 
                                   Muns + Anio,
)

# Chaisemartin 2020

event_chaiselogquarting <- 
  DIDmultiplegtDYN::did_multiplegt_dyn(
    df = base_censo,
    outcome = "logquarting",
    group = "Muns",
    time = "Anio",
    treatment = "DTO_dummy",
    effects = 4,
    placebo = 4
  )

event_chaiselogquarting$results
event_chaiselogquarting$plot

# Chaisemartin 2024

event_intlogquarting <- 
  DIDmultiplegtDYN::did_multiplegt_dyn(
    df = base_censo,
    outcome = "logquarting",
    group = "Muns",
    time = "Anio",
    treatment = "DTO_dummy_cont",
    effects = 4,
    placebo = 4
  )

event_intlogquarting$results
event_intlogquarting$plot




# PLOTS DEL EVENT STUDY

library(dplyr)
library(ggplot2)


# 1. Callaway & Sant’Anna
callaway_df <- data.frame(
  period = as.numeric(mw.dyn$egt),
  estimate = as.numeric(mw.dyn$att.egt),
  se = as.numeric(mw.dyn$se.egt),
  Method = "Callaway & Sant'Anna (2021)"
) %>%
  mutate(
    conf.low = as.numeric(estimate - 1.96 * se),
    conf.high = as.numeric(estimate + 1.96 * se)
  )

# 2. Borusyak et al.
borusyak_df <- event_bor_logquarting %>%
  mutate(
    Method = "Borusyak et al (2021)",
    period = as.numeric(term),
    estimate = as.numeric(estimate),
    se = as.numeric(std.error),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high)
  ) %>%
  select(Method, period, estimate, se, conf.low, conf.high) %>%
  filter(!is.na(estimate))  # Elimina periodos sin estimaciones

# 3. Chaisemartin & D’Haultfoeuille 2020
chaise2020_placebos <- as.data.frame(event_chaiselogquarting$results$Placebos) %>%
  mutate(
    period = -as.numeric(gsub("Placebo_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2020_effects <- as.data.frame(event_chaiselogquarting$results$Effects) %>%
  mutate(
    period = as.numeric(gsub("Effect_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2020_df <- bind_rows(chaise2020_placebos, chaise2020_effects) %>%
  mutate(Method = "de Chaisemartin (2020)")

# 4. Chaisemartin & D’Haultfoeuille 2024
chaise2024_placebos <- as.data.frame(event_intlogquarting$results$Placebos) %>%
  mutate(
    period = -as.numeric(gsub("Placebo_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2024_effects <- as.data.frame(event_intlogquarting$results$Effects) %>%
  mutate(
    period = as.numeric(gsub("Effect_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2024_df <- bind_rows(chaise2024_placebos, chaise2024_effects) %>%
  mutate(Method = " de Chaisemartin (2024)")

# Combinar todos los data frames
event_studies_df <- bind_rows(callaway_df, borusyak_df, chaise2020_df, chaise2024_df)

# 5. Graficar
p<-ggplot(event_studies_df, aes(x = period, y = estimate, color = Method, shape = Method)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.4) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(
    #title = "Comparación de Event Studies",
    x = "Time to treatment",
    y = "Coefficient") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 13),       # Tamaño del texto de la leyenda
    legend.title = element_text(size = 15),      # Tamaño del título de la leyenda
    axis.title.x = element_text(size = 15),      # Tamaño del título del eje x
    axis.title.y = element_text(size = 15),      # Tamaño del título del eje y
    axis.text.x = element_text(size = 14),       # Tamaño de los valores del eje x
    axis.text.y = element_text(size = 14)        # Tamaño de los valores del eje y
  )
ggsave(
  filename = "C:/Users/mateo/Desktop/Figuras finales/event_study_logquarting.jpeg", 
  plot = p, 
  width = 12, 
  height = 7, 
  dpi = 300
)

# PLOT DEL AVERAGE TREATMENT EFFECT ON THE TREATED

library(ggplot2)

# Extraer los valores de ATT y su error estándar desde los objetos

# Para callogquarting
ATT_callogquarting <- callogquarting$overall.att
SE_callogquarting <- callogquarting$overall.se
CI_lower_callogquarting <- ATT_callogquarting - 1.96 * SE_callogquarting
CI_upper_callogquarting <- ATT_callogquarting + 1.96 * SE_callogquarting

# Para bor_logquarting
ATT_bor_logquarting <- bor_logquarting$estimate[bor_logquarting$term == "treat"]
SE_bor_logquarting <- bor_logquarting$std.error[bor_logquarting$term == "treat"]
CI_lower_bor_logquarting <- ATT_bor_logquarting - 1.96 * SE_bor_logquarting
CI_upper_bor_logquarting <- ATT_bor_logquarting + 1.96 * SE_bor_logquarting

# Para event_chaiselogquarting (averaged treatment effect)
ATT_event_chaiselogquarting <- event_chaiselogquarting$results$ATE[1, "Estimate"]
SE_event_chaiselogquarting <- event_chaiselogquarting$results$ATE[1, "SE"]
CI_lower_event_chaiselogquarting <- event_chaiselogquarting$results$ATE[1, "LB CI"]
CI_upper_event_chaiselogquarting <- event_chaiselogquarting$results$ATE[1, "UB CI"]

# Para event_intlogquarting (averaged treatment effect)
ATT_event_intlogquarting <- event_intlogquarting$results$ATE[1, "Estimate"]
SE_event_intlogquarting <- event_intlogquarting$results$ATE[1, "SE"]
CI_lower_event_intlogquarting <- ATT_event_intlogquarting - 1.96 * SE_event_intlogquarting
CI_upper_event_intlogquarting <- ATT_event_intlogquarting + 1.96 * SE_event_intlogquarting

# Crear el dataframe para graficar
results_df <- data.frame(
  Method = c("Callaway & Sant'Anna (2021)", "Borusyak et al (2021)", "de Chaisemartin (2020)", "de Chaisemartin (2024)"),
  ATT = c(ATT_callogquarting, ATT_bor_logquarting, ATT_event_chaiselogquarting, ATT_event_intlogquarting),
  SE = c(SE_callogquarting, SE_bor_logquarting, SE_event_chaiselogquarting, SE_event_intlogquarting),
  CI_lower = c(CI_lower_callogquarting, CI_lower_bor_logquarting, CI_lower_event_chaiselogquarting, CI_lower_event_intlogquarting),
  CI_upper = c(CI_upper_callogquarting, CI_upper_bor_logquarting, CI_upper_event_chaiselogquarting, CI_upper_event_intlogquarting)
)

p<-ggplot(results_df, aes(y = Method, x = ATT, xmin = CI_lower, xmax = CI_upper)) +
  geom_pointrange() +
  geom_text(aes(label = round(ATT, 3), y = Method), vjust = -1, size = 5.5) +  # vjust controla la posición vertical
  theme_minimal() +
  geom_vline(xintercept = 0) +
  labs(
    # title = "ATT Estimates with 95% Confidence Intervals", 
    y = "Method", 
    x = "Average Treatment Effect (ATT)"
  ) +
  theme(
    axis.title.x = element_text(size = 15),      # Título del eje x
    axis.title.y = element_text(size = 15),      # Título del eje y
    axis.text.x  = element_text(size = 15),      # Etiquetas del eje x
    axis.text.y  = element_text(size = 15),      # Etiquetas del eje y
    plot.title   = element_text(size = 16, face = "bold")  # Título del gráfico
  )
ggsave(
  filename = "C:/Users/mateo/Desktop/Figuras finales/coefs_logquarting.jpeg", 
  plot = p, 
  width = 12, 
  height = 7, 
  dpi = 300
)













### TOTAL MUNICIPAL AVAILABLE POLICE

# Callaway
event_callogpol <- att_gt(yname = "logpol",
                          gname = "first_treated",
                          idname = "Muns",
                          tname = "Anio",
                          xformla = ~1,
                          data = base_censo,
                          allow_unbalanced_panel = TRUE,
                          est_method = "reg",
                          control_group = c("notyettreated","nevertreated")
)

mw.dyn = aggte(event_callogpol, type = "dynamic", na.rm = TRUE)

callogpol = aggte(event_callogpol, type = "simple", na.rm = TRUE)
ggdid(mw.dyn,
      title = "Event Study - Safety expenditure for public servants",
      xlab = "Period",
      ylab = "Coefficient",
      xgap = 4)

# Borusyak

event_bor_logpol = did_imputation(base_censo,
                                  yname = "logpol",
                                  gname =  "first_treated",
                                  tname = "Anio",
                                  idname = "Muns",
                                  first_stage = ~0 |
                                    Muns + Anio,
                                  horizon = TRUE,
                                  pretrends = TRUE) # No sé por qué con TRUE explota la desviación estándar
# mutate(type = "Borusyak") |> 
#  select(type, coef = estimate, se = std.error, conf.low, conf.high)
bor_logpol = did_imputation(base_censo,
                            yname = "logpol",
                            gname = "first_treated",
                            tname = "Anio",
                            idname = "Muns",
                            first_stage = ~0 | 
                              Muns + Anio,
)

# Chaisemartin 2020

event_chaiselogpol <- 
  DIDmultiplegtDYN::did_multiplegt_dyn(
    df = base_censo,
    outcome = "logpol",
    group = "Muns",
    time = "Anio",
    treatment = "DTO_dummy",
    effects = 4,
    placebo = 4
  )

event_chaiselogpol$results
event_chaiselogpol$plot

# Chaisemartin 2024

event_intlogpol <- 
  DIDmultiplegtDYN::did_multiplegt_dyn(
    df = base_censo,
    outcome = "logpol",
    group = "Muns",
    time = "Anio",
    treatment = "DTO_dummy_cont",
    effects = 4,
    placebo = 4
  )

event_intlogpol$results
event_intlogpol$plot




# PLOTS DEL EVENT STUDY

library(dplyr)
library(ggplot2)


# 1. Callaway & Sant’Anna
callaway_df <- data.frame(
  period = as.numeric(mw.dyn$egt),
  estimate = as.numeric(mw.dyn$att.egt),
  se = as.numeric(mw.dyn$se.egt),
  Method = "Callaway & Sant'Anna (2021)"
) %>%
  mutate(
    conf.low = as.numeric(estimate - 1.96 * se),
    conf.high = as.numeric(estimate + 1.96 * se)
  )

# 2. Borusyak et al.
borusyak_df <- event_bor_logpol %>%
  mutate(
    Method = "Borusyak et al (2021)",
    period = as.numeric(term),
    estimate = as.numeric(estimate),
    se = as.numeric(std.error),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high)
  ) %>%
  select(Method, period, estimate, se, conf.low, conf.high) %>%
  filter(!is.na(estimate))  # Elimina periodos sin estimaciones

# 3. Chaisemartin & D’Haultfoeuille 2020
chaise2020_placebos <- as.data.frame(event_chaiselogpol$results$Placebos) %>%
  mutate(
    period = -as.numeric(gsub("Placebo_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2020_effects <- as.data.frame(event_chaiselogpol$results$Effects) %>%
  mutate(
    period = as.numeric(gsub("Effect_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2020_df <- bind_rows(chaise2020_placebos, chaise2020_effects) %>%
  mutate(Method = "de Chaisemartin (2020)")

# 4. Chaisemartin & D’Haultfoeuille 2024
chaise2024_placebos <- as.data.frame(event_intlogpol$results$Placebos) %>%
  mutate(
    period = -as.numeric(gsub("Placebo_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2024_effects <- as.data.frame(event_intlogpol$results$Effects) %>%
  mutate(
    period = as.numeric(gsub("Effect_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2024_df <- bind_rows(chaise2024_placebos, chaise2024_effects) %>%
  mutate(Method = " de Chaisemartin (2024)")

# Combinar todos los data frames
event_studies_df <- bind_rows(callaway_df, borusyak_df, chaise2020_df, chaise2024_df)

# 5. Graficar
p<-ggplot(event_studies_df, aes(x = period, y = estimate, color = Method, shape = Method)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.4) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(
    #title = "Comparación de Event Studies",
    x = "Time to treatment",
    y = "Coefficient") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 13),       # Tamaño del texto de la leyenda
    legend.title = element_text(size = 15),      # Tamaño del título de la leyenda
    axis.title.x = element_text(size = 15),      # Tamaño del título del eje x
    axis.title.y = element_text(size = 15),      # Tamaño del título del eje y
    axis.text.x = element_text(size = 14),       # Tamaño de los valores del eje x
    axis.text.y = element_text(size = 14)        # Tamaño de los valores del eje y
  )
ggsave(
  filename = "C:/Users/mateo/Desktop/Figuras finales/event_study_logpol.jpeg", 
  plot = p, 
  width = 12, 
  height = 7, 
  dpi = 300
)

# PLOT DEL AVERAGE TREATMENT EFFECT ON THE TREATED

library(ggplot2)

# Extraer los valores de ATT y su error estándar desde los objetos

# Para callogpol
ATT_callogpol <- callogpol$overall.att
SE_callogpol <- callogpol$overall.se
CI_lower_callogpol <- ATT_callogpol - 1.96 * SE_callogpol
CI_upper_callogpol <- ATT_callogpol + 1.96 * SE_callogpol

# Para bor_logpol
ATT_bor_logpol <- bor_logpol$estimate[bor_logpol$term == "treat"]
SE_bor_logpol <- bor_logpol$std.error[bor_logpol$term == "treat"]
CI_lower_bor_logpol <- ATT_bor_logpol - 1.96 * SE_bor_logpol
CI_upper_bor_logpol <- ATT_bor_logpol + 1.96 * SE_bor_logpol

# Para event_chaiselogpol (averaged treatment effect)
ATT_event_chaiselogpol <- event_chaiselogpol$results$ATE[1, "Estimate"]
SE_event_chaiselogpol <- event_chaiselogpol$results$ATE[1, "SE"]
CI_lower_event_chaiselogpol <- event_chaiselogpol$results$ATE[1, "LB CI"]
CI_upper_event_chaiselogpol <- event_chaiselogpol$results$ATE[1, "UB CI"]

# Para event_intlogpol (averaged treatment effect)
ATT_event_intlogpol <- event_intlogpol$results$ATE[1, "Estimate"]
SE_event_intlogpol <- event_intlogpol$results$ATE[1, "SE"]
CI_lower_event_intlogpol <- ATT_event_intlogpol - 1.96 * SE_event_intlogpol
CI_upper_event_intlogpol <- ATT_event_intlogpol + 1.96 * SE_event_intlogpol

# Crear el dataframe para graficar
results_df <- data.frame(
  Method = c("Callaway & Sant'Anna (2021)", "Borusyak et al (2021)", "de Chaisemartin (2020)", "de Chaisemartin (2024)"),
  ATT = c(ATT_callogpol, ATT_bor_logpol, ATT_event_chaiselogpol, ATT_event_intlogpol),
  SE = c(SE_callogpol, SE_bor_logpol, SE_event_chaiselogpol, SE_event_intlogpol),
  CI_lower = c(CI_lower_callogpol, CI_lower_bor_logpol, CI_lower_event_chaiselogpol, CI_lower_event_intlogpol),
  CI_upper = c(CI_upper_callogpol, CI_upper_bor_logpol, CI_upper_event_chaiselogpol, CI_upper_event_intlogpol)
)

p<-ggplot(results_df, aes(y = Method, x = ATT, xmin = CI_lower, xmax = CI_upper)) +
  geom_pointrange() +
  geom_text(aes(label = round(ATT, 3), y = Method), vjust = -1, size = 5.5) +  # vjust controla la posición vertical
  theme_minimal() +
  geom_vline(xintercept = 0) +
  labs(
    # title = "ATT Estimates with 95% Confidence Intervals", 
    y = "Method", 
    x = "Average Treatment Effect (ATT)"
  ) +
  theme(
    axis.title.x = element_text(size = 15),      # Título del eje x
    axis.title.y = element_text(size = 15),      # Título del eje y
    axis.text.x  = element_text(size = 15),      # Etiquetas del eje x
    axis.text.y  = element_text(size = 15),      # Etiquetas del eje y
    plot.title   = element_text(size = 16, face = "bold")  # Título del gráfico
  )
ggsave(
  filename = "C:/Users/mateo/Desktop/Figuras finales/coefs_logpol.jpeg", 
  plot = p, 
  width = 12, 
  height = 7, 
  dpi = 300
)









### SE ME OLVIDÓ COPIAR EL RESTO DE LAS QUE SALEN DEL CENSO EN ESTE CÓDIGO PERO ES COPY PASTE DE LA DE ARRIBA
### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!








### TOTAL MUNICIPAL AVAILABLE POLICE

# Callaway
event_callogpatrimonio <- att_gt(yname = "logpatrimonio",
                                 gname = "first_treated",
                                 idname = "Muns",
                                 tname = "Anio",
                                 xformla = ~1,
                                 data = base_crimenes,
                                 allow_unbalanced_panel = TRUE,
                                 est_method = "reg",
                                 control_group = c("notyettreated","nevertreated")
)

mw.dyn = aggte(event_callogpatrimonio, type = "dynamic", na.rm = TRUE)

callogpatrimonio = aggte(event_callogpatrimonio, type = "simple", na.rm = TRUE)
ggdid(mw.dyn,
      title = "Event Study - Safety expenditure for public servants",
      xlab = "Period",
      ylab = "Coefficient",
      xgap = 4)

# Borusyak

event_bor_logpatrimonio = did_imputation(base_crimenes,
                                         yname = "logpatrimonio",
                                         gname =  "first_treated",
                                         tname = "Anio",
                                         idname = "Muns",
                                         first_stage = ~0 |
                                           Muns + Anio,
                                         horizon = TRUE,
                                         pretrends = TRUE) # No sé por qué con TRUE explota la desviación estándar
# mutate(type = "Borusyak") |> 
#  select(type, coef = estimate, se = std.error, conf.low, conf.high)
bor_logpatrimonio = did_imputation(base_crimenes,
                                   yname = "logpatrimonio",
                                   gname = "first_treated",
                                   tname = "Anio",
                                   idname = "Muns",
                                   first_stage = ~0 | 
                                     Muns + Anio,
)

# Chaisemartin 2020

event_chaiselogpatrimonio <- 
  DIDmultiplegtDYN::did_multiplegt_dyn(
    df = base_crimenes,
    outcome = "logpatrimonio",
    group = "Muns",
    time = "Anio",
    treatment = "DTO_dummy",
    effects = 5,
    placebo = 5
  )

event_chaiselogpatrimonio$results
event_chaiselogpatrimonio$plot

# Chaisemartin 2024

event_intlogpatrimonio <- 
  DIDmultiplegtDYN::did_multiplegt_dyn(
    df = base_crimenes,
    outcome = "logpatrimonio",
    group = "Muns",
    time = "Anio",
    treatment = "DTO_dummy_cont",
    effects = 5,
    placebo = 5
  )

event_intlogpatrimonio$results
event_intlogpatrimonio$plot




# PLOTS DEL EVENT STUDY

library(dplyr)
library(ggplot2)


# 1. Callaway & Sant’Anna
callaway_df <- data.frame(
  period = as.numeric(mw.dyn$egt),
  estimate = as.numeric(mw.dyn$att.egt),
  se = as.numeric(mw.dyn$se.egt),
  Method = "Callaway & Sant'Anna (2021)"
) %>%
  mutate(
    conf.low = as.numeric(estimate - 1.96 * se),
    conf.high = as.numeric(estimate + 1.96 * se)
  )

# 2. Borusyak et al.
borusyak_df <- event_bor_logpatrimonio %>%
  mutate(
    Method = "Borusyak et al (2021)",
    period = as.numeric(term),
    estimate = as.numeric(estimate),
    se = as.numeric(std.error),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high)
  ) %>%
  select(Method, period, estimate, se, conf.low, conf.high) %>%
  filter(!is.na(estimate))  # Elimina periodos sin estimaciones

# 3. Chaisemartin & D’Haultfoeuille 2020
chaise2020_placebos <- as.data.frame(event_chaiselogpatrimonio$results$Placebos) %>%
  mutate(
    period = -as.numeric(gsub("Placebo_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2020_effects <- as.data.frame(event_chaiselogpatrimonio$results$Effects) %>%
  mutate(
    period = as.numeric(gsub("Effect_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2020_df <- bind_rows(chaise2020_placebos, chaise2020_effects) %>%
  mutate(Method = "de Chaisemartin (2020)")

# 4. Chaisemartin & D’Haultfoeuille 2024
chaise2024_placebos <- as.data.frame(event_intlogpatrimonio$results$Placebos) %>%
  mutate(
    period = -as.numeric(gsub("Placebo_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2024_effects <- as.data.frame(event_intlogpatrimonio$results$Effects) %>%
  mutate(
    period = as.numeric(gsub("Effect_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2024_df <- bind_rows(chaise2024_placebos, chaise2024_effects) %>%
  mutate(Method = " de Chaisemartin (2024)")

# Combinar todos los data frames
event_studies_df <- bind_rows(callaway_df, borusyak_df, chaise2020_df, chaise2024_df)

# 5. Graficar
p<-ggplot(event_studies_df, aes(x = period, y = estimate, color = Method, shape = Method)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.4) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(
    #title = "Comparación de Event Studies",
    x = "Time to treatment",
    y = "Coefficient") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 13),       # Tamaño del texto de la leyenda
    legend.title = element_text(size = 15),      # Tamaño del título de la leyenda
    axis.title.x = element_text(size = 15),      # Tamaño del título del eje x
    axis.title.y = element_text(size = 15),      # Tamaño del título del eje y
    axis.text.x = element_text(size = 14),       # Tamaño de los valores del eje x
    axis.text.y = element_text(size = 14)        # Tamaño de los valores del eje y
  )
ggsave(
  filename = "C:/Users/mateo/Desktop/Figuras finales/event_study_logpatrimonio.jpeg", 
  plot = p, 
  width = 12, 
  height = 7, 
  dpi = 300
)

# PLOT DEL AVERAGE TREATMENT EFFECT ON THE TREATED

library(ggplot2)

# Extraer los valores de ATT y su error estándar desde los objetos

# Para callogpatrimonio
ATT_callogpatrimonio <- callogpatrimonio$overall.att
SE_callogpatrimonio <- callogpatrimonio$overall.se
CI_lower_callogpatrimonio <- ATT_callogpatrimonio - 1.96 * SE_callogpatrimonio
CI_upper_callogpatrimonio <- ATT_callogpatrimonio + 1.96 * SE_callogpatrimonio

# Para bor_logpatrimonio
ATT_bor_logpatrimonio <- bor_logpatrimonio$estimate[bor_logpatrimonio$term == "treat"]
SE_bor_logpatrimonio <- bor_logpatrimonio$std.error[bor_logpatrimonio$term == "treat"]
CI_lower_bor_logpatrimonio <- ATT_bor_logpatrimonio - 1.96 * SE_bor_logpatrimonio
CI_upper_bor_logpatrimonio <- ATT_bor_logpatrimonio + 1.96 * SE_bor_logpatrimonio

# Para event_chaiselogpatrimonio (averaged treatment effect)
ATT_event_chaiselogpatrimonio <- event_chaiselogpatrimonio$results$ATE[1, "Estimate"]
SE_event_chaiselogpatrimonio <- event_chaiselogpatrimonio$results$ATE[1, "SE"]
CI_lower_event_chaiselogpatrimonio <- event_chaiselogpatrimonio$results$ATE[1, "LB CI"]
CI_upper_event_chaiselogpatrimonio <- event_chaiselogpatrimonio$results$ATE[1, "UB CI"]

# Para event_intlogpatrimonio (averaged treatment effect)
ATT_event_intlogpatrimonio <- event_intlogpatrimonio$results$ATE[1, "Estimate"]
SE_event_intlogpatrimonio <- event_intlogpatrimonio$results$ATE[1, "SE"]
CI_lower_event_intlogpatrimonio <- ATT_event_intlogpatrimonio - 1.96 * SE_event_intlogpatrimonio
CI_upper_event_intlogpatrimonio <- ATT_event_intlogpatrimonio + 1.96 * SE_event_intlogpatrimonio

# Crear el dataframe para graficar
results_df <- data.frame(
  Method = c("Callaway & Sant'Anna (2021)", "Borusyak et al (2021)", "de Chaisemartin (2020)", "de Chaisemartin (2024)"),
  ATT = c(ATT_callogpatrimonio, ATT_bor_logpatrimonio, ATT_event_chaiselogpatrimonio, ATT_event_intlogpatrimonio),
  SE = c(SE_callogpatrimonio, SE_bor_logpatrimonio, SE_event_chaiselogpatrimonio, SE_event_intlogpatrimonio),
  CI_lower = c(CI_lower_callogpatrimonio, CI_lower_bor_logpatrimonio, CI_lower_event_chaiselogpatrimonio, CI_lower_event_intlogpatrimonio),
  CI_upper = c(CI_upper_callogpatrimonio, CI_upper_bor_logpatrimonio, CI_upper_event_chaiselogpatrimonio, CI_upper_event_intlogpatrimonio)
)

p<-ggplot(results_df, aes(y = Method, x = ATT, xmin = CI_lower, xmax = CI_upper)) +
  geom_pointrange() +
  geom_text(aes(label = round(ATT, 3), y = Method), vjust = -1, size = 5.5) +  # vjust controla la posición vertical
  theme_minimal() +
  geom_vline(xintercept = 0) +
  labs(
    # title = "ATT Estimates with 95% Confidence Intervals", 
    y = "Method", 
    x = "Average Treatment Effect (ATT)"
  ) +
  theme(
    axis.title.x = element_text(size = 15),      # Título del eje x
    axis.title.y = element_text(size = 15),      # Título del eje y
    axis.text.x  = element_text(size = 15),      # Etiquetas del eje x
    axis.text.y  = element_text(size = 15),      # Etiquetas del eje y
    plot.title   = element_text(size = 16, face = "bold")  # Título del gráfico
  )
ggsave(
  filename = "C:/Users/mateo/Desktop/Figuras finales/coefs_logpatrimonio.jpeg", 
  plot = p, 
  width = 12, 
  height = 7, 
  dpi = 300
)










### TOTAL MUNICIPAL AVAILABLE POLICE

# Callaway
event_calloglesiones <- att_gt(yname = "loglesiones",
                               gname = "first_treated",
                               idname = "Muns",
                               tname = "Anio",
                               xformla = ~1,
                               data = base_crimenes,
                               allow_unbalanced_panel = TRUE,
                               est_method = "reg",
                               control_group = c("notyettreated","nevertreated")
)

mw.dyn = aggte(event_calloglesiones, type = "dynamic", na.rm = TRUE)

calloglesiones = aggte(event_calloglesiones, type = "simple", na.rm = TRUE)
ggdid(mw.dyn,
      title = "Event Study - Safety expenditure for public servants",
      xlab = "Period",
      ylab = "Coefficient",
      xgap = 4)

# Borusyak

event_bor_loglesiones = did_imputation(base_crimenes,
                                       yname = "loglesiones",
                                       gname =  "first_treated",
                                       tname = "Anio",
                                       idname = "Muns",
                                       first_stage = ~0 |
                                         Muns + Anio,
                                       horizon = TRUE,
                                       pretrends = TRUE) # No sé por qué con TRUE explota la desviación estándar
# mutate(type = "Borusyak") |> 
#  select(type, coef = estimate, se = std.error, conf.low, conf.high)
bor_loglesiones = did_imputation(base_crimenes,
                                 yname = "loglesiones",
                                 gname = "first_treated",
                                 tname = "Anio",
                                 idname = "Muns",
                                 first_stage = ~0 | 
                                   Muns + Anio,
)

# Chaisemartin 2020

event_chaiseloglesiones <- 
  DIDmultiplegtDYN::did_multiplegt_dyn(
    df = base_crimenes,
    outcome = "loglesiones",
    group = "Muns",
    time = "Anio",
    treatment = "DTO_dummy",
    effects = 5,
    placebo = 5
  )

event_chaiseloglesiones$results
event_chaiseloglesiones$plot

# Chaisemartin 2024

event_intloglesiones <- 
  DIDmultiplegtDYN::did_multiplegt_dyn(
    df = base_crimenes,
    outcome = "loglesiones",
    group = "Muns",
    time = "Anio",
    treatment = "DTO_dummy_cont",
    effects = 5,
    placebo = 5
  )

event_intloglesiones$results
event_intloglesiones$plot




# PLOTS DEL EVENT STUDY

library(dplyr)
library(ggplot2)


# 1. Callaway & Sant’Anna
callaway_df <- data.frame(
  period = as.numeric(mw.dyn$egt),
  estimate = as.numeric(mw.dyn$att.egt),
  se = as.numeric(mw.dyn$se.egt),
  Method = "Callaway & Sant'Anna (2021)"
) %>%
  mutate(
    conf.low = as.numeric(estimate - 1.96 * se),
    conf.high = as.numeric(estimate + 1.96 * se)
  )

# 2. Borusyak et al.
borusyak_df <- event_bor_loglesiones %>%
  mutate(
    Method = "Borusyak et al (2021)",
    period = as.numeric(term),
    estimate = as.numeric(estimate),
    se = as.numeric(std.error),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high)
  ) %>%
  select(Method, period, estimate, se, conf.low, conf.high) %>%
  filter(!is.na(estimate))  # Elimina periodos sin estimaciones

# 3. Chaisemartin & D’Haultfoeuille 2020
chaise2020_placebos <- as.data.frame(event_chaiseloglesiones$results$Placebos) %>%
  mutate(
    period = -as.numeric(gsub("Placebo_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2020_effects <- as.data.frame(event_chaiseloglesiones$results$Effects) %>%
  mutate(
    period = as.numeric(gsub("Effect_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2020_df <- bind_rows(chaise2020_placebos, chaise2020_effects) %>%
  mutate(Method = "de Chaisemartin (2020)")

# 4. Chaisemartin & D’Haultfoeuille 2024
chaise2024_placebos <- as.data.frame(event_intloglesiones$results$Placebos) %>%
  mutate(
    period = -as.numeric(gsub("Placebo_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2024_effects <- as.data.frame(event_intloglesiones$results$Effects) %>%
  mutate(
    period = as.numeric(gsub("Effect_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2024_df <- bind_rows(chaise2024_placebos, chaise2024_effects) %>%
  mutate(Method = " de Chaisemartin (2024)")

# Combinar todos los data frames
event_studies_df <- bind_rows(callaway_df, borusyak_df, chaise2020_df, chaise2024_df)

# 5. Graficar
p<-ggplot(event_studies_df, aes(x = period, y = estimate, color = Method, shape = Method)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.4) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(
    #title = "Comparación de Event Studies",
    x = "Time to treatment",
    y = "Coefficient") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 13),       # Tamaño del texto de la leyenda
    legend.title = element_text(size = 15),      # Tamaño del título de la leyenda
    axis.title.x = element_text(size = 15),      # Tamaño del título del eje x
    axis.title.y = element_text(size = 15),      # Tamaño del título del eje y
    axis.text.x = element_text(size = 14),       # Tamaño de los valores del eje x
    axis.text.y = element_text(size = 14)        # Tamaño de los valores del eje y
  )
ggsave(
  filename = "C:/Users/mateo/Desktop/Figuras finales/event_study_loglesiones.jpeg", 
  plot = p, 
  width = 12, 
  height = 7, 
  dpi = 300
)

# PLOT DEL AVERAGE TREATMENT EFFECT ON THE TREATED

library(ggplot2)

# Extraer los valores de ATT y su error estándar desde los objetos

# Para calloglesiones
ATT_calloglesiones <- calloglesiones$overall.att
SE_calloglesiones <- calloglesiones$overall.se
CI_lower_calloglesiones <- ATT_calloglesiones - 1.96 * SE_calloglesiones
CI_upper_calloglesiones <- ATT_calloglesiones + 1.96 * SE_calloglesiones

# Para bor_loglesiones
ATT_bor_loglesiones <- bor_loglesiones$estimate[bor_loglesiones$term == "treat"]
SE_bor_loglesiones <- bor_loglesiones$std.error[bor_loglesiones$term == "treat"]
CI_lower_bor_loglesiones <- ATT_bor_loglesiones - 1.96 * SE_bor_loglesiones
CI_upper_bor_loglesiones <- ATT_bor_loglesiones + 1.96 * SE_bor_loglesiones

# Para event_chaiseloglesiones (averaged treatment effect)
ATT_event_chaiseloglesiones <- event_chaiseloglesiones$results$ATE[1, "Estimate"]
SE_event_chaiseloglesiones <- event_chaiseloglesiones$results$ATE[1, "SE"]
CI_lower_event_chaiseloglesiones <- event_chaiseloglesiones$results$ATE[1, "LB CI"]
CI_upper_event_chaiseloglesiones <- event_chaiseloglesiones$results$ATE[1, "UB CI"]

# Para event_intloglesiones (averaged treatment effect)
ATT_event_intloglesiones <- event_intloglesiones$results$ATE[1, "Estimate"]
SE_event_intloglesiones <- event_intloglesiones$results$ATE[1, "SE"]
CI_lower_event_intloglesiones <- ATT_event_intloglesiones - 1.96 * SE_event_intloglesiones
CI_upper_event_intloglesiones <- ATT_event_intloglesiones + 1.96 * SE_event_intloglesiones

# Crear el dataframe para graficar
results_df <- data.frame(
  Method = c("Callaway & Sant'Anna (2021)", "Borusyak et al (2021)", "de Chaisemartin (2020)", "de Chaisemartin (2024)"),
  ATT = c(ATT_calloglesiones, ATT_bor_loglesiones, ATT_event_chaiseloglesiones, ATT_event_intloglesiones),
  SE = c(SE_calloglesiones, SE_bor_loglesiones, SE_event_chaiseloglesiones, SE_event_intloglesiones),
  CI_lower = c(CI_lower_calloglesiones, CI_lower_bor_loglesiones, CI_lower_event_chaiseloglesiones, CI_lower_event_intloglesiones),
  CI_upper = c(CI_upper_calloglesiones, CI_upper_bor_loglesiones, CI_upper_event_chaiseloglesiones, CI_upper_event_intloglesiones)
)

p<-ggplot(results_df, aes(y = Method, x = ATT, xmin = CI_lower, xmax = CI_upper)) +
  geom_pointrange() +
  geom_text(aes(label = round(ATT, 3), y = Method), vjust = -1, size = 5.5) +  # vjust controla la posición vertical
  theme_minimal() +
  geom_vline(xintercept = 0) +
  labs(
    # title = "ATT Estimates with 95% Confidence Intervals", 
    y = "Method", 
    x = "Average Treatment Effect (ATT)"
  ) +
  theme(
    axis.title.x = element_text(size = 15),      # Título del eje x
    axis.title.y = element_text(size = 15),      # Título del eje y
    axis.text.x  = element_text(size = 15),      # Etiquetas del eje x
    axis.text.y  = element_text(size = 15),      # Etiquetas del eje y
    plot.title   = element_text(size = 16, face = "bold")  # Título del gráfico
  )
ggsave(
  filename = "C:/Users/mateo/Desktop/Figuras finales/coefs_loglesiones.jpeg", 
  plot = p, 
  width = 12, 
  height = 7, 
  dpi = 300
)









### TOTAL MUNICIPAL AVAILABLE POLICE

# Callaway
event_callogviolacion <- att_gt(yname = "logviolacion",
                                gname = "first_treated",
                                idname = "Muns",
                                tname = "Anio",
                                xformla = ~1,
                                data = base_crimenes,
                                allow_unbalanced_panel = TRUE,
                                est_method = "reg",
                                control_group = c("notyettreated","nevertreated")
)

mw.dyn = aggte(event_callogviolacion, type = "dynamic", na.rm = TRUE)

callogviolacion = aggte(event_callogviolacion, type = "simple", na.rm = TRUE)
ggdid(mw.dyn,
      title = "Event Study - Safety expenditure for public servants",
      xlab = "Period",
      ylab = "Coefficient",
      xgap = 4)

# Borusyak

event_bor_logviolacion = did_imputation(base_crimenes,
                                        yname = "logviolacion",
                                        gname =  "first_treated",
                                        tname = "Anio",
                                        idname = "Muns",
                                        first_stage = ~0 |
                                          Muns + Anio,
                                        horizon = TRUE,
                                        pretrends = TRUE) # No sé por qué con TRUE explota la desviación estándar
# mutate(type = "Borusyak") |> 
#  select(type, coef = estimate, se = std.error, conf.low, conf.high)
bor_logviolacion = did_imputation(base_crimenes,
                                  yname = "logviolacion",
                                  gname = "first_treated",
                                  tname = "Anio",
                                  idname = "Muns",
                                  first_stage = ~0 | 
                                    Muns + Anio,
)

# Chaisemartin 2020

event_chaiselogviolacion <- 
  DIDmultiplegtDYN::did_multiplegt_dyn(
    df = base_crimenes,
    outcome = "logviolacion",
    group = "Muns",
    time = "Anio",
    treatment = "DTO_dummy",
    effects = 5,
    placebo = 5
  )

event_chaiselogviolacion$results
event_chaiselogviolacion$plot

# Chaisemartin 2024

event_intlogviolacion <- 
  DIDmultiplegtDYN::did_multiplegt_dyn(
    df = base_crimenes,
    outcome = "logviolacion",
    group = "Muns",
    time = "Anio",
    treatment = "DTO_dummy_cont",
    effects = 5,
    placebo = 5
  )

event_intlogviolacion$results
event_intlogviolacion$plot




# PLOTS DEL EVENT STUDY

library(dplyr)
library(ggplot2)


# 1. Callaway & Sant’Anna
callaway_df <- data.frame(
  period = as.numeric(mw.dyn$egt),
  estimate = as.numeric(mw.dyn$att.egt),
  se = as.numeric(mw.dyn$se.egt),
  Method = "Callaway & Sant'Anna (2021)"
) %>%
  mutate(
    conf.low = as.numeric(estimate - 1.96 * se),
    conf.high = as.numeric(estimate + 1.96 * se)
  )

# 2. Borusyak et al.
borusyak_df <- event_bor_logviolacion %>%
  mutate(
    Method = "Borusyak et al (2021)",
    period = as.numeric(term),
    estimate = as.numeric(estimate),
    se = as.numeric(std.error),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high)
  ) %>%
  select(Method, period, estimate, se, conf.low, conf.high) %>%
  filter(!is.na(estimate))  # Elimina periodos sin estimaciones

# 3. Chaisemartin & D’Haultfoeuille 2020
chaise2020_placebos <- as.data.frame(event_chaiselogviolacion$results$Placebos) %>%
  mutate(
    period = -as.numeric(gsub("Placebo_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2020_effects <- as.data.frame(event_chaiselogviolacion$results$Effects) %>%
  mutate(
    period = as.numeric(gsub("Effect_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2020_df <- bind_rows(chaise2020_placebos, chaise2020_effects) %>%
  mutate(Method = "de Chaisemartin (2020)")

# 4. Chaisemartin & D’Haultfoeuille 2024
chaise2024_placebos <- as.data.frame(event_intlogviolacion$results$Placebos) %>%
  mutate(
    period = -as.numeric(gsub("Placebo_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2024_effects <- as.data.frame(event_intlogviolacion$results$Effects) %>%
  mutate(
    period = as.numeric(gsub("Effect_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2024_df <- bind_rows(chaise2024_placebos, chaise2024_effects) %>%
  mutate(Method = " de Chaisemartin (2024)")

# Combinar todos los data frames
event_studies_df <- bind_rows(callaway_df, borusyak_df, chaise2020_df, chaise2024_df)

# 5. Graficar
p<-ggplot(event_studies_df, aes(x = period, y = estimate, color = Method, shape = Method)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.4) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(
    #title = "Comparación de Event Studies",
    x = "Time to treatment",
    y = "Coefficient") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 13),       # Tamaño del texto de la leyenda
    legend.title = element_text(size = 15),      # Tamaño del título de la leyenda
    axis.title.x = element_text(size = 15),      # Tamaño del título del eje x
    axis.title.y = element_text(size = 15),      # Tamaño del título del eje y
    axis.text.x = element_text(size = 14),       # Tamaño de los valores del eje x
    axis.text.y = element_text(size = 14)        # Tamaño de los valores del eje y
  )
ggsave(
  filename = "C:/Users/mateo/Desktop/Figuras finales/event_study_logviolacion.jpeg", 
  plot = p, 
  width = 12, 
  height = 7, 
  dpi = 300
)

# PLOT DEL AVERAGE TREATMENT EFFECT ON THE TREATED

library(ggplot2)

# Extraer los valores de ATT y su error estándar desde los objetos

# Para callogviolacion
ATT_callogviolacion <- callogviolacion$overall.att
SE_callogviolacion <- callogviolacion$overall.se
CI_lower_callogviolacion <- ATT_callogviolacion - 1.96 * SE_callogviolacion
CI_upper_callogviolacion <- ATT_callogviolacion + 1.96 * SE_callogviolacion

# Para bor_logviolacion
ATT_bor_logviolacion <- bor_logviolacion$estimate[bor_logviolacion$term == "treat"]
SE_bor_logviolacion <- bor_logviolacion$std.error[bor_logviolacion$term == "treat"]
CI_lower_bor_logviolacion <- ATT_bor_logviolacion - 1.96 * SE_bor_logviolacion
CI_upper_bor_logviolacion <- ATT_bor_logviolacion + 1.96 * SE_bor_logviolacion

# Para event_chaiselogviolacion (averaged treatment effect)
ATT_event_chaiselogviolacion <- event_chaiselogviolacion$results$ATE[1, "Estimate"]
SE_event_chaiselogviolacion <- event_chaiselogviolacion$results$ATE[1, "SE"]
CI_lower_event_chaiselogviolacion <- event_chaiselogviolacion$results$ATE[1, "LB CI"]
CI_upper_event_chaiselogviolacion <- event_chaiselogviolacion$results$ATE[1, "UB CI"]

# Para event_intlogviolacion (averaged treatment effect)
ATT_event_intlogviolacion <- event_intlogviolacion$results$ATE[1, "Estimate"]
SE_event_intlogviolacion <- event_intlogviolacion$results$ATE[1, "SE"]
CI_lower_event_intlogviolacion <- ATT_event_intlogviolacion - 1.96 * SE_event_intlogviolacion
CI_upper_event_intlogviolacion <- ATT_event_intlogviolacion + 1.96 * SE_event_intlogviolacion

# Crear el dataframe para graficar
results_df <- data.frame(
  Method = c("Callaway & Sant'Anna (2021)", "Borusyak et al (2021)", "de Chaisemartin (2020)", "de Chaisemartin (2024)"),
  ATT = c(ATT_callogviolacion, ATT_bor_logviolacion, ATT_event_chaiselogviolacion, ATT_event_intlogviolacion),
  SE = c(SE_callogviolacion, SE_bor_logviolacion, SE_event_chaiselogviolacion, SE_event_intlogviolacion),
  CI_lower = c(CI_lower_callogviolacion, CI_lower_bor_logviolacion, CI_lower_event_chaiselogviolacion, CI_lower_event_intlogviolacion),
  CI_upper = c(CI_upper_callogviolacion, CI_upper_bor_logviolacion, CI_upper_event_chaiselogviolacion, CI_upper_event_intlogviolacion)
)

p<-ggplot(results_df, aes(y = Method, x = ATT, xmin = CI_lower, xmax = CI_upper)) +
  geom_pointrange() +
  geom_text(aes(label = round(ATT, 3), y = Method), vjust = -1, size = 5.5) +  # vjust controla la posición vertical
  theme_minimal() +
  geom_vline(xintercept = 0) +
  labs(
    # title = "ATT Estimates with 95% Confidence Intervals", 
    y = "Method", 
    x = "Average Treatment Effect (ATT)"
  ) +
  theme(
    axis.title.x = element_text(size = 15),      # Título del eje x
    axis.title.y = element_text(size = 15),      # Título del eje y
    axis.text.x  = element_text(size = 15),      # Etiquetas del eje x
    axis.text.y  = element_text(size = 15),      # Etiquetas del eje y
    plot.title   = element_text(size = 16, face = "bold")  # Título del gráfico
  )
ggsave(
  filename = "C:/Users/mateo/Desktop/Figuras finales/coefs_logviolacion.jpeg", 
  plot = p, 
  width = 12, 
  height = 7, 
  dpi = 300
)






### TOTAL MUNICIPAL AVAILABLE POLICE

# Callaway
event_callogrobo <- att_gt(yname = "logrobo",
                           gname = "first_treated",
                           idname = "Muns",
                           tname = "Anio",
                           xformla = ~1,
                           data = base_crimenes,
                           allow_unbalanced_panel = TRUE,
                           est_method = "reg",
                           control_group = c("notyettreated","nevertreated")
)

mw.dyn = aggte(event_callogrobo, type = "dynamic", na.rm = TRUE)

callogrobo = aggte(event_callogrobo, type = "simple", na.rm = TRUE)
ggdid(mw.dyn,
      title = "Event Study - Safety expenditure for public servants",
      xlab = "Period",
      ylab = "Coefficient",
      xgap = 4)

# Borusyak

event_bor_logrobo = did_imputation(base_crimenes,
                                   yname = "logrobo",
                                   gname =  "first_treated",
                                   tname = "Anio",
                                   idname = "Muns",
                                   first_stage = ~0 |
                                     Muns + Anio,
                                   horizon = TRUE,
                                   pretrends = TRUE) # No sé por qué con TRUE explota la desviación estándar
# mutate(type = "Borusyak") |> 
#  select(type, coef = estimate, se = std.error, conf.low, conf.high)
bor_logrobo = did_imputation(base_crimenes,
                             yname = "logrobo",
                             gname = "first_treated",
                             tname = "Anio",
                             idname = "Muns",
                             first_stage = ~0 | 
                               Muns + Anio,
)

# Chaisemartin 2020

event_chaiselogrobo <- 
  DIDmultiplegtDYN::did_multiplegt_dyn(
    df = base_crimenes,
    outcome = "logrobo",
    group = "Muns",
    time = "Anio",
    treatment = "DTO_dummy",
    effects = 5,
    placebo = 5
  )

event_chaiselogrobo$results
event_chaiselogrobo$plot

# Chaisemartin 2024

event_intlogrobo <- 
  DIDmultiplegtDYN::did_multiplegt_dyn(
    df = base_crimenes,
    outcome = "logrobo",
    group = "Muns",
    time = "Anio",
    treatment = "DTO_dummy_cont",
    effects = 5,
    placebo = 5
  )

event_intlogrobo$results
event_intlogrobo$plot




# PLOTS DEL EVENT STUDY

library(dplyr)
library(ggplot2)


# 1. Callaway & Sant’Anna
callaway_df <- data.frame(
  period = as.numeric(mw.dyn$egt),
  estimate = as.numeric(mw.dyn$att.egt),
  se = as.numeric(mw.dyn$se.egt),
  Method = "Callaway & Sant'Anna (2021)"
) %>%
  mutate(
    conf.low = as.numeric(estimate - 1.96 * se),
    conf.high = as.numeric(estimate + 1.96 * se)
  )

# 2. Borusyak et al.
borusyak_df <- event_bor_logrobo %>%
  mutate(
    Method = "Borusyak et al (2021)",
    period = as.numeric(term),
    estimate = as.numeric(estimate),
    se = as.numeric(std.error),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high)
  ) %>%
  select(Method, period, estimate, se, conf.low, conf.high) %>%
  filter(!is.na(estimate))  # Elimina periodos sin estimaciones

# 3. Chaisemartin & D’Haultfoeuille 2020
chaise2020_placebos <- as.data.frame(event_chaiselogrobo$results$Placebos) %>%
  mutate(
    period = -as.numeric(gsub("Placebo_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2020_effects <- as.data.frame(event_chaiselogrobo$results$Effects) %>%
  mutate(
    period = as.numeric(gsub("Effect_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2020_df <- bind_rows(chaise2020_placebos, chaise2020_effects) %>%
  mutate(Method = "de Chaisemartin (2020)")

# 4. Chaisemartin & D’Haultfoeuille 2024
chaise2024_placebos <- as.data.frame(event_intlogrobo$results$Placebos) %>%
  mutate(
    period = -as.numeric(gsub("Placebo_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2024_effects <- as.data.frame(event_intlogrobo$results$Effects) %>%
  mutate(
    period = as.numeric(gsub("Effect_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2024_df <- bind_rows(chaise2024_placebos, chaise2024_effects) %>%
  mutate(Method = " de Chaisemartin (2024)")

# Combinar todos los data frames
event_studies_df <- bind_rows(callaway_df, borusyak_df, chaise2020_df, chaise2024_df)

# 5. Graficar
p<-ggplot(event_studies_df, aes(x = period, y = estimate, color = Method, shape = Method)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.4) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(
    #title = "Comparación de Event Studies",
    x = "Time to treatment",
    y = "Coefficient") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 13),       # Tamaño del texto de la leyenda
    legend.title = element_text(size = 15),      # Tamaño del título de la leyenda
    axis.title.x = element_text(size = 15),      # Tamaño del título del eje x
    axis.title.y = element_text(size = 15),      # Tamaño del título del eje y
    axis.text.x = element_text(size = 14),       # Tamaño de los valores del eje x
    axis.text.y = element_text(size = 14)        # Tamaño de los valores del eje y
  )
ggsave(
  filename = "C:/Users/mateo/Desktop/Figuras finales/event_study_logrobo.jpeg", 
  plot = p, 
  width = 12, 
  height = 7, 
  dpi = 300
)

# PLOT DEL AVERAGE TREATMENT EFFECT ON THE TREATED

library(ggplot2)

# Extraer los valores de ATT y su error estándar desde los objetos

# Para callogrobo
ATT_callogrobo <- callogrobo$overall.att
SE_callogrobo <- callogrobo$overall.se
CI_lower_callogrobo <- ATT_callogrobo - 1.96 * SE_callogrobo
CI_upper_callogrobo <- ATT_callogrobo + 1.96 * SE_callogrobo

# Para bor_logrobo
ATT_bor_logrobo <- bor_logrobo$estimate[bor_logrobo$term == "treat"]
SE_bor_logrobo <- bor_logrobo$std.error[bor_logrobo$term == "treat"]
CI_lower_bor_logrobo <- ATT_bor_logrobo - 1.96 * SE_bor_logrobo
CI_upper_bor_logrobo <- ATT_bor_logrobo + 1.96 * SE_bor_logrobo

# Para event_chaiselogrobo (averaged treatment effect)
ATT_event_chaiselogrobo <- event_chaiselogrobo$results$ATE[1, "Estimate"]
SE_event_chaiselogrobo <- event_chaiselogrobo$results$ATE[1, "SE"]
CI_lower_event_chaiselogrobo <- event_chaiselogrobo$results$ATE[1, "LB CI"]
CI_upper_event_chaiselogrobo <- event_chaiselogrobo$results$ATE[1, "UB CI"]

# Para event_intlogrobo (averaged treatment effect)
ATT_event_intlogrobo <- event_intlogrobo$results$ATE[1, "Estimate"]
SE_event_intlogrobo <- event_intlogrobo$results$ATE[1, "SE"]
CI_lower_event_intlogrobo <- ATT_event_intlogrobo - 1.96 * SE_event_intlogrobo
CI_upper_event_intlogrobo <- ATT_event_intlogrobo + 1.96 * SE_event_intlogrobo

# Crear el dataframe para graficar
results_df <- data.frame(
  Method = c("Callaway & Sant'Anna (2021)", "Borusyak et al (2021)", "de Chaisemartin (2020)", "de Chaisemartin (2024)"),
  ATT = c(ATT_callogrobo, ATT_bor_logrobo, ATT_event_chaiselogrobo, ATT_event_intlogrobo),
  SE = c(SE_callogrobo, SE_bor_logrobo, SE_event_chaiselogrobo, SE_event_intlogrobo),
  CI_lower = c(CI_lower_callogrobo, CI_lower_bor_logrobo, CI_lower_event_chaiselogrobo, CI_lower_event_intlogrobo),
  CI_upper = c(CI_upper_callogrobo, CI_upper_bor_logrobo, CI_upper_event_chaiselogrobo, CI_upper_event_intlogrobo)
)

p<-ggplot(results_df, aes(y = Method, x = ATT, xmin = CI_lower, xmax = CI_upper)) +
  geom_pointrange() +
  geom_text(aes(label = round(ATT, 3), y = Method), vjust = -1, size = 5.5) +  # vjust controla la posición vertical
  theme_minimal() +
  geom_vline(xintercept = 0) +
  labs(
    # title = "ATT Estimates with 95% Confidence Intervals", 
    y = "Method", 
    x = "Average Treatment Effect (ATT)"
  ) +
  theme(
    axis.title.x = element_text(size = 15),      # Título del eje x
    axis.title.y = element_text(size = 15),      # Título del eje y
    axis.text.x  = element_text(size = 15),      # Etiquetas del eje x
    axis.text.y  = element_text(size = 15),      # Etiquetas del eje y
    plot.title   = element_text(size = 16, face = "bold")  # Título del gráfico
  )
ggsave(
  filename = "C:/Users/mateo/Desktop/Figuras finales/coefs_logrobo.jpeg", 
  plot = p, 
  width = 12, 
  height = 7, 
  dpi = 300
)






### TOTAL MUNICIPAL AVAILABLE POLICE

# Callaway
event_callogganado <- att_gt(yname = "logganado",
                             gname = "first_treated",
                             idname = "Muns",
                             tname = "Anio",
                             xformla = ~1,
                             data = base_ganado_carreteras,
                             allow_unbalanced_panel = TRUE,
                             est_method = "reg",
                             control_group = c("notyettreated","nevertreated")
)

mw.dyn = aggte(event_callogganado, type = "dynamic", na.rm = TRUE)

callogganado = aggte(event_callogganado, type = "simple", na.rm = TRUE)
ggdid(mw.dyn,
      title = "Event Study - Safety expenditure for public servants",
      xlab = "Period",
      ylab = "Coefficient",
      xgap = 4)

# Borusyak

event_bor_logganado = did_imputation(base_ganado_carreteras,
                                     yname = "logganado",
                                     gname =  "first_treated",
                                     tname = "Anio",
                                     idname = "Muns",
                                     first_stage = ~0 |
                                       Muns + Anio,
                                     horizon = TRUE,
                                     pretrends = TRUE) # No sé por qué con TRUE explota la desviación estándar
# mutate(type = "Borusyak") |> 
#  select(type, coef = estimate, se = std.error, conf.low, conf.high)
bor_logganado = did_imputation(base_ganado_carreteras,
                               yname = "logganado",
                               gname = "first_treated",
                               tname = "Anio",
                               idname = "Muns",
                               first_stage = ~0 | 
                                 Muns + Anio,
)

# Chaisemartin 2020

event_chaiselogganado <- 
  DIDmultiplegtDYN::did_multiplegt_dyn(
    df = base_ganado_carreteras,
    outcome = "logganado",
    group = "Muns",
    time = "Anio",
    treatment = "DTO_dummy",
    effects = 5,
    placebo = 5
  )

event_chaiselogganado$results
event_chaiselogganado$plot

# Chaisemartin 2024

event_intlogganado <- 
  DIDmultiplegtDYN::did_multiplegt_dyn(
    df = base_ganado_carreteras,
    outcome = "logganado",
    group = "Muns",
    time = "Anio",
    treatment = "DTO_dummy_cont",
    effects = 5,
    placebo = 5
  )

event_intlogganado$results
event_intlogganado$plot




# PLOTS DEL EVENT STUDY

library(dplyr)
library(ggplot2)


# 1. Callaway & Sant’Anna
callaway_df <- data.frame(
  period = as.numeric(mw.dyn$egt),
  estimate = as.numeric(mw.dyn$att.egt),
  se = as.numeric(mw.dyn$se.egt),
  Method = "Callaway & Sant'Anna (2021)"
) %>%
  mutate(
    conf.low = as.numeric(estimate - 1.96 * se),
    conf.high = as.numeric(estimate + 1.96 * se)
  )

# 2. Borusyak et al.
borusyak_df <- event_bor_logganado %>%
  mutate(
    Method = "Borusyak et al (2021)",
    period = as.numeric(term),
    estimate = as.numeric(estimate),
    se = as.numeric(std.error),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high)
  ) %>%
  select(Method, period, estimate, se, conf.low, conf.high) %>%
  filter(!is.na(estimate))  # Elimina periodos sin estimaciones

# 3. Chaisemartin & D’Haultfoeuille 2020
chaise2020_placebos <- as.data.frame(event_chaiselogganado$results$Placebos) %>%
  mutate(
    period = -as.numeric(gsub("Placebo_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2020_effects <- as.data.frame(event_chaiselogganado$results$Effects) %>%
  mutate(
    period = as.numeric(gsub("Effect_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2020_df <- bind_rows(chaise2020_placebos, chaise2020_effects) %>%
  mutate(Method = "de Chaisemartin (2020)")

# 4. Chaisemartin & D’Haultfoeuille 2024
chaise2024_placebos <- as.data.frame(event_intlogganado$results$Placebos) %>%
  mutate(
    period = -as.numeric(gsub("Placebo_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2024_effects <- as.data.frame(event_intlogganado$results$Effects) %>%
  mutate(
    period = as.numeric(gsub("Effect_", "", rownames(.))),
    estimate = as.numeric(Estimate),
    se = as.numeric(SE),
    conf.low = as.numeric(`LB CI`),
    conf.high = as.numeric(`UB CI`)
  ) %>%
  select(period, estimate, se, conf.low, conf.high)

chaise2024_df <- bind_rows(chaise2024_placebos, chaise2024_effects) %>%
  mutate(Method = " de Chaisemartin (2024)")

# Combinar todos los data frames
event_studies_df <- bind_rows(callaway_df, borusyak_df, chaise2020_df, chaise2024_df)

# 5. Graficar
p<-ggplot(event_studies_df, aes(x = period, y = estimate, color = Method, shape = Method)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.4) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(
    #title = "Comparación de Event Studies",
    x = "Time to treatment",
    y = "Coefficient") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 13),       # Tamaño del texto de la leyenda
    legend.title = element_text(size = 15),      # Tamaño del título de la leyenda
    axis.title.x = element_text(size = 15),      # Tamaño del título del eje x
    axis.title.y = element_text(size = 15),      # Tamaño del título del eje y
    axis.text.x = element_text(size = 14),       # Tamaño de los valores del eje x
    axis.text.y = element_text(size = 14)        # Tamaño de los valores del eje y
  )
ggsave(
  filename = "C:/Users/mateo/Desktop/Figuras finales/event_study_logganado.jpeg", 
  plot = p, 
  width = 12, 
  height = 7, 
  dpi = 300
)

# Plot of ATT

library(ggplot2)



# Para callogganado
ATT_callogganado <- callogganado$overall.att
SE_callogganado <- callogganado$overall.se
CI_lower_callogganado <- ATT_callogganado - 1.96 * SE_callogganado
CI_upper_callogganado <- ATT_callogganado + 1.96 * SE_callogganado

# Para bor_logganado
ATT_bor_logganado <- bor_logganado$estimate[bor_logganado$term == "treat"]
SE_bor_logganado <- bor_logganado$std.error[bor_logganado$term == "treat"]
CI_lower_bor_logganado <- ATT_bor_logganado - 1.96 * SE_bor_logganado
CI_upper_bor_logganado <- ATT_bor_logganado + 1.96 * SE_bor_logganado

# Para event_chaiselogganado (averaged treatment effect)
ATT_event_chaiselogganado <- event_chaiselogganado$results$ATE[1, "Estimate"]
SE_event_chaiselogganado <- event_chaiselogganado$results$ATE[1, "SE"]
CI_lower_event_chaiselogganado <- event_chaiselogganado$results$ATE[1, "LB CI"]
CI_upper_event_chaiselogganado <- event_chaiselogganado$results$ATE[1, "UB CI"]

# Para event_intlogganado (averaged treatment effect)
ATT_event_intlogganado <- event_intlogganado$results$ATE[1, "Estimate"]
SE_event_intlogganado <- event_intlogganado$results$ATE[1, "SE"]
CI_lower_event_intlogganado <- ATT_event_intlogganado - 1.96 * SE_event_intlogganado
CI_upper_event_intlogganado <- ATT_event_intlogganado + 1.96 * SE_event_intlogganado

# Crear el dataframe para graficar
results_df <- data.frame(
  Method = c("Callaway & Sant'Anna (2021)", "Borusyak et al (2021)", "de Chaisemartin (2020)", "de Chaisemartin (2024)"),
  ATT = c(ATT_callogganado, ATT_bor_logganado, ATT_event_chaiselogganado, ATT_event_intlogganado),
  SE = c(SE_callogganado, SE_bor_logganado, SE_event_chaiselogganado, SE_event_intlogganado),
  CI_lower = c(CI_lower_callogganado, CI_lower_bor_logganado, CI_lower_event_chaiselogganado, CI_lower_event_intlogganado),
  CI_upper = c(CI_upper_callogganado, CI_upper_bor_logganado, CI_upper_event_chaiselogganado, CI_upper_event_intlogganado)
)

p<-ggplot(results_df, aes(y = Method, x = ATT, xmin = CI_lower, xmax = CI_upper)) +
  geom_pointrange() +
  geom_text(aes(label = round(ATT, 3), y = Method), vjust = -1, size = 5.5) +  # vjust controla la posición vertical
  theme_minimal() +
  geom_vline(xintercept = 0) +
  labs(
    # title = "ATT Estimates with 95% Confidence Intervals", 
    y = "Method", 
    x = "Average Treatment Effect (ATT)"
  ) +
  theme(
    axis.title.x = element_text(size = 15),      # Título del eje x
    axis.title.y = element_text(size = 15),      # Título del eje y
    axis.text.x  = element_text(size = 15),      # Etiquetas del eje x
    axis.text.y  = element_text(size = 15),      # Etiquetas del eje y
    plot.title   = element_text(size = 16, face = "bold")  # Título del gráfico
  )
ggsave(
  filename = "C:/Users/mateo/Desktop/Figuras finales/coefs_logganado.jpeg", 
  plot = p, 
  width = 12, 
  height = 7, 
  dpi = 300
)





# Para calloghomicidios
ATT_calloghomicidios <- calloghomicidios$overall.att
SE_calloghomicidios <- calloghomicidios$overall.se
CI_lower_calloghomicidios <- ATT_calloghomicidios - 1.96 * SE_calloghomicidios
CI_upper_calloghomicidios <- ATT_calloghomicidios + 1.96 * SE_calloghomicidios

# Para bor_loghomicidios
ATT_bor_loghomicidios <- bor_loghomicidios$estimate[bor_loghomicidios$term == "treat"]
SE_bor_loghomicidios <- bor_loghomicidios$std.error[bor_loghomicidios$term == "treat"]
CI_lower_bor_loghomicidios <- ATT_bor_loghomicidios - 1.96 * SE_bor_loghomicidios
CI_upper_bor_loghomicidios <- ATT_bor_loghomicidios + 1.96 * SE_bor_loghomicidios

# Para event_chaiseloghomicidios (averaged treatment effect)
ATT_event_chaiseloghomicidios <- event_chaiseloghomicidios$results$ATE[1, "Estimate"]
SE_event_chaiseloghomicidios <- event_chaiseloghomicidios$results$ATE[1, "SE"]
CI_lower_event_chaiseloghomicidios <- event_chaiseloghomicidios$results$ATE[1, "LB CI"]
CI_upper_event_chaiseloghomicidios <- event_chaiseloghomicidios$results$ATE[1, "UB CI"]

# Para event_intloghomicidios (averaged treatment effect)
ATT_event_intloghomicidios <- event_intloghomicidios$results$ATE[1, "Estimate"]
SE_event_intloghomicidios <- event_intloghomicidios$results$ATE[1, "SE"]
CI_lower_event_intloghomicidios <- ATT_event_intloghomicidios - 1.96 * SE_event_intloghomicidios
CI_upper_event_intloghomicidios <- ATT_event_intloghomicidios + 1.96 * SE_event_intloghomicidios

# Crear el dataframe para graficar
results_df <- data.frame(
  Method = c("Callaway & Sant'Anna (2021)", "Borusyak et al (2021)", "de Chaisemartin (2020)", "de Chaisemartin (2024)"),
  ATT = c(ATT_calloghomicidios, ATT_bor_loghomicidios, ATT_event_chaiseloghomicidios, ATT_event_intloghomicidios),
  SE = c(SE_calloghomicidios, SE_bor_loghomicidios, SE_event_chaiseloghomicidios, SE_event_intloghomicidios),
  CI_lower = c(CI_lower_calloghomicidios, CI_lower_bor_loghomicidios, CI_lower_event_chaiseloghomicidios, CI_lower_event_intloghomicidios),
  CI_upper = c(CI_upper_calloghomicidios, CI_upper_bor_loghomicidios, CI_upper_event_chaiseloghomicidios, CI_upper_event_intloghomicidios)
)

p<-ggplot(results_df, aes(y = Method, x = ATT, xmin = CI_lower, xmax = CI_upper)) +
  geom_pointrange() +
  geom_text(aes(label = round(ATT, 3), y = Method), vjust = -1, size = 5.5) +  # vjust controla la posición vertical
  theme_minimal() +
  geom_vline(xintercept = 0) +
  labs(
    # title = "ATT Estimates with 95% Confidence Intervals", 
    y = "Method", 
    x = "Average Treatment Effect (ATT)"
  ) +
  theme(
    axis.title.x = element_text(size = 15),      # Título del eje x
    axis.title.y = element_text(size = 15),      # Título del eje y
    axis.text.x  = element_text(size = 15),      # Etiquetas del eje x
    axis.text.y  = element_text(size = 15),      # Etiquetas del eje y
    plot.title   = element_text(size = 16, face = "bold")  # Título del gráfico
  )
ggsave(
  filename = "C:/Users/mateo/Desktop/Figuras finales/coefs_loghomicidios.jpeg", 
  plot = p, 
  width = 12, 
  height = 7, 
  dpi = 300
)


