# Figures of the application forecast for Nordic TFR up to 2050
# used for presentation 12.01.24 Finnish Pension Office

# Init ------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  global = 'src/00-global_functions.R',
  forecast_and_PIs = 'out/05-forecast_and_PIs.rds'
)
paths$output <- list(
  fig = 'out'
)

# constants specific to this analysis
cnst <- within(list(), {
  # list of countries
  cntr = c("FIN", "SWE", "NOR", "DEN")
  # list of forecast models and observed
  models = c("LC", "PPS", "ARCH", "naive", "observed")
  # plot colors
  colors = list(
    models = c(LC = '#fc8d62', PPS = '#66c2a5', ARCH = '#e78ac3',
               naive = '#8da0cb', observed = 'grey50'),
    PI.type = c(model = '#e6ab02', emp.SNO = '#e41a1c', emp.rawQ = '#984ea3', emp.publ = '#66c2a5',
                emp.SNO_pooled = '#e41a5c', emp.rawQ_pooled = '#984ef3', emp.publ_pooled = '#66c2c5')
  )
})

# global functions
source(paths$input$global)

# list containers for analysis artifacts
fig <- list()


# load data ------------------------------------------------------
forecast_and_PIs <- readRDS(paths$input$forecast_and_PIs)

# data wrangling
forecast_and_PIs$model <- replace_na(forecast_and_PIs$model, "observed")

L95 <- forecast_and_PIs %>% filter(model != "observed") %>%
             select(Country, Year, model, predicted, L95.emp.SNO, L95.emp.rawQ, L95.model, L95.emp.publ,
                    L95.emp.SNO_pooled, L95.emp.rawQ_pooled, L95.emp.publ_pooled) %>%
             pivot_longer(cols = c(L95.emp.SNO, L95.emp.rawQ, L95.model, L95.emp.publ,
                                   L95.emp.SNO_pooled, L95.emp.rawQ_pooled, L95.emp.publ_pooled),
                          names_to = "PI.type",
                          names_prefix = "L95.",
                          values_to = "L95")

L80 <- forecast_and_PIs %>% filter(model != "observed") %>%
  select(Country, Year, model, predicted, L80.emp.SNO, L80.emp.rawQ, L80.model, L80.emp.publ,
         L80.emp.SNO_pooled, L80.emp.rawQ_pooled, L80.emp.publ_pooled) %>%
  pivot_longer(cols = c(L80.emp.SNO, L80.emp.rawQ, L80.model, L80.emp.publ,
                        L80.emp.SNO_pooled, L80.emp.rawQ_pooled, L80.emp.publ_pooled),
               names_to = "PI.type",
               names_prefix = "L80.",
               values_to = "L80")

U95 <- forecast_and_PIs %>% filter(model != "observed") %>%
  select(Country, Year, model, predicted, U95.emp.SNO, U95.emp.rawQ, U95.model, U95.emp.publ,
         U95.emp.SNO_pooled, U95.emp.rawQ_pooled, U95.emp.publ_pooled) %>%
  pivot_longer(cols = c(U95.emp.SNO, U95.emp.rawQ, U95.model, U95.emp.publ,
                        U95.emp.SNO_pooled, U95.emp.rawQ_pooled, U95.emp.publ_pooled),
               names_to = "PI.type",
               names_prefix = "U95.",
               values_to = "U95")

U80 <- forecast_and_PIs %>% filter(model != "observed") %>%
  select(Country, Year, model, predicted, U80.emp.SNO, U80.emp.rawQ, U80.model, U80.emp.publ,
         U80.emp.SNO_pooled, U80.emp.rawQ_pooled, U80.emp.publ_pooled) %>%
  pivot_longer(cols = c(U80.emp.SNO, U80.emp.rawQ, U80.model, U80.emp.publ,
                        U80.emp.SNO_pooled, U80.emp.rawQ_pooled, U80.emp.publ_pooled),
               names_to = "PI.type",
               names_prefix = "U80.",
               values_to = "U80")

forecast <- forecast_and_PIs %>% select(Country, Year, observed, model, predicted)

plot_data <- forecast %>% left_join(L95) %>% left_join(L80) %>% left_join(U80) %>% left_join(U95)


# Figures --------------------------------------------------------

# all forecasts

plot_FIN <- forecast_and_PIs %>%
  filter(Country == "FIN" & model %in% c("observed", "naive", "PPS", "LC")) %>%
  ggplot() +
  aes(x = Year, y = predicted, color = model) +
  geom_line(linewidth = 1) +
  geom_line(aes(x = Year, y = observed),
            linewidth = 1) +
  figspec$MyGGplotTheme(axis = 'xy') +
  labs(y = 'TFR', x = 'Year',
       title = "Finland") +
  scale_color_manual(
    values = cnst$colors$models
  ) +
  geom_label_repel(data = filter(forecast_and_PIs, Year == 2050 & Country == "FIN" & model %in% c("observed", "naive", "PPS", "LC")),
                  aes(label = round(predicted, 2)),
                  nudge_x = 1,
                  size = 4)

plot_DEN <- forecast_and_PIs %>%
  filter(Country == "DEN") %>%
  ggplot() +
  aes(x = Year, y = predicted, color = model) +
  geom_line(linewidth = 1) +
  geom_line(aes(x = Year, y = observed),
            linewidth = 1) +
  figspec$MyGGplotTheme(axis = 'xy') +
  labs(y = 'TFR', x = 'Year',
       title = "Denmark") +
  scale_color_manual(
    values = cnst$colors$models
  ) +
  geom_label_repel(data = filter(forecast_and_PIs, Year == 2050 & Country == "DEN"),
                   aes(label = round(predicted, 2)),
                   nudge_x = 1,
                   size = 4)


plot_SWE <- forecast_and_PIs %>%
  filter(Country == "SWE") %>%
  ggplot() +
  aes(x = Year, y = predicted, color = model) +
  geom_line(linewidth = 1) +
  geom_line(aes(x = Year, y = observed),
            linewidth = 1) +
  figspec$MyGGplotTheme(axis = 'xy') +
  labs(y = 'TFR', x = 'Year',
       title = "Sweden") +
  scale_color_manual(
    values = cnst$colors$models
  ) +
  geom_label_repel(data = filter(forecast_and_PIs, Year == 2050 & Country == "SWE"),
                   aes(label = round(predicted, 2)),
                   nudge_x = 1,
                   size = 4)


plot_NOR <- forecast_and_PIs %>%
  filter(Country == "NOR") %>%
  ggplot() +
  aes(x = Year, y = predicted, color = model) +
  geom_line(linewidth = 1) +
  geom_line(aes(x = Year, y = observed),
            linewidth = 1) +
  figspec$MyGGplotTheme(axis = 'xy') +
  labs(y = 'TFR', x = 'Year',
       title = "Norway") +
  scale_color_manual(
    values = cnst$colors$models
  ) +
  geom_label_repel(data = filter(forecast_and_PIs, Year == 2050 & Country == "NOR"),
                   aes(label = round(predicted, 2)),
                   nudge_x = 1,
                   size = 4)


# one forecast with different prediction intervals
plot_PPS_FIN <- plot_data %>%
    filter(Country == "FIN" & model %in% c("PPS", "observed")) %>%
    ggplot() +
    aes(x = Year, y = predicted) +
    geom_line(aes(x = Year, y = observed),
              linewidth = 1, color = 'grey50') +
    geom_ribbon(
      aes(x = Year, ymin = L95, ymax = U95, color = PI.type),
      fill = 'grey90', alpha=0.5
    ) +
    figspec$MyGGplotTheme(axis = 'xy') +
    geom_line(linewidth = 1, color ='#66c2a5') +
    labs(y = 'TFR', x = 'Year',
         title = "Finland, PPS forecast") +
    scale_color_manual(
      values = cnst$colors$PI.type
    )

plot_LC_FIN <- plot_data %>%
  filter(Country == "FIN" & model %in% c("LC", "observed")) %>%
  ggplot() +
  aes(x = Year, y = predicted) +
  geom_line(aes(x = Year, y = observed),
            linewidth = 1, color = 'grey50') +
  geom_ribbon(
    aes(x = Year, ymin = L95, ymax = U95, color = PI.type),
    fill = 'grey90', alpha=0.5
  ) +
  figspec$MyGGplotTheme(axis = 'xy') +
  geom_line(linewidth = 1, color ='#fc8d62') +
  labs(y = 'TFR', x = 'Year',
       title = "Finland, LC forecast") +
  scale_color_manual(
    values = cnst$colors$PI.type
  )

plot_PPS_SWE <- plot_data %>%
  filter(Country == "SWE" & model %in% c("PPS", "observed")) %>%
  ggplot() +
  aes(x = Year, y = predicted) +
  geom_line(aes(x = Year, y = observed),
            linewidth = 1, color = 'grey50') +
  geom_ribbon(
    aes(x = Year, ymin = L95, ymax = U95, color = PI.type),
    fill = 'grey90', alpha=0.5
  ) +
  figspec$MyGGplotTheme(axis = 'xy') +
  geom_line(linewidth = 1, color ='#66c2a5') +
  labs(y = 'TFR', x = 'Year',
       title = "Sweden, PPS forecast") +
  scale_color_manual(
    values = cnst$colors$PI.type
  )

plot_LC_SWE <- plot_data %>%
  filter(Country == "SWE" & model %in% c("LC", "observed")) %>%
  ggplot() +
  aes(x = Year, y = predicted) +
  geom_line(aes(x = Year, y = observed),
            linewidth = 1, color = 'grey50') +
  geom_ribbon(
    aes(x = Year, ymin = L95, ymax = U95, color = PI.type),
    fill = 'grey90', alpha=0.5
  ) +
  figspec$MyGGplotTheme(axis = 'xy') +
  geom_line(linewidth = 1, color ='#fc8d62') +
  labs(y = 'TFR', x = 'Year',
       title = "Sweden, LC forecast") +
  scale_color_manual(
    values = cnst$colors$PI.type
  )


plot_PPS_DEN <- plot_data %>%
  filter(Country == "DEN" & model %in% c("PPS", "observed")) %>%
  ggplot() +
  aes(x = Year, y = predicted) +
  geom_line(aes(x = Year, y = observed),
            linewidth = 1, color = 'grey50') +
  geom_ribbon(
    aes(x = Year, ymin = L95, ymax = U95, color = PI.type),
    fill = 'grey90', alpha=0.5
  ) +
  figspec$MyGGplotTheme(axis = 'xy') +
  geom_line(linewidth = 1, color ='#66c2a5') +
  labs(y = 'TFR', x = 'Year',
       title = "Denmark, PPS forecast") +
  scale_color_manual(
    values = cnst$colors$PI.type
  )

plot_LC_DEN <- plot_data %>%
  filter(Country == "DEN" & model %in% c("LC", "observed")) %>%
  ggplot() +
  aes(x = Year, y = predicted) +
  geom_line(aes(x = Year, y = observed),
            linewidth = 1, color = 'grey50') +
  geom_ribbon(
    aes(x = Year, ymin = L95, ymax = U95, color = PI.type),
    fill = 'grey90', alpha=0.5
  ) +
  figspec$MyGGplotTheme(axis = 'xy') +
  geom_line(linewidth = 1, color ='#fc8d62') +
  labs(y = 'TFR', x = 'Year',
       title = "Denmark, LC forecast") +
  scale_color_manual(
    values = cnst$colors$PI.type
  )



plot_PPS_NOR <- plot_data %>%
  filter(Country == "NOR" & model %in% c("PPS", "observed")) %>%
  ggplot() +
  aes(x = Year, y = predicted) +
  geom_line(aes(x = Year, y = observed),
            linewidth = 1, color = 'grey50') +
  geom_ribbon(
    aes(x = Year, ymin = L95, ymax = U95, color = PI.type),
    fill = 'grey90', alpha=0.5
  ) +
  figspec$MyGGplotTheme(axis = 'xy') +
  geom_line(linewidth = 1, color ='#66c2a5') +
  labs(y = 'TFR', x = 'Year',
       title = "Norway, PPS forecast") +
  scale_color_manual(
    values = cnst$colors$PI.type
  )

plot_LC_NOR <- plot_data %>%
  filter(Country == "NOR" & model %in% c("LC", "observed")) %>%
  ggplot() +
  aes(x = Year, y = predicted) +
  geom_line(aes(x = Year, y = observed),
            linewidth = 1, color = 'grey50') +
  geom_ribbon(
    aes(x = Year, ymin = L95, ymax = U95, color = PI.type),
    fill = 'grey90', alpha=0.5
  ) +
  figspec$MyGGplotTheme(axis = 'xy') +
  geom_line(linewidth = 1, color ='#fc8d62') +
  labs(y = 'TFR', x = 'Year',
       title = "Norway, LC forecast") +
  scale_color_manual(
    values = cnst$colors$PI.type
  )



# one model all countries -------------------------------

# PPS, all countries
forecast_and_PIs %>%
  filter(model %in% c("observed", "naive", "PPS")) %>%
  ggplot() +
  aes(x = Year, y = predicted, color = Country) +
  geom_line(data = filter(forecast_and_PIs, model == "PPS"),
            linewidth = 1,
            linetype = "longdash") +
  geom_line(data = filter(forecast_and_PIs, model == "naive"),
            linetype = "dashed", alpha = 0.5) +
  geom_line(aes(x = Year, y = observed),
            linewidth = 1) +
  figspec$MyGGplotTheme(axis = 'xy') +
  labs(y = 'TFR', x = 'Year',
       title = "PPS Forecast") +
  geom_label_repel(data = filter(forecast_and_PIs, Year == 2050 &
                                 model %in% c("observed", "PPS")),
                   aes(label = round(predicted, 2)),
                   nudge_x = 1,
                   size = 4)

# LC, all countries
forecast_and_PIs %>%
  filter(model %in% c("observed", "naive", "LC")) %>%
  ggplot() +
  aes(x = Year, y = predicted, color = Country) +
  geom_line(data = filter(forecast_and_PIs, model == "LC"),
            linewidth = 1,
            linetype = "longdash") +
  geom_line(data = filter(forecast_and_PIs, model == "naive"),
            linetype = "dashed", alpha = 0.5) +
  geom_line(aes(x = Year, y = observed),
            linewidth = 1) +
  figspec$MyGGplotTheme(axis = 'xy') +
  labs(y = 'TFR', x = 'Year',
       title = "LC Forecast") +
  geom_label_repel(data = filter(forecast_and_PIs, Year == 2050 &
                                   model %in% c("observed", "LC")),
                   aes(label = round(predicted, 2)),
                   nudge_x = 1,
                   size = 4)


# single country, PPS, naive and PIs from model and empirical from raw quantiles
for (i in 1:length(cnst$cntr)) {
  plot <- plot_data %>%
    filter(Country == "cnst$cntr[i]" &
           model %in% c("PPS", "observed") &
           PI.type %in% c("emp.rawQ", "model", NA, "emp.SNO", "emp.publ")) %>%
    ggplot() +
    aes(x = Year, y = predicted) +
    geom_line(aes(x = Year, y = observed),
              linewidth = 1, color = 'grey50') +
    geom_ribbon(
      aes(x = Year, ymin = L95, ymax = U95, color = PI.type),
      fill = 'grey90', alpha=0.5
    ) +
    figspec$MyGGplotTheme(axis = 'xy') +
    geom_line(linewidth = 1, linetype = "longdash", color = 'grey50') +
    labs(y = 'TFR', x = 'Year', color = "PI Type", 
         title = paste(cnst$cntr[i], ", PPS forecast and 95% prediction intervals", sep = "")) +
    scale_color_manual(
      values = cnst$colors$PI.type
    )  +
    geom_label_repel(data = filter(forecast_and_PIs, Year == 2050 &
                                     model %in% c("observed", "PPS") &
                                     Country == cnst$cntr[i]),
                     aes(label = round(predicted, 2)),
                     nudge_x = 1,
                     size = 4)
  print(plot)
}

for (i in 1:length(cnst$cntr)) {
  plot <- plot_data %>%
    filter(Country == cnst$cntr[i] &
             model %in% c("LC", "observed") &
             PI.type %in% c("emp.rawQ", "model", NA)) %>%
    ggplot() +
    aes(x = Year, y = predicted) +
    geom_line(aes(x = Year, y = observed),
              linewidth = 1, color = 'grey50') +
    geom_ribbon(
      aes(x = Year, ymin = L95, ymax = U95, color = PI.type),
      fill = 'grey90', alpha=0.5
    ) +
    figspec$MyGGplotTheme(axis = 'xy') +
    geom_line(linewidth = 1, linetype = "longdash", color = 'grey50') +
    labs(y = 'TFR', x = 'Year', color = "PI Type", 
         title = paste(cnst$cntr[i], ", LC forecast and 95% prediction intervals", sep = "")) +
    scale_color_manual(
      values = cnst$colors$PI.type
    )  +
    geom_label_repel(data = filter(forecast_and_PIs, Year == 2050 &
                                     model %in% c("observed", "LC") &
                                     Country == cnst$cntr[i]),
                     aes(label = round(predicted, 2)),
                     nudge_x = 1,
                     size = 4)
  print(plot)
}


############### DGD ###################

plot_data %>%
  filter(Country == "FIN" &
           model %in% c("PPS", "observed")) %>%
  ggplot() +
  aes(x = Year, y = predicted) +
  geom_line(aes(x = Year, y = observed),
            linewidth = 1, color = 'grey50') +
  geom_ribbon(
    aes(x = Year, ymin = L95, ymax = U95, color = PI.type),
    fill = 'grey90', alpha=0.3, size = 1
  ) +
  figspec$MyGGplotTheme(axis = 'xy') +
  geom_line(linewidth = 1, linetype = "longdash", color = 'grey50') +
  labs(y = 'TFR', x = 'Year', color = "PI Type", 
       title = paste("Finland", ", PPS forecast and 95% prediction intervals", sep = "")) +
  scale_color_manual(
    values = cnst$colors$PI.type
  )  +
  geom_label_repel(data = filter(forecast_and_PIs, Year == 2050 &
                                   model %in% c("observed", "PPS") &
                                   Country == "FIN"),
                   aes(label = round(predicted, 2)),
                   nudge_x = 1,
                   size = 4)

plot_data %>%
  filter(Country == "FIN" &
           model %in% c("LC", "observed")) %>%
  ggplot() +
  aes(x = Year, y = predicted) +
  geom_line(aes(x = Year, y = observed),
            linewidth = 1, color = 'grey50') +
  geom_ribbon(
    aes(x = Year, ymin = L95, ymax = U95, color = PI.type),
    fill = 'grey90', alpha=0.3, size = 1
  ) +
  figspec$MyGGplotTheme(axis = 'xy') +
  geom_line(linewidth = 1, linetype = "longdash", color = 'grey50') +
  labs(y = 'TFR', x = 'Year', color = "PI Type", 
       title = paste("Finland", ", LC forecast and 95% prediction intervals", sep = "")) +
  scale_color_manual(
    values = cnst$colors$PI.type
  )  +
  geom_label_repel(data = filter(forecast_and_PIs, Year == 2050 &
                                   model %in% c("observed", "LC") &
                                   Country == "FIN"),
                   aes(label = round(predicted, 2)),
                   nudge_x = 1,
                   size = 4)


############### Pension Office ###################

forecast_and_PIs %>%
  filter(Country == "FIN" & model %in% c("observed", "naive", "PPS", "LC")) %>%
  ggplot() +
  aes(x = Year, y = predicted, color = model) +
  geom_line(linewidth = 1, linetype = "longdash") +
  geom_line(aes(x = Year, y = observed),
            linewidth = 1) +
  figspec$MyGGplotTheme(axis = 'xy') +
  labs(y = 'TFR', x = 'Year',
       title = "Finland") +
  scale_color_manual(
    values = cnst$colors$models
  ) +
  geom_label_repel(data = filter(forecast_and_PIs, Year == 2050 & Country == "FIN" & model %in% c("observed", "naive", "PPS", "LC")),
                   aes(label = round(predicted, 2)),
                   nudge_x = 1,
                   size = 4)

plot_data %>%
  filter(Country == "FIN" &
           model %in% c("PPS", "observed") &
           PI.type %in% c(NA, "observed", "emp.publ_pooled", "emp.rawQ_pooled", "emp.SNO_pooled", "model")) %>%
  ggplot() +
  aes(x = Year, y = predicted) +
  geom_line(aes(x = Year, y = observed),
            linewidth = 1, color = 'grey50') +
  geom_ribbon(
    aes(x = Year, ymin = L95, ymax = U95, color = PI.type),
    fill = 'grey90', alpha=0.3, size = 1
  ) +
  figspec$MyGGplotTheme(axis = 'xy') +
  geom_line(linewidth = 1, linetype = "longdash", color = 'grey50') +
  labs(y = 'TFR', x = 'Year', color = "PI Type", 
       title = paste("Finland", ", PPS forecast and 95% prediction intervals", sep = "")) +
  scale_color_manual(
    values = cnst$colors$PI.type
  )  +
  geom_label_repel(data = filter(forecast_and_PIs, Year == 2050 &
                                   model %in% c("observed", "PPS") &
                                   Country == "FIN"),
                   aes(label = round(predicted, 2)),
                   nudge_x = 1,
                   size = 4)

plot_data %>%
  filter(Country == "FIN" &
           model %in% c("PPS", "observed") &
           PI.type %in% c(NA, "observed", "emp.SNO_pooled", "model")) %>%
  ggplot() +
  aes(x = Year, y = predicted) +
  geom_line(aes(x = Year, y = observed),
            linewidth = 1, color = 'grey50') +
  geom_ribbon(
    aes(x = Year, ymin = L95, ymax = U95, color = PI.type),
    fill = 'grey90', alpha=0.3, size = 1
  ) +
  figspec$MyGGplotTheme(axis = 'xy') +
  geom_line(linewidth = 1, linetype = "longdash", color = 'grey50') +
  labs(y = 'TFR', x = 'Year', color = "PI Type", 
       title = paste("Finland", ", PPS forecast and 95% prediction intervals", sep = "")) +
  scale_color_manual(
    values = cnst$colors$PI.type
  )  +
  geom_label_repel(data = filter(forecast_and_PIs, Year == 2050 &
                                   model %in% c("observed", "PPS") &
                                   Country == "FIN"),
                   aes(label = round(predicted, 2)),
                   nudge_x = 1,
                   size = 4)

plot_data %>%
  filter(Country == "FIN" &
           model %in% c("PPS", "observed") &
           PI.type %in% c(NA, "observed", "emp.publ", "emp.rawQ", "emp.SNO", "model")) %>%
  ggplot() +
  aes(x = Year, y = predicted) +
  geom_line(aes(x = Year, y = observed),
            linewidth = 1, color = 'grey50') +
  geom_ribbon(
    aes(x = Year, ymin = L95, ymax = U95, color = PI.type),
    fill = 'grey90', alpha=0.3, size = 1
  ) +
  figspec$MyGGplotTheme(axis = 'xy') +
  geom_line(linewidth = 1, linetype = "longdash", color = 'grey50') +
  labs(y = 'TFR', x = 'Year', color = "PI Type", 
       title = paste("Finland", ", PPS forecast and 95% prediction intervals", sep = "")) +
  scale_color_manual(
    values = cnst$colors$PI.type
  )  +
  geom_label_repel(data = filter(forecast_and_PIs, Year == 2050 &
                                   model %in% c("observed", "PPS") &
                                   Country == "FIN"),
                   aes(label = round(predicted, 2)),
                   nudge_x = 1,
                   size = 4)

plot_data %>%
  filter(Country == "FIN" &
           model %in% c("LC", "observed") &
           PI.type %in% c(NA, "observed", "emp.publ_pooled", "emp.rawQ_pooled", "emp.SNO_pooled", "model")) %>%
  ggplot() +
  aes(x = Year, y = predicted) +
  geom_line(aes(x = Year, y = observed),
            linewidth = 1, color = 'grey50') +
  geom_ribbon(
    aes(x = Year, ymin = L95, ymax = U95, color = PI.type),
    fill = 'grey90', alpha=0.3, size = 1
  ) +
  figspec$MyGGplotTheme(axis = 'xy') +
  geom_line(linewidth = 1, linetype = "longdash", color = 'grey50') +
  labs(y = 'TFR', x = 'Year', color = "PI Type", 
       title = paste("Finland", ", LC forecast and 95% prediction intervals", sep = "")) +
  scale_color_manual(
    values = cnst$colors$PI.type
  )  +
  geom_label_repel(data = filter(forecast_and_PIs, Year == 2050 &
                                   model %in% c("observed", "LC") &
                                   Country == "FIN"),
                   aes(label = round(predicted, 2)),
                   nudge_x = 1,
                   size = 4)

plot_data %>%
  filter(Country == "FIN" &
           model %in% c("LC", "observed") &
           PI.type %in% c(NA, "observed", "emp.publ", "emp.rawQ", "emp.SNO", "model")) %>%
  ggplot() +
  aes(x = Year, y = predicted) +
  geom_line(aes(x = Year, y = observed),
            linewidth = 1, color = 'grey50') +
  geom_ribbon(
    aes(x = Year, ymin = L95, ymax = U95, color = PI.type),
    fill = 'grey90', alpha=0.3, size = 1
  ) +
  figspec$MyGGplotTheme(axis = 'xy') +
  geom_line(linewidth = 1, linetype = "longdash", color = 'grey50') +
  labs(y = 'TFR', x = 'Year', color = "PI Type", 
       title = paste("Finland", ", LC forecast and 95% prediction intervals", sep = "")) +
  scale_color_manual(
    values = cnst$colors$PI.type
  )  +
  geom_label_repel(data = filter(forecast_and_PIs, Year == 2050 &
                                   model %in% c("observed", "LC") &
                                   Country == "FIN"),
                   aes(label = round(predicted, 2)),
                   nudge_x = 1,
                   size = 4)
