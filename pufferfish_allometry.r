library(tidyverse)
library(brms)
library(khroma)

data_sl_bw <- 
  readr::read_csv("pufferfish_sl_bw.csv") %>% 
  dplyr::mutate(
    capture_date = lubridate::dmy(capture_date)
  ) %>% 
  dplyr::mutate(across(where(is_character),as_factor))


summary(lm(log10_BW ~ log10_SL, data = data_sl_bw))


fit_sl_bw_01 <- 
  brms::brm(
    formula = log10(bw) ~ log10(sl) + place + species, 
    data = data_sl_bw,
    family = gaussian(),
    chains = 4,
    iter = 2000,
    cores = 4,
    backend = "cmdstanr"
  )
summary(fit_sl_bw_01)

fit_sl_bw_02 <- 
  brms::brm(
    formula = log10_BW ~ log10_SL + place + species + (1 + log10_SL|place + species), 
    data = data_sl_bw,
    family = gaussian(),
    chains = 4,
    iter = 2000,
    cores = 4,
    backend = "cmdstanr"
  )

summary(fit_sl_bw_01)
summary(fit_sl_bw_02)

loo(fit_sl_bw_01)
loo(fit_sl_bw_02)


plot_sl_bw <- 
  data_sl_bw %>% 
  ggplot2::ggplot(
    aes(
      x = log10_SL,
      y = log10_BW,
      color = species
    )
  ) +
  geom_point() +
  xlim(0,3) +
  ylim(0,3) +
  labs(
    x = "Standard length (log Trans. Unit: CM)",
    y = "Body weight (log Trans. Unit: g)"
  ) +
  scale_color_okabeito()+
  theme_classic() +
  theme(
    legend.position = "bottom"
  )
plot_sl_bw
ggsave(
  "plot_sl_bw.pdf",
  plot = plot_sl_bw,
  height = 100,
  width = 100,
  units = "mm"
)