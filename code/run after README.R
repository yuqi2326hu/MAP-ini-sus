### used packages
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(grid)
library(deSolve)
library(data.table)
library(asreml)
library(tibble)
library(stringr)
library(fitdistrplus)
library(zoo)
library(mvtnorm)
library(mhsmm)

### upload dataset
sirebv <- read.table("TrBVSires.txt",
                     header = FALSE, sep = "", stringsAsFactors = FALSE) %>%
  rename(indi = V1,
         true_bv = V2)
sire <- sirebv$indi
buyin_pop <- read.table("AllBuyIn.txt",
                        header = TRUE, sep = "", stringsAsFactors = FALSE) %>% pull(1)
true_state <- read.table("pheno_r1_wkFullInfo 3.txt",
                         header = TRUE, sep = "", stringsAsFactors = FALSE) %>%
  mutate(test_time = if_else(test_time > 260, 260, test_time)) %>%
  mutate(birthweek = test_time - age) %>%
  mutate(
    test_time = round(test_time, 2),
    age       = round(age, 2),
    birthweek = round(birthweek, 2)
  ) %>%
  distinct(indi, test_time, .keep_all = TRUE) %>%
  filter(indi != "f12_25616") 

annually_test <- read.table("pheno_r1_an 8.txt",
                            header = TRUE, sep = "", stringsAsFactors = FALSE)
##### check the repeat reccord
annually_test %>%
  count(indi, test_time) %>%
  filter(n > 1)      ### no state change, so only keep the first record
annually_test <- annually_test %>%
  mutate(test_time = if_else(test_time > 260, 260, test_time)) %>%
  mutate(birthweek = test_time - age) %>%
  mutate(
    test_time = round(test_time, 2),
    age       = round(age, 2),
    birthweek = round(birthweek, 2)
  ) %>%
  distinct(indi, test_time, .keep_all = TRUE) %>%
  filter(indi != "f12_25616")

weekly_test <- read.table("pheno_r1_wk 8.txt",
                          header = TRUE, sep = "", stringsAsFactors = FALSE)
weekly_test <- weekly_test %>%
  mutate(test_time = if_else(test_time > 260, 260, test_time)) %>%
  mutate(birthweek = test_time - age) %>%
  mutate(
    test_time = round(test_time, 2),
    age       = round(age, 2),
    birthweek = round(birthweek, 2)
  ) %>%
  distinct(indi, test_time, .keep_all = TRUE) %>%
  filter(indi != "f12_25616")

seasonal_test <- read.table("pheno_r1_se 8.txt",
                            header = TRUE, sep = "", stringsAsFactors = FALSE)
seasonal_test %>%
  count(indi, test_time) %>%
  filter(n > 1)      ### no state change, so only keep the first record
seasonal_test <- seasonal_test %>%
  mutate(test_time = if_else(test_time > 260, 260, test_time)) %>%
  mutate(birthweek = test_time - age) %>%
  mutate(
    test_time = round(test_time, 2),
    age       = round(age, 2),
    birthweek = round(birthweek, 2)
  ) %>%
  distinct(indi, test_time, .keep_all = TRUE) %>%
  filter(indi != "f12_25616")

monthly_test <- read.table("pheno_r1_mo 8.txt",
                            header = TRUE, sep = "", stringsAsFactors = FALSE)
monthly_test %>%
  count(indi, test_time) %>%
  filter(n > 1)      ### no state change, so only keep the first record
monthly_test <- monthly_test %>%
  mutate(test_time = if_else(test_time > 260, 260, test_time)) %>%
  mutate(birthweek = test_time - age) %>%
  mutate(
    test_time = round(test_time, 2),
    age       = round(age, 2),
    birthweek = round(birthweek, 2)
  ) %>%
  distinct(indi, test_time, .keep_all = TRUE) %>%
  filter(indi != "f12_25616")


pedigree_info <- read.table("ped_r1 8.txt",
                            header = TRUE, sep = "", stringsAsFactors = FALSE)
all_animals <- pedigree_info %>%
  dplyr::select(indi, sireid, damid) %>%
  unlist(use.names = FALSE) %>%
  unique()
str(pedigree_info)
pedigree_info <- pedigree_info %>%
  mutate(sireid = as.character(sireid))
str(pedigree_info)
Ainv_full <- ainverse(pedigree_info)

## parameters & functions 
sh <- 2.1
de <- 0.15
g <- 0.0629
tol <- 1e-4
max_iter <- 100
vmax <- function(...) pmax(..., na.rm = TRUE)        ####ignore NA
vmin <- function(...) pmin(..., na.rm = TRUE)

## visualize base pattern
state_cols <- c(
  "Itr" = "#D91E1E",  
  "Il"  = "#1E5AD9",  
  "Ih"  = "#000000"
)

state_cols_l <- c(
  "Itr" = "#F7C6C6",  
  "Il"  = "#C6D9FF",  
  "Ih"  = "#CCCCCC" 
)
base_theme <- theme_classic(base_size = 20) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 20),
    axis.line = element_line(color = "grey80", size = 0.5),
    axis.ticks = element_line(color = "grey80", size = 0.4),
    legend.text = element_text(size = 20),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width  = unit(0.5, "cm"),
    axis.title = element_text(size = 20),
    axis.text  = element_text(size = 16, color = "grey70"),
    panel.spacing = unit(1, "lines"),
    plot.margin   = margin(10, 20, 10, 20),
    plot.title    = element_text(face = "bold", hjust = 0.5, size = 20),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background  = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )

####saving image
for (i in 11:20) {
  farm_name <- paste0("farm_", i)
  
  ### true
  ggsave(
    filename = paste0("true_exposure_plot_farm", i, ".png"),
    plot     = true_run_for_10_farms[[farm_name]][["exposure_plot"]],
    width    = 10.5,
    height   = 6,
    dpi      = 300,
    bg       = "transparent"
  )
  
  ggsave(
    filename = paste0("true_E_plot_farm", i, ".png"),
    plot     = true_run_for_10_farms[[farm_name]][["E_plot"]],
    width    = 10,
    height   = 6,
    dpi      = 300,
    bg       = "transparent"
  )
  
  ggsave(
    filename = paste0("true_n_I_plot_farm", i, ".png"),
    plot     = true_run_for_10_farms[[farm_name]][["n_I_plot"]],
    width    = 10,
    height   = 6,
    dpi      = 300,
    bg       = "transparent"
  )
  
  ###annually
  ggsave(
    filename = paste0("an_exposure_plot_farm", i, ".png"),
    plot     = an_run_for_10_farms[[farm_name]][["exposure_plot"]],
    width    = 10.5,
    height   = 6,
    dpi      = 300,
    bg       = "transparent"
  )
  
  ggsave(
    filename = paste0("an_E_plot_farm", i, ".png"),
    plot     = an_run_for_10_farms[[farm_name]][["E_plot"]],
    width    = 10,
    height   = 6,
    dpi      = 300,
    bg       = "transparent"
  )
  
  ggsave(
    filename = paste0("an_n_I_plot_farm", i, ".png"),
    plot     = an_run_for_10_farms[[farm_name]][["n_I_plot"]],
    width    = 10,
    height   = 6,
    dpi      = 300,
    bg       = "transparent"
  )
  
  ##weekly
  ggsave(
    filename = paste0("wk_exposure_plot_farm", i, ".png"),
    plot     = wk_run_for_10_farms[[farm_name]][["exposure_plot"]],
    width    = 10.5,
    height   = 6,
    dpi      = 300,
    bg       = "transparent"
  )
  
  ggsave(
    filename = paste0("wk_E_plot_farm", i, ".png"),
    plot     = wk_run_for_10_farms[[farm_name]][["E_plot"]],
    width    = 10,
    height   = 6,
    dpi      = 300,
    bg       = "transparent"
  )
  
  ggsave(
    filename = paste0("wk_n_I_plot_farm", i, ".png"),
    plot     = wk_run_for_10_farms[[farm_name]][["n_I_plot"]],
    width    = 10,
    height   = 6,
    dpi      = 300,
    bg       = "transparent"
  )
  
  ### seasonal
  ggsave(
    filename = paste0("se_exposure_plot_farm", i, ".png"),
    plot     = se_run_for_10_farms[[farm_name]][["exposure_plot"]],
    width    = 10.5,
    height   = 6,
    dpi      = 300,
    bg       = "transparent"
  )
  
  ggsave(
    filename = paste0("se_E_plot_farm", i, ".png"),
    plot     = se_run_for_10_farms[[farm_name]][["E_plot"]],
    width    = 10,
    height   = 6,
    dpi      = 300,
    bg       = "transparent"
  )
  
  ggsave(
    filename = paste0("se_n_I_plot_farm", i, ".png"),
    plot     = se_run_for_10_farms[[farm_name]][["n_I_plot"]],
    width    = 10,
    height   = 6,
    dpi      = 300,
    bg       = "transparent"
  )
  
  ### monthly
  ggsave(
    filename = paste0("mo_exposure_plot_farm", i, ".png"),
    plot     = mo_run_for_10_farms[[farm_name]][["exposure_plot"]],
    width    = 10.5,
    height   = 6,
    dpi      = 300,
    bg       = "transparent"
  )
  
  ggsave(
    filename = paste0("mo_E_plot_farm", i, ".png"),
    plot     = mo_run_for_10_farms[[farm_name]][["E_plot"]],
    width    = 10,
    height   = 6,
    dpi      = 300,
    bg       = "transparent"
  )
  
  ggsave(
    filename = paste0("mo_n_I_plot_farm", i, ".png"),
    plot     = mo_run_for_10_farms[[farm_name]][["n_I_plot"]],
    width    = 10,
    height   = 6,
    dpi      = 300,
    bg       = "transparent"
  )
}




