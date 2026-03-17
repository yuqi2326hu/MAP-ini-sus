### shedding pattern
true_pattern <- true_state %>%
  mutate(pattern = case_when(
    state == "S" ~ 0L,
    state %in% c("Itrap", "Itrcp") ~ 1L,
    state %in% c("L1cp", "L2cp", "L1ap", "L2ap") ~ 2L,
    state %in% c("Ilap", "Ilcp") ~ 3L,
    state %in% c("Ihap", "Ihcp") ~ 4L,
    state %in% "nbItr" ~ 5L
  ))
anyNA(true_pattern)
###population distribution
true_pop <- true_pattern %>%
  group_by(indi) %>%
  arrange(test_time, age) %>%
  summarise(farm = unique(farm),
            start_time = min(test_time),
            end_time = max(test_time),
            inf_time   = if (any(pattern != 0)) min(test_time[pattern != 0]) else NA,
            .groups = "drop"
            )
time_grid <- seq(
  floor(min(true_pop$start_time)),
  ceiling(max(true_pop$end_time)),
  by = 1
)
count_df <- expand.grid(
  time = time_grid,
  farm = unique(true_pop$farm)
) %>%
  rowwise() %>%
  mutate(
    population = sum(true_pop$farm == farm &
                       true_pop$start_time <= time &
                       true_pop$end_time >= time),
    
    infected   = sum(true_pop$farm == farm &
                       true_pop$start_time <= time &
                       true_pop$end_time >= time &
                       !is.na(true_pop$inf_time) &
                       true_pop$inf_time <= time),
    
    prevalence = ifelse(population > 0, infected / population, NA)
  ) %>%
  ungroup()

plot_df <- count_df %>%
  pivot_longer(
    cols = c(population, infected),
    names_to = "type",
    values_to = "count"
  ) %>%
  filter(time <= 259)
ggplot(plot_df, aes(x = time, y = prevalence, color = factor(farm))) +
  geom_line(linewidth = 1) +
  labs(
    x = "Time",
    y = "Prevalence",
    color = "Farm",
    title = "Prevalence of infected animals over time by farm"
  ) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 260)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    axis.title = element_text(size = 20),
    axis.text  = element_text(size = 18),
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 14)
  )








infected_by_mom_true <- true_pattern %>%
  filter(pattern == 5) %>%
  pull(indi) %>%
  unique()
born_pop_true <- true_pattern %>%
  group_by(indi) %>%
  filter(!(indi %in% buyin_pop)) %>%
  filter(first(birthweek) >= 0) %>%
  pull(indi) %>% unique()
ini_pop_true <- true_pattern %>%
  group_by(indi) %>%
  filter(!(indi %in% buyin_pop)) %>%
  filter(first(birthweek) < 0) %>%
  pull(indi) %>% unique()
all_indi_true <- true_pattern %>%
  distinct(indi) %>%
  pull(indi)

##interval divided
true_interval <- true_pattern %>%
  group_by(indi) %>%
  arrange(test_time, age, .by_group = TRUE) %>%
  mutate(Lmid = (lag(test_time) + test_time) / 2,
         Rmid = (lead(test_time) + test_time) / 2) %>%
  mutate(
    time_age100 = birthweek + 100,
    L = if_else(row_number() == 1, case_when(
      indi %in% buyin_pop ~ min(time_age100, test_time),
      indi %in% born_pop_true ~ birthweek,
      indi %in% ini_pop_true ~ 0
    ), Lmid),
    R = if_else(row_number() == n(), test_time, Rmid)) %>%
  mutate(run_id = data.table::rleid(pattern)) %>%
  group_by(indi, run_id) %>%
  summarise(
    birthweek = first(birthweek),
    pattern = unique(pattern),
    gender  = unique(gender),
    farm    = unique(farm),
    truebv  = unique(trBVs),
    L = min(L),
    R = max(R),
    age_L = L - birthweek,
    age_R = R - birthweek,
    .groups = "drop"
  ) 
anyNA(true_interval)
###check any gap or overlap
gap_overlap_animals_true <- true_interval %>%
  group_by(indi) %>%
  arrange(L, .by_group = TRUE) %>%
  mutate(
    L = round(L, 2),
    R = round(R, 2),
    gap      = lead(L) - R,
    overlap  = R - lead(L)
  ) %>%
  ungroup()
### filter susceptible animals and calving pen
true_I_state_base <- true_interval %>%
  mutate(time_age52 = birthweek + 52) %>%
  rowwise() %>%
  mutate(cross = (L < time_age52 & R > time_age52),   ##TRUE need to be divided
         pen = NA_character_)    
true_I_state <- bind_rows(
  true_I_state_base %>% 
    filter(cross == FALSE) %>%
    mutate(pen = if_else(age_R <= 52, "calf", "adult")),
  true_I_state_base %>% 
    filter(cross == TRUE) %>%
    mutate(R = time_age52,
           age_R = 52,
           pen = "calf"),
  true_I_state_base %>% 
    filter(cross == TRUE) %>%
    mutate(L = time_age52,
           age_L = 52,
           pen = "adult")
) %>% 
  filter(L < R) %>%     ####die after born
  mutate(L = round(L, 2),
         R = round(R, 2)) %>%
  dplyr::select(-run_id, -gender, -truebv) %>%
  mutate(L = ifelse(age_L < 1, L + (1 - age_L), L),
         age_L = ifelse(age_L < 1, 1, age_L)) %>%
  filter(L < R) %>%
  dplyr::select(-cross, -birthweek, -time_age52, -age_L, -age_R)
anyNA(true_I_state)  

##functions to get the exposure includes the plot
run_for_one_farm_L_old_for_all_pen <- function(df) {
  dt <- as.data.table(df)
  dt[, `:=`(
    farm    = as.factor(farm),
    pen     = factor(pen, levels = c("calf", "adult")),
    pattern = as.factor(pattern)
  )]
  time_points <- sort(unique(c(dt$L, dt$R)))
  time_grid <- data.table(  
    start = head(time_points, -1),     ##except the last record
    end   = tail(time_points, -1)      ##except the first record
  )
  time_grid[, mid := (start + end) / 2]
  setkey(dt, L, R)
  setkey(time_grid, start, end)
  ####[L, R)
  ov <- foverlaps(
    x       = time_grid,
    y       = dt,
    by.x    = c("start", "end"),
    by.y    = c("L", "R"),
    type    = "within",
    nomatch = 0L
  )
  counts <- ov[, .(n = uniqueN(indi)), by = .(start, end, mid, pattern, pen)]
  
  counts[, pattern := fcase(
    pattern == 1, "Itr",
    pattern == 3, "Il",
    pattern == 4, "Ih"
  )]
  counts[, pattern := factor(pattern, levels = c("Itr","Il","Ih"))]
  counts[, pen := factor(pen, levels = c("calf", "adult"))]
  
  time_key <- unique(time_grid[, .(start, end, mid)])
  pp_key <- CJ(
    pen     = levels(counts$pen),
    pattern = levels(counts$pattern),
    unique  = TRUE
  )
  time_key[, tmp := 1L]
  pp_key[,  tmp := 1L]
  full_key <- time_key[pp_key, on = "tmp", allow.cartesian = TRUE][, tmp := NULL]
  setkey(full_key, start, end, mid, pen, pattern)
  setkey(counts,   start, end, mid, pen, pattern)
  counts_full <- counts[full_key]
  counts_full[is.na(n), n := 0L]
  counts <- counts_full
  counts[, pen := factor(pen, levels = c("calf", "adult"))]
  
  setorder(counts, pen, pattern, start)    ## make sure the order is correct
  ##plot
  plot_df <- counts[!is.na(pattern), .(mid, pen, pattern, n)]
  farm_label <- unique(dt$farm)[1]
  title_text <- paste0("Farm ", farm_label)
  p_line <- ggplot(plot_df, aes(x = mid, y = n, color = pattern)) +
    geom_line(linewidth = 0.8) +
    # geom_point(size = 0.5) +
    facet_wrap(~ pen, ncol = 1, scales = "free_y") +
    labs(
      title = title_text,
      x = "Time (wk)",
      y = "Population",
      color = NULL
    ) +
    scale_x_continuous(limits = c(0, 260), breaks = seq(0, 260, 50)) +
    scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, 15)) +
    scale_color_manual(values = state_cols_l, drop = FALSE) +
    base_theme
  
  ##piece wise function
  make_piece_wise <- function(d) {
    if (nrow(d) == 0L) return(function(t) 0) 
    setorder(d, start)
    stepfun(d$start, c(0, d$n))
  }
  p_Itr_calf  <- make_piece_wise(counts[pattern == "Itr" & pen == "calf",  .(start, n)])
  p_Il_calf   <- make_piece_wise(counts[pattern == "Il"  & pen == "calf",  .(start, n)])
  p_Ih_calf   <- make_piece_wise(counts[pattern == "Ih"  & pen == "calf",  .(start, n)])
  p_Itr_adult <- make_piece_wise(counts[pattern == "Itr" & pen == "adult", .(start, n)])
  p_Il_adult  <- make_piece_wise(counts[pattern == "Il"  & pen == "adult", .(start, n)])
  p_Ih_adult  <- make_piece_wise(counts[pattern == "Ih"  & pen == "adult", .(start, n)])
  
  #### E calculation
  time_points <- seq(0, 260, 0.01)
  ##function calculate for one piece wise function
  solve_one_fun <- function(p_fun) {
    E0  <- 0
    out <- NULL
    
    for (k in 1:max_iter) {
      ode_formula <- function(t, y, parms) {
        E  <- y[1]
        dE <- sh * p_fun(t) - de * E
        list(c(dE))
      }
      out   <- as.data.table(ode(y = c(E = E0), times = time_points, func = ode_formula))
      E_end <- tail(out$E, 1)
      if (abs(E_end - E0) <= tol * (1 + abs(E_end))) break
      E0 <- E_end
    }
    
    out_final <- as.data.table(ode(y = c(E = E_end), times = time_points, func = ode_formula))
    out_final
  }
  
  E_Itr_calf  <- solve_one_fun(p_Itr_calf )[, .(time, E, pen="calf",  pattern="Itr")]
  E_Il_calf   <- solve_one_fun(p_Il_calf  )[, .(time, E, pen="calf",  pattern="Il" )]
  E_Ih_calf   <- solve_one_fun(p_Ih_calf  )[, .(time, E, pen="calf",  pattern="Ih" )]
  E_Itr_adult <- solve_one_fun(p_Itr_adult)[, .(time, E, pen="adult", pattern="Itr")]
  E_Il_adult  <- solve_one_fun(p_Il_adult )[, .(time, E, pen="adult", pattern="Il" )]
  E_Ih_adult  <- solve_one_fun(p_Ih_adult )[, .(time, E, pen="adult", pattern="Ih" )]
  
  E_plot_df <- rbindlist(list(
    E_Itr_calf, E_Il_calf, E_Ih_calf,
    E_Itr_adult, E_Il_adult, E_Ih_adult
  ))[
    , farm := farm_label
  ]
  
  E_plot_df[, pattern := factor(pattern, levels = c("Itr", "Il", "Ih"))]
  E_plot_df[, pen := factor(pen, levels = c("calf", "adult"))]
  
  p_E <- ggplot(E_plot_df, aes(x = time, y = E, color = pattern)) +
    geom_line(linewidth = 0.8) +
    # geom_point(size = 0.5) +
    facet_wrap(~ pen, ncol = 1, scales = "free_y") +
    labs(
      title = title_text,
      x = "Time (wk)",
      y = "Environmental contamination level",
      color = NULL
    ) +
    scale_x_continuous(limits = c(0, 260), breaks = seq(0, 260, 50)) +
    scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 200)) +
    scale_color_manual(values = state_cols_l, drop = TRUE) +
    base_theme
  
  ### exposure calculate (infectivity)
  E_exposure <- E_plot_df
  E_exposure[, beta := fcase(
    pattern == "Itr", 0.0299,
    pattern == "Il",  0.0150,
    pattern == "Ih",  0.219,
    default = NA_real_
  )]
  E_exposure[, exposure := beta * E]
  
  p_expo <- ggplot(E_exposure, aes(x = time, y = exposure, color = pattern)) +
    geom_line(linewidth = 0.8) +
    # geom_point(size = 0.5) +
    facet_wrap(~ pen, ncol = 1, scales = "free_y") +
    labs(
      title = title_text,
      x = "Time (wk)",
      y = "Force of environmental contamination",
      color = NULL
    ) +
    scale_x_continuous(limits = c(0, 260), breaks = seq(0, 260, 50)) +
    scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 10)) +
    scale_color_manual(values = state_cols_l, drop = TRUE) +
    base_theme
  
  return(list(
    n_I_df = plot_df,
    n_I_plot = p_line,
    E_df = E_plot_df,
    E_plot = p_E,
    exposure_df = E_exposure,
    exposure_plot = p_expo
  ))
}
true_I_state <- true_I_state %>% filter(pen == "calf") %>% dplyr::select(-pen)
run_for_one_farm_L <- function(df) {
  dt <- as.data.table(df)
  dt[, `:=`(
    farm    = as.factor(farm),
    pattern = as.factor(pattern)
  )]
  time_points <- sort(unique(c(dt$L, dt$R)))
  time_grid <- data.table(  
    start = head(time_points, -1),     ##except the last record
    end   = tail(time_points, -1)      ##except the first record
  )
  time_grid[, mid := (start + end) / 2]
  setkey(dt, L, R)
  setkey(time_grid, start, end)
  ####[L, R)
  ov <- foverlaps(
    x       = time_grid,
    y       = dt,
    by.x    = c("start", "end"),
    by.y    = c("L", "R"),
    type    = "within",
    nomatch = 0L
  )
  counts <- ov[, .(n = uniqueN(indi)), by = .(start, end, mid, pattern)]
  
  counts[, pattern := fcase(
    pattern == 1, "Itr",
    pattern == 3, "Il",
    pattern == 4, "Ih"
  )]
  counts[, pattern := factor(pattern, levels = c("Itr","Il","Ih"))]
  
  time_key <- unique(time_grid[, .(start, end, mid)])
  pp_key <- CJ(
    pattern = levels(counts$pattern),
    unique  = TRUE
  )
  time_key[, tmp := 1L]
  pp_key[,  tmp := 1L]
  full_key <- time_key[pp_key, on = "tmp", allow.cartesian = TRUE][, tmp := NULL]
  setkey(full_key, start, end, mid, pattern)
  setkey(counts,   start, end, mid, pattern)
  counts_full <- counts[full_key]
  counts_full[is.na(n), n := 0L]
  counts <- counts_full
  
  setorder(counts, pattern, start)    ## make sure the order is correct
  ##plot
  plot_df <- counts[!is.na(pattern), .(mid, pattern, n)]
  farm_label <- unique(dt$farm)[1]
  title_text <- paste0("Farm ", farm_label)
  p_line <- ggplot(plot_df, aes(x = mid, y = n, color = pattern)) +
    geom_line(linewidth = 1.2) +
    # geom_point(size = 0.5) +
    labs(
      title = title_text,
      x = "Time (wk)",
      y = "Population",
      color = NULL
    ) +
    scale_x_continuous(limits = c(0, 260), breaks = seq(0, 260, 50)) +
    scale_y_continuous(limits = c(0, 28), breaks = seq(0, 28, 4)) +
    scale_color_manual(values = state_cols_l, drop = FALSE) +
    base_theme
  
  ##piece wise function
  make_piece_wise <- function(d) {
    if (nrow(d) == 0L) return(function(t) 0) 
    setorder(d, start)
    stepfun(d$start, c(0, d$n))
  }
  p_Itr_calf  <- make_piece_wise(counts[pattern == "Itr", .(start, n)])
  p_Il_calf   <- make_piece_wise(counts[pattern == "Il", .(start, n)])
  p_Ih_calf   <- make_piece_wise(counts[pattern == "Ih", .(start, n)])
  
  #### E calculation
  time_points <- seq(0, 260, 0.01)
  ##function calculate for one piece wise function
  solve_one_fun <- function(p_fun) {
    E0  <- 0
    out <- NULL
    
    for (k in 1:max_iter) {
      ode_formula <- function(t, y, parms) {
        E  <- y[1]
        dE <- sh * p_fun(t) - de * E
        list(c(dE))
      }
      out   <- as.data.table(ode(y = c(E = E0), times = time_points, func = ode_formula))
      E_end <- tail(out$E, 1)
      if (abs(E_end - E0) <= tol * (1 + abs(E_end))) break
      E0 <- E_end
    }
    
    out_final <- as.data.table(ode(y = c(E = E_end), times = time_points, func = ode_formula))
    out_final
  }
  
  E_Itr_calf  <- solve_one_fun(p_Itr_calf )[, .(time, E, pattern="Itr")]
  E_Il_calf   <- solve_one_fun(p_Il_calf  )[, .(time, E, pattern="Il" )]
  E_Ih_calf   <- solve_one_fun(p_Ih_calf  )[, .(time, E, pattern="Ih" )]
  
  E_plot_df <- rbindlist(list(
    E_Itr_calf, E_Il_calf, E_Ih_calf
  ))[
    , farm := farm_label
  ]
  
  E_plot_df[, pattern := factor(pattern, levels = c("Itr", "Il", "Ih"))]
  
  p_E <- ggplot(E_plot_df, aes(x = time, y = E, color = pattern)) +
    geom_line(linewidth = 1.2) +
    # geom_point(size = 0.5) +
    labs(
      title = title_text,
      x = "Time (wk)",
      y = "Environmental contamination level",
      color = NULL
    ) +
    scale_x_continuous(limits = c(0, 260), breaks = seq(0, 260, 50)) +
    scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50)) +
    scale_color_manual(values = state_cols_l, drop = TRUE) +
    base_theme
  
  ### exposure calculate (infectivity)
  E_exposure <- E_plot_df
  E_exposure[, beta := fcase(
    pattern == "Itr", 0.0299,
    pattern == "Il",  0.0150,
    pattern == "Ih",  0.219,
    default = NA_real_
  )]
  E_exposure[, exposure := beta * E]
  p_expo <- ggplot(E_exposure, aes(x = time, y = exposure, color = pattern)) +
    geom_line(linewidth = 0.8) +
    # geom_point(size = 1) +
    labs(
      title = title_text,
      x = "Time (wk)",
      y = "Weighted environmental contamination level",
      color = NULL
    ) +
    scale_x_continuous(limits = c(0, 260), breaks = seq(0, 260, 50)) +
    scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, 3)) +
    scale_color_manual(values = state_cols_l, drop = TRUE) +
    base_theme
  
  return(list(
    n_I_df = plot_df,
    n_I_plot = p_line,
    E_df = E_plot_df,
    E_plot = p_E,
    exposure_df = E_exposure,
    exposure_plot = p_expo
  ))
}

##run for 10 farms, results are in wk_run_for_10_farms
farm_ids <- sort(unique(true_state$farm))
true_run_for_10_farms <- lapply(farm_ids, function(fid) {
  message("Running exposure_table for farm ", fid, " ...")
  df <- true_I_state %>% filter(farm == fid)
  run_for_one_farm_L(df)
})
names(true_run_for_10_farms) <- paste0("farm_", farm_ids)
exposure_combined_true <- rbindlist(
  lapply(names(true_run_for_10_farms), function(farm_name) {
    df <- true_run_for_10_farms[[farm_name]]$exposure_df
    return(df)
  }),
  use.names = TRUE,
  fill = TRUE
)
exp_cp_true_base <- exposure_combined_true %>%
  group_by(farm, time) %>%           
  summarise(exposure = sum(exposure, na.rm = TRUE), .groups = "drop") %>%
  arrange(farm, time) %>%
  group_by(farm) %>%
  mutate(time_next = lead(time)) %>%                
  ungroup() %>%
  filter(!is.na(time_next)) %>%
  rename(start = time, end = time_next) %>%
  mutate(start = round(start, 2),
         end = round(end, 2)) %>%
  as.data.table()
exp_cp_true <- exp_cp_true_base %>%
  arrange(start) %>%
  slice_head(n = 10) %>%
  mutate(start = -260,
         end = 0) %>%
  bind_rows(exp_cp_true_base)

###binary
binary_true <- true_pattern %>%
  filter(!indi %in% buyin_pop) %>%
  filter(!indi %in% infected_by_mom_true) %>%
  group_by(indi) %>%
  arrange(test_time, age, .by_group = TRUE) %>%
  filter(max(age) > 1) %>%
  summarise(
    result  = as.integer(any(pattern != 0, na.rm = TRUE)),
    time_1  = first(birthweek) + 1,
    time_end = pmin(first(birthweek) + 20, last(test_time), na.rm = TRUE),
    age_time_1 = 1,
    age_time_end = time_end - first(birthweek),
    gender  = unique(gender),
    farm    = unique(farm),
    truebv  = unique(trBVs),
    .groups = "drop"
  ) %>%
  filter(time_1 < time_end) %>%
  mutate(age_decay_offset = log((exp(-g * age_time_1) - exp(-g * age_time_end)) / (g * (age_time_end - age_time_1))))

## exposure calculate
str(exp_cp_true)
str(binary_true)
binary_true_dt <- as.data.table(binary_true)
binary_true_dt[, farm := as.factor(farm)]
exp_cp_true_dt <- as.data.table(exp_cp_true)
setkey(binary_true_dt, farm, time_1, time_end)
setkey(exp_cp_true_dt, farm, start,  end)
ov <- foverlaps(
  x = exp_cp_true_dt,          
  y = binary_true_dt,               
  by.x = c("farm","start","end"),
  by.y = c("farm","time_1","time_end"),
  type = "any",                
  nomatch = 0L
)
ov[, overlap := pmin(end, time_end) - pmax(start, time_1)]
ov <- ov[overlap > 0]
ov[, contrib := exposure * overlap]

##model data prepare
true_offset_binary <- ov %>%
  group_by(farm, indi, result, time_1, time_end, age_time_1, age_time_end, gender, truebv, age_decay_offset) %>%
  summarise(expo_offset = log(sum(contrib) / 76),
            .groups = "drop" ) %>%
  mutate(offset = expo_offset + age_decay_offset) %>%
  mutate(
    indi   = as.factor(indi),
    gender = as.factor(gender),
    farm   = as.factor(farm)
  ) 

### compare with annually
common_indi <- intersect(
  unique(true_offset_binary$indi),
  unique(an_offset_binary$indi)
)
true_common <- true_offset_binary %>%
  filter(indi %in% common_indi)
an_common <- an_offset_binary %>%
  filter(indi %in% common_indi)
##expo_offset
true_small <- true_common %>% dplyr::select(indi, expo_offset_true = expo_offset)
an_small <- an_common %>% dplyr::select(indi, expo_offset_an = expo_offset)
compare_offset <- true_small %>%
  inner_join(an_small, by = "indi")
ggplot(compare_offset, aes(x = expo_offset_an, y = expo_offset_true)) +
  geom_point(alpha = 0.6, color = "#1f77b4") +
  geom_abline(slope = 1, intercept = 0, color = "darkgreen", linetype = "dotted") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    x = "Annually-based exposure offset",
    y = "True-state-based exposure offset",
    title = "Comparison of exposure offset"
  ) +
  theme_minimal(base_size = 13)
cor(compare_offset$expo_offset_true, compare_offset$expo_offset_an, method = "spearman")
cor(compare_offset$expo_offset_true, compare_offset$expo_offset_an, method = "pearson")
##age_offset
true_small <- true_common %>% dplyr::select(indi, age_decay_offset_true = age_decay_offset)
an_small <- an_common %>% dplyr::select(indi, age_decay_offset_an = age_decay_offset)
compare_offset <- true_small %>%
  inner_join(an_small, by = "indi")
ggplot(compare_offset, aes(x = age_decay_offset_an, y = age_decay_offset_true)) +
  geom_point(alpha = 0.6, color = "#1f77b4") +
  geom_abline(slope = 1, intercept = 0, color = "darkgreen", linetype = "dotted") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    x = "Annually-based age_decay_offset",
    y = "true-based age_decay_offset",
    title = "Comparison of age_decay_offset (true vs annually)"
  ) +
  theme_minimal(base_size = 13)
cor(compare_offset$age_decay_offset_true, compare_offset$age_decay_offset_an, method = "spearman")
cor(compare_offset$age_decay_offset_true, compare_offset$age_decay_offset_an, method = "pearson")
##result
true_small <- true_common %>% dplyr::select(indi, result_true = result)
an_small <- an_common %>% dplyr::select(indi, result_an = result)
compare_offset <- true_small %>%
  inner_join(an_small, by = "indi")
tab <- compare_offset %>%
  count(result_an, result_true) %>%
  complete(result_an = 0:1, result_true = 0:1, fill = list(n = 0))
ggplot(tab, aes(x = factor(result_an), y = factor(result_true), fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), color = "black", size = 5) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    x = "Annually-based result",
    y = "true-based result",
    title = "Confusion Matrix: Annually vs true result"
  ) +
  theme_minimal(base_size = 14)
cor(compare_offset$result_true, compare_offset$result_an, method = "spearman")
cor(compare_offset$result_true, compare_offset$result_an, method = "pearson")
# TP <- sum(compare_offset$result_an == 1 & compare_offset$result_true == 1, na.rm = TRUE)
# FN <- sum(compare_offset$result_an == 0 & compare_offset$result_true == 1, na.rm = TRUE)
# TN <- sum(compare_offset$result_an == 0 & compare_offset$result_true == 0, na.rm = TRUE)
# FP <- sum(compare_offset$result_an == 1 & compare_offset$result_true == 0, na.rm = TRUE)
# 
# sensitivity <- if ((TP + FN) == 0) NA_real_ else TP / (TP + FN)
# specificity <- if ((TN + FP) == 0) NA_real_ else TN / (TN + FP)
# 
# metrics <- tibble::tibble(
#   TP = TP, FN = FN, TN = TN, FP = FP,
#   sensitivity = sensitivity,
#   specificity = specificity
# )



##asreml
cloglog_true <- asreml(
  fixed    = result ~ 1 + offset(offset),
  random   = ~ vm(indi, Ainv_full) + farm,
  data     = true_offset_binary,
  family   = asr_binomial(link = "cloglog"),
  workspace = 2e8,
  maxit    = 50
)
wald(cloglog_true)
summary(cloglog_true)

esti_bv_true <- as.data.frame(cloglog_true$coefficients$random) %>%
  rownames_to_column("indi") %>%
  as_tibble() %>%
  mutate(
    indi = str_remove(indi, "^vm\\(indi, Ainv_full\\)_") 
  )

sire_com_true <- esti_bv_true %>%
  mutate(indi = suppressWarnings(as.numeric(indi))) %>%
  filter(!is.na(indi)) %>%
  filter(indi >= 1 & indi <= 90) %>%
  arrange(indi) %>%
  left_join(sirebv, by = "indi") %>%
  mutate(true_bv = V1,
         esti_bv = effect)
ggplot(sire_com_true, aes(x = esti_bv, y = true_bv)) +
  geom_point(alpha = 0.6, color = "#1f77b4") +
  geom_abline(slope = 1, intercept = 0, color = "darkgreen", linetype = "dotted")+
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(
    x = "Estimated sire breeding value",
    y = "True sire breeding value",
    title = "Comparison between true and estimated sire breeding values"
  ) +
  theme_minimal(base_size = 13)
cor(sire_com_true$true_bv, sire_com_true$esti_bv, method = "spearman")
cor(sire_com_true$true_bv, sire_com_true$esti_bv, method = "pearson")

####true bv
compare_bv_true <- true_offset_binary %>%
  dplyr::select(indi, result, truebv) %>%
  left_join(esti_bv_true,  by = "indi") %>%
  rename(	
    true_bv = truebv,
    esti_bv = effect)
anyNA(compare_bv_true)
ggplot(compare_bv_true, aes(x = esti_bv, y = true_bv)) +
  geom_point(alpha = 0.6, color = "#1f77b4") +
  geom_abline(slope = 1, intercept = 0, color = "darkgreen", linetype = "dotted")+
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(
    x = "Estimated breeding value",
    y = "True breeding value",
    title = "Comparison between true and estimated breeding values"
  ) +
  theme_minimal(base_size = 13)
cor(compare_bv_true$true_bv, compare_bv_true$esti_bv, method = "spearman")
cor(compare_bv_true$true_bv, compare_bv_true$esti_bv, method = "pearson")
cor(compare_bv_true$result, compare_bv_true$true_bv, method = "pearson")
lm(true_bv ~ esti_bv, data = compare_bv_true)
ggplot(compare_bv_true, aes(x = true_bv, fill = factor(result))) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#d62728"),
                    name = "result",
                    labels = c("0 = negative", "1 = positive")) +
  theme_bw() +
  labs(
    x = "True breeding value",
    y = "Count",
    title = "Distribution of true breeding value by result"
  )

