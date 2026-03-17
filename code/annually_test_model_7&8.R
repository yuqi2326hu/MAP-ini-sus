infected_by_mom_an <- annually_test %>%
  filter(age < 1, state == 1) %>%
  pull(indi) %>%
  unique()
born_pop_an <- annually_test %>%
  group_by(indi) %>%
  filter(!(indi %in% buyin_pop)) %>%
  filter(first(birthweek) >= 0) %>%
  pull(indi) %>% unique()
ini_pop_an <- annually_test %>%
  group_by(indi) %>%
  filter(!(indi %in% buyin_pop)) %>%
  filter(first(birthweek) < 0) %>%
  pull(indi) %>% unique()
all_indi_an <- annually_test %>%
  distinct(indi) %>%
  pull(indi)
pop_indi_an <- c(buyin_pop, born_pop_an, ini_pop_an)
setdiff(pop_indi_an, all_indi_an)
setdiff(all_indi_an, pop_indi_an)
dup_pop_an <- pop_indi_an[duplicated(pop_indi_an)]
unique(dup_pop_an)

##shedding pattern
annually_pattern_excl_3 <- annually_test %>%
  arrange(indi, test_time, age) %>%
  group_by(indi) %>%
  mutate(
    future_neg = {
      v <- state == 0
      rev(cummax(rev(v)))
    }
  ) %>%
  mutate(
    has_transient = any(state == 1 & future_neg),
    first_pos_before52 = (state == 1 & age <= 52) & (cumsum(state == 1 & age <= 52) == 1),
    pattern = case_when(
      state == 0                                                          ~ 0L,
      state == 1 & future_neg                                             ~ 11L,
      state == 1 & !has_transient & first_pos_before52                    ~ 12L,
      state == 1                                                          ~ 2L,
      TRUE                                                                ~ NA_integer_
    )
  ) %>%
  dplyr::select(-has_transient, -first_pos_before52, -future_neg) %>%
  ungroup()
transient_an <- annually_pattern_excl_3 %>%
  group_by(pattern) %>%
  summarise(n = n_distinct(unique(indi)))
annually_pattern_excl_3 <- annually_pattern_excl_3 %>%
  mutate(pattern = if_else(pattern %in% c(11L, 12L), 1L, pattern))
annually_pattern_3 <- annually_pattern_excl_3 %>%
  group_by(indi) %>%
  arrange(test_time, age, .by_group = TRUE) %>%
  mutate(
    third_pattern2 = {
      idx <- which(pattern == 2)
      if (length(idx) >= 3) idx[3] else NA_integer_        
    }
  ) %>%
  filter(!is.na(third_pattern2)) %>%
  filter(row_number() >= third_pattern2) %>%
  filter(age > 208) %>%
  slice_tail(n = 2) %>%
  mutate(
    gap = if (n() == 2) last(test_time) - first(test_time) else NA_real_,
    pattern = case_when(
      n() == 1                                      ~ 3L,
      n() == 2 & gap > 16  & row_number() == n()    ~ 3L,
      n() == 2 & gap <= 16                          ~ 3L,
      TRUE                                          ~ as.integer(pattern)
    )
  ) %>%
  filter(pattern == 3) %>%
  dplyr::select(-third_pattern2, -gap) %>%
  ungroup()
annually_pattern <- annually_pattern_excl_3 %>%
  rows_update(annually_pattern_3, by = c("indi", "test_time", "age")) 
anyNA(annually_pattern$pattern)
annually_pattern %>%
  group_by(pattern) %>%
  summarise(n = n())
#### compare with the true pattern
true_pattern_noL <- true_state %>%
  mutate(pattern = case_when(
    state == "S" ~ 0L,
    state %in% c("Itrap", "Itrcp") ~ 1L,
    state %in% c("L1cp", "L2cp", "L1ap", "L2ap") ~ 2L,
    state %in% c("Ilap", "Ilcp") ~ 3L,
    state %in% c("Ihap", "Ihcp") ~ 4L,
    state %in% "nbItr" ~ 1L
  ))
anyNA(true_pattern_noL)
annually_pattern1 <- annually_pattern %>%
  mutate(pattern = case_when(
    pattern == 2 ~3L,
    pattern == 3 ~4L,
    TRUE ~ pattern
  )) %>%
  group_by(indi) %>%
  mutate(pattern = if_else(pattern == 0 & cummax(pattern == 1), 2L, pattern))
anyNA(annually_pattern1)
compare_an <- annually_pattern1 %>%
  left_join(true_pattern_noL %>% dplyr::select(test_time, indi, pattern) %>% rename(true = pattern), by = c("test_time", "indi")) %>%
  mutate(
    esti_pattern = factor(
      pattern,
      levels = c(0, 1, 2, 3, 4),
      labels = c("susceptible", "transient", "latent", "low", "high")
    ),
    true_pattern = factor(
      true,
      levels = c(0, 1, 2, 3, 4),
      labels = c("susceptible", "transient", "latent", "low", "high")
    )
  )
tab_an <- addmargins(table(compare_an$true_pattern, compare_an$esti_pattern))
an_pattern_sen <- compare_an %>%
  mutate(correct = (esti_pattern == true_pattern)) %>%
  group_by(true_pattern) %>%
  summarise(
    n = n(),
    correct_n = sum(correct),
    sensitivity = correct_n/n
  )
an_pattern_pre <- compare_an %>%
  mutate(correct = (esti_pattern == true_pattern)) %>%
  group_by(esti_pattern) %>%
  summarise(
    n = n(),
    correct_n = sum(correct),
    precision = correct_n/n
  )

### shedding time interval
dur_an <- function(p) {
  ifelse(
    p == 1, 12,
    ifelse(
      p %in% c(2, 0), 52,
      ifelse(p == 3, 52, NA_real_)
    )
  )
}
annually_interval_divide <- annually_pattern %>%
  group_by(indi) %>%
  arrange(test_time, age, .by_group = TRUE) %>%
  mutate(
    prev_pattern = lag(pattern),
    time_age100 = birthweek + 100, 
    L = NA_real_,
    R = NA_real_,
    L = if_else(row_number() == 1, case_when(
      indi %in% buyin_pop ~ min(time_age100, test_time),
      indi %in% born_pop_an ~ birthweek,
      indi %in% ini_pop_an ~ 0
    ), L),
    R = if_else(row_number() == n(), test_time, R),
    L_mid = NA_real_,
    L_mid = if_else(n() > 1 & row_number() > 1, (lag(test_time) + test_time) / 2, L_mid),
    R_mid = NA_real_,
    R_mid = if_else(n() > 1 & row_number() < n(), (test_time + lead(test_time)) / 2, R_mid),
    L = if_else(row_number() > 1, vmax(L_mid, test_time - vmin(dur_an(pattern) / 2, age / 2), 0), L),
    R = if_else(row_number() < n(), vmin(R_mid, test_time + dur_an(pattern) / 2), R)
  ) %>%
  dplyr::select(-L_mid, -R_mid, -prev_pattern, -time_age100) %>%
  ungroup()
anyNA(annually_interval_divide)
### add one pattern == 0 record after the pattern == 1
annually_1_2 <- annually_interval_divide %>%
  group_by(indi) %>%
  arrange(test_time, age, .by_group = TRUE) %>%
  mutate(nextL = lead(L)) %>%
  filter(pattern == 1) %>%
  mutate(L = R,
         R = nextL,
         pattern = 0,
         state = 0) %>%
  filter(L < R) %>%
  dplyr::select(-nextL)
annually_interval_combined <- bind_rows(annually_1_2, annually_interval_divide) %>%
  arrange(indi, L)
###fill the gap
full_gap <- annually_interval_combined %>%
  group_by(indi) %>%
  arrange(L, .by_group = TRUE) %>%
  mutate(leadL = lead(L),
         gap = lead(L) - R) %>%
  filter(gap > 0) %>%
  mutate(R = leadL) %>%
  filter(L < R) %>%
  dplyr::select(-gap, -leadL)
annually_interval <- annually_interval_combined %>%
  rows_update(full_gap, by = c("indi", "L")) %>%
  arrange(indi, L)  %>%
  group_by(indi) %>%
  mutate(segment_id = cumsum(pattern != lag(pattern, default = first(pattern)))) %>%
  group_by(indi, segment_id) %>%
  summarise(farm = unique(farm),
            L = min(L),
            R = max(R),
            pattern = unique(pattern),
            birthweek = first(birthweek),
            .groups = "drop") %>%
  arrange(L) %>%
  group_by(indi) %>%
  mutate(Rmax = max(R),
         L = ifelse(pattern ==3, Rmax - 16, L),
         R = ifelse(!is.na(lead(pattern)) & lead(pattern) == 3, Rmax - 16, R)) %>%
  dplyr::select(-segment_id, -Rmax) %>%
  ungroup()
anyNA(annually_interval)
###check any gap or overlap
gap_overlap_animals_an <- annually_interval %>%
  group_by(indi) %>%
  arrange(L, .by_group = TRUE) %>%
  mutate(
    L = round(L, 2),
    R = round(R, 2),
    gap      = lead(L) - R,
    overlap  = R - lead(L)
  ) %>%
  ungroup()
### divide the calf pen and adult pen, calving age < 1, calf age < 52, adult >= 52
annually_I_state_base <- annually_interval %>%
  mutate(time_age52 = birthweek + 52) %>%
  dplyr::select(indi, farm, pattern, L, R, birthweek, time_age52) %>%
  rowwise() %>%
  mutate(age_L = L - birthweek,
         age_R = R - birthweek,
         cross = (L < time_age52 & R > time_age52),   ##TRUE need to be divided
         pen = NA_character_)    
annually_I_state <- bind_rows(
  annually_I_state_base %>% 
    filter(cross == FALSE) %>%
    mutate(pen = if_else(age_R <= 52, "calf", "adult")),
  annually_I_state_base %>% 
    filter(cross == TRUE) %>%
    mutate(R = time_age52,
           age_R = 52,
           pen = "calf"),
  annually_I_state_base %>% 
    filter(cross == TRUE) %>%
    mutate(L = time_age52,
           age_L = 52,
           pen = "adult")
) %>%
  filter(L < R) %>%     ####die after born
  mutate(L = round(L, 2),
         R = round(R, 2))
annually_I_state <- annually_I_state %>%
  mutate(L = ifelse(age_L < 1, L + (1 - age_L), L),
         age_L = ifelse(age_L < 1, 1, age_L)) %>%
  filter(L < R) %>%
  dplyr::select(-cross, -birthweek, -time_age52, -age_L, -age_R)
anyNA(annually_I_state)


##functions to get the exposure includes the plot
run_for_one_farm_old_for_all_pen <- function(df) {
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
    pattern == 2, "Il",
    pattern == 3, "Ih"
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
    scale_color_manual(values = state_cols, drop = FALSE) +
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
    scale_color_manual(values = state_cols, drop = TRUE) +
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
    scale_color_manual(values = state_cols, drop = TRUE) +
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
annually_I_state <- annually_I_state %>% filter(pen == "calf") %>% dplyr::select(-pen)
run_for_one_farm <- function(df) {
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
    pattern == 2, "Il",
    pattern == 3, "Ih"
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
    geom_line(linewidth = 0.8) +
    # geom_point(size = 0.5) +
    labs(
      title = title_text,
      x = "Time (wk)",
      y = "Population",
      color = NULL
    ) +
    scale_x_continuous(limits = c(0, 260), breaks = seq(0, 260, 50)) +
    scale_y_continuous(limits = c(0, 28), breaks = seq(0, 28, 4)) +
    scale_color_manual(values = state_cols, drop = FALSE) +
    base_theme
  
  ##piece wise function
  make_piece_wise <- function(d) {
    if (nrow(d) == 0L) return(function(t) 0) 
    setorder(d, start)
    stepfun(d$start, c(0, d$n))
  }
  p_Itr_calf  <- make_piece_wise(counts[pattern == "Itr",  .(start, n)])
  p_Il_calf   <- make_piece_wise(counts[pattern == "Il",  .(start, n)])
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
    geom_line(linewidth = 0.8) +
    # geom_point(size = 0.5) +
    labs(
      title = title_text,
      x = "Time (wk)",
      y = "Environmental contamination level",
      color = NULL
    ) +
    scale_x_continuous(limits = c(0, 260), breaks = seq(0, 260, 50)) +
    scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50)) +
    scale_color_manual(values = state_cols, drop = TRUE) +
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
    scale_color_manual(values = state_cols, drop = TRUE) +
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

##run for 10 farms, results are in an_run_for_10_farms
farm_ids <- sort(unique(annually_test$farm))
an_run_for_10_farms <- lapply(farm_ids, function(fid) {
  message("Running exposure_table for farm ", fid, " ...")
  df <- annually_I_state %>% filter(farm == fid)
  run_for_one_farm(df)
})
names(an_run_for_10_farms) <- paste0("farm_", farm_ids)
exposure_combined_an <- rbindlist(
  lapply(names(an_run_for_10_farms), function(farm_name) {
    df <- an_run_for_10_farms[[farm_name]]$exposure_df
    return(df)
  }),
  use.names = TRUE,
  fill = TRUE
)
exp_cp_an_base <- exposure_combined_an %>%
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
exp_cp_an <- exp_cp_an_base %>%
  arrange(start) %>%
  slice_head(n = 10) %>%
  mutate(start = -260,
         end = 0) %>%
  bind_rows(exp_cp_an_base)

###binary

# inf_an <- annually_test %>%
#   filter(!indi %in% buyin_pop) %>%
#   filter(!indi %in% infected_by_mom_an) %>%
#   group_by(indi) %>%
#   arrange(test_time, age, .by_group = TRUE) %>%
#   filter(max(age) > 1) %>%
#   # filter(first(birthweek) + 1 >= 0) %>%
#   summarise(
#     result = as.integer(any(state == 1 & age <= 52, na.rm = TRUE))
#     # result  = as.integer(any(state == 1, na.rm = TRUE))
#   ) %>%
#   filter(result == 1)
    
binary_an <- annually_test %>%
  # filter(indi %in% a) %>%
  filter(!indi %in% buyin_pop) %>%
  filter(!indi %in% infected_by_mom_an) %>%
  filter(!indi %in% ini_pop_an) %>%
  group_by(indi) %>%
  arrange(test_time, age, .by_group = TRUE) %>%
  filter(max(age) > 1) %>%
  summarise(
    result  = as.integer(any(state == 1, na.rm = TRUE)),
    # result = as.integer(
    #   if (any(age <= 52, na.rm = TRUE)) {
    #     any(state == 1 & age <= 52, na.rm = TRUE)
    #   } else {
    #     any(state == 1, na.rm = TRUE)
    #   }
    # ),
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
str(exp_cp_an)
str(binary_an)
binary_an_dt <- as.data.table(binary_an)
binary_an_dt[, farm := as.factor(farm)]
exp_cp_an_dt <- as.data.table(exp_cp_an)
setkey(binary_an_dt, farm, time_1, time_end)
setkey(exp_cp_an_dt, farm, start,  end)
ov <- foverlaps(
  x = exp_cp_an_dt,          
  y = binary_an_dt,               
  by.x = c("farm","start","end"),
  by.y = c("farm","time_1","time_end"),
  type = "any",                
  nomatch = 0L
)
ov[, overlap := pmin(end, time_end) - pmax(start, time_1)]
ov <- ov[overlap > 0]
ov[, contrib := exposure * overlap]

##model data prepare
an_offset_binary <- ov %>%
  group_by(farm, indi, result, time_1, time_end, age_time_1, age_time_end, gender, truebv, age_decay_offset) %>%
  summarise(expo_offset = log(sum(contrib) / 76),
            .groups = "drop" ) %>%
  mutate(offset = expo_offset + age_decay_offset) %>%
  mutate(
    indi   = as.factor(indi),
    gender = as.factor(gender),
    farm   = as.factor(farm)
  ) 
##asreml
cloglog_annually <- asreml(
  fixed    = result ~ 1 + offset(offset),
  random   = ~ vm(indi, Ainv_full) + farm,
  data     = an_offset_binary,
  family   = asr_binomial(link = "cloglog"),
  workspace = 2e7,
  maxit    = 50
)
# cloglog_annually <- update(cloglog_annually)
cloglog_annually$coef$fixed
wald(cloglog_annually)
summary(cloglog_annually)
# lrt(cloglog_annually, update(cloglog_annually, random = ~ farm), boundary = TRUE)
esti_bv_an <- as.data.frame(cloglog_annually$coef$random) %>%
  rownames_to_column("term") %>%
  filter(grepl("^vm\\(indi, Ainv_full\\)_", term)) %>%
  transmute(
    indi = sub("^vm\\(indi, Ainv_full\\)_", "", term),
    effect = effect
  )
# pv_indi_an <- predict(
#   object  = cloglog_annually,
#   classify = "indi",
#   onlyuse  = "vm(indi, Ainv_full)",
#   pworkspace = 5e8
# )
# pv_se_an <- pv_indi_an$pvals
# esti_bv_an <- inner_join(esti_bv_an, pv_se_an, by="indi") %>%
#   dplyr::select(indi, effect, std.error)
# sire_com_an <- esti_bv_an %>%
#   mutate(indi = suppressWarnings(as.numeric(indi))) %>%
#   filter(!is.na(indi)) %>%
#   filter(indi >= 1 & indi <= 90) %>%
#   arrange(indi) %>%
#   left_join(sirebv, by = "indi") %>%
#   mutate(true_bv = V1,
#          esti_bv = effect)
# ggplot(sire_com_an, aes(x = esti_bv, y = true_bv)) +
#   geom_point(alpha = 0.6, color = "#1f77b4") +
#   geom_abline(slope = 1, intercept = 0, color = "darkgreen", linetype = "dotted")+
#   geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
#   labs(
#     x = "Estimated sire breeding value",
#     y = "True sire breeding value",
#     title = "Comparison between true and estimated sire breeding values"
#   ) +
#   theme_minimal(base_size = 13)
# cor(sire_com_an$true_bv, sire_com_an$esti_bv, method = "spearman")
# cor(sire_com_an$true_bv, sire_com_an$esti_bv, method = "pearson")

####true bv
compare_bv_an <- an_offset_binary %>%
  dplyr::select(indi, result, truebv) %>%
  left_join(esti_bv_an,  by = "indi") %>%
  rename(	
    true_bv = truebv,
    esti_bv = effect)
# anyNA(compare_bv_an)
# ggplot(compare_bv_an, aes(x = esti_bv, y = true_bv)) +
#   geom_point(alpha = 0.6, color = "#1f77b4") +
#   geom_abline(slope = 1, intercept = 0, color = "darkgreen", linetype = "dotted")+
#   geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
#   labs(
#     x = "Estimated breeding value",
#     y = "True breeding value",
#     title = "Comparison between true and estimated breeding values"
#   ) +
#   theme_minimal(base_size = 13)
cor.test(compare_bv_an$true_bv, compare_bv_an$esti_bv)
sire_com_an <- esti_bv_an %>%
  mutate(indi = as.character(indi)) %>%
  filter(indi %in% as.character(sire)) %>%
  rename(esti_bv = effect) %>%
  left_join(
    sirebv %>% mutate(indi = as.character(indi)),
    by = "indi"
  )
cor.test(sire_com_an$true_bv, sire_com_an$esti_bv)
cor.test(compare_bv_an$result, compare_bv_an$true_bv, method = "pearson")
lm(true_bv ~ esti_bv, data = compare_bv_an)

true_re <- weekly_test %>%
  group_by(indi) %>%
  summarise(result = if_else(any(state) == 1, 1, 0),
            true_bv = unique(trBVs)) 
ggplot(true_re, aes(x = true_bv, fill = factor(result))) +
  geom_histogram(aes(y = after_stat(density)),
                 alpha = 0.5,
                 position = "identity",
                 bins = 30) +
  scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#d62728"),
                    name = "result",
                    labels = c("0 = negative", "1 = positive")) +
  theme_bw() +
  labs(
    x = "True breeding value",
    y = "Density",
    title = "Distribution of true breeding value by result"
  )











### threshold model
binary_an <- annually_test %>%
  # filter(indi %in% a) %>%
  filter(!indi %in% buyin_pop) %>%
  filter(!indi %in% infected_by_mom_an) %>%
  group_by(indi) %>%
  arrange(test_time, age, .by_group = TRUE) %>%
  filter(max(age) > 1) %>%
  # filter(first(birthweek) + 1 >= 0) %>%
  summarise(
    result  = as.integer(any(state == 1, na.rm = TRUE)),
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
  mutate(age_decay_offset = 0)
cloglog_annually <- asreml(
  fixed    = result ~ 1 + expo_offset + age_decay_offset,
  random   = ~ vm(indi, Ainv_full) + farm,
  data     = an_offset_binary,
  family   = asr_binomial(link = "probit"),
  workspace = 2e8,
  maxit    = 50
)
# cloglog_annually <- update(cloglog_annually)
wald(cloglog_annually)
summary(cloglog_annually)



