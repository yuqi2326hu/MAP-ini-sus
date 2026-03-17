infected_by_mom_wk_1 <- weekly_test %>%
  group_by(indi) %>%
  arrange(test_time, age, .by_group = TRUE) %>%
  mutate(lag_age = lag(age)) %>%
  filter(state == 1) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(inf_age = (age + lag_age) / 2) %>%
  filter(inf_age <= 1) %>%
  pull(indi) %>%
  unique()
infected_by_mom_wk_2 <- weekly_test %>%
  filter(age < 1, state == 1) %>%
  pull(indi) %>%
  unique()
infected_by_mom_wk <- union(
  infected_by_mom_wk_1,
  infected_by_mom_wk_2
)
born_pop_wk <- weekly_test %>%
  group_by(indi) %>%
  filter(!(indi %in% buyin_pop)) %>%
  filter(first(birthweek) >= 0) %>%
  pull(indi) %>% unique()
ini_pop_wk <- weekly_test %>%
  group_by(indi) %>%
  filter(!(indi %in% buyin_pop)) %>%
  filter(first(birthweek) < 0) %>%
  pull(indi) %>% unique()
all_indi_wk <- weekly_test %>%
  distinct(indi) %>%
  pull(indi)
pop_indi_wk <- c(buyin_pop, born_pop_wk, ini_pop_wk)
setdiff(pop_indi_wk, all_indi_wk)
setdiff(all_indi_wk, pop_indi_wk)
dup_pop_wk <- pop_indi_wk[duplicated(pop_indi_wk)]
unique(dup_pop_wk)
##shedding pattern
state_changed <- weekly_test %>%
  group_by(indi) %>%
  arrange(test_time, age, .by_group = TRUE) %>%
  summarise(n_change = sum(state != lag(state, default = first(state))))
state_never_changed <- state_changed %>%
  filter(n_change == 0) %>%
  pull(indi)
state_changed_23 <- state_changed %>%
  filter(n_change >= 2) %>%
  pull(indi)
state_changed_once <- state_changed %>%
  filter(n_change == 1) %>%
  pull(indi)
state_changed_once_1_0 <- weekly_test %>%
  group_by(indi) %>%
  arrange(test_time, age, .by_group = TRUE) %>%
  filter(indi %in% state_changed_once) %>%
  filter(state == 0 & lag(state) == 1) %>%
  pull(indi)
state_changed_once_0_1 <- weekly_test %>%
  group_by(indi) %>%
  arrange(test_time, age, .by_group = TRUE) %>%
  filter(indi %in% state_changed_once) %>%
  filter(state == 1 & lag(state) == 0) %>%
  pull(indi)
weekly_pattern_change0 <- weekly_test %>%
  filter(indi %in% state_never_changed) %>%
  arrange(indi, test_time, age) %>%
  group_by(indi) %>%
  mutate(
    dur = max(age) - min(age),
    pattern = case_when(
      state == 0                                    ~ 0L,
      state == 1 & dur > 52                         ~ 3L,
      state == 1 & dur <= 52                        ~ case_when(
        min(age) <= 52                              ~ 1L,
        min(age) > 52                               ~ 3L
      ), 
      TRUE                                          ~ NA_integer_
    )) %>%
  dplyr::select(-dur) %>%
  ungroup()
weekly_pattern_change1 <- weekly_test %>%
  filter(indi %in% state_changed_once) %>%
  arrange(indi, test_time, age) %>%
  group_by(indi) %>%
  mutate(
    state_prev = lag(state),
    change_age_01 = {
      idx <- which(state_prev == 0 & state == 1)
      if (length(idx) == 0) NA_real_ else age[idx]
    }
  ) %>%
  mutate(
    dur1 = sum(state == 1),
    pattern = case_when(
      indi %in% state_changed_once_1_0 ~ case_when(
        state == 1 ~ 1L,
        state == 0 ~ 2L
      ),
      indi %in% state_changed_once_0_1 ~ case_when(
        dur1 > 52 ~ case_when(
          state == 1 ~ 3L,
          state == 0 ~ 2L
        ),
        dur1 <= 52 ~ case_when(
          change_age_01 <= 52 ~ case_when(
            state == 0 ~ 0L,
            state == 1 ~ 1L
          ),
          change_age_01 > 52 ~ case_when(
            state == 0 ~ 2L,
            state == 1 ~ 3L
          )
        )
      )
    )) %>%
  dplyr::select(-state_prev, -change_age_01, -dur1) %>%
  ungroup()
weekly_pattern_change23 <- weekly_test %>%
  arrange(indi, test_time, age) %>%
  group_by(indi) %>%
  filter(indi %in% state_changed_23) %>%
  mutate(
    seen_pos_before = lag(cummax(state == 1), default = FALSE),
    future_neg = {
      v <- state == 0
      rev(cummax(rev(v)))
    }
  ) %>%
  mutate(
    pattern = case_when(
      state == 0 & !seen_pos_before                              ~ 0L,
      state == 1 & future_neg                                    ~ 1L,
      state == 0 & seen_pos_before                               ~ 2L,
      state == 1 & !future_neg                                   ~ 3L,
      TRUE                                                       ~ NA_integer_
    )
  ) %>%
  dplyr::select(-seen_pos_before, -future_neg) %>%
  ungroup()
weekly_excl_pattern4 <- bind_rows(weekly_pattern_change0, weekly_pattern_change1, weekly_pattern_change23)
weekly_pattern_4 <- weekly_excl_pattern4 %>%
  group_by(indi) %>%
  arrange(test_time, age, .by_group = TRUE) %>%
  mutate(cum_p3 = cumsum(pattern == 3L)) %>%
  filter(cum_p3 > 104 & age > 208) %>%
  slice_tail(n = 16) %>%
  mutate(pattern = 4) %>%
  dplyr::select(-cum_p3)
weekly_pattern <- weekly_excl_pattern4 %>%
  rows_update(weekly_pattern_4, by = c("indi", "test_time", "age"))
anyNA(weekly_pattern)

##compare with true pattern
true_pattern_L <- true_state %>%
  mutate(pattern = case_when(
    state == "S" ~ 0L,
    state %in% c("Itrap", "Itrcp") ~ 1L,
    state %in% c("L1cp", "L2cp", "L1ap", "L2ap") ~ 2L,
    state %in% c("Ilap", "Ilcp") ~ 3L,
    state %in% c("Ihap", "Ihcp") ~ 4L,
    state %in% "nbItr" ~ 1L
  ))
anyNA(true_pattern_L)
compare_wk <- weekly_pattern %>%
  left_join(true_pattern_L %>% dplyr::select(test_time, indi, pattern) %>% rename(true = pattern), by = c("test_time", "indi")) %>%
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
com_cp_wk <- compare_wk %>%
  filter(test_time <= 52)
tab_cp_wk <- addmargins(table(com_cp_wk$true_pattern, com_cp_wk$esti_pattern))
tab_wk <- addmargins(table(compare_wk$true_pattern, compare_wk$esti_pattern))
wk_pattern_sen <- compare_wk %>%
  mutate(correct = (esti_pattern == true_pattern)) %>%
  group_by(true_pattern) %>%
  summarise(
    n = n(),
    correct_n = sum(correct),
    sensitivity = correct_n/n
  )
wk_pattern_pre <- compare_wk %>%
  mutate(correct = (esti_pattern == true_pattern)) %>%
  group_by(esti_pattern) %>%
  summarise(
    n = n(),
    correct_n = sum(correct),
    precision = correct_n/n
  )

##interval divided
weekly_interval <- weekly_pattern %>%
  group_by(indi) %>%
  arrange(test_time, age, .by_group = TRUE) %>%
  mutate(Lmid = (lag(test_time) + test_time) / 2,
         Rmid = (lead(test_time) + test_time) / 2) %>%
  mutate(
    time_age100 = birthweek + 100,
    L = if_else(row_number() == 1, case_when(
      indi %in% buyin_pop ~ min(time_age100, test_time),
      indi %in% born_pop_wk ~ birthweek,
      indi %in% ini_pop_wk ~ 0
    ), Lmid),
    R = if_else(row_number() == n(), test_time, Rmid)) %>%
  mutate(run_id = data.table::rleid(pattern)) %>%
  group_by(indi, run_id) %>%
  summarise(
    birthweek = first(birthweek),
    state = unique(state),
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
anyNA(weekly_interval)
###check any gap or overlap
gap_overlap_animals_wk <- weekly_interval %>%
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
weekly_I_state_base <- weekly_interval %>%
  mutate(time_age52 = birthweek + 52) %>%
  rowwise() %>%
  mutate(cross = (L < time_age52 & R > time_age52),   ##TRUE need to be divided
         pen = NA_character_)    
weekly_I_state <- bind_rows(
  weekly_I_state_base %>% 
    filter(cross == FALSE) %>%
    mutate(pen = if_else(age_R <= 52, "calf", "adult")),
  weekly_I_state_base %>% 
    filter(cross == TRUE) %>%
    mutate(R = time_age52,
           age_R = 52,
           pen = "calf"),
  weekly_I_state_base %>% 
    filter(cross == TRUE) %>%
    mutate(L = time_age52,
           age_L = 52,
           pen = "adult")
) %>% 
  filter(L < R) %>%     ####die after born
  mutate(L = round(L, 2),
         R = round(R, 2)) %>%
  dplyr::select(-run_id, -state, -gender, -truebv) %>%
  mutate(L = ifelse(age_L < 1, L + (1 - age_L), L),
         age_L = ifelse(age_L < 1, 1, age_L)) %>%
  filter(L < R) %>%
  dplyr::select(-cross, -birthweek, -time_age52, -age_L, -age_R)
anyNA(weekly_I_state)  

##functions to get the exposure includes the plot
weekly_I_state <- weekly_I_state %>% filter(pen == "calf") %>% dplyr::select(-pen)
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

##run for 10 farms, results are in wk_run_for_10_farms
farm_ids <- sort(unique(weekly_test$farm))
wk_run_for_10_farms <- lapply(farm_ids, function(fid) {
  message("Running exposure_table for farm ", fid, " ...")
  df <- weekly_I_state %>% filter(farm == fid)
  run_for_one_farm_L(df)
})
names(wk_run_for_10_farms) <- paste0("farm_", farm_ids)
exposure_combined_wk <- rbindlist(
  lapply(names(wk_run_for_10_farms), function(farm_name) {
    df <- wk_run_for_10_farms[[farm_name]]$exposure_df
    return(df)
  }),
  use.names = TRUE,
  fill = TRUE
)
exp_cp_wk_base <- exposure_combined_wk %>%
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
exp_cp_wk <- exp_cp_wk_base %>%
  arrange(start) %>%
  slice_head(n = 10) %>%
  mutate(start = -260,
         end = 0) %>%
  bind_rows(exp_cp_wk_base)

###binary
binary_wk <- weekly_test %>%
  # filter(indi %in% a)
  filter(!indi %in% buyin_pop) %>%
  filter(!indi %in% infected_by_mom_wk_2) %>%
  filter(!indi %in% ini_pop_wk) %>%
  group_by(indi) %>%
  arrange(test_time, age, .by_group = TRUE) %>%
  filter(max(age) > 1) %>%
  summarise(
    # result  = as.integer(any(state == 1, na.rm = TRUE)),
    result = as.integer(
      if (any(age <= 52, na.rm = TRUE)) {
        any(state == 1 & age <= 52, na.rm = TRUE)
      } else {
        any(state == 1, na.rm = TRUE)
      }
    ),
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
str(exp_cp_wk)
str(binary_wk)
binary_wk_dt <- as.data.table(binary_wk)
binary_wk_dt[, farm := as.factor(farm)]
exp_cp_wk_dt <- as.data.table(exp_cp_wk)
setkey(binary_wk_dt, farm, time_1, time_end)
setkey(exp_cp_wk_dt, farm, start,  end)
ov <- foverlaps(
  x = exp_cp_wk_dt,          
  y = binary_wk_dt,               
  by.x = c("farm","start","end"),
  by.y = c("farm","time_1","time_end"),
  type = "any",                
  nomatch = 0L
)
ov[, overlap := pmin(end, time_end) - pmax(start, time_1)]
ov <- ov[overlap > 0]
ov[, contrib := exposure * overlap]

##model data prepare
wk_offset_binary <- ov %>%
  group_by(farm, indi, result, time_1, time_end, age_time_1, age_time_end, gender, truebv, age_decay_offset) %>%
  summarise(expo_offset = log(sum(contrib) / 76),
            .groups = "drop" ) %>%
  mutate(offset = expo_offset + age_decay_offset) %>%
  mutate(
    indi   = as.factor(indi),
    gender = as.factor(gender),
    farm   = as.factor(farm)
  ) 

### compare with true
common_indi <- intersect(
  unique(true_offset_binary$indi),
  unique(wk_offset_binary$indi)
)
true_common <- true_offset_binary %>%
  filter(indi %in% common_indi)
wk_common <- wk_offset_binary %>%
  filter(indi %in% common_indi)
##expo_offset
true_small <- true_common %>% dplyr::select(indi, expo_offset_true = expo_offset)
wk_small <- wk_common %>% dplyr::select(indi, expo_offset_wk = expo_offset)
compare_offset <- true_small %>%
  inner_join(wk_small, by = "indi")
ggplot(compare_offset, aes(x = expo_offset_wk, y = expo_offset_true)) +
  geom_point(alpha = 0.6, color = "#1f77b4") +
  geom_abline(slope = 1, intercept = 0, color = "darkgreen", linetype = "dotted") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    x = "Weekly-based exposure offset",
    y = "True-state-based exposure offset",
    title = "Comparison of exposure offset"
  ) +
  theme_minimal(base_size = 13)
cor(compare_offset$expo_offset_true, compare_offset$expo_offset_wk, method = "spearman")
cor(compare_offset$expo_offset_true, compare_offset$expo_offset_wk, method = "pearson")

### compare with annually
common_indi <- intersect(
  unique(wk_offset_binary$indi),
  unique(an_offset_binary$indi)
)
wk_common <- wk_offset_binary %>%
  filter(indi %in% common_indi)
an_common <- an_offset_binary %>%
  filter(indi %in% common_indi)
##expo_offset
wk_small <- wk_common %>% dplyr::select(indi, expo_offset_wk = expo_offset)
an_small <- an_common %>% dplyr::select(indi, expo_offset_an = expo_offset)
compare_offset <- wk_small %>%
  inner_join(an_small, by = "indi")
ggplot(compare_offset, aes(x = expo_offset_an, y = expo_offset_wk)) +
  geom_point(alpha = 0.6, color = "#1f77b4") +
  geom_abline(slope = 1, intercept = 0, color = "darkgreen", linetype = "dotted") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    x = "Annually-based expo_offset",
    y = "Weekly-based expo_offset",
    title = "Comparison of expo_offset (weekly vs annually)"
  ) +
  theme_minimal(base_size = 13)
cor(compare_offset$expo_offset_wk, compare_offset$expo_offset_an, method = "spearman")
cor(compare_offset$expo_offset_wk, compare_offset$expo_offset_an, method = "pearson")
##age_offset
wk_small <- wk_common %>% dplyr::select(indi, age_decay_offset_wk = age_decay_offset)
an_small <- an_common %>% dplyr::select(indi, age_decay_offset_an = age_decay_offset)
compare_offset <- wk_small %>%
  inner_join(an_small, by = "indi")
ggplot(compare_offset, aes(x = age_decay_offset_an, y = age_decay_offset_wk)) +
  geom_point(alpha = 0.6, color = "#1f77b4") +
  geom_abline(slope = 1, intercept = 0, color = "darkgreen", linetype = "dotted") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    x = "Annually-based age_decay_offset",
    y = "Weekly-based age_decay_offset",
    title = "Comparison of age_decay_offset (weekly vs annually)"
  ) +
  theme_minimal(base_size = 13)
cor(compare_offset$age_decay_offset_wk, compare_offset$age_decay_offset_an, method = "spearman")
cor(compare_offset$age_decay_offset_wk, compare_offset$age_decay_offset_an, method = "pearson")
##result
wk_small <- wk_common %>% dplyr::select(indi, result_wk = result)
an_small <- an_common %>% dplyr::select(indi, result_an = result)
compare_offset <- wk_small %>%
  inner_join(an_small, by = "indi")

# false_neg <- wk_small %>%
#   inner_join(an_small, by = "indi") %>%
#   mutate(diff = result_wk - result_an) %>%
#   filter(diff == 0) %>%
#   pull(indi) %>%
#   unique()

tab <- compare_offset %>%
  count(result_an, result_wk) %>%
  complete(result_an = 0:1, result_wk = 0:1, fill = list(n = 0))
ggplot(tab, aes(x = factor(result_an), y = factor(result_wk), fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), color = "black", size = 5) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    x = "Annually-based result",
    y = "Weekly-based result",
    title = "Confusion Matrix: Annually vs Weekly result"
  ) +
  theme_minimal(base_size = 14)
cor(compare_offset$result_wk, compare_offset$result_an, method = "spearman")
cor(compare_offset$result_wk, compare_offset$result_an, method = "pearson")



##asreml
cloglog_weekly <- asreml(
  fixed    = result ~ 1 + offset(offset),
  random   = ~ vm(indi, Ainv_full) + farm,
  data     = wk_offset_binary,
  family   = asr_binomial(link = "cloglog"),
  workspace = 2e7,
  maxit    = 50
)
wald(cloglog_weekly)
summary(cloglog_weekly)

esti_bv_wk <- as.data.frame(cloglog_weekly$coefficients$random) %>%
  rownames_to_column("indi") %>%
  as_tibble() %>%
  mutate(
    indi = str_remove(indi, "^vm\\(indi, Ainv_full\\)_") 
  )
sire_com_wk <- esti_bv_wk %>%
  mutate(indi = as.character(indi)) %>%
  filter(indi %in% as.character(sire)) %>%
  rename(esti_bv = effect) %>%
  left_join(
    sirebv %>% mutate(indi = as.character(indi)),
    by = "indi"
  )
ggplot(sire_com_wk, aes(x = esti_bv, y = true_bv)) +
  geom_point(alpha = 0.6, color = "#1f77b4") +
  geom_abline(slope = 1, intercept = 0, color = "darkgreen", linetype = "dotted")+
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(
    x = "Estimated sire breeding value",
    y = "True sire breeding value",
    title = "Comparison between true and estimated sire breeding values"
  ) +
  theme_minimal(base_size = 13)
cor.test(sire_com_wk$true_bv, sire_com_wk$esti_bv)
####true bv
compare_bv_wk <- wk_offset_binary %>%
  dplyr::select(indi, result, truebv) %>%
  left_join(esti_bv_wk,  by = "indi") %>%
  rename(	
    true_bv = truebv,
    esti_bv = effect)
anyNA(compare_bv_wk)
ggplot(compare_bv_wk, aes(x = esti_bv, y = true_bv)) +
  geom_point(alpha = 0.6, color = "#1f77b4") +
  geom_abline(slope = 1, intercept = 0, color = "darkgreen", linetype = "dotted")+
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(
    x = "Estimated breeding value",
    y = "True breeding value",
    title = "Comparison between true and estimated breeding values"
  ) +
  theme_minimal(base_size = 13)
cor.test(compare_bv_wk$true_bv, compare_bv_wk$esti_bv, method = "pearson")
cor(compare_bv_wk$result, compare_bv_wk$true_bv, method = "pearson")
lm(true_bv ~ esti_bv, data = compare_bv_wk)
ggplot(compare_bv_wk, aes(x = true_bv, fill = factor(result))) +
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




