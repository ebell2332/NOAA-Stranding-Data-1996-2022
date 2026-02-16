grid_str <- read_excel("/Users/ebell23/Downloads/grid25_strand_pol_all.xlsx")
colnames(grid_str)

#load all packages needed for hurdle estimation and negative binomial model
pkgs <- c(
  "readxl", "dplyr", "tidyr", "stringr",
  "ggplot2", "patchwork",
  "MASS",          # glm.nb
  #"countreg",      # zerotrunc (zero-truncated Poisson) - no longer in new R version
  "pscl",           #hurdle and poison model
  "clubSandwich",  # cluster-robust vcov (CR2)
  "lmtest",        # coeftest
  "broom",          # tidy()
  "broom.mixed",
  "glmmTMB",
  "sandwich"
)
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install)
lapply(pkgs, library, character.only = TRUE)

####Fix and mutate variables and NAs####
grid_str <- grid_str %>%
  dplyr::rename(year = Year_num) %>%
  dplyr::rename(
    direct   = DirectFlag,
    indirect = IndirectFlag
  )

grid_str <- grid_str %>%
  replace_na(list(direct = 0, indirect = 0, cat_overlap = 0, PolCat_count = 0)) #replace all NA with 0s

unique(grid_str$direct) #make sure values are 0 and 1
unique(grid_str$indirect) #make sure values are 0 and 1
unique(grid_str$cat_overlap)
unique(grid_str$PolCat_count)

#Create derived variables
grid_str <- grid_str %>%
  mutate(coastal = as.integer(dist_km <= 5), #create integer for grid cells that are less than 5km from the coast as 0 and 1
         any_strand = as.integer(n_strandings > 0), #create stranding numbers as 0 and 1 for when stranding occurs
        latregion  = dplyr::ntile(grid_lat, 5),  # cut(grid_lat), group(5)
        year = factor(year)
        )

summary_stat <- grid_str %>%
  dplyr::select(n_strandings, indirect, direct, cat_overlap, coastal)
print(summary(summary_stat)) #summary 


####Global Variables####
controls_hurdle <- "factor(year)" #for the hurdle estimation
controls_nb     <- "factor(year) + coastal + factor(latregion)" #negative binomiale estimation
cluster_var     <- "grid_id" #cluster variable

polvars         <- c("direct", "indirect") #policy target variables
polvars_factor  <- "factor(cat_overlap)" #policy category variable factors

grid_str <- grid_str %>%
  dplyr::select(year, grid_id, n_strandings,
    dplyr::all_of(polvars),
    cat_overlap, PolCat_count, PolCat_List,
    dist_km, grid_lat, grid_lon, coastal, any_strand, latregion
  )


# 4) Helper functions: clustered vcov + tidy with robust SE
# ----------------------------
vcov_cluster_CR2 <- function(model, cluster_vec) {
  clubSandwich::vcovCR(model, cluster = cluster_vec, type = "CR2")
}

tidy_cluster <- function(model, cluster_vec, conf_level = 0.95) {
  V <- vcov_cluster_CR2(model, cluster_vec)
  ct <- lmtest::coeftest(model, vcov. = V)
  out <- broom::tidy(ct) |>
    dplyr::rename(term = term, estimate = estimate, std.error = std.error, statistic = statistic, p.value = p.value)
  
  # Wald CI
  alpha <- 1 - conf_level
  z <- qnorm(1 - alpha / 2)
  out |>
    dplyr::mutate(
      conf.low  = estimate - z * std.error,
      conf.high = estimate + z * std.error
    )
}

# Prediction grid helper (Stata margins at(pol=(0 1)))
make_at_grid <- function(grid_str, focal_var, at_vals = c(0, 1)) {
  # holds other vars at typical values:
  # - factors at most common level
  # - numeric at median
  base <- grid_str |>
    dplyr::summarise(
      year      = stats::median(year, na.rm = TRUE),
      coastal   = as.integer(stats::median(coastal, na.rm = TRUE) >= 0.5),
      latregion = stats::median(latregion, na.rm = TRUE),
      direct    = as.integer(stats::median(direct, na.rm = TRUE) >= 0.5),
      indirect  = as.integer(stats::median(indirect, na.rm = TRUE) >= 0.5),
      cat_overlap = stats::median(cat_overlap, na.rm = TRUE)
    )
  grid <- base[rep(1, length(at_vals)), , drop = FALSE]
  grid[[focal_var]] <- at_vals
  grid
}

# Robust prediction SE via delta method (approx):
#  - For GLM logit: predict on link scale -> se.fit -> transform to prob
#  - For count models: predict on link scale -> transform to mean
predict_with_cluster_se <- function(model, newdata, cluster_vec, type = c("response", "link")) {
  type <- match.arg(type)
  V <- vcov_cluster_CR2(model, cluster_vec)
  X <- stats::model.matrix(stats::terms(model), newdata)
  
  eta <- as.numeric(X %*% stats::coef(model))
  se_eta <- sqrt(diag(X %*% V %*% t(X)))
  
  if (type == "link") {
    return(dplyr::tibble(fit = eta, se = se_eta))
  }
  
  # response scale depends on model family/link
  if (inherits(model, "glm") && model$family$family == "binomial") {
    mu <- stats::plogis(eta)
    # transform CI through link
    lo <- stats::plogis(eta - 1.96 * se_eta)
    hi <- stats::plogis(eta + 1.96 * se_eta)
    return(dplyr::tibble(fit = mu, conf.low = lo, conf.high = hi))
  } else {
    # log link assumed for count models here
    mu <- exp(eta)
    lo <- exp(eta - 1.96 * se_eta)
    hi <- exp(eta + 1.96 * se_eta)
    return(dplyr::tibble(fit = mu, conf.low = lo, conf.high = hi))
  }
}

# ----- HURDLE MODEL (Direct/Indirect): Stage 1 logit -----
form_hurdle1_di <- as.formula(
  paste("any_strand ~", paste(polvars, collapse = " + "), "+", controls_hurdle)
)
hurdle1_di <- glm(form_hurdle1_di, data = grid_str, family = binomial())

# ----- HURDLE MODEL (Direct/Indirect): Stage 2 zero-truncated Poisson -----
# tpoisson ... if n_strandings > 0, ll(0)
str_pos <- grid_str |> dplyr::filter(n_strandings > 0) 

#form_hurdle2_di <- as.formula(
  #paste("n_strandings ~", paste(polvars, collapse = " + "), "+", controls_hurdle)
#)
#hurdle2_di <- pscl::zeroinfl(form_hurdle2_di, data = str_pos, dist = "poisson") did'nt run becuase the lowest number was not 0

hurdle2_di <- pscl::zeroinfl(n_strandings~ direct + indirect + factor(year) | direct + indirect + factor(year), data = grid_str, dist = "poisson") #depends on year and policy attributes

#----Hurdle Model (policy target): Uses hurdle() and combies logit and tpoisson
hurdle_di <- pscl::hurdle(n_strandings~ direct + indirect + factor(year), data = grid_str, dist = "poisson", zero.dist = "binomial")
summary(hurdle_di)


# ----- HURDLE MODEL (Policy overlap): Stage 1 -----
form_hurdle1_ov <- as.formula(
  paste("any_strand ~", polvars_factor, "+", controls_hurdle)
)
hurdle1_overlap <- glm(form_hurdle1_ov, data = grid_str, family = binomial())

# ----- HURDLE MODEL (Policy overlap): Stage 2 -----
form_hurdle2_ov <- as.formula(
  paste("n_strandings ~", polvars_factor, "+", controls_hurdle)
)
hurdle2_overlap <- pscl::zeroinfl(form_hurdle2_ov, data = grid_str, dist = "poisson")

hurdle_di2 <- pscl::hurdle(n_strandings ~ factor(cat_overlap) + factor(year), data = grid_str, dist = "poisson", zero.dist = "binomial")
summary(hurdle_di2)

# ----- NEGATIVE BINOMIAL with geographic controls -----
form_nb_di <- as.formula(
  paste("n_strandings ~", paste(polvars, collapse = " + "), "+", controls_nb) #policy target
)
nb_di <- MASS::glm.nb(form_nb_di, data = grid_str)

form_nb_ov <- as.formula(
  paste("n_strandings ~", polvars_factor, "+", controls_nb) #policy overlap
)
nb_overlap <- MASS::glm.nb(form_nb_ov, data = grid_str)

# 6) COEFFICIENT PLOTS (ggplot equivalent to coefplot)

# Hurdle coef plot (direct/indirect)
  #-------------
  # Extract the two components as separate "models"
 sum_di <- summary(hurdle_di)
   count_di <- as.data.frame(sum_di$coefficients$count)   # intensive margin (truncated count model)
  count_di$term <- row.names(count_di)
 count_di$margin <- "Intensive"
  
  zero_di <- as.data.frame(sum_di$coefficients$zero)  # in==extensive margin (logit for zero vs >0)
  zero_di$term <- row.names(zero_di)
  zero_di$margin <- "Extensive"
  
  di_df <- bind_rows(count_di, zero_di) %>%
    rename(
      estimate = Estimate,
      std.error = `Std. Error`
    ) %>%
    mutate(
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error
    ) %>%
    filter(term %in% c("direct", "indirect")) %>%
    mutate(term = recode(term,
                         direct = "Direct policy",
                         indirect = "Indirect policy"))
  
  
  p_coef_di <- ggplot(di_df, aes(x = estimate, y = term, color = margin)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   position = position_dodge(width = 0.5), height = 0.2) +
    labs(
      title = "Policy associations with strandings",
      subtitle = "Hurdle model: Extensive (logit) vs Intensive (truncated Poisson)",
      x = "Coefficient (log odds / log count)",
      y = NULL,
      shape = NULL
    ) +
    theme_minimal()
p_coef_di

#Hurdle coef plot (overlap count)
 #-----------
sum_ov <- summary(hurdle_di2)

count_ov <- as.data.frame(sum_ov$coefficients$count)
count_ov$term <- rownames(count_ov)
count_ov$margin <- "Intensive (count part)"

zero_ov <- as.data.frame(sum_ov$coefficients$zero)
zero_ov$term <- rownames(zero_ov)
zero_ov$margin <- "Extensive (zero part)"

ov_df <- bind_rows(count_ov, zero_ov) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`
  ) %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  ) %>%
  # keep the overlap dummy terms (baseline is 0)
  filter(term %in% c("factor(cat_overlap)1", "factor(cat_overlap)2")) %>%
  mutate(term = recode(term,
                       `factor(cat_overlap)1` = "1 policy type",
                       `factor(cat_overlap)2` = "2+ policy types"))

p_coef_ov <- ggplot(ov_df, aes(x = estimate, y = term, color = margin)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.5), height = 0.2) +
  labs(
    title = "Policy overlap associations with strandings",
    subtitle = "Reference group: 0 policy types",
    x = "Coefficient (log odds / log count)",
    y = NULL,
    shape = NULL
  ) +
  theme_minimal()

p_coef_ov


# 7) MARGINS-STYLE PLOTS (Policy overlap)
# ----------------------------

# Extensive margin: predicted Pr(any_strand) by cat_overlap
new_ov <- data.frame(
  cat_overlap = c(0, 1, 2),
  year = stats::median(grid_str$year, na.rm = TRUE)
)
# add factor(year) consistency: year used then factor() in formula
pred_ext <- predict_with_cluster_se(
  hurdle1_overlap,
  newdata = new_ov,
  cluster_vec = grid_str[[cluster_var]],
  type = "response"
) |>
  dplyr::bind_cols(new_ov)

p_ext <- pred_ext |>
  ggplot(aes(x = cat_overlap, y = fit)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  scale_x_continuous(breaks = c(0, 1, 2), labels = c("None", "One", "Two+")) +
  labs(x = "Number of policy types", y = "Predicted Pr(any stranding)", title = "Extensive margin") +
  theme_minimal() 
  
# Intensive margin: predicted count given >0 by cat_overlap (zero-truncated Poisson)
pred_int <- predict_with_cluster_se(
  hurdle2_overlap,
  newdata = new_ov,
  cluster_vec = str_pos[[cluster_var]],
  type = "response"
) |>
  dplyr::bind_cols(new_ov)

p_int <- pred_int |>
  ggplot(aes(x = cat_overlap, y = fit)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  scale_x_continuous(breaks = c(0, 1, 2), labels = c("None", "One", "Two+")) +
  labs(x = "Number of policy types", y = "Predicted count (given stranding occurs)", title = "Intensive margin") +
  theme_minimal()

p_margins_overlap <- p_ext + p_int + patchwork::plot_layout(ncol = 2) +
  plot_annotation(
    title = "Policy overlap and strandings",
    caption = "Reference: 0 policy types"
  )
print(p_margins_overlap)


# 8) MODEL COMPARISON: 2x3 GRID (direct vs indirect; 3 specs)
# ----------------------------
make_panel <- function(pol) {
  poltitle <- stringr::str_to_title(pol)
  
  # Stage 1: logit Pr(any)
  m1 <- hurdle1_di  # includes both direct+indirect already, like your Stata loop
  new_pol <- make_at_grid(grid_str, focal_var = pol, at_vals = c(0, 1))
  pr1 <- predict_with_cluster_se(m1, new_pol, grid_str[[cluster_var]], type = "response") |>
    dplyr::bind_cols(new_pol) |>
    dplyr::mutate(level = factor(.data[[pol]], levels = c(0, 1), labels = c("Absent", "Present")))
  
  p1 <- ggplot(pr1, aes(x = level, y = fit)) +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
    labs(
      x = paste0(poltitle, " policy"),
      y = "Probability",
      title = "Extensive Margin (Logit)",
      subtitle = "Pr(any stranding)"
    ) +
    theme_minimal()
  
  # Stage 2: truncated Poisson, mean count given >0
  m2 <- hurdle2_di
  pr2 <- predict_with_cluster_se(m2, new_pol, str_pos[[cluster_var]], type = "response") |>
    dplyr::bind_cols(new_pol) |>
    dplyr::mutate(level = factor(.data[[pol]], levels = c(0, 1), labels = c("Absent", "Present")))
  
  p2 <- ggplot(pr2, aes(x = level, y = fit)) +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
    labs(
      x = paste0(poltitle, " policy"),
      y = "Count",
      title = "Intensive Margin (Poisson)",
      subtitle = "Count (given >0)"
    ) +
    theme_minimal()
  
  # NB: all obs, geo controls
  m3 <- nb_di
  pr3 <- predict_with_cluster_se(m3, new_pol, grid_str[[cluster_var]], type = "response") |>
    dplyr::bind_cols(new_pol) |>
    dplyr::mutate(level = factor(.data[[pol]], levels = c(0, 1), labels = c("Absent", "Present")))
  
  p3 <- ggplot(pr3, aes(x = level, y = fit)) +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
    labs(
      x = paste0(poltitle, " policy"),
      y = "Count",
      title = "Negative Binomial Model",
      subtitle = "Count (all obs)"
    ) +
    theme_minimal()
  
  list(p1 = p1, p2 = p2, p3 = p3)
}

pan_direct   <- make_panel("direct")
pan_indirect <- make_panel("indirect")

p_2x3 <- (pan_direct$p1 + pan_direct$p2 + pan_direct$p3) /
  (pan_indirect$p1 + pan_indirect$p2 + pan_indirect$p3) +
  plot_annotation(
    title = "Policy associations: Robustness across specifications",
    subtitle = "Top row: Direct policies  |  Bottom row: Indirect policies",
    caption = "All models include year fixed effects. Negative Binomial also includes latregion + coastal (≤5km)."
  )
print(p_2x3)


# 9) MODEL COMPARISON: COEFFICIENT PLOT (DI terms across 3 models)
# ----------------------------
plot_coef_models(
  models = list(hurdle1_di, hurdle2_di, nb_di),
  model_names = c("Hurdle: Extensive", "Hurdle: Intensive", "NB: Geo controls"),
  keep_terms_regex = "^(direct|indirect)$",
  cluster_vec = grid_str[[cluster_var]],
  title = "Policy associations: Model comparison",
  subtitle = "Hurdle Stage 1: log odds; Stage 2 & NB: log counts",
  relabel = relabel_di
)

h_di <- pscl::hurdle(
  n_strandings ~ direct + indirect + year | direct + indirect + year ,
  data = grid_str,
  dist = "poisson",
  zero.dist = "binomial"
)

grid_str <- grid_str %>% mutate(cat_overlap = factor(cat_overlap, levels = c(0,1,2)))

h_ov <- pscl::hurdle(
  n_strandings ~ cat_overlap + year | cat_overlap + year,
  data = grid_str,
  dist = "poisson",
  zero.dist = "binomial"
)

#Predict Direct/Indirect----
  
  get_di_manual <- function(h, df, pol) {
    nd <- data.frame(
      direct = 0L,
      indirect = 0L,
      year = factor(levels(df$year)[1], levels = levels(df$year))
    )
    nd <- nd[rep(1, 2), , drop = FALSE]
    nd[[pol]] <- c(0L, 1L)
    
    s <- summary(h)
    b_zero  <- s$coefficients$zero[, "Estimate"]
    b_count <- s$coefficients$count[, "Estimate"]
    
    X_zero  <- model.matrix(~ direct + indirect + year, data = nd)
    X_count <- model.matrix(~ direct + indirect + year, data = nd)
    
    p0 <- plogis(as.numeric(X_zero %*% b_zero))
    pr_any <- 1 - p0
    
    lambda <- exp(as.numeric(X_count %*% b_count))
    mean_pos <- lambda / (1 - exp(-lambda))
    mean_all <- pr_any * mean_pos
    
    tibble(
      policy = pol,
      level = factor(nd[[pol]], levels = c(0,1), labels = c("Absent","Present")),
      pr_any = pr_any,
      mean_pos = mean_pos,
      mean_all = mean_all
    )
  }
  
  marg_di <- bind_rows(
    get_di_manual(h_di, grid_str, "direct"),
    get_di_manual(h_di, grid_str, "indirect")
  )
  
  marg_di
  
  
#Predict Overlap (0,1,2+)
  nd_ov <- data.frame(
    cat_overlap = factor(c(0, 1, 2), levels = levels(grid_str$cat_overlap)),
    year = factor(levels(grid_str$year)[1], levels = levels(grid_str$year))
  )
  
  # ---- Manual prediction ----
  s <- summary(h_ov)
  
  b_zero  <- s$coefficients$zero[, "Estimate"]
  b_count <- s$coefficients$count[, "Estimate"]
  
  X_zero  <- model.matrix(~ cat_overlap + year, data = nd_ov)
  X_count <- model.matrix(~ cat_overlap + year, data = nd_ov)
  
  eta0 <- as.numeric(X_zero %*% b_zero)
  p0   <- plogis(eta0)                      # Pr(Y=0)
  pr_any <- 1 - p0
  
  eta_c <- as.numeric(X_count %*% b_count)
  lambda <- exp(eta_c)                      # Poisson rate parameter
  
  mean_pos <- lambda / (1 - exp(-lambda))   # E[Y | Y>0] for Poisson
  mean_all <- pr_any * mean_pos             # unconditional mean
  
  marg_ov <- tibble(
    overlap  = factor(c("None","One","Two+"), levels = c("None","One","Two+")),
    pr_any   = pr_any,
    mean_pos = mean_pos,
    mean_all = mean_all
  )
  
  marg_ov  
  
 
  
#Predict Negative Binomial with Geographic Controls
  nb_di <- MASS::glm.nb(
    n_strandings ~ direct + indirect + year + coastal + factor(latregion),
    data = grid_str
  )
  
  # ----------------------------
  # 4) NB predicted means for direct/indirect (Absent vs Present)
  # ----------------------------
  nb_di <- MASS::glm.nb(
    n_strandings ~ direct + indirect + year + coastal + factor(latregion),
    data = grid_str
  )
  
   get_nb_binary <- function(m, df, pol) {
    
    # hold year at first level; other policy at 0; hold geo controls at typical values
    nd <- data.frame(
      direct   = 0L,
      indirect = 0L,
      year     = factor(levels(df$year)[1], levels = levels(df$year)),
      coastal  = as.integer(stats::median(df$coastal, na.rm = TRUE) >= 0.5),
      latregion = stats::median(df$latregion, na.rm = TRUE)
    )
    nd <- nd[rep(1, 2), , drop = FALSE]
    nd[[pol]] <- c(0L, 1L)
    
    mu <- predict(m, newdata = nd, type = "response")
    
    tibble(
      policy = pol,
      level  = factor(nd[[pol]], levels = c(0, 1), labels = c("Absent", "Present")),
      nb_mean = as.numeric(mu)
    )
  }
  
  marg_nb <- bind_rows(
    get_nb_binary(nb_di, grid_str, "direct"),
    get_nb_binary(nb_di, grid_str, "indirect")
  )  

  # 5) Panel plot (2x3) like your Stata comparison figure
  # ----------------------------
  plot_col <- function(dat, y, title, xlab, ylab) {
    ggplot(dat, aes(x = level, y = .data[[y]])) +
      geom_point(size = 2) +
      geom_line(aes(group = 1)) +
      labs(title = title, x = xlab, y = ylab) +
      theme_minimal()
  }
  
  # Direct row
  p_ext_direct <- plot_col(
    marg_di %>% filter(policy == "direct"),
    y = "pr_any",
    title = "Extensive Margin (Hurdle)",
    xlab = "Direct policy",
    ylab = "Pr(any stranding)"
  )
  
  p_int_direct <- plot_col(
    marg_di %>% filter(policy == "direct"),
    y = "mean_pos",
    title = "Intensive Margin (Hurdle)",
    xlab = "Direct policy",
    ylab = "E[count | >0]"
  )
  
  p_nb_direct <- plot_col(
    marg_nb %>% filter(policy == "direct"),
    y = "nb_mean",
    title = "Negative Binomial Model",
    xlab = "Direct policy",
    ylab = "E[count]"
  )
  
  # Indirect row
  p_ext_ind <- plot_col(
    marg_di %>% filter(policy == "indirect"),
    y = "pr_any",
    title = "Extensive Margin (Hurdle)",
    xlab = "Indirect policy",
    ylab = "Pr(any stranding)"
  )
  
  p_int_ind <- plot_col(
    marg_di %>% filter(policy == "indirect"),
    y = "mean_pos",
    title = "Intensive Margin (Hurdle)",
    xlab = "Indirect policy",
    ylab = "E[count | >0]"
  )
  
  p_nb_ind <- plot_col(
    marg_nb %>% filter(policy == "indirect"),
    y = "nb_mean",
    title = "Negative Binomial Model",
    xlab = "Indirect policy",
    ylab = "E[count]"
  )
  
library(patchwork)  
  p_2x3 <- (p_ext_direct + p_int_direct + p_nb_direct) /
    (p_ext_ind    + p_int_ind    + p_nb_ind) +
    plot_annotation(
      title = "Policy associations: Robustness across specifications",
      subtitle = "Top row: Direct policies  |  Bottom row: Indirect policies",
      caption = "Hurdle: Pr(any) from zero part; Intensive is truncated Poisson mean. NB includes year FE + coastal + latregion."
    )
  
  p_2x3 
  


  
  
#Temporal Direct/Indirect Predictions----
  predict_by_year_di <- function(h, df) {
    
    yrs <- levels(df$year)
    
    # make a prediction grid:
    # policy = direct/indirect
    # level  = Absent/Present
    # year   = each year FE level
    nd <- tidyr::expand_grid(
      policy = c("direct", "indirect"),
      level  = c("Absent", "Present"),
      year   = factor(yrs, levels = yrs)
    ) %>%
      mutate(
        direct   = 0L,
        indirect = 0L
      ) %>%
      mutate(
        direct   = ifelse(policy == "direct"   & level == "Present", 1L, direct),
        indirect = ifelse(policy == "indirect" & level == "Present", 1L, indirect)
      )
    
    # coefficients
    s <- summary(h)
    b0 <- s$coefficients$zero[, "Estimate"]
    bc <- s$coefficients$count[, "Estimate"]
    
    # model matrices (must match the formulas used in h_di)
    X0 <- model.matrix(~ direct + indirect + year, data = nd)
    Xc <- model.matrix(~ direct + indirect + year, data = nd)
    
    # extensive margin
    p0 <- plogis(as.numeric(X0 %*% b0))     # Pr(Y=0)
    pr_any <- 1 - p0
    
    # intensive margin (truncated Poisson)
    lambda <- exp(as.numeric(Xc %*% bc))    # Poisson rate
    mean_pos <- lambda / (1 - exp(-lambda)) # E[Y|Y>0]
    
    nd %>%
      mutate(
        pr_any = pr_any,
        mean_pos = mean_pos
      )
  }
  
  pred_di_year <- predict_by_year_di(h_di, grid_str)
  head(pred_di_year)  
  
  #extensive 
  p_ext_year <- ggplot(pred_di_year, aes(x = year, y = pr_any, group = level, color = level)) +
    geom_line() +
    geom_point(size = 1.2) +
    facet_wrap(~ policy, ncol = 1, scales = "free_y") +
    labs(
      title = "Extensive margin over time",
      subtitle = "Predicted Pr(any stranding) by year; Absent vs Present",
      x = "Year",
      y = "Predicted Pr(any)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  p_ext_year 
  
  #intensive
  p_int_year <- ggplot(pred_di_year, aes(x = year, y = mean_pos, group = level, color = level)) +
    geom_line() +
    geom_point(size = 1.2) +
    facet_wrap(~ policy, ncol = 1, scales = "free_y") +
    labs(
      title = "Intensive margin over time",
      subtitle = "Predicted E[count | stranding occurs] by year; Absent vs Present",
      x = "Year",
      y = "Predicted E[count | >0]"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  p_int_year
  
  
#Map grid-specific policy effects (change in predcited(any) and chnage in expected count for direct/indirect)  
  
  
  
  #With/without Grid FE Model and Plots----
    #For Direct/Indirect
  grid_str_pos <- grid_str %>%
    filter(n_strandings >0) %>%
    mutate(grid_id = factor(grid_id),
           year = factor(year))

  
  library(glmmTMB)
  #With Grid FE
grid_fe <- glmmTMB(n_strandings ~ direct + indirect + grid_id + year, family = truncated_poisson(link = "log"), data = grid_str_pos) #zero-truncated poisson with grids as FE

coef_df <- broom.mixed::tidy(grid_fe, conf.int = TRUE) %>%
  filter(term %in% c("direct", "indirect")) %>%
  mutate(term = recode(term,
                       direct = "Direct policy present",
                       indirect = "Indirect policy present"))

coef_df_plot <- coef_df %>% #flips the estimates to be negative to show potential effectiveness
  mutate(
    estimate_plot = -estimate,
    conf.low_plot = -conf.high,
    conf.high_plot = -conf.low
  )

  #Plotting within grid (Grid FE)
ggplot(coef_df_plot, aes(x = estimate_plot, y = term, color = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.8) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low_plot, xmax = conf.high_plot),
                 height = 0.2, linewidth = 1) +
  scale_color_manual(values = c("Direct policy present" = "steelblue", 
                                "Indirect policy present" = "indianred3")) +
  scale_y_discrete(limits = rev) + # reverse order on y-axis
  labs(
    title = "Within-grid associations (Grid FE), zero-truncated Poisson",
    x = "Effect on stranding counts (conditional on > 0)",
    y = NULL,
    color = "Policy Target Presence"
  ) +
  theme(
    legend.position = "bottom", 
    legend.text = element_text(color = "black", size = 12),
    plot.title = element_text(color = "black", size = 15),      # Title color and size
    axis.title.x = element_text(color = "black", size = 14),   # X-axis title
    axis.text = element_text(size = 12, color = "black"),  # Axis tick labels
  ) 

  #Cluster standard error (SE) by Grid----
grid_df_pos <- grid_str %>%
  filter(n_strandings >0) %>%
  mutate(grid_id = factor(grid_id),
         year = factor(year))

library(purrr)

grid_fe_se <- glmmTMB(n_strandings ~ direct + indirect + grid_id + year, family = truncated_poisson(link = "log"), data = grid_df_pos) #zero-truncated poisson with grids as FE


#Bootstrap Method - estimates SE and CI
fixef(grid_fe_se)$cond[c("direct", "indirect")]

boot_cluster <- function(data, B = 300, seed = 1) {
  set.seed(seed)
  grids <- levels(data$grid_id)
  G <- length(grids)
  
  one_b <- function(b) {
    samp <- sample(grids, size = G, replace = TRUE)
    
    d_b <- map_dfr(seq_along(samp), function(i) {
      g <- samp[i]
      tmp <- data %>% filter(grid_id == g)
      # rename FE so duplicated sampled grids stay identifiable
      tmp$grid_id <- factor(paste0(as.character(g), "_b", i))
      tmp
    })
    
    fit_b <- glmmTMB(
      n_strandings ~ direct + indirect + grid_id + year,
      family = truncated_poisson(link="log"),
      data = d_b
    )
    
    bb <- fixef(fit_b)$cond[c("direct","indirect")]
    tibble(b = b, direct = unname(bb["direct"]), indirect = unname(bb["indirect"]))
  }
  
  map_dfr(1:B, one_b)
}

boot_draws <- boot_cluster(grid_df_pos, B = 300, seed = 1)


b_hat <- fixef(grid_fe_se)$cond[c("direct","indirect")]

summ_term <- function(draws, term, bhat) {
  v <- draws[[term]]
  se <- sd(v, na.rm = TRUE)
  beta <- unname(bhat)
  z <- beta / se
  tibble(
    term = term,
    beta = beta,
    SE   = se,
    z    = z,
    p    = 2 * pnorm(abs(z), lower.tail = FALSE),
    conf.low  = beta - 1.96 * se,
    conf.high = beta + 1.96 * se
  )
}

out <- bind_rows(
  summ_term(boot_draws, "direct",   b_hat["direct"]),
  summ_term(boot_draws, "indirect", b_hat["indirect"])
)

out

    #plotting
plot_di <- out %>%
  mutate(
    term = recode(term,
                  direct = "Direct policy present",
                  indirect = "Indirect policy present"))

ggplot(plot_di, aes(y = term, color = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.8) +
  geom_segment(aes(x = conf.low, xend = conf.high, yend = term), linewidth = 1.2) +
  geom_point(aes(x = beta), size = 3) +
  coord_cartesian(xlim = c(min(plot_di$conf.low), 0)) +
  scale_color_manual(values = c(
    "Direct policy present"   = "steelblue",
    "Indirect policy present" = "indianred3"
  )) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  labs(
    title = "Within-grid associations by policy target",
    x = "Coefficient (sign-flipped), clustered by grid (cluster bootstrap)",
    y = NULL
  )


#Alternative method----works best in replicating stata ----
library(sandwich)
library(lmtest)

# Filter to positive counts
grid_df_pos <- grid_str %>%
  filter(n_strandings >0) %>%
  mutate(grid_id = factor(grid_id),
         year = factor(year))

grid_df_positive <- grid_df_pos[grid_df_pos$n_strandings > 0, ]

# Regular Poisson (note: this is NOT exactly the same, just an approximation)
model <- glm(n_strandings ~ direct + indirect + factor(grid_id) + factor(year),
             family = poisson(link = "log"),
             data = grid_df_positive)

# Cluster-robust SEs
mf <- model.frame(model)
vcov_cluster <- vcovCL(model, cluster = mf$grid_id, type = "HC0")
ct <- coeftest(model, vcov = vcov_cluster)

ct_df <- data.frame(
  term = rownames(ct),
  estimate = ct[, "Estimate"],
  se = ct[, "Std. Error"],
  z = ct[, "z value"],
  p = ct[, "Pr(>|z|)"],
  row.names = NULL)

ct_df <- ct_df[ct_df$term %in% c("direct", "indirect"), ]

# Stata-style 95% CI (normal)
ct_df$conf.low  <- ct_df$estimate - 1.96 * ct_df$se
ct_df$conf.high <- ct_df$estimate + 1.96 * ct_df$se

# relabel + sign flip (to show on left like your figure)
ct_df$term_lab <- ifelse(ct_df$term == "direct",
                         "Direct policy present",
                         "Indirect policy present")



pol_targ_se <- ggplot(ct_df, aes(y = term_lab, color = term_lab)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.8) +
  geom_segment(aes(x = conf.low, xend = conf.high, yend = term_lab), linewidth = 1.2) +
  geom_point(aes(x = estimate), size = 3) +
  scale_y_discrete(limits = rev) + # reverse order on y-axis
  coord_cartesian(xlim = c(min(ct_df$conf.low), 0)) +
  scale_color_manual(values = c(
    "Direct policy present"   = "steelblue",
    "Indirect policy present" = "indianred3"
  )) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", axis.text.y = element_text(color = "black", size = 14)) +
  labs(
    title = "Within grid associations by active policy targets and categories",
    x = "Coefficient (sign-flipped), clustered by grid",
    y = NULL
  )


  #without grid FE----
nogrid_fe <- glmmTMB(n_strandings ~ direct + indirect, family = truncated_poisson, data = grid_str_pos) #zero-truncaeted poisson without grid as FE

coef_df1 <- broom.mixed::tidy(nogrid_fe, conf.int = TRUE) %>%
  filter(term %in% c("direct", "indirect")) %>%
  mutate(term = recode(term,
                       direct = "Direct policy present",
                       indirect = "Indirect policy present"))



ggplot(coef_df1, aes(x = estimate, y = term, color = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.8) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2, linewidth = 1) +
  scale_color_manual(values = c("Direct policy present" = "steelblue", 
                      "Indirect policy present" = "indianred3")) +
  scale_y_discrete(limits = rev) + # reverse order on y-axis
  labs(
    title = "Withinout-grid associations (Non-Grid FE), zero-truncated Poisson",
    x = "Effect on stranding counts (conditional on > 0)",
    y = NULL,
    color = "Policy Target Presence"
  ) +
  theme(
    legend.position = "bottom", 
    legend.text = element_text(color = "black", size = 12),
    plot.title = element_text(color = "black", size = 15),      # Title color and size
    axis.title.x = element_text(color = "black", size = 14),   # X-axis title
    axis.text = element_text(size = 12, color = "black"),  # Axis tick labels
  ) 


  #Policy Overlap----


#Plotting within grid (Grid FE)
# Filter to positive counts
grid_df_positive <- grid_df_pos[grid_df_pos$n_strandings > 0, ]
grid_df_positive <- grid_df_positive %>%
  mutate(cat_overlap = factor(cat_overlap),
         year = factor(year),
         grid_id = factor(grid_id))

# Regular Poisson (note: this is NOT exactly the same, just an approximation)
model_ov <- glm(n_strandings ~ cat_overlap + grid_id + year,
             family = poisson(link = "log"),
             data = grid_df_positive)

# Cluster-robust SEs
mf_ov <- model.frame(model_ov)
vcov_cluster <- vcovCL(model_ov, cluster = mf_ov$grid_id, type = "HC0")
ct_ov <- coeftest(model_ov, vcov = vcov_cluster)

ct_df_ov <- data.frame(
  term = rownames(ct_ov),
  estimate = ct_ov[, "Estimate"],
  se = ct_ov[, "Std. Error"],
  z = ct_ov[, "z value"],
  p = ct_ov[, "Pr(>|z|)"],
  row.names = NULL)

ct_df_ov <- ct_df_ov[ct_df_ov$term %in% c("cat_overlap1", "cat_overlap2"), ]

# Stata-style 95% CI (normal)
ct_df_ov$conf.low  <- ct_df_ov$estimate - 1.96 * ct_df_ov$se
ct_df_ov$conf.high <- ct_df_ov$estimate + 1.96 * ct_df_ov$se

# relabel + sign flip (to show on left like your figure)
ct_df_ov$term_lab_ov <- ifelse(ct_df_ov$term == "cat_overlap1",
                         "1 Policy category present", 
                         "2+ Policy categories present")



pol_ov_se <- ggplot(ct_df_ov, aes(y = term_lab_ov, color = term_lab_ov)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.8) +
  geom_segment(aes(x = conf.low, xend = conf.high, yend = term_lab_ov), linewidth = 1.2) +
  geom_point(aes(x = estimate), size = 3) +
  coord_cartesian(xlim = c(min(ct_df$conf.low), 0)) +
  scale_color_manual(values = c(
    "1 Policy category present"   = "steelblue",
    "2+ Policy categories present" = "indianred3"
  )) +
  scale_y_discrete(limits = rev) + # reverse order on y-axis
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", axis.text.y = element_text(color = "black", size = 14)) +
  labs(
    title = "Within-grid associations by policy categories active",
    x = "Coefficient (sign-flipped), clustered by grid",
    y = NULL
  ) 




library(patchwork)
allgridfe <- (pol_targ_se + theme(title = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(),axis.text.x = element_blank()))/pol_ov_se + (theme(title = element_blank())) #removes x-axis text and tick marks from top graph and removes title from second graph

allgridfe + plot_annotation(title = "Within grid associations by active policy targets and categories", caption = "Clustered by grid", theme = theme(plot.title = element_text(size = 18, hjust = 0.5), plot.caption = element_text(size = 14, hjust = 0.9))) 

(pol_targ_se + theme(axis.title.x = element_blank())) + pol_ov_se 



#Combine Target and Overlap into 1 plot without patchwork

combined_grid_fe <- dplyr::bind_rows(ct_df, ct_df_ov)
combined_grid_fe$term = factor(combined_grid_fe$term, 
                                        levels=c("direct", "indirect", "cat_overlap1", "cat_overlap2"),labels =c("Direct policy present", "Indirect policy present", "1 policy category present", "2+ policy categories present")) #change order to match how you want it displayed

grid_colors <- c("steelblue", "indianred3", "#004D40", "#004D40")

gridfe_pols <- ggplot(combined_grid_fe, aes(y = term, color = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.8) +
  geom_segment(aes(x = conf.low, xend = conf.high, yend = term), linewidth = 7) +
  geom_point(aes(x = estimate), size = 12) +
  coord_cartesian(xlim = c(-1.02, 0)) + #makes the plot a little narrower by adjusting x-axis
  scale_color_manual(values = grid_colors) +
  scale_y_discrete(limits = rev) + # reverse order on y-axis
  theme(legend.position = "none", 
        axis.line.x = element_line(color = "black", linewidth = 1), 
        axis.line.y = element_line(color = "black", linewidth =1),
        plot.title = element_text(color = "black", size = 24),
        axis.text.y = element_text(color = "black", size = 21),
        plot.subtitle = element_text(color = "black", size = 18),
        plot.caption = element_text(color = "black", size = 18)) +
  labs( 
    #title = "Within grid associations by active policy target and categories",
    subtitle = "Truncated Poisson Results: Coefficients",
    x = NULL, 
    y = NULL,
    caption = "Note: Both models include grid cell and year fixed effects and cluster SEs by grid cell"
  ) 

ggsave("gridfe_plot.png", plot = gridfe_pols, dpi = 500)
