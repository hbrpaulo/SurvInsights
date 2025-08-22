library(survival)
library(flexsurv)
library(dplyr)
library(tidyr)
library(ggplot2)

.profile_labels <- function(nd) {
  as.character(apply(nd, 1, function(r) {
    paste(paste0(names(r), "=", r), collapse = ", ")
  }))
}
.label_df_rows <- function(df) {
  apply(df, 1, function(r) paste(paste0(names(r), "=", r), collapse = ", "))
}

.survreg_weibull_S <- function(fit, newdata_row, times) {
  shape <- 1 / fit$scale
  mu <- as.numeric(predict(fit, newdata = newdata_row, type = "lp"))
  eta <- exp(mu)
  S <- exp( - (times / eta)^shape )
  data.frame(t = times, S = S)
}

.flexsurv_weibull_S <- function(fit, newdata_row, times, conf_level = 0.95) {
  s <- summary(fit, newdata = newdata_row, t = times,
               type = "survival", ci = TRUE, cl = conf_level)
  df <- do.call(rbind, lapply(s, as.data.frame))
  df %>% transmute(t = time, S = est, S_lcl = lcl, S_ucl = ucl)
}

.km_S_at_T <- function(sf, T_max) {
  s <- summary(sf)
  if (is.null(s$strata)) {
    df <- data.frame(t = s$time, S = s$surv)
    last_idx <- max(which(df$t <= T_max), na.rm = TRUE)
    S_T <- if (is.finite(last_idx)) df$S[last_idx] else 1
    return(data.frame(profile = "(all)", t = T_max, S_T_KM = S_T))
  }
  df <- data.frame(profile_raw = as.character(s$strata),
                   t = s$time, S = s$surv)
  df$profile <- sub("^\\.__km_profile=", "", df$profile_raw)
  df %>%
    group_by(profile) %>%
    summarize(
      S_T_KM = if (any(t <= T_max)) S[which.max(t[t <= T_max])] else 1,
      .groups = "drop"
    ) %>%
    mutate(t = T_max) %>%
    select(profile, t, S_T_KM)
}

weibull_surv_curves_up_to_T <- function(
    data,
    time_col,
    status_col,
    rhs,
    newdata,
    T_max,
    n_grid = 200,
    backend = c("survreg","flexsurv"),
    plot = TRUE,
    conf_level = 0.95,
    overlay_km = FALSE,
    km_by = NULL,
    alpha_km = .2,
    alpha_reg = 1
) {
  stopifnot(T_max > 0, n_grid >= 2)
  backend <- match.arg(backend)
  fml <- as.formula(paste0("Surv(", time_col, ", ", status_col, ") ~ ", rhs))
  tgrid <- seq(0, T_max, length.out = n_grid)
  
  if (backend == "survreg") {
    fit <- survreg(fml, data = data, dist = "weibull")
  } else {
    fit <- flexsurvreg(fml, data = data, dist = "weibull")
  }
  
  if (!is.null(km_by)) {
    if (!all(km_by %in% names(data))) stop("Every km_by variable must exist in 'data'.")
    if (!all(km_by %in% names(newdata))) stop("Every km_by variable must exist in 'newdata'.")
    prof_labels <- .label_df_rows(newdata[, km_by, drop = FALSE])
  } else {
    prof_labels <- .profile_labels(newdata)
  }
  
  pieces <- vector("list", nrow(newdata))
  for (i in seq_len(nrow(newdata))) {
    nd_i <- newdata[i, , drop = FALSE]
    if (backend == "survreg") {
      df_i <- .survreg_weibull_S(fit, nd_i, tgrid) %>%
        mutate(profile = prof_labels[i])
    } else {
      df_i <- .flexsurv_weibull_S(fit, nd_i, tgrid, conf_level) %>%
        mutate(profile = prof_labels[i])
    }
    pieces[[i]] <- df_i
  }
  pred_df <- bind_rows(pieces)
  
  if (backend == "survreg") {
    shape <- 1 / fit$scale
    mu_vec <- as.numeric(predict(fit, newdata = newdata, type = "lp"))
    eta_vec <- exp(mu_vec)
    S_T <- exp( - (T_max / eta_vec)^shape )
    surv_at_T <- data.frame(profile = prof_labels, t = T_max, S_T = S_T)
  } else {
    pieces_T <- vector("list", nrow(newdata))
    for (i in seq_len(nrow(newdata))) {
      sT <- summary(fit, newdata = newdata[i, , drop = FALSE],
                    t = T_max, type = "survival", ci = TRUE, cl = conf_level)
      row <- as.data.frame(sT[[1]])
      pieces_T[[i]] <- data.frame(
        profile = prof_labels[i],
        t = row$time,
        S_T = row$est,
        S_T_lcl = row$lcl,
        S_T_ucl = row$ucl
      )
    }
    surv_at_T <- bind_rows(pieces_T)
  }
  
  km_df <- NULL
  km_surv_at_T <- NULL
  if (overlay_km) {
    if (is.null(km_by) || length(km_by) == 0)
      stop("Provide 'km_by' to stratify the KM.")
    
    data <- data %>%
      mutate(`.__km_profile` = .label_df_rows(across(all_of(km_by))))
    
    # per-stratum maximum observed time (censored or event), truncated at T_max
    limits <- data %>%
      group_by(`.__km_profile`) %>%
      summarize(t_end = max(.data[[time_col]], na.rm = TRUE), .groups = "drop") %>%
      transmute(profile = as.character(`.__km_profile`),
                t_end = pmin(t_end, T_max))
    
    sf <- survfit(as.formula(paste0("Surv(", time_col, ", ", status_col, ") ~ .__km_profile")), data = data)
    
    s <- summary(sf)
    base_km <- data.frame(
      t = s$time,
      S = s$surv,
      profile = sub("^\\.__km_profile=", "", as.character(s$strata))
    )
    
    # keep only steps up to each profile's t_end
    km_df <- base_km %>%
      inner_join(limits, by = "profile") %>%
      filter(t <= t_end) %>%
      select(profile, t, S, t_end)
    
    # add starting point (0,1) for every profile
    start_df <- limits %>% transmute(profile, t = 0, S = 1, t_end)
    km_df <- bind_rows(start_df, km_df) %>%
      arrange(profile, t)
    
    # extend horizontally to t_end (so the plateau aparece até o último tempo observado)
    ext_df <- km_df %>%
      group_by(profile) %>%
      summarize(t = unique(t_end)[1],
                S = dplyr::last(S),
                .groups = "drop")
    km_df <- bind_rows(km_df %>% select(profile, t, S), ext_df) %>%
      arrange(profile, t)
    
    # KM at T_max = last known step at time <= min(T_max, t_end)
    km_surv_at_T <- .km_S_at_T(sf, T_max)
  }
  
  p <- NULL
  if (plot) {
    p <- ggplot(pred_df, aes(x = t, y = S, color = profile)) +
      geom_line(size = 1, alpha = alpha_reg) +
      geom_vline(xintercept = T_max, linetype = "dashed") +
      coord_cartesian(xlim = c(0, T_max), ylim = c(0, 1)) +
      labs(x = "time", y = "S(t)", color = "profile") +
      theme_minimal(base_size = 12)
    
    if ("S_lcl" %in% names(pred_df)) {
      # p <- p + geom_ribbon(aes(ymin = S_lcl, ymax = S_ucl, fill = profile),
      #                      alpha = 0.15, linewidth = 0, inherit.aes = FALSE)
    }
    if (!is.null(km_df) && nrow(km_df) > 0) {
      p <- p + geom_step(data = km_df, aes(x = t, y = S, color = profile),
                         linewidth = 0.9, alpha = alpha_km)
    }
  }
  
  list(
    fit = fit,
    curves = pred_df,
    survival_at_T = surv_at_T,
    km_curves = km_df,
    km_survival_at_T = km_surv_at_T,
    plot = p
  )
}

# 
# # Example ---------------------------------------------------------------------
# # Simulated data (same as antes)
# set.seed(1)
# n <- 400
# x1 <- rbinom(n, 1, 0.5)
# x2 <- rnorm(n, 0, 1)
# beta_shape <- 1.6
# eta_base <- 800
# b0 <- log(eta_base); b1 <- log(1.3); b2 <- -0.20
# eta_x <- exp(b0 + b1 * x1 + b2 * x2)
# u <- runif(n)
# time <- eta_x * (-log(u))^(1/beta_shape)
# cens <- rexp(n, rate = 1/1500)
# tt <- pmin(time, cens)
# status <- as.integer(time <= cens)
# dat <- data.frame(tt = tt, status = status, x1 = x1, x2 = x2)
# 
# # Profiles to evaluate (labels will use km_by = "x1")
# nd <- data.frame(x1 = c(0, 1), x2 = c(0, 0))
# 
# res <- weibull_surv_curves_up_to_T(
#   data = dat,
#   time_col = "tt",
#   status_col = "status",
#   rhs = "x1 + x2",
#   newdata = nd,
#   T_max = 5500,
#   backend = "flexsurv",
#   plot = TRUE,
#   overlay_km = TRUE,
#   km_by = c("x1")
# )
# 
# res$plot
# res$survival_at_T
# res$km_survival_at_T
