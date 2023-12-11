#' Extract the cohort label from the subject ID
#'
#' @description
#' This function extracts the cohort ID (e.g., SAB) from the subject ID
#' (e.g., SubjectID). By default it takes the first three characters.
#'
#' @param dat A dataframe containing the variables of interest. A dataframe.
#' @param id_var The variable name to be used to identify subjects. A string.
#' @param st Integer indicating the first element to be extracted. An integer.
#' @param en Integer indicating the last element to be extracted. An integer.
#'
#' @returns A dataframe containing a new column named `cohort`. A dataframe.
#'
#' @export
extract_cohort <- function(dat, id_var, st, en) {
  dat <- dat |>
    tidylog::mutate(cohort = substr(.data[[id_var]], st, en))
  dat$cohort <- as.factor(dat$cohort)

  return(dat)
}
################################################################################

#' Convert time variables to season
#'
#' @param dat A dataframe containing the variables of interest. A dataframe.
#' @param cols A vector of column names as strings. A vector.
#'
#' @returns A dataframe with additional columns for seasons. A dataframe.
#'
#' @export
convert_time_season <- function(dat, cols) {
  ret <- dat |>
    tidylog::mutate(dplyr::across(
      dplyr::any_of(cols),
      ~ lubridate::month(..1)
    )) |>
    tidylog::mutate(dplyr::across(
      dplyr::any_of(cols),
      ~ dplyr::case_when(
        ..1 %in% c(12, 1, 2) ~ "winter",
        ..1 %in% c(3, 4, 5) ~ "spring",
        ..1 %in% c(6, 7, 8) ~ "summer",
        ..1 %in% c(9, 10, 11) ~ "autumn"
      )
    ))

  return(ret)
}
################################################################################

#' Basic pre-processing of dataframes
#'
#' @description
#' This function performs basic pre-processing of a given dataframe:
#' \itemize{
#'  \item Data cleaning:
#'    \itemize{
#'      \item Handling values below the limit of detection (LOD) or quantification (LOQ).
#'      \item Handling missing values.
#'    }
#'  \item Variable transformations:
#'  \itemize{
#'      \item Standardization (with custom center and scale functions).
#'      \item Correcting for urine dilution with creatinine.
#'      \item Outcome bounding (for e.g., TMLE).
#'    }
#' }
#'
#' @param dat A dataframe containing the variables of interest. A dataframe.
#' @param dat_desc A optional dataframe containing the same variables but
#' with information on the type of values (e.g., 2 for value <LOD).
#' This is necessary to distinguish the type of missing values
#' (e.g., missing because <LOD or because no sample was available). A dataframe.
#' @param covariates A dataframe containing additional variables. A dataframe.
#' @param outcome A string indicating the outcome variable. A string.
#' @param dat_llodq A optional dataframe containing the LOD/LOQ values (`val`)
#' for the variables (`var`) of interest. A dataframe.
#' @param dic_steps A ordered, nested named list of steps to perform. A list. It can
#' include the following elements:
#' * `llodq`, to handle values <LOD/LOQ. A named list with elements:
#'  * `id_val`, which values in `dat_desc` should be considered. A vector of integers.
#'  * `method`, the method to be used. Currently, only a replacement
#'  approach is supported. A string.
#'  * `divide_by`, .
#'  * `creatinine_threshold`, subjects for which the creatinine levels are
#'  below this value will not be processed. Currently not used. A double.
#'  * `threshold_within`, the threshold within each group for the
#'  fraction of values corresponding to `id_val`. An integer.
#'  * `threshold_overall`, the overall threshold for the
#'  fraction of values corresponding to `id_val`. An integer.
#'  * `tune_sigma`, currently not supported. A double.
#' * `missings`, to handle missing values. A named list with elements:
#'  * `threshold_within`, the missing value threshold within each group. An integer.
#'  * `threshold_overall`, the overall missing value threshold. An integer.
#'  * `use_additional_covariates`, .
#'  * `selected_covariates`, a vector of covariates' names. A vector.
#'  * `method_imputation`, method to be used to impute values. A string.
#'  * `k`, the number of nearest neighbors to use for kNN. An integer.
#'  * `path_save_res`, path to directory where to save figures.
#'  Currently, variables can be imputed in a univariate way (`univariate`), using
#'  selected covariates (`selected`), or all the covariates
#'  available in `covariates` (`all`). A string.
#' * `creatinine`, to handle confounding by dilution. A named list with elements:
#'  * `method`, the method to be used. Currently, only covariate-adjusted
#'  standardization is implemented (`cas`). A string.
#'  * `method_fit_args`, options for fitting the models.
#'  Currently, only the family to be used within \link[stats]{glm}. A list.
#'  * `creatinine_covariates_names`, .
#'  * `creatinine_name`, .
#'  * `path_save_res`, path to directory where to save figures.
#' * `transform`, to transform variables. A named list with elements:
#'  * `transformation_fun`, the transformation function (e.g., `log`).
#' * `standardization`, to standardize variables. A named list with elements:
#'  * `center_fun`, the centering function (e.g., `median`).
#'  * `scale_fun`, the scaling function (e.g., `IQR`).
#' * `bound`, to bound the outcome variable. A named list with elements:
#' @param id_var The variable name to be used to identify subjects. A string.
#' @param by_var The variable name to group by. A string.
#' @md
#'
#' @returns A pre-processed dataframe. A dataframe.
#'
#' @export
preproc_data <- function(dat,
                         dat_desc,
                         covariates,
                         outcome,
                         dat_llodq,
                         dic_steps,
                         id_var,
                         by_var) {
  dat_ret <- dat

  for (step in names(dic_steps)) {
    dat_ret <- switch(step,
      #######################
      "llodq" = handle_llodq(
        #######################
        dat = dat_ret,
        id_var = id_var,
        by_var = by_var,
        dat_desc = dat_desc,
        id_val = dic_steps$llodq$id_val,
        method = dic_steps$llodq$method,
        replacement_vals = dat_llodq,
        divide_by = dic_steps$llodq$divide_by,
        creatinine_threshold = dic_steps$llodq$creatinine_threshold,
        frac_val_threshold_within = dic_steps$llodq$threshold_within,
        frac_val_threshold_overall = dic_steps$llodq$threshold_overall,
        tune_sigma = dic_steps$llodq$tune_sigma
      )$dat,
      ###################################
      "missings" = handle_missing_values(
        ###################################
        dat = dat_ret,
        covariates = covariates,
        use_additional_covariates = dic_steps$missings$use_additional_covariates,
        selected_covariates = dic_steps$missings$selected_covariates,
        id_var = id_var,
        by_var = by_var,
        threshold_within = dic_steps$missings$threshold_within,
        threshold_overall = dic_steps$missings$threshold_overall,
        method_imputation = dic_steps$missings$method_imputation,
        k = dic_steps$missings$k,
        path_save_res = dic_steps$missings$path_save_res
      )$dat_imputed,
      #############################################
      "creatinine" = handle_creatinine_confounding(
        #############################################
        dat = dat_ret,
        covariates = covariates,
        id_var = id_var,
        by_var = by_var,
        covariates_names = dic_steps$creatinine$creatinine_covariates_names,
        creatinine = dic_steps$creatinine$creatinine_name,
        method = dic_steps$creatinine$method,
        method_fit_args = dic_steps$creatinine$method_fit_args,
        path_save_res = dic_steps$creatinine$path_save_res
      ),
      ####################################
      "transform" = handle_transformation(
        ####################################
        dat = dat_ret,
        id_var = id_var,
        by_var = by_var,
        transformation_fun = dic_steps$transform$transformation_fun
      ),
      ###########################################
      "standardization" = handle_standardization(
        ###########################################
        dat = dat_ret,
        id_var = id_var,
        by_var = by_var,
        center_fun = dic_steps$standardization$center_fun,
        scale_fun = dic_steps$standardization$scale_fun
      ),
      ########################
      "bound" = bound_outcome(
        ########################
        dat = dat_ret,
        var = outcome
      ),
      stop("Invalid `step` option.")
    )
  } # End loop over steps

  return(dat = dat_ret)
}
################################################################################

#' Various strategies to handle values below the limit of detection/quantification
#'
#' @description
#'
#' @param dat A dataframe containing the variables of interest. A dataframe.
#' @param id_var
#' @param by_var
#' @param dat_desc
#' @param id_val
#' @param method
#' @param replacement_vals
#' @param divide_by
#' @param creatinine_threshold
#' @param frac_val_threshold_within
#' @param frac_val_threshold_overall
#' @param tune_sigma
#'
#' @returns
#'
#' @export
handle_llodq <- function(dat,
                         id_var,
                         by_var,
                         dat_desc,
                         id_val,
                         method,
                         replacement_vals,
                         divide_by,
                         creatinine_threshold,
                         frac_val_threshold_within,
                         frac_val_threshold_overall,
                         tune_sigma) {
  # List of supported methods
  supported <- list("truncated_normal", "replace")
  if (!method %in% supported) {
    stop(
      glue::glue("{method} is currently not supported.",
        method = method
      ),
      call. = TRUE
    )
  }

  # Checks
  assertthat::assert_that(nrow(dat) == nrow(dat_desc),
    msg = "Mismatch in the number of rows between the provided dataframes."
  )
  assertthat::assert_that(ncol(dat) == ncol(dat_desc),
    msg = "Mismatch in the number of columns between the provided dataframes."
  )
  assertthat::assert_that(identical(dat[[id_var]], dat_desc[[id_var]]),
    msg = "The order of the rows does not match between dataframes."
  )

  # Select and apply method
  if (method == "truncated_normal") {
    warning("This method has not been tested yet.",
      call. = TRUE
    )

    # Imputation based on quantile regression (based on impQRILC.R)
    # Loop over the samples/subjects
    dat_desc <-
      tidylog::select(dat_desc, -dplyr::all_of(c(id_var, by_var)))
    dat_imputed <- dat
    lapply(1:nrow(dat), function(idx) {
      frac_nas <- sum(dat_desc[idx, ] == id_val) / ncol(dat_desc)

      if (frac_nas == 0) {
        dat_imputed[idx, ] <- dat[idx, ]
      } else {
        # Estimate mean and sd using quantile regression
        upper_q <- 0.99
        q_normal <- qnorm(
          seq(
            (frac_nas + 0.001),
            (upper_q + 0.001),
            (upper_q - frac_nas) / (upper_q * 100)
          ),
          mean = 0,
          sd = 1
        )
        q_samples <- quantile(dat[idx, ],
          probs = seq(
            0.001,
            (upper_q + 0.001),
            0.01
          ),
          na.rm = TRUE
        )
        fit <- lm(q_samples ~ q_normal)
        mean_fit <- as.numeric(fit$coefficients[1])
        sd_fit <- as.numeric(fit$coefficients[2])

        # Generate data from a multivariate distribution w/ MLE parameters
        tmp <- tmvtnorm::rtmvnorm(
          n = ncol(dat),
          mean = mean_fit,
          sigma = sd_fit * tune_sigma,
          upper = qnorm((frac_nas + 0.001),
            mean = mean_fit,
            sd = sd_fit
          ),
          algorithm = c("gibbs")
        ) # End data generation
        sample_imputed <- dat[idx, ]
        sample_imputed[which(dat_desc[idx, ] == id_val)] <- tmp[which(dat_desc[idx, ] == id_val)]
        dat_imputed[idx, ] <- sample_imputed
      }
    }) # End loop over samples
    # End truncated_normal method
  } else if (method == "replace") {
    # Imputation method where values <LOD/LOQ are replaced with e.g., LOD/2
    ## Check fraction of missing values (within and overall)
    frac_within <- dat_desc |>
      tidylog::select(-dplyr::all_of(id_var)) |>
      tidylog::mutate(
        dplyr::across(
          dplyr::everything(),
          \(x) dplyr::case_when(
            x %in% id_val ~ NA,
            .default = x
          )
        )
      )
    frac_within <- frac_within |>
      tidylog::group_by(.data[[by_var]]) |>
      naniar::miss_var_summary() |>
      tidylog::filter(pct_miss > frac_val_threshold_within) |>
      tidylog::ungroup()
    dat <- dat |>
      tidylog::select(-dplyr::all_of(frac_within$variable))

    frac_overall <- dat_desc |>
      tidylog::select(-dplyr::all_of(c(id_var, by_var))) |>
      tidylog::mutate(
        dplyr::across(
          dplyr::everything(),
          \(x) dplyr::case_when(
            x %in% id_val ~ NA,
            .default = x
          )
        )
      )
    frac_overall <- frac_overall |>
      naniar::miss_var_summary() |>
      tidylog::filter(pct_miss > frac_val_threshold_overall) |>
      tidylog::ungroup()
    dat <- dat |>
      tidylog::select(-dplyr::any_of(frac_overall$variable))

    ## Impute remaining
    dat_imputed <- lapply(colnames(dat), function(col_) {
      lapply(1:nrow(dat), function(idx) {
        if (is.na(dat[idx, col_]) & dat_desc[idx, col_] %in% id_val) {
          replacement_vals[replacement_vals$var == col_, ]$val / divide_by
        } else {
          dat[idx, col_]
        }
      }) |>
        unlist() |>
        unname()
    })
    names(dat_imputed) <- colnames(dat)
    dat_imputed <- tibble::as_tibble(dat_imputed)

    # Checks
    assertthat::assert_that(
      ncol(dat_imputed) == ncol(dat),
      msg = "Mismatch in the number of columns in the imputed dataset."
    )
    assertthat::assert_that(
      nrow(dat_imputed) == nrow(dat),
      msg = "Mismatch in the number of rows in the imputed dataset."
    )
  } # End replace method

  return(list(dat = dat_imputed))
}
################################################################################

#' Various strategies to handle missing values
#'
#' @description
#' Given a dataframe, this function performs the following steps:
#' * Removal of variables with a fraction of missing values greater than
#' the chosen threshold, within each group.
#' * Removal of variables with a fraction of missing values greater than
#' the chosen threshold, for the entire dataframe.
#' * Imputation of the remaining variables.
#' @md
#'
#' @param dat A dataframe containing the variables of interest. A dataframe.
#' @param covariates A dataframe containing additional variables. A dataframe.
#' @param use_additional_covariates
#' @param selected_covariates
#' @param id_var The variable name to be used to identify subjects. A string.
#' @param by_var The variable name to group by. A string.
#' @param threshold_within The missing value threshold within each group. An integer.
#' @param threshold_overall The overall missing value threshold. An integer.
#' @param method_imputation
#' @param k Number of nearest neighbors used for kNN.
#' @param path_save_res
#'
#' @returns A named list containing the results of the steps described above.
#' The imputed dataframe is named `dat_imputed`.
#'
#' @export
handle_missing_values <- function(dat,
                                  covariates,
                                  use_additional_covariates,
                                  selected_covariates,
                                  id_var,
                                  by_var,
                                  threshold_within,
                                  threshold_overall,
                                  method_imputation,
                                  k,
                                  path_save_res) {
  if (sum(is.na(dat)) == 0) {
    message("No missing values found.\n")

    return(list(
      dat_imputed = dat
    ))
  }

  vis_miss_before <- naniar::vis_miss(
    tidylog::select(
      dat, -dplyr::all_of(c(id_var, by_var))
    ),
    cluster = FALSE,
    show_perc = TRUE,
    show_perc_col = TRUE
  ) +
    ggplot2::theme(
      legend.position = "right",
      panel.grid = ggplot2::element_blank(),
      axis.text.x.top = ggplot2::element_text(vjust = -0.7)
    )

  # Step 1: group by factor and remove variables with a high
  #         fraction of missing values within each group
  step1 <- dat |>
    tidylog::select(-dplyr::all_of(id_var)) |>
    tidylog::group_by(.data[[by_var]]) |>
    naniar::miss_var_summary() |>
    tidylog::filter(pct_miss > threshold_within) |>
    tidylog::ungroup()
  dat <- dat |>
    tidylog::select(-dplyr::all_of(step1$variable))

  # Step 2: remove variables with a high fraction of missing
  #         values overall
  step2 <- dat |>
    tidylog::select(-dplyr::all_of(id_var)) |>
    naniar::miss_var_summary() |>
    tidylog::filter(pct_miss > threshold_overall)
  dat <- dat |>
    tidylog::select(-dplyr::all_of(step2$variable))

  # Step 3: impute the remaining variables
  ## Check whether to perform imputation by including additional variables
  cols_to_remove <- NULL
  if (use_additional_covariates == TRUE) {
    cols_to_remove <- ifelse(is.null(selected_covariates),
      colnames(covariates),
      selected_covariates
    )

    dat <- tidylog::full_join(
      dat,
      covariates |>
        tidylog::select(dplyr::all_of(c(
          id_var,
          cols_to_remove
        ))),
      by = id_var,
      suffix = c("", ".y")
    ) |>
      tidylog::select(-dplyr::ends_with(".y"))
  }

  ##############################################################################
  dat_imp <- switch(method_imputation,
    "vim.knn" = VIM::kNN(
      data = dat,
      variable = setdiff(
        colnames(dat),
        c(cols_to_remove, id_var, by_var)
      ),
      metric = NULL,
      k = k,
      dist_var = setdiff(
        colnames(dat),
        c(id_var)
      ),
      weights = NULL,
      numFun = median,
      catFun = VIM::maxCat,
      methodStand = "iqr",
      addRandom = FALSE,
      useImputedDist = FALSE,
      weightDist = FALSE,
      imp_var = FALSE
    ),
    # http://statistikat.github.io/VIM/reference/irmi.html
    "vim.irmi" = VIM::irmi(),
    # http://statistikat.github.io/VIM/reference/regressionImp.html
    "vim.reg" = VIM::regressionImp()
  ) |>
    tibble::as_tibble()

  assertthat::assert_that(
    identical(dat[[id_var]], dat_imp[[id_var]]),
    msg = "The order of the rows does not match between dataframes."
  )
  ##############################################################################

  if (!is.null(cols_to_remove)) {
    dat_imp <- dat_imp |>
      tidylog::select(-dplyr::all_of(setdiff(cols_to_remove, c(id_var, by_var))))
  }

  vis_miss_after <- naniar::vis_miss(
    tidylog::select(
      dat_imp, -dplyr::all_of(c(id_var, by_var))
    ),
    cluster = FALSE,
    show_perc = TRUE,
    show_perc_col = TRUE
  ) +
    ggplot2::theme(
      legend.position = "right",
      panel.grid = ggplot2::element_blank(),
      axis.text.x.top = ggplot2::element_text(vjust = -0.7)
    )

  # Step 4: save figures

  return(
    list(
      step1 = step1,
      step2 = step2,
      dat_imputed = dat_imp,
      vis_miss_before = vis_miss_before,
      vis_miss_after = vis_miss_after
    )
  )
}
################################################################################

#' Title
#'
#' @description
#'
#' @param dat
#' @param covariates
#' @param id_var
#' @param by_var
#' @param covariates_names
#' @param creatinine
#' @param method
#' @param method_fit_args
#' @param path_save_res
#'
#' @returns
#'
#' @export
handle_creatinine_confounding <- function(dat,
                                          covariates,
                                          id_var,
                                          by_var,
                                          covariates_names,
                                          creatinine,
                                          method,
                                          method_fit_args,
                                          path_save_res) {
  if (!is.null(covariates)) {
    assertthat::assert_that(
      nrow(dat) == nrow(covariates),
      msg = "The number of rows does not match between dataframes."
    )
  }

  # List of variables to which the method should be applied
  var_names <- setdiff(colnames(dat), c(id_var, by_var))

  # Covariate-adjusted standardization
  cas <- function() {
    warning("Creatinine values are currently predicted without weights.",
      call. = TRUE
    )

    # Step 1: estimate weights for creatinine
    wts <- rep(1, times = nrow(dat))

    # Step 2: predict creatinine with weights
    ## Formula for model fitting
    form <- paste0(
      creatinine,
      " ~ ",
      paste0(
        setdiff(
          covariates_names$numerical, creatinine
        ),
        collapse = " + "
      ),
      " + ",
      paste0("factor(",
        setdiff(
          covariates_names$categorical, creatinine
        ),
        ")",
        collapse = " + "
      )
    )
    ## Fit model for creatinine
    mod_creatine <- glm(
      formula = as.formula(form),
      data = covariates,
      weights = wts,
      family = method_fit_args$family
    )
    ## Check fitted model
    check_mod_creat <- check_model(
      model = mod_creatine,
      path_save_res = path_save_res
    )
    ## Predicted creatinine values
    covariates <- covariates |>
      modelr::add_predictions(
        model = mod_creatine,
        var = "cpred",
        type = "response"
      )

    # Step 3: compute `Cratio = exposure / (C_obs / Cpred)`
    dat <-
      tidylog::full_join(dat, covariates[, c(id_var, creatinine, "cpred")],
        by = id_var
      ) |>
      tidylog::mutate(dplyr::across(
        dplyr::all_of(var_names),
        \(x) {
          x / (.data[[creatinine]] / cpred)
        }
      )) |>
      tidylog::select(-dplyr::all_of(c(creatinine, "cpred")))

    return(dat)
  } # End function cas

  dat_ret <- switch(method,
    "cas" = cas(),
    stop("Invalid `method`.")
  )

  return(dat_ret)
}
################################################################################

#' Title
#'
#' @param dat
#' @param id_var
#' @param by_var
#' @param transformation_fun
#'
#' @return
#'
#' @export
handle_transformation <- function(dat, id_var, by_var, transformation_fun) {
  # When log-transforming, original variable should be strictly positive
  if (deparse(transformation_fun) %in% c(log, log10, log2, log1p, logb)) {
    res <- dat |>
      tidylog::select(-dplyr::all_of(c(id_var, by_var))) |>
      tidylog::gather() |>
      tidylog::group_by(key) |>
      tidylog::summarise(
        res = any(value[!is.na(value)] <= 0)
      )

    if (sum(res$res) > 0) {
      warning(
        paste0(
          "One or more variables contain negative values.",
          " Log-transformation is not appropriate in this case."
        ),
        call. = TRUE
      )

      cat(
        res[res$res == TRUE, ]$key,
        sep = "\n"
      )
    }
  } # End check negative values for log-transformation

  dat_ret <- dat |>
    tidylog::mutate(dplyr::across(
      dplyr::all_of(setdiff(
        colnames(dat), c(id_var, by_var)
      )),
      \(x) transformation_fun(x)
    ))

  return(dat_ret)
}
################################################################################

#' Title
#'
#' @param dat
#' @param id_var
#' @param by_var
#' @param center_fun
#' @param scale_fun
#'
#' @returns
#'
#' @export
handle_standardization <- function(dat,
                                   id_var, by_var,
                                   center_fun, scale_fun) {
  dat_ret <- dat |>
    tidylog::mutate(dplyr::across(
      dplyr::all_of(setdiff(
        colnames(dat), c(id_var, by_var)
      )),
      \(x) (x - center_fun(x)) / scale_fun(x)
    ))

  return(dat_ret)
}
################################################################################

#' Bound a outcome variable
#'
#' @description
#' In order to be able to use e.g., TMLE, with a continuous outcome,
#' it is necessary to bound it between 0 and 1. This function performs
#' the necessary steps.
#'
#' @param dat A dataframe containing the variables of interest. A dataframe.
#' @param var The variable name corresponding to the outcome to be bounded. A string.
#'
#' @returns A dataframe containing the bounded outcome.
#'
#' @export
bound_outcome <- function(dat, var) {
  b <- max(dat[[var]], na.rm = TRUE)
  a <- min(dat[[var]], na.rm = TRUE)
  num <- dat[[var]] - a
  den <- b - a
  dat[[var]] <- num / den

  assertthat::assert_that(min(dat[[var]], na.rm = TRUE) >= 0,
    msg = "Outcome is not bounded (min)."
  )
  assertthat::assert_that(max(dat[[var]], na.rm = TRUE) <= 1,
    msg = "Outcome is not bounded (max)."
  )

  return(dat)
}
################################################################################
