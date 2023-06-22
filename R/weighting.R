#' Title
#'
#' @param dat
#' @param exposure
#' @param covariates
#' @param method
#' @param method_args
#'
#' @return
#' @export
estimate_weights <- function(dat,
                             exposure,
                             covariates = NULL,
                             method,
                             method_args) {
  # Create formula from exposure and covariates
  if (is.null(covariates)) {
    covariates <- setdiff(colnames(dat), exposure)
  }
  assertthat::assert_that(!exposure %in% colnames(covariates))
  form <- as.formula(
    paste0(exposure,
           " ~ ",
           paste0(covariates, collapse = " + "))
    )

  # Estimate weights using `WeightIt`
  other_args <- list(
    use_kernel = method_args$use_kernel,
    discrete = ifelse(method == "super",
                      method_args$sl_discrete,
                      FALSE)
  )
  if (method_args$sl_lib == FALSE) {
    method_args$SL.library <- c("SL.bartMachine", "SL.earth", "SL.gam",
                                "SL.gbm", "SL.glm", "SL.glmnet",
                                "SL.loess", "SL.ranger")
  }
  ret <- WeightIt::weightit(formula = form,
                            data = dat,
                            method = method,
                            #estimand = method_args$estimand,
                            #stabilize = method_args$stabilize,
                            #by = method_args$by,
                            #s.weights = method_args$s.weights,
                            ps = NULL,
                            SL.library = method_args$SL.library,
                            subclass = NULL,
                            missing = ,
                            verbose = FALSE,
                            include.obj = TRUE,
                            ... = other_args)

  return(list(
    weights = ret,
    exposure = exposure,
    covariates = covariates
  ))
}

#' Title
#'
#' @param dat
#' @param exposure
#' @param covariates
#' @param weights
#' @param path_save
#'
#' @return
#' @export
explore_balance <- function(exposure,
                            covariates,
                            weights) {
  threshold_cor <- 0.1

  # Assessing balance numerically
  tab <- cobalt::bal.tab(weights,
                         stats = c("c", "k"),
                         un = TRUE,
                         thresholds = c(cor = threshold_cor),
                         poly = 3)

  # Assessing balance graphically
  graph <- lapply(covariates, function(x) {
    cobalt::bal.plot(weights,
                     var.name = x,
                     which = "both")
  })

  # Summarizing balance in a Love plot
  love <- cobalt::love.plot(weights,
                            stats = c("c", "ks"),
                            abs = TRUE,
                            var.order = "unadjusted",
                            thresholds = c(cor = threshold_cor),
                            line = TRUE,
                            title = exposure)

  return(list(
    exposure = exposure,
    tab = tab,
    graph = graph,
    love = love
  ))
}

#' Title
#'
#' @param dat
#' @param exposure
#' @param covariates
#' @param weights
#' @param method
#' @param method_args
#'
#' @return
#' @export
fit_model_weighted <- function(dat,
                               exposure,
                               covariates,
                               weights,
                               method,
                               method_args) {}
