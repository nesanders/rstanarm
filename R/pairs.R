#' Pairs plot for stanreg objects
#' 
#' The pairs method for stanreg objects is a wrapper for 
#' \code{\link[bayesplot]{mcmc_pairs}} in the \pkg{bayesplot} package.
#' \strong{Be careful not to specify too many parameters to include or the plot
#' will be both hard to read and slow to render.}
#'
#' @method pairs stanreg
#' @export
#' @templateVar stanregArg x
#' @template args-stanreg-object
#' @template args-regex-pars
#' @param ... Currently ignored.
#' @param pars An optional character vetor of parameter names. All parameters 
#'   are included by default, but for models with more than just a few 
#'   parameters it may be far too many to visualize on a small computer screen 
#'   and also may require substantial computing time.
#' @param transformations,diag_fun,off_diag_fun,diag_args,off_diag_args,np_style
#'   Same as for \code{\link[bayesplot]{mcmc_pairs}}.
#' @param condition Same as for \code{\link[bayesplot]{mcmc_pairs}} 
#'   \strong{except the default is different}. In the \pkg{bayesplot} package 
#'   the default is \code{condition=NULL} whereas for \pkg{rstanarm} models we
#'   default to \code{condition="accept_stat__"}.
#'
#' @return Multiple ggplot objects arranged in a grid using 
#'   \code{\link[bayesplot]{bayesplot_grid}} (essentially a gtable object from 
#'   \code{\link[gridExtra]{arrangeGrob}}).
#' 
#' @examples
#' if (!exists("example_model")) example(example_model)
#' pairs(example_model, pars = c("(Intercept)", "log-posterior"))
#' 
#' \donttest{
#' # for demonstration purposes, intentionally fit a model that
#' # will (almost certainly) have some divergences
#' fit <- stan_glm(
#'   mpg ~ ., data = mtcars,
#'   iter = 1000,
#'   # this combo of prior and adapt_delta should lead to some divergences
#'   prior = hs(),
#'   adapt_delta = 0.9
#' )
#'
#' # split the draws according to above/below median accept_stat__ (default
#' # value of 'condition' arg) and show approximate location of 
#' # divergences (red points)
#' pairs(
#'   fit, 
#'   pars = c("wt", "sigma", "log-posterior"),
#'   off_diag_args = list(size = 1, alpha = 0.5),
#' )
#' 
#' # split the draws according to above/below median log-posterior 
#' # value (condition="lp__") and also change the type of plots on 
#' # both the diagonal and off-diagonal
#' bayesplot::color_scheme_set("brightblue")
#' pairs(
#'   fit,
#'   pars = c("wt", "sigma", "log-posterior"),
#'   diag_fun = "dens",
#'   off_diag_fun = "hex",
#'   condition = "lp__"
#' )
#' }
#' 
pairs.stanreg <- function(x,
                       pars = NULL,
                       regex_pars = NULL,
                       transformations = list(),
                       ...,
                       condition = "accept_stat__",
                       diag_fun = c("hist", "dens"),
                       diag_args = list(),
                       off_diag_fun = c("scatter", "hex"),
                       off_diag_args = list(),
                       np_style = list()) {
  if (!used.sampling(x))
    STOP_sampling_only("pairs")
  
  warn_unused_bayesplot_args(...)
  
  arr <- as.array.stanreg(x, pars = pars, regex_pars = regex_pars)
  bayesplot::mcmc_pairs(
    x = arr,
    transformations = transformations,
    condition = condition,
    diag_fun = diag_fun, 
    off_diag_fun = off_diag_fun,
    diag_args = diag_args,
    off_diag_args = off_diag_args,
    lp = bayesplot::log_posterior(x), 
    np = bayesplot::nuts_params(x), 
    np_style = np_style,
    max_treedepth = nuts_max_treedepth(x)
  )
}

# Get max_treedepth or set it to 10
# @param x stanreg object
nuts_max_treedepth <- function(x) {
  cntrl <- x$stanfit@stan_args[[1]]$control %ORifNULL% list()
  cntrl[["max_treedepth"]] %ORifNULL% 10
}

# Warn if user specifies arguments that are automatically populated from the
# stanreg object
warn_unused_bayesplot_args <- function(...) {
  dots <- list(...)
  for (arg in c("np", "lp", "max_treedepth")) {
    if (arg %in% names(dots))
      warning("Ignoring user-specified argument '", arg ,
              "' because it is taken directly from the stanreg object.", 
              call. = FALSE)
  }
}
