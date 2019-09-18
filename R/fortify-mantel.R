#' Fortify a mantel's test data to data.frame
#' @description
#' \code{fortify_mantel()}  provides a unified interface for mantel's test, specifying different
#' distance function (now support \code{\link[stats]{dist}} and \code{\link[vegan]{vegdist}}) and different mantel's test functions (
#' (now support \code{\link[vegan]{mantel}}, \code{\link[ade4]{mantel.rtest}} and \code{\link[ade4]{mantel.randtest}}).
#'
#' @param gdis a list of dissimilarity matrices, data.frame or a dist objects.
#' @param mdis a list of dissimilarity matrices, data.frame or a dist objects.
#' @param gdist_fun a string of distance function, it is used to handle `gdis` parameter.
#' @param mdist_fun a string of distance function, it is used to handle `mdis` parameter.
#' @param mantel_fun a string of mantel's test function.
#' @param gdist_params a list of extra params passing to `gdist_fun`.
#' @param mdist_params a list of extra params passing to `mdist_fun`.
#' @param process logical, if TRUE cat processing bar.
#' @param ... a list of extra params passing to `mantel_fun`.
#'
#' @details
#' For every element in `gdis` and `mdis`, if no unique name is specified, it will be automatically setted by
#' "gdis" or "mdis" with a incremental number.
#'
#' @seealso
#' \code{\link[vegan]{vegdist}}, \code{\link[vegan]{mantel}}, \code{\link[ade4]{mantel.rtest}} and
#' \code{\link[ade4]{mantel.randtest}}.
#' @examples
#' library(vegan)
#' data(varespec)
#' data(varechem)
#' fortify_mantel(varechem, list(spec = varespec))
#' @importFrom stats dist
#' @importFrom vegan vegdist
#' @importFrom vegan mantel
#' @importFrom ade4 mantel.rtest
#' @importFrom ade4 mantel.randtest
#' @export
fortify_mantel <- function(gdis,
                           mdis,
                           gdist_fun = "vegdist",
                           mdist_fun = "vegdist",
                           mantel_fun = "mantel",
                           gdist_params = list(),
                           mdist_params = list(),
                           process = TRUE,
                           ...
)
{
  if(!is.list(gdis))
    stop("`gdis` need a list.", call. = FALSE)
  if(!is.list(mdis))
    stop("`mdis` need a list.", call. = FALSE)
  mantel_extra_params <- list(...)
  if(!all(vapply(gdis, inherits, logical(1), "dist")))
      dist_x_params <- modify_list2(dist_params(gdist_fun), gdist_params, )
  if(!all(vapply(mdis, inherits, logical(1), "dist")))
      dist_y_params <- modify_list2(dist_params(mdist_fun), mdist_params)
  gdis <- make_list_names(gdis, pre = "gdis")
  mdis <- make_list_names(mdis, pre = "mdis")
  nm_gdis <- names(gdis)
  nm_mdis <- names(mdis)
  n <- length(gdis)
  m <- length(mdis)
  len <- n * m
  stat <- rep_len(NA, len)
  sign <- rep_len(NA, len)
  if(process)
    cat("Compute mantel statistic, please wait a moment:\n")
  for(i in seq_len(n)) {
    if(!inherits(gdis[[i]], "dist"))
      x_dist <- do.call(gdist_fun, modify_list(x = gdis[[i]], params = dist_x_params))
    for (j in seq_len(m)) {
      idx <- (i - 1) * m + j
      if(process)
        cat_message(idx, len)
      if(!inherits(mdis[[j]], "dist"))
        y_dist <- do.call(mdist_fun, modify_list(x = mdis[[j]], params = dist_y_params))
      mantel_data_params <- switch (mantel_fun,
        mantel = modify_list(xdis = x_dist, ydis = y_dist, params = mantel_extra_params),
        mantel.rtest = modify_list(m1 = x_dist, m2 = y_dist, params = mantel_extra_params),
        mantel.randtest = modify_list(m1 = x_dist, m2 = y_dist, params = mantel_extra_params)
      )
      mantel <- do.call(mantel_fun, mantel_data_params)
      if(mantel_fun == "mantel") {
        stat[idx] <- mantel[["statistic"]]
        sign[idx] <- mantel[["signif"]]
      } else {
        stat[idx] <- mantel[["obs"]]
        sign[idx] <- mantel[["pvalue"]]
      }
    }
  }
  df <- data.frame(gdis = rep(nm_gdis, each = m),
             mdis = rep(nm_mdis, n),
             statistic = stat,
             signif = sign,
             stringsAsFactors = FALSE)
  class(df) <- c("mantel_df", "data.frame")
  df

}

#' @noRd
dist_params <- function(.f) {
  veg_dist_params <- list(method = "bray",
                          binary = FALSE,
                          diag = FALSE,
                          upper = FALSE,
                          na.rm = FALSE)
  stat_dist_params <- list(method = "euclidean",
                           diag = FALSE,
                           upper = FALSE,
                           p = 2)
  switch (.f,
    vegdist = veg_dist_params,
    dist = stat_dist_params
  )

}
