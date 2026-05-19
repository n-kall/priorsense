##' @srrstats {G2.3b} case sensitive is indicated
##' @param div_measure Character (case sensitive) specifying the
##'   divergence measure to use. The following methods are
##'   implemented:
##'
##' * `"cjs_dist"`: Cumulative Jensen-Shannon distance. Default
##'   method. See function [cjs_dist()] for more details.
##'
##' * `"js_dist"`: Jensen-Shannon distance. First estimates density using
##' [stats::density()]. Requires the `philentropy` package. See
##' [philentropy::jensen_shannon()].
##'
##' * `"js_div"`: Jensen-Shannon divergence. First estimates density using
##' [stats::density()]. Requires the `philentropy` package. See
##' [philentropy::jensen_shannon()].
##'
##' * `"hellinger_dist"`: Hellinger distance. First estimates density using
##' [stats::density()]. Requires the `philentropy` package. See
##' [philentropy::hellinger()].
##'
##' * `"kl_dist"`: Kullback-Leibler distance. First esimates density using
##' [stats::density()]. Requires the `philentropy` package. See
##' [philentropy::kullback_leibler_distance()].
##'
##' * `"kl_div"`: Kullback-Leibler divergence. First estimates density using
##' [stats::density()]. Requires the `philentropy` package. See
##' [philentropy::kullback_leibler_distance()].
##' 
##' * `"ks_dist"`: Kolmogorov-Smirnov distance. See [stats::ks.test()].
##'
##' * `"ws_dist"`: Wassterstein distance (pass `measure_args = list(p = N)`) for
##' a different order, where N is the order. Requires the `transport`
##' package. See [transport::wasserstein1d].
##' @param measure_args Named list of further arguments passed to divergence
##'   measure functions.
