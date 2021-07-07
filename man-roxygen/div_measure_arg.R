##' @param div_measure The divergence measure to use. The
##'   following methods are implemented:
##'
##' * `"cjs_dist"`: Cumulative Jensen-Shannon distance. Default
##'   method. See function `cjs_dist` for more details.
##'
##' * `"js_dist"`: Jensen-Shannon distance.
##'
##' * `"js_div"`: Jensen-Shannon divergence.
##'
##' * `"hellinger_dist"`: Hellinger distance.
##'
##' * `"kl_dist"`: Kullback-Leibler distance.
##'
##' * `"kl_div"`: Kullback-Leibler divergence.
##' 
##' * `"ks_dist"`: Kolmogorov-Smirnov distance.
##'
##' * `"hellinger_dist"`: Hellinger distance.
##'
##' * `"ws_dist"`: Wassterstein distance (pass `measure_args = list(p = N)`)
##' for a different order, where N is the order.
##' @param measure_args Named list of further arguments passed to divergence measure functions.
