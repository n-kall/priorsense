##' @param prior_selection Vector specifying partitions of component to be
##'   included in power-scaling. Default is NULL, which takes all partitions. If
##'   this is a character, then it is appended to the variable name (specified
##'   by `log_prior_name`) with a separator between them. If numeric, then it is
##'   appended inside `[]`.
##' @param likelihood_selection Vector specifying partitions of component to be
##'   included in power-scaling. Default is NULL, which takes all partitions. If
##'   this is a character, then it is appended to the variable name (specified
##'   by `log_lik_name`) with a separator between them. If numeric, then it is
##'   appended inside `[]`.
##' @param separator Character specifying the separator between the component
##'   name and the partition. The default is `"_"`.
