################################################################################
## for each multivariate time series ...
mk_test <- function(mult_ts){
  mk_list <- list()
  for( ts_idx in 1:ncol(mult_ts)){
    mk_list[[colnames(mult_ts[, ts_idx])]] = Kendall::MannKendall(mult_ts[, ts_idx])
  }
  mk_list
}
##  for each element in list of multivariate time series ...
list_mk_test <- function(ts_list){
  ml_list <- list()
  for( idx in 1:length(ts_list)){ # for( (idx, object) in list) ??
    print(idx)
    ml_list[[names(ts_list)[idx]]] = mk_test(ts_list[[idx]])
  }
  ml_list
}
################################################################################
#kendall_list <- list_mk_test(ts_list)
kendall_list <- lapply(X = ts_flat, FUN = Kendall::MannKendall)
dtbl <- rbindlist(kendall_list, fill = TRUE)
dtbl[, name := names(kendall_list)]


####
sheet_index_tbl <- lapply(X = names(kendall_list), FUN = function(x) {
  unlist(strsplit(x, ".xlsx__"))
})

stri <- names(kendall_list)[[1]]
unlist(strsplit(stri, ".xlsx__"))
lapply(X = names(kendall_list), FUN = function(x) {
  unlist(strsplit(x, ".xlsx__"))
})