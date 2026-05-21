all_clear <- function(){
  stop_r5()
  gc()
  .jgc(R.gc = TRUE)
}
