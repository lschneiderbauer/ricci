`%c%` <- function(f1, f2) {
  function(...) {
    f1(f2(...))
  }
}
