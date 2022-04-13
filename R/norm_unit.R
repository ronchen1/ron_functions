norm_unit <- function(d) {
  # center and scale a matrix by turning into unit vector after centering
  d <- scale(d, scale = FALSE)
  d/t(matrix(replicate(nrow(d),sqrt(colSums(d^2))),nrow=ncol(d)))
  }