#' 
#' 
#' Row-wise Box-Cox Transformation 
#' 
#' @param admissions Vector of class `numeric` for the number of cases admitted.
#' 
#' @param lsystem Vector of class `character` for the livelihood systems in the 
#'    dataset.
#' 
#' @returns A vector of class `double` with `lsystem` specific box-cox values.
#' 


row_wise_box_cox <- function(admissions, lsystems) {

  lsys_levels <- levels(factor(lsystems))
  lambdas <- c(-0.1569491, 0.04069383, 0.4264235, -0.5802248, -0.6727553)
  
  # Initialize output vector with same length as admissions
  .admissions <- numeric(length(admissions))
  
  for (i in seq_along(lsys_levels)) {
    lsys <- lsys_levels[i]
    lambda <- lambdas[i]
    
    # Apply Box-Cox to subset of admissions by lsystems category
    idx <- lsystems == lsys
    .admissions[idx] <- box_cox(admissions[idx], lambda = lambda)
  }
  
  .admissions
}