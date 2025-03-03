#' @title missing_block_count
#' @description Identifies the number of missing blocks in an incomplete dataset with blockwise missing patterns
#' @param A vector of missing proportion values output using the function missing_blocks_coverage  
#' @return Number of blocks for which missing proportion values is optimal (i.e., first smallest first-order derivative)
#' @examples
#' data(Bike_50)
#' missing_prop <- missing_blocks_coverage(Bike_50)
#' missing_block_count(missing_prop)
#' @export
#' @importFrom 
missing_block_count <- function(missing_prop_val)
{
  x_values = seq(1,length(missing_prop_val))
  y_values = missing_prop_val
  
  # Max values to create line
  max_x_x <- max(x_values)
  max_x_y <- y_values[which.max(x_values)]
  max_y_y <- max(y_values)
  max_y_x <- x_values[which.max(y_values)]
  max_df <- data.frame(x = c(max_y_x, max_x_x), y = c(max_y_y, max_x_y))
  
  # Creating straight line between the max values
  fit <- lm(max_df$y ~ max_df$x)
  
  # Distance from point to line
  distances <- c()
  for(i in 1:length(x_values)) {
    distances <- c(distances, abs(coef(fit)[2]*x_values[i] - y_values[i] + coef(fit)[1]) / sqrt(coef(fit)[2]^2 + 1^2))
  }
  
  # Max distance point
  x_max_dist <- x_values[which.max(distances)]
  y_max_dist <- y_values[which.max(distances)]
  
  return(c(x_max_dist))
}
