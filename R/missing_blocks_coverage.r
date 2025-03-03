#' @title missing_blocks_coverage
#'
#' @description Finds the missing proportion value as number of blocks increase
#'
#' @param data_in A data set object
#' @param low_threshold The total percentage of missing values permissible in the final residual dataset after excluding the missing blocks.  
#' Default is 0.05 or 5% (i.e., if there are 10 rows and 10 columns, 5 or less cells may be empty) 
#' @param n The number of iterations to use to identfy the number of missing blocks. Default is 10
#'
#' @return Missing proportion value as a function of number of blocks. The lower number of blocks is 1 (i.e, entire dataset is one block),
#' and highest number of blocks is equal to the number of columns
#' @examples
#' data(Bike_50)
#' missing_prop <- missing_blocks_coverage(Bike_50)
#' @export
#' @importFrom 
missing_blocks_coverage <- function(data_in,low_threshold = 0.05, n=10)
{

  ## For larger datasets, we consider representative samples: 
  if(nrow(data_in) > 50000)
  {
     set.seed(12345)
     data_in = data_in[sample(nrow(data_in),50000,replace=FALSE ),]
  }

  data_present <- data.frame( matrix(as.integer(!is.na(data_in)),nrow = nrow(data_in), ncol = ncol(data_in))) 
  names(data_present) <- names(data_in) 
  
  missing_prop <- function(MVI,k, low_threshold, n=10)
  {
    determine_best_cluster <- function(data_in,clus_number, n=10)
    {
      # n <- 10
      kmeans_o <- lapply(1:n,function(x) { set.seed(x); a <- kmeans(data_in,clus_number); return(a) } ) 
      opt_kmeans <- which.max(lapply(kmeans_o,function(x) {100-x$tot.withinss/x$totss*100})) 
      best_cluster <- kmeans_o[[opt_kmeans]]
      return(best_cluster)
    }	
    
    member_clusters <- determine_best_cluster(MVI,k)
    members <- member_clusters$cluster
    data_subs_1 <- split(MVI,members)  
    data_subs_2 <- list(length(data_subs_1)) ## This is for identifying the columns
    missingness <- vector(length = length(data_subs_1))
    for(list in 1:length(data_subs_1))
    {
	data_subs_1[[list]] = data.frame(data_subs_1[[list]])
      completeness <- colSums(data_subs_1[[list]])
      data_subs_2[[list]] <- data_subs_1[[list]][,which(completeness > (low_threshold)*nrow(data_subs_1[[list]])) ]  
	  data_subs_2[[list]] = data.frame(data_subs_2[[list]])
	  if(ncol(data_subs_2[[list]]) > 1)
	  {
	  missing_vals <- nrow(data_subs_2[[list]]) - colSums(data_subs_2[[list]])
      }
       else {  	missing_vals <- nrow(data_subs_2[[list]]) - sum(data_subs_2[[list]])  } 
      missingness[list] <- sum(missing_vals)
    }
	missing_C = sum(missingness)/(dim(MVI)[1]*dim(MVI)[2]) 
    return( missing_C )
  }
  if( ncol(data_in) <= 50)
  {
  M <- (ncol(data_in))  
  }
  else  { M = 50  }  ## Rarely do we come across more than 20 independent data sources
  
  missing_prop_val <- vector(length=M)
  for(k in 1:M)
  {
    missing_prop_val[k] <- missing_prop(data_present,k, low_threshold)
  }
  return(missing_prop_val)
}
