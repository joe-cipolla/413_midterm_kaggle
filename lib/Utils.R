calc_rmse <- function(df){
  (sum((df$total_sales - df$yhat)^2)/nrow(df))^0.5
}
