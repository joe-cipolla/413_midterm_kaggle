# Monthly tslm forecasting ----------------------------------------------------------
tslm_tr <-
  df_master %>% select(date, ym, shop_id, item_id, item_cnt_day) %>%
  group_by(ym, shop_id, item_id) %>%
  summarize(total_sales = sum(item_cnt_day)) %>%
  ungroup()
tslm_tr

unique_shops <- tslm_tr$shop_id %>% unique()
unique_items <- tslm_tr$item_id %>% unique()
h <- 1

tslm_out <- vector(mode = 'list', length = length(unique_shops))
names(tslm_out) <- unique_shops

cl<-makeCluster(35)
registerDoParallel(cl)

# For each shop
for(i in seq_along(unique_shops)){
  cat('\nProcessing shop', unique_shops[i])
  tr_data <- tslm_tr %>% filter(shop_id == unique_shops[i])
  tr_data_ts <- dcast(formula = ym+shop_id~item_id,data = tr_data,fun.aggregate = sum, value.var = 'total_sales', fill = 0)
  tr_data_ts <- ts(tr_data_ts[,-1:-2], start=c(2013,1), frequency = 12)
  tslm_yhats <- matrix(nrow=h, ncol = ncol(tr_data_ts),dimnames = list(seq_len(h),colnames(tr_data_ts)))
  # For each item within that shop
  result <- foreach(j=1:ncol(tr_data_ts), .packages = 'forecast', .combine = 'rbind') %dopar% {
    cat('\n...Processing item', colnames(tr_data_ts)[j])
    ts_to_fit <- ts(tr_data_ts[, j], start=c(2013,01), frequency=12)
    fit <- tslm(ts_to_fit ~ trend + season)
    fc <- forecast(fit, h=h)
    tslm_yhats[, j] <- round(as.numeric(fc$mean),digits = 0)
    tslm_yhats[, j] <- ifelse(tslm_yhats[, j]<0, 0, tslm_yhats[, j])
  }
  rownames(result) <- colnames(tr_data_ts)
  df <- tbl_df(result) %>% rename(forecast=V1)
  df$item_id <- rownames(result)
  df$shop_id <- unique_shops[i]
  tslm_out[[i]] <- df
}

tslm_result_monthlyforecast <- purrr::reduce(.x = tslm_out, .f = bind_rows)
tslm_result_monthlyforecast

cache('tslm_result_monthlyforecast')
