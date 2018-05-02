# Monthly tslm forecasting ----------------------------------------------------------

# tslm_tr <-
#   df_train %>% select(date, yw, shop_id, item_id, item_cnt_day) %>%
#   group_by(yw, shop_id, item_id) %>%
#   summarize(total_sales = sum(item_cnt_day)) %>%
#   ungroup()
#
# tslm_test <- df_test %>%
#   select(yw, shop_id, item_id, item_cnt_day) %>%
#   group_by(yw, shop_id, item_id) %>%
#   summarize(total_sales = sum(item_cnt_day)) %>%
#   ungroup()

tslm_tr <-
  df_master %>% select(date, ym, shop_id, item_id, item_cnt_day) %>%
  group_by(ym, shop_id, item_id) %>%
  summarize(total_sales = sum(item_cnt_day)) %>%
  ungroup()

unique_shops <- tslm_tr$shop_id %>% unique()
unique_items <- tslm_tr$item_id %>% unique()
h <- 4

tslm_out <- vector(mode = 'list', length = length(unique_shops))
names(tslm_out) <- unique_shops
# for (i in seq_along(unique_shops)) {
#   tslm_out[[i]] <- matrix(data = 0, nrow=h, ncol = length(unique_items))
#   colnames(tslm_out[[i]]) <- unique_items
#   rownames(tslm_out[[i]]) <- seq_len(h)
# }

# For each shop
for(i in seq_along(unique_shops)){
  cat('\nProcessing shop', unique_shops[i])
  tr_data <- tslm_tr %>% filter(shop_id == unique_shops[i])
  tr_data_ts <- dcast(formula = ym+shop_id~item_id,data = tr_data,fun.aggregate = sum, value.var = 'total_sales', fill = 0)
  tr_data_ts <- ts(tr_data_ts[,-1:-2], start=c(2013,1), frequency = 12)
  tslm_yhats <- matrix(nrow=h, ncol = ncol(tr_data_ts),dimnames = list(seq_len(h),colnames(tr_data_ts)))
  # For each item within that shop
  for(j in 1:ncol(tr_data_ts)){
    cat('\n...Processing item', colnames(tr_data_ts)[j])
    ts_to_fit <- ts(tr_data_ts[, j], start=c(2013,01), frequency=12)
    fit <- tslm(ts_to_fit ~ trend + season, lambda = 'auto')
    fc <- forecast(fit, h=h)
    tslm_yhats[, j] <- as.numeric(fc$mean)
    tslm_yhats[, j] <- ifelse(tslm_yhats[, j]<0, 0, tslm_yhats[, j])
  }
  tslm_out[[i]] <- tslm_yhats
}
