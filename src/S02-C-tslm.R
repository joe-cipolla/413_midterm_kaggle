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
names(tslm_out) <- paste0('shop_id_',unique_shops)
for (i in seq_along(unique_shops)) {
  tslm_out[[i]] <- matrix(data = 0, nrow=h, ncol = length(unique_items))
  colnames(tslm_out[[i]]) <- unique_items
  rownames(tslm_out[[i]]) <- seq_len(h)
}

# For each shop
for(i in seq_along(unique_shops)){
  tr_data <- tslm_tr %>% filter(shop_id == unique_shops[i]) %>% select(-shop_id)
  tr_data_ts <- dcast(formula = ym~item_id,data = tr_data,fun.aggregate = sum, value.var = 'total_sales', fill = 0)
  tslm_yhats <- matrix(nrow=h, ncol = ncol(tr_data_ts))
  # For each item within that shop
  for(j in 2:ncol(tr_data_ts)){
    ts_to_fit <- ts(tr_data_ts[, j], frequency=52)
    fit <- tslm(ts_to_fit ~ trend + season)
    fc <- forecast(fit, h=h)
    tslm_yhats[, j] <- as.numeric(fc$mean)
    tslm_yhats[, j] <- ifelse(tslm_yhats[, j]<0, 0, tslm_yhats[, j])
  }
  tslm_out[[i]] <- tslm_yhats
}


# %>%
#   separate(yw, into = c('yr','wk'), sep = '-', convert = F, remove = F) %>%
#   mutate(yr = as.numeric(yr) - 1,
#          find_yw = paste0(yr,'-',wk)) %>%
#   left_join(snaive_tr,by = c('find_yw' = 'yw', 'shop_id', 'item_id')) %>%
#   tidyr::replace_na(replace = list(total_sales.y=0)) %>%
#   select(-find_yw,-yr,-wk) %>%
#   rename(total_sales = total_sales.x,
#          yhat = total_sales.y)

snaive_test

calc_rmse(snaive_test)
# [1] 7.619994