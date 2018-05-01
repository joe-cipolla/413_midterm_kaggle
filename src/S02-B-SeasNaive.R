# Weekly seasonal naive method
# Basically pulls the total sales for the shop-item combination for the same week# from
# the previous year
# If the value doesn't exist, it's set to zero
snaive_tr <-
  df_train %>% select(date, yw, shop_id, item_id, item_cnt_day) %>%
  group_by(yw, shop_id, item_id) %>%
  summarize(total_sales = sum(item_cnt_day)) %>%
  ungroup()

snaive_test <- df_test %>%
  select(yw, shop_id, item_id, item_cnt_day) %>%
  group_by(yw, shop_id, item_id) %>%
  summarize(total_sales = sum(item_cnt_day)) %>%
  ungroup() %>%
  separate(yw, into = c('yr','wk'), sep = '-', convert = F, remove = F) %>%
  mutate(yr = as.numeric(yr) - 1,
         find_yw = paste0(yr,'-',wk)) %>%
  left_join(snaive_tr,by = c('find_yw' = 'yw', 'shop_id', 'item_id')) %>%
  tidyr::replace_na(replace = list('total_sales.y'=0)) %>%
  select(-find_yw,-yr,-wk) %>%
  rename(total_sales = total_sales.x,
         yhat = total_sales.y)

snaive_test

calc_rmse(snaive_test)
# [1] 7.619994


# Monthly seasonal naive method
# Basically pulls the total sales for the shop-item combination for the same month# from
# the previous year
# If the value doesn't exist, it's set to zero
snaive_tr <-
  df_train %>% select(date, ym, shop_id, item_id, item_cnt_day) %>%
  group_by(ym, shop_id, item_id) %>%
  summarize(total_sales = sum(item_cnt_day)) %>%
  ungroup()

snaive_test <- df_test %>%
  select(ym, shop_id, item_id, item_cnt_day) %>%
  group_by(ym, shop_id, item_id) %>%
  summarize(total_sales = sum(item_cnt_day)) %>%
  ungroup() %>%
  separate(ym, into = c('yr','mnth'), sep = '-', convert = F, remove = F) %>%
  mutate(yr = as.numeric(yr) - 1,
         find_ym = paste0(yr,'-',mnth)) %>%
  left_join(snaive_tr,by = c('find_ym' = 'ym', 'shop_id', 'item_id')) %>%
  tidyr::replace_na(replace = list('total_sales.y'=0)) %>%
  select(-find_ym,-yr,-mnth) %>%
  rename(total_sales = total_sales.x,
         yhat = total_sales.y)

snaive_test

calc_rmse(snaive_test)
# [1] 10.87307