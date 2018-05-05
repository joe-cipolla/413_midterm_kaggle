# How does the distribution of items vary along time? ---------------------

df_master %>%
  group_by(date_block_num, itemcat_lvl1) %>%
  summarise(total_sales = sum(item_cnt_day)) %>%
  ungroup() %>%
  group_by(date_block_num) %>%
  mutate(sales_prop = total_sales / sum(total_sales)) %>%
  ggplot() +
  geom_area(
    aes(x = date_block_num,
        y = sales_prop,
        fill = itemcat_lvl1),
    size = .5,
    color = 'black'
  ) -> p
plotly::ggplotly(p)

df_master %>%
  group_by(date_block_num, itemcat_lvl1) %>%
  summarise(total_sales = sum(item_cnt_day)) %>%
  ungroup() %>%
  group_by(date_block_num) %>%
  mutate(sales_prop = total_sales / sum(total_sales)) %>%
  ggplot() +
  geom_line(aes(x = date_block_num,
                y = sales_prop,
                color = itemcat_lvl1),
            size = 1) -> p
plotly::ggplotly(p)

df_master %>%
  group_by(date_block_num, shop_id) %>%
  summarise(total_sales = sum(item_cnt_day)) %>%
  ungroup() %>%
  group_by(date_block_num) %>%
  mutate(sales_prop = total_sales / sum(total_sales)) %>%
  ggplot() +
  geom_area(aes(x = date_block_num,
                y = sales_prop,
                fill = shop_id),
            color = 'black',
            size = .5) -> p
plotly::ggplotly(p)

df_master %>%
  group_by(date_block_num, item_category_id, item_id) %>%
  summarise(total_sales = sum(item_cnt_day)) %>%
  ungroup() %>%
  group_by(date_block_num, item_category_id) %>%
  mutate(sales_prop = total_sales / sum(total_sales)) %>%
  filter(item_category_id==5) %>%
  ggplot() +
  geom_area(aes(x = date_block_num,
                y = sales_prop,
                fill = item_id),
            color = 'black',
            size = .5) -> p
plotly::ggplotly(p)

df_master %>%
  group_by(yw, shop_id) %>%
  summarise(total_sales = sum(item_cnt_day)) %>%
  # ungroup() %>%
  # group_by(yw) %>%
  # mutate(sales_prop = total_sales / sum(total_sales)) %>%
  ungroup() %>%
  filter(shop_id=='S36') %>%
  separate(yw, sep = '-', into = c('yr','wk'), remove = F) %>%
  mutate(dt =  lubridate::weeks(wk) + lubridate::ymd(paste0(yr,'01-01'))) %>%
  ggplot() +
  geom_line(aes(x = dt,
                y = total_sales,
                color = shop_id),
            # color = 'black',
            size = 1) +
  geom_point(aes(x = dt,
                y = total_sales,
                color = shop_id),
            # color = 'black',
            size = 4) +
  scale_x_date(date_breaks = '1 month',
               date_minor_breaks = '1 week')+
  theme(axis.text.x = element_text(angle = 90))

# Weekly forecast using auto-arima ----------------------------------------

c3_tr <-
  df_master %>% select(date, yw, shop_id, item_cnt_day, is_december) %>%
  group_by(yw, shop_id) %>%
  summarize(total_sales = sum(item_cnt_day),
            is_december = sum(is_december)>0) %>%
  ungroup() %>%
  separate(yw, sep = '-', into = c('yr','wk'), remove = F) %>%
  mutate(
    # wk48 = as.numeric(wk == '48'),
    # wk49 = as.numeric(wk == '49'),
    # wk50 = as.numeric(wk == '50'),
    wk51 = as.numeric(wk == '51'),
    wk52 = as.numeric(wk == '52'),
    wk53 = as.numeric(wk == '53'),
    wk01 = as.numeric(wk == '01')
  )

unique_shops <- c3_tr$shop_id %>% unique()
h <- 4

# Shop S36 results in a rank deficient matrix for xreg since it only has 3 observations even at a week level
# Removing S36 from here

unique_shops <- head(unique_shops, -1)

cl<-makeCluster(35)
registerDoParallel(cl)

tslm_yhats <- vector(mode = 'list', length = length(unique_shops))
for(i in seq_along(unique_shops)) {
  cat('\nProcessing shop', unique_shops[i])
  tr_data <- c3_tr %>% filter(shop_id == unique_shops[i])
  tr_data_ts <-
    dcast(
      formula = yw ~ shop_id,
      data = tr_data,
      fun.aggregate = sum,
      value.var = 'total_sales',
      fill = 0
    )
  tr_data_ts <- ts(tr_data_ts[, -1],
                   start = c(2013, 1),
                   frequency = 52)
  fit <-
    tr_data_ts %>% auto.arima(stepwise = F,
                              xreg = as.matrix(tr_data[, 7:10]),
                              seasonal = T)
  fit %>% summary
  xr <-
    tr_data %>% select(-yw, -yr, -shop_id, -total_sales, -is_december) %>% distinct() %>%
    filter(wk %in% as.character(45:48)) %>% select(-wk) %>% as.matrix()
  fc <- fit %>% forecast(h = h, xreg = xr)
  autoplot(fc)
  tslm_yhats[[i]] <- round(as.numeric(fc$mean), digits = 0)
  tslm_yhats[[i]] <- ifelse(tslm_yhats[[i]] < 0, 0, tslm_yhats[[i]])
}

c3_autoarima_out <- purrr::reduce(.x = tslm_yhats, .f = rbind)
row.names(c3_autoarima_out) <- unique_shops
c3_autoarima_out

cache('c3_autoarima_out')

df <- tbl_df(c3_autoarima_out)
colnames(df) <- paste0('h', 1:4)
df$shop_id <- as.numeric(str_extract(unique_shops,'[0-9].*'))
df <- df %>% transmute(shop_id, total_nov_sales=h1+h2+h3+h4)

item_pc_sales_per_shop_in_october <- df_master %>%
  filter(month %in% c(5,6,7,8,9,10), year==2015) %>%
  select(shop_id, item_id, item_cnt_day) %>%
  group_by(shop_id, item_id) %>%
  summarize(total_itemsales_per_shop = sum(item_cnt_day)) %>%
  ungroup() %>%
  group_by(shop_id) %>%
  mutate(total_sales_per_shop = sum(total_itemsales_per_shop)) %>%
  ungroup() %>%
  mutate(item_sales_as_pc_of_total = total_itemsales_per_shop/total_sales_per_shop,
         shop_id = as.numeric(str_extract(shop_id,'[0-9].*')),
         item_id = as.numeric(str_extract(item_id,'[0-9].*')))

estimated_nov_sales <- item_pc_sales_per_shop_in_october %>%
  left_join(df) %>%
  mutate(estimated_sales_in_nov = round(item_sales_as_pc_of_total * total_nov_sales))

c3_weekly_testout <- kaggle_test %>%
  left_join(estimated_nov_sales) %>%
  select(id=ID, shop_id, item_id, item_cnt_month = estimated_sales_in_nov)
c3_weekly_testout

# For those shops which are still NA, let's try assigning an average sales value per month per shop
# calculated not at a shop_id+item_id level, but just at an item_id level
# Removing the month of Dec since it'll inflate the values

average_item_sales_per_month <- df_master %>% filter(month != 12) %>% group_by(item_id, month) %>%
  summarize(total_item_sales_by_month = sum(item_cnt_day, na.rm = T)) %>%
  group_by(item_id) %>%
  summarise(no_of_months = n(),
            total_item_sales = sum(total_item_sales_by_month)) %>%
  transmute(item_id = as.numeric(str_extract(item_id,'[0-9].*')),
            average_item_sales_per_month = total_item_sales / no_of_months,
            average_item_sales_per_month_per_shop = average_item_sales_per_month / length(unique(df_master$shop_id))) %>%
  select(-average_item_sales_per_month)

c3_weekly_testout <- c3_weekly_testout %>%
  left_join(average_item_sales_per_month) %>%
  mutate(item_cnt_month=ifelse(is.na(item_cnt_month), average_item_sales_per_month_per_shop, item_cnt_month)) %>%
  select(-average_item_sales_per_month_per_shop)
c3_weekly_testout

map_int(c3_weekly_testout, ~sum(is.na(.x)))

# What are the remaining NA items?
unfound_item_id <- c3_weekly_testout %>% filter(is.na(item_cnt_month)) %>% distinct(item_id) %>% pull(item_id)
length(unfound_item_id)
# Do they even exist in df_master?
table(unfound_item_id %in% as.numeric(str_extract(df_master$item_id,'[0-9].*')))

# Only 7 are found in the master! (Probably in the month of December)
# How should I tackle these 'seen-for-the-first-time' item_ids?
# Perhaps I can use the item_category_id, or just set them to zero.
# I'm setting them to zero :)
c3_weekly_testout$item_cnt_month[is.na(c3_weekly_testout$item_cnt_month)] <- 0
c3_weekly_testout

c3_weekly_testout %>%
  select(id, item_cnt_month) %>%
  write_csv(path = 'logs/c3_weekly_testout.csv',col_names = T)
cache('c3_weekly_testout')

#Kaggle score 3.77304


# Monthly forecast using auto-arima ----------------------------------------

c3_tr <-
  df_master %>% select(date, ym, shop_id, item_cnt_day) %>%
  group_by(ym, shop_id) %>%
  summarize(total_sales = sum(item_cnt_day)) %>%
  ungroup() %>%
  separate(ym, sep = '-', into = c('yr','mnth'), remove = F) %>%
  mutate(
    is_nov = as.numeric(mnth == '11'),
    is_dec = as.numeric(mnth == '12'),
    is_jan = as.numeric(mnth == '01')
  ) %>%
  bind_rows(list(ym='2015-02',
            yr='2015',
            mnth='02',
            shop_id='S10',
            total_sales=0,
            is_nov=0, is_dec=0, is_jan=0
            ))

unique_shops <- c3_tr$shop_id %>% unique()
h <- 1

# Shop S36 results in a rank deficient matrix for xreg since it only has 3 observations even at a week level
# Removing S36 from here
unique_shops <- head(unique_shops, -1)

cl<-makeCluster(4)
registerDoParallel(cl)

tslm_yhats <- vector(mode = 'list', length = length(unique_shops))
for(i in seq_along(unique_shops)) {
  cat('\nProcessing shop', unique_shops[i])
  tr_data <- c3_tr %>% filter(shop_id == unique_shops[i])
  tr_data_ts <-
    dcast(
      formula = ym ~ shop_id,
      data = tr_data,
      fun.aggregate = sum,
      value.var = 'total_sales',
      fill = 0
    )
  tr_data_ts <- ts(tr_data_ts[, -1],
                   start = c(2013, 1),
                   frequency = 12)
  fit <-
    tr_data_ts %>% auto.arima(stepwise = F,
                              xreg = as.matrix(tr_data[, 6:8]),
                              seasonal = T)
  fit %>% summary
  xr <- matrix(c(1,0,0), nrow = 1)
  fc <- fit %>% forecast(h = h, xreg = xr)
  autoplot(fc)
  tslm_yhats[[i]] <- round(as.numeric(fc$mean), digits = 0)
  tslm_yhats[[i]] <- ifelse(tslm_yhats[[i]] < 0, 0, tslm_yhats[[i]])
}

c3b_autoarima_out <- purrr::reduce(.x = tslm_yhats, .f = rbind)
row.names(c3b_autoarima_out) <- unique_shops
c3b_autoarima_out

cache('c3b_autoarima_out')

df <- tbl_df(c3b_autoarima_out)
colnames(df) <- 'total_nov_sales'
df$shop_id <- as.numeric(str_extract(unique_shops,'[0-9].*'))

item_pc_sales_per_shop_over_6_months <- df_master %>%
  filter(month %in% c(5,6,7,8,9,10), year==2015) %>%
  select(shop_id, item_id, item_cnt_day) %>%
  group_by(shop_id, item_id) %>%
  summarize(total_itemsales_per_shop = sum(item_cnt_day)) %>%
  ungroup() %>%
  group_by(shop_id) %>%
  mutate(total_sales_per_shop = sum(total_itemsales_per_shop)) %>%
  ungroup() %>%
  mutate(item_sales_as_pc_of_total = total_itemsales_per_shop/total_sales_per_shop,
         shop_id = as.numeric(str_extract(shop_id,'[0-9].*')),
         item_id = as.numeric(str_extract(item_id,'[0-9].*')))

estimated_nov_sales <- item_pc_sales_per_shop_over_6_months %>%
  left_join(df) %>%
  mutate(estimated_sales_in_nov = round(item_sales_as_pc_of_total * total_nov_sales))

c3_monthly_testout <- kaggle_test %>%
  left_join(estimated_nov_sales) %>%
  select(id=ID, shop_id, item_id, item_cnt_month = estimated_sales_in_nov)
c3_monthly_testout

# For those shops which are still NA, let's try assigning an average sales value per month per shop
# calculated not at a shop_id+item_id level, but just at an item_id level
# Removing the month of Dec since it'll inflate the values

average_item_sales_per_month <- df_master %>% filter(month != 12) %>% group_by(item_id, month) %>%
  summarize(total_item_sales_by_month = sum(item_cnt_day, na.rm = T)) %>%
  group_by(item_id) %>%
  summarise(no_of_months = n(),
            total_item_sales = sum(total_item_sales_by_month)) %>%
  transmute(item_id = as.numeric(str_extract(item_id,'[0-9].*')),
            average_item_sales_per_month = total_item_sales / no_of_months,
            average_item_sales_per_month_per_shop = average_item_sales_per_month / length(unique(df_master$shop_id))) %>%
  select(-average_item_sales_per_month)

c3_monthly_testout <- c3_monthly_testout %>%
  left_join(average_item_sales_per_month) %>%
  mutate(item_cnt_month=ifelse(is.na(item_cnt_month), average_item_sales_per_month_per_shop, item_cnt_month)) %>%
  select(-average_item_sales_per_month_per_shop)
c3_monthly_testout

map_int(c3_monthly_testout, ~sum(is.na(.x)))

# What are the remaining NA items?
unfound_item_id <- c3_monthly_testout %>% filter(is.na(item_cnt_month)) %>% distinct(item_id) %>% pull(item_id)
length(unfound_item_id)
# Do they even exist in df_master?
table(unfound_item_id %in% as.numeric(str_extract(df_master$item_id,'[0-9].*')))

# Only 7 are found in the master! (Probably in the month of December)
# How should I tackle these 'seen-for-the-first-time' item_ids?
# Perhaps I can use the item_category_id, or just set them to zero.
# I'm setting them to zero :)
c3_monthly_testout$item_cnt_month[is.na(c3_monthly_testout$item_cnt_month)] <- 0
c3_monthly_testout

c3_monthly_testout %>%
  select(id, item_cnt_month) %>%
  write_csv(path = 'logs/c3_monthly_testout.csv',col_names = T)
cache('c3_monthly_testout')

#Kaggle score ?