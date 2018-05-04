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

# Weekly tslm forecasting ----------------------------------------------------------
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

df <- tbl_df(c3_autoarima_out)
colnames(df) <- paste0('h', 1:4)
df$shop_id <- unique_shops

cache('c3_autoarima_out')
