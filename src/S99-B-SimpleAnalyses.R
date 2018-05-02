df_train
top_items
top_shops

ts <- df_train %>%
  filter(shop_id == top_shops[1],
         item_id == top_items[1]) %>%
  mutate(ym = paste0(year, str_pad(month,width = 2,side = 'left',pad = '0')),
         yw = paste0(year, str_pad(week,width = 2,side = 'left',pad = '0')))
ts_1 <- aggregate(cbind(item_cnt_day, weekend, holiday)~yw, ts, FUN = sum)
ts_1 <- ts(ts_1[,-1], start = c(2013,17), frequency = 52)
plot(ts_1)
ggtsdisplay(ts_1[,'item_cnt_day'])
ggtsdisplay(diff(ts_1))
stlf(ts_1, s.window = 'period') %>% forecast(h=4*6) %>% autoplot()
auto.arima(ts_1[,'item_cnt_day'], stepwise = F, xreg = ts_1[,c('weekend','holiday')]) %>%
  forecast(h=5, xreg = matrix(c(2, 0, 2, 4, 2, 3, 2, 0, 2, 0, 2, 0, 2, 1, 2, 4, 2, 0, 2, 0, 2, 0), ncol = 2, byrow = T)) %>% autoplot()

