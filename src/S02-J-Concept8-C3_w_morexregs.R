# # Data cleanup ----------------------------------------------------------------------
#
# # Based on the EDA done so far, removing these item level 1 categories from the dataset. These item cats
# # are one off sales and do not trend / affect the Nov forecast. They will only make it more difficult to
# # forecast
#
# itemcat_lvl1_to_remove <- c('Tickets (figure)','Official')
# spiky_but_might_be_able_to_model <- c('Delivery of goods')
#
# df_master %>%
#   mutate(flag = itemcat_lvl1 %in% itemcat_lvl1_to_remove) %>%
#   filter(!flag) %>%
#   select(-flag) -> c8_df_master
# c8_df_master
#
# # Weekly forecast using auto-arima ----------------------------------------
# table1 <- c8_df_master %>%
#   group_by(yw, shop_id) %>%
#   summarise(weekly_item_sales = sum(item_cnt_day)) %>%
#   ungroup()
# table2 <- c8_df_master %>% distinct(yw, is_december,holiday,weekend)
# table3 <- c8_df_master %>%
#   distinct(shop_id,loc_lvl1,loc_lvl2,shop_center_lgl,shop_cluster_id,sec_lgl,tc_lgl,trc_lgl,trk_lgl)
# # table4 <- c8_df_master %>%
# #   distinct(item_category_id, itemcat_lvl1, itemcat_lvl2)
#
# c8_tr <- table1 %>%
#   left_join(table2) %>%
#   left_join(table3) %>%
#   separate(yw, sep = '-', into = c('yr','wk'), remove = F) %>%
#   mutate(
#     wk51 = as.numeric(wk == '51'),
#     wk52 = as.numeric(wk == '52'),
#     wk53 = as.numeric(wk == '53'),
#     wk01 = as.numeric(wk == '01')
#   )
#
# unique_shops <- c8_tr$shop_id %>% unique()
# h <- 4
#
# # Shop S36 results in a rank deficient matrix for xreg since it only has 3 observations even at a week level
# # Removing S36 from here
# unique_shops <- head(unique_shops, -1)
#
# # cl<-makeCluster(35)
# # registerDoParallel(cl)
#
# autoarima_yhats <- vector(mode = 'list', length = length(unique_shops))
# for(i in seq_along(unique_shops)) {
#   cat('\nProcessing shop', unique_shops[i])
#   tr_data <- c8_tr %>% filter(shop_id == unique_shops[i])
#   tr_data_ts <-
#     dcast(
#       formula = yw ~ shop_id,
#       data = tr_data,
#       fun.aggregate = sum,
#       value.var = 'weekly_item_sales',
#       fill = 0
#     )
#   tr_data_ts <- ts(tr_data_ts[, -1],
#                    start = c(2013, 1),
#                    frequency = 52)
#
#   fit <-
#     tr_data_ts %>% auto.arima(stepwise = F,
#                               xreg = as.matrix(tr_data[, 7:10]),
#                               seasonal = T)
#   fit %>% summary
#   xr <-
#     tr_data %>% select(-yw, -yr, -shop_id, -total_sales, -is_december) %>% distinct() %>%
#     filter(wk %in% as.character(45:48)) %>% select(-wk) %>% as.matrix()
#   fc <- fit %>% forecast(h = h, xreg = xr)
#   autoplot(fc)
#   autoarima_yhats[[i]] <- as.numeric(fc$mean)
#   autoarima_yhats[[i]] <- ifelse(autoarima_yhats[[i]] < 0, 0, autoarima_yhats[[i]])
# }
#
# c3_autoarima_out <- purrr::reduce(.x = autoarima_yhats, .f = rbind)
# row.names(c3_autoarima_out) <- unique_shops
# c3_autoarima_out
#
# c3_autoarima_out <- rbind(c3_autoarima_out,S36=rep(c8_tr %>% filter(shop_id=='S36') %>% summarize(mean(total_sales)) %>% pull(),4))
#
# cache('c3_autoarima_out')
#
# df <- tbl_df(c3_autoarima_out)
# colnames(df) <- paste0('h', 1:4)
# df$shop_id <- as.numeric(str_extract(c8_tr$shop_id %>% unique(),'[0-9].*'))
# df <- df %>% transmute(shop_id, total_nov_sales=h1+h2+h3+h4)
#
# item_pc_sales_per_shop_over_6_months <- df_master %>%
#   filter(month %in% c(5,6,7,8,9,10), year==2015) %>%
#   select(shop_id, item_id, item_cnt_day) %>%
#   group_by(shop_id, item_id) %>%
#   summarize(total_itemsales_per_shop = sum(item_cnt_day)) %>%
#   ungroup() %>%
#   group_by(shop_id) %>%
#   mutate(total_sales_per_shop = sum(total_itemsales_per_shop)) %>%
#   ungroup() %>%
#   mutate(item_sales_as_pc_of_total = total_itemsales_per_shop/total_sales_per_shop,
#          shop_id = as.numeric(str_extract(shop_id,'[0-9].*')),
#          item_id = as.numeric(str_extract(item_id,'[0-9].*')))
#
# estimated_nov_sales <- item_pc_sales_per_shop_over_6_months %>%
#   left_join(df) %>%
#   mutate(estimated_sales_in_nov = item_sales_as_pc_of_total * total_nov_sales)
#
# c3_weekly_testout <- kaggle_test %>%
#   left_join(estimated_nov_sales) %>%
#   select(id=ID, shop_id, item_id, item_cnt_month = estimated_sales_in_nov)
# c3_weekly_testout
#
# # For those shops which are still NA, let's try assigning an average sales value per month per shop
# # calculated not at a shop_id+item_id level, but just at an item_id level
# # Removing the month of Dec since it'll inflate the values
#
# average_item_sales_per_month <- df_master %>% filter(month != 12) %>% group_by(item_id, month) %>%
#   summarize(total_item_sales_by_month = sum(item_cnt_day, na.rm = T)) %>%
#   group_by(item_id) %>%
#   summarise(no_of_months = n(),
#             total_item_sales = sum(total_item_sales_by_month)) %>%
#   transmute(item_id = as.numeric(str_extract(item_id,'[0-9].*')),
#             average_item_sales_per_month = total_item_sales / no_of_months,
#             average_item_sales_per_month_per_shop = average_item_sales_per_month / length(unique(df_master$shop_id))) %>%
#   select(-average_item_sales_per_month)
#
# c3_weekly_testout <- c3_weekly_testout %>%
#   left_join(average_item_sales_per_month) %>%
#   mutate(item_cnt_month=ifelse(is.na(item_cnt_month), average_item_sales_per_month_per_shop, item_cnt_month)) %>%
#   select(-average_item_sales_per_month_per_shop)
# c3_weekly_testout
#
# map_int(c3_weekly_testout, ~sum(is.na(.x)))
#
# # What are the remaining NA items?
# unfound_item_id <- c3_weekly_testout %>% filter(is.na(item_cnt_month)) %>% distinct(item_id) %>% pull(item_id)
# length(unfound_item_id)
# # Do they even exist in df_master?
# table(unfound_item_id %in% as.numeric(str_extract(df_master$item_id,'[0-9].*')))
#
# # Only 7 are found in the master! (Probably in the month of December)
# # How should I tackle these 'seen-for-the-first-time' item_ids?
# # Perhaps I can use the item_category_id, or just set them to zero.
# # I'm setting them to zero :)
# c3_weekly_testout$item_cnt_month[is.na(c3_weekly_testout$item_cnt_month)] <- 0
# c3_weekly_testout$item_cnt_month[c3_weekly_testout$item_cnt_month>20] <- 20
#
# c3_weekly_testout %>%
#   select(id, item_cnt_month) %>%
#   write_csv(path = 'logs/c3_weekly_testout.csv',col_names = T)
# cache('c3_weekly_testout')
#
# # Without S36 put back in, and without clipping to 20, and with round(): Kaggle score 3.77304
# # With S36, with clipping and without round() :  2.93434 (rank 185)
