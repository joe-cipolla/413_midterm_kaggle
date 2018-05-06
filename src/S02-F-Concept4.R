# 1. Split data by shop_clusters. Perhaps some types of models will fit better
# for each cluster
# 2. investigate which type of model per cluster - ets or auto arima or whatever
# 3. develop forecast for nov, for shop_id level, weekly aggregation, include xreg where possible
# use indicator variables for weeks using the week-hclust model output
#
#
# open question:
# 1. should i remove spikes from digital and online before doing all this?
#
#
#


to_join <- df_master %>% mutate(item_id = as.numeric(str_extract(item_id,pattern = '[0-9].*'))) %>% select(item_id, item_category_id, itemcat_lvl1, itemcat_lvl2) %>% distinct()
joined <- kaggle_test %>%
  left_join(to_join)
joined %>% count(itemcat_lvl1)

df_master %>%
  # filter(itemcat_lvl1 %in% top_items_categories) %>%
  group_by(itemcat_lvl1, week_block_num) %>%
  tally(wt = item_cnt_day) %>%
  dcast(week_block_num~itemcat_lvl1) %>% select(-week_block_num) %>% tbl_df() %>%
  rownames_to_column('wk_num') %>% mutate(wk_num=as.integer(wk_num)) %>%
  replace_na(replace = list(`Tickets (figure)` = 0)) %>%
  xyplot(`Tickets (figure)`~wk_num,.,type='b')
df_master %>%
  group_by(itemcat_lvl1, week_block_num) %>%
  tally(wt = item_cnt_day) %>%
  dcast(week_block_num~itemcat_lvl1) %>% select(-week_block_num) %>% tbl_df() %>%
  rownames_to_column('wk_num') %>% mutate(wk_num=as.integer(wk_num)) %>%
  replace_na(replace = list(Official = 0)) %>%
  xyplot(`Official`~wk_num,.,type='b')
df_master %>%
  group_by(itemcat_lvl1, week_block_num) %>%
  tally(wt = item_cnt_day) %>%
  dcast(week_block_num~itemcat_lvl1) %>% select(-week_block_num) %>% tbl_df() %>%
  rownames_to_column('wk_num') %>% mutate(wk_num=as.integer(wk_num)) %>%
  replace_na(replace = list(`Delivery of goods` = 0)) %>%
  xyplot(`Delivery of goods`~wk_num,.,type='b')
df_master %>%
  group_by(itemcat_lvl1, date) %>%
  tally(wt = item_cnt_day) %>%
  filter(itemcat_lvl1=='Delivery of goods') %>%
  ggplot(aes(x=date,y=n))+geom_line()+
  scale_x_date(date_breaks = '1 month')+
  theme(axis.text.x = element_text(angle = 90))
df_master %>%
  filter(itemcat_lvl1=='Delivery of goods') %>%
  group_by(itemcat_lvl1, date_block_num) %>%
  tally(wt = item_cnt_day) %>%
  xyplot(n~date_block_num,.,type=c('b','g'),scales = list(x=list(at=c(12,24,36))))

# Data cleanup ----------------------------------------------------------------------

# Based on the EDA done so far, removing these item level 1 categories from the dataset. These item cats
# are one off sales and do not trend / affect the Nov forecast. They will only make it more difficult to
# forecast

itemcat_lvl1_to_remove <- c('Tickets (figure)','Official')
spiky_but_might_be_able_to_model <- c('Delivery of goods')

df_master %>%
  mutate(flag = itemcat_lvl1 %in% itemcat_lvl1_to_remove) %>%
  filter(!flag) %>%
  select(-flag) -> c4_df_master
c4_df_master


# Does the clustering change now? ---------------------------------------------------

# shop_id , aggregated by week_block_num
c4_df_master %>%
  group_by(shop_id, week_block_num) %>%
  tally(wt = item_cnt_day) %>%
  dcast(week_block_num~shop_id) %>% select(-week_block_num) %>%
  as.matrix() %>% t() %>% replace_na(replace = 0)-> shop_id_matrix

diss(shop_id_matrix, 'COR') %>% hclust(method = 'single') -> hclust_out
new_order <- hclust_out$order
# png('graphs/distmat_shopid.png', width = 800, height = 600)
diss(shop_id_matrix, 'COR') %>% as.matrix() %>% corrplot::corrplot(is.corr = F, method = 'color', hclust.method = 'sing', order = 'hclust')
# dev.off()
# png('graphs/hclust_shopid.png', width = 1000, height = 600)
plot(hclust_out, main = 'Shop ID Clusters for Weekly Aggregated Time Series')
k=5
rect.hclust(hclust_out, k=k)
# dev.off()
x <- rect.hclust(hclust_out, k=k)
names(x) <- paste0('shop_cluster_',1:k)
shop_clusters <- map2_df(x,names(x),.f = ~tibble(shop_cluster_id=.y,shop_id=names(.x)))
shop_clusters

# png('graphs/zoo_clust_shopid.png', width = 800, height = 1200)
zoo::plot.zoo(t(shop_id_matrix)[,new_order],main = 'Shop ID', col=as.numeric(as.factor(shop_clusters$shop_cluster_id)),cex=0.8)
# dev.off()

c4_df_master <- c4_df_master %>%
  left_join(shop_clusters)


# Prophet Functions -----------------------------------------------------------------

holidays <- russian_holidays %>% select(ds=date,holiday=holiday_name)

get_prophet_yhats <- function(daily_ts, ts_name, period=31, save_plots=T) {
  m <- prophet(daily_ts, holidays = holidays)
  future <- make_future_dataframe(m, periods = period)
  forecast <- predict(m, future)
  plot(m, forecast) -> p
  if(save_plots)
    ggsave(paste0('graphs/prophet/', ts_name, '.png'), p, width = 10, height = 6, units = 'in')
  else
    print(p)
  forecast %>% filter(ds > ymd('2015-10-31') & ds < ymd('2015-12-01')) %>% pull(yhat)
}

prophet_execute_shop <- function(shop_id_filter, ...){
  cat('\nProcessing shop...',shop_id_filter,'\n')
  c4_df_master %>%
    filter(shop_id %in% shop_id_filter) %>% select(date, item_cnt_day) %>% group_by(date) %>%
    summarize(y = sum(item_cnt_day)) %>% ungroup() %>%
    transmute(ds = date, y) -> daily_ts
  daily_yhats <- get_prophet_yhats_shoplevel(daily_ts, shop_id_filter, ...)
  sum(daily_yhats)
}

# # Step I - Shop ID November Forecast ------------------------------------------------
#
# ## Cluster 1
# cluster_1_shops <- shop_clusters %>% filter(shop_cluster_id == 'shop_cluster_1') %>% pull(shop_id)
# # cluster_1_results <- c4_df_master %>% filter(shop_cluster_id == 'shop_cluster_1') %>% summarize(fc_nov = sum(item_cnt_day))
# cluster_1_results <- list()
# for (i in seq_along(cluster_1_shops)) {
#   c4_df_master %>%
#     filter(shop_id %in% cluster_1_shops[i]) %>%
#     select(date, shop_id, week_block_num, item_cnt_day) %>% group_by(date) %>%
#     summarize(y = sum(item_cnt_day)) %>%
#     ungroup() %>%
#     transmute(ds = date, y) -> daily_ts
#   cluster_1_results[[i]] <- get_prophet_yhats(daily_ts, cluster_1_shops[i])
# }
#
# ## Cluster 2
# cluster_2_shops <- shop_clusters %>% filter(shop_cluster_id == 'shop_cluster_2') %>% pull(shop_id)
#
# cluster_2_results <- list()
# for (i in seq_along(cluster_2_shops)) {
#   c4_df_master %>%
#     filter(shop_id %in% cluster_2_shops[i]) %>%
#     select(date, shop_id, week_block_num, item_cnt_day) %>% group_by(date) %>%
#     summarize(y = sum(item_cnt_day)) %>%
#     ungroup() %>%
#     transmute(ds = date, y) -> daily_ts
#   cluster_2_results[[i]] <- get_prophet_yhats(daily_ts, cluster_2_shops[i])
# }
#
# # Cluster 3
# cluster_3_shops <- shop_clusters %>% filter(shop_cluster_id == 'shop_cluster_3') %>% pull(shop_id)
#
# cluster_3_results <- list()
# for (i in seq_along(cluster_3_shops)) {
#   c4_df_master %>%
#     filter(shop_id %in% cluster_3_shops[i]) %>%
#     select(date, shop_id, week_block_num, item_cnt_day) %>% group_by(date) %>%
#     summarize(y = sum(item_cnt_day)) %>%
#     ungroup() %>%
#     transmute(ds = date, y) -> daily_ts
#   cluster_3_results[[i]] <- get_prophet_yhats(daily_ts, cluster_3_shops[i])
# }
#
# # Cluster 4
# cluster_4_shops <- shop_clusters %>% filter(shop_cluster_id == 'shop_cluster_4') %>% pull(shop_id)
# cluster_4_results <- list()
# for (i in seq_along(cluster_4_shops)) {
#   c4_df_master %>%
#     filter(shop_id %in% cluster_4_shops[i]) %>%
#     select(date, shop_id, week_block_num, item_cnt_day) %>% group_by(date) %>%
#     summarize(y = sum(item_cnt_day)) %>%
#     ungroup() %>%
#     transmute(ds = date, y) -> daily_ts
#   cluster_4_results[[i]] <- get_prophet_yhats(daily_ts, cluster_4_shops[i])
# }
#
# # Cluster 5
# cluster_5_shops <- shop_clusters %>% filter(shop_cluster_id == 'shop_cluster_5') %>% pull(shop_id)
# cluster_5_results <- list()
# for (i in seq_along(cluster_5_shops)) {
#   c4_df_master %>%
#     filter(shop_id %in% cluster_5_shops[i]) %>%
#     select(date, shop_id, week_block_num, item_cnt_day) %>% group_by(date) %>%
#     summarize(y = sum(item_cnt_day)) %>%
#     ungroup() %>%
#     transmute(ds = date, y) -> daily_ts
#   cluster_5_results[[i]] <- get_prophet_yhats(daily_ts, cluster_5_shops[i])
# }
#
# cluster_2_shops



# Step I - Shop ID Nov Forecast using Prophet ---------------------------------------

shop_list <- unique(df_master$shop_id)
fc_shop_prophet <- map_dbl(t(shop_list),~prophet_execute_shop(.x, save_plots=T))
fc_shop <- tibble(shop_id = shop_list, fc_shop_prophet)

# Step II - Shop ID + Item Category lvel Forecast using Prophet

cl<-makeCluster(4)
registerDoParallel(cl)

shop_list <- unique(c4_df_master$shop_id)
prophet_execute_shop_itemcat <- function(shop_id_filter, ...) {
  cat('\nProcessing shop...', shop_id_filter, '\n')
  c4_df_master %>%
    filter(shop_id %in% shop_id_filter) %>%
    select(date, itemcat_lvl1, item_cnt_day) %>%
    group_by(date, itemcat_lvl1) %>%
    summarize(y = sum(item_cnt_day)) %>% ungroup() -> shoplevel_ts

  itemcat_list <- unique(shoplevel_ts$itemcat_lvl1)
  itemlevel_results <- tibble(itemcat_list)
  itemlevel_results$fc <- 0
  for (i in seq_along(itemcat_list)) {
    shoplevel_ts %>% filter(itemcat_lvl1 == itemcat_list[i]) %>%
      transmute(ds = date, y) -> daily_ts
    daily_yhats <- get_prophet_yhats(daily_ts,
                                     paste0(shop_id_filter, itemcat_list[i]))
    itemlevel_results$fc[i] <- sum(daily_yhats)
  }
  itemlevel_results
}
fc_shop_prophet <- map(t(shop_list),~prophet_execute_shop_itemcat(.x, save_plots=T))
