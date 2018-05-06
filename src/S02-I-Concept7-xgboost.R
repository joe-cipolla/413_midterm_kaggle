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

shop_clusters
itemcatlvl1_cluster
loclvl1_clusters


table1 <- c4_df_master %>%
  group_by(date, shop_id, item_id) %>%
  summarise(daily_item_sales = sum(item_cnt_day)) %>%
  ungroup()
table2 <- c4_df_master %>%
  distinct(date,date_block_num) %>% timetk::tk_augment_timeseries_signature()
table3 <- c4_df_master %>%
  distinct(shop_id,loc_lvl1,loc_lvl2,shop_center_lgl,shop_cluster_id,sec_lgl,tc_lgl,trc_lgl,trk_lgl)
table4 <- c4_df_master %>%
  distinct(item_id, item_category_id, itemcat_lvl1, itemcat_lvl2)

#
# intermediate_result <- table1 %>% filter(shop_id=='S10') %>% dcast(date~item_id,fill = 0)

c7_df_master <- table1 %>%
  left_join(table2) %>%
  left_join(table3) %>%
  left_join(table4)

c7_df_master %>%
  select(-date,-date_block_num,-index.num,-diff,-year.iso,-month.xts,-hour,-minute,-second,-hour12,-am.pm,-wday.xts,-wday)


total_interval <- interval(min(c7_df_master$date), max(c7_df_master$date))
train_interval <- interval(min(c7_df_master$date), max(c7_df_master$date)) * .85

c7_df_train <- c7_df_master %>% filter(between(date, date(int_start(train_interval)), date(int_end(train_interval))))
c7_df_test  <- c7_df_master %>% filter(!between(date, date(int_start(train_interval)), date(int_end(train_interval))))

c7_df_train

xgboost(c7_df_train,
        )