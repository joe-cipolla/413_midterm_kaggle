
# How can we cluster shops? ---------------------------------------------------------

# shop_id , aggregated by week_block_num
df_master %>%
  group_by(shop_id, week_block_num) %>%
  tally(wt = item_cnt_day) %>%
  dcast(week_block_num~shop_id) %>% select(-week_block_num) %>%
  as.matrix() %>% t() %>% replace_na(replace = 0)-> shop_id_matrix

diss(shop_id_matrix, 'COR') %>% hclust(method = 'compl') -> hclust_out
new_order <- hclust_out$order
png('graphs/distmat_shopid.png', width = 800, height = 600)
diss(shop_id_matrix, 'COR') %>% as.matrix() %>% corrplot::corrplot(is.corr = F, method = 'color', hclust.method = 'compl', order = 'FPC')
dev.off()
png('graphs/hclust_shopid.png', width = 1000, height = 600)
plot(hclust_out, main = 'Shop ID Clusters for Weekly Aggregated Time Series')
rect.hclust(hclust_out, k=5)
dev.off()

# here are the 5 clusters of time series
x <- rect.hclust(hclust_out, k=5)
names(x) <- paste0('shop_cluster_',1:5)
shop_clusters <- map2_df(x,names(x),.f = ~tibble(shop_cluster_id=.y,shop_id=names(.x)))
shop_clusters

png('graphs/zoo_clust_shopid.png', width = 800, height = 1200)
zoo::plot.zoo(t(shop_id_matrix)[,new_order],main = 'Shop ID', col=as.numeric(as.factor(shop_clusters$shop_cluster_id)))
dev.off()

# How can we cluster Location Level 1 -----------------------------------------------

# Location Level 1
df_master %>%
  # filter(loc_lvl1 %in% top_loc_lvl1) %>%
  group_by(loc_lvl1, week_block_num) %>%
  tally(wt = item_cnt_day) %>%
  dcast(week_block_num~loc_lvl1) %>%
  fill(Yakutsk, .direction = 'up') %>% select(-week_block_num) %>%
  as.matrix() %>% t() %>% replace_na(replace = 0)-> loc_matrix

diss(loc_matrix, 'COR') %>% hclust() -> hclust_out
new_order <- hclust_out$order
png('graphs/distmat_loclvl1id.png', width = 800, height = 600)
diss(loc_matrix, 'COR') %>% as.matrix() %>% corrplot::corrplot(is.corr = F, method = 'color', hclust.method = 'compl', order = 'FPC')
dev.off()
k=4
png('graphs/hclust_loclvl1id.png', width = 1000, height = 600)
hclust_out %>% plot(main = 'Location Level 1')
rect.hclust(hclust_out, k=k)
dev.off()

# here are the 4 clusters of time series
x <- rect.hclust(hclust_out, k=k)
names(x) <- paste0('loclvl1_cluster_',1:k)
loclvl1_clusters <- map2_df(x,names(x),.f = ~tibble(loclvl1_cluster_id=.y,loc_lvl1=names(.x)))
loclvl1_clusters

png('graphs/zoo_clust_loclvl1id.png', width = 800, height = 1200)
zoo::plot.zoo(t(loc_matrix)[,new_order], main = 'Location Level 1', col=as.numeric(as.factor(loclvl1_clusters$loclvl1_cluster_id)))
dev.off()

# How can we cluster item categories at level 1 -------------------------------------

# itemcat_lvl1
df_master %>%
  # filter(itemcat_lvl1 %in% top_items_categories) %>%
  group_by(itemcat_lvl1, week_block_num) %>%
  tally(wt = item_cnt_day) %>%
  dcast(week_block_num~itemcat_lvl1) %>% select(-week_block_num) %>%
  as.matrix() %>% t() %>% replace_na(replace = 0) -> itemcat_matrix

diss(itemcat_matrix, 'COR') %>% hclust() -> hclust_out
new_order <- hclust_out$order
png('graphs/distmat_itemcat.png', width = 800, height = 600)
diss(itemcat_matrix, 'COR') %>% as.matrix() %>% corrplot::corrplot(is.corr = F, method = 'color', hclust.method = 'compl', order = 'FPC', addrect = 2)
dev.off()
k=8
png('graphs/hclust_itemcat.png', width = 1000, height = 600)
hclust_out %>% plot(main = 'Item category Level 1')
rect.hclust(hclust_out, k=k)
dev.off()
# here are the 8 clusters of time series
x <- rect.hclust(hclust_out, k=k)
names(x) <- paste0('itemcatlvl1_cluster_',1:k)
itemcatlvl1_cluster <- map2_df(x,names(x),.f = ~tibble(itemcatlvl1_cluster_id=.y,loc_lvl1=names(.x)))
itemcatlvl1_cluster

png('graphs/zoo_clust_itemcat.png', width = 800, height = 1200)
zoo::plot.zoo(t(itemcat_matrix)[,new_order],main = 'Item Cat Level 1', col=as.numeric(as.factor(itemcatlvl1_cluster$itemcatlvl1_cluster_id)), lwd=2)
dev.off()



# Are some weeks different than other weeks? ----------------------------------------

# is there something unique about weeks?
df_master %>%
  group_by(week, year) %>%
  tally(wt = item_cnt_day) %>%
  dcast(year~week) %>%
  map_df(~tidyr::replace_na(.x, replace = 0)) %>%
  select(-year) %>%
  as.matrix() %>% t() -> week_matrix

diss(week_matrix, 'COR') %>% hclust(method = 'compl')-> hclust_out
new_order <- hclust_out$order
diss(week_matrix, 'COR') %>% as.matrix() %>% corrplot::corrplot(is.corr = F, method = 'color', hclust.method = 'compl', order = 'FPC')
hclust_out %>% plot(main = 'How would the weeks cluster together?')
k=6
rect.hclust(hclust_out, k=k)

# here are the 6 clusters of time series
x <- rect.hclust(hclust_out, k=k)
names(x) <- paste0('week_cluster',1:k)
week_cluster <- map2_df(x,names(x),.f = ~tibble(week_cluster_id=.y,week_number=.x))
week_cluster
zoo::plot.zoo(t(week_matrix)[,new_order],main = 'Week #', col=as.numeric(as.factor(week_cluster$week_cluster_id)), lwd=2)



# Cache variables -------------------------------------------------------------------

cache('shop_clusters')
cache('itemcatlvl1_cluster')
cache('loclvl1_clusters')
cache('week_cluster')
