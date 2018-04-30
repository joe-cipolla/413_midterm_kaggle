plot_topshop_ts <- function(df_master, top_shops){
  df_master %>%
    filter(shop_id %in% top_shops) %>%
    group_by(shop_id, date_block_num) %>%
    tally(wt = item_cnt_day) %>%
    ggplot(aes(x=date_block_num, y=n))+
    geom_line(aes(color=shop_id))+
    geom_point(aes(color=shop_id, pch=shop_id))+
    theme_bw()+
    labs(title = 'Total items sold in top shops')
}
plot_topitem_ts <- function(df_master, top_items){
  df_master %>%
    filter(item_id %in% top_items) %>%
    group_by(item_name, date_block_num) %>%
    tally(wt = item_cnt_day) %>%
    ggplot(aes(x=date_block_num, y=n))+
    geom_line(aes(color=item_name))+
    geom_point(aes(color=item_name, pch=item_name))+
    theme_bw()+
    labs(title = 'Total items sold in top items')
}
plot_top_itemcats_ts <- function(df_master, top_itemcats){
  df_master %>%
    filter(itemcat_lvl1 %in% top_itemcats) %>%
    group_by(itemcat_lvl1, date_block_num) %>%
    tally(wt = item_cnt_day) %>%
    ggplot(aes(x=date_block_num, y=n))+
    geom_line(aes(color=itemcat_lvl1))+
    geom_point(aes(color=itemcat_lvl1, pch=itemcat_lvl1))+
    theme_bw()+
    labs(title = 'Total items sold in top item categories')
}
