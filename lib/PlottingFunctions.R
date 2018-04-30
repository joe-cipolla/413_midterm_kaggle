plot_topshop_ts <- function(df_master, top_shops){
  df_master %>%
    filter(shop_id %in% top_shops) %>%
    group_by(shop_id, date_block_num) %>%
    tally(wt = item_cnt_day) %>%
    ggplot(aes(x=date_block_num, y=n))+
    geom_line(aes(color=shop_id))+
    geom_point(aes(color=shop_id, pch=shop_id))+
    theme_bw()+
    scale_x_continuous(breaks = seq(0,36,12))+
    labs(title = 'Total items sold in top shops')
}
plot_topitemcategories_ts <- function(df_master, top_items_categories){
  df_master %>%
    filter(itemcat_lvl1 %in% top_items_categories) %>%
    group_by(itemcat_lvl1, date_block_num) %>%
    tally(wt = item_cnt_day) %>%
    ggplot(aes(x=date_block_num, y=n))+
    geom_line()+ geom_point()+
    theme_bw()+
    scale_x_continuous(breaks = seq(0,36,12))+
    labs(title = 'Total item categories sold')+
    facet_wrap(~itemcat_lvl1,ncol = 1, scales = 'free_y', strip.position = 'right')
}
plot_top_loc_lvl1_ts <- function(df_master, top_loc_lvl1){
  df_master %>%
    filter(loc_lvl1 %in% top_loc_lvl1) %>%
    group_by(loc_lvl1, date_block_num) %>%
    tally(wt = item_cnt_day) %>%
    ggplot(aes(x=date_block_num, y=n))+
    geom_line()+ geom_point()+
    theme_bw()+
    scale_x_continuous(breaks = seq(0,36,12))+
    labs(title = 'Total location - level 1')+
    facet_wrap(~loc_lvl1,ncol = 1, scales = 'free_y', strip.position = 'right')
}
