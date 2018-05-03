master_ts <-
  df_master %>% select(date_block_num, shop_id, item_id, item_price, item_cnt_day)

# Monthly aggregates
master_ts <-
  aggregate(
    x = master_ts$item_cnt_day,
    by = list(
      month_num = master_ts$date_block_num,
      shop_id = master_ts$shop_id
    ),
    FUN = sum,
    simplify = T
  )
master_ts <- dcast(formula = month_num ~ shop_id, master_ts, fill = 0)
master_ts <- ts(master_ts[, -1], start = c(2013, 1), frequency = 12)
zoo::plot.zoo(master_ts)
shop_names <- colnames(master_ts)
for (i in seq_along(shop_names)) {
  png(filename = paste0('graphs/monthlyaverages_shop_id_',shop_names[i],'.png'),width = 800, height = 600)
  seasonplot(master_ts[, i], year.labels.left = T, main = paste0('Shop ID: ',shop_names[i]), col = c('red','forestgreen','navyblue'))
  dev.off()
}

# Shops Closed ----------------------------------------------------------------------
# Have any shops closed down?
# YES!
closed_shops <- c(0,1,8,11,13,17,23,27,29,30,32,33,40,43,54)
only_oct_sales <- c(9,20)
odd_ball <- c(36) #Shop 36 opens Oct-2015 - nothing to forecast. This will have to be
# specially handled

# Once these shops are rmeoved, does it affect the final number of items we need to consider?
all_shops <- shop_names
df_master %>%
  mutate(remove_shops = shop_id %in% c(closed_shops, only_oct_sales)) %>%
  filter(!remove_shops) %>%
  distinct(item_id) %>%
  pull(item_id) -> item_id_after_shops_removed
length(unique(item_id_after_shops_removed))
length(unique(df_master$item_id))
# YES! we get rid of 781 items

# There are 82 items in the test dataset which do not show up in the training dataset!
# > base::setdiff(unique(test_delete$item_id), unique(as.numeric(df_master$item_id)))
#  [1] 21937 21967 21830 21844 22001 22002 22118 22167 22087 22088 22091 22100 22101 22102 22060 22006 22005 22004 21942 21956 21961 21968 22003
# [24] 22145 22163 21843 21881 21811 22092 22164 21848 22022 22162 21827 21828 21906 22071 22104 22050 21955 21929 21928 21842 21835 21976 22069
# [47] 22059 22105 22139 22137 22111 22106 21914 21957 21947 21940 21946 22008 22007 22015 22154 21826 21864 21962 22054 21973 22068 21902 21980
# [70] 22013 22166 21995 21978 22035 22017 21829 21812 21825 21975 21974 21953 21948
# > base::setdiff(unique(test_delete$item_id), unique(as.numeric(df_master$item_id))) %>% length
# [1] 82
# There are no shops in the test dataset which do not show up in the training dataset.
# > base::setdiff(unique(test_delete$shop_id), unique(as.numeric(df_master$shop_id)))
# integer(0)


# Removing the closed shops from the master data set
df_master <- df_master %>%
  mutate(remove_shops = shop_id %in% c(closed_shops, only_oct_sales, odd_ball)) %>%
  filter(!remove_shops) %>%
  select(-remove_shops)
