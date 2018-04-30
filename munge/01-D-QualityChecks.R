# Are there any negative sales counts?
df_master %>% filter(item_cnt_day < 0) %>% count(item_cnt_day)

# It's possible that the negative value is incorrectly assigned. Taking absolute values
# and reassigning back to the data
df_master <-
  df_master %>% mutate(
    neg_item_cnt_day = item_cnt_day < 0,
    item_cnt_day = ifelse(neg_item_cnt_day, abs(item_cnt_day), item_cnt_day)
  )

# -ve item price?
df_master %>% filter(as.numeric(item_price) < 0)
# Generic method to find and replace negative values by looking up price using item_id
avg_item_prices <-
  df_master %>% group_by(item_id) %>% summarise(avg_item_price = mean(item_price))
df_master <- df_master %>%
  left_join(avg_item_prices) %>%
  mutate(item_price = ifelse(item_price < 0, avg_item_price, item_price)) %>%
  select(-avg_item_price)

# Are there any negative ID numbers?
df_master %>% filter(as.numeric(shop_id) < 0)
df_master %>% filter(as.numeric(item_id) < 0)

# -ve date block num?
df_master %>% filter(as.numeric(date_block_num) < 0)

# Any wierd other things?
df_master %>% count(year)
df_master %>% count(loc_lvl1)
df_master %>% count(loc_lvl2)
df_master %>% count(itemcat_lvl1)

# Combining Str. Chkalov 39m? and Str. Chkalov 39 m2 into one
df_master <- df_master %>%
  mutate(
    loc_lvl2 = ifelse(
      loc_lvl2 == 'Str. Chkalov 39m?' | loc_lvl2 == 'Str. Chkalov 39 m²',
      'Str Chkalov',
      loc_lvl2
    )
  )

# itemcat level 1 cleanup
df_master <- df_master %>%
  mutate(
    itemcat_lvl1 = ifelse(
      str_detect(
        as.character(itemcat_lvl1),
        '[Pp]ayment [Cc]ards[A-Za-z\\s\\(\\)]*'
      ),
      'Payment Cards',
      as.character(itemcat_lvl1)
    ),
    itemcat_lvl1 = fct_infreq(itemcat_lvl1)
  )
cache('df_master')