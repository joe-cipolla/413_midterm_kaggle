kaggle_test <- read_csv('data/test.csv', col_types = 'iii')

result_df <- tslm_result_monthlyforecast %>%
  mutate(
    item_id = as.numeric(str_extract(item_id, pattern = '[0-9].*')),
    shop_id = as.numeric(str_extract(shop_id, pattern = '[0-9].*'))
  )

kaggle_test %<>%
  left_join(result_df)

kaggle_test

odd_ball_prep <- df_master %>%
  filter(shop_id %in% odd_ball,
         ym == '2015-10')