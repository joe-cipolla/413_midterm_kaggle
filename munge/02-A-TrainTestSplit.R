total_interval <- interval(min(df_master$date), max(df_master$date))
train_interval <-
  interval(min(df_master$date), max(df_master$date)) * .85

df_train <-
  df_master %>% filter(between(date, date(int_start(train_interval)), date(int_end(train_interval))))
df_test <-
  df_master %>% filter(!between(date, date(int_start(train_interval)), date(int_end(train_interval))))

cache('df_train')
cache('df_test')
