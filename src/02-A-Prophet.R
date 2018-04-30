library(prophet)

prof.ts <- df_train %>%
  group_by(date) %>%
  tally(wt = item_cnt_day) %>%
  rename(ds = date, y = n)


m <- prophet(prof.ts)
