tmtk_master <- df_master %>%
  group_by(date,shop_id) %>%
  summarize(sales_per_day = sum(item_cnt_day)) %>%
  ungroup()

tmtk_sub <- tmtk_master %>% filter(shop_id=='S28') %>% select(-shop_id)

# idx <- tmtk_sub %>% tk_index()
# tk_sign <- tk_get_timeseries_signature(idx)
# tk_make_future_timeseries(idx, n_future = 5)

tmtk_sub <- tmtk_sub %>%
  tk_augment_timeseries_signature() %>%
  select(-date, -index.num, - diff, -year, -month.xts, -month.lbl, -hour,-minute,-second,-hour12,-am.pm,-wday.xts,-week.iso)
tmtk_sub

fit_lm <- lm(sales_per_day ~ ., data = tmtk_sub)

# fit_lm %>%
#   augment() %>%
#   ggplot(aes(x = rownames(.), y = .resid)) +
#   geom_hline(yintercept = 0, color = "red") +
#   geom_point(color = 'gray', size = 2, shape=21) +
#   labs(title = "Training Set: lm() Model Residuals", x = "") +
#   scale_y_continuous(limits = c(-5000, 5000))

summary(fit_lm)
sqrt(mean(fit_lm$residuals^2))

rpart::rpart(item_cnt_day~., df_master ) -> partfit
plot(partfit);text(partfit)
