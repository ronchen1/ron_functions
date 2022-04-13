# check which columns are a factor:
t0_t1 %>% Filter(f = is.factor) %>% names
