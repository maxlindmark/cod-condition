# Test the bias in Fulton's K from using an exponent of 3 instead of 2.98

library(ggplot2)
library(RColorBrewer)
library(patchwork)
library(tidyverse)
library(scales)

d <- data.frame(length = c(0.01, seq(1, 60, 1)))

d$weight <- (exp(-4.65)*d$length^2.98) 

ggplot(d, aes(weight, length)) + geom_point()

d$fulton_k <- 100*(d$weight/(d$length^3))

#d$fulton_k_corr <- 100*(d$weight/(d$length^2.98))

d$le_cren <- d$weight / (exp(-4.65)*d$length^2.98)

ggplot(d) + 
  geom_line(aes(length, fulton_k, color = "Fulton's K with b=3"), size = 2) +
  #geom_line(aes(length, fulton_k_corr, color = "b=2.98"), size = 2) +
  geom_line(aes(length, le_cren, color = "Le Cren"), size = 2) +
  theme_classic(base_size = 12) + 
  labs(x = "Length (cm)", y = "Condition factor") + 
  scale_color_brewer(palette = "Dark2", name = "L-W exponent") +
  theme(legend.position = c(0.8, 0.5))

ggsave("figures/supp/fulton_bias.png", width = 6.5, height = 6.5, dpi = 600)




# Now compare to real data
df <- readr::read_csv("https://raw.githubusercontent.com/maxlindmark/cod_condition/master/data/for_analysis/mdat_cond.csv")

df$fulton_k <- 100*(df$weight_g/(df$length_cm^3))

df$le_cren <- df$weight_g / (exp(-4.65)*df$length_cm^2.98)

ggplot(df) + 
  geom_point(aes(length_cm, fulton_k, color = "Fulton's K with b=3"), size = 2, alpha = 0.2) +
  geom_point(aes(length_cm, le_cren, color = "Le Cren"), size = 2, alpha = 0.2) +
  stat_smooth(aes(length_cm, fulton_k, color = "Fulton's K with b=3"), size = 2) + 
  stat_smooth(aes(length_cm, le_cren, color = "Le Cren"), size = 2) +
  theme_classic(base_size = 12) + 
  labs(x = "Length (cm)", y = "Condition factor") + 
  scale_color_brewer(palette = "Dark2", name = "L-W exponent") +
  theme(legend.position = c(0.8, 0.5))

ggplot(df) + 
  geom_point(aes(length_cm, fulton_k, color = "Fulton's K with b=3"), size = 1, alpha = 0.2) +
  geom_point(aes(length_cm, le_cren, color = "Le Cren"), size = 1, alpha = 0.2) +
  stat_smooth(aes(length_cm, fulton_k, color = "Fulton's K with b=3"), size = 1) + 
  stat_smooth(aes(length_cm, le_cren, color = "Le Cren"), size = 1) +
  theme_classic(base_size = 12) + 
  facet_wrap(~year) +
  labs(x = "Length (cm)", y = "Condition factor") + 
  scale_color_brewer(palette = "Dark2", name = "L-W exponent") 


p1 <- ggplot(df) + 
  stat_smooth(method = "gam", aes(length_cm, fulton_k, group = year, color = year), size = 1, se = FALSE) + 
  theme_classic(base_size = 12) + 
  labs(x = "Length (cm)", y = "Condition factor") + 
  #scale_color_gradient2(midpoint = ((2019-1993)/2) + 1993) +
  scale_color_gradient2(midpoint = 2004) +
  ggtitle("Fulton's condition factor")

p2 <- ggplot(df) + 
  stat_smooth(method = "gam", aes(length_cm, le_cren, group = year, color = year), size = 1, se = FALSE) + 
  theme_classic(base_size = 12) + 
  labs(x = "Length (cm)", y = "Condition factor") + 
  scale_color_gradient2(midpoint = 2004) +
  #scale_color_gradient2(midpoint = ((2019-1993)/2) + 1993) +
  ggtitle("le_cren")

p1 + p2

ggsave("figures/supp/compare_index_time_period.png", width = 6.5, height = 6.5, dpi = 600)


p3 <- 
  df %>%
  mutate(time_period = ifelse(year < 2005, "1993-2004", "2005-2019")) %>% 
  ggplot(., aes(length_cm, fulton_k, group = time_period, color = time_period)) + 
  stat_smooth(size = 2) + 
  theme_classic(base_size = 12) + 
  labs(x = "Length (cm)", y = "Condition factor") + 
  ggtitle("Fulton's K") +
  geom_vline(xintercept = c(20, 70), linetype = 2, alpha =  0.5, size = 1.2) +
  #geom_rug(sides = "b") + 
  NULL

p4 <- 
  df %>%
  mutate(time_period = ifelse(year < 2005, "1993-2004", "2005-2019")) %>% 
  ggplot(., aes(length_cm, le_cren, group = time_period, color = time_period)) + 
  stat_smooth(size = 2) + 
  theme_classic(base_size = 12) + 
  labs(x = "Length (cm)", y = "Le Cren's condition index") + 
  #geom_vline(xintercept = c(18, 72), linetype = 2, alpha =  0.5, size = 1.2) +
  coord_cartesian(expand = 0, xlim = c(0, 125)) +
  guides(color = FALSE) + 
  #geom_rug(sides = "b", alpha = 0.2) +
  #geom_histogram(aes(length_cm, stat(count / sum(count)), inherit.aes = FALSE, alpha = 0.5) +
  NULL

p5 <- 
  df %>%
  mutate(time_period = ifelse(year < 2005, "1993-2004", "2005-2019")) %>% 
  ggplot(.,) + 
  geom_histogram(aes(x = length_cm, y = ..ncount..), alpha = 1) +
  scale_y_continuous(labels = percent_format()) +
  theme_classic(base_size = 12) + 
  coord_cartesian(expand = 0, xlim = c(0, 125)) +
  #geom_vline(xintercept = c(18, 72), linetype = 2, alpha =  0.5, size = 1.2) +
  theme(legend.position = c(0.8, 0.5)) + 
  NULL

# How many cod are below and above?
df %>% filter(length_cm < 18) %>% nrow() / df %>% nrow()
df %>% filter(length_cm > 115) %>% nrow() / df %>% nrow()

p4 / p5

#ggsave("figures/supp/le_cren_index_time_period.png", width = 6.5, height = 6.5, dpi = 600)

# Now plot the slope (change in condition factor) over time for 5 cm size-classes
df <- df %>%
  mutate(lenCls = cut(length_cm, breaks = seq(0, 150, 10)),
         lenCls_fct = factor(lenCls))

# df %>%
#   group_by(lenCls, year) %>%
#   summarise(n = n()) %>% 
#   arrange(n) %>% 
#   data.frame()

pal <- brewer.pal(n = 5, name = "Dark2")

# Plot annul condition factor by year
ggplot(df, aes(year, le_cren)) +
  geom_point(alpha = 0.2, size = 0.5) +
  geom_hline(yintercept = 1, linetype = 2, color = pal[2], size = 1.3) + 
  stat_smooth(method = "gam", formula = y ~ s(x, k = 3), color = pal[1]) + 
  facet_wrap(~lenCls_fct) +
  theme_classic(base_size = 12) + 
  labs(y = "Le Cren's Condition index", x = "Year") +
  coord_cartesian(ylim = c(0.15, 3)) +
  NULL


cond_year_est_le_cren <- df %>%
  mutate(year_ct = year - 1993) %>% 
  split(.$lenCls_fct) %>%
  purrr::map(~lm(le_cren ~ year_ct, data = .x)) %>%
  purrr::map_df(broom::tidy, .id = 'lenCls_fct') %>%
  filter(term %in% c('year_ct', "(Intercept)")) %>% 
  dplyr::select(-c(statistic, p.value)) %>% 
  mutate(model = "le_cren")

cond_year_est_fulton <- df %>%
  mutate(year_ct = year - 1993) %>% 
  split(.$lenCls_fct) %>%
  purrr::map(~lm(fulton_k ~ year_ct, data = .x)) %>%
  purrr::map_df(broom::tidy, .id = 'lenCls_fct') %>%
  filter(term %in% c('year_ct', "(Intercept)")) %>% 
  dplyr::select(-c(statistic, p.value)) %>% 
  mutate(model = "fulton_k")

# Bind together
cond_year_slopes <- bind_rows(cond_year_est_fulton, cond_year_est_le_cren) %>% filter(term == "year_ct")

cond_year_intercept <- bind_rows(cond_year_est_fulton, cond_year_est_le_cren) %>% filter(term == "(Intercept)")

# Plot the slopes
cond_year_slopes %>% 
  separate(lenCls_fct, c("lwr", "upr"), sep = ",") %>%
  separate(upr, c("upr", "scrap"), sep = "]") %>%
  dplyr::select(-scrap) %>% 
  mutate(upr_num = as.numeric(upr)) %>% 
  filter(upr_num < 115) %>% 
  ggplot(., aes(as.factor(upr_num), estimate, color = model)) +
  geom_hline(yintercept = 0, alpha = 0.5, linetype = 2) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(x = as.factor(upr_num),
                    ymax = estimate + 1.96*std.error,
                    ymin = estimate - 1.96*std.error,
                    color = model),
                width = 0.4, position = position_dodge(width = 0.5)) +
  theme_classic(base_size = 12) + 
  labs(y = "Slope (condition factor ~ year)", x = "Length-class (upper value)") +
  NULL
  
# Plot the change
cond_year_slopes %>% 
  separate(lenCls_fct, c("lwr", "upr"), sep = ",") %>%
  separate(upr, c("upr", "scrap"), sep = "]") %>%
  dplyr::select(-scrap) %>% 
  mutate(upr_num = as.numeric(upr)) %>% 
  filter(upr_num < 115) %>% 
  mutate(tot_change = estimate*(2019-1993)) %>% 
  ggplot(., aes(as.factor(upr_num), estimate, color = model)) +
  geom_hline(yintercept = 0, alpha = 0.5, linetype = 2) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) + 
  theme_classic(base_size = 12) + 
  labs(y = "Total change in condition factor", x = "Length-class (upper value)") +
  NULL

# Plot the % change
cond_year_intercept2 <- cond_year_intercept %>%
  mutate(intercept = estimate) %>% 
  dplyr::select(-std.error, -term, -estimate)

cond_year_percent <- cond_year_slopes %>% 
  mutate(slope = estimate) %>% 
  dplyr::select(-std.error, -term, -estimate) %>% 
  left_join(cond_year_intercept2) %>% 
  mutate(percent_change = (intercept + slope*(2019-1993))/intercept)

# Plot the change
cond_year_percent %>% 
  separate(lenCls_fct, c("lwr", "upr"), sep = ",") %>%
  separate(upr, c("upr", "scrap"), sep = "]") %>%
  dplyr::select(-scrap) %>% 
  mutate(upr_num = as.numeric(upr)) %>% 
  filter(upr_num < 115) %>% 
  ggplot(., aes(as.factor(upr_num), percent_change, color = model)) +
  geom_hline(yintercept = 1, alpha = 0.5, linetype = 2) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) + 
  theme_classic(base_size = 12) + 
  labs(y = "Change in condition factor", x = "Length-class (upper value)") +
  NULL

# Plot the intercepts
cond_year_intercept %>% 
  separate(lenCls_fct, c("lwr", "upr"), sep = ",") %>%
  separate(upr, c("upr", "scrap"), sep = "]") %>%
  dplyr::select(-scrap) %>% 
  mutate(upr_num = as.numeric(upr)) %>% 
  filter(upr_num < 115) %>% 
  ggplot(., aes(as.factor(upr_num), estimate, color = model)) +
  geom_hline(yintercept = 1, alpha = 0.5, linetype = 2) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(x = as.factor(upr_num),
                    ymax = estimate + 1.96*std.error,
                    ymin = estimate - 1.96*std.error,
                    color = model),
                width = 0.4, position = position_dodge(width = 0.5)) +
  theme_classic(base_size = 12) + 
  labs(y = "Condition in 1993 (intercept)", x = "Length-class (upper value)") +
  NULL

# Final plot
p6 <- cond_year_percent %>% 
  filter(model == "le_cren") %>% 
  separate(lenCls_fct, c("lwr", "upr"), sep = ",") %>%
  separate(upr, c("upr", "scrap"), sep = "]") %>%
  dplyr::select(-scrap) %>% 
  mutate(upr_num = as.numeric(upr)) %>% 
  filter(upr_num < 115) %>% 
  ggplot(., aes(as.factor(upr_num), percent_change)) +
  geom_hline(yintercept = 1, alpha = 0.5, linetype = 2) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) + 
  theme_classic(base_size = 12) + 
  labs(y = "Change in condition factor (1993-2019)", x = "Length-class (upper value)") +
  NULL

p6

ggsave("figures/supp/percent_change_lecren.png", width = 6.5, height = 6.5, dpi = 600)

