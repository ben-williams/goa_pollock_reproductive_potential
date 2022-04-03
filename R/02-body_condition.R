# Gulf of Alaska walleye pollock reproductive potential
# ben.williams@noaa.gov
# 2022-1

# load ----
source("R/01-clean_data.R")

# Kr ----
# model relative body condition
m1 <- gam(weight ~ s(length, k=3) + s(value, k=3) + s(winter, k=3) + s(summer,k=3) +
            Age + Mature, data = filter(dat2, name=="ay_bio"), gamma = 1.4)
m2 <- gam(weight ~ s(length, k=3) + s(value, k=3) + s(winter, k=3) + s(summer,k=3) +
            Age + Mature, data = filter(dat2, name=="lay_bio"), gamma = 1.4)
m3 <- gam(weight ~ s(length, k=3) + s(value, k=3) + s(winter, k=3) + s(summer,k=3) +
            Age + Mature, data = filter(dat2, name=="y_bio"), gamma = 1.4)
m4 <- gam(weight ~ s(length, k=3) + s(value, k=3) + s(winter, k=3) + s(summer,k=3) +
            Age + Mature, data = filter(dat2, name=="ly_bio"), gamma = 1.4)
m5 <- gam(weight ~ s(length, k=3) + s(value, k=3) + s(winter, k=3) + s(summer,k=3) +
            Age + Mature, data = filter(dat2, name=="ay_abund"), gamma = 1.4)
m6 <- gam(weight ~ s(length, k=3) + s(value, k=3) + s(winter, k=3) + s(summer,k=3) +
            Age + Mature, data = filter(dat2, name=="lay_abund"), gamma = 1.4)
m7 <- gam(weight ~ s(length, k=3) + s(value, k=3) + s(winter, k=3) + s(summer,k=3) +
            Age + Mature, data = filter(dat2, name=="y_abund"), gamm = 1.4)
m8 <- gam(weight ~ s(length, k=3) + s(value, k=3) + s(winter, k=3) + s(summer,k=3) +
            Age + Mature, data = filter(dat2, name=="ly_abund"), gamma = 1.4)

AIC(m1, m2, m3, m4, m5, m6, m7, m8) %>%
  rownames_to_column("model") %>%
  mutate(delta = min(AIC) - AIC) %>%
  arrange(-delta)

summary(m3)
plot(m3, shade = T, page = 1)


dat2 %>%
  filter(age>1) %>%
  group_by(age) %>%
  summarise(ln = quantile(length, 0.9, na.rm=T),
            lln = quantile(length, 0.1, na.rm = T)) %>%
  ungroup() -> lngs

pop %>%
  filter(name=="y_bio") %>%
  summarise(al = min(scale(value))) -> abs

expand.grid(length = scale(10:70),
            age = 2:10,
            year = unique(dat2$year),
            Mature = factor(1),
            latitude = 57.5755,
            longitude = -155.0948,
            value = seq(100,1300, 100),
            winter = seq(4.5, 6.5, 0.1),
            summer = seq(8.9, 11.9, 0.1)) %>%
  # left_join(sst_dat) %>%
  left_join(lngs) %>%
  left_join(abs) %>%
  dplyr::select(-year) %>%
  filter(length>=lln & length <=ln, value <= al) %>%
  ungroup %>%
  mutate(Age = factor(age)) -> krnew

kr_pred <- predict(m3, krnew, type='response', se=TRUE)
kr_preds <- predict(m3, dat2, type='response', se=TRUE)

top <- c("7500", "15000", "1,100", "1,300", "1,500", "1,900")
names(top) <- c("625.006", "957.503", "1102.617", "1335.328", "1520.328", "1875.511")
side <- c("age-1","age-2","age-3","age-4","age-5","age-6","age-7","age-8","age-9","age-10")
names(side) <- c("-1","-2","-3","-4","-5","-6","-7","-8","-9","-10")

krnew %>%
  mutate(Kr = as.numeric(kr_pred$fit)) %>%
  filter(!is.na(age)) -> krnew

dat2 %>%
  mutate(fit = as.numeric(kr_preds$fit)) %>%
  filter(!is.na(age)) -> dat2


# krnew %>%
# filter(value <=1000) |>
dat2 %>%
  filter(name=="y_bio") %>%
  mutate(age = -age) %>%
  group_by(age, winter, summer, value) %>%
  summarise(Kr = mean(fit)) %>%
  ggplot(aes(winter, summer)) +
  geom_point(aes(color = Kr, fill = Kr), size = 3, pch = 15) +
  # geom_point(alpha = 1, size = 16) +
  scale_fill_scico(name = expression(K[r]),
                   palette = "roma", direction = -1) +
  scale_color_scico(name = expression(K[r]),
                    palette = "roma", direction = -1) +
  facet_grid(age~value/1000,
             labeller = labeller(age = side)) +
  ylab("Summer Temperature (°C)\n") +
  xlab("\nWinter Temperature (°C)") +
  theme(panel.spacing.x=unit(0.5, "mm"),
        panel.spacing.y=unit(0.5, "mm"))

# ggsave("figs/Kr.png", width = 6.5, height = 6.5, units = "in", dpi = 300)

krnew |>
  ggplot(aes(value, Kr, color = Age)) +
  stat_summary(fun = mean, geom = "line")

krnew |>
  ggplot(aes(summer, Kr, color = Age)) +
  stat_summary(fun = mean, geom = "line")

krnew |>
  ggplot(aes(winter, Kr, color = Age)) +
  stat_summary(fun = mean, geom = "smooth") +
  geom_point(alpha = 0.2)
