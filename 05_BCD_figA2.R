library(dplyr)
library(viridis)
library(mgcv)
library(visreg)
library("rnaturalearth") 
library(ggplot2)
library(cowplot)
library(ggeffects)
library(DHARMa)
library(rethinking) # this is just for colors!
# can use rethinking slim: devtools::install_github("rmcelreath/rethinking@slim")
library(sf)
library(oce)
readRDS("results/g2F.rds")
load("results/crab_cod_clean.Rdata")

# aiming for facet_wrap plots, each column is a model, each row is a variable

# original model
g2F <- gam(sn_bcd_yn ~ year_fact + 
             s(jday) + t2(X,Y) +
             s(bot_temp) + s(deep) + 
             s(sn_pop) + s(sn_avg_width) + s(sn_avg_shell), crab_cod_clean, 
           family = binomial(), select = TRUE, method = "REML") 
# no penalization
g2_NP <- gam(sn_bcd_yn ~ year_fact + 
             s(jday) + t2(X,Y) +
             s(bot_temp) + s(deep) + 
             s(sn_pop) + s(sn_avg_width) + s(sn_avg_shell), crab_cod_clean, 
           family = binomial(), method = "REML") 
# bs = ts
g2_BS <- gam(sn_bcd_yn ~ year_fact + 
             s(jday, bs = "ts") + t2(X,Y, bs = "ts") +
             s(bot_temp, bs = "ts") + s(deep, bs = "ts") + 
             s(sn_pop, bs = "ts") + s(sn_avg_width, bs = "ts") + s(sn_avg_shell, bs = "ts"), crab_cod_clean, 
           family = binomial(), method = "REML") 

var_list = c(
  year_fact = 2010,
  jday = mean(crab_cod_clean$jday),
  bot_temp = mean(crab_cod_clean$bot_temp),
  deep = mean(crab_cod_clean$deep),
  sn_pop = mean(crab_cod_clean$sn_pop),
  X = mean(crab_cod_clean$X),
  Y = mean(crab_cod_clean$Y),
  sn_avg_width = mean(crab_cod_clean$sn_avg_width),
  sn_avg_shell = mean(crab_cod_clean$sn_avg_shell)
)

#-----YEAR-----
{year_fact_pred95_2 <- ggpredict(g2F, terms = "year_fact", condition = var_list)
year_fact_pred50_2 <- ggpredict(g2F, terms = "year_fact", condition = var_list, ci.lvl = 0.5)
year_fact_pred95_NP <- ggpredict(g2_NP, terms = "year_fact", condition = var_list)
year_fact_pred50_NP <- ggpredict(g2_NP, terms = "year_fact", condition = var_list, ci.lvl = 0.5)
year_fact_pred95_BS <- ggpredict(g2_BS, terms = "year_fact", condition = var_list)
year_fact_pred50_BS <- ggpredict(g2_BS, terms = "year_fact", condition = var_list, ci.lvl = 0.5)
year_fact_pred95_2$year <- as.numeric(year_fact_pred95_2$x) + 1988
year_fact_pred95_2$year[32] <- 2021
year_fact_pred95_2$conf <- rep(0.95); year_fact_pred95_2$model <- rep("main text")
year_fact_pred50_2$year <- as.numeric(year_fact_pred50_2$x) + 1988
year_fact_pred50_2$year[32] <- 2021
year_fact_pred50_2$conf <- rep(0.5); year_fact_pred50_2$model <- rep("main text")
year_fact_pred95_NP$year <- as.numeric(year_fact_pred95_NP$x) + 1988
year_fact_pred95_NP$year[32] <- 2021
year_fact_pred95_NP$conf <- rep(0.95); year_fact_pred95_NP$model <- rep("no pen.")
year_fact_pred50_NP$year <- as.numeric(year_fact_pred50_NP$x) + 1988
year_fact_pred50_NP$year[32] <- 2021
year_fact_pred50_NP$conf <- rep(0.5); year_fact_pred50_NP$model <- rep("no pen.")
year_fact_pred95_BS$year <- as.numeric(year_fact_pred95_BS$x) + 1988
year_fact_pred95_BS$year[32] <- 2021
year_fact_pred95_BS$conf <- rep(0.95); year_fact_pred95_BS$model <- rep("ts = 'bs'")
year_fact_pred50_BS$year <- as.numeric(year_fact_pred50_BS$x) + 1988
year_fact_pred50_BS$year[32] <- 2021
year_fact_pred50_BS$conf <- rep(0.5); year_fact_pred50_BS$model <- rep("ts = 'bs'")
year_fact <- rbind(year_fact_pred95_2, year_fact_pred50_2, 
                   year_fact_pred95_NP, year_fact_pred50_NP, 
                   year_fact_pred95_BS, year_fact_pred50_BS)}
year_A2 <- ggplot(NULL, aes(year, log(predicted/(1-predicted)), col = model)) + 
  geom_linerange(data = year_fact %>% filter(conf == 0.95), 
                 aes(ymin = log(conf.low/(1-conf.low)), ymax = log(conf.high/(1-conf.high))), 
                 alpha = 0.2, lwd = 1) + 
  geom_linerange(data = year_fact %>% filter(conf == 0.5), 
                 aes(ymin = log(conf.low/(1-conf.low)), ymax = log(conf.high/(1-conf.high))), 
                 alpha = 0.5, lwd = 1) + 
  geom_point(data = year_fact %>% filter(conf == 0.5), size = 1, alpha = 1) + 
  geom_point(data = crab_cod_clean, aes(year, 7.5*sn_bcd_yn-7.5), pch = "|", col = "gray78") + 
  facet_wrap(~model) + 
  theme_classic() +   theme(text = element_text(size = 12)) + 
  labs(x = "Year", y = "log odds \nStation BCD+") + scale_color_manual(values = c("#1F6AB0", "#5ec962", "#440154"))

#-----DATE-----
{jday_pred95_2 <- ggpredict(g2F, terms = "jday", condition = var_list)
jday_pred50_2 <- ggpredict(g2F, terms = "jday", condition = var_list, ci.lvl = 0.5)
jday_pred95_NP <- ggpredict(g2_NP, terms = "jday", condition = var_list)
jday_pred50_NP <- ggpredict(g2_NP, terms = "jday", condition = var_list, ci.lvl = 0.5)
jday_pred95_BS <- ggpredict(g2_BS, terms = "jday", condition = var_list)
jday_pred50_BS <- ggpredict(g2_BS, terms = "jday", condition = var_list, ci.lvl = 0.5)
jday_pred95_2$conf <- rep(0.95); jday_pred95_2$model <- rep("main text")
jday_pred50_2$conf <- rep(0.5); jday_pred50_2$model <- rep("main text")
jday_pred95_NP$conf <- rep(0.95); jday_pred95_NP$model <- rep("no pen.")
jday_pred50_NP$conf <- rep(0.5); jday_pred50_NP$model <- rep("no pen.")
jday_pred95_BS$conf <- rep(0.95); jday_pred95_BS$model <- rep("ts = 'bs'")
jday_pred50_BS$conf <- rep(0.5); jday_pred50_BS$model <- rep("ts = 'bs'")
jday <- rbind(jday_pred95_2, jday_pred50_2, 
                   jday_pred95_NP, jday_pred50_NP, 
                   jday_pred95_BS, jday_pred50_BS)}
jday_A2 <- ggplot(NULL, aes(x, log(predicted/(1-predicted)), fill = model)) + 
  geom_ribbon(data = jday %>% filter(conf == 0.95), 
                 aes(ymin = log(conf.low/(1-conf.low)), ymax = log(conf.high/(1-conf.high))), 
                 alpha = 0.2) + 
  geom_ribbon(data = jday %>% filter(conf == 0.5), 
                 aes(ymin = log(conf.low/(1-conf.low)), ymax = log(conf.high/(1-conf.high))), 
                 alpha = 0.5) + 
  geom_line(data = jday %>% filter(conf == 0.5), aes(col = model), size = 1.2, alpha = 1) + 
  geom_point(data = crab_cod_clean, aes(jday, 7.5*sn_bcd_yn-7.5, fill = NULL), pch = "|", col = "gray78") + 
  facet_wrap(~model) + 
  theme_classic() +   theme(text = element_text(size = 12)) + 
  labs(x = "Sample date (Julian day)", y = "log odds \nStation BCD+") + scale_color_manual(values = c("#1F6AB0", "#5ec962", "#440154")) + scale_fill_manual(values = c("#1F6AB0", "#5ec962", "#440154"))




#-----POP-----
{sn_pop_pred95_2 <- ggpredict(g2F, terms = "sn_pop", condition = var_list)
sn_pop_pred50_2 <- ggpredict(g2F, terms = "sn_pop", condition = var_list, ci.lvl = 0.5)
sn_pop_pred95_NP <- ggpredict(g2_NP, terms = "sn_pop", condition = var_list)
sn_pop_pred50_NP <- ggpredict(g2_NP, terms = "sn_pop", condition = var_list, ci.lvl = 0.5)
sn_pop_pred95_BS <- ggpredict(g2_BS, terms = "sn_pop", condition = var_list)
sn_pop_pred50_BS <- ggpredict(g2_BS, terms = "sn_pop", condition = var_list, ci.lvl = 0.5)
sn_pop_pred95_2$conf <- rep(0.95); sn_pop_pred95_2$model <- rep("main text")
sn_pop_pred50_2$conf <- rep(0.5); sn_pop_pred50_2$model <- rep("main text")
sn_pop_pred95_NP$conf <- rep(0.95); sn_pop_pred95_NP$model <- rep("no pen.")
sn_pop_pred50_NP$conf <- rep(0.5); sn_pop_pred50_NP$model <- rep("no pen.")
sn_pop_pred95_BS$conf <- rep(0.95); sn_pop_pred95_BS$model <- rep("ts = 'bs'")
sn_pop_pred50_BS$conf <- rep(0.5); sn_pop_pred50_BS$model <- rep("ts = 'bs'")
sn_pop <- rbind(sn_pop_pred95_2, sn_pop_pred50_2, 
              sn_pop_pred95_NP, sn_pop_pred50_NP, 
              sn_pop_pred95_BS, sn_pop_pred50_BS)}
sn_pop_A2 <- ggplot(NULL, aes(x, log(predicted/(1-predicted)), fill = model)) + 
  geom_ribbon(data = sn_pop %>% filter(conf == 0.95), 
              aes(ymin = log(conf.low/(1-conf.low)), ymax = log(conf.high/(1-conf.high))), 
              alpha = 0.2) + 
  geom_ribbon(data = sn_pop %>% filter(conf == 0.5), 
              aes(ymin = log(conf.low/(1-conf.low)), ymax = log(conf.high/(1-conf.high))), 
              alpha = 0.5) + 
  geom_line(data = sn_pop %>% filter(conf == 0.5), aes(col = model), size = 1.2, alpha = 1) + 
  geom_point(data = crab_cod_clean, aes(sn_pop, 8.5*sn_bcd_yn-9, fill = NULL), pch = "|", col = "gray78") + 
  facet_wrap(~model) + 
  theme_classic() +   theme(text = element_text(size = 12)) + 
  labs(x = "Snow crab density (log(CPUE))", y = "log odds \nStation BCD+") + scale_color_manual(values = c("#1F6AB0", "#5ec962", "#440154")) + scale_fill_manual(values = c("#1F6AB0", "#5ec962", "#440154"))




#-----WIDTH-----
{sn_avg_width_pred95_2 <- ggpredict(g2F, terms = "sn_avg_width", condition = var_list)
sn_avg_width_pred50_2 <- ggpredict(g2F, terms = "sn_avg_width", condition = var_list, ci.lvl = 0.5)
sn_avg_width_pred95_NP <- ggpredict(g2_NP, terms = "sn_avg_width", condition = var_list)
sn_avg_width_pred50_NP <- ggpredict(g2_NP, terms = "sn_avg_width", condition = var_list, ci.lvl = 0.5)
sn_avg_width_pred95_BS <- ggpredict(g2_BS, terms = "sn_avg_width", condition = var_list)
sn_avg_width_pred50_BS <- ggpredict(g2_BS, terms = "sn_avg_width", condition = var_list, ci.lvl = 0.5)
sn_avg_width_pred95_2$conf <- rep(0.95); sn_avg_width_pred95_2$model <- rep("main text")
sn_avg_width_pred50_2$conf <- rep(0.5); sn_avg_width_pred50_2$model <- rep("main text")
sn_avg_width_pred95_NP$conf <- rep(0.95); sn_avg_width_pred95_NP$model <- rep("no pen.")
sn_avg_width_pred50_NP$conf <- rep(0.5); sn_avg_width_pred50_NP$model <- rep("no pen.")
sn_avg_width_pred95_BS$conf <- rep(0.95); sn_avg_width_pred95_BS$model <- rep("ts = 'bs'")
sn_avg_width_pred50_BS$conf <- rep(0.5); sn_avg_width_pred50_BS$model <- rep("ts = 'bs'")
sn_avg_width <- rbind(sn_avg_width_pred95_2, sn_avg_width_pred50_2, 
                sn_avg_width_pred95_NP, sn_avg_width_pred50_NP, 
                sn_avg_width_pred95_BS, sn_avg_width_pred50_BS)}
sn_avg_width_A2 <- ggplot(NULL, aes(x, log(predicted/(1-predicted)), fill = model)) + 
  geom_ribbon(data = sn_avg_width %>% filter(conf == 0.95), 
              aes(ymin = log(conf.low/(1-conf.low)), ymax = log(conf.high/(1-conf.high))), 
              alpha = 0.2) + 
  geom_ribbon(data = sn_avg_width %>% filter(conf == 0.5), 
              aes(ymin = log(conf.low/(1-conf.low)), ymax = log(conf.high/(1-conf.high))), 
              alpha = 0.5) + 
  geom_line(data = sn_avg_width %>% filter(conf == 0.5), aes(col = model), size = 1.2, alpha = 1) + 
  geom_point(data = crab_cod_clean, aes(sn_avg_width, 7.5*sn_bcd_yn-7.5, fill = NULL), pch = "|", col = "gray78") + 
  facet_wrap(~model) + 
  theme_classic() +   theme(text = element_text(size = 12)) + 
  labs(x = "Mean carapace width (mm)", y = "log odds \nStation BCD+") + scale_color_manual(values = c("#1F6AB0", "#5ec962", "#440154")) + scale_fill_manual(values = c("#1F6AB0", "#5ec962", "#440154"))




#-----SHELL-----
{sn_avg_shell_pred95_2 <- ggpredict(g2F, terms = "sn_avg_shell", condition = var_list)
sn_avg_shell_pred50_2 <- ggpredict(g2F, terms = "sn_avg_shell", condition = var_list, ci.lvl = 0.5)
sn_avg_shell_pred95_NP <- ggpredict(g2_NP, terms = "sn_avg_shell", condition = var_list)
sn_avg_shell_pred50_NP <- ggpredict(g2_NP, terms = "sn_avg_shell", condition = var_list, ci.lvl = 0.5)
sn_avg_shell_pred95_BS <- ggpredict(g2_BS, terms = "sn_avg_shell", condition = var_list)
sn_avg_shell_pred50_BS <- ggpredict(g2_BS, terms = "sn_avg_shell", condition = var_list, ci.lvl = 0.5)
sn_avg_shell_pred95_2$conf <- rep(0.95); sn_avg_shell_pred95_2$model <- rep("main text")
sn_avg_shell_pred50_2$conf <- rep(0.5); sn_avg_shell_pred50_2$model <- rep("main text")
sn_avg_shell_pred95_NP$conf <- rep(0.95); sn_avg_shell_pred95_NP$model <- rep("no pen.")
sn_avg_shell_pred50_NP$conf <- rep(0.5); sn_avg_shell_pred50_NP$model <- rep("no pen.")
sn_avg_shell_pred95_BS$conf <- rep(0.95); sn_avg_shell_pred95_BS$model <- rep("ts = 'bs'")
sn_avg_shell_pred50_BS$conf <- rep(0.5); sn_avg_shell_pred50_BS$model <- rep("ts = 'bs'")
sn_avg_shell <- rbind(sn_avg_shell_pred95_2, sn_avg_shell_pred50_2, 
                      sn_avg_shell_pred95_NP, sn_avg_shell_pred50_NP, 
                      sn_avg_shell_pred95_BS, sn_avg_shell_pred50_BS)}
sn_avg_shell_A2 <- ggplot(NULL, aes(x, log(predicted/(1-predicted)), fill = model)) + 
  geom_ribbon(data = sn_avg_shell %>% filter(conf == 0.95), 
              aes(ymin = log(conf.low/(1-conf.low)), ymax = log(conf.high/(1-conf.high))), 
              alpha = 0.2) + 
  geom_ribbon(data = sn_avg_shell %>% filter(conf == 0.5), 
              aes(ymin = log(conf.low/(1-conf.low)), ymax = log(conf.high/(1-conf.high))), 
              alpha = 0.5) + 
  geom_line(data = sn_avg_shell %>% filter(conf == 0.5), aes(col = model), size = 1.2, alpha = 1) + 
  geom_point(data = crab_cod_clean, aes(sn_avg_shell, 7.5*sn_bcd_yn-7.5, fill = NULL), pch = "|", col = "gray78") + 
  facet_wrap(~model) + 
  theme_classic() +   theme(text = element_text(size = 12)) + 
  labs(x = "Mean shell condition", y = "log odds \nStation BCD+") + scale_color_manual(values = c("#1F6AB0", "#5ec962", "#440154")) + scale_fill_manual(values = c("#1F6AB0", "#5ec962", "#440154"))



#-----TEMP-----
{bot_temp_pred95_2 <- ggpredict(g2F, terms = "bot_temp", condition = var_list)
bot_temp_pred50_2 <- ggpredict(g2F, terms = "bot_temp", condition = var_list, ci.lvl = 0.5)
bot_temp_pred95_NP <- ggpredict(g2_NP, terms = "bot_temp", condition = var_list)
bot_temp_pred50_NP <- ggpredict(g2_NP, terms = "bot_temp", condition = var_list, ci.lvl = 0.5)
bot_temp_pred95_BS <- ggpredict(g2_BS, terms = "bot_temp", condition = var_list)
bot_temp_pred50_BS <- ggpredict(g2_BS, terms = "bot_temp", condition = var_list, ci.lvl = 0.5)
bot_temp_pred95_2$conf <- rep(0.95); bot_temp_pred95_2$model <- rep("main text")
bot_temp_pred50_2$conf <- rep(0.5); bot_temp_pred50_2$model <- rep("main text")
bot_temp_pred95_NP$conf <- rep(0.95); bot_temp_pred95_NP$model <- rep("no pen.")
bot_temp_pred50_NP$conf <- rep(0.5); bot_temp_pred50_NP$model <- rep("no pen.")
bot_temp_pred95_BS$conf <- rep(0.95); bot_temp_pred95_BS$model <- rep("ts = 'bs'")
bot_temp_pred50_BS$conf <- rep(0.5); bot_temp_pred50_BS$model <- rep("ts = 'bs'")
bot_temp <- rbind(bot_temp_pred95_2, bot_temp_pred50_2, 
                      bot_temp_pred95_NP, bot_temp_pred50_NP, 
                      bot_temp_pred95_BS, bot_temp_pred50_BS)}
bot_temp_A2 <- ggplot(NULL, aes(x, log(predicted/(1-predicted)), fill = model)) + 
  geom_ribbon(data = bot_temp %>% filter(conf == 0.95), 
              aes(ymin = log(conf.low/(1-conf.low)), ymax = log(conf.high/(1-conf.high))), 
              alpha = 0.2) + 
  geom_ribbon(data = bot_temp %>% filter(conf == 0.5), 
              aes(ymin = log(conf.low/(1-conf.low)), ymax = log(conf.high/(1-conf.high))), 
              alpha = 0.5) + 
  geom_line(data = bot_temp %>% filter(conf == 0.5), aes(col = model), size = 1.2, alpha = 1) + 
  geom_point(data = crab_cod_clean, aes(bot_temp, 7.5*sn_bcd_yn-7.5, fill = NULL), pch = "|", col = "gray78") + 
  facet_wrap(~model) + 
  theme_classic() +   theme(text = element_text(size = 12)) + 
  labs(x = "Bottom temperature (°C)", y = "log odds \nStation BCD+") + scale_color_manual(values = c("#1F6AB0", "#5ec962", "#440154")) + scale_fill_manual(values = c("#1F6AB0", "#5ec962", "#440154"))



#-----DEEP-----
{deep_pred95_2 <- ggpredict(g2F, terms = "deep", condition = var_list)
deep_pred50_2 <- ggpredict(g2F, terms = "deep", condition = var_list, ci.lvl = 0.5)
deep_pred95_NP <- ggpredict(g2_NP, terms = "deep", condition = var_list)
deep_pred50_NP <- ggpredict(g2_NP, terms = "deep", condition = var_list, ci.lvl = 0.5)
deep_pred95_BS <- ggpredict(g2_BS, terms = "deep", condition = var_list)
deep_pred50_BS <- ggpredict(g2_BS, terms = "deep", condition = var_list, ci.lvl = 0.5)
deep_pred95_2$conf <- rep(0.95); deep_pred95_2$model <- rep("main text")
deep_pred50_2$conf <- rep(0.5); deep_pred50_2$model <- rep("main text")
deep_pred95_NP$conf <- rep(0.95); deep_pred95_NP$model <- rep("no pen.")
deep_pred50_NP$conf <- rep(0.5); deep_pred50_NP$model <- rep("no pen.")
deep_pred95_BS$conf <- rep(0.95); deep_pred95_BS$model <- rep("ts = 'bs'")
deep_pred50_BS$conf <- rep(0.5); deep_pred50_BS$model <- rep("ts = 'bs'")
deep <- rbind(deep_pred95_2, deep_pred50_2, 
                  deep_pred95_NP, deep_pred50_NP, 
                  deep_pred95_BS, deep_pred50_BS)}
deep_A2 <- ggplot(NULL, aes(x, log(predicted/(1-predicted)), fill = model)) + 
  geom_ribbon(data = deep %>% filter(conf == 0.95), 
              aes(ymin = log(conf.low/(1-conf.low)), ymax = log(conf.high/(1-conf.high))), 
              alpha = 0.2) + 
  geom_ribbon(data = deep %>% filter(conf == 0.5), 
              aes(ymin = log(conf.low/(1-conf.low)), ymax = log(conf.high/(1-conf.high))), 
              alpha = 0.5) + 
  geom_line(data = deep %>% filter(conf == 0.5), aes(col = model), size = 1.2, alpha = 1) + 
  geom_point(data = crab_cod_clean, aes(deep, 7.5*sn_bcd_yn-7.5, fill = NULL), pch = "|", col = "gray78") + 
  facet_wrap(~model) + 
  theme_classic() +   theme(text = element_text(size = 12)) + 
  labs(x = "Station bottom depth (m)", y = "log odds \nStation BCD+") + scale_color_manual(values = c("#1F6AB0", "#5ec962", "#440154")) + scale_fill_manual(values = c("#1F6AB0", "#5ec962", "#440154"))




#-----SPACE-----
var_list <- list(
  year_fact = as.factor(2010),
  jday = mean(crab_cod_clean$jday),
  bot_temp = mean(crab_cod_clean$bot_temp),
  # deep = mean(crab_cod_clean$deep),
  sn_pop = mean(crab_cod_clean$sn_pop),
  # X = mean(sn_pos$X),
  # Y = mean(sn_pos$Y),
  sn_avg_width = mean(crab_cod_clean$sn_avg_width),
  sn_avg_shell = mean(crab_cod_clean$sn_avg_shell)
)
# this is getting the space filter to show visreg at stations
pts1 <- st_as_sf(x = crab_cod_clean, coords = c('X', 'Y'), crs = '+init=EPSG:32602')
my_hull <- st_convex_hull(st_union(pts1))
# get map of AK
world <- ne_countries(scale = "medium", returnclass = "sf")
lon_1<-min(crab_cod_clean$long,na.rm=T)-0.5
lon_2<-max(crab_cod_clean$long,na.rm=T)+0.5
lat_1<-min(crab_cod_clean$lat,na.rm=T)-0.5
lat_2<-max(crab_cod_clean$lat,na.rm=T)+0.5
xy_p1 <- visreg2d(g2F, "X", "Y", cond = var_list, scale = "response")
xy_zl1 <- unlist(as.list(as.matrix(xy_p1[["z"]])))
xy_d1 <- expand.grid(x_val = xy_p1$x, y_val = xy_p1$y)
xy_d1$z_val <- xy_zl1
# this is filtering and shape conversion -- this is in UTM
xy_d1_shape <- st_as_sf(xy_d1, coords = c("x_val","y_val"), crs = '+init=EPSG:32602')
filter_xy_d1 <- st_intersection(xy_d1_shape, my_hull)
xy_d1_DF <- data.frame(filter_xy_d1); xy_d1_DF$geometry <- NULL
xy_d1_DF <- merge(xy_d1, xy_d1_DF, by = c("z_val"))
colnames(xy_d1_DF)[2] ="X"
colnames(xy_d1_DF)[3] ="Y"
# reconvert to data frame 
lat_long <- utm2lonlat(xy_d1_DF$X, xy_d1_DF$Y, zone = 2, hemisphere = "N", km = T)
xy_d1_DF$lat <- lat_long$latitude; xy_d1_DF$long <- lat_long$longitude
# plot: for A2 text
XY_2 <- ggplot(NULL) +
  geom_tile(data = xy_d1_DF, aes(long, lat,
                                 fill = (log(z_val/(1-z_val))),
                                 height = 0.2, width = 0.22)) +
  scale_fill_distiller(palette="Blues",
                       direction = 1, na.value="grey",
  # scale_fill_viridis(direction = -1, begin = 0, end = 0.8,
  name = "log odds \nstation BCD +") + 
  # guide = guide_legend(direction = "horizontal")) +
  theme_classic() +
  theme(legend.position="bottom", legend.key.size = unit(0.45, 'cm'),         
        legend.title = element_text(size=6), 
        axis.text.x = element_text(angle = 45, vjust = 0.5, size =6),
        axis.text.y = element_text(size =6),
        strip.text.x = element_text(margin= margin(1,0,1,0))) + # , legend.text = element_text(size=4)) +
  scale_y_continuous(breaks = c(54, 56, 58, 60, 62)) + 
  labs(y = "Latitude", x = "Longitude")  +
  geom_sf(data=world) +
  coord_sf(xlim = c(lon_1,lon_2), ylim = c(lat_1,lat_2), expand = FALSE)

xy_p1 <- visreg2d(g2_NP, "X", "Y", cond = var_list, scale = "response")
xy_zl1 <- unlist(as.list(as.matrix(xy_p1[["z"]])))
xy_d1 <- expand.grid(x_val = xy_p1$x, y_val = xy_p1$y)
xy_d1$z_val <- xy_zl1
# this is filtering and shape conversion -- this is in UTM
xy_d1_shape <- st_as_sf(xy_d1, coords = c("x_val","y_val"), crs = '+init=EPSG:32602')
filter_xy_d1 <- st_intersection(xy_d1_shape, my_hull)
xy_d1_DF <- data.frame(filter_xy_d1); xy_d1_DF$geometry <- NULL
xy_d1_DF <- merge(xy_d1, xy_d1_DF, by = c("z_val"))
colnames(xy_d1_DF)[2] ="X"
colnames(xy_d1_DF)[3] ="Y"
# reconvert to data frame 
lat_long <- utm2lonlat(xy_d1_DF$X, xy_d1_DF$Y, zone = 2, hemisphere = "N", km = T)
xy_d1_DF$lat <- lat_long$latitude; xy_d1_DF$long <- lat_long$longitude
# plot: for A2 text
XY_NP <- ggplot(NULL) +
  geom_tile(data = xy_d1_DF, aes(long, lat,
                                 fill = (log(z_val/(1-z_val))),
                                 height = 0.2, width = 0.22)) +
  scale_fill_distiller(palette="Greens",
                       direction = 1, na.value="grey",
                       # scale_fill_viridis(direction = -1, begin = 0, end = 0.8,
                       name = "log odds \nStation BCD +") + 
  # guide = guide_legend(direction = "horizontal")) +
  theme_classic() +
  theme(legend.position="bottom", legend.key.size = unit(0.45, 'cm'),         
        legend.title = element_text(size=6), 
        axis.text.x = element_text(angle = 45, vjust = 0.5, size =6),
        axis.text.y = element_text(size =6),
        strip.text.x = element_text(margin= margin(1,0,1,0))) + # , legend.text = element_text(size=4)) +
  scale_y_continuous(breaks = c(54, 56, 58, 60, 62)) + 
  labs(y = "Latitude", x = "Longitude")  +
  geom_sf(data=world) +
  coord_sf(xlim = c(lon_1,lon_2), ylim = c(lat_1,lat_2), expand = FALSE)

xy_p1 <- visreg2d(g2_BS, "X", "Y", cond = var_list, scale = "response")
xy_zl1 <- unlist(as.list(as.matrix(xy_p1[["z"]])))
xy_d1 <- expand.grid(x_val = xy_p1$x, y_val = xy_p1$y)
xy_d1$z_val <- xy_zl1
# this is filtering and shape conversion -- this is in UTM
xy_d1_shape <- st_as_sf(xy_d1, coords = c("x_val","y_val"), crs = '+init=EPSG:32602')
filter_xy_d1 <- st_intersection(xy_d1_shape, my_hull)
xy_d1_DF <- data.frame(filter_xy_d1); xy_d1_DF$geometry <- NULL
xy_d1_DF <- merge(xy_d1, xy_d1_DF, by = c("z_val"))
colnames(xy_d1_DF)[2] ="X"
colnames(xy_d1_DF)[3] ="Y"
# reconvert to data frame 
lat_long <- utm2lonlat(xy_d1_DF$X, xy_d1_DF$Y, zone = 2, hemisphere = "N", km = T)
xy_d1_DF$lat <- lat_long$latitude; xy_d1_DF$long <- lat_long$longitude
# plot: for A2 text
XY_BS <- ggplot(NULL) +
  geom_tile(data = xy_d1_DF, aes(long, lat,
                                 fill = (log(z_val/(1-z_val))),
                                 height = 0.2, width = 0.22)) +
  scale_fill_distiller(palette="Purples",
                       direction = 1, na.value="grey",
                       # scale_fill_viridis(direction = -1, begin = 0, end = 0.8,
                       name = "log odds \nStation BCD +") + 
                       # guide = guide_legend(direction = "horizontal")) +
  theme_classic() +
  theme(legend.position="bottom", legend.key.size = unit(0.45, 'cm'),         
        legend.title = element_text(size=6), 
        axis.text.x = element_text(angle = 45, vjust = 0.5, size =6),
        axis.text.y = element_text(size =6),
        strip.text.x = element_text(margin= margin(1,0,1,0))) + # , legend.text = element_text(size=4)) +
  scale_y_continuous(breaks = c(54, 56, 58, 60, 62)) + 
  labs(y = "Latitude", x = "Longitude") +
  geom_sf(data=world) +
  coord_sf(xlim = c(lon_1,lon_2), ylim = c(lat_1,lat_2), expand = FALSE)


# ----SAVE FIGURES----
pg_factors_A2 <- plot_grid(
  sn_pop_A2,
  sn_avg_width_A2,
  sn_avg_shell_A2,
  deep_A2,
  bot_temp_A2,
  ncol = 1,
  labels = c("A*",  "B*", "C*", "D", "E*"),
  label_x = 0, label_y = 1,
  hjust = -0.5, vjust = 1)
png("plots/figA2_pg_factors.png",height=250,width=170,res=400,units='mm')
print(pg_factors_A2)
dev.off()

pg_time <- plot_grid(year_A2, jday_A2, ncol = 1, labels = c("A*", "B"))
pg_space <- plot_grid(XY_2, XY_NP, XY_BS, ncol = 3)

pg_spacetime_A2 <- plot_grid(
  pg_time, pg_space, nrow = 2,
  labels = c(NA, "C*"),
  rel_heights = c(1, 0.5))
png("plots/figA2_pg_spacetime.png",height=170,width=170,res=400,units='mm')
print(pg_spacetime_A2)
dev.off()





