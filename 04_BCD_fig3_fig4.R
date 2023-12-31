#' make sample plots for best fitting BCD model
#' author: LJB
#' date: 11/15/2023
#' start product: g2F.rds in results folder
#' end product: fig 3 and 4 printed to plots folder

# ----LOAD LIBS & DATA----
library(dplyr)
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
readRDS("~/results/g2F.rds")
load("~/results/crab_cod_clean.Rdata")

# ----VISUALIZE MODEL: FIG 3 CONDITIONALS----
sn_pos <- crab_cod_clean %>% filter(sn_bcd_yn == 1)
var_list = c(
  year_fact = 2010,
  jday = mean(sn_pos$jday),
  bot_temp = mean(sn_pos$bot_temp),
  deep = mean(sn_pos$deep),
  sn_pop = mean(sn_pos$sn_pop),
  X = mean(sn_pos$X),
  Y = mean(sn_pos$Y),
  sn_avg_width = mean(sn_pos$sn_avg_width),
  sn_avg_shell = mean(sn_pos$sn_avg_shell)
)
# note use of postive station means

# YEAR
year_fact_pred95_2 <- ggpredict(g2F, terms = "year_fact", condition = var_list)
year_fact_pred50_2 <- ggpredict(g2F, terms = "year_fact", condition = var_list, ci.lvl = 0.5)
# fix axis
year_fact_pred95_2$year <- as.numeric(year_fact_pred95_2$x) + 1988
year_fact_pred95_2$year[32] <- 2021
year_fact_pred50_2$year <- as.numeric(year_fact_pred50_2$x) + 1988
year_fact_pred50_2$year[32] <- 2021
year_fact_2 <- ggplot(year_fact_pred95_2, aes(year, predicted)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                col = col.alpha("mediumpurple3", 0.2)) + 
  geom_errorbar(data = year_fact_pred50_2, 
                aes(ymin = conf.low, ymax = conf.high), 
                col = col.alpha("mediumpurple3", 0.5)) + 
  geom_point(col = "mediumpurple3") + 
  theme_classic() + 
  labs(x = "year", y = "P(BCD +)") + 
  geom_point(data = crab_cod_clean, aes(year, sn_bcd_yn), pch = "|", col = "gray78") + 
  theme(text = element_text(size = 12))

# DAY
jday_pred95_2 <- ggpredict(g2F, terms = "jday", condition = var_list)
jday_pred50_2 <- ggpredict(g2F, terms = "jday", condition = var_list, ci.lvl = 0.5)
jday_2 <- ggplot(jday_pred95_2, aes(x, predicted)) + 
  geom_point(data = crab_cod_clean, aes(jday, sn_bcd_yn), pch = "|", col = "gray78") + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              fill = col.alpha("mediumpurple3", 0.2)) + 
  geom_ribbon(data = jday_pred50_2, aes(ymin = conf.low, ymax = conf.high), 
              fill = col.alpha("mediumpurple3", 0.5)) + 
  geom_line(col = "mediumpurple3") + 
  theme_classic() + 
  labs(x = "sample day", y = "P(BCD +)") + 
  theme(text = element_text(size = 12))

# SPACE
var_list <- list(
  year_fact = as.factor(2010),
  jday = mean(sn_pos$jday),
  bot_temp = mean(sn_pos$bot_temp),
  deep = mean(sn_pos$deep),
  sn_pop = mean(sn_pos$sn_pop),
  # X = mean(sn_pos$X),
  # Y = mean(sn_pos$Y),
  sn_avg_width = mean(sn_pos$sn_avg_width),
  sn_avg_shell = mean(sn_pos$sn_avg_shell)
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
# plot
XY1 <- ggplot(NULL) + 
  geom_tile(data = xy_d1_DF, aes(long, lat, 
                                 fill = (z_val), 
                                 height = 0.2, width = 0.22)) +  
  scale_fill_distiller(palette="Purples", 
                       direction = 1, na.value="grey", 
                       lim = c(0, 1), name = "probability \nBCD +") + 
  theme_classic() + 
  theme(legend.position="bottom", text = element_text(size = 12)) + 
  labs(y = "latitude", x = "longitude") + 
  geom_sf(data=world) + 
  coord_sf(xlim = c(lon_1,lon_2), ylim = c(lat_1,lat_2), expand = FALSE)


# ----VISUALIZE MODEL: FIG 4 CONDITIONALS----
sn_pos <- crab_cod_clean %>% filter(sn_bcd_yn == 1)
var_list = c(
  year_fact = 2010,
  jday = mean(sn_pos$jday),
  bot_temp = mean(sn_pos$bot_temp),
  deep = mean(sn_pos$deep),
  sn_pop = mean(sn_pos$sn_pop),
  X = mean(sn_pos$X),
  Y = mean(sn_pos$Y),
  sn_avg_width = mean(sn_pos$sn_avg_width),
  sn_avg_shell = mean(sn_pos$sn_avg_shell)
)

# POP
pop_pred95_2 <- ggpredict(g2F, terms = "sn_pop", condition = var_list)
pop_pred50_2 <- ggpredict(g2F, terms = "sn_pop", condition = var_list, ci.lvl = 0.5)
pop_2 <- ggplot(pop_pred95_2, aes(x, predicted)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              fill = col.alpha("mediumpurple3", 0.2)) + 
  geom_ribbon(data = pop_pred50_2, aes(ymin = conf.low, 
                                       ymax = conf.high), 
              fill = col.alpha("mediumpurple3", 0.5)) + 
  geom_line(col = "mediumpurple3") + 
  theme_classic() + 
  labs(x = "log(snow crab CPUE)", y = "P(BCD +)") + 
  geom_point(data = crab_cod_clean, aes(sn_pop, sn_bcd_yn), 
             pch = "|", col = "gray78") + 
  theme(text = element_text(size = 12))

# SHELL
sn_avg_shell_pred95_2 <- ggpredict(g2F, terms = "sn_avg_shell", condition = var_list)
sn_avg_shell_pred50_2 <- ggpredict(g2F, terms = "sn_avg_shell", condition = var_list, ci.lvl = 0.5)
sn_avg_shell_2 <- ggplot(sn_avg_shell_pred95_2, aes(x, predicted)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              fill = col.alpha("mediumpurple3", 0.2)) + 
  geom_ribbon(data = sn_avg_shell_pred50_2, 
              aes(ymin = conf.low, ymax = conf.high), 
              fill = col.alpha("mediumpurple3", 0.5)) + 
  geom_line(col = "mediumpurple3") +
  theme_classic() + 
  labs(x = "avg snow crab shell condition", y = "P(BCD +)") + 
  geom_point(data = crab_cod_clean, aes(sn_avg_shell, sn_bcd_yn), pch = "|", col = "gray78") + 
  theme(text = element_text(size = 12))

# WIDTH
sn_avg_width_pred95_2 <- ggpredict(g2F, terms = "sn_avg_width", condition = var_list)
sn_avg_width_pred50_2 <- ggpredict(g2F, terms = "sn_avg_width", condition = var_list, ci.lvl = 0.5)
sn_avg_width_2 <- ggplot(sn_avg_width_pred95_2, aes(x, predicted)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              fill = col.alpha("mediumpurple3", 0.2)) + 
  geom_ribbon(data = sn_avg_width_pred50_2, 
              aes(ymin = conf.low, ymax = conf.high), 
              fill = col.alpha("mediumpurple3", 0.5)) + 
  geom_line(col = "mediumpurple3") + 
  theme_classic() + 
  labs(x = "avg snow crab width", y = "P(BCD +)") + 
  geom_point(data = crab_cod_clean, aes(sn_avg_width, sn_bcd_yn), pch = "|", col = "gray78") + 
  theme(text = element_text(size = 12))

# DEPTH
deep_pred95_2 <- ggpredict(g2F, terms = "deep", condition = var_list)
deep_pred50_2 <- ggpredict(g2F, terms = "deep", condition = var_list, ci.lvl = 0.5)
deep_2 <- ggplot(deep_pred95_2, aes(x, predicted)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              fill = col.alpha("mediumpurple3", 0.2)) + 
  geom_ribbon(data = deep_pred50_2, aes(ymin = conf.low, ymax = conf.high), 
              fill = col.alpha("mediumpurple3", 0.5)) + 
  geom_line(col = "mediumpurple3") + 
  theme_classic() + 
  labs(x = "bottom depth", y = "P(BCD +)") + 
  geom_point(data = crab_cod_clean, aes(deep, sn_bcd_yn), pch = "|", col = "gray78") + 
  theme(text = element_text(size = 12))

# TEMP
bot_temp_pred95_2 <- ggpredict(g2F, terms = "bot_temp", condition = var_list)
bot_temp_pred50_2 <- ggpredict(g2F, terms = "bot_temp", condition = var_list, ci.lvl = 0.5)
bot_temp_2 <- ggplot(bot_temp_pred95_2, aes(x, predicted)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              fill = col.alpha("mediumpurple3", 0.2)) + 
  geom_ribbon(data = bot_temp_pred50_2, aes(ymin = conf.low, ymax = conf.high), 
              fill = col.alpha("mediumpurple3", 0.5)) + 
  geom_line(col = "mediumpurple3") + 
  theme_classic() + 
  labs(x = "bottom temperature", y = "P(BCD +)") + 
  geom_point(data = crab_cod_clean, aes(bot_temp, sn_bcd_yn), pch = "|", col = "gray78") + 
  theme(text = element_text(size = 12))

# ----SAVE FIGURES----
pg_factors_2 <- plot_grid(
  pop_2, 
  bot_temp_2,
  sn_avg_width_2,
  deep_2, 
  sn_avg_shell_2, 
  NULL,
  ncol = 2, 
  labels = c("A*", "D*", "B*", "E", "C*", ""),
  label_x = 0, label_y = 1,
  hjust = -0.5, vjust = 1)
png("~/plots/fig4_pg_factors_2.jpg",height=200,width=170,res=400,units='mm')
print(pg_factors_2)
dev.off()

pg_spacetime <- plot_grid(
  year_fact_2,
  jday_2, 
  XY1, nrow = 3, 
  labels = c("A*", "B", "C*"),
  rel_heights = c(0.65, 0.65, 1))
png("~/plots/fig3_pg_spacetime.jpg",height=180,width=90,res=400,units='mm')
print(pg_spacetime)
dev.off()

