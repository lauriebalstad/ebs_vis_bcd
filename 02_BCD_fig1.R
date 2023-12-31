#' make sample plots for best fitting BCD model
#' author: LJB
#' date: 11/15/2023
#' start product: crab_cod_clean.Rdata in results folder
#' end product: fig 1 printed to plots folder

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
readRDS("~/results/g2F.rds")
load("~/results/crab_cod_clean.Rdata")

# ----VISUALIZE DATA----
world <- ne_countries(scale = "medium", returnclass = "sf")
lon_1<-min(crab_cod_clean$long,na.rm=T)
lon_2<-max(crab_cod_clean$long,na.rm=T)
lat_1<-min(crab_cod_clean$lat,na.rm=T)
lat_2<-max(crab_cod_clean$lat,na.rm=T)

# get prev overall
p_sn <- crab_cod_clean %>% 
  group_by(year) %>% 
  summarise(prev = 100*sum(sn_count_bcd)/sum(sn_Tot_Pop))

# get station prev too!
p_st <- crab_cod_clean %>% 
  group_by(year) %>% 
  summarise(prev = sum(sn_bcd_yn)/length(sn_bcd_yn)*100)

sn_prev <- ggplot(p_sn, aes(year, prev)) + 
  geom_line() + 
  labs(x = "year", y = "crab prevalence (%)") + 
  theme_classic() + 
  theme(text = element_text(size = 12))

st_prev <- ggplot(p_st, aes(year, prev)) + 
  geom_line() + 
  labs(x = "year", y = "station prevalence (%)") + 
  theme_classic() + 
  theme(text = element_text(size = 12))

st_dots <- ggplot() + 
  geom_point(data = crab_cod_clean %>% 
               filter(year %in% c(1994, 2006, 2016, 2021)), 
             aes(x = long, y = lat, col = as.factor(sn_bcd_yn)), 
             size = 1.2) +  
  theme_classic() + 
  scale_color_manual(values = c("gray80", "gray40")) + 
  geom_sf(data=world) + coord_sf(xlim = c(lon_1-1,lon_2+1), 
                                 ylim = c(lat_1-1,lat_2+1), 
                                 expand = FALSE) + 
  facet_wrap(~year, nrow = 2) + 
  theme(strip.text.x = element_text(margin= margin(1,0,1,0), size = 14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        strip.background = element_rect(color="white",fill="white"), 
        legend.position = "right", 
        text=element_text(size = 12)) + 
  labs(col = "BCD +", x = "longitutde", y = "latitude") + 
  geom_point(data = crab_cod_clean %>% 
               filter(year %in% c(1994, 2006, 2016, 2021)) %>% 
               filter(sn_bcd_yn == 1), aes(x = long, y = lat), 
             col = "gray40", size = 1.2) + 
  guides(color = guide_legend(override.aes = list(size = 8)))

time_series <- plot_grid(sn_prev, st_prev, nrow = 1, labels = "AUTO")
overview_plot <- plot_grid(time_series, st_dots, nrow = 2, rel_heights = c(0.5, 1), labels = c("", "C"))

# save figure
png("~/plots/fig1_overview_plot.jpg",height=190,width=170,res=400,units='mm')
print(overview_plot)
dev.off()
