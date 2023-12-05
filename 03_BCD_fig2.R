#' make sample plots for best fitting BCD model
#' author: LJB
#' date: 11/15/2023
#' start product: g2F.rds in results folder
#' end product: fig 2 printed to plots folder

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

# ----VISUALIZE MODEL: PREDICTIONS----
world <- ne_countries(scale = "medium", returnclass = "sf")
lon_1<-min(crab_cod_clean$long,na.rm=T)
lon_2<-max(crab_cod_clean$long,na.rm=T)
lat_1<-min(crab_cod_clean$lat,na.rm=T)
lat_2<-max(crab_cod_clean$lat,na.rm=T)

crab_cod_clean$pred_2 <- predict.gam(g2F)

inv_logit <- function(x) {
  return(exp(x)/(1+exp(x)))
}

real_dat <- ggplot() + 
  geom_point(data = crab_cod_clean %>% 
               filter(year %in% c(1994, 2007, 2016)), 
             aes(x = long, y = lat, col = as.factor(sn_bcd_yn)), 
             size = 1.2) +  
  theme_classic() + 
  scale_color_manual(values = c("gray80", "gray40")) + 
  geom_sf(data=world) + 
  coord_sf(xlim = c(lon_1-1,lon_2+1), 
           ylim = c(lat_1-1,lat_2+1), 
           expand = FALSE) + 
  facet_wrap(~year, nrow = 3) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 12), 
        strip.text.x = element_text(margin= margin(1,0,1,0), size = 14), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        strip.background = element_rect(color="white",fill="white"), 
        legend.position = "bottom", text=element_text(size = 12)) + 
  labs(col = "BCD +", x = "longitutde", y = "latitude") + 
  geom_point(data = crab_cod_clean %>% 
               filter(year %in% c(1994, 2007, 2016)) %>% 
               filter(sn_bcd_yn == 1), 
             aes(x = long, y = lat), 
             col = "gray40", size = 1.2) + 
  guides(color = guide_legend(override.aes = list(size = 8))) + 
  ggtitle("Data") + theme(plot.title = element_text(hjust = 0.5))

pred_2_dat <- ggplot() + 
  geom_point(data = crab_cod_clean %>% 
               filter(year %in% c(1994, 2007, 2016)), 
             aes(x = long, y = lat, col = inv_logit(pred_2))) + 
  theme_classic() + 
  scale_color_distiller(palette="Purples", 
                        direction = 1, 
                        na.value="grey", 
                        lim = c(0, 1)) + 
  labs(col = "P(BCD +)", x = "longitutde", y = "latitude") + 
  geom_sf(data=world) + 
  coord_sf(xlim = c(lon_1-1,lon_2+1), 
           ylim = c(lat_1-1,lat_2+1), 
           expand = FALSE) + 
  facet_wrap(~year, nrow = 3) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 12), 
        strip.text.x = element_text(margin= margin(1,0,1,0), size = 14), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        strip.background = element_rect(color="white",fill="white"), 
        legend.position = "bottom", 
        text=element_text(size = 12)) + 
  ggtitle("Model") + 
  theme(plot.title = element_text(hjust = 0.5)) 

legend_RD <- get_legend(
  real_dat + theme(legend.box.margin = margin(0, 0, 0, 12))
)
legend_2 <- get_legend(
  pred_2_dat + theme(legend.box.margin = margin(0, 0, 0, 12))
)
leg_plot <- plot_grid(legend_RD, legend_2, 
                      ncol = 2)

data_model <- plot_grid(
  real_dat + theme(legend.position="none"), 
  pred_2_dat+ theme(legend.position="none"),
  ncol = 2)
dat_mod_leg <- plot_grid(data_model, leg_plot, 
                         nrow = 2, rel_heights = c(1, 0.1))

# save plot to folder
png("~/plots/fig2_data_model.jpg",height=220,width=170,res=400,units='mm')
print(dat_mod_leg)
dev.off()
