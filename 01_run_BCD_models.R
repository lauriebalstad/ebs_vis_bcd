#' run different 0/1 models for EBS snow crab
#' author: LJB
#' date: 11/15/2023
#' start product: crab_cod.Rdata in results folder
#' end product: g2F.Rdata, best fitting model and tables in results folder

# ----LOAD LIBS----
library(dplyr)
library(mgcv)
library(GGally)
library(stats)
library(DHARMa)
library(sdmTMB)

# ----LOAD DATA----
load("~/results/crab_cod.Rdata")
crab_cod <- crab_cod %>% filter(complete.cases(bot_temp, sn_avg_width))

# ----BASIC DATA CHECKS & PREP----
# need to get populations logged
crab_cod$sn_pop = log10(crab_cod$sn_Tot_Pop + 1) 

# get year, cod, tans as a factor 
crab_cod$year_fact = as.factor(crab_cod$year)
crab_cod$cod_yn = as.factor(crab_cod$cod_pres)
crab_cod$tan_yn = as.factor(crab_cod$tanner_pres)

# get N/Es --> suppress warnings
suppressWarnings(suppressMessages({
  crab_cod <- add_utm_columns(
  crab_cod,
  ll_names = c("long", "lat"),
  ll_crs = 4326,
  utm_names = c("X", "Y"), 
  units = "km"
)}
))

crab_cod_clean <- crab_cod # just complete cases, etc. 

save(crab_cod_clean, file = "~/results/crab_cod_clean.Rdata")

# check for covariance
figA1 <- ggcorr(crab_cod[, c(1, 3:4, 6:16)], nbreaks = 5, label = T, 
       label_round = 2, hjust = 0.75)
png("~/plots/figA1_survey_correlations.jpg",height=180,width=180,res=400,units='mm')
print(figA1)
dev.off()

# ----BUILD MODELS----
# null model 
g0 <- gam(sn_bcd_yn ~year_fact + 
            s(jday) + t2(X,Y), crab_cod_clean, 
          family = binomial(), select = TRUE)

# full model
g1 <- gam(sn_bcd_yn ~ year_fact + 
            s(jday) + t2(X,Y) + 
            cod_yn + tan_yn + 
            s(bot_temp) + s(deep) + 
            s(sn_pop) + s(sn_avg_width) + s(sn_avg_shell), crab_cod_clean, 
          family = binomial(), select = TRUE)

# demo + enviro
g2 <- gam(sn_bcd_yn ~ year_fact + 
            s(jday) + t2(X,Y) +
            s(bot_temp) + s(deep) + 
            s(sn_pop) + s(sn_avg_width) + s(sn_avg_shell), crab_cod_clean, 
          family = binomial(), select = TRUE) 

# demo + community
g3 <- gam(sn_bcd_yn ~ year_fact + 
            s(jday) + t2(X,Y) + 
            cod_yn + tan_yn + 
            s(sn_pop) + s(sn_avg_width) + s(sn_avg_shell), crab_cod_clean, 
          family = binomial(), select = TRUE)

# demo only
g4 <- gam(sn_bcd_yn ~ year_fact + 
            s(jday) + t2(X,Y) + 
            s(sn_pop) + s(sn_avg_width) + s(sn_avg_shell), crab_cod_clean, 
          family = binomial(), select = TRUE)

a <- AIC(g0, g1, g2, g3, g4)
a$ajd_r2 <- c(summary(g0)$r.sq, 
              summary(g1)$r.sq, 
              summary(g2)$r.sq, 
              summary(g3)$r.sq, 
              summary(g4)$r.sq)
a$dev_explained <- c(summary(g0)$dev.expl,
                     summary(g1)$dev.expl, 
                     summary(g2)$dev.expl, 
                     summary(g3)$dev.expl, 
                     summary(g4)$dev.expl)
a_ordered <- arrange(a, AIC) 
write.csv(a_ordered, "~/results/table2_AIC_values.csv")

# refit best model with REML
g2F <- gam(sn_bcd_yn ~ year_fact + 
             s(jday) + t2(X,Y) +
             s(bot_temp) + s(deep) + 
             s(sn_pop) + s(sn_avg_width) + s(sn_avg_shell), crab_cod_clean, 
           family = binomial(), select = TRUE, method = "REML") 
g2F_summary <- summary(g2F)
g2F_parametrics <- data.frame(year <- c("intercept", as.character(1990:2019), "2021"), 
                              estimate <- as.data.frame(g2F_summary$p.coeff)$`g2F_summary$p.coeff`,
                              std_error <- as.data.frame(g2F_summary$se)$`g2F_summary$se`[1:32], 
                              z_val <- as.data.frame(g2F_summary$p.t)$`g2F_summary$p.t`, 
                              p_value <- as.data.frame(g2F_summary$p.pv)$`g2F_summary$p.pv`)
g2F_smooths <- data.frame(variable <- c("date", "X,Y", "temperature", "depth", "pop_density", "avg_width", "avg_shell"), 
                          eff_deg_free <- as.data.frame(g2F_summary$edf)$`g2F_summary$edf`, 
                          chi_sq <- as.data.frame(g2F_summary$chi.sq)$`g2F_summary$chi.sq`, 
                          p_value <- as.data.frame(g2F_summary$s.pv)$`g2F_summary$s.pv`)
write.csv(g2F_parametrics, "~/results/table3_parametric.csv")
write.csv(g2F_smooths, "~/results/table3_smooths.csv")

# ----MODEL CHECKS----
# general checks --> think this prints in the console?
gam.check(g2F)
# residual chekcs
png("~/results/dispersion_model_check.jpg",height=170,width=340,res=400,units='mm')
print(testDispersion(g2F) )
dev.off()
so2 <- simulateResiduals(g2F, plot = F) # diddo
png("~/results/overview_model_check.jpg",height=170,width=340,res=400,units='mm')
print(plot(so2))
dev.off()
# check individual parameters
png("~/results/parameter_sample_model_check.jpg",height=170,width=170,res=400,units='mm')
print(plotResiduals(so2, crab_cod$jday)) # etc
dev.off()

saveRDS(g2F, "~/results/g2F.rds")


