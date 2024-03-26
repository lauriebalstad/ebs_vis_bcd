#' load and organize BCD data to run 0/1 models for EBS snow crab
#' author: LJB
#' date: 11/15/2023
#' start product: EBS survey CSVs in data folder
#' end product: crab_cod.Rdata in results folder

# ----LOAD LIBS----
library(dplyr)
library(lubridate)

# ----LOAD DATA----
# note BCD counts started in 1989
survDAT_sn <- readRDS("data/snow_survey.rds")
snow_bcd <- survDAT_sn %>% filter(AKFIN_SURVEY_YEAR > 1988) %>% 
  arrange(AKFIN_SURVEY_YEAR, GIS_STATION) 
snow_bcd <- survDAT_sn %>% filter(AKFIN_SURVEY_YEAR > 1988) %>% 
  arrange(AKFIN_SURVEY_YEAR, GIS_STATION) 
# survDAT_tan <- read.csv("data/EBSCrab_Haul_tanner.csv",header=T,skip=5)
survDAT_tan <- readRDS("data/tan_survey.rds")
tanner_bcd <- survDAT_tan %>% filter(AKFIN_SURVEY_YEAR > 1988) %>% 
  arrange(AKFIN_SURVEY_YEAR, GIS_STATION)
# survDAT_cod <- read.csv("data/race_cpue_by_haul.csv", skip = 7, header =T, check.names = F)
survDAT_cod <- readRDS("data/cod_survey.rds")
survDAT_cod <- survDAT_cod %>% filter(Year > 1988) %>% arrange(Year)
# allStation_cod <- read.csv("data/Haul Descriptions.csv", header = T, check.names = F)
allStation_cod <- readRDS("data/cod_haul.rds")

# ----SUMMARY DATA FOR WRANGLING----
# will help later
drvYear_sn <- as.numeric(substr(snow_bcd$CRUISE,1,4))
SurvYR_sn <- unique(drvYear_sn) # getting a list of all the unique years
AllStation_sn <-unique(snow_bcd$GIS_STATION) # and all the unique stations (445)

AllStnLoc_sn <- matrix(ncol=2,nrow=length(AllStation_sn)) # empty matrix rows are stations, cols are lat/long
for(w in 1:length(AllStation_sn))
{
  temp <- snow_bcd[snow_bcd$GIS_STATION==AllStation_sn[w],] # temporary df
  AllStnLoc_sn[w,1] <- temp$MID_LATITUDE[1] # station lat
  AllStnLoc_sn[w,2] <- temp$MID_LONGITUDE[1] # station long
}

StationYr_sn <- matrix(nrow = length(SurvYR_sn), ncol = length(AllStation_sn))

# ----STATION/YEAR COVARIATES----
# get covariates by station/year
sn_All <- matrix(nrow = length(SurvYR_sn), ncol = length(AllStation_sn)) # snow crab density
sn_bcd_count <- matrix(nrow = length(SurvYR_sn), ncol = length(AllStation_sn)) # BCD counts --> convert to 0/1 later
sn_width_avg <- matrix(nrow = length(SurvYR_sn), ncol = length(AllStation_sn)) # effort weighted avg. size 
sn_shell_avg <- matrix(nrow = length(SurvYR_sn), ncol = length(AllStation_sn)) # effort weighted avg. shell condition
sn_bot_temp <- matrix(nrow = length(SurvYR_sn), ncol = length(AllStation_sn)) # bottom temperature
sn_deep <- matrix(nrow = length(SurvYR_sn), ncol = length(AllStation_sn)) # bottom depth
sn_date <- matrix(nrow = length(SurvYR_sn), ncol = length(AllStation_sn)) # collection date

# loop through snow crab data
for(y in 1:length(SurvYR_sn))
{
  
  ## recall survDAT is now sp_bcd
  
  fileyr <- SurvYR_sn[y] # index variable
  yrDAT <- snow_bcd[snow_bcd$AKFIN_SURVEY_YEAR==SurvYR_sn[y],] 
  # grab just one year at a time (subset survDAT) -- got thorugh rows
  
  # density at station
  # go through cols -- make sure to align bc have subset of stations per year
  for(j in 1:length(AllStation_sn)) 
  {
    
    # for that year, grab one station at a time
    stationALL <- yrDAT[yrDAT$GIS_STATION==AllStation_sn[j],] 
    
    StationYr_sn[y,j] <- as.character(AllStation_sn[j]) # informational matrix fill
    
    Hauls <- (unique(stationALL$HAUL)) # will use later as index variable
    
    # total density of all crab
    sn_All_SY <- stationALL 
    # finally, by disease (to get density)
    sn_bcd <- stationALL[stationALL$DISEASE_CODE==2  &
                           !is.na(stationALL$DISEASE_CODE), ] # check this
     
    # densities across hauls in crabs per km^2 (lots of index variables)
    sn_All_temp <- NA    
    sn_bcd_temp <- NA
    
    # now grab one haul at a time (from that one station in that one year)
    for(k in 1:length(Hauls)) 
    {
      
      # total density
      SampFactM <- sn_All_SY$SAMPLING_FACTOR[which(sn_All_SY$HAUL==Hauls[k])] 
      AreaSweptM <- sn_All_SY$AREA_SWEPT[which(sn_All_SY$HAUL==Hauls[k])]
      sn_All_temp[k] <- sum(c(SampFactM/AreaSweptM), na.rm = TRUE)
      
      # finally, get density of crab with bcd
      SampFactM <- sn_bcd$SAMPLING_FACTOR[which(sn_bcd$HAUL==Hauls[k])] 
      AreaSweptM <- sn_bcd$AREA_SWEPT[which(sn_bcd$HAUL==Hauls[k])]
      sn_bcd_temp <- sum(c(SampFactM/AreaSweptM), na.rm = TRUE)

    }
    
    # putting the mean into the matrix for each group by year and station
    sn_All[y, j] <- mean(sn_All_temp)
    
    # getting disease info 
    sn_bcd_count[y, j] <- ifelse(mean(sn_bcd_temp)>=0, mean(sn_bcd_temp), NA) # for counts
    
    # adding average size at station
    sn_width_avg[y, j] <- ifelse(all(is.na(sn_All_SY$WIDTH)), NA, mean(sn_All_SY$WIDTH, na.rm = TRUE, weight = sn_All_SY$SAMPLING_FACTOR/sn_All_SY$AREA_SWEPT))
    sn_shell_avg[y, j] <- ifelse(all(is.na(sn_All_SY$SHELL_CONDITION)), NA, mean(sn_All_SY$SHELL_CONDITION, na.rm = TRUE, weight = sn_All_SY$SAMPLING_FACTOR/sn_All_SY$AREA_SWEPT))
    
    # environementals -- bottom/surface temperatures, depth
    sn_bot_temp[y, j] <- stationALL$GEAR_TEMPERATURE[1]
    sn_deep[y, j] <- stationALL$BOTTOM_DEPTH[1] 
    sn_date[y, j] <- yday(mdy(stationALL$START_DATE[1]))
    
  }
}

# ----TANNER/COD PRESENCE----
# get 0/1 tanner crab
tanner01 <- tanner_bcd %>% group_by(AKFIN_SURVEY_YEAR, GIS_STATION) %>% 
  summarise(tanner_pres = ifelse(sum(SAMPLING_FACTOR, na.rm = T) > 0, 1, 0)) 
colnames(tanner01) <- c("year", "station", "tanner_pres")
# get 0/1 cod
allStation_cod <- allStation_cod %>% filter(Year > 1988) %>% arrange(Year)
survALL_cod <- merge(survDAT_cod, allStation_cod, by=c("Year", "Haul Join ID"))
survALL_cod$station = survALL_cod$"Station ID"
cod01 <- survALL_cod  %>% group_by(Year, station) %>% 
  summarise(cod_pres = ifelse(sum(`Weight CPUE (kg/km2)`) > 0, 1, 0)) 
colnames(cod01) <- c("year", "station", "cod_pres")

# ----COMBINE INTO CRAB_COD----
# summarize data and put together
ebs_dat_sn <- data.frame(

  year = rep(SurvYR_sn,each = ncol(sn_All)), 
  station = c(t(StationYr_sn)), 
  lat = rep(AllStnLoc_sn[,1],(nrow(sn_All))), 
  long = rep(AllStnLoc_sn[,2],(nrow(sn_All))), 
  loc = "EBS", 

  bot_temp = c(t(sn_bot_temp)),
  deep = c(t(sn_deep)),
  jday = c(t(sn_date)),

  sn_Tot_Pop = c(t(sn_All)),
  
  sn_count_bcd = c(t(sn_bcd_count)), # for counts

  sn_avg_width = c(t(sn_width_avg)),
  sn_avg_shell = c(t(sn_shell_avg))
  
) 

ebs_dat_sn$sn_bcd_yn <- ifelse(ebs_dat_sn$sn_count_bcd >= 1, 1, 0)
ebs_dat_sn_temp <- ebs_dat_sn %>% filter(!is.na(station)) %>% 
  arrange(year, lat, long)

# put together with tanner and cod 01s
ebs_dat <- merge(ebs_dat_sn_temp, tanner01, by=c("year","station"))
crab_cod <- merge(ebs_dat, cod01, by = c("year", "station"))

# ----SAVE----
save(crab_cod, file = "results/crab_cod.RData")





