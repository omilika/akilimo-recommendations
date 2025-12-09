

#####################################################################################################3
#####################################################################################################3
## no more used functions
#####################################################################################################3


#' gets lat or lon and find the closest matching lat and lon in the WLY data, at 0.05 (5Km) resolution, used to source ISRIC data
# getclosestcoor <- function(ll) {
#   seqdec <- data.frame(ss = seq(from = 0.025, to = 0.975, by = 0.05))
#   lf <- floor(ll)
#   ld <- as.numeric(paste("0.", strsplit(gsub('\\.', "_", ll), "_")[[1]][2], sep = ""))
#   seqdec$diff <- seqdec$ss - ld
#   ll2 <- lf + seqdec[seqdec$diff == min(abs(seqdec$diff)),]$ss
#   return(ll2)
# }


#' This was an attempt to make soil dat asourcing and fitting the random forest model in real time, but as it adds too much time to give advice it is not used
#' @param FCY farmers current yield, used as control yeidl for RF model
#' @param country should be NG or TZ
#' @param PD the ith number of the year
#' @param HD how many days the crop was on the field between planting and harvest
#' @example ISRIC_FRmodel_CY(lat=4.775, lon=8.425, country = "NG", FCY=3.75, PD = 254, HD= 350)
# ISRIC_FRmodel_CY <- function(lat = lat, lon = lon, country = country, FCY = FCY, PD = PD, HD = HD) {
#
#   ## get closest pixel with WLY data
#   lat2 <- getclosestcoor(ll = lat)
#   lon2 <- getclosestcoor(ll = lon)
#
#   ## get soil NPK
#   ISRIC_SoilData_t <- getISRICData(lat = lat, lon = lon, country = country)
#   SoilData_fcy <- Rfmodel_Wrapper(ISRIC_SoilData = ISRIC_SoilData_t, FCY, country = country)
#
#   ## get WLY:get PDand HD to the closest daes fr which we have WLY
#   Nigeria_WLY_365 <- readRDS("NG_WLY_LINTUL.RDS")
#   # Nigeria_WLY_365 <- readRDS("NG_WLY_LINTUL.RDS")
#   pdates <- data.frame(wlyPD = unique(Nigeria_WLY_365$pl_Date))
#   pdates$diff <- PD - pdates$wlyPD
#   PD2 <- pdates[pdates$diff == min(abs(pdates$diff)), "wlyPD"]
#   hdates <- data.frame(wlyHD = seq(214, 361, 7))
#   hdates$diff <- HD - hdates$wlyHD
#   HD2 <- hdates[hdates$diff == min(abs(hdates$diff)), "wlyHD"]
#   Nigeria_WLY_365[Nigeria_WLY_365$pl_Date == PD2,]
#   Nigeria_WLY_365[Nigeria_WLY_365$lat == lat2,]
#   wlypd <- Nigeria_WLY_365[Nigeria_WLY_365$lon == lon2 &
#                              Nigeria_WLY_365$lat == lat2 &
#                              Nigeria_WLY_365$pl_Date == PD2,]
#   wlydata <- wlypd[, c("lat", "lon", "pl_Date", "location")]
#   wlydata$water_limited_yield <- wlypd[, colnames(wlypd) == HD2]
#   wlydata$zone <- country
#   wlydata$daysOnField <- HD
#   wlydata <- wlydata[, c("lat", "lon", "water_limited_yield", "location", "pl_Date", "zone", "daysOnField")]
#
#   ## get CY
#   require(plyr)
#   wlydata$Current_Yield <- QUEFTS_WLY_CY(SoilData = SoilData_fcy, country = country, wlyd = wlydata)
#   Planting_Nigeria <- data.frame(st = seq(1, 365, 7), en = (seq(1, 365, 7) + 454), weekNr = seq(1:53))
#   WLYData <- merge(wlydata, Planting_Nigeria[, c("st", "weekNr"),], by.x = "pl_Date", by.y = "st")
#   return(WLYData)
#
# }
#


#' country will be NG or TZ
# getISRICData <- function(lat = lat, lon = lon, country = country) {
#   #setwd("/home/akilimo/projects/gisdata")
#   setwd("/home/akilimo//gisdata")
#   p <- data.frame(x = lon, y = lat)
#
#   ######################################################
#   ## Clay content (0-2 micro meter) mass fraction in %; for ODK NOT data
#   #####################################################
#
#   list_tif <- c("CLYPPT_M_sl1_250m_ll",
#                 "CLYPPT_M_sl2_250m_ll",
#                 "CLYPPT_M_sl3_250m_ll")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#
#   Clay <- stack(list_tif)
#   Clay_data <- as.data.frame(raster::extract(Clay, p))
#   Clay_data$long <- p$x
#   Clay_data$lat <- p$y
#   colnames(Clay_data) <- c("Clay_5", "Clay_15", "Clay_30", "long", "lat")
#   Clay_data$Clay_averaged <- ((Clay_data$Clay_5 * 5) +
#                                 (Clay_data$Clay_15 * 10) +
#                                 (Clay_data$Clay_30 * 15)) / 30
#
#
#   ######################################################
#   ## Cation exchange capacity of soil in cmolc/kg; TODO redownload this layers
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("CECSOL_M_sl1_250m_ll",
#                 "CECSOL_M_sl2_250m_ll",
#                 "CECSOL_M_sl3_250m_ll")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   CEC <- stack(list_tif)
#   CEC_data <- as.data.frame(raster::extract(CEC, p))
#   CEC_data$long <- p$x
#   CEC_data$lat <- p$y
#   colnames(CEC_data) <- c("CEC_5", "CEC_15", "CEC_30", "long", "lat")
#   CEC_data$CEC_averaged <- ((CEC_data$CEC_5 * 5) +
#                               (CEC_data$CEC_15 * 10) +
#                               (CEC_data$CEC_30 * 15)) / 30
#
#   ######################################################
#   ## Soil organic carbon content (fine earth fraction) in g per kg for ODK NOT data
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("ORCDRC_M_sl1_250m_ll",
#                 "ORCDRC_M_sl2_250m_ll",
#                 "ORCDRC_M_sl3_250m_ll")
#
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   SOC <- stack(list_tif)
#   SOC_data <- as.data.frame(raster::extract(SOC, p))
#   SOC_data$long <- p$x
#   SOC_data$lat <- p$y
#   colnames(SOC_data) <- c("SOC_5", "SOC_15", "SOC_30", "long", "lat")
#
#   SOC_data$SOM_5 <- SOC_data$SOC_5 * 2
#   SOC_data$SOM_15 <- SOC_data$SOC_15 * 2
#   SOC_data$SOM_30 <- SOC_data$SOC_30 * 2
#
#   SOC_data$percentSOC_5 <- SOC_data$SOC_5 / 10
#   SOC_data$percentSOC_15 <- SOC_data$SOC_15 / 10
#   SOC_data$percentSOC_30 <- SOC_data$SOC_30 / 10
#
#   SOC_data$percentSOM_5 <- SOC_data$SOM_5 / 10
#   SOC_data$percentSOM_15 <- SOC_data$SOM_15 / 10
#   SOC_data$percentSOM_30 <- SOC_data$SOM_30 / 10
#
#   SOC_data$percentSOC_averaged <- ((SOC_data$percentSOC_5 * 5) +
#                                      (SOC_data$percentSOC_15 * 10) +
#                                      (SOC_data$percentSOC_30 * 15)) / 30
#   SOC_data$percentSOM_averaged <- ((SOC_data$percentSOM_5 * 5) +
#                                      (SOC_data$percentSOM_15 * 10) +
#                                      (SOC_data$percentSOM_30 * 15)) / 30
#
#
#   ######################################################
#   ## pH x 10 in H2O for ODK NOT data
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("PHIHOX_M_sl1_250m_ll",
#                 "PHIHOX_M_sl2_250m_ll",
#                 "PHIHOX_M_sl3_250m_ll")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   pH <- stack(list_tif)
#   # extract values for points
#   pH_data <- as.data.frame(raster::extract(pH, p))
#   pH_data$long <- p$x
#   pH_data$lat <- p$y
#   colnames(pH_data) <- c("pH_5", "pH_15", "pH_30", "long", "lat")
#   pH_data$pH_averaged <- ((pH_data$pH_5 * 5) +
#                             (pH_data$pH_15 * 10) +
#                             (pH_data$pH_30 * 15)) / 30
#   pH_data$pH_5 <- pH_data$pH_5 / 10
#   pH_data$pH_15 <- pH_data$pH_15 / 10
#   pH_data$pH_30 <- pH_data$pH_30 / 10
#   pH_data$pH_averaged <- pH_data$pH_averaged / 10
#
#
#   ######################################################
#   ## Silt content (2-50 micro meter) mass fraction in % for ODK NOT data
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("SLTPPT_M_sl1_250m_ll",
#                 "SLTPPT_M_sl2_250m_ll",
#                 "SLTPPT_M_sl3_250m_ll")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   Silt <- stack(list_tif)
#   # extract values for points
#   silt_data <- as.data.frame(raster::extract(Silt, p))
#   silt_data$long <- p$x
#   silt_data$lat <- p$y
#   colnames(silt_data) <- c("silt_5", "silt_15", "silt_30", "long", "lat")
#   silt_data$silt_averaged <- ((silt_data$silt_5 * 5) +
#                                 (silt_data$silt_15 * 10) +
#                                 (silt_data$silt_30 * 15)) / 30
#   head(silt_data)
#
#
#   ######################################################
#   ## Sand content (50-2000 micro meter) mass fraction in %, for ODK NOT data
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("SNDPPT_M_sl1_250m_ll",
#                 "SNDPPT_M_sl2_250m_ll",
#                 "SNDPPT_M_sl3_250m_ll")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   Sand <- stack(list_tif)
#   # extract values for points
#   Sand_data <- as.data.frame(raster::extract(Sand, p))
#   Sand_data$long <- p$x
#   Sand_data$lat <- p$y
#   colnames(Sand_data) <- c("Sand_5", "Sand_15", "Sand_30", "long", "lat")
#   Sand_data$Sand_averaged <- ((Sand_data$Sand_5 * 5) +
#                                 (Sand_data$Sand_15 * 10) +
#                                 (Sand_data$Sand_30 * 15)) / 30
#
#
#   ######################################################
#   ## Bulk density (fine earth) in kg / cubic-meter, for ODK NOT data
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("BLDFIE_M_sl1_250m_ll",
#                 "BLDFIE_M_sl2_250m_ll",
#                 "BLDFIE_M_sl3_250m_ll")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   BD <- stack(list_tif)
#   # extract values for points
#   BD_data <- as.data.frame(raster::extract(BD, p))
#
#
#   BD_data$long <- p$x
#   BD_data$lat <- p$y
#   colnames(BD_data) <- c("BD_5", "BD_15", "BD_30", "long", "lat")
#   BD_data$tonsoilha <- ((BD_data$BD_5 * 5) +
#                           (BD_data$BD_15 * 10) +
#                           (BD_data$BD_30 * 15)) / 30
#   BD_data$kgSoilHa <- BD_data$tonsoilha * 1000
#   head(BD_data)
#
#   ######################################################
#   ## Total Nitrogen (N) content of the soil fine earth fraction in mg/kg (ppm)
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("af250m_nutrient_n_m_agg30cm")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   TN <- raster(list_tif)
#   TN_data <- as.data.frame(raster::extract(TN, p))
#   TN_data$long <- p$x
#   TN_data$lat <- p$y
#   colnames(TN_data) <- c("TotalN", "long", "lat")
#
#   ######################################################
#   ## Extractable Manganese (Mn) content of the soil fine earth fraction in mg/kg (ppm)
#   ## as measured according to the soil analytical procedure of Mehlich 3 and spatially predicted for 0-30 cm depth interval at 250 m
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("af250m_nutrient_mn_m_agg30cm")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   Mn <- raster(list_tif)
#   Mn_data <- as.data.frame(raster::extract(Mn, p))
#   Mn_data$long <- p$x
#   Mn_data$lat <- p$y
#   colnames(Mn_data) <- c("Mn", "long", "lat")
#
#
#   ######################################################
#   ## Extractable Boron (B) content of the soil fine earth fraction in
#   ## mg/100kg (pp100m) as measured according to the soil analytical procedure of Mehlich 3 and spatially predicted for 0-30 cm depth interval at 250 m
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("af250m_nutrient_b_m_agg30cm")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   B <- raster(list_tif)
#   B_data <- as.data.frame(raster::extract(B, p))
#   B_data$long <- p$x
#   B_data$lat <- p$y
#   colnames(B_data) <- c("B", "long", "lat")
#
#   ######################################################
#   ## Extractable Calcium (Ca) content of the soil fine earth fraction in mg/kg (ppm) as measured according to the soil analytical procedure
#   ## of Mehlich 3 and spatially predicted for 0-30 cm depth interval at 250 m
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("af250m_nutrient_ca_m_agg30cm")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   Ca <- raster(list_tif)
#   Ca_data <- as.data.frame(raster::extract(Ca, p))
#   Ca_data$long <- p$x
#   Ca_data$lat <- p$y
#   colnames(Ca_data) <- c("Ca", "long", "lat")
#
#   ######################################################
#   ## Extractable Iron (Fe) content of the soil fine earth fraction in mg/kg (ppm)
#   ## as measured according to the soil analytical procedure of Mehlich 3 and spatially predicted for 0-30 cm depth interval at 250 m
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("af250m_nutrient_fe_m_agg30cm")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   Fe <- raster(list_tif)
#   Fe_data <- as.data.frame(raster::extract(Fe, p))
#   Fe_data$long <- p$x
#   Fe_data$lat <- p$y
#   colnames(Fe_data) <- c("Fe", "long", "lat")
#
#
#   ######################################################
#   ##  Extractable Sodium Copper (Cu) of the soil fine earth fraction in mg/100kg (pp100m) as measured according
#   ## to the soil analytical procedure of Mehlich 3 and spatially predicted for 0-30 cm depth interval at 250 m
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("af250m_nutrient_cu_m_agg30cm")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   Cu <- raster(list_tif)
#   Cu_data <- as.data.frame(raster::extract(Cu, p))
#   Cu_data$long <- p$x
#   Cu_data$lat <- p$y
#   colnames(Cu_data) <- c("Cu", "long", "lat")
#
#
#   ######################################################
#   ## Extractable Aluminium (Al) content of the soil fine earth fraction in mg/kg (ppm) as measured according to
#   ## the soil analytical procedure of Mehlich 3 and spatially predicted for 0-30 cm depth interval at 250 m
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("af250m_nutrient_al_m_agg30cm")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   Al <- raster(list_tif)
#   Al_data <- as.data.frame(raster::extract(Al, p))
#   Al_data$long <- p$x
#   Al_data$lat <- p$y
#   colnames(Al_data) <- c("Al", "long", "lat")
#
#
#   ######################################################
#   ##  Extractable Manganese (Mn) content of the soil fine earth fraction in mg/kg (ppm) as measured according to the soil
#   ## canalytical procedure of Mehlich 3 and spatially predicted for 0-30 cm depth interval at 250 m
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("af250m_nutrient_mg_m_agg30cm")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   Mg <- raster(list_tif)
#   Mg_data <- as.data.frame(raster::extract(Mg, p))
#   Mg_data$long <- p$x
#   Mg_data$lat <- p$y
#   colnames(Mg_data) <- c("Mg", "long", "lat")
#
#
#   ######################################################
#   ## Extractable Sodium content (Na) of the soil fine earth fraction in mg/kg (ppm) as measured according to the soil
#   ## analytical procedure of Mehlich 3 and spatially predicted for 0-30 cm depth interval at 250 m
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("af250m_nutrient_na_m_agg30cm")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   Na <- raster(list_tif)
#   Na_data <- as.data.frame(raster::extract(Na, p))
#   Na_data$long <- p$x
#   Na_data$lat <- p$y
#   colnames(Na_data) <- c("Na", "long", "lat")
#
#
#   ######################################################
#   ##  Extractable Potassium (K) content of the soil fine earth fraction in mg/kg (ppm) as measured according to
#   ## the soil analytical procedure of Mehlich 3 and spatially predicted for 0-30 cm depth interval at 250 m
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("af250m_nutrient_k_m_agg30cm")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   K <- raster(list_tif)
#   K_mehlich3 <- as.data.frame(raster::extract(K, p))
#   K_mehlich3$long <- p$x
#   K_mehlich3$lat <- p$y
#   colnames(K_mehlich3) <- c("K_Mehlich", "long", "lat")
#   K_mehlich3$exchK <- K_mehlich3$K_Mehlich * 1.17
#
#   ######################################################
#   ## Extractable Phosphorus (P) content of the soil fine earth fraction in mg/100kg (pp100m)
#   ## as measured according to the soil analytical procedure of Mehlich 3 and spatially predicted for 0-30 cm depth interval at 250 m
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("af250m_nutrient_p_m_agg30cm")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   Phos <- raster(list_tif)
#   P_mehlich3 <- as.data.frame(raster::extract(Phos, p))
#   P_mehlich3$long <- p$x
#   P_mehlich3$lat <- p$y
#   colnames(P_mehlich3) <- c("P_Mehlich", "long", "lat")
#   P_mehlich3$P_Mehlich <- P_mehlich3$P_Mehlich / 100
#
#
#   ######################################################
#   ## Nutrient clusters based on fuzzy k-means of the soil fine earth fraction and spatially predicted at 250 m spatial
#   ## resolution across sub-Saharan Africa using Machine Learning (ensemble between random forest and gradient boosting)
#   ## using soil data from the Africa Soil Profiles database (AfSP) compiled by AfSIS and recent soil data newly collected by AfSIS in
#   ## partnership with EthioSIS (Ethiopia), GhaSIS (Ghana) and NiSIS (Nigeria as made possible by OCP Africa and IITA), combined with soil
#   ## data as made available by Wageningen University and Research, IFDC, VitalSigns, University of California and the OneAcreFund. [Values M = mean value predicted].
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("af250m_nutrient_ncluster_m")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   soilclusters <- stack(list_tif)
#   soilClus_data <- as.data.frame(raster::extract(soilclusters, p))
#   soilClus_data$long <- p$x
#   soilClus_data$lat <- p$y
#   colnames(soilClus_data) <- c("ncluster", "long", "lat")
#
#
#   ################ merging
#
#
#   ISRIC_NOT1 <- merge(Clay_data, CEC_data, by = c("long", "lat"))
#   ISRIC_NOT2 <- merge(ISRIC_NOT1, SOC_data, by = c("long", "lat"))
#   ISRIC_NOT3 <- merge(ISRIC_NOT2, pH_data, by = c("long", "lat"))
#   ISRIC_NOT4 <- merge(ISRIC_NOT3, silt_data, by = c("long", "lat"))
#   ISRIC_NOT5 <- merge(ISRIC_NOT4, Sand_data, by = c("long", "lat"))
#   ISRIC_NOT6 <- merge(ISRIC_NOT5, BD_data, by = c("long", "lat"))
#   ISRIC_NOT7 <- merge(ISRIC_NOT6, K_mehlich3, by = c("long", "lat"))
#   ISRIC_NOT8 <- merge(ISRIC_NOT7, P_mehlich3, by = c("long", "lat"))
#   ISRIC_NOT9 <- merge(ISRIC_NOT8, TN_data, by = c("long", "lat"))
#   ISRIC_NOT10 <- merge(ISRIC_NOT9, Mn_data, by = c("long", "lat"))
#   ISRIC_NOT11 <- merge(ISRIC_NOT10, B_data, by = c("long", "lat"))
#   # ISRIC_NOT12 <- merge(ISRIC_NOT11, Zn_data, by=c("long","lat"))
#   ISRIC_NOT13 <- merge(ISRIC_NOT11, Ca_data, by = c("long", "lat"))
#   ISRIC_NOT14 <- merge(ISRIC_NOT13, Fe_data, by = c("long", "lat"))
#   ISRIC_NOT15 <- merge(ISRIC_NOT14, Cu_data, by = c("long", "lat"))
#   ISRIC_NOT16 <- merge(ISRIC_NOT15, Al_data, by = c("long", "lat"))
#   ISRIC_NOT17 <- merge(ISRIC_NOT16, Mg_data, by = c("long", "lat"))
#   ISRIC_NOT18 <- merge(ISRIC_NOT17, Na_data, by = c("long", "lat"))
#   # ISRIC_NOT19 <- merge(ISRIC_NOT18, K_data, by=c("long","lat"))
#   # ISRIC_NOT20 <- merge(ISRIC_NOT19, P_data, by=c("long","lat"))
#   ISRIC_NOT <- merge(ISRIC_NOT18, soilClus_data, by = c("long", "lat"))
#
#   ISRIC_NOT <- droplevels(ISRIC_NOT[!is.na(ISRIC_NOT$long),])
#
#
#   YS <- NULL
#   for (k in 1:nrow(ISRIC_NOT)) {
#     ssk <- ISRIC_NOT[k,]
#     if (ssk$pH_averaged <= 6.5) {
#       ssk$olsenP <- 0.55 * ssk$P_Mehlich
#     } else if (ssk$pH_averaged <= 7.3 & ssk$pH_averaged > 6.5) {
#       ssk$olsenP <- 0.5 * ssk$P_Mehlich
#     } else {
#       ssk$olsenP <- 0.45 * ssk$P_Mehlich
#     }
#     YS <- rbind(YS, ssk)
#   }
#
#
#   ISRIC_NOT <- YS
#
#   ISRIC_NOT$KgKinHa <- ((ISRIC_NOT$exchK * ISRIC_NOT$kgSoilHa) / 1000000) ## K in kg/ha
#   ISRIC_NOT$KgPinHa <- (ISRIC_NOT$olsenP * ISRIC_NOT$kgSoilHa) / 1000000 ## P kg/ha
#
#
#   ######################################################################################################
#   ## pedotransfer function from harvest choice. sand, silt, clay and OM in percentage
#   ######################################################################################################
#
#
#   ##### WP ######
#   ISRIC_NOT$wp5a <- (-0.024 * ISRIC_NOT$Sand_5 / 100) +
#     0.487 * ISRIC_NOT$Clay_5 / 100 +
#     0.006 * ISRIC_NOT$percentSOM_5 +
#     0.005 * (ISRIC_NOT$Sand_5 / 100 * ISRIC_NOT$percentSOM_5) - 0.013 * (ISRIC_NOT$Clay_5 / 100 * ISRIC_NOT$percentSOM_5) +
#     0.068 * (ISRIC_NOT$Sand_5 / 100 * ISRIC_NOT$Clay_5 / 100) +
#     0.031
#   ISRIC_NOT$wp_5 <- (ISRIC_NOT$wp5a + (0.14 * ISRIC_NOT$wp5a - 0.02)) * 100
#
#   ISRIC_NOT$wp15a <- -0.024 * ISRIC_NOT$Sand_15 / 100 +
#     0.487 * ISRIC_NOT$Clay_15 / 100 +
#     0.006 * ISRIC_NOT$percentSOM_15 +
#     0.005 * (ISRIC_NOT$Sand_15 / 100 * ISRIC_NOT$percentSOM_15) - 0.013 * (ISRIC_NOT$Clay_15 / 100 * ISRIC_NOT$percentSOM_15) +
#     0.068 * (ISRIC_NOT$Sand_15 / 100 * ISRIC_NOT$Clay_15 / 100) +
#     0.031
#   ISRIC_NOT$wp_15 <- (ISRIC_NOT$wp15a + (0.14 * ISRIC_NOT$wp15a - 0.02)) * 100
#
#   ISRIC_NOT$wp30a <- -0.024 * ISRIC_NOT$Sand_30 / 100 +
#     0.487 * ISRIC_NOT$Clay_30 / 100 +
#     0.006 * ISRIC_NOT$percentSOM_30 +
#     0.005 * (ISRIC_NOT$Sand_30 / 100 * ISRIC_NOT$percentSOM_30) - 0.013 * (ISRIC_NOT$Clay_30 / 100 * ISRIC_NOT$percentSOM_30) +
#     0.068 * (ISRIC_NOT$Sand_30 / 100 * ISRIC_NOT$Clay_30 / 100) +
#     0.031
#   ISRIC_NOT$wp_30 <- (ISRIC_NOT$wp30a + (0.14 * ISRIC_NOT$wp30a - 0.02)) * 100
#
#   ISRIC_NOT$wpA <- -0.024 * ISRIC_NOT$Sand_averaged / 100 +
#     0.487 * ISRIC_NOT$Clay_averaged / 100 +
#     0.006 * ISRIC_NOT$percentSOM_averaged +
#     0.005 * (ISRIC_NOT$Sand_averaged / 100 * ISRIC_NOT$percentSOM_averaged) - 0.013 * (ISRIC_NOT$Clay_averaged / 100 * ISRIC_NOT$percentSOM_averaged) +
#     0.068 * (ISRIC_NOT$Sand_averaged / 100 * ISRIC_NOT$Clay_averaged / 100) +
#     0.031
#   ISRIC_NOT$wp_wAverage <- (ISRIC_NOT$wpA + (0.14 * ISRIC_NOT$wpA - 0.02)) * 100
#
#   ISRIC_NOT <- subset(ISRIC_NOT, select = -c(wp5a, wp15a, wp30a, wpA))
#
#
#   ##### FC ######
#   ISRIC_NOT$FC1 <- -0.251 * ISRIC_NOT$Sand_5 / 100 +
#     0.195 * ISRIC_NOT$Clay_5 / 100 +
#     0.011 * ISRIC_NOT$percentSOM_5 +
#     0.006 * (ISRIC_NOT$Sand_5 / 100 * ISRIC_NOT$percentSOM_5) - 0.027 * (ISRIC_NOT$Clay_5 / 100 * ISRIC_NOT$percentSOM_5) +
#     0.452 * (ISRIC_NOT$Sand_5 / 100 * ISRIC_NOT$Clay_5 / 100) +
#     0.299
#   ISRIC_NOT$FC_5 <- (ISRIC_NOT$FC1 + (1.283 * ISRIC_NOT$FC1^2 -
#                                         0.374 * ISRIC_NOT$FC1 -
#                                         0.015)) * 100
#
#
#   ISRIC_NOT$FC2 <- -0.251 * ISRIC_NOT$Sand_15 / 100 +
#     0.195 * ISRIC_NOT$Clay_15 / 100 +
#     0.011 * ISRIC_NOT$percentSOM_15 +
#     0.006 * (ISRIC_NOT$Sand_15 / 100 * ISRIC_NOT$percentSOM_15) - 0.027 * (ISRIC_NOT$Clay_15 / 100 * ISRIC_NOT$percentSOM_15) +
#     0.452 * (ISRIC_NOT$Sand_15 / 100 * ISRIC_NOT$Clay_15 / 100) +
#     0.299
#   ISRIC_NOT$FC_15 <- (ISRIC_NOT$FC2 + (1.283 * ISRIC_NOT$FC2^2 -
#                                          0.374 * ISRIC_NOT$FC2 -
#                                          0.015)) * 100
#
#
#   ISRIC_NOT$FC3 <- -0.251 * ISRIC_NOT$Sand_30 / 100 +
#     0.195 * ISRIC_NOT$Clay_30 / 100 +
#     0.011 * ISRIC_NOT$percentSOM_30 +
#     0.006 * (ISRIC_NOT$Sand_30 / 100 * ISRIC_NOT$percentSOM_30) - 0.027 * (ISRIC_NOT$Clay_30 / 100 * ISRIC_NOT$percentSOM_30) +
#     0.452 * (ISRIC_NOT$Sand_30 / 100 * ISRIC_NOT$Clay_30 / 100) +
#     0.299
#   ISRIC_NOT$FC_30 <- (ISRIC_NOT$FC3 + (1.283 * ISRIC_NOT$FC3^2 -
#                                          0.374 * ISRIC_NOT$FC3 -
#                                          0.015)) * 100
#
#   ISRIC_NOT$FCA <- -0.251 * ISRIC_NOT$Sand_averaged / 100 +
#     0.195 * ISRIC_NOT$Clay_averaged / 100 +
#     0.011 * ISRIC_NOT$percentSOM_averaged +
#     0.006 * (ISRIC_NOT$Sand_averaged / 100 * ISRIC_NOT$percentSOM_averaged) - 0.027 * (ISRIC_NOT$Clay_averaged / 100 * ISRIC_NOT$percentSOM_averaged) +
#     0.452 * (ISRIC_NOT$Sand_averaged / 100 * ISRIC_NOT$Clay_averaged / 100) +
#     0.299
#   ISRIC_NOT$FC_wAverage <- (ISRIC_NOT$FCA + (1.283 * ISRIC_NOT$FCA^2 -
#                                                0.374 * ISRIC_NOT$FCA -
#                                                0.015)) * 100
#
#   ISRIC_NOT <- subset(ISRIC_NOT, select = -c(FC1, FC2, FC3, FCA))
#
#
#   ##### soil water at ######
#   ISRIC_NOT$swsa5 <- 0.278 * (ISRIC_NOT$Sand_5 / 100) +
#     0.034 * (ISRIC_NOT$Clay_5 / 100) +
#     0.022 * ISRIC_NOT$percentSOM_5 -
#     0.018 * (ISRIC_NOT$Sand_5 / 100 * ISRIC_NOT$percentSOM_5) -
#     0.027 * (ISRIC_NOT$Clay_5 / 100 * ISRIC_NOT$percentSOM_5) -
#     0.584 * (ISRIC_NOT$Sand_5 / 100 * ISRIC_NOT$Clay_5 / 100) + 0.078
#   ISRIC_NOT$swsb5 <- (ISRIC_NOT$swsa5 + (0.636 * ISRIC_NOT$swsa5 - 0.107)) * 100
#   ISRIC_NOT$sws_5 <- (ISRIC_NOT$FC_5 / 100 + ISRIC_NOT$swsb5 / 100 - (0.097 * ISRIC_NOT$Sand_5 / 100) + 0.043) * 100
#
#
#   ISRIC_NOT$swsa15 <- 0.278 * (ISRIC_NOT$Sand_15 / 100) +
#     0.034 * (ISRIC_NOT$Clay_15 / 100) +
#     0.022 * ISRIC_NOT$percentSOM_15 -
#     0.018 * (ISRIC_NOT$Sand_15 / 100 * ISRIC_NOT$percentSOM_15) -
#     0.027 * (ISRIC_NOT$Clay_15 / 100 * ISRIC_NOT$percentSOM_15) -
#     0.584 * (ISRIC_NOT$Sand_15 / 100 * ISRIC_NOT$Clay_15 / 100) + 0.078
#   ISRIC_NOT$swsb15 <- (ISRIC_NOT$swsa15 + (0.636 * ISRIC_NOT$swsa15 - 0.107)) * 100
#   ISRIC_NOT$sws_15 <- (ISRIC_NOT$FC_15 / 100 + ISRIC_NOT$swsb15 / 100 - (0.097 * ISRIC_NOT$Sand_15 / 100) + 0.043) * 100
#
#
#   ISRIC_NOT$swsa30 <- 0.278 * (ISRIC_NOT$Sand_30 / 100) +
#     0.034 * (ISRIC_NOT$Clay_30 / 100) +
#     0.022 * ISRIC_NOT$percentSOM_30 -
#     0.018 * (ISRIC_NOT$Sand_30 / 100 * ISRIC_NOT$percentSOM_30) -
#     0.027 * (ISRIC_NOT$Clay_30 / 100 * ISRIC_NOT$percentSOM_30) -
#     0.584 * (ISRIC_NOT$Sand_30 / 100 * ISRIC_NOT$Clay_30 / 100) + 0.078
#   ISRIC_NOT$swsb30 <- (ISRIC_NOT$swsa30 + (0.636 * ISRIC_NOT$swsa30 - 0.107)) * 100
#   ISRIC_NOT$sws_30 <- (ISRIC_NOT$FC_30 / 100 + ISRIC_NOT$swsb30 / 100 - (0.097 * ISRIC_NOT$Sand_30 / 100) + 0.043) * 100
#
#
#   ISRIC_NOT$swsaA <- 0.278 * (ISRIC_NOT$Sand_averaged / 100) +
#     0.034 * (ISRIC_NOT$Clay_averaged / 100) +
#     0.022 * ISRIC_NOT$percentSOM_averaged -
#     0.018 * (ISRIC_NOT$Sand_averaged / 100 * ISRIC_NOT$percentSOM_averaged) -
#     0.027 * (ISRIC_NOT$Clay_averaged / 100 * ISRIC_NOT$percentSOM_averaged) -
#     0.584 * (ISRIC_NOT$Sand_averaged / 100 * ISRIC_NOT$Clay_averaged / 100) + 0.078
#   ISRIC_NOT$swsbA <- (ISRIC_NOT$swsaA + (0.636 * ISRIC_NOT$swsaA - 0.107)) * 100
#   ISRIC_NOT$sws_wAverage <- (ISRIC_NOT$FC_wAverage / 100 + ISRIC_NOT$swsbA / 100 - (0.097 * ISRIC_NOT$Sand_averaged / 100) + 0.043) * 100
#
#   ISRIC_NOT <- subset(ISRIC_NOT, select = -c(swsa5, swsb5, swsa15, swsb15, swsa30, swsb30, swsaA, swsbA))
#   if (country == "NG") {
#     ISRIC_NOT$country <- "Nigeria"
#   } else if (country == "GH") {
#     ISRIC_NOT$country <- "Ghana"
#   } else if (country == "RW") {
#     ISRIC_NOT$country <- "Rwanda"
#   }else if (country == "TZ") {
#     ISRIC_NOT$country <- "Tanzania"
#   }
#
#   #setwd("D:/ACAI_Wrapper/AOI_shapefiles")
#   return(ISRIC_NOT)
# }


#' develope the model based on the realtionship between soil NPK, ISRIC soil data and the root yield for the control treatment for the control
#' ISRIC_SoilData is the new location for which we seek soil NPK estimates
#' FCY is Farmer-reported current yield, in tonnes FM per ha (optional, default value = 11)
#' testN_RF the training set for soil N RF model
#' testP_RF the training set for soil P RF model
#' testK_RF the training set for soil K RF model
#' #' It also requires the RF_N1.RData, RF_P1.RData, RF_K1.RData
#' getsoilNPK_RFmodel <- function(ISRIC_SoilData = ISRIC_SoilData, country = country, lat = lat, long = long, Ndata_Train = Ndata_Train, Pdata_Train = Pdata_Train, Kdata_Train = Kdata_Train) {
#'
#'   ISRIC_SoilData$ncluster <- as.factor(ISRIC_SoilData$ncluster)
#'
#'
#'   RF_N_B <- readRDS("RandforestN.RDS")
#'   RF_P_B <- readRDS("RandforestP.RDS")
#'   RF_K_B <- readRDS("RandforestK.RDS")
#'
#'   ## soil N
#'   dcolnames <- c("soilN", "exchK", "olsenP", "Clay_5", "Clay_15", "Clay_30", "percentSOM_5", "percentSOM_15", "percentSOM_30", "pH_5",
#'                  "pH_15", "pH_30", "silt_5", "silt_15", "silt_30", "BD_5", "BD_15", "BD_30", "CEC_5", "CEC_15", "CEC_30", "percentSOC_5",
#'                  "percentSOC_15", "percentSOC_30", "FC_5", "FC_15", "FC_30", "wp_5", "wp_15", "wp_30", "sws_5", "sws_15", "sws_30",
#'                  "TotalN", "Mn", "B", "Ca", "Fe", "Cu", "Al", "Mg", "Na", "ncluster", "country", "CON")
#'
#'   ISRIC_SoilData1 <- ISRIC_SoilData[, dcolnames]
#'
#'   for (f in 1:length(names(ISRIC_SoilData1))) {
#'     levels(ISRIC_SoilData1[, f]) <- levels(Ndata_Train[, f])
#'   }
#'   #
#'   # ISRIC_SoilData1 <- rbind(testN_RF[1,], ISRIC_SoilData1)
#'   # ISRIC_SoilData1 <- ISRIC_SoilData1[-1,]
#'   # ISRIC_SoilData1$ncluster <- as.factor(ISRIC_SoilData1$ncluster)
#'   ISRIC_SoilData$soilN <- exp(predict(RF_N_B, ISRIC_SoilData1))
#'
#'   ## soil P
#'   Pcolnames <- c("soilP", "exchK", "olsenP", "Clay_5", "Clay_15", "Clay_30", "percentSOM_5", "percentSOM_15", "percentSOM_30", "pH_5",
#'                  "pH_15", "pH_30", "silt_5", "silt_15", "silt_30", "BD_5", "BD_15", "BD_30", "CEC_5", "CEC_15", "CEC_30", "percentSOC_5",
#'                  "percentSOC_15", "percentSOC_30", "FC_5", "FC_15", "FC_30", "wp_5", "wp_15", "wp_30", "sws_5", "sws_15", "sws_30",
#'                  "TotalN", "Mn", "B", "Ca", "Fe", "Cu", "Al", "Mg", "Na", "ncluster", "country", "CON")
#'
#'   ISRIC_SoilData2 <- ISRIC_SoilData[, Pcolnames]
#'   for (f in 1:length(names(ISRIC_SoilData2))) {
#'     levels(ISRIC_SoilData2[, f]) <- levels(Pdata_Train[, f])
#'   }
#'   #
#'   # ISRIC_SoilData2 <- rbind(testP_RF[1,], ISRIC_SoilData2)
#'   # ISRIC_SoilData2 <- ISRIC_SoilData2[-1,]
#'   ISRIC_SoilData$soilP <- exp(predict(RF_P_B, ISRIC_SoilData2))
#'
#'   ## soil K
#'   Kcolnames <- c("soilK", "exchK", "olsenP", "Clay_5", "Clay_15", "Clay_30", "percentSOM_5", "percentSOM_15", "percentSOM_30", "pH_5",
#'                  "pH_15", "pH_30", "silt_5", "silt_15", "silt_30", "BD_5", "BD_15", "BD_30", "CEC_5", "CEC_15", "CEC_30", "percentSOC_5",
#'                  "percentSOC_15", "percentSOC_30", "FC_5", "FC_15", "FC_30", "wp_5", "wp_15", "wp_30", "sws_5", "sws_15", "sws_30",
#'                  "TotalN", "Mn", "B", "Ca", "Fe", "Cu", "Al", "Mg", "Na", "ncluster", "country", "CON")
#'
#'   ISRIC_SoilData3 <- ISRIC_SoilData[, Kcolnames]
#'
#'   for (f in 1:length(names(ISRIC_SoilData3))) {
#'     levels(ISRIC_SoilData3[, f]) <- levels(Kdata_Train[, f])
#'   }
#'   # ISRIC_SoilData3 <- rbind(testK_RF[1,], ISRIC_SoilData3)
#'   # ISRIC_SoilData3 <- ISRIC_SoilData3[-1,]
#'   ISRIC_SoilData$soilK <- exp(predict(RF_K_B, ISRIC_SoilData3))
#'
#'   ISRIC_SoilData$rec_N <- 0.5
#'   ISRIC_SoilData$rec_P <- 0.15
#'   ISRIC_SoilData$rec_K <- 0.5
#'   ISRIC_SoilData$rel_N <- 1
#'   ISRIC_SoilData$rel_P <- ISRIC_SoilData$soilP / ISRIC_SoilData$soilN
#'   ISRIC_SoilData$rel_K <- ISRIC_SoilData$soilK / ISRIC_SoilData$soilN
#'   ISRIC_SoilData$lat <- lat
#'   ISRIC_SoilData$long <- long
#'   ISRIC_SoilData$location <- paste(ISRIC_SoilData$lat, ISRIC_SoilData$long, sep = "_")
#'   ISRIC_SoilData$Zone <- country
#'   ISRIC_SoilData <- ISRIC_SoilData[, c("location", "lat", "long", "soilN", "soilP", "soilK", "Zone", "rec_N", "rec_P", "rec_K", "rel_N", "rel_P", "rel_K")]
#'
#'
#'   return(ISRIC_SoilData)
#'
#' }
#'


#' #' RF model works only if the factor levels are exactly identical to the data used to develp the model,
#' Rfmodel_Wrapper_obsolete <- function(ISRIC_SoilData = ISRIC_SoilData, FCY = FCY, country = country) {
#'
#'   ### To avoid having a new factor level while using RF for prediction, add the soil data from NG here and remove it
#'   GIS_soilINS_modData2 <- read.csv("NOT_GIS_CON.csv") ## NOT data used to develope the RF model
#'   #setwd("D:/ACAI_Wrapper/cloud_compute")
#'   # GIS_soilINS_modData2 <- read.csv("NOT_GIS_CON.csv")
#'   GIS_soilINS_modData2$Clay_5 <- as.numeric(GIS_soilINS_modData2$Clay_5)
#'   GIS_soilINS_modData2$Clay_15 <- as.numeric(GIS_soilINS_modData2$Clay_15)
#'   GIS_soilINS_modData2$Clay_30 <- as.numeric(GIS_soilINS_modData2$Clay_30)
#'   GIS_soilINS_modData2$silt_5 <- as.numeric(GIS_soilINS_modData2$silt_5)
#'   GIS_soilINS_modData2$silt_15 <- as.numeric(GIS_soilINS_modData2$silt_15)
#'   GIS_soilINS_modData2$silt_30 <- as.numeric(GIS_soilINS_modData2$silt_30)
#'   GIS_soilINS_modData2$BD_5 <- as.numeric(GIS_soilINS_modData2$BD_5)
#'   GIS_soilINS_modData2$BD_15 <- as.numeric(GIS_soilINS_modData2$BD_15)
#'   GIS_soilINS_modData2$BD_30 <- as.numeric(GIS_soilINS_modData2$BD_30)
#'   GIS_soilINS_modData2$CEC_5 <- as.numeric(GIS_soilINS_modData2$CEC_5)
#'   GIS_soilINS_modData2$CEC_15 <- as.numeric(GIS_soilINS_modData2$CEC_15)
#'   GIS_soilINS_modData2$CEC_30 <- as.numeric(GIS_soilINS_modData2$CEC_30)
#'   GIS_soilINS_modData2$TotalN <- as.numeric(GIS_soilINS_modData2$TotalN)
#'   GIS_soilINS_modData2$Mn <- as.numeric(GIS_soilINS_modData2$Mn)
#'   GIS_soilINS_modData2$B <- as.numeric(GIS_soilINS_modData2$B)
#'   GIS_soilINS_modData2$Ca <- as.numeric(GIS_soilINS_modData2$Ca)
#'   GIS_soilINS_modData2$Fe <- as.numeric(GIS_soilINS_modData2$Fe)
#'   GIS_soilINS_modData2$Cu <- as.numeric(GIS_soilINS_modData2$Cu)
#'   GIS_soilINS_modData2$Al <- as.numeric(GIS_soilINS_modData2$Al)
#'   GIS_soilINS_modData2$Mg <- as.numeric(GIS_soilINS_modData2$Mg)
#'   GIS_soilINS_modData2$Na <- as.numeric(GIS_soilINS_modData2$Na)
#'   GIS_soilINS_modData2$ncluster <- as.factor(GIS_soilINS_modData2$ncluster)
#'   GIS_soilINS_modData2$CON <- as.numeric(GIS_soilINS_modData2$CON)
#'   GIS_soilINS_modData2 <- subset(GIS_soilINS_modData2, select = -c(Zn))
#'
#'
#'   NG_SoilData <- read.csv('NG_SoilData.csv')
#'   #NG_SoilData <- read.csv('NG_SoilData.csv')
#'   NG_SoilData$Clay_5 <- as.numeric(NG_SoilData$Clay_5)
#'   NG_SoilData$Clay_15 <- as.numeric(NG_SoilData$Clay_15)
#'   NG_SoilData$Clay_30 <- as.numeric(NG_SoilData$Clay_30)
#'   NG_SoilData$silt_5 <- as.numeric(NG_SoilData$silt_5)
#'   NG_SoilData$silt_15 <- as.numeric(NG_SoilData$silt_15)
#'   NG_SoilData$silt_30 <- as.numeric(NG_SoilData$silt_30)
#'   NG_SoilData$BD_5 <- as.numeric(NG_SoilData$BD_5)
#'   NG_SoilData$BD_15 <- as.numeric(NG_SoilData$BD_15)
#'   NG_SoilData$BD_30 <- as.numeric(NG_SoilData$BD_30)
#'   NG_SoilData$CEC_5 <- as.numeric(NG_SoilData$CEC_5)
#'   NG_SoilData$CEC_15 <- as.numeric(NG_SoilData$CEC_15)
#'   NG_SoilData$CEC_30 <- as.numeric(NG_SoilData$CEC_30)
#'   NG_SoilData$TotalN <- as.numeric(NG_SoilData$TotalN)
#'   NG_SoilData$Mn <- as.numeric(NG_SoilData$Mn)
#'   NG_SoilData$B <- as.numeric(NG_SoilData$B)
#'   NG_SoilData$Ca <- as.numeric(NG_SoilData$Ca)
#'   NG_SoilData$Fe <- as.numeric(NG_SoilData$Fe)
#'   NG_SoilData$Cu <- as.numeric(NG_SoilData$Cu)
#'   NG_SoilData$Al <- as.numeric(NG_SoilData$Al)
#'   NG_SoilData$Mg <- as.numeric(NG_SoilData$Mg)
#'   NG_SoilData$Na <- as.numeric(NG_SoilData$Na)
#'   NG_SoilData$ncluster <- as.factor(NG_SoilData$ncluster)
#'   NG_SoilData$country <- "Nigeria"
#'   NG_SoilData$CON <- as.numeric(5)
#'
#'   NG_SoilData$soilN <- 0
#'   NG_SoilData$soilP <- 0
#'   NG_SoilData$soilK <- 0
#'
#'   NG_SoilData <- NG_SoilData[, c("soilN", "soilP", "soilK", "exchK", "olsenP", "Clay_5", "Clay_15", "Clay_30", "percentSOM_5", "percentSOM_15", "percentSOM_30",
#'                                  "pH_5", "pH_15", "pH_30", "silt_5", "silt_15", "silt_30", "BD_5", "BD_15", "BD_30", "CEC_5", "CEC_15", "CEC_30", "percentSOC_5",
#'                                  "percentSOC_15", "percentSOC_30", "FC_5", "FC_15", "FC_30", "wp_5", "wp_15", "wp_30", "sws_5", "sws_15", "sws_30",
#'                                  "TotalN", "Mn", "B", "Ca", "Fe", "Cu", "Al", "Mg", "Na", "ncluster", "country", "CON")]
#'
#'
#'   NG_SoilData$use <- "Valid"
#'   GIS_soilINS_modData2$use <- "train"
#'   factoring <- rbind(GIS_soilINS_modData2, NG_SoilData)
#'
#'   for (f in 1:length(names(factoring))) {
#'     levels(GIS_soilINS_modData2[, f]) <- levels(factoring[, f])
#'   }
#'
#'
#'   GIS_soilINS_modData3 <- factoring[factoring$use == "train",]
#'   GIS_soilINS_modData3 <- subset(GIS_soilINS_modData3, select = -c(use))
#'
#'   NG_SoilData <- factoring[factoring$use == "Valid",]
#'   NG_SoilData <- subset(NG_SoilData, select = -c(use))
#'
#'
#'   ### Data partioning
#'   set.seed(444)
#'   GIS_soilINS_modData3$ncluster <- as.factor(GIS_soilINS_modData3$ncluster)
#'   ind <- sample(2, nrow(GIS_soilINS_modData3), replace = TRUE, prob = c(0.7, 0.3)) ## where conrtol yield is used as a covariate
#'   trainData <- GIS_soilINS_modData3[ind == 1,]
#'   testData <- GIS_soilINS_modData3[ind == 2,]
#'
#'   Ndata_Train <- subset(trainData, select = -c(soilP, soilK))
#'   Pdata_Train <- subset(trainData, select = -c(soilN, soilK))
#'   Kdata_Train <- subset(trainData, select = -c(soilN, soilP))
#'
#'   lat <- ISRIC_SoilData$lat
#'   long <- ISRIC_SoilData$long
#'   ISRIC_SoilData$CON <- as.numeric(FCY)
#'
#'
#'   ISRIC_SoilData$soilN <- 0
#'   ISRIC_SoilData$soilP <- 0
#'   ISRIC_SoilData$soilK <- 0
#'
#'   ISRIC_SoilData <- ISRIC_SoilData[, c("soilN", "soilP", "soilK", "exchK", "olsenP", "Clay_5", "Clay_15", "Clay_30", "percentSOM_5", "percentSOM_15", "percentSOM_30",
#'                                        "pH_5", "pH_15", "pH_30", "silt_5", "silt_15", "silt_30", "BD_5", "BD_15", "BD_30", "CEC_5", "CEC_15", "CEC_30", "percentSOC_5",
#'                                        "percentSOC_15", "percentSOC_30", "FC_5", "FC_15", "FC_30", "wp_5", "wp_15", "wp_30", "sws_5", "sws_15", "sws_30",
#'                                        "TotalN", "Mn", "B", "Ca", "Fe", "Cu", "Al", "Mg", "Na", "ncluster", "country", "CON")]
#'
#'   for (f in 1:length(names(ISRIC_SoilData))) {
#'     levels(ISRIC_SoilData[, f]) <- levels(trainData[, f])
#'   }
#'   ISRIC_SoilData$country <- as.factor(ISRIC_SoilData$country)
#'
#'   SoilData_fcy_A <- getsoilNPK_RFmodel(ISRIC_SoilData = ISRIC_SoilData, country = country, lat = lat, long = long, Ndata_Train, Pdata_Train, Kdata_Train)
#'
#'
#'   return(SoilData_fcy_A)
#' }
#'
#'
#'
# getFRrecommendations_ODK <- function(areaHa = 1,
#                                      lat,
#                                      lon,
#                                      PD,
#                                      FCY = 11,
#                                      rootUP,
#                                      fertilizers = fertilizers #dataframe containing price of urea, DAP and MOP in USD in local currency
# ) {
#
#   #rounding lat and lon to centroid of 5x5km pixel
#   latr <- floor(lat * 10) / 10 + ifelse(lat - (floor(lat * 10) / 10) < 0.05, 0.025, 0.075)
#   lonr <- floor(lon * 10) / 10 + ifelse(lat - (floor(lon * 10) / 10) < 0.05, 0.025, 0.075)
#
#   #pick up the recommended fertilizer application rates
#   flp <- paste0("E:/03-projects/ACAI/ODK briefcase storage/created forms/DSTs/media_FR/inputDST_FR_perpixel/E", lonr, "N", latr, ".csv")
#
#   if (!file.exists(flp)) {
#
#     #trans
#
#     if (country == "NG") {
#       print(norecom_ng)
#     }else if (country == "TZ"){
#       print(norecom_tz)
#     }else{
#       print(norecom_rw)
#     }
#
#
#     ds <- NULL
#
#   }else {
#
#     res <- read.csv(flp)
#     FCY <- min(5, ceiling(FCY / 7.5))
#     plw <- as.numeric(format(PD, format = "%W"))
#     res <- res[res$PW == plw & res$FCY == FCY,]
#
#     fertilizer_rates = data.frame(type = gsub("rate", "", names(res)[grepl("rate", names(res))]),
#                                   rate = round(as.numeric(res[, grepl("rate", names(res))]),digits=0))
#
#
#
#
#     res <- res[, !grepl("rate", names(res))]
#
#     res <- subset(res, select = -c(PW, FCY))
#     res$TC <- fertilizers$price %*% fertilizer_rates$rate * areaHa
#     res$GR <- (res$TY - res$CY) * rootUP * areaHa
#
#     if (res$GR < 2 * res$TC) {
#       res$TY <- res$CY
#       res$TC <- 0
#       res$GR <- 0
#       fertilizer_rates$rate <- 0
#     }
#
#     ds <- list(rec = res, fertilizer_rates = fertilizer_rates)
#
#   }
#
#   return(ds)
#
# }


