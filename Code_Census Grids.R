################################################ Census Grids  ---------------------------------
gc()

library(sf)
library(raster)
library(fasterize)





##### Masks and auxiliars
a_lim = st_read("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/SIG", "LimGrade_WGS23s")
a_proj = st_crs(a_lim)
a_proj_txt = as.character(a_proj[2])
a_ext_txt = st_bbox(a_lim)
a_ext = extent (a_ext_txt[1], a_ext_txt[3], a_ext_txt[2], a_ext_txt[4])

## grade
a_raster_10 = raster(a_lim, nrows=10200, ncols=15600, crs=a_proj_txt)
values(a_raster_10) = 0
writeRaster (a_raster_10, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/SIG/LimGrade_WGS23s_Gr10.tif", overwrite=F, datatype='INT2U')

## rmsp
a_rmsp = st_read("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/SIG", "LimRMSP_WGS23s")
masc_rmsp = fasterize(a_rmsp, a_raster_10, field="ID")
writeRaster(masc_rmsp, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/SIG/LimRMSP_WGS23s_Gr10.tif", overwrite=F, datatype='INT2U')

## agua
class1992_30m = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Class/Class_1992.tif")
class1992 = resample(class1992_30m, a_raster_10, method="ngb")
writeRaster(class1992, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Class/Class_1992_Gr10.tif")
class2010_30m = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Class/Class_2010.tif")
class2010 = resample(class2010_30m, a_raster_10, method="ngb")
writeRaster(class2010, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Class/Class_2010_Gr10.tif")

M_agua = c(0, 1, 0, 1, 2, 0, 2, 3, 0, 3, 4, 10)
rcl_agua = matrix(M_agua, ncol=3, byrow=TRUE)
class1992_r_agua = reclassify(class1992, rcl_agua)
class2010_r_agua = reclassify(class2010, rcl_agua)
masc_agua = class1992_r_agua + class2010_r_agua
masc_agua = crop(masc_agua, a_ext)
#freq(masc_agua)
masc_agua[is.na(masc_agua[])] = 0
#freq(masc_agua)
masc_agua = masc_agua * masc_rmsp
plot(masc_agua)
#freq(masc_agua)
masc_agua[masc_agua>=20] = 10
plot(masc_agua)
#freq(masc_agua)
writeRaster(masc_agua, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/Masc_Agua_Gr10.tif", overwrite=F, datatype='INT2U')

## logradouros
foc=11
logrAll = raster ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/Logr/LogrAll_Gr10.tif")
logrAll[is.na(logrAll[])] = 0
logrExcl = raster ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/Logr/LogrExcl_Gr10.tif")
logrExcl[is.na(logrExcl[])] = 0
logr = logrAll - logrExcl
logr[logr==-1] = 1
plot(logr)
logr_rmsp = logr*masc_rmsp
plot(logr_rmsp)
logr_foc11 = focal(logr_rmsp, w=matrix(1,nrow=foc,ncol=foc), pad=T, fun=max, na.rm=T)
plot(logr_foc11)
writeRaster(logr_foc11, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/Masc_LogrFoc11_Gr10.tif", overwrite=F)





##### Raster Urb Preparation
rm(list=ls())
gc()

library(sf)
library(raster)
library(fasterize)
a_raster_10 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/SIG/LimGrade_WGS23s_Gr10.tif")


## 1991 ou 2010
class = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Class/Class_2010_Gr10.tif")
set = st_read ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade", "Censo2010_WGS23s_LimGrade")

class = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Class/Class_1992_Gr10.tif")
set = st_read ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade", "Censo1991_WGS23s_LimGrade")


## Contagem de Pixels por Setor
set_no = fasterize(set, a_raster_10, "No")
set_no_df = as.data.frame(freq(set_no))

## Class com Masc Agua
masc_agua = raster ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/Masc_Agua_Gr10.tif")
class_corragua = class + masc_agua
class_corragua[class_corragua>10] = 4
#plot(class_corragua)
# 1 = Vegetação => não urbano
# 2 = Impermeabilização  => urbano
# 3 = Solo Exposto => não urbano
# 4 = Água => não urbano
M = c(0, 1, 0, 1, 2, 1, 2, 3, 0, 3, 4, 0)
rcl = matrix(M, ncol=3, byrow=TRUE)
# 0 = não urbano
# 1 = urbano
urb_corragua = reclassify(class_corragua, rcl)
#freq(urb_corragua)
#plot(urb_corragua)

## corr 0 - count urb no setor
set_u_corr0 = set_no*urb_corragua
set_u_corr0_df = as.data.frame(freq(set_u_corr0))
names(set_u_corr0_df) [2] = c("Px10_U_0")
set_u_corr0_df$Px10_U_0[which(set_u_corr0_df$value==0)] = NA
set_u_corr0_df = na.omit(set_u_corr0_df)

set_corr0 = merge(set_no_df, set_u_corr0_df, by="value", all=T)
names(set_corr0) [2] = c("Px10_0")
set_corr0$Px10_U_0[is.na(set_corr0$Px10_U_0)] = 0

set_all_corr0 = merge(set, set_corr0, by.x="No", by.y="value", all=T)
set_all_corr0$Px10_U_0[which(set_all_corr0$No==0)] = NA

## corr1 - setores sem urb, com urb a partir de logradouros
logr_foc11 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/Masc_LogrFoc11_Gr10.tif")

set_all_corr0$corr1 = 0
set_all_corr0$corr1[which(set_all_corr0$Px10_U_0==0)] = 1
set_all_corr0$corr1[is.na(set_all_corr0$Hab)] = 0
set_all_corr0$corr1[which(set_all_corr0$Hab==0)] = 0
set_all_corr0$corr1[which(set_all_corr0$No==0)] = NA

set_all_corr1_masc = fasterize(set_all_corr0, a_raster_10, "corr1")
plot(set_all_corr1_masc)

set_all_corr1_masc_logr = set_all_corr1_masc + logr_foc11
set_all_corr1_masc_logr = reclassify(set_all_corr1_masc_logr, c(-1,1,0, 1,2,1))
plot(set_all_corr1_masc_logr)

set_u_corr1 = set_no * set_all_corr1_masc_logr
set_u_corr1_df = as.data.frame(freq(set_u_corr1))
names(set_u_corr1_df) [2] = c("Px10_U_1")
set_u_corr1_df$Px10_U_1[which(set_u_corr1_df$value==0)] = NA
set_u_corr1_df = na.omit(set_u_corr1_df)

set_all_corr1 = merge(set_all_corr0, set_u_corr1_df, by.x="No", by.y="value", all=T)
set_all_corr1$Px10_U_1[is.na(set_all_corr1$Px10_U_1)] = 0
set_all_corr1$Px10_T_1 = set_all_corr1$Px10_U_0 + set_all_corr1$Px10_U_1

## corr2 - setores sem urb, com urb a partir filtro pop > 15
set_all_corr1$corr2 = 0
set_all_corr1$corr2 [which(set_all_corr1$Px10_T_1==0 & set_all_corr1$Hab>15)] = 1
set_all_corr1$corr2[which(set_all_corr1$No==0)] = NA

set_all_corr2_masc_set = fasterize(set_all_corr1, a_raster_10, "corr2")

## urbano corr
urb_10m_corr = urb_corragua + set_all_corr1_masc_logr + set_all_corr2_masc_set
plot(urb_10m_corr)

## write
writeRaster(urb_10m_corr, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/Urb2010_Corr_Gr10.tif", overwrite=F, datatype='INT2U')
writeRaster(urb_10m_corr, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/Urb1992_Corr_Gr10.tif", overwrite=F, datatype='INT2U')





##### Census Tracts 2010
rm(list=ls())
gc()

library(sf)
library(raster)
library(fasterize)
a_raster_10 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/SIG/LimGrade_WGS23s_Gr10.tif")

set = st_read ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade", "Censo2010_WGS23s_LimGrade")
urb_10m_corr = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/Urb2010_Corr_Gr10.tif")

set

set$RnTot = set$RendMRe * set$Dom
#set$RnMed_SM = set$RendMRe / 510
set$RnTot_18 = set$RnTot * 1.6134801
set$RnMed_18 = set$RendMRe * 1.6134801

set

renda_sem0eNA = set$RendMRe
renda_sem0eNA[renda_sem0eNA==0] = NA
renda_sem0eNA = na.omit(renda_sem0eNA)
renda_mean = mean(renda_sem0eNA)
renda_sd = sd(renda_sem0eNA)
set$RnMedNor = (set$RendMRe-renda_mean)/renda_sd

set

set_no = fasterize(set, a_raster_10, "No")
length(unique(set_no)) == length(st_dimension(set))
set_no_df = as.data.frame(freq(set_no))

set_u_corr = set_no*urb_10m_corr
set_u_corr_df = as.data.frame(freq(set_u_corr))
names(set_u_corr_df) [2] = c("Px10_U")
set_u_corr_df$Px10_U[which(set_u_corr_df$value==0)] = NA
set_u_corr_df = na.omit(set_u_corr_df)

set_corr = merge(set_no_df, set_u_corr_df, by="value", all=T)
names(set_corr) [2] = c("Px10")
set_corr$Px10_U[which(set_corr$value==0)] = NA
set_corr = na.omit(set_corr)

set_all = merge(set, set_corr, by.x="No", by.y="value", all=T)

set_all$Ha_Pix = (set_all$Px10*(10*10))/10000
set_all$UHa_Pix = (set_all$Px10_U*(10*10))/10000
set_all$HabUHa = set_all$Hab/set_all$UHa_Pix
set_all$DomUHa = set_all$Dom/set_all$UHa_Pix

st_write(set_all, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/Censo2010_WGS23s_LimGrade_Gr10m_Urb_corr.shp", delete_dsn=F)





##### Census Tracts 1991 
rm(list=ls())
gc()

library(sf)
library(raster)
library(fasterize)
a_raster_10 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/SIG/LimGrade_WGS23s_Gr10.tif")

set = st_read ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade", "Censo1991_WGS23s_LimGrade")
urb_10m_corr = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/Urb1992_Corr_Gr10.tif")

set

#set$RnMed_SM = set$RendMCh / 36161.6
set$RnTot_18 = (set$RendTot / 2750000) * 52841.59
set$RnMed_18 = (set$RendMCh / 2750000) * 52841.59

set

renda_sem0eNA = set$RendMCh
renda_sem0eNA[renda_sem0eNA==0] = NA
renda_sem0eNA = na.omit(renda_sem0eNA)
renda_mean = mean(renda_sem0eNA)
renda_sd = sd(renda_sem0eNA)
set$RnMedNor = (set$RendMCh-renda_mean)/renda_sd

set

set_no = fasterize(set, a_raster_10, "No")
length(unique(set_no)) == length(st_dimension(set))
set_no_df = as.data.frame(freq(set_no))

set_u_corr = set_no*urb_10m_corr
set_u_corr_df = as.data.frame(freq(set_u_corr))
names(set_u_corr_df) [2] = c("Px10_U")
set_u_corr_df$Px10_U[which(set_u_corr_df$value==0)] = NA
set_u_corr_df = na.omit(set_u_corr_df)

set_corr = merge(set_no_df, set_u_corr_df, by="value", all=T)
names(set_corr) [2] = c("Px10")
set_corr$Px10_U[which(set_corr$value==0)] = NA
set_corr = na.omit(set_corr)

set_all = merge(set, set_corr, by.x="No", by.y="value", all=T)

set_all$Ha_Pix = (set_all$Px10*(10*10))/10000
set_all$UHa_Pix = (set_all$Px10_U*(10*10))/10000
set_all$HabUHa = set_all$Hab/set_all$UHa_Pix
set_all$DomUHa = set_all$Dom/set_all$UHa_Pix

st_write(set_all, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/Censo1991_WGS23s_LimGrade_Gr10m_Urb_corr.shp", delete_dsn=F)





##### Dasymetric Households Estimation
rm(list=ls())
gc()

library(sf)
library(raster)
library(fasterize)
a_raster_10 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/SIG/LimGrade_WGS23s_Gr10.tif")

## 2010
urb_10m_corr = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/Urb2010_Corr_Gr10.tif")
set_all = st_read ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade", "Censo2010_WGS23s_LimGrade_Gr10m_Urb_corr")
dom_set = as.data.frame(set_all[,"Dom"])
dom_set_sum = sum (dom_set[,1], na.rm=T)
r_DomUHa = fasterize(set_all, a_raster_10, field="DomUHa")
r_DomDas_10 = (r_DomUHa * urb_10m_corr) * ((10*10)/10000)
cellStats(r_DomDas_10, stat=sum)
dom_set_sum
writeRaster(r_DomDas_10, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/DomDas_Gr10_2010.tif", overwrite=F, datatype='FLT4S')

## 1991
rm(list=ls())
a_raster_10 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/SIG/LimGrade_WGS23s_Gr10.tif")

set_all = st_read ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade", "Censo1991_WGS23s_LimGrade_Gr10m_Urb_corr")
urb_10m_corr = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/Urb1992_Corr_Gr10.tif")
dom_set = as.data.frame(set_all[,"Dom"])
dom_set_sum = sum (dom_set[,1], na.rm=T)
r_DomUHa = fasterize(set_all, a_raster_10, field="DomUHa")
r_DomDas_10 = (r_DomUHa * urb_10m_corr) * ((10*10)/10000)
cellStats(r_DomDas_10, stat=sum)
dom_set_sum
writeRaster(r_DomDas_10, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/DomDas_Gr10_1991.tif", overwrite=F, datatype='FLT4S')





###### Income Estimations 
rm(list=ls())
gc()

library(sf)
library(raster)
library(fasterize)
a_raster_10 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/SIG/LimGrade_WGS23s_Gr10.tif")

## 2010
set_all = st_read ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade", "Censo2010_WGS23s_LimGrade_Gr10m_Urb_corr")
masc_DomDas = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/DomDas_Gr10_2010.tif")
masc_DomDas[masc_DomDas==0] = NA
masc_DomDas[masc_DomDas>0] = 1
plot(masc_DomDas)
r_Rend18_10 = fasterize (set_all, a_raster_10, "RnMed_18")
r_Rend18_10_u = r_Rend18_10 * masc_DomDas
plot(r_Rend18_10_u)
writeRaster(r_Rend18_10_u, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/Rend18_Gr10_2010.tif", overwrite=F, datatype='FLT4S')

## 1991
rm(list=ls())
a_raster_10 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/SIG/LimGrade_WGS23s_Gr10.tif")

set_all = st_read ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade", "Censo1991_WGS23s_LimGrade_Gr10m_Urb_corr")
masc_DomDas = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/DomDas_Gr10_1991.tif")
masc_DomDas[masc_DomDas==0] = NA
masc_DomDas[masc_DomDas>0] = 1
plot(masc_DomDas)
r_Rend18_10 = fasterize (set_all, a_raster_10, "RnMed_18")
r_Rend18_10_u = r_Rend18_10 * masc_DomDas
plot(r_Rend18_10_u)
writeRaster(r_Rend18_10_u, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/Rend18_Gr10_1991.tif", overwrite=F, datatype='FLT4S')





##### Households Change
rm(list=ls())
gc()

library(raster)

r_DomDas_2010 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/DomDas_Gr10_2010.tif")
r_DomDas_1991 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/DomDas_Gr10_1991.tif")
r_DomDas_10e91 = r_DomDas_2010 - r_DomDas_1991
writeRaster(r_DomDas_10e91, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/Dif_DomDas_Gr10_10e91.tif", overwrite=F, datatype='FLT8S')





##### Mean Income Change
rm(list=ls())
gc()

library(raster)

r_Inc_2010 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/Rend18_Gr10_2010.tif")
r_Inc_1991 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/Rend18_Gr10_1991.tif")
values(r_Inc_2010)[values(r_Inc_2010)==0] = NA
values(r_Inc_1991)[values(r_Inc_1991)==0] = NA
r_Inc_10e91 = r_Inc_2010 - r_Inc_1991
writeRaster(r_Inc_10e91, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/Dif_Rend18_Gr10_10e91.tif", overwrite=F, datatype='FLT4S')





##### Adicional Grids
rm(list=ls())
gc()

library(sf)
library(raster)
library(fasterize)

a_raster_10 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/SIG/LimGrade_WGS23s_Gr10.tif")


## prep 2010
r_DomDas_2010 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/DomDas_Gr10_2010.tif")

masc_DomDas = r_DomDas_2010
masc_DomDas[masc_DomDas==0] = NA
masc_DomDas[masc_DomDas>0] = 1

set_all = st_read ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade", "Censo2010_WGS23s_LimGrade_Gr10m_Urb_corr_Adic")


## grids 2010
r_SetSEsg = fasterize(set_all, a_raster_10, field="PSEsg")
r_SEsg_10 = r_DomDas_2010 * r_SetSEsg
writeRaster(r_SEsg_10, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/DomDas_SEsg_Gr10_2010.tif", overwrite=F, datatype='FLT4S')

r_SetSPav = fasterize(set_all, a_raster_10, field="PDmSPav")
r_SPav_10 = r_DomDas_2010 * r_SetSPav
writeRaster(r_SPav_10, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/DomDas_SPav_Gr10_2010.tif", overwrite=F, datatype='FLT4S')

r_SetSMF = fasterize(set_all, a_raster_10, field="PDmSMF")
r_SMF_10 = r_DomDas_2010 * r_SetSMF
writeRaster(r_SMF_10, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/DomDas_SMF_Gr10_2010.tif", overwrite=F, datatype='FLT4S')

r_SetSBL = fasterize(set_all, a_raster_10, field="PDmSBL")
r_SBL_10 = r_DomDas_2010 * r_SetSBL
writeRaster(r_SBL_10, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/DomDas_SBL_Gr10_2010.tif", overwrite=F, datatype='FLT4S')

r_SetEsgCA = fasterize(set_all, a_raster_10, field="PDmEsgCA")
r_EsgCA_10 = r_DomDas_2010 * r_SetEsgCA
writeRaster(r_EsgCA_10, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/DomDas_EsgCA_Gr10_2010.tif", overwrite=F, datatype='FLT4S')

r_SetPReFe = fasterize(set_all, a_raster_10, field="PReFe")
r_PReFe_10 = masc_DomDas * r_SetPReFe
writeRaster(r_PReFe_10, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/PReFe_Gr10_2010.tif", overwrite=F, datatype='FLT4S')

r_SetPReMa = fasterize(set_all, a_raster_10, field="PReMa")
r_PReMa_10 = masc_DomDas * r_SetPReMa
writeRaster(r_PReMa_10, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/PReMa_Gr10_2010.tif", overwrite=F, datatype='FLT4S')

r_SetPReAlf = fasterize(set_all, a_raster_10, field="PReAlf")
r_PReAlf_10 = masc_DomDas * r_SetPReAlf
writeRaster(r_PReAlf_10, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/PReAlf_Gr10_2010.tif", overwrite=F, datatype='FLT4S')

r_SetPReFeAlf = fasterize(set_all, a_raster_10, field="PReFeAlf")
r_PReFeAlf_10 = masc_DomDas * r_SetPReFeAlf
writeRaster(r_PReFeAlf_10, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/PReFeAlf_Gr10_2010.tif", overwrite=F, datatype='FLT4S')

r_SetPReMaAlf = fasterize(set_all, a_raster_10, field="PReMaAlf")
r_PReMaAlf_10 = masc_DomDas * r_SetPReMaAlf
writeRaster(r_PReMaAlf_10, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/PReMaAlf_Gr10_2010.tif", overwrite=F, datatype='FLT4S')

r_SetHabDm = fasterize(set_all, a_raster_10, field="HabDm")
r_HabDm_10 = masc_DomDas * r_SetHabDm
writeRaster(r_HabDm_10, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/HabDm_Gr10_2010.tif", overwrite=F, datatype='FLT4S')




##### Check grids
rm(list=ls())
gc()

library(raster)
library(sf)

set_2010 = st_read ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade", "Censo2010_WGS23s_LimGrade_Gr10m_Urb_corr")
r_DomDas_2010 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/DomDas_Gr10_2010.tif")
sumDom_2010_set = sum(set_2010$Dom, na.rm=T)
sumDom_2010_grid = cellStats(r_DomDas_2010, sum)

set_1991 = st_read ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade", "Censo1991_WGS23s_LimGrade_Gr10m_Urb_corr")
r_DomDas_1991 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/DomDas_Gr10_1991.tif")
sumDom_1991_set = sum(set_1991$Dom, na.rm=T)
sumDom_1991_grid = cellStats(r_DomDas_1991, sum)

sumDom_10e91_set = sumDom_2010_set - sumDom_1991_set
r_DomDas_10e91 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/Dif_DomDas_Gr10_10e91.tif")
sumDom_10e91_grid  = cellStats(r_DomDas_10e91, sum)
