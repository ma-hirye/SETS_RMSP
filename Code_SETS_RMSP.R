################################################ Class  ---------------------------------
rm(list=ls())
gc()

library(raster)
library (velox)
library(sf)
library (caret)


##### Image 2010
imag = brick("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Class/Mosaic_2010.tif")
crs = crs(imag)
imag_ndvi = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Class/NDVI_2010.tif")
imag_green_v = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Class/greenest_v_2010.tif")
imag_green_i = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Class/greenest_i_2010.tif")

names_images = c("imag", "imag_ndvi", "imag_green_v", "imag_green_i")
img = stack (mget(names_images))
names_bandas = c("imag_B1", "imag_B2", "imag_B3", "imag_B4", "imag_B5", "imag_B7", "NDVI",  "Green_v", "Green_i")

IMG = velox(img)


##### Samples
pts = st_read("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Class", "Class_Pixels_Sampled_QGrupo")
table(pts$Q_Tipo)
table(pts$Q_Grupo)

PTS = pts[,c("No")]
PTS$class = as.factor(pts$Q_Grupo)
PTS = na.omit(PTS)

IMG_PTS = IMG$extract_points(sp=PTS)
df_IMG = as.data.frame(IMG_PTS)
names(df_IMG) = names_bandas

df_PTS = as.data.frame(PTS)
df_PTS = df_PTS[,c("No", "class")]
SAMPLES = cbind(df_PTS[,c("No", "class")], df_IMG)
SAMPLES = na.omit(SAMPLES)
str(SAMPLES)
table(SAMPLES$class)

No_Classes = length(unique(SAMPLES$class))


##### Split training 
set.seed(24)
SAMPLESIndex = createDataPartition(SAMPLES$class, p = .50, list = FALSE)

train = SAMPLES[SAMPLESIndex,]
No_train = train[,c("No")]
train = train[,-1]
table(train$class)
str(train)

test  = SAMPLES[-SAMPLESIndex,]
No_test = test[,c("No")]
test = test[,-1]
table(test$class)
str(test)
#test

name_outcome = c("class")
predictors = names_bandas


##### Model
model = train(train[,predictors], train[,name_outcome], method='svmRadial')

model


##### Model tuning - não é necessário


##### Variable importance estimation using caret
varImp = varImp(object=model)
plot(varImp,main="Variable Importance")

# accuracy Assessment
predictions = predict.train(object=model,test[,predictors],type="raw")
cf = confusionMatrix(predictions,test[,name_outcome])
cf


##### Save
sink("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Class/Class_NumTestTrain.txt")
cat("Train")
cat("\n")
No_train
cat("\n")
cat("Test")
cat("\n")
No_test
sink ()

sink("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Class/Class_Model.txt")
model
sink ()

jpeg('C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Class/Class_VarImp.jpg', width = 1024, height = 768)
plot(varImp,main="Variable Importance")
dev.off()

sink("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Class/Class_CF.txt")
predictors
cf
sink ()


##### Class 2010
df_img = as.data.frame(img, xy = TRUE)
df_img = na.omit(df_img)
names(df_img) = c("X", "Y", names_bandas)
df_img = na.omit(df_img)

classify = predict.train(object=model, df_img[,predictors], type="raw")
class =as.numeric(classify)
df_img_class = cbind(df_img[,1:2], class)
img_class = rasterFromXYZ(df_img_class, crs=crs)

dev.off()
plot(img_class, col=rainbow(8))

writeRaster(img_class, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Class/Class_2010.tif")


##### Class 1992
imag = brick("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Class/Mosaic_1992.tif")
crs = crs(imag)
imag_ndvi = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Class/NDVI_1992.tif")
imag_green_v = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Class/greenest_v_1992.tif")
imag_green_i = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Class/greenest_i_1992.tif")

names_images = c("imag", "imag_ndvi", "imag_green_v", "imag_green_i")
img = stack (mget(names_images))
names_bandas = c("imag_B1", "imag_B2", "imag_B3", "imag_B4", "imag_B5", "imag_B7", "NDVI",  "Green_v", "Green_i")

df_img = as.data.frame(img, xy = TRUE)
df_img = na.omit(df_img)
names(df_img) = c("X", "Y", names_bandas)
df_img = na.omit(df_img)

classify = predict.train(object=model, df_img[,predictors], type="raw")
class =as.numeric(classify)
df_img_class = cbind(df_img[,1:2], class)
img_class = rasterFromXYZ(df_img_class, crs=crs)

dev.off()
plot(img_class, col=rainbow(8))

writeRaster(img_class, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Class/Class_1992.tif")





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





################################################ Estat - Focal  ---------------------------------
rm(list=ls())
gc()

library(raster)
library(sf)
library(fasterize)


#foc = 11
foc = 7

a_raster_10 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/SIG/LimGrade_WGS23s_Gr10.tif")


## Mean household income
MeanInc2010 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/Rend18_Gr10_2010.tif")
MeanInc2010_foc = focal(MeanInc2010, w=matrix(1,nrow=foc,ncol=foc), pad=T, fun=mean, na.rm=T)
writeRaster (MeanInc2010_foc, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Data_MeanInc2010_foc.tif", overwrite=F, datatype='FLT4S')


## Mean household income change
MeanIncDif = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/Dif_Rend18_Gr10_10e91.tif")
MeanIncDif_foc = focal(MeanIncDif, w=matrix(1,nrow=foc,ncol=foc), pad=T, fun=mean, na.rm=T)
writeRaster (MeanIncDif_foc, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Data_MeanIncDif_foc.tif", overwrite=F, datatype='FLT4S')


## Households in 2010
Dom2010 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/DomDas_Gr10_2010.tif")
Dom2010_foc = focal(Dom2010, w=matrix(1,nrow=foc,ncol=foc), pad=T, fun=sum, na.rm=T)
writeRaster (Dom2010_foc, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Data_Dom2010_foc.tif", overwrite=F, datatype='FLT4S')


## Households change
DomDif = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/Dif_DomDas_Gr10_10e91.tif")
DomDif_foc = focal(DomDif, w=matrix(1,nrow=foc,ncol=foc), pad=T, fun=sum, na.rm=T)
writeRaster (DomDif_foc, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Data_DomDif_foc.tif", overwrite=F, datatype='FLT4S')


## Subnormal settlements 2010
Set2010 = st_read ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade", "Censo2010_WGS23s_LimGrade")
Set2010$Subn = 0
Set2010$Subn [Set2010$TIPO=="S"] = 1
Subn2010 = fasterize(Set2010, a_raster_10, "Subn")
plot(Subn2010)
Subn2010_foc = focal(Subn2010, w=matrix(1,nrow=foc,ncol=foc), pad=T, fun=modal, na.rm=T)
writeRaster (Subn2010_foc, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Data_Subn2010_foc.tif", overwrite=F, datatype='INT1U')
writeRaster (Subn2010, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Data_Subn2010.tif", overwrite=F, datatype='INT1U')


## Subnormal settlements change 
Set1991 = st_read ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade", "Censo1991_WGS23s_LimGrade")
Set1991$Subn = 0 
Set1991$Subn [Set1991$TIPO=="S"] = 2
Subn1991 = fasterize(Set1991, a_raster_10, "Subn")
SubnChange = Subn2010 + Subn1991
SubnChange [SubnChange==3] = 0
SubnChange_foc = focal(SubnChange, w=matrix(1,nrow=foc,ncol=foc), pad=T, fun=modal, na.rm=T)
writeRaster (SubnChange_foc, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Data_SubnChange_foc.tif", overwrite=F, datatype='INT1U')
writeRaster (SubnChange, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Data_SubnChange.tif", overwrite=F, datatype='INT1U')


## Mass movement susceptibility 
Suscep_sh = st_read("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/SIG", "GeotecniaSuscep_MovMass_34Mun_WGS23s")
Suscep_sh$Class = NA
Suscep_sh$Class [Suscep_sh$Classe=="Alta"] = 3
Suscep_sh$Class [Suscep_sh$Classe=="Media"] = 2
Suscep_sh$Class [Suscep_sh$Classe=="Baixa"] = 1
Suscep = fasterize(Suscep_sh, a_raster_10, "Class")
plot(Suscep)
Suscep_foc = focal(Suscep, w=matrix(1,nrow=foc,ncol=foc), pad=T, fun=modal, na.rm=T)
writeRaster (Suscep_foc, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Data_Suscep_foc.tif", overwrite=F, datatype='INT1U')


## Slope 
Slope = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Decliv/CurvEmplSlopeWGS23sGr10_GDAL.tif")
Slope_foc = focal(Slope, w=matrix(1,nrow=foc,ncol=foc), pad=T, fun=max, na.rm=T)
writeRaster (Slope_foc, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Data_Slope_foc.tif", overwrite=F, datatype='FLT4S')


## Aspect
Aspect = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Decliv/CurvEmplAspectWGS23sGr10_GDAL.tif")
Aspect_foc = focal(Aspect, w=matrix(1,nrow=foc,ncol=foc), pad=T, fun=median, na.rm=T)
writeRaster (Aspect_foc, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Data_Slope_foc.tif", overwrite=F, datatype='FLT4S')


## Veg
Veg2010 = raster ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Class/Class_2010_Gr10.tif")
Veg2010 [Veg2010>1] = 0
plot(Veg2010)
masc_rmsp = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/SIG/LimRMSP_WGS23s_Gr10.tif")
Veg2010_rmsp = Veg2010 * masc_rmsp
plot(Veg2010_rmsp)
Veg2010_foc = focal(Veg2010, w=matrix(1,nrow=foc,ncol=foc), pad=T, fun=mean, na.rm=T)
plot(Veg2010_foc)
writeRaster (Veg2010_foc, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Data_Veg2010_foc.tif", overwrite=F, datatype='FLT4S')


## Veg Change
Veg1992 = raster ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Class/Class_1992_Gr10.tif")
Veg1992 [Veg1992>1] = 0
plot(Veg1992)
Veg1992_rmsp = Veg1992 * masc_rmsp
VegDif = Veg2010_rmsp - Veg1992_rmsp
VegDif_foc = focal(VegDif, w=matrix(1,nrow=foc,ncol=foc), pad=T, fun=mean, na.rm=T)
plot(VegDif_foc)
writeRaster (VegDif_foc, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Data_VegDif_foc.tif", overwrite=F, datatype='FLT4S')





##### Adicional Grids
SEsg10 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/DomDas_SEsg_Gr10_2010.tif")
SEsg10_foc = focal(SEsg10, w=matrix(1,nrow=foc,ncol=foc), pad=T, fun=sum, na.rm=T)
writeRaster (SEsg10_foc, "D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_SEsg10_foc.tif", overwrite=F, datatype='FLT4S')

SPav10 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/DomDas_SPav_Gr10_2010.tif")
SPav10_foc = focal(SPav10, w=matrix(1,nrow=foc,ncol=foc), pad=T, fun=sum, na.rm=T)
writeRaster (SPav10_foc, "D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_SPav10_foc.tif", overwrite=F, datatype='FLT4S')

SMF10 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/DomDas_SMF_Gr10_2010.tif")
SMF10_foc = focal(SMF10, w=matrix(1,nrow=foc,ncol=foc), pad=T, fun=sum, na.rm=T)
writeRaster (SMF10_foc, "D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_SMF10_foc.tif", overwrite=F, datatype='FLT4S')

SBL10 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/DomDas_SBL_Gr10_2010.tif")
SBL10_foc = focal(SBL10, w=matrix(1,nrow=foc,ncol=foc), pad=T, fun=sum, na.rm=T)
writeRaster (SBL10_foc, "D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_SBL10_foc.tif", overwrite=F, datatype='FLT4S')

EsgCA10 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/DomDas_EsgCA_Gr10_2010.tif")
EsgCA10_foc = focal(EsgCA10, w=matrix(1,nrow=foc,ncol=foc), pad=T, fun=sum, na.rm=T)
writeRaster (EsgCA10_foc, "D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_EsgCA10_foc.tif", overwrite=F, datatype='FLT4S')

PReFe10 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/PReFe_Gr10_2010.tif")
PReFe10_foc = focal(PReFe10, w=matrix(1,nrow=foc,ncol=foc), pad=T, fun=mean, na.rm=T)
writeRaster (PReFe10_foc, "D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_PReFe10_foc.tif", overwrite=F, datatype='FLT4S')

PReMa10 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/PReMa_Gr10_2010.tif")
PReMa10_foc = focal(PReMa10, w=matrix(1,nrow=foc,ncol=foc), pad=T, fun=mean, na.rm=T)
writeRaster (PReMa10_foc, "D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_PReMa10_foc.tif", overwrite=F, datatype='FLT4S')

PReAlf10 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/PReAlf_Gr10_2010.tif")
PReAlf10_foc = focal(PReAlf10, w=matrix(1,nrow=foc,ncol=foc), pad=T, fun=mean, na.rm=T)
writeRaster (PReAlf10_foc, "D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_PReAlf10_foc.tif", overwrite=F, datatype='FLT4S')

PReFeAlf10 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/PReFeAlf_Gr10_2010.tif")
PReFeAlf10_foc = focal(PReFeAlf10, w=matrix(1,nrow=foc,ncol=foc), pad=T, fun=mean, na.rm=T)
writeRaster (PReFeAlf10_foc, "D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_PReFeAlf10_foc.tif", overwrite=F, datatype='FLT4S')

PReMaAlf10 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/PReMaAlf_Gr10_2010.tif")
PReMaAlf10_foc = focal(PReMaAlf10, w=matrix(1,nrow=foc,ncol=foc), pad=T, fun=mean, na.rm=T)
writeRaster (PReMaAlf10_foc, "D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_PReMaAlf10_foc.tif", overwrite=F, datatype='FLT4S')

HabDm10 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/HabDm_Gr10_2010.tif")
HabDm10_foc = focal(HabDm10, w=matrix(1,nrow=foc,ncol=foc), pad=T, fun=mean, na.rm=T)
writeRaster (HabDm10_foc, "D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_HabDm10_foc.tif", overwrite=F, datatype='FLT4S')





################################################ Estat - Preparação ---------------------------------
rm(list=ls())
gc()


## Samples yes
library(raster)
library(sf)
library(fasterize)

Ano = 2010
set.seed(24)
foc = 7

a_raster_10 = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/SIG/LimGrade_WGS23s_Gr10.tif")

disas = st_read ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Desastres", "Desastres_IGDez2017_Selecionados_Filtro2_WGS23s")
disas_Ano = disas [disas$AnoHidrol=="2010", ]
pts_yes = disas_Ano
pts_yes$ID = 0
pts_yes$Date = as.Date(as.character(pts_yes$Data_Adota), format="%Y/%m/%d")
pts_yes = pts_yes[, c("ID_do_Even", "ID", "Date")]
pts_yes$Class = "yes"
pts_yes = na.omit(pts_yes)
pts_yes$ID = c(1:nrow(pts_yes))

st_write(pts_yes, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Inter/PTS_Yes2038.shp", delete_dsn=F)


## Masc disasters (w/ buff 50m) 
#pts_yes = st_read ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Inter/Estat", "PTS_Yes2038")

r_pts_yes = rasterize (pts_yes, y=a_raster_10, 'ID')
plot(r_pts_yes)
r_pts_yes[is.na(r_pts_yes)] = 0
r_pts_yes[r_pts_yes>0] = 1
r_pts_yes_foc = focal(r_pts_yes, w=matrix(1,nrow=foc,ncol=foc), pad=T, fun=max )
r_pts_yes_foc[is.na(r_pts_yes_foc)] = 0
plot(r_pts_yes_foc)
r_pts_yes_foc[r_pts_yes_foc==1] = 2
r_pts_yes_foc[r_pts_yes_foc==0] = 1
r_pts_yes_foc[r_pts_yes_foc==2] = 0
plot(r_pts_yes_foc)
writeRaster(r_pts_yes_foc, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Inter/Masc_PTSYes2038_foc.tif", overwrite=F, datatype='INT1U')


## Masc dom plus disasters
masc_DomDas = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Grade/DomDas_Gr10_2010.tif")
masc_DomDas[masc_DomDas==0] = NA
masc_DomDas[masc_DomDas>0] = 1
plot(masc_DomDas)
writeRaster(masc_DomDas, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Inter/Masc_Dom.tif", overwrite=F, datatype='INT1U')


## Masc excl Mun
masc_exclMun = st_read ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Inter", "Masc_Mun")
masc_exclMun$masc = 0
masc_exclMun$masc [masc_exclMun$MapSusc==1 & masc_exclMun$DisasAnH10==1] = 1
r_masc_exclMun = fasterize (masc_exclMun, a_raster_10, 'masc')
plot(r_masc_exclMun)
writeRaster(r_masc_exclMun, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Inter/Masc_exclMun.tif", overwrite=F, datatype='INT1U')


## Masc disasters plus dom plus munexcl
masc = r_pts_yes_foc * masc_DomDas * r_masc_exclMun
masc[masc!=1]=NA
plot(masc)
writeRaster(masc, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Inter/Masc.tif", overwrite=F, datatype='INT1U')


## No Points
masc = raster ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Inter/Masc.tif")
pts_no_all = as.data.frame(masc, xy=T, na.rm=T)
#set.seed(24)
#pts_no = pts_no_all[sample(nrow(pts_no_all), 1000000), c("x","y")]
pts_no = pts_no_all[,1:2]
pts_no$ID_do_Even = c("sd")
pts_no$ID = c(1:nrow(pts_no))
set.seed(24)
pts_no$Date = sample(seq(as.Date('2009/10/01'), as.Date('2010/09/30'), by="day"), nrow(pts_no), replace=T)
pts_no$Class = "no"
pts_no_sf = st_as_sf(pts_no, coords = c("x","y"), crs=32723)
st_write(pts_no_sf, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Inter/PTS_No.shp", delete_dsn=F)


## Yes and No Points
pts_yes = st_read("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Inter", "PTS_Yes2038")
pts_no = st_read ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Inter", "PTS_No")
pts = rbind (pts_yes, pts_no)
pts$ID = 1:nrow(pts)
st_write(pts, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Inter/PTS.shp", delete_dsn=F)


## Rainfall - All Previous Days
rm(list=ls())
gc()

library(raster)
library(sf)

Ano = 2010

pts = st_read ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Inter", "PTS")

tmp = pts
tmp$nodia = (as.numeric(tmp$Date - (as.Date("2008.6.01", format="%Y.%m.%d"))))+1
tmp_df = cbind(as.data.frame(tmp), as.data.frame(st_coordinates(tmp)))

tmp$value = NA
range (tmp$nodia)

pts_rain = pts 
pts_rain$Rain0d = NA
pts_rain$Rain1d = NA
pts_rain$Rain2d = NA
pts_rain$Rain3d = NA
pts_rain$Rain4d = NA
pts_rain$Rain5d = NA
pts_rain$Rain6d = NA
pts_rain$Rain7d = NA
pts_rain$Rain8d = NA
pts_rain$Rain9d = NA
pts_rain$Rain10d = NA
pts_rain$Rain11d = NA
pts_rain$Rain12d = NA
pts_rain$Rain13d = NA
pts_rain$Rain14d = NA
pts_rain$Rain15d = NA
pts_rain$Rain16d = NA
pts_rain$Rain17d = NA
pts_rain$Rain18d = NA
pts_rain$Rain19d = NA
pts_rain$Rain20d = NA
pts_rain$Rain21d = NA
pts_rain$Rain22d = NA
pts_rain$Rain23d = NA
pts_rain$Rain24d = NA
pts_rain$Rain25d = NA
pts_rain$Rain26d = NA
pts_rain$Rain27d = NA
pts_rain$Rain28d = NA
pts_rain$Rain29d = NA
pts_rain$Rain30d = NA
pts_rain$Rain35d = NA
pts_rain$Rain40d = NA
pts_rain$Rain45d = NA
pts_rain$Rain60d = NA
pts_rain$Rain90d = NA
pts_rain$Rain120d = NA

listbandRG = list.files("C:/ADriveSync/Andando/Doutorado/_Tese/Processa_Chuva/CHIRPS_Total_2009a2011/",full.names = T)
fun1 <- function(x){readAll(brick(x))}
my.Area.tot.RG <- sapply(listbandRG, fun1) # this is equivalent as a loop

library(dplyr)
system.time({
  for (r in 1:length(my.Area.tot.RG)) {
    rain = my.Area.tot.RG[[r]]
    list_select= vector("list", length(min(range(tmp_df$nodia)):max(range(tmp_df$nodia))))
    for (i in min(range(tmp_df$nodia)):max(range(tmp_df$nodia))) {
      tmp_df_selec = tmp_df[which(tmp_df$nodia==i),]
      tmp_df_selec$value = as.numeric(extract(rain[[i]], tmp_df_selec[,c("X", "Y")], df=F))
      list_select[[i]]=tmp_df_selec
      #tmp_value = rbind(tmp_value, tmp_df_selec)
      print(paste (r, "- ", i))
    }
    tmp_value = bind_rows(list_select, .id = "column_label") 
    tmp_merge = merge(tmp_df, tmp_value[,c("ID","value")], by="ID")
    pts_rain[,r+5] = tmp_merge[,"value"]
  }
})

pts_rain$Rain0d1d = pts_rain$Rain0d + pts_rain$Rain1d
pts_rain$Rain0d2d = pts_rain$Rain0d + pts_rain$Rain2d
pts_rain$Rain0d3d = pts_rain$Rain0d + pts_rain$Rain3d
pts_rain$Rain0d4d = pts_rain$Rain0d + pts_rain$Rain4d
pts_rain$Rain0d5d = pts_rain$Rain0d + pts_rain$Rain5d
pts_rain$Rain0d6d = pts_rain$Rain0d + pts_rain$Rain6d
pts_rain$Rain0d7d = pts_rain$Rain0d + pts_rain$Rain7d
pts_rain$Rain0d8d = pts_rain$Rain0d + pts_rain$Rain8d
pts_rain$Rain0d9d = pts_rain$Rain0d + pts_rain$Rain9d
pts_rain$Rain0d10d = pts_rain$Rain0d + pts_rain$Rain10d
pts_rain$Rain0d11d = pts_rain$Rain0d + pts_rain$Rain11d
pts_rain$Rain0d12d = pts_rain$Rain0d + pts_rain$Rain12d
pts_rain$Rain0d13d = pts_rain$Rain0d + pts_rain$Rain13d
pts_rain$Rain0d14d = pts_rain$Rain0d + pts_rain$Rain14d
pts_rain$Rain0d15d = pts_rain$Rain0d + pts_rain$Rain15d
pts_rain$Rain0d16d = pts_rain$Rain0d + pts_rain$Rain16d
pts_rain$Rain0d17d = pts_rain$Rain0d + pts_rain$Rain17d
pts_rain$Rain0d18d = pts_rain$Rain0d + pts_rain$Rain18d
pts_rain$Rain0d19d = pts_rain$Rain0d + pts_rain$Rain19d
pts_rain$Rain0d20d = pts_rain$Rain0d + pts_rain$Rain20d
pts_rain$Rain0d21d = pts_rain$Rain0d + pts_rain$Rain21d
pts_rain$Rain0d22d = pts_rain$Rain0d + pts_rain$Rain22d
pts_rain$Rain0d23d = pts_rain$Rain0d + pts_rain$Rain23d
pts_rain$Rain0d24d = pts_rain$Rain0d + pts_rain$Rain24d
pts_rain$Rain0d25d = pts_rain$Rain0d + pts_rain$Rain25d
pts_rain$Rain0d26d = pts_rain$Rain0d + pts_rain$Rain26d
pts_rain$Rain0d27d = pts_rain$Rain0d + pts_rain$Rain27d
pts_rain$Rain0d28d = pts_rain$Rain0d + pts_rain$Rain28d
pts_rain$Rain0d29d = pts_rain$Rain0d + pts_rain$Rain29d
pts_rain$Rain0d30d = pts_rain$Rain0d + pts_rain$Rain30d
pts_rain$Rain0d35d = pts_rain$Rain0d + pts_rain$Rain35d
pts_rain$Rain0d40d = pts_rain$Rain0d + pts_rain$Rain40d
pts_rain$Rain0d45d = pts_rain$Rain0d + pts_rain$Rain45d
pts_rain$Rain0d60d = pts_rain$Rain0d + pts_rain$Rain60d
pts_rain$Rain0d90d = pts_rain$Rain0d + pts_rain$Rain90d
pts_rain$Rain0d120d = pts_rain$Rain0d + pts_rain$Rain120d

write.csv2 (pts_rain, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Inter/DataRain_PTS.csv")

pts_rain_sf = st_as_sf(pts_rain, coords = c("x","y"), crs=32723)
st_write(pts_rain_sf, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Inter/DataRain_PTS.shp", delete_dsn=F)





################################################ Estat - Extract explanatory variables ---------------------------------
rm(list=ls())
gc()

library(raster)
library(sf)
library(caret)

Ano = 2010

pts = st_read ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Inter", "PTS")
Data=pts

Data_Dom2010_foc = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Inter/Data_Dom2010_foc.tif")
tab = extract(Data_Dom2010_foc, pts, df=T)
Data$Dm2010 = tab[,c("Data_Dom2010_foc")]

Data_DomDif_foc = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Inter/Data_DomDif_foc.tif")
tab = extract(Data_DomDif_foc, pts, df=T)
Data$DmDif = tab[,c("Data_DomDif_foc")]

Data_MeanInc2010_foc = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Inter/Data_MeanInc2010_foc.tif")
tab = extract(Data_MeanInc2010_foc, pts, df=T)
Data$MnInc2010 = tab[,c("Data_MeanInc2010_foc")]

Data_MeanIncDif_foc = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Inter/Data_MeanIncDif_foc.tif")
tab = extract(Data_MeanIncDif_foc, pts, df=T)
Data$MnIncDif = tab[,c("Data_MeanIncDif_foc")]

Data_Subn2010_foc = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Inter/Data_Subn2010_foc.tif")
tab = extract(Data_Subn2010_foc, pts, df=T)
Data$Subn2010 = as.factor(tab[,c("Data_Subn2010_foc")])

Data_SubnChange_foc = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Inter/Data_SubnChange_foc.tif")
tab = extract(Data_SubnChange_foc, pts, df=T)
Data$SubnChn = as.factor(tab[,c("Data_SubnChange_foc")])

Data_Suscep_foc = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Inter/Data_Suscep_foc.tif")
tab = extract(Data_Suscep_foc, pts, df=T)
Data$Suscep = as.factor(tab[,c("Data_Suscep_foc")])

Data_Veg2010_foc = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Inter/Data_Veg2010_foc.tif")
tab = extract(Data_Veg2010_foc, pts, df=T)
tab[is.na(tab)] = 0
Data$Veg2010 = tab[,c("Data_Veg2010_foc")]

Data_VegDif_foc = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Inter/Data_VegDif_foc.tif")
tab = extract(Data_VegDif_foc, pts, df=T)
tab[is.na(tab)] = 0
Data$VegDif = tab[,c("Data_VegDif_foc")]

Data_Slope_foc = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Inter/Data_Slope_foc.tif")
tab = extract(Data_Slope_foc, pts, df=T)
Data$Slope = tab[,c("Data_Slope_foc")]

Data_Aspect_foc = raster("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Inter/Data_Aspect_foc.tif")
tab = extract(Data_Aspect_foc, pts, df=T)
Data$Aspect = tab[,c("Data_Aspect_foc")]

pts_rain = read.csv2 ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Inter/DataRain_PTS.csv", header=T)  
Data$Rain0d = pts_rain$Rain0d
Data$Rain0d1d = pts_rain$Rain0d1d
Data$Rain0d2d = pts_rain$Rain0d2d
Data$Rain0d3d = pts_rain$Rain0d3d
Data$Rain0d4d = pts_rain$Rain0d4d
Data$Rain0d5d = pts_rain$Rain0d5d
Data$Rain0d6d = pts_rain$Rain0d6d
Data$Rain0d7d = pts_rain$Rain0d7d
Data$Rain0d8d = pts_rain$Rain0d8d
Data$Rain0d9d = pts_rain$Rain0d9d
Data$Rain0d10d = pts_rain$Rain0d10d
Data$Rain0d11d = pts_rain$Rain0d11d
Data$Rain0d12d = pts_rain$Rain0d12d
Data$Rain0d13d = pts_rain$Rain0d13d
Data$Rain0d14d = pts_rain$Rain0d14d
Data$Rain0d15d = pts_rain$Rain0d15d
Data$Rain0d16d = pts_rain$Rain0d16d
Data$Rain0d17d = pts_rain$Rain0d17d
Data$Rain0d18d = pts_rain$Rain0d18d
Data$Rain0d19d = pts_rain$Rain0d19d
Data$Rain0d20d = pts_rain$Rain0d20d
Data$Rain0d21d = pts_rain$Rain0d21d
Data$Rain0d22d = pts_rain$Rain0d22d
Data$Rain0d23d = pts_rain$Rain0d23d
Data$Rain0d24d = pts_rain$Rain0d24d
Data$Rain0d25d = pts_rain$Rain0d25d
Data$Rain0d26d = pts_rain$Rain0d26d
Data$Rain0d27d = pts_rain$Rain0d27d
Data$Rain0d28d = pts_rain$Rain0d28d
Data$Rain0d29d = pts_rain$Rain0d29d
Data$Rain0d30d = pts_rain$Rain0d30d
Data$Rain0d35d = pts_rain$Rain0d35d
Data$Rain0d40d = pts_rain$Rain0d40d
Data$Rain0d45d = pts_rain$Rain0d45d
Data$Rain0d60d = pts_rain$Rain0d60d 
Data$Rain0d90d = pts_rain$Rain0d90d
Data$Rain0d120d = pts_rain$Rain0d120d


## Adicional variables
pts = st_read ("D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter", "PTS")

Data = st_read ("D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter", "DataAll_PTS_old")
Data = Data[,-(53:55)]

pts_rain = read.csv2 ("D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/DataRain_PTS.csv", header=T)  
Data$RainIn0d14d = pts_rain$Rain0d / pts_rain$Rain0d14d
Data$RainIn0d14d [Data$Rain0d==0] = 0
Data$RainIn0d14d [pts_rain$Rain0d14d==0] = 0
Data$RainIn1d14d = pts_rain$Rain0d1d / pts_rain$Rain0d14d
Data$RainIn1d14d [Data$Rain0d1d==0] = 0
Data$RainIn1d14d [pts_rain$Rain0d14d==0] = 0
Data$RainIn2d14d = pts_rain$Rain0d2d / pts_rain$Rain0d14d
Data$RainIn2d14d [Data$Rain0d2d==0] = 0
Data$RainIn2d14d [pts_rain$Rain0d14d==0] = 0

Data_Dom2010_foc = raster("D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_Dom2010_foc.tif")
Dom = extract(Data_Dom2010_foc, pts, df=T)

Data_SEsg10_foc = raster("D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_SEsg10_foc.tif")
tab = extract(Data_SEsg10_foc, pts, df=T)
Data$PSEsg10 = tab[,c("Data_SEsg10_foc")]/Dom[,c("Data_Dom2010_foc")]
Data$PSEsg10 [tab[,c("Data_SEsg10_foc")]==0] = 0
Data$PSEsg10 [Dom[,c("Data_Dom2010_foc")]==0] = 0

Data_SPav10_foc = raster("D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_SPav10_foc.tif")
tab = extract(Data_SPav10_foc, pts, df=T)
Data$PSPav10 = tab[,c("Data_SPav10_foc")]/Dom[,c("Data_Dom2010_foc")]
Data$PSPav10 [tab[,c("Data_SPav10_foc")]==0] = 0
Data$PSPav10 [Dom[,c("Data_Dom2010_foc")]==0] = 0

Data_SMF10_foc = raster("D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_SMF10_foc.tif")
tab = extract(Data_SMF10_foc, pts, df=T)
Data$PSMF10 = tab[,c("Data_SMF10_foc")]/Dom[,c("Data_Dom2010_foc")]
Data$PSMF10 [tab[,c("Data_SMF10_foc")]==0] = 0
Data$PSMF10 [Dom[,c("Data_Dom2010_foc")]==0] = 0

Data_SBL10_foc = raster("D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_SBL10_foc.tif")
tab = extract(Data_SBL10_foc, pts, df=T)
Data$PSBL10 = tab[,c("Data_SBL10_foc")]/Dom[,c("Data_Dom2010_foc")]
Data$PSBL10 [tab[,c("Data_SBL10_foc")]==0] = 0
Data$PSBL10 [Dom[,c("Data_Dom2010_foc")]==0] = 0

Data_EsgCA10_foc = raster("D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_EsgCA10_foc.tif")
tab = extract(Data_EsgCA10_foc, pts, df=T)
Data$PEsgCA10 = tab[,c("Data_EsgCA10_foc")]/Dom[,c("Data_Dom2010_foc")]
Data$PEsgCA10 [tab[,c("Data_EsgCA10_foc")]==0] = 0
Data$PEsgCA10 [Dom[,c("Data_Dom2010_foc")]==0] = 0

Data_PReFe10_foc = raster("D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_PReFe10_foc.tif")
tab = extract(Data_PReFe10_foc, pts, df=T)
Data$PReFe10 = tab[,c("Data_PReFe10_foc")]

Data_PReMa10_foc = raster("D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_PReMa10_foc.tif")
tab = extract(Data_PReMa10_foc, pts, df=T)
Data$PReMa10 = tab[,c("Data_PReMa10_foc")]

Data_PReAlf10_foc = raster("D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_PReAlf10_foc.tif")
tab = extract(Data_PReAlf10_foc, pts, df=T)
Data$PReAlf10 = tab[,c("Data_PReAlf10_foc")]

Data_PReFeAlf10_foc = raster("D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_PReFeAlf10_foc.tif")
tab = extract(Data_PReFeAlf10_foc, pts, df=T)
Data$PReFeAlf10 = tab[,c("Data_PReFeAlf10_foc")]

Data_PReMaAlf10_foc = raster("D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_PReMaAlf10_foc.tif")
tab = extract(Data_PReMaAlf10_foc, pts, df=T)
Data$PReMaAlf10 = tab[,c("Data_PReMaAlf10_foc")]

Data_HabDm10_foc = raster("D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_HabDm10_foc.tif")
tab = extract(Data_HabDm10_foc, pts, df=T)
Data$HabDm10 = tab[,c("Data_HabDm10_foc")]


## Save
st_write(Data, "D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/DataAll_PTS.shp", delete_dsn=F)
write.csv2 (Data, "D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/DataAll_PTS.csv")

set.seed(24)
DataIndex = createDataPartition(Data$Class, p=0.8, list=F)
Data_train = Data[DataIndex,]
Data_test = Data[-DataIndex,]

colnames(Data_test)[16] = c("Rn0d")
colnames(Data_test)[17] = c("Rn0d1d") 
colnames(Data_test)[18] = c("Rn0d2d")
colnames(Data_test)[19] = c("Rn0d3d")
colnames(Data_test)[20] = c("Rn0d4d")
colnames(Data_test)[21] = c("Rn0d5d")
colnames(Data_test)[22] = c("Rn0d6d")
colnames(Data_test)[23] = c("Rn0d7d")
colnames(Data_test)[24] = c("Rn0d8d")
colnames(Data_test)[25] = c("Rn0d9d")

write.csv2 (Data_train, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/DataAll_Train_PTS.csv")
write.csv2 (Data_test, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/DataAll_Test_PTS.csv")





################################################ Estat - Rain Regr ---------------------------------
rm(list=ls())
gc()

library(data.table)
library(lmtest) #lr test
library(pscl) #R2
library(ggplot2)

rm(list=ls()[! ls() %in% c("pts_data")])
# pts_data = read.csv2 ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/DataAll_Train_PTS.csv", header=T)

Ano = 2010
# Nboot = 2
Nboot = 1000
Nsamples = 1000

Var_Rain = c("ID", "Class",
            "Rn0d",
            "Rn0d1d","Rn0d2d","Rn0d3d","Rn0d4d","Rn0d5d",
            "Rn0d6d","Rn0d7d","Rn0d8d","Rn0d9d","Rn0d10d",
            "Rn0d11d","Rn0d12d","Rn0d13d","Rn0d14d","Rn0d15d",
            "Rn0d16d","Rn0d17d","Rn0d18d","Rn0d19d","Rn0d20d",
            "Rn0d21d","Rn0d22d","Rn0d23d","Rn0d24d","Rn0d25d",
            "Rn0d26d","Rn0d27d","Rn0d28d","Rn0d29d","Rn0d30d",
            "Rn0d35d","Rn0d40d","Rn0d45d","Rn0d60d",
            "Rn0d90d","Rn0d120")

nVar_Rain = length(Var_Rain)-2

data = as.data.frame(pts_data)
data = data[,Var_Rain]
tab_class = as.data.frame(data$Class)
names(tab_class) = c("Class")
tab_class$Cl = NA
tab_class$Cl[tab_class$Class=="yes"] = 1
tab_class$Cl[tab_class$Class=="no"] = 0
data$Class = as.factor(tab_class$Cl)
rm(tab_class)
data = droplevels(data)

data_Rain = data.table(data)
setkeyv(data_Rain,c("Class"))

# Variable selection [v] and Bootstrap to model calculation [b]
#Model parameters: Coefficient (intercept&parameter); std. error (intercept&parameter); p-value (intercept&parameter)[association between variable and response]
#Statistical Tests for Individual Predictors: Likelihood Ratio Test [significance of the addition of new term to the model] 
#Goodness of Fit: R2 (Nagelkerke R^2 index) from lrm (rms package)
VarEstat_rain = c("coef","coef.se","coef.p-val", 
                  "LRchi2","LRchi2.pvalue",
                  "r2Nagelkerke")

Model_Rain_Samples_train = data.frame(Boot=1:Nboot, Samples=NA)

Model_Rain_coef = data.frame(matrix(NA, ncol=Nboot, nrow=nVar_Rain), row.names=c(Var_Rain[-(1:2)]))
colnames(Model_Rain_coef) = paste("Boot",1:Nboot, sep=" ")
Model_Rain_coef.se = Model_Rain_coef
Model_Rain_coef.pvalue = Model_Rain_coef

Model_Rain_intercept = Model_Rain_coef
Model_Rain_intercept.se = Model_Rain_coef
Model_Rain_intercept.pvalue = Model_Rain_coef

Model_Rain_LogLik_l0 = Model_Rain_coef
Model_Rain_LogLik_lM = Model_Rain_coef

Model_Rain_LRchi2 = Model_Rain_coef
Model_Rain_LRchi2.pvalue = Model_Rain_coef
Model_Rain_r2Nagelkerke = Model_Rain_coef
Model_Rain_aic = Model_Rain_coef

for (b in (1:Nboot)) {
  tmp_Rain = rbind((data_Rain[data_Rain$Class==1,][sample(nrow(data_Rain[data_Rain$Class==1,]), Nsamples, replace=F),]), 
              (data_Rain[data_Rain$Class==0,][sample(nrow(data_Rain[data_Rain$Class==0,]), Nsamples, replace=F),]))
  Model_Rain_Samples_train[b,2] = paste(as.vector(tmp_Rain$ID),collapse="-")
  for (v in (1:nVar_Rain)) { 
    cols = c("Class",Var_Rain[2+v])
    tmp_Rainv = tmp_Rain[,cols,with=F]
    tmp_Rainv = na.omit(tmp_Rainv)
    m_Rain = glm(formula=Class ~ ., family=binomial(link="logit"), data=tmp_Rainv)
    Model_Rain_coef[v,b] = as.numeric(coef(summary(m_Rain))[2,1])
    Model_Rain_coef.se[v,b] = as.numeric(coef(summary(m_Rain))[2,2])
    Model_Rain_coef.pvalue[v,b] = as.numeric(coef(summary(m_Rain))[2,4])
    Model_Rain_intercept[v,b] = as.numeric(coef(summary(m_Rain))[1,1])
    Model_Rain_intercept.se[v,b] = as.numeric(coef(summary(m_Rain))[1,2])
    Model_Rain_intercept.pvalue[v,b] = as.numeric(coef(summary(m_Rain))[1,4])
    tmp_Rain_lrtest = lrtest(m_Rain) 
    Model_Rain_LogLik_lM[v,b] = as.numeric(tmp_Rain_lrtest[1,2])
    Model_Rain_LogLik_l0[v,b] = as.numeric(tmp_Rain_lrtest[2,2])
    Model_Rain_LRchi2[v,b] = as.numeric(tmp_Rain_lrtest[2,4])
    Model_Rain_LRchi2.pvalue[v,b] = as.numeric(tmp_Rain_lrtest[2,5])
    tmp_Rain_pR2 = pR2(m_Rain)
    Model_Rain_r2Nagelkerke[v,b] = as.numeric(tmp_Rain_pR2[6])
    Model_Rain_aic[v,b] = as.numeric(m_Rain$aic)
    print(paste("variav =", Var_Rain[2+v], "- boot =", b))
  }
}

# write.csv2 (Model_Rain_Samples_train,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Rain_Samples_train.csv")
# write.csv2 (Model_Rain_coef,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Rain_coef.csv")
# write.csv2 (Model_Rain_coef.se,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Rain_coef.se.csv")
# write.csv2 (Model_Rain_coef.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Rain_coef.pvalue.csv")
# write.csv2 (Model_Rain_intercept,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Rain_intercept.csv")
# write.csv2 (Model_Rain_intercept.se,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Rain_intercept.se.csv")
# write.csv2 (Model_Rain_intercept.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Rain_interceptf.pvalue.csv")
# write.csv2 (Model_Rain_LRchi2,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Rain_LRchi2.csv")
# write.csv2 (Model_Rain_LRchi2.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Rain_LRchi2.pvalue.csv")
# write.csv2 (Model_Rain_r2Nagelkerke,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Rain_r2Nagelkerke.csv")
# write.csv2 (Model_Rain_LogLik_lM,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Rain_LogLik_lM.csv")
# write.csv2 (Model_Rain_LogLik_l0,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Rain_LogLik_l0.csv")
# write.csv2 (Model_Rain_aic,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Rain_aic.csv")

VarEstat_RainAll = c("mean.coef_param","CIinf mean.coef_param","CIsup coef_param",
                  "mean.coef.p-value","CIinf mean.coef.p-value","CIsup mean.coef.p-value",
                  "mean.LL0","CIinf mean.LL0","CIsup mean.LL0",
                  "mean.LLM0","CIinf mean.LLM0","CIsup mean.LLM0",
                  "mean.LLMx","CIinf mean.LLMx","CIsup mean.LLMx",
                  "mean.LRchi2_MXm0","CIinf mean.LRchi2_MXm0","CIsup mean_MXm0.LRchi2",
                  "mean.LRchi2_MXM0","CIinf mean.LRchi2_MXM0","CIsup mean_MXM0.LRchi2",
                  "mean.LRchi2_MXm0.pvalue","CIinf mean.LRchi2_MXm0.pvalue","CIsup mean_MXm0.LRchi2.pvalue",
                  "mean.LRchi2_MXM0.pvalue","CIinf mean.LRchi2_MXM0.pvalue","CIsup mean_MXM0.LRchi2.pvalue",
                  "mean.r2Nagelkerke","CIinf mean.r2Nagelkerke", "CIsup mean.r2Nagelkerke",
                  "mean.aic","CIinf mean.aic", "CIsup mean.aic")

Model_RainAll = data.frame(matrix(NA, ncol=length(VarEstat_RainAll), nrow=nVar_Rain))
colnames(Model_RainAll) = c(VarEstat_RainAll)
rownames(Model_RainAll) = Var_Rain[-(1:2)]

for (v in 1:nVar_Rain) {
  variv = Var_Rain[v+2]
  Model_RainAll[v,1] = mean(as.numeric(Model_Rain_coef[v,]))
  Model_RainAll[v,2] = mean(as.numeric(Model_Rain_coef[v,])) - (qnorm(0.975)*sd(as.numeric(Model_Rain_coef[v,]))/sqrt(Nboot))
  Model_RainAll[v,3] = mean(as.numeric(Model_Rain_coef[v,])) + (qnorm(0.975)*sd(as.numeric(Model_Rain_coef[v,]))/sqrt(Nboot))
  Model_RainAll[v,4] = mean(as.numeric(Model_Rain_coef.pvalue[v,]))
  Model_RainAll[v,5] = mean(as.numeric(Model_Rain_coef.pvalue[v,])) - (qnorm(0.975)*sd(as.numeric(Model_Rain_coef.pvalue[v,]))/sqrt(Nboot))
  Model_RainAll[v,6] = mean(as.numeric(Model_Rain_coef.pvalue[v,])) + (qnorm(0.975)*sd(as.numeric(Model_Rain_coef.pvalue[v,]))/sqrt(Nboot))
  
  Model_RainAll[v,7] = mean(as.numeric(Model_Rain_LogLik_l0[v,]))
  Model_RainAll[v,8] = mean(as.numeric(Model_Rain_LogLik_l0[v,])) - (qnorm(0.975)*((sd(as.numeric(Model_Rain_LogLik_l0[v,])))/sqrt(Nboot)))
  Model_RainAll[v,9] = mean(as.numeric(Model_Rain_LogLik_l0[v,])) + (qnorm(0.975)*((sd(as.numeric(Model_Rain_LogLik_l0[v,])))/sqrt(Nboot)))
  Model_RainAll[v,10] = mean(as.numeric(Model_Rain_LogLik_lM[v,]))
  Model_RainAll[v,11] = mean(as.numeric(Model_Rain_LogLik_lM[v,])) - (qnorm(0.975)*((sd(as.numeric(Model_Rain_LogLik_lM[v,])))/sqrt(Nboot)))
  Model_RainAll[v,12] = mean(as.numeric(Model_Rain_LogLik_lM[v,])) + (qnorm(0.975)*((sd(as.numeric(Model_Rain_LogLik_lM[v,])))/sqrt(Nboot)))
  
  Model_RainAll[v,16] = mean(as.numeric(Model_Rain_LRchi2[v,]))
  Model_RainAll[v,17] = mean(as.numeric(Model_Rain_LRchi2[v,])) - (qnorm(0.975)*sd(as.numeric(Model_Rain_LRchi2[v,]))/sqrt(Nboot))
  Model_RainAll[v,18] = mean(as.numeric(Model_Rain_LRchi2[v,])) + (qnorm(0.975)*sd(as.numeric(Model_Rain_LRchi2[v,]))/sqrt(Nboot))
  
  Model_RainAll[v,22] = mean(as.numeric(Model_Rain_LRchi2.pvalue[v,]))
  Model_RainAll[v,23] = mean(as.numeric(Model_Rain_LRchi2.pvalue[v,])) - (qnorm(0.975)*sd(as.numeric(Model_Rain_LRchi2.pvalue[v,]))/sqrt(Nboot))
  Model_RainAll[v,24] = mean(as.numeric(Model_Rain_LRchi2.pvalue[v,])) + (qnorm(0.975)*sd(as.numeric(Model_Rain_LRchi2.pvalue[v,]))/sqrt(Nboot))
  
  Model_RainAll[v,28] = mean(as.numeric(Model_Rain_r2Nagelkerke[v,]))
  Model_RainAll[v,29] = mean(as.numeric(Model_Rain_r2Nagelkerke[v,])) - (qnorm(0.975)*sd(as.numeric(Model_Rain_r2Nagelkerke[v,]))/sqrt(Nboot))
  Model_RainAll[v,30] = mean(as.numeric(Model_Rain_r2Nagelkerke[v,])) + (qnorm(0.975)*sd(as.numeric(Model_Rain_r2Nagelkerke[v,]))/sqrt(Nboot))
  
  Model_RainAll[v,31] = mean(as.numeric(Model_Rain_aic[v,]))
  Model_RainAll[v,32] = mean(as.numeric(Model_Rain_aic[v,])) - (qnorm(0.975)*((sd(as.numeric(Model_Rain_aic[v,])))/sqrt(Nboot)))
  Model_RainAll[v,33] = mean(as.numeric(Model_Rain_aic[v,])) + (qnorm(0.975)*((sd(as.numeric(Model_Rain_aic[v,])))/sqrt(Nboot)))
  
  print(variv)
}

# write.csv2 (Model_RainAll,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_2010_Rain.csv" )


# Plot - Model
Model_RainAll = read.csv2("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_2010_Rain.csv", header=T)

Data_plot = as.data.frame(c(0:30, 35, 40 , 45, 60, 90, 120))
names(Data_plot) = c("RainD")
Data_plot$RainR2 = Model_RainAll[,"mean.LRchi2_MXm0"]
Data_plot$RainR2inf = Model_RainAll[,"CIinf.mean.LRchi2_MXm0"]
Data_plot$RainR2sup = Model_RainAll[,"CIsup.mean_MXm0.LRchi2"]

min(Data_plot$RainR2)
max(Data_plot$RainR2)

png(height=900, width=900, pointsize=15,
    file=c("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Exploration/Rain-Model_Rain_LRTest.png"))
p = ggplot(Data_plot, aes(x=RainD, y=(RainR2), ymin=(RainR2inf), ymax=(RainR2sup))) +
  geom_ribbon(alpha=.5) +
  geom_line() +
  geom_point() +
  labs(x="Number of previous days considered") + theme(axis.title=element_text(size=20), axis.text=element_text(size=15)) +
  scale_x_continuous(minor_breaks=seq(0,125,5), breaks=seq(0,125,10), limits=c(0,125)) +
  labs(y="mean LR Chi-squared") + theme(axis.title=element_text(size=20), axis.text=element_text(size=15))  +
  scale_y_continuous(minor_breaks=seq(300,1000,20), breaks=seq(300,1000,100), limits=c(300,1000)) + 
  labs(title=Ano) + theme(axis.text=element_text(size=15))
plot(p)
dev.off()

# Plot - Model
Data_plot = as.data.frame(c(0:30, 35, 40 , 45, 60, 90, 120))
names(Data_plot) = c("RainD")
Data_plot$RainR2 = Model_RainAll[,"mean.r2Nagelkerke"]
Data_plot$RainR2inf = Model_RainAll[,"CIinf.mean.r2Nagelkerke"]
Data_plot$RainR2sup = Model_RainAll[,"CIsup.mean.r2Nagelkerke"]

png(height=900, width=900, pointsize=15,
    file=c("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Exploration/Rain-Model_Rain_r2Nagelkerke.png"))
p = ggplot(Data_plot, aes(x=RainD, y=(RainR2*100), ymin=(RainR2inf*100), ymax=(RainR2sup*100))) +
  geom_ribbon(alpha=.5) +
  geom_line() +
  geom_point() +
  labs(x="Number of previous days considered") + theme(axis.title=element_text(size=20), axis.text=element_text(size=15)) +
  scale_x_continuous(minor_breaks=seq(0,125,5), breaks=seq(0,125,10), limits=c(0,125)) +
  labs(y="mean Pseudo R2") + theme(axis.title=element_text(size=20), axis.text=element_text(size=15))  +
  scale_y_continuous(minor_breaks=seq(0,60,2), breaks=seq(0,60,10), limits=c(0,60)) + 
  labs(title=Ano) + theme(axis.text=element_text(size=15))
plot(p)
dev.off()

 
################################################ Estat - DE (Data Exploration) ---------------------------------
rm(list=ls())
gc()

library(caret)
library(ggplot2)

#rm(list=ls()[! ls() %in% c("pts_data")])
pts_data = read.csv2 ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/DataAll_PTS.csv", header=T)

Ano = 2010

data = as.data.frame(pts_data)

data$Sbn2010 = as.factor(data$Sbn2010)

data$SubnChn = as.factor(data$SubnChn)

data$SubnChn2cl = data$SubnChn
data$SubnChn2cl [data$SubnChn==2] = 0
data$SubnChn2cl = as.factor(data$SubnChn2cl)

data$Suscep2e3 [data$Suscep==1] = 0
data$Suscep2e3 [data$Suscep==2] = 1
data$Suscep2e3 [data$Suscep==3] = 1
data$Suscep2e3 = as.factor(data$Suscep2e3)

data$Suscep3 [data$Suscep==1] = 0
data$Suscep3 [data$Suscep==2] = 0
data$Suscep3 [data$Suscep==3] = 1
data$Suscep3 = as.factor(data$Suscep3)

data$Suscep = as.factor(data$Suscep)
unique(data$Suscep)

tab_class = as.data.frame(data$Class)
names(tab_class) = c("Class")
tab_class$Cl = NA
tab_class$Cl[tab_class$Class=="yes"] = 1
tab_class$Cl[tab_class$Class=="no"] = 0
data$Class = as.factor(tab_class$Cl)
rm(tab_class)

data$AspectD = NA
data$AspectD [is.na(data$Aspect)] = "Flat"
data$AspectD [data$Aspect==0] = "N"
data$AspectD [data$Aspect>=(337.5)| data$Aspect<(22.5)] = "N"
data$AspectD [data$Aspect>=(22.5) & data$Aspect<(67.5)] = "NE"
data$AspectD [data$Aspect>=(67.5) & data$Aspect<(112.5)] = "E"
data$AspectD [data$Aspect>=(112.5) & data$Aspect<(157.5)] = "SE"
data$AspectD [data$Aspect>=(157.5) & data$Aspect<(202.5)] = "S"
data$AspectD [data$Aspect>=(202.5) & data$Aspect<(247.5)] = "SW"
data$AspectD [data$Aspect>=(247.5) & data$Aspect<(292.5)] = "W"
data$AspectD [data$Aspect>=(292.5) & data$Aspect<(337.5)] = "NW"
data$AspectD = as.factor(data$AspectD)
unique(data$AspectD)

data$AspectDN = 0
data$AspectDN [data$AspectD=="N"] = 1
data$AspectDN = as.factor(data$AspectDN)
data$AspectDNE = 0
data$AspectDNE [data$AspectD=="NE"] = 1
data$AspectDNE = as.factor(data$AspectDNE)
data$AspectDE = 0
data$AspectDE [data$AspectD=="E"] = 1
data$AspectDE = as.factor(data$AspectDE)
data$AspectDSE = 0
data$AspectDSE [data$AspectD=="SE"] = 1
data$AspectDSE = as.factor(data$AspectDSE)
data$AspectDS = 0
data$AspectDS [data$AspectD=="S"] = 1
data$AspectDS = as.factor(data$AspectDS)
data$AspectDSW = 0
data$AspectDSW [data$AspectD=="SW"] = 1
data$AspectDSW = as.factor(data$AspectDSW)
data$AspectDW = 0
data$AspectDW [data$AspectD=="W"] = 1
data$AspectDW = as.factor(data$AspectDW)
data$AspectDNW = 0
data$AspectDNW [data$AspectD=="NW"] = 1
data$AspectDNW = as.factor(data$AspectDNW)
data$AspectDFlat = 0
data$AspectDFlat [data$AspectD=="Flat"] = 1
data$AspectDFlat = as.factor(data$AspectDFlat)

data$AspectD4 = NA
data$AspectD4 [is.na(data$Aspect)] = "0" #"Flat"
data$AspectD4 [data$Aspect==0] =  "1" #"N"
data$AspectD4 [data$Aspect>=(315)| data$Aspect<(45)] =  "1" #"N"
data$AspectD4 [data$Aspect>=(45) & data$Aspect<(135)] =  "2" #"E"
data$AspectD4 [data$Aspect>=(135) & data$Aspect<(225)] =  "3" #"S"
data$AspectD4 [data$Aspect>=(225) & data$Aspect<(315)] =  "4" #"W"
data$AspectD4 = as.factor(data$AspectD4)
unique(data$AspectD4)

data$AspectD4Flat = 0
data$AspectD4Flat [data$AspectD=="Flat"] = 1
data$AspectD4Flat = as.factor(data$AspectD4Flat)
data$AspectD4N = 0
data$AspectD4N [data$AspectD=="N"] = 1
data$AspectD4N = as.factor(data$AspectD4N)
data$AspectD4E = 0
data$AspectD4E [data$AspectD=="E"] = 1
data$AspectD4E = as.factor(data$AspectD4E)
data$AspectD4S = 0
data$AspectD4S [data$AspectD=="S"] = 1
data$AspectD4S = as.factor(data$AspectD4S)
data$AspectD4W = 0
data$AspectD4W [data$AspectD=="W"] = 1
data$AspectD4W = as.factor(data$AspectD4W)

data = droplevels(data)

rm(list=ls()[! ls() %in% c("pts_data","data")])


########################################### DE - Rains ---------------------------------
VarRain = c("Class",
            "Rain0d",
            "Ran0d1d","Ran0d2d","Ran0d3d","Ran0d4d","Ran0d5d",
            "Ran0d6d","Ran0d7d","Ran0d8d","Ran0d9d","Rn0d10d",
            "Rn0d11d","Rn0d12d","Rn0d13d","Rn0d14d","Rn0d15d",
            "Rn0d16d","Rn0d17d","Rn0d18d","Rn0d19d","Rn0d20d",
            "Rn0d21d","Rn0d22d","Rn0d23d","Rn0d24d","Rn0d25d",
            "Rn0d26d","Rn0d27d","Rn0d28d","Rn0d29d","Rn0d30d",
            "Rn0d35d","Rn0d40d","Rn0d45d","Rn0d60d",
            "Rn0d90d","Rn0d120")

names = VarRain [-1]

names_Hist = c("Rainfall in the event day (mm)","Accumulated Rainfall in the event day and previous day (mm)",
               "Accumulated Rainfall in the event day and 2 previous days (mm)","Accumulated Rainfall in the event day and 3 previous days (mm)",
               "Accumulated Rainfall in the event day and 4 previous days (mm)","Accumulated Rainfall in the event day and 5 previous days (mm)",
               "Accumulated Rainfall in the event day and 6 previous days (mm)","Accumulated Rainfall in the event day and 7 previous days (mm)",
               "Accumulated Rainfall in the event day and 8 previous days (mm)","Accumulated Rainfall in the event day and 9 previous days (mm)",
               "Accumulated Rainfall in the event day and 10 previous days (mm)","Accumulated Rainfall in the event day and 11 previous days (mm)",
               "Accumulated Rainfall in the event day and 12 previous days (mm)","Accumulated Rainfall in the event day and 13 previous days (mm)",
               "Accumulated Rainfall in the event day and 14 previous days (mm)","Accumulated Rainfall in the event day and 15 previous days (mm)",
               "Accumulated Rainfall in the event day and 16 previous days (mm)","Accumulated Rainfall in the event day and 17 previous days (mm)",
               "Accumulated Rainfall in the event day and 18 previous days (mm)","Accumulated Rainfall in the event day and 19 previous days (mm)",
               "Accumulated Rainfall in the event day and 20 previous days (mm)","Accumulated Rainfall in the event day and 21 previous days (mm)",
               "Accumulated Rainfall in the event day and 22 previous days (mm)","Accumulated Rainfall in the event day and 23 previous days (mm)",
               "Accumulated Rainfall in the event day and 24 previous days (mm)","Accumulated Rainfall in the event day and 25 previous days (mm)",
               "Accumulated Rainfall in the event day and 26 previous days (mm)","Accumulated Rainfall in the event day and 27 previous days (mm)",
               "Accumulated Rainfall in the event day and 28 previous days (mm)","Accumulated Rainfall in the event day and 29 previous days (mm)",
               "Accumulated Rainfall in the event day and 30 previous days (mm)","Accumulated Rainfall in the event day and 35 previous days (mm)",
               "Accumulated Rainfall in the event day and 40 previous days (mm)","Accumulated Rainfall in the event day and 45 previous days (mm)",
               "Accumulated Rainfall in the event day and 60 previous days (mm)", "Accumulated Rainfall in the event day and 90 previous days (mm)",
               "Accumulated Rainfall in the event day and 120 previous days (mm)")

dataRain = as.data.frame(pts_data [VarRain])

for (i in 1:length(names)) {
  data_Class = data.frame(matrix(NA, ncol=2, nrow=nrow(dataRain)))
  names(data_Class) = c("Disaster", "Var")
  data_Class$Disaster = dataRain[,c("Class")]
  data_Class$Var = dataRain[,i+1]
  t = t.test(Var ~ Disaster, data=data_Class)
  info = paste("t-test for mean \n mean no =", round(t$estimate[[1]],3), 
               "\n mean yes =", round(t$estimate[[2]],3), "\n p-value =", t$p.value)
  png(height=900, width=900, pointsize=15,
      file=paste("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Exploration/Rain-",i,".png", sep=""))
  p = ggplot(data_Class, aes(Var, fill=Disaster)) +
    geom_histogram(aes(y = ..density..), position='identity',
                   binwidth=diff(range(data_Class[data_Class$Disaster=="no",2]))/30) + 
    theme(legend.text=element_text(size=12), legend.position=c(0.9,0.9)) +
    scale_fill_manual(values=alpha(c("Turquoise", "LightCoral"), .5)) +
    labs(x=names_Hist[i]) + theme(axis.title=element_text(size=20), axis.text=element_text(size=15)) +
    geom_vline(xintercept=mean(data_Class[data_Class$Disaster=="no",2], na.rm=T), color="SteelBlue") +
    geom_vline(xintercept=mean(data_Class[data_Class$Disaster=="yes",2], na.rm=T), color="DeepPink") +
    labs(title=info) + theme(plot.title=element_text(size=rel(1.4)))
  plot(p)
  dev.off()
  print(i)
}


########################################### DE - All ---------------------------------
# Variav_All = c("Class",
#                "Rn0d14d","RainIn0d14d","RainIn1d14d","RainIn2d14d",
#                "Slope",
#                "Aspect","AspectD","AspectD4",
#                "Veg2010","VegDif","Suscep",
#                "MnI2010","MnIncDf",
#                "Sbn2010",
#                "SubnChn","SubnChn2cl",
#                "Dm2010","DmDif",
#                "PSEsg10","PSPav10","PSMF10","PSBL10","PEsgCA10",
#                "PReAlf10","PReFeAlf10","PReMaAlf10",
#                "PReFe10","PReMa10",
#                "HabDm10")
# Variav_AllsClass = Variav_All[-1]
# 
# data = data[,Variav_All]

sink("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Exploration/Data_Summary.txt")
summary(data)
sink()




########################################### DE - Histograms ---------------------------------
colours2=c("Turquoise", "LightCoral")
add.alpha <- function(colours2, alpha=1){
  if(missing(colours2))
    stop("Please provide a vector of colours.")
  apply(sapply(colours2, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}


# Variav_AllsClassDesag = c(Variav_AllsClass[1:7],"AspectD1","AspectD2","AspectD3","AspectD4","AspectD5","AspectD6","AspectD7","AspectD8","AspectD9",
                          # Variav_AllsClass[8],"AspectD4-Flat","AspectD4-N","AspectD4-E","AspectD4-S","AspectD4-W",
                          # Variav_AllsClass[9:11],"Suscep-1","Suscep-2","Suscep-3",
                          # Variav_AllsClass[12:14],"Sbn2010-0","Sbn2010-1",
                          # Variav_AllsClass[15],"SubnChn-0","SubnChn-1","SubnChn-2",
                          # Variav_AllsClass[16:28])
# T_result = data.frame(matrix(NA, ncol=10, nrow=length(Variav_AllsClassDesag)), row.names=Variav_AllsClassDesag)
T_result = data.frame(matrix(NA, ncol=10, nrow=0))
colnames(T_result) = c("Cat","PTSYes","PTSNo","PropYes","PropNo","MeanYes","MeanNo","t","df","pval")


## Continuous
Variav_C = c("Dm2010","DmDif","MnI2010","MnIncDf",
             "PSEsg10","PSPav10","PSMF10","PSBL10","PEsgCA10",
             "PReFe10","PReMa10","PReAlf10","PReFeAlf10","PReMaAlf10",
             "HabDm10",
             "Veg2010","VegDif","Slope","Aspect",
             "Ran0d1d","Ran0d7d","Rn0d21d","Rn0d28d","Rn0d60d","Rn0d120",
             "Rn0d14d","RainIn0d14d","RainIn1d14d","RainIn2d14d")

Variav_C_names = c("Households (2010) (in units)","Household Change (2010-1991) (in units)", 
                   "Mean Household Income (2010) (in R$ 0f 2018)", "Mean Household Income Change (2010-1991) (in R$ of 2018)", 
                   "Households without sewerage access (2010) (in %)","Households in unpeaved streets (2010) (in %)",
                   "Households in streets without storm sewer (curb) (2010) (in %)", "Households in streets without  storm sewer (grating) (2010) (in %)",
                   "Households in streets with open sewage (2010) (in %)",
                   "Female households' responsible (2010) (in %)","Male households' responsible (2010) (in %)",
                   "Literate households' responsible (2010) (in %)",
                   "Literate female households' responsible (2010) (in %)","Literate male households' responsible (2010) (in %)",
                   "People per household (2010)",
                   "Vegetation (in %)","Vegetation Change (2010-1992) (in %)", "Slope (in degrees)", "Aspect (in degrees, 0°=>N)",
                   "Antecedent Rainfall (day + previous day) (in mm)", "Antecedent Rainfall (day + 7 previous days) (in mm)",
                   "Antecedent Rainfall (day + 21 previous days) (in mm)","Antecedent Rainfall (day + 28 previous days) (in mm)",
                   "Antecedent Rainfall (day + 60 previous days) (in mm)","Antecedent Rainfall (day + 120 previous days) (in mm)",
                   "Antecedent Rainfall (day + 14 previous days) (in mm)", 
                   "Rainfall index - current day / 14 previous days (in %)","Rainfall index - previous day / 14 previous days (in %)",
                   "Rainfall index - 2 previous days / 14 previous days (in %)")

data_Hist = data[,Variav_C]
names_Hist = Variav_C_names

for (i in 1:ncol(data_Hist)) {
  Var = colnames(data_Hist[i])
  data_Hist_Class = data.frame(matrix(NA, ncol=2, nrow=nrow(data)))
  names(data_Hist_Class) = c("Disaster", "Var")
  data_Hist_Class$Disaster = data[,c("Class")]
  data_Hist_Class$Var = data_Hist[,i]
  data_Hist_Class = na.omit(data_Hist_Class)
  t = t.test(Var ~ Disaster, data=data_Hist_Class)
  info = paste("t-test for mean \n mean no =", round(t$estimate[[1]],3), 
               "\n mean yes =", round(t$estimate[[2]],3), "\n p-value =", t$p.value)
  png(height=900, width=900, pointsize=15,
      file=paste("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Exploration/DistHist",i,"-",colnames(data_Hist[i]),".png", sep=""))
  p = ggplot(data_Hist_Class, aes(Var, fill=Disaster)) +
    geom_histogram(aes(y = ..density..), position='identity',
                   binwidth=diff(range(data_Hist_Class[data_Hist_Class$Disaster=="0",2]))/30) + 
    theme(legend.text=element_text(size=12), legend.position=c(0.9,0.9)) +
    scale_fill_manual(values=alpha(c("Turquoise", "LightCoral"), .5)) +
    labs(x=names_Hist[i]) + theme(axis.title=element_text(size=20), axis.text=element_text(size=15)) +
    geom_vline(xintercept=mean(data_Hist_Class[data_Hist_Class$Disaster=="0",2], na.rm=T), color="SteelBlue") +
    geom_vline(xintercept=mean(data_Hist_Class[data_Hist_Class$Disaster=="1s",2], na.rm=T), color="DeepPink") +
    labs(title=info) + theme(plot.title=element_text(size=rel(1.4)))
  plot(p)
  dev.off()
  T_result[Var,"PTSYes"]  = nrow(data_Hist_Class [data_Hist_Class$Disaster=="1",])
  T_result[Var,"PropYes"]  = nrow(data_Hist_Class [data_Hist_Class$Disaster=="1",])/nrow(data_Hist_Class)
  T_result[Var,"PTSNo"]  = nrow(data_Hist_Class [data_Hist_Class$Disaster=="0",])
  T_result[Var,"PropNo"]  = nrow(data_Hist_Class [data_Hist_Class$Disaster=="0",])/nrow(data_Hist_Class)
  T_result[Var,"MeanYes"] = round(t$estimate[[2]],3)
  T_result[Var,"MeanNo"] = round(t$estimate[[1]],3)
  T_result[Var,"t"] = as.numeric(t$statistic)
  T_result[Var,"df"] = as.numeric(t$parameter)
  T_result[Var,"pval"] = as.numeric(t$p.value)
  print(i)
  }


## Subn2010
counts = table(data[,"Sbn2010"],data$Class)
counts = na.omit(counts)
counts_prop = prop.table(counts, 1)
cat=c("regular","subn in 2010")
t = prop.test(counts, correct=FALSE)
info = paste(t$method, "\n % of landslides locations in:    regular settl. =", round(100-((t$estimate[1]*100)),3), 
             "%    subn. settl. =", round(100-((t$estimate[2]*100)),3), 
             "%\n p-value =", t$p.value)
png(height=900, width=900, pointsize=15,
    file=("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Exploration/DistBar-1-Subn2010.png"))
barplot(t(counts_prop),
        xlab="Subnormal Settlement (2010)", 
        cex.lab=1.6, col=add.alpha(colours2,alpha=.5), border=NA, cex.axis=1.4, cex.names=1.4,
        legend = c("non-landslides locations","landslides locations"), beside=F, names.arg=cat)
title(main=info, cex=1.4, adj=0, font.main=1, line=1)
dev.off()
T_result["Sbn2010-0", "Cat"] = cat[1]
T_result["Sbn2010-1", "Cat"] = cat[2]
T_result["Sbn2010-0","PTSYes"]  = counts[1,2]
T_result["Sbn2010-1","PTSYes"]  = counts[2,2]
T_result["Sbn2010-0","PTSNo"]  = counts[1,1] 
T_result["Sbn2010-1","PTSNo"]  = counts[2,1]  
T_result["Sbn2010-0","PropYes"]  = counts_prop [1,2]
T_result["Sbn2010-1","PropYes"]  = counts_prop [2,2]
T_result["Sbn2010-0","PropNo"]  = counts_prop [1,1] 
T_result["Sbn2010-1","PropNo"]  = counts_prop [2,1]
T_result["Sbn2010","t"] = as.numeric(t$statistic)
T_result["Sbn2010","df"] = as.numeric(t$parameter)
T_result["Sbn2010","pval"] = as.numeric(t$p.value)

## SubnChn 3 classes
counts = table(data[,"SubnChn"],data$Class)
counts = na.omit(counts)
counts_prop = prop.table(counts, 1)
cat = c("no change","subn in 2010","subn in 1991")
t = prop.test(counts, correct=FALSE)
info = paste(t$method, "\n % of landslides locations in:    no change =", round(100-((t$estimate[1]*100)),3), 
             "%    subn in 2010 =", round(100-((t$estimate[2]*100)),3), 
             "%    subn in 1991 =", round(100-((t$estimate[3]*100)),3), 
             "%\n p-value =", t$p.value)
png(height=900, width=900, pointsize=15,
    file=("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Exploration/DistBar-2-SubnChn.png"))
barplot(t(counts_prop),
        xlab="Subnormal Settlement Change (1991-2010)", cex.lab=1.6, 
        col=add.alpha(colours2,alpha=.5), border=NA, cex.axis=1.4, cex.names=1.4,
        legend = c("non-landslides locations","landslides locations"), beside=F, names.arg=cat)
title(main=info, cex=1.4, adj=0, font.main=1, line=1)
dev.off()
T_result["SubnChn-0", "Cat"] = cat[1]
T_result["SubnChn-1", "Cat"] = cat[2]
T_result["SubnChn-2", "Cat"] = cat[3]
T_result["SubnChn-0","PTSYes"]  = counts[1,2]
T_result["SubnChn-1","PTSYes"]  = counts[2,2]
T_result["SubnChn-2","PTSYes"]  = counts[3,2]
T_result["SubnChn-0","PTSNo"]  = counts[1,1] 
T_result["SubnChn-1","PTSNo"]  = counts[2,1]  
T_result["SubnChn-2","PTSNo"]  = counts[3,1] 
T_result["SubnChn-0","PropYes"]  = counts_prop [1,2]
T_result["SubnChn-1","PropYes"]  = counts_prop [2,2]
T_result["SubnChn-2","PropYes"]  = counts_prop [3,2]
T_result["SubnChn-0","PropNo"]  = counts_prop [1,1] 
T_result["SubnChn-1","PropNo"]  = counts_prop [2,1]
T_result["SubnChn-2","PropNo"]  = counts_prop [3,1]
T_result["SubnChn","t"] = as.numeric(t$statistic)
T_result["SubnChn","df"] = as.numeric(t$parameter)
T_result["SubnChn","pval"] = as.numeric(t$p.value)


## SubnChn 2 classes
counts = table(data[,"SubnChn2cl"],data$Class)
counts = na.omit(counts)
counts_prop = prop.table(counts, 1)
cat = c("no change","change")
t = prop.test(counts, correct=FALSE)
info = paste(t$method, "\n % of landslides locations in:    no change =", round(100-((t$estimate[1]*100)),3), 
             "%    change =", round(100-((t$estimate[2]*100)),3), 
             "%\n p-value =", t$p.value)
png(height=900, width=900, pointsize=15,
    file=("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Exploration/DistBar-3-SubnChn2cl.png"))
barplot(t(counts_prop),
        xlab="Subnormal Settlement Change (1991-2010)", 
        cex.lab=1.6, col=add.alpha(colours2,alpha=.5), border=NA, cex.axis=1.4, cex.names=1.4,
        legend = c("non-landslides locations","landslides locations"), beside=F, names.arg=cat)
title(main=info, cex=1.4, adj=0, font.main=1, line=1)
dev.off()
T_result["SubnChn2cl-0", "Cat"] = cat[1]
T_result["SubnChn2cl-1", "Cat"] = cat[2]
T_result["SubnChn2cl-0","PTSYes"]  = counts[1,2]
T_result["SubnChn2cl-1","PTSYes"]  = counts[2,2]
T_result["SubnChn2cl-0","PTSNo"]  = counts[1,1] 
T_result["SubnChn2cl-1","PTSNo"]  = counts[2,1]  
T_result["SubnChn2cl-0","PropYes"]  = counts_prop [1,2]
T_result["SubnChn2cl-1","PropYes"]  = counts_prop [2,2]
T_result["SubnChn2cl-0","PropNo"]  = counts_prop [1,1] 
T_result["SubnChn2cl-1","PropNo"]  = counts_prop [2,1]
T_result["SubnChn2cl","t"] = as.numeric(t$statistic)
T_result["SubnChn2cl","df"] = as.numeric(t$parameter)
T_result["SubnChn2cl","pval"] = as.numeric(t$p.value)


## Suscep
counts = table(data[,"Suscep"],data$Class)
counts = na.omit(counts)
counts_prop = prop.table(counts, 1)
cat = c("Low", "Medium", "High")
t = prop.test(counts, correct=FALSE)
info = paste(t$method, "\n % of landslides locations in:    low =", round(100-((t$estimate[1]*100)),3), 
             "%    medium =", round(100-((t$estimate[2]*100)),3), 
             "%    high =", round(100-((t$estimate[3]*100)),3), 
             "%\n p-value =", t$p.value)
png(height=900, width=900, pointsize=15,
    file=("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Exploration/DistBar-4-Suscep.png"))
barplot(t(counts_prop),
        xlab="Mass-movement susceptibility", cex.lab=1.6, 
        col=add.alpha(colours2,alpha=.5), border=NA, cex.axis=1.4, cex.names=1.4,
        legend = c("non-landslides locations","landslides locations"), beside=F, names.arg=cat)
title(main=info, cex=1.4, adj=0, font.main=1, line=1)
dev.off()
T_result["Suscep-0", "Cat"] = cat[1]
T_result["Suscep-1", "Cat"] = cat[2]
T_result["Suscep-2", "Cat"] = cat[3]
T_result["Suscep-0","PTSYes"]  = counts[1,2]
T_result["Suscep-1","PTSYes"]  = counts[2,2]
T_result["Suscep-2","PTSYes"]  = counts[3,2]
T_result["Suscep-0","PTSNo"]  = counts[1,1] 
T_result["Suscep-1","PTSNo"]  = counts[2,1]  
T_result["Suscep-2","PTSNo"]  = counts[3,1] 
T_result["Suscep-0","PropYes"]  = counts_prop [1,2]
T_result["Suscep-1","PropYes"]  = counts_prop [2,2]
T_result["Suscep-2","PropYes"]  = counts_prop [3,2]
T_result["Suscep-0","PropNo"]  = counts_prop [1,1] 
T_result["Suscep-1","PropNo"]  = counts_prop [2,1]
T_result["Suscep-2","PropNo"]  = counts_prop [3,1]
T_result["Suscep","t"] = as.numeric(t$statistic)
T_result["Suscep","df"] = as.numeric(t$parameter)
T_result["Suscep","pval"] = as.numeric(t$p.value)


## AspectD4
counts = table(data[,"AspectD4"],data$Class)
counts = na.omit(counts)
counts_prop = prop.table(counts, 1)
cat = c("E","Flat","N","S","W")
t = prop.test(counts, correct=FALSE)
info = paste(t$method, "\n % of landslides locations in:    E =", round(100-((t$estimate[1]*100)),3), 
             "%    Flat =", round(100-((t$estimate[2]*100)),3), 
             "%    N =", round(100-((t$estimate[3]*100)),3), 
             "%    S =", round(100-((t$estimate[4]*100)),3), 
             "%    W =", round(100-((t$estimate[5]*100)),3), 
             "%\n p-value =", t$p.value)
png(height=900, width=900, pointsize=15,
    file=("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Exploration/DistBar-5-AspectD4.png"))
barplot(t(counts_prop),
        xlab="Aspect", cex.lab=1.6, 
        col=add.alpha(colours2,alpha=.5), border=NA, cex.axis=1.4, cex.names=1.4,
        legend = c("non-landslides locations","landslides locations"), beside=F, names.arg=cat)
title(main=info, cex=1.4, adj=0, font.main=1, line=1)
dev.off()
T_result["AspectD4-1", "Cat"] = cat[1]
T_result["AspectD4-2", "Cat"] = cat[2]
T_result["AspectD4-3", "Cat"] = cat[3]
T_result["AspectD4-4", "Cat"] = cat[4]
T_result["AspectD4-5", "Cat"] = cat[5]
T_result["AspectD4-1","PTSYes"]  = counts[1,2]
T_result["AspectD4-2","PTSYes"]  = counts[2,2]
T_result["AspectD4-3","PTSYes"]  = counts[3,2]
T_result["AspectD4-4","PTSYes"]  = counts[4,2]
T_result["AspectD4-5","PTSYes"]  = counts[5,2]
T_result["AspectD4-1","PTSNo"]  = counts[1,1] 
T_result["AspectD4-2","PTSNo"]  = counts[2,1]  
T_result["AspectD4-3","PTSNo"]  = counts[3,1] 
T_result["AspectD4-4","PTSNo"]  = counts[4,1]  
T_result["AspectD4-5","PTSNo"]  = counts[5,1] 
T_result["AspectD4-1","PropYes"]  = counts_prop [1,2]
T_result["AspectD4-2","PropYes"]  = counts_prop [2,2]
T_result["AspectD4-3","PropYes"]  = counts_prop [3,2]
T_result["AspectD4-4","PropYes"]  = counts_prop [4,2]
T_result["AspectD4-5","PropYes"]  = counts_prop [5,2]
T_result["AspectD4-1","PropNo"]  = counts_prop [1,1] 
T_result["AspectD4-2","PropNo"]  = counts_prop [2,1]
T_result["AspectD4-3","PropNo"]  = counts_prop [3,1]
T_result["AspectD4-4","PropNo"]  = counts_prop [4,1]
T_result["AspectD4-5","PropNo"]  = counts_prop [5,1]
T_result["AspectD4","t"] = as.numeric(t$statistic)
T_result["AspectD4","df"] = as.numeric(t$parameter)
T_result["AspectD4","pval"] = as.numeric(t$p.value)


# AspectDdummy e AspectD4dummy
Variav_D = c("AspectDN","AspectDNE","AspectDE","AspectDSE","AspectDS","AspectDSW","AspectDW","AspectDNW","AspectDFlat",
             "AspectD4N","AspectD4E","AspectD4S","AspectD4W")
Variav_D_names = c("Aspect-N","Aspect-NE","Aspect-E","Aspect-SE","Aspect-S","Aspect-SW","Aspect-W","Aspect-NW","Aspect-Flat",
                   "Aspect4D-N","Aspect4D-W","Aspect4D-S","Aspect4D-W")
for (d in 1:9) {
  dir = Variav_D[d]
  counts = table(data[,dir],data$Class)
  counts = na.omit(counts)
  counts_prop = prop.table(counts, 2)
  cat=c("other",dir)
  t = prop.test(counts, correct=FALSE)
  info = paste(t$method, "\n % of landslides locations in:    in other directions =",round((100-(t$estimate[1]*100)),3), 
               "%    in", dir, " =", round((100-(t$estimate[2]*100)),3), 
               "%\n p-value =", t$p.value)
  file = paste("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Exploration/DistBar-",d+5,"-",dir,".png",sep="")
  png(height=900, width=900, pointsize=15,
      file=file)
  barplot(t(counts_prop), 
          xlab=dir, cex.lab=1.6, col=add.alpha(colours2,alpha=.5), border=NA, cex.axis=1.4, cex.names=1.4,
          legend = c("non-landslides locations","landslides locations"), beside=F, names.arg=cat)
  title(main=info, cex=1.4, adj=0, font.main=1, line=1)
  dev.off()
  nom0 = paste(dir, "-0")
  nom1 = paste(dir, "-1")
  T_result[nom0, "Cat"] = cat[1]
  T_result[nom1, "Cat"] = cat[2]
  T_result[nom0,"PTSYes"]  = counts[1,2]
  T_result[nom1,"PTSYes"]  = counts[2,2]
  T_result[nom0,"PTSNo"]  = counts[1,1]
  T_result[nom1,"PTSNo"]  = counts[2,1]
  T_result[nom0,"PropYes"]  = counts_prop [1,2]
  T_result[nom1,"PropYes"]  = counts_prop [2,2]
  T_result[nom0,"PropNo"]  = counts_prop [1,1]
  T_result[nom1,"PropNo"]  = counts_prop [2,1]
  T_result[dir,"t"] = as.numeric(t$statistic)
  T_result[dir,"df"] = as.numeric(t$parameter)
  T_result[dir,"pval"] = as.numeric(t$p.value)
  print(d)
}

for (d in 10:13) {
  dir = Variav_D[d]
  counts = table(data[,dir],data$Class)
  counts = na.omit(counts)
  counts_prop = prop.table(counts, 2)
  cat=c("other",dir)
  t = prop.test(counts, correct=FALSE)
  info = paste(t$method, "\n % of landslides locations in:    in other directions =",round((100-(t$estimate[1]*100)),3),
               "%    in", dir, " =", round((100-(t$estimate[2]*100)),3), 
               "%\n p-value =", t$p.value)
  file = paste("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Exploration/DistBar-",d+5,"-",dir,".png",sep="")
  png(height=900, width=900, pointsize=15,
      file=file)
  barplot(t(counts_prop), 
          xlab=dir, cex.lab=1.6, col=add.alpha(colours2,alpha=.5), border=NA, cex.axis=1.4, cex.names=1.4,
          legend=c("non-landslides locations","landslides locations"), beside=F, names.arg=cat)
  title(main=info, cex=1.4, adj=0, font.main=1, line=1)
  dev.off()
  nom0 = paste(dir, "-0")
  nom1 = paste(dir, "-1")
  T_result[nom0, "Cat"] = cat[1]
  T_result[nom1, "Cat"] = cat[2]
  T_result[nom0,"PTSYes"]  = counts[1,2]
  T_result[nom1,"PTSYes"]  = counts[2,2]
  T_result[nom0,"PTSNo"]  = counts[1,1]
  T_result[nom1,"PTSNo"]  = counts[2,1]
  T_result[nom0,"PropYes"]  = counts_prop [1,2]
  T_result[nom1,"PropYes"]  = counts_prop [2,2]
  T_result[nom0,"PropNo"]  = counts_prop [1,1]
  T_result[nom1,"PropNo"]  = counts_prop [2,1]
  T_result[dir,"t"] = as.numeric(t$statistic)
  T_result[dir,"df"] = as.numeric(t$parameter)
  T_result[dir,"pval"] = as.numeric(t$p.value)
  print(d)
}

write.csv2 (T_result,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Exploration/T_result.csv" )



########################################### DE - Correlation ---------------------------------
library("Hmisc")
library("corrplot")
#http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software


## continuous variables
#https://geographyfieldwork.com/SpearmansRankCalculator.html
#H0: The The two variables are independent
#H1: The two variables are related.
#p-value menor que 0.05, eu rejeito H0 (ou seja, eu rejeito que não há correlação entre as variáveis - eu aceito que ha correlação)
#p-value maior que 0.05, eu aceito H0 (ou seja, eu aceito que não há correlação entre as variáveis)

#data_c = as.matrix(data[,Variav_C])

Var_UniPaper = c("Rn0d14d",
                 "Slope",
                 "Veg2010","VegDif",
                 "MnI2010","MnIncDf","PReAlf10",
                 "Dm2010","DmDif",
                 "PSPav10","PSMF10","PSBL10","PEsgCA10")
data_c = as.matrix(data[,Var_UniPaper])

data_c = na.omit(data_c)

corr_data_c = rcorr(data_c, type="spearman")
corr_data_c

info = c("Spearman correlation coefficient")
#nomefile = c("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Exploration/Corr_Data-Cont.png")
nomefile = c("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Exploration/Corr_DataUniPaper-Cont.png")
png(height=900, width=900, pointsize=15,file=nomefile)
corrplot(corr_data_c$r, type="upper", p.mat=corr_data_c$P, sig.level=0.05, insig="blank", addCoef.col="black", 
         tl.cex=1.2, tl.col="black", tl.srt=45, mar=c(0,0,7,0), number.cex= 7/ncol(data_c), diag=F)
title(main=info, cex=1.4, adj=0, font.main=1, line=-2)
dev.off()


## discrete variables
#https://www.r-bloggers.com/chi-squared-test/
 #H0: The The two variables are independent
 #H1: The two variables are related.
 #p-value menor que 0.05, eu rejeito H0 (ou seja, eu rejeito que não há correlação entre as variáveis - eu aceito que ha correlação)
 #p-value maior que 0.05, eu aceito H0 (ou seja, eu aceito que não há correlação entre as variáveis)
#http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r (gráfico)
library(corrplot)
 
################################################ Estat - UM (Univariable Model) ---------------------------------
rm(list=ls())
gc()

library(data.table)
library(lmtest) #lr test
library(pscl) #R2

rm(list=ls()[! ls() %in% c("pts_data")])
# pts_data = read.csv2 ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/DataAll_Train_PTS.csv", header=T)

Ano = 2010
Nboot = 1000
#Nboot = 2
Nsamples = 1000

data = as.data.frame(pts_data)

data$Sbn2010 = as.factor(data$Sbn2010)

data$SubnChn [data$SubnChn==2] = 0
data$SubnChn = as.factor(data$SubnChn)

data$Suscep2e3 [data$Suscep==1] = 0
data$Suscep2e3 [data$Suscep==2] = 1
data$Suscep2e3 [data$Suscep==3] = 1
data$Suscep2e3 = as.factor(data$Suscep2e3)

data$Suscep3 [data$Suscep==1] = 0
data$Suscep3 [data$Suscep==2] = 0
data$Suscep3 [data$Suscep==3] = 1
data$Suscep3 = as.factor(data$Suscep3)

data$Suscep = as.factor(data$Suscep)
unique(data$Suscep)

tab_class = as.data.frame(data$Class)
names(tab_class) = c("Class")
tab_class$Cl = NA
tab_class$Cl[tab_class$Class=="yes"] = 1
tab_class$Cl[tab_class$Class=="no"] = 0
data$Class = as.factor(tab_class$Cl)
rm(tab_class)

data$AspectD = NA
data$AspectD [is.na(data$Aspect)] = "Flat"
data$AspectD [data$Aspect==0] = "N"
data$AspectD [data$Aspect>=(337.5)| data$Aspect<(22.5)] = "N"
data$AspectD [data$Aspect>=(22.5) & data$Aspect<(67.5)] = "NE"
data$AspectD [data$Aspect>=(67.5) & data$Aspect<(112.5)] = "E"
data$AspectD [data$Aspect>=(112.5) & data$Aspect<(157.5)] = "SE"
data$AspectD [data$Aspect>=(157.5) & data$Aspect<(202.5)] = "S"
data$AspectD [data$Aspect>=(202.5) & data$Aspect<(247.5)] = "SW"
data$AspectD [data$Aspect>=(247.5) & data$Aspect<(292.5)] = "W"
data$AspectD [data$Aspect>=(292.5) & data$Aspect<(337.5)] = "NW"
data$AspectD = as.factor(data$AspectD)
unique(data$AspectD)

data$AspectDN = 0
data$AspectDN [data$AspectD=="N"] = 1
data$AspectDN = as.factor(data$AspectDN)
data$AspectDNE = 0
data$AspectDNE [data$AspectD=="NE"] = 1
data$AspectDNE = as.factor(data$AspectDNE)
data$AspectDE = 0
data$AspectDE [data$AspectD=="E"] = 1
data$AspectDE = as.factor(data$AspectDE)
data$AspectDSE = 0
data$AspectDSE [data$AspectD=="SE"] = 1
data$AspectDSE = as.factor(data$AspectDSE)
data$AspectDS = 0
data$AspectDS [data$AspectD=="S"] = 1
data$AspectDS = as.factor(data$AspectDS)
data$AspectDSW = 0
data$AspectDSW [data$AspectD=="SW"] = 1
data$AspectDSW = as.factor(data$AspectDSW)
data$AspectDW = 0
data$AspectDW [data$AspectD=="W"] = 1
data$AspectDW = as.factor(data$AspectDW)
data$AspectDNW = 0
data$AspectDNW [data$AspectD=="NW"] = 1
data$AspectDNW = as.factor(data$AspectDNW)
data$AspectDFlat = 0
data$AspectDFlat [data$AspectD=="Flat"] = 1
data$AspectDFlat = as.factor(data$AspectDFlat)

data$AspectD4 = NA
data$AspectD4 [is.na(data$Aspect)] = "0" #"Flat"
data$AspectD4 [data$Aspect==0] =  "1" #"N"
data$AspectD4 [data$Aspect>=(315)| data$Aspect<(45)] =  "1" #"N"
data$AspectD4 [data$Aspect>=(45) & data$Aspect<(135)] =  "2" #"E"
data$AspectD4 [data$Aspect>=(135) & data$Aspect<(225)] =  "3" #"S"
data$AspectD4 [data$Aspect>=(225) & data$Aspect<(315)] =  "4" #"W"
data$AspectD4 = as.factor(data$AspectD4)
unique(data$AspectD4)

data$AspectD4Flat = 0
data$AspectD4Flat [data$AspectD=="Flat"] = 1
data$AspectD4Flat = as.factor(data$AspectD4Flat)
data$AspectD4N = 0
data$AspectD4N [data$AspectD=="N"] = 1
data$AspectD4N = as.factor(data$AspectD4N)
data$AspectD4E = 0
data$AspectD4E [data$AspectD=="E"] = 1
data$AspectD4E = as.factor(data$AspectD4E)
data$AspectD4S = 0
data$AspectD4S [data$AspectD=="S"] = 1
data$AspectD4S = as.factor(data$AspectD4S)
data$AspectD4W = 0
data$AspectD4W [data$AspectD=="W"] = 1
data$AspectD4W = as.factor(data$AspectD4W)

data = droplevels(data)

# Variav_All = c("ID","Class",
#                "Rn0d14d","RainIn0d14d","RainIn1d14d","RainIn2d14d",
#                "Slope",
#                "Aspect","AspectD4",
#                "Veg2010","VegDif","Suscep",
#                "MnI2010","MnIncDf",
#                "Sbn2010","SubnChn",
#                "Dm2010","DmDif",
#                "PSEsg10","PSPav10","PSMF10","PSBL10","PEsgCA10",
#                "PReAlf10","PReFeAlf10","PReMaAlf10",
#                "PReFe10","PReMa10",
#                "HabDm10")
# Variav_AllsClass = Variav_All[-1]
# 
# data = data[,Variav_All]




########################################### UM - Continuous variables and discrete variables with 2 classes ---------------------------------
rm(list=ls()[! ls() %in% c("pts_data", "data")])

Ano = 2010
Nsamples = 1000
Nboot = 1000
#Nboot = 2

## Continuous variables and discrete variables with 2 classes
# Variable selection [v] and Bootstrap to model calculation [b]
#Model parameters: Coefficient (intercept&parameter); std. error (intercept&parameter); p-value (intercept&parameter)[association between variable and response]
#Statistical Tests for Individual Predictors: Likelihood Ratio Test [significance of the addition of new term to the model] 
#Goodness of Fit: R2 (Nagelkerke R^2 index) from lrm (rms package)
VarEstat_Cont = c("coef","coef.se","coef.p-val", 
                  "LRchi2","LRchi2.pvalue",
                  "r2Nagelkerke")

Var_Cont = c("ID", "Class",
             "Rn0d14d","RainIn0d14d","RainIn1d14d","RainIn2d14d",
             "Slope",
             "Aspect",
             "Veg2010","VegDif",
             "MnI2010","MnIncDf",
             "Sbn2010","SubnChn",
             "Dm2010","DmDif",
             "PSEsg10","PSPav10","PSMF10","PSBL10","PEsgCA10",
             "PReAlf10","PReFeAlf10","PReMaAlf10",
             "PReFe10","PReMa10",
             "HabDm10")
nVar_Cont = length(Var_Cont)-2

data_Cont = data[,Var_Cont]
data_Cont = data.table(data[Var_Cont])
setkeyv(data_Cont,c("Class"))

Model_Cont_Samples_train = data.frame(Boot=1:Nboot, Variav=NA, Samples=NA)

Model_Cont_coef = data.frame(matrix(NA, ncol=Nboot, nrow=nVar_Cont), row.names=c(Var_Cont[-(1:2)]))
colnames(Model_Cont_coef) = paste("Boot",1:Nboot, sep=" ")
Model_Cont_coef.se = Model_Cont_coef
Model_Cont_coef.pvalue = Model_Cont_coef
Model_Cont_intercept = Model_Cont_coef
Model_Cont_intercept.se = Model_Cont_coef
Model_Cont_intercept.pvalue = Model_Cont_coef
Model_Cont_LRchi2 = Model_Cont_coef
Model_Cont_LRchi2.pvalue = Model_Cont_coef
Model_Cont_r2Nagelkerke = Model_Cont_coef
Model_Cont_aic = Model_Cont_coef

v=1
b=1
for (v in (1:nVar_Cont)) {
  cols = c("ID", "Class",Var_Cont[2+v])
  tmp_Contv = data_Cont[,cols,with=F]
  tmp_Contv = na.omit(tmp_Contv)
  for (b in (1:Nboot)) {
    tmp_Cont = rbind((tmp_Contv[tmp_Contv$Class==1,][sample(nrow(tmp_Contv[tmp_Contv$Class==1,]), Nsamples, replace=F),]),
                   (tmp_Contv[tmp_Contv$Class==0,][sample(nrow(tmp_Contv[tmp_Contv$Class==0,]), Nsamples, replace=F),]))
    Model_Cont_Samples_train[b,2] = Var_Cont[2+v]
    Model_Cont_Samples_train[b,3] = paste(as.vector(tmp_Cont$ID),collapse="-")
    tmp_Cont = tmp_Cont[,-1]
    m_Cont = glm(formula=Class ~ ., family=binomial(link="logit"), data=tmp_Cont)
    Model_Cont_coef[v,b] = as.numeric(coef(summary(m_Cont))[2,1])
    Model_Cont_coef.se[v,b] = as.numeric(coef(summary(m_Cont))[2,2])
    Model_Cont_coef.pvalue[v,b] = as.numeric(coef(summary(m_Cont))[2,4])
    Model_Cont_intercept[v,b] = as.numeric(coef(summary(m_Cont))[1,1])
    Model_Cont_intercept.se[v,b] = as.numeric(coef(summary(m_Cont))[1,2])
    Model_Cont_intercept.pvalue[v,b] = as.numeric(coef(summary(m_Cont))[1,4])
    tmp_Cont_lrtest = lrtest(m_Cont) 
    Model_Cont_LRchi2[v,b] = as.numeric(tmp_Cont_lrtest[2,4])
    Model_Cont_LRchi2.pvalue[v,b] = as.numeric(tmp_Cont_lrtest[2,5])
    tmp_Cont_pR2 = pR2(m_Cont)
    Model_Cont_r2Nagelkerke[v,b] = as.numeric(tmp_Cont_pR2[6])
    Model_Cont_aic[v,b] = as.numeric(m_Cont$aic)
    print(paste("boot =", b, "- variav =", Var_Cont[2+v]))
    }
  }

write.csv2 (Model_Cont_Samples_train,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Cont_Samples_train.csv")
write.csv2 (Model_Cont_coef,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Cont_coef.csv")
write.csv2 (Model_Cont_coef.se,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Cont_coef.se.csv")
write.csv2 (Model_Cont_coef.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Cont_coef.pvalue.csv")
write.csv2 (Model_Cont_intercept,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Cont_intercept.csv")
write.csv2 (Model_Cont_intercept.se,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Cont_intercept.se.csv")
write.csv2 (Model_Cont_intercept.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Cont_interceptf.pvalue.csv")
write.csv2 (Model_Cont_LRchi2,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Cont_LRchi2.csv")
write.csv2 (Model_Cont_LRchi2.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Cont_LRchi2.pvalue.csv")
write.csv2 (Model_Cont_r2Nagelkerke,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Cont_r2Nagelkerke.csv")
write.csv2 (Model_Cont_aic,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Cont_aic.csv")

VarEstat_ContAll = c("mean.coef_param","CIinf mean.coef_param","CIsup coef_param",
                  "mean.coef.p-value","CIinf mean.coef.p-value","CIsup mean.coef.p-value",
                  "mean.LL0","CIinf mean.LL0","CIsup mean.LL0",
                  "mean.LLMx","CIinf mean.LLMx","CIsup mean.LLMx",
                  "mean.LRchi2_MXm0","CIinf mean.LRchi2_MXm0","CIsup mean_MXm0.LRchi2",
                  "mean.LRchi2_MXm0.pvalue","CIinf mean.LRchi2_MXm0.pvalue","CIsup mean_MXm0.LRchi2.pvalue",
                  "mean.r2Nagelkerke","CIinf mean.r2Nagelkerke", "CIsup mean.r2Nagelkerke",
                  "mean.aic","CIinf mean.aic", "CIsup mean.aic")

Model_ContAll = data.frame(matrix(NA, ncol=length(VarEstat_ContAll), nrow=nVar_Cont))
colnames(Model_ContAll) = c(VarEstat_ContAll)
rownames(Model_ContAll) = Var_Cont[-(1:2)]

for (v in 1:nVar_Cont) {
  variv = Var_Cont[v+2]
  Model_ContAll[v,1] = mean(as.numeric(Model_Cont_coef[v,]))
  Model_ContAll[v,2] = mean(as.numeric(Model_Cont_coef[v,])) - (qnorm(0.975)*((sd(as.numeric(Model_Cont_coef[v,])))/sqrt(Nboot)))
  Model_ContAll[v,3] = mean(as.numeric(Model_Cont_coef[v,])) + (qnorm(0.975)*((sd(as.numeric(Model_Cont_coef[v,])))/sqrt(Nboot)))
  Model_ContAll[v,4] = mean(as.numeric(Model_Cont_coef.pvalue[v,]))
  Model_ContAll[v,5] = mean(as.numeric(Model_Cont_coef.pvalue[v,])) - (qnorm(0.975)*((sd(as.numeric(Model_Cont_coef.pvalue[v,])))/sqrt(Nboot)))
  Model_ContAll[v,6] = mean(as.numeric(Model_Cont_coef.pvalue[v,])) + (qnorm(0.975)*((sd(as.numeric(Model_Cont_coef.pvalue[v,])))/sqrt(Nboot)))
  Model_ContAll[v,7] = NA
  Model_ContAll[v,8] = NA
  Model_ContAll[v,9] = NA
  Model_ContAll[v,10] = NA
  Model_ContAll[v,11] = NA
  Model_ContAll[v,12] = NA
  Model_ContAll[v,13] = mean(as.numeric(Model_Cont_LRchi2[v,]))
  Model_ContAll[v,14] = mean(as.numeric(Model_Cont_LRchi2[v,])) - (qnorm(0.975)*((sd(as.numeric(Model_Cont_LRchi2[v,])))/sqrt(Nboot)))
  Model_ContAll[v,15] = mean(as.numeric(Model_Cont_LRchi2[v,])) + (qnorm(0.975)*((sd(as.numeric(Model_Cont_LRchi2[v,])))/sqrt(Nboot)))
  Model_ContAll[v,16] = mean(as.numeric(Model_Cont_LRchi2.pvalue[v,]))
  Model_ContAll[v,17] = mean(as.numeric(Model_Cont_LRchi2.pvalue[v,])) - (qnorm(0.975)*((sd(as.numeric(Model_Cont_LRchi2.pvalue[v,])))/sqrt(Nboot)))
  Model_ContAll[v,18] = mean(as.numeric(Model_Cont_LRchi2.pvalue[v,])) + (qnorm(0.975)*((sd(as.numeric(Model_Cont_LRchi2.pvalue[v,])))/sqrt(Nboot)))
  Model_ContAll[v,19] = mean(as.numeric(Model_Cont_r2Nagelkerke[v,]))
  Model_ContAll[v,20] = mean(as.numeric(Model_Cont_r2Nagelkerke[v,])) - (qnorm(0.975)*((sd(as.numeric(Model_Cont_r2Nagelkerke[v,])))/sqrt(Nboot)))
  Model_ContAll[v,21] = mean(as.numeric(Model_Cont_r2Nagelkerke[v,])) + (qnorm(0.975)*((sd(as.numeric(Model_Cont_r2Nagelkerke[v,])))/sqrt(Nboot)))
  Model_ContAll[v,22] = mean(as.numeric(Model_Cont_aic[v,]))
  Model_ContAll[v,23] = mean(as.numeric(Model_Cont_aic[v,])) - (qnorm(0.975)*((sd(as.numeric(Model_Cont_aic[v,])))/sqrt(Nboot)))
  Model_ContAll[v,24] = mean(as.numeric(Model_Cont_aic[v,])) + (qnorm(0.975)*((sd(as.numeric(Model_Cont_aic[v,])))/sqrt(Nboot)))
  print(v)
}

write.csv2 (Model_ContAll,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_2010_Cont.csv" )

Model_Cont_intercept = read.csv2 ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Cont_intercept.csv", header=T)
rownames (Model_Cont_intercept) = Model_Cont_intercept$X
Model_Cont_intercept = Model_Cont_intercept[,-1]
VarEstat_ContAll_intercept = c("mean.coef_param","CIinf mean.coef_param","CIsup coef_param")
Model_ContAll_intercept = data.frame(matrix(NA, ncol=length(VarEstat_ContAll_intercept), nrow=(nVar_Cont)))
colnames(Model_ContAll_intercept) = c(VarEstat_ContAll_intercept)
rown = Var_Cont[-(1:2)]
rownames(Model_ContAll_intercept) = rown
for (vdes in 1:length(rown)) {
    varDesagNom = rown[vdes]
    Model_ContAll_intercept[varDesagNom,1] = mean(as.numeric(Model_Cont_intercept[varDesagNom,]))
    Model_ContAll_intercept[varDesagNom,2] = mean(as.numeric(Model_Cont_intercept[varDesagNom,])) - (qnorm(0.975)*sd(as.numeric(Model_Cont_intercept[varDesagNom,]))/sqrt(Nboot))
    Model_ContAll_intercept[varDesagNom,3] = mean(as.numeric(Model_Cont_intercept[varDesagNom,])) + (qnorm(0.975)*sd(as.numeric(Model_Cont_intercept[varDesagNom,]))/sqrt(Nboot))
    print(varDesagNom)
  }
write.csv2 (Model_ContAll_intercept,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_2010_Cont_intercept.csv" )





########################################### UM - Discrete variables Suscep ---------------------------------
rm(list=ls()[! ls() %in% c("pts_data", "data")])

Ano = 2010
Nsamples = 1000
Nboot = 1000
#Nboot = 2

#Model parameters: Coefficient (intercept&parameter); std. error (intercept&parameter); p-value (intercept&parameter)[association between variable and response]
#Statistical Tests for Individual Predictors: Likelihood Ratio Test [significance of the addition of new term to the model] 
#Goodness of Fit: R2 (Nagelkerke R^2 index) from lrm (rms package)
VarEstat_Disc = c("coef","coef.se","coef.p-val", 
                  "LRchi2","LRchi2.pvalue",
                  "r2Nagelkerke")

Var_Disc = c("ID", "Class","Suscep")
Var_DiscDesag = c("ID","Class","Suscep2","Suscep3")
nVar_Disc = length(Var_Disc) - 2
nVar_DiscDesag = length(Var_DiscDesag) - 2

data_Disc = data.table(data[Var_Disc])
setkeyv(data_Disc,c("Class"))

Model_Disc_Samples_train = data.frame(Boot=1:Nboot, Variav=NA, Samples=NA)

Model_Disc_coef = data.frame(matrix(NA, ncol=Nboot, nrow=nVar_DiscDesag), row.names=c(Var_DiscDesag[-(1:2)]))
colnames(Model_Disc_coef) = paste("Boot",1:Nboot, sep=" ")
Model_Disc_coef.se = Model_Disc_coef
Model_Disc_coef.pvalue = Model_Disc_coef

Model_Disc_intercept = data.frame(matrix(NA, ncol=Nboot, nrow=nVar_Disc), row.names=c(Var_Disc[-(1:2)]))
colnames(Model_Disc_intercept) = paste("Boot",1:Nboot, sep=" ")
Model_Disc_intercept.se = Model_Disc_intercept
Model_Disc_intercept.pvalue = Model_Disc_intercept

Model_Disc_LRchi2 = Model_Disc_intercept
Model_Disc_LRchi2.pvalue = Model_Disc_intercept
Model_Disc_r2Nagelkerke = Model_Disc_intercept
Model_Disc_aic = Model_Disc_intercept

# v=1
# b=1
for (v in (1:nVar_Disc)) {
  cols = c("ID", "Class",Var_Disc[2+v])
  tmp_Discv = data_Disc[,cols,with=F]
  tmp_Discv = na.omit(tmp_Discv)
  for (b in (1:Nboot)) {
    tmp_Disc = rbind((tmp_Discv[tmp_Discv$Class==1,][sample(nrow(tmp_Discv[tmp_Discv$Class==1,]), Nsamples, replace=F),]),
                    (tmp_Discv[tmp_Discv$Class==0,][sample(nrow(tmp_Discv[tmp_Discv$Class==0,]), Nsamples, replace=F),]))
    Model_Disc_Samples_train[b,2] = Var_Disc[2+v]
    Model_Disc_Samples_train[b,3] = paste(as.vector(tmp_Disc$ID),collapse="-")
    tmp_Disc = tmp_Disc[,-1]
    m_Disc = glm(formula=Class ~ ., family=binomial(link="logit"), data=tmp_Disc)
    tmp_Disc_coef = as.data.frame(coef(summary(m_Disc))[,1])
    for (p in (1:(nrow(tmp_Disc_coef)))) {Model_Disc_coef[rownames(tmp_Disc_coef)[p],b] = tmp_Disc_coef[rownames(tmp_Disc_coef)[p],1]}
    tmp_Disc_coef.se = as.data.frame(coef(summary(m_Disc))[,2])
    for (p in (1:(nrow(tmp_Disc_coef.se)))) {Model_Disc_coef.se[rownames(tmp_Disc_coef.se)[p],b] = tmp_Disc_coef.se[rownames(tmp_Disc_coef.se)[p],1]}
    tmp_Disc_coef.pvalue = as.data.frame(coef(summary(m_Disc))[,4])
    for (p in (1:nrow(tmp_Disc_coef.pvalue))) {Model_Disc_coef.pvalue[rownames(tmp_Disc_coef.pvalue)[p],b] = tmp_Disc_coef.pvalue[rownames(tmp_Disc_coef.pvalue)[p],1]}
    Model_Disc_intercept[v,b] = as.numeric(coef(summary(m_Disc))[1,1])
    Model_Disc_intercept.se[v,b] = as.numeric(coef(summary(m_Disc))[1,2])
    Model_Disc_intercept.pvalue[v,b] = as.numeric(coef(summary(m_Disc))[1,4])
    tmp_Disc_lrtest = lrtest(m_Disc) 
    Model_Disc_LRchi2[v,b] = as.numeric(tmp_Disc_lrtest[2,4])
    Model_Disc_LRchi2.pvalue[v,b] = as.numeric(tmp_Disc_lrtest[2,5])
    tmp_Disc_pR2 = pR2(m_Disc)
    Model_Disc_r2Nagelkerke[v,b] = as.numeric(tmp_Disc_pR2[6])
    Model_Disc_aic[v,b] = as.numeric(m_Disc$aic)
    print(paste("boot =", b, "- variav =", Var_Disc[2+v]))
    }
}

write.csv2 (Model_Disc_Samples_train,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_Samples_train.csv")
write.csv2 (Model_Disc_coef,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_coef.csv")
write.csv2 (Model_Disc_coef.se,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_coef.se.csv")
write.csv2 (Model_Disc_coef.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_coef.pvalue.csv")
write.csv2 (Model_Disc_intercept,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_intercept.csv")
write.csv2 (Model_Disc_intercept.se,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_intercept.se.csv")
write.csv2 (Model_Disc_intercept.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_interceptf.pvalue.csv")
write.csv2 (Model_Disc_LRchi2,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_LRchi2.csv")
write.csv2 (Model_Disc_LRchi2.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_LRchi2.pvalue.csv")
write.csv2 (Model_Disc_r2Nagelkerke,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_r2Nagelkerke.csv")
write.csv2 (Model_Disc_aic,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_aic.csv")

VarEstat_DiscAll = c("mean.coef_param","CIinf mean.coef_param","CIsup coef_param",
                     "mean.coef.p-value","CIinf mean.coef.p-value","CIsup mean.coef.p-value",
                     "mean.LL0","CIinf mean.LL0","CIsup mean.LL0",
                     "mean.LLMx","CIinf mean.LLMx","CIsup mean.LLMx",
                     "mean.LRchi2_MXm0","CIinf mean.LRchi2_MXm0","CIsup mean_MXm0.LRchi2",
                     "mean.LRchi2_MXm0.pvalue","CIinf mean.LRchi2_MXm0.pvalue","CIsup mean_MXm0.LRchi2.pvalue",
                     "mean.r2Nagelkerke","CIinf mean.r2Nagelkerke", "CIsup mean.r2Nagelkerke",
                     "mean.aic","CIinf mean.aic", "CIsup mean.aic")

Model_DiscAll = data.frame(matrix(NA, ncol=length(VarEstat_DiscAll), nrow=(1+nVar_DiscDesag+nVar_Disc)))
colnames(Model_DiscAll) = c(VarEstat_DiscAll)
rown = c("(Intercept)",Var_DiscDesag[3:4],Var_Disc[3])
rownames(Model_DiscAll) = rown

for (v in 1:nVar_Disc) {
  variv = Var_Disc[v+2]
  for (vdes in 1:length(rown)) {
    varDesagNom = rown[vdes]
    Model_DiscAll[varDesagNom,1] = mean(as.numeric(Model_Disc_coef[varDesagNom,]))
    Model_DiscAll[varDesagNom,2] = mean(as.numeric(Model_Disc_coef[varDesagNom,])) - (qnorm(0.975)*sd(as.numeric(Model_Disc_coef[varDesagNom,]))/sqrt(Nboot))
    Model_DiscAll[varDesagNom,3] = mean(as.numeric(Model_Disc_coef[varDesagNom,])) + (qnorm(0.975)*sd(as.numeric(Model_Disc_coef[varDesagNom,]))/sqrt(Nboot))
    Model_DiscAll[varDesagNom,4] = mean(as.numeric(Model_Disc_coef.pvalue[varDesagNom,]))
    Model_DiscAll[varDesagNom,5] = mean(as.numeric(Model_Disc_coef.pvalue[varDesagNom,])) - (qnorm(0.975)*sd(as.numeric(Model_Disc_coef.pvalue[varDesagNom,]))/sqrt(Nboot))
    Model_DiscAll[varDesagNom,6] = mean(as.numeric(Model_Disc_coef.pvalue[varDesagNom,])) + (qnorm(0.975)*sd(as.numeric(Model_Disc_coef.pvalue[varDesagNom,]))/sqrt(Nboot))
    print(varDesagNom)
  }
  Model_DiscAll[variv,13] = mean(as.numeric(Model_Disc_LRchi2[variv,]))
  Model_DiscAll[variv,14] = mean(as.numeric(Model_Disc_LRchi2[variv,]) - (qnorm(0.975)*((sd(as.numeric(Model_Disc_LRchi2[variv,])))/sqrt(Nboot))))
  Model_DiscAll[variv,15] = mean(as.numeric(Model_Disc_LRchi2[variv,]) + (qnorm(0.975)*((sd(as.numeric(Model_Disc_LRchi2[variv,])))/sqrt(Nboot))))
  Model_DiscAll[variv,16] = mean(as.numeric(Model_Disc_LRchi2.pvalue[variv,]))
  Model_DiscAll[variv,17] = mean(as.numeric(Model_Disc_LRchi2.pvalue[variv,])) - (qnorm(0.975)*sd(as.numeric(Model_Disc_LRchi2.pvalue[variv,]))/sqrt(Nboot))
  Model_DiscAll[variv,18] = mean(as.numeric(Model_Disc_LRchi2.pvalue[variv,])) + (qnorm(0.975)*sd(as.numeric(Model_Disc_LRchi2.pvalue[variv,]))/sqrt(Nboot))
  Model_DiscAll[variv,19] = mean(as.numeric(Model_Disc_r2Nagelkerke[variv,]))
  Model_DiscAll[variv,20] = mean(as.numeric(Model_Disc_r2Nagelkerke[variv,])) - (qnorm(0.975)*sd(as.numeric(Model_Disc_r2Nagelkerke[variv,]))/sqrt(Nboot))
  Model_DiscAll[variv,21] = mean(as.numeric(Model_Disc_r2Nagelkerke[variv,])) + (qnorm(0.975)*sd(as.numeric(Model_Disc_r2Nagelkerke[variv,]))/sqrt(Nboot))
  Model_DiscAll[variv,22] = mean(as.numeric(Model_Disc_aic[variv,]))
  Model_DiscAll[variv,23] = mean(as.numeric(Model_Disc_aic[variv,])) - (qnorm(0.975)*((sd(as.numeric(Model_Disc_aic[variv,])))/sqrt(Nboot)))
  Model_DiscAll[variv,24] = mean(as.numeric(Model_Disc_aic[variv,])) + (qnorm(0.975)*((sd(as.numeric(Model_Disc_aic[variv,])))/sqrt(Nboot)))
  
  print(variv)
}

write.csv2 (Model_DiscAll,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_2010_Disc.csv" )




########################################### UM - Discrete variables AspectD4 ---------------------------------
rm(list=ls()[! ls() %in% c("pts_data", "data")])

Ano = 2010
Nsamples = 1000
Nboot = 1000
#Nboot = 2

#Model parameters: Coefficient (intercept&parameter); std. error (intercept&parameter); p-value (intercept&parameter)[association between variable and response]
#Statistical Tests for Individual Predictors: Likelihood Ratio Test [significance of the addition of new term to the model] 
#Goodness of Fit: R2 (Nagelkerke R^2 index) from lrm (rms package)
VarEstat_Disc = c("coef","coef.se","coef.p-val", 
                  "LRchi2","LRchi2.pvalue",
                  "r2Nagelkerke")

Var_Disc4cl = c("ID", "Class","AspectD4")
Var_Disc4clDesag = c("ID", "Class","AspectD40", "AspectD41", "AspectD42", "AspectD43", "AspectD44")
nVar_Disc4cl = length(Var_Disc4cl) - 2
nVar_Disc4clDesag = length(Var_Disc4clDesag) - 2

data_Disc4cl = data.table(data[Var_Disc4cl])
setkeyv(data_Disc4cl,c("Class"))

Model_Disc4cl_Samples_train = data.frame(Boot=1:Nboot, Variav=NA, Samples=NA)

Model_Disc4cl_coef = data.frame(matrix(NA, ncol=Nboot, nrow=nVar_Disc4clDesag), row.names=c(Var_Disc4clDesag[-(1:2)]))
colnames(Model_Disc4cl_coef) = paste("Boot",1:Nboot, sep=" ")
Model_Disc4cl_coef.se = Model_Disc4cl_coef
Model_Disc4cl_coef.pvalue = Model_Disc4cl_coef

Model_Disc4cl_intercept = data.frame(matrix(NA, ncol=Nboot, nrow=nVar_Disc4cl), row.names=c(Var_Disc4cl[-(1:2)]))
colnames(Model_Disc4cl_intercept) = paste("Boot",1:Nboot, sep=" ")
Model_Disc4cl_intercept.se = Model_Disc4cl_intercept
Model_Disc4cl_intercept.pvalue = Model_Disc4cl_intercept

Model_Disc4cl_LRchi2 = Model_Disc4cl_intercept
Model_Disc4cl_LRchi2.pvalue = Model_Disc4cl_intercept
Model_Disc4cl_r2Nagelkerke = Model_Disc4cl_intercept
Model_Disc4cl_aic = Model_Disc4cl_intercept

# v=1
# b=1
for (v in (1:nVar_Disc4cl)) {
  cols = c("ID", "Class",Var_Disc4cl[2+v])
  tmp_Disc4clv = data_Disc4cl[,cols,with=F]
  tmp_Disc4clv = na.omit(tmp_Disc4clv)
  for (b in (1:Nboot)) {
    tmp_Disc4cl = rbind((tmp_Disc4clv[tmp_Disc4clv$Class==1,][sample(nrow(tmp_Disc4clv[tmp_Disc4clv$Class==1,]), Nsamples, replace=F),]),
                     (tmp_Disc4clv[tmp_Disc4clv$Class==0,][sample(nrow(tmp_Disc4clv[tmp_Disc4clv$Class==0,]), Nsamples, replace=F),]))
    Model_Disc4cl_Samples_train[b,2] = Var_Disc4cl[2+v]
    Model_Disc4cl_Samples_train[b,3] = paste(as.vector(tmp_Disc4cl$ID),collapse="-")
    tmp_Disc4cl = tmp_Disc4cl[,-1]
    m_Disc4cl = glm(formula=Class ~ ., family=binomial(link="logit"), data=tmp_Disc4cl)
    tmp_Disc4cl_coef = as.data.frame(coef(summary(m_Disc4cl))[,1])
    for (p in (1:(nrow(tmp_Disc4cl_coef)))) {Model_Disc4cl_coef[rownames(tmp_Disc4cl_coef)[p],b] = tmp_Disc4cl_coef[rownames(tmp_Disc4cl_coef)[p],1]}
    tmp_Disc4cl_coef.se = as.data.frame(coef(summary(m_Disc4cl))[,2])
    for (p in (1:(nrow(tmp_Disc4cl_coef.se)))) {Model_Disc4cl_coef.se[rownames(tmp_Disc4cl_coef.se)[p],b] = tmp_Disc4cl_coef.se[rownames(tmp_Disc4cl_coef.se)[p],1]}
    tmp_Disc4cl_coef.pvalue = as.data.frame(coef(summary(m_Disc4cl))[,4])
    for (p in (1:nrow(tmp_Disc4cl_coef.pvalue))) {Model_Disc4cl_coef.pvalue[rownames(tmp_Disc4cl_coef.pvalue)[p],b] = tmp_Disc4cl_coef.pvalue[rownames(tmp_Disc4cl_coef.pvalue)[p],1]}
    Model_Disc4cl_intercept[v,b] = as.numeric(coef(summary(m_Disc4cl))[1,1])
    Model_Disc4cl_intercept.se[v,b] = as.numeric(coef(summary(m_Disc4cl))[1,2])
    Model_Disc4cl_intercept.pvalue[v,b] = as.numeric(coef(summary(m_Disc4cl))[1,4])
    tmp_Disc4cl_lrtest = lrtest(m_Disc4cl) 
    Model_Disc4cl_LRchi2[v,b] = as.numeric(tmp_Disc4cl_lrtest[2,4])
    Model_Disc4cl_LRchi2.pvalue[v,b] = as.numeric(tmp_Disc4cl_lrtest[2,5])
    tmp_Disc4cl_pR2 = pR2(m_Disc4cl)
    Model_Disc4cl_r2Nagelkerke[v,b] = as.numeric(tmp_Disc4cl_pR2[6])
    Model_Disc4cl_aic[v,b] = as.numeric(m_Disc4cl$aic)
    print(paste("boot =", b, "- variav =", Var_Disc4cl[2+v]))
  }
}

write.csv2 (Model_Disc4cl_Samples_train,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_AspectD4_Samples_train.csv")
write.csv2 (Model_Disc4cl_coef,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_AspectD4_coef.csv")
write.csv2 (Model_Disc4cl_coef.se,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_AspectD4_coef.se.csv")
write.csv2 (Model_Disc4cl_coef.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_AspectD4_coef.pvalue.csv")
write.csv2 (Model_Disc4cl_intercept,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_AspectD4_intercept.csv")
write.csv2 (Model_Disc4cl_intercept.se,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_AspectD4_intercept.se.csv")
write.csv2 (Model_Disc4cl_intercept.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_AspectD4_interceptf.pvalue.csv")
write.csv2 (Model_Disc4cl_LRchi2,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_AspectD4_LRchi2.csv")
write.csv2 (Model_Disc4cl_LRchi2.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_AspectD4_LRchi2.pvalue.csv")
write.csv2 (Model_Disc4cl_r2Nagelkerke,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_AspectD4_r2Nagelkerke.csv")
write.csv2 (Model_Disc4cl_aic,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_AspectD4_aic.csv")


VarEstat_Disc4clAll = c("mean.coef_param","CIinf mean.coef_param","CIsup coef_param",
                        "mean.coef.p-value","CIinf mean.coef.p-value","CIsup mean.coef.p-value",
                        "mean.LL0","CIinf mean.LL0","CIsup mean.LL0",
                        "mean.LLMx","CIinf mean.LLMx","CIsup mean.LLMx",
                        "mean.LRchi2_MXm0","CIinf mean.LRchi2_MXm0","CIsup mean_MXm0.LRchi2",
                        "mean.LRchi2_MXm0.pvalue","CIinf mean.LRchi2_MXm0.pvalue","CIsup mean_MXm0.LRchi2.pvalue",
                        "mean.r2Nagelkerke","CIinf mean.r2Nagelkerke", "CIsup mean.r2Nagelkerke",
                        "mean.aic","CIinf mean.aic", "CIsup mean.aic")

Model_Disc4clAll = data.frame(matrix(NA, ncol=length(VarEstat_Disc4clAll), nrow=(1+nVar_Disc4clDesag-1+nVar_Disc4cl)))
colnames(Model_Disc4clAll) = c(VarEstat_Disc4clAll)
rown = c("(Intercept)",Var_Disc4clDesag[4:7],Var_Disc4cl[3])
rownames(Model_Disc4clAll) = rown

for (v in 1:nVar_Disc4cl) {
  variv = Var_Disc4cl[v+2]
  for (vdes in 1:length(rown)) {
    varDesagNom = rown[vdes]
    Model_Disc4clAll[varDesagNom,1] = mean(as.numeric(Model_Disc4cl_coef[varDesagNom,]))
    Model_Disc4clAll[varDesagNom,2] = mean(as.numeric(Model_Disc4cl_coef[varDesagNom,])) - (qnorm(0.975)*sd(as.numeric(Model_Disc4cl_coef[varDesagNom,]))/sqrt(Nboot))
    Model_Disc4clAll[varDesagNom,3] = mean(as.numeric(Model_Disc4cl_coef[varDesagNom,])) + (qnorm(0.975)*sd(as.numeric(Model_Disc4cl_coef[varDesagNom,]))/sqrt(Nboot))
    Model_Disc4clAll[varDesagNom,4] = mean(as.numeric(Model_Disc4cl_coef.pvalue[varDesagNom,]))
    Model_Disc4clAll[varDesagNom,5] = mean(as.numeric(Model_Disc4cl_coef.pvalue[varDesagNom,])) - (qnorm(0.975)*sd(as.numeric(Model_Disc4cl_coef.pvalue[varDesagNom,]))/sqrt(Nboot))
    Model_Disc4clAll[varDesagNom,6] = mean(as.numeric(Model_Disc4cl_coef.pvalue[varDesagNom,])) + (qnorm(0.975)*sd(as.numeric(Model_Disc4cl_coef.pvalue[varDesagNom,]))/sqrt(Nboot))
    print(varDesagNom)
  }
  Model_Disc4clAll[variv,13] = mean(as.numeric(Model_Disc4cl_LRchi2[variv,]))
  Model_Disc4clAll[variv,14] = mean(as.numeric(Model_Disc4cl_LRchi2[variv,]) - (qnorm(0.975)*((sd(as.numeric(Model_Disc4cl_LRchi2[variv,])))/sqrt(Nboot))))
  Model_Disc4clAll[variv,15] = mean(as.numeric(Model_Disc4cl_LRchi2[variv,]) + (qnorm(0.975)*((sd(as.numeric(Model_Disc4cl_LRchi2[variv,])))/sqrt(Nboot))))
  Model_Disc4clAll[variv,16] = mean(as.numeric(Model_Disc4cl_LRchi2.pvalue[variv,]))
  Model_Disc4clAll[variv,17] = mean(as.numeric(Model_Disc4cl_LRchi2.pvalue[variv,])) - (qnorm(0.975)*sd(as.numeric(Model_Disc4cl_LRchi2.pvalue[variv,]))/sqrt(Nboot))
  Model_Disc4clAll[variv,18] = mean(as.numeric(Model_Disc4cl_LRchi2.pvalue[variv,])) + (qnorm(0.975)*sd(as.numeric(Model_Disc4cl_LRchi2.pvalue[variv,]))/sqrt(Nboot))
  Model_Disc4clAll[variv,19] = mean(as.numeric(Model_Disc4cl_r2Nagelkerke[variv,]))
  Model_Disc4clAll[variv,20] = mean(as.numeric(Model_Disc4cl_r2Nagelkerke[variv,])) - (qnorm(0.975)*sd(as.numeric(Model_Disc4cl_r2Nagelkerke[variv,]))/sqrt(Nboot))
  Model_Disc4clAll[variv,21] = mean(as.numeric(Model_Disc4cl_r2Nagelkerke[variv,])) + (qnorm(0.975)*sd(as.numeric(Model_Disc4cl_r2Nagelkerke[variv,]))/sqrt(Nboot))
  Model_Disc4clAll[variv,22] = mean(as.numeric(Model_Disc4cl_aic[variv,]))
  Model_Disc4clAll[variv,23] = mean(as.numeric(Model_Disc4cl_aic[variv,])) - (qnorm(0.975)*((sd(as.numeric(Model_Disc4cl_aic[variv,])))/sqrt(Nboot)))
  Model_Disc4clAll[variv,24] = mean(as.numeric(Model_Disc4cl_aic[variv,])) + (qnorm(0.975)*((sd(as.numeric(Model_Disc4cl_aic[variv,])))/sqrt(Nboot)))
  
  print(variv)
}

write.csv2 (Model_Disc4clAll,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_2010_Disc_AspectD4.csv" )


 

########################################### UM - Discrete variables AspectD4 (dummy) ---------------------------------
dir = c("Flat","N","E","S","W")
Variav_AspectD4dum = c("ID", "Class", "AspectD4Flat", "AspectD4N","AspectD4E","AspectD4S","AspectD4W")
data = data[,Variav_AspectD4dum]

# ad=1
# Nboot=2
for (ad in 1:length(dir)) {
  Var_AspectDum = Variav_AspectD4dum[2+ad]
  Var_AspectDumDesag = paste(Var_AspectDum,"1",sep="")
  Var_AspectDumDesag_row.names = c("(Intercept)",Var_AspectDumDesag,Var_AspectDum)
  
  data_AspectDum = data.table(data[,c("ID", "Class",Var_AspectDum)])
  setkeyv(data_AspectDum,c("Class"))
  data_AspectDum = na.omit(data_AspectDum)

  Model_AspectDum_Samples_train = data.frame(Boot=1:Nboot, Samples=NA)

  Model_AspectDum_coef = data.frame(matrix(NA, ncol=Nboot, nrow=3), row.names=Var_AspectDumDesag_row.names)
  colnames(Model_AspectDum_coef) = paste("Boot",1:Nboot, sep=" ")
  Model_AspectDum_coef.se = Model_AspectDum_coef
  Model_AspectDum_coef.pvalue = Model_AspectDum_coef

  Model_AspectDum_LogLik_l0 = as.numeric()
  Model_AspectDum_LogLik_lM = as.numeric()

  Model_AspectDum_LRchi2 = as.numeric()
  Model_AspectDum_LRchi2.pvalue = as.numeric()
  Model_AspectDum_r2Nagelkerke = as.numeric()
  Model_AspectDum_aic = as.numeric()

  for (b in (1:Nboot)) {
    tmp_AspectDum = rbind((data_AspectDum[data_AspectDum$Class==1,][sample(nrow(data_AspectDum[data_AspectDum$Class==1,]), Nsamples, replace=F),]),
                         (data_AspectDum[data_AspectDum$Class==0,][sample(nrow(data_AspectDum[data_AspectDum$Class==0,]), Nsamples, replace=F),]))
    tmp_AspectDum = na.omit(tmp_AspectDum)
    Model_AspectDum_Samples_train[b,2] = paste(as.vector(tmp_AspectDum$ID),collapse="-")
    tmp_AspectDum = tmp_AspectDum [,-1]
    m_AspectDum = glm(formula=Class ~ ., family=binomial(link="logit"), data=tmp_AspectDum)
    tmp_AspectDum_coef = as.data.frame(coef(summary(m_AspectDum))[,1])
    for (p in (1:(nrow(tmp_AspectDum_coef)))) {Model_AspectDum_coef[rownames(tmp_AspectDum_coef)[p],b] = tmp_AspectDum_coef[rownames(tmp_AspectDum_coef)[p],1]}
    tmp_AspectDum_coef.se = as.data.frame(coef(summary(m_AspectDum))[,2])
    for (p in (1:(nrow(tmp_AspectDum_coef.se)))) {Model_AspectDum_coef.se[rownames(tmp_AspectDum_coef.se)[p],b] = tmp_AspectDum_coef.se[rownames(tmp_AspectDum_coef.se)[p],1]}
    tmp_AspectDum_coef.pvalue = as.data.frame(coef(summary(m_AspectDum))[,4])
    for (p in (1:nrow(tmp_AspectDum_coef.pvalue))) {Model_AspectDum_coef.pvalue[rownames(tmp_AspectDum_coef.pvalue)[p],b] = tmp_AspectDum_coef.pvalue[rownames(tmp_AspectDum_coef.pvalue)[p],1]}
    tmp_AspectDum_lrtest = lrtest(m_AspectDum) 
    Model_AspectDum_LogLik_l0[b] = as.numeric(tmp_AspectDum_lrtest[2,2])
    Model_AspectDum_LogLik_lM[b] = as.numeric(tmp_AspectDum_lrtest[2,1])
    Model_AspectDum_LRchi2[b] = as.numeric(tmp_AspectDum_lrtest[2,4])
    Model_AspectDum_LRchi2.pvalue[b] = as.numeric(tmp_AspectDum_lrtest[2,5])
    tmp_AspectDum_pR2 = pR2(m_AspectDum)
    Model_AspectDum_r2Nagelkerke[b] = as.numeric(tmp_AspectDum_pR2[6])
    Model_AspectDum_aic[b] = as.numeric(m_AspectDum$aic)
    print(paste("boot =", b, "- variav =", dir[ad]))
    }
  
  file = paste("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_AspectD4dum-",dir[ad],"_Samples_train.csv", sep="")
  write.csv2 (Model_AspectDum_Samples_train,file)
  file = paste("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_AspectD4dum-",dir[ad],"_coef.csv", sep="")
  write.csv2 (Model_AspectDum_coef,file)
  file = paste("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_AspectD4dum-",dir[ad],"_coef.se.csv", sep="")
  write.csv2 (Model_AspectDum_coef.se,file)
  file = paste("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_AspectD4dum-",dir[ad],"_coef.pvalue.csv", sep="")
  write.csv2 (Model_AspectDum_coef.pvalue,file)
  file = paste("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_AspectD4dum-",dir[ad],"_intercept.csv", sep="")
  write.csv2 (Model_AspectDum_LRchi2,file)
  file = paste("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_AspectD4dum-",dir[ad],"_LRchi2.pvalue.csv", sep="")
  write.csv2 (Model_AspectDum_LRchi2.pvalue,file)
  file = paste("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_AspectD4dum-",dir[ad],"_r2Nagelkerke.csv", sep="")
  write.csv2 (Model_AspectDum_r2Nagelkerke,file)
  file = paste("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_AspectD4dum-",dir[ad],"_aic.csv", sep="")
  write.csv2 (Model_AspectDum_aic,file)

  VarEstat_AspectDumAll = c("mean.coef_param","CIinf mean.coef_param","CIsup coef_param",
                            "mean.coef.p-value","CIinf mean.coef.p-value","CIsup mean.coef.p-value",
                            "mean.LL0","CIinf mean.LL0","CIsup mean.LL0",
                            "mean.LLMx","CIinf mean.LLMx","CIsup mean.LLMx",
                            "mean.LRchi2_MXm0","CIinf mean.LRchi2_MXm0","CIsup mean_MXm0.LRchi2",
                            "mean.LRchi2_MXm0.pvalue","CIinf mean.LRchi2_MXm0.pvalue","CIsup mean_MXm0.LRchi2.pvalue",
                            "mean.r2Nagelkerke","CIinf mean.r2Nagelkerke", "CIsup mean.r2Nagelkerke",
                            "mean.aic","CIinf mean.aic", "CIsup mean.aic")

  Model_AspectDumAll = data.frame(matrix(NA, ncol=length(VarEstat_AspectDumAll), nrow=(3)))
  colnames(Model_AspectDumAll) = c(VarEstat_AspectDumAll)
  rownames(Model_AspectDumAll) = Var_AspectDumDesag_row.names

  for (vdes in 1:2) {
     varDesagNom = Var_AspectDumDesag_row.names[vdes]
     Model_AspectDumAll[varDesagNom,1] = mean(as.numeric(Model_AspectDum_coef[varDesagNom,]))
     Model_AspectDumAll[varDesagNom,2] = mean(as.numeric(Model_AspectDum_coef[varDesagNom,])) - (qnorm(0.975)*sd(as.numeric(Model_AspectDum_coef[varDesagNom,]))/sqrt(Nboot))
     Model_AspectDumAll[varDesagNom,3] = mean(as.numeric(Model_AspectDum_coef[varDesagNom,])) + (qnorm(0.975)*sd(as.numeric(Model_AspectDum_coef[varDesagNom,]))/sqrt(Nboot))
     Model_AspectDumAll[varDesagNom,4] = mean(as.numeric(Model_AspectDum_coef.pvalue[varDesagNom,]))
     Model_AspectDumAll[varDesagNom,5] = mean(as.numeric(Model_AspectDum_coef.pvalue[varDesagNom,])) - (qnorm(0.975)*sd(as.numeric(Model_AspectDum_coef.pvalue[varDesagNom,]))/sqrt(Nboot))
     Model_AspectDumAll[varDesagNom,6] = mean(as.numeric(Model_AspectDum_coef.pvalue[varDesagNom,])) + (qnorm(0.975)*sd(as.numeric(Model_AspectDum_coef.pvalue[varDesagNom,]))/sqrt(Nboot))
     print(varDesagNom)
   }
   Model_AspectDumAll[Var_AspectDum,7] = mean(as.numeric(Model_AspectDum_LogLik_l0))
   Model_AspectDumAll[Var_AspectDum,8] = mean(as.numeric(Model_AspectDum_LogLik_l0) - (qnorm(0.975)*((sd(as.numeric(Model_AspectDum_LogLik_l0)))/sqrt(Nboot))))
   Model_AspectDumAll[Var_AspectDum,9] = mean(as.numeric(Model_AspectDum_LogLik_l0) + (qnorm(0.975)*((sd(as.numeric(Model_AspectDum_LogLik_l0)))/sqrt(Nboot))))
   Model_AspectDumAll[Var_AspectDum,10] = mean(as.numeric(Model_AspectDum_LogLik_lM))
   Model_AspectDumAll[Var_AspectDum,11] = mean(as.numeric(Model_AspectDum_LogLik_lM) - (qnorm(0.975)*((sd(as.numeric(Model_AspectDum_LogLik_lM)))/sqrt(Nboot))))
   Model_AspectDumAll[Var_AspectDum,12] = mean(as.numeric(Model_AspectDum_LogLik_lM) + (qnorm(0.975)*((sd(as.numeric(Model_AspectDum_LogLik_lM)))/sqrt(Nboot))))
   
   Model_AspectDumAll[Var_AspectDum,13] = mean(as.numeric(Model_AspectDum_LRchi2))
   Model_AspectDumAll[Var_AspectDum,14] = mean(as.numeric(Model_AspectDum_LRchi2) - (qnorm(0.975)*((sd(as.numeric(Model_AspectDum_LRchi2)))/sqrt(Nboot))))
   Model_AspectDumAll[Var_AspectDum,15] = mean(as.numeric(Model_AspectDum_LRchi2) + (qnorm(0.975)*((sd(as.numeric(Model_AspectDum_LRchi2)))/sqrt(Nboot))))
   
   Model_AspectDumAll[Var_AspectDum,16] = mean(as.numeric(Model_AspectDum_LRchi2.pvalue))
   Model_AspectDumAll[Var_AspectDum,17] = mean(as.numeric(Model_AspectDum_LRchi2.pvalue)) - (qnorm(0.975)*sd(as.numeric(Model_AspectDum_LRchi2.pvalue))/sqrt(Nboot))
   Model_AspectDumAll[Var_AspectDum,19] = mean(as.numeric(Model_AspectDum_LRchi2.pvalue)) + (qnorm(0.975)*sd(as.numeric(Model_AspectDum_LRchi2.pvalue))/sqrt(Nboot))
   
   Model_AspectDumAll[Var_AspectDum,19] = mean(as.numeric(Model_AspectDum_r2Nagelkerke))
   Model_AspectDumAll[Var_AspectDum,20] = mean(as.numeric(Model_AspectDum_r2Nagelkerke)) - (qnorm(0.975)*sd(as.numeric(Model_AspectDum_r2Nagelkerke))/sqrt(Nboot))
   Model_AspectDumAll[Var_AspectDum,21] = mean(as.numeric(Model_AspectDum_r2Nagelkerke)) + (qnorm(0.975)*sd(as.numeric(Model_AspectDum_r2Nagelkerke))/sqrt(Nboot))
   Model_AspectDumAll[Var_AspectDum,22] = mean(as.numeric(Model_AspectDum_aic))
   Model_AspectDumAll[Var_AspectDum,23] = mean(as.numeric(Model_AspectDum_aic)) - (qnorm(0.975)*((sd(as.numeric(Model_AspectDum_aic)))/sqrt(Nboot)))
   Model_AspectDumAll[Var_AspectDum,24] = mean(as.numeric(Model_AspectDum_aic)) + (qnorm(0.975)*((sd(as.numeric(Model_AspectDum_aic)))/sqrt(Nboot)))
  
 print(Var_AspectDum)


 file = paste("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_2010_Disc_AspectD4dum-",dir[ad],".csv", sep="")
 write.csv2 (Model_AspectDumAll, file ) 
}




########################################### UM - Discrete variables AspectD (dummy) ---------------------------------
dir = c("Flat","N","NE","E","SE","S","SW","W","NW")
# ad=2
# Nboot=2


for (ad in 1:9) {
  dirdum = dir[ad]
  data$AspectDum = 0
  data$AspectDum [data$AspectD==dirdum] = 1
  data$AspectDum = as.factor(data$AspectDum)
  unique(data$AspectDum)
  
  Var_AspectDum = c("ID", "Class","AspectDum")
  Var_AspectDumDesag = c("ID", "Class","AspectDum1")
  nVar_AspectDum = length(Var_AspectDum) - 2
  nVar_AspectDumDesag = length(Var_AspectDumDesag) - 2
  
  data_AspectDum = data.table(data[,Var_AspectDum])
  setkeyv(data_AspectDum,c("Class"))
  data_AspectDum = na.omit(data_AspectDum)
  
  Model_AspectDum_Samples_train = data.frame(Boot=1:Nboot, Samples=NA)
  
  Model_AspectDum_coef = data.frame(matrix(NA, ncol=Nboot, nrow=nVar_AspectDumDesag), row.names=c(Var_AspectDumDesag[-(1:2)]))
  colnames(Model_AspectDum_coef) = paste("Boot",1:Nboot, sep=" ")
  Model_AspectDum_coef.se = Model_AspectDum_coef
  Model_AspectDum_coef.pvalue = Model_AspectDum_coef
  
  Model_AspectDum_intercept = data.frame(matrix(NA, ncol=Nboot, nrow=nVar_AspectDum), row.names=c(Var_AspectDum[-(1:2)]))
  colnames(Model_AspectDum_intercept) = paste("Boot",1:Nboot, sep=" ")
  Model_AspectDum_intercept.se = Model_AspectDum_intercept
  Model_AspectDum_intercept.pvalue = Model_AspectDum_intercept
  
  Model_AspectDum_LRchi2 = Model_AspectDum_intercept
  Model_AspectDum_LRchi2.pvalue = Model_AspectDum_intercept
  Model_AspectDum_r2Nagelkerke = Model_AspectDum_intercept
  Model_AspectDum_aic = Model_AspectDum_intercept
  
  for (b in (1:Nboot)) {
    tmp_AspectDum = rbind((data_AspectDum[data_AspectDum$Class==1,][sample(nrow(data_AspectDum[data_AspectDum$Class==1,]), Nsamples, replace=F),]),
                          (data_AspectDum[data_AspectDum$Class==0,][sample(nrow(data_AspectDum[data_AspectDum$Class==0,]), Nsamples, replace=F),]))
    Model_AspectDum_Samples_train[b,2] = paste(as.vector(tmp_AspectDum$ID),collapse="-")
    for (v in (1:nVar_AspectDum)) { 
      cols = c("Class",Var_AspectDum[2+v])
      tmp_AspectDumv = tmp_AspectDum[,cols,with=F]
      tmp_AspectDumv = na.omit(tmp_AspectDumv)
      m_AspectDum = glm(formula=Class ~ ., family=binomial(link="logit"), data=tmp_AspectDumv)
      tmp_AspectDum_coef = as.data.frame(coef(summary(m_AspectDum))[,1])
      for (p in (1:(nrow(tmp_AspectDum_coef)))) {Model_AspectDum_coef[rownames(tmp_AspectDum_coef)[p],b] = tmp_AspectDum_coef[rownames(tmp_AspectDum_coef)[p],1]}
      tmp_AspectDum_coef.se = as.data.frame(coef(summary(m_AspectDum))[,2])
      for (p in (1:(nrow(tmp_AspectDum_coef.se)))) {Model_AspectDum_coef.se[rownames(tmp_AspectDum_coef.se)[p],b] = tmp_AspectDum_coef.se[rownames(tmp_AspectDum_coef.se)[p],1]}
      tmp_AspectDum_coef.pvalue = as.data.frame(coef(summary(m_AspectDum))[,4])
      for (p in (1:nrow(tmp_AspectDum_coef.pvalue))) {Model_AspectDum_coef.pvalue[rownames(tmp_AspectDum_coef.pvalue)[p],b] = tmp_AspectDum_coef.pvalue[rownames(tmp_AspectDum_coef.pvalue)[p],1]}
      tmp_AspectDum_lrtest = lrtest(m_AspectDum) 
      Model_AspectDum_LRchi2[v,b] = as.numeric(tmp_AspectDum_lrtest[2,4])
      Model_AspectDum_LRchi2.pvalue[v,b] = as.numeric(tmp_AspectDum_lrtest[2,5])
      tmp_AspectDum_pR2 = pR2(m_AspectDum)
      Model_AspectDum_r2Nagelkerke[v,b] = as.numeric(tmp_AspectDum_pR2[6])
      Model_AspectDum_aic[v,b] = as.numeric(m_AspectDum$aic)
      print(paste("boot =", b, "- Var_AspectDum =", dirdum))
    }
  }
  
  file = paste("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_AspectDum-",dirdum,"_Samples_train.csv", sep="")
  write.csv2 (Model_AspectDum_Samples_train,file)
  file = paste("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_AspectDum-",dirdum,"_coef.csv", sep="")
  write.csv2 (Model_AspectDum_coef,file)
  file = paste("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_AspectDum-",dirdum,"_coef.se.csv", sep="")
  write.csv2 (Model_AspectDum_coef.se,file)
  file = paste("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_AspectDum-",dirdum,"_coef.pvalue.csv", sep="")
  write.csv2 (Model_AspectDum_coef.pvalue,file)
  file = paste("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_AspectDum-",dirdum,"_intercept.csv", sep="")
  write.csv2 (Model_AspectDum_LRchi2,file)
  file = paste("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_AspectDum-",dirdum,"_LRchi2.pvalue.csv", sep="")
  write.csv2 (Model_AspectDum_LRchi2.pvalue,file)
  file = paste("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_AspectDum-",dirdum,"_r2Nagelkerke.csv", sep="")
  write.csv2 (Model_AspectDum_r2Nagelkerke,file)
  file = paste("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_Disc_AspectDum-",dirdum,"_aic.csv", sep="")
  write.csv2 (Model_AspectDum_aic,file)
  
  VarEstat_AspectDumAll = c("mean.coef_param","CIinf mean.coef_param","CIsup coef_param",
                            "mean.coef.p-value","CIinf mean.coef.p-value","CIsup mean.coef.p-value",
                            "mean.LL0","CIinf mean.LL0","CIsup mean.LL0",
                            "mean.LLMx","CIinf mean.LLMx","CIsup mean.LLMx",
                            "mean.LRchi2_MXm0","CIinf mean.LRchi2_MXm0","CIsup mean_MXm0.LRchi2",
                            "mean.LRchi2_MXm0.pvalue","CIinf mean.LRchi2_MXm0.pvalue","CIsup mean_MXm0.LRchi2.pvalue",
                            "mean.r2Nagelkerke","CIinf mean.r2Nagelkerke", "CIsup mean.r2Nagelkerke",
                            "mean.aic","CIinf mean.aic", "CIsup mean.aic")
  
  Model_AspectDumAll = data.frame(matrix(NA, ncol=length(VarEstat_AspectDumAll), nrow=(3)))
  colnames(Model_AspectDumAll) = c(VarEstat_AspectDumAll)
  rownames(Model_AspectDumAll) = c("(Intercept)", "AspectDum1", "AspectDum")
  
  variv = Var_AspectDum[v+2]
  varDesag = c("(Intercept)", "AspectDum1", "AspectDum")
  for (vdes in 1:2) {
    varDesagNom = varDesag[vdes]
    Model_AspectDumAll[varDesagNom,1] = mean(as.numeric(Model_AspectDum_coef[varDesagNom,]))
    Model_AspectDumAll[varDesagNom,2] = mean(as.numeric(Model_AspectDum_coef[varDesagNom,])) - (qnorm(0.975)*sd(as.numeric(Model_AspectDum_coef[varDesagNom,]))/sqrt(Nboot))
    Model_AspectDumAll[varDesagNom,3] = mean(as.numeric(Model_AspectDum_coef[varDesagNom,])) + (qnorm(0.975)*sd(as.numeric(Model_AspectDum_coef[varDesagNom,]))/sqrt(Nboot))
    Model_AspectDumAll[varDesagNom,4] = mean(as.numeric(Model_AspectDum_coef.pvalue[varDesagNom,]))
    Model_AspectDumAll[varDesagNom,5] = mean(as.numeric(Model_AspectDum_coef.pvalue[varDesagNom,])) - (qnorm(0.975)*sd(as.numeric(Model_AspectDum_coef.pvalue[varDesagNom,]))/sqrt(Nboot))
    Model_AspectDumAll[varDesagNom,6] = mean(as.numeric(Model_AspectDum_coef.pvalue[varDesagNom,])) + (qnorm(0.975)*sd(as.numeric(Model_AspectDum_coef.pvalue[varDesagNom,]))/sqrt(Nboot))
    print(varDesagNom)
  }
  Model_AspectDumAll[variv,13] = mean(as.numeric(Model_AspectDum_LRchi2[variv,]))
  Model_AspectDumAll[variv,14] = mean(as.numeric(Model_AspectDum_LRchi2[variv,]) - (qnorm(0.975)*((sd(as.numeric(Model_AspectDum_LRchi2[variv,])))/sqrt(Nboot))))
  Model_AspectDumAll[variv,15] = mean(as.numeric(Model_AspectDum_LRchi2[variv,]) + (qnorm(0.975)*((sd(as.numeric(Model_AspectDum_LRchi2[variv,])))/sqrt(Nboot))))
  Model_AspectDumAll[variv,16] = mean(as.numeric(Model_AspectDum_LRchi2.pvalue[variv,]))
  Model_AspectDumAll[variv,17] = mean(as.numeric(Model_AspectDum_LRchi2.pvalue[variv,])) - (qnorm(0.975)*sd(as.numeric(Model_AspectDum_LRchi2.pvalue[variv,]))/sqrt(Nboot))
  Model_AspectDumAll[variv,18] = mean(as.numeric(Model_AspectDum_LRchi2.pvalue[variv,])) + (qnorm(0.975)*sd(as.numeric(Model_AspectDum_LRchi2.pvalue[variv,]))/sqrt(Nboot))
  Model_AspectDumAll[variv,19] = mean(as.numeric(Model_AspectDum_r2Nagelkerke[variv,]))
  Model_AspectDumAll[variv,20] = mean(as.numeric(Model_AspectDum_r2Nagelkerke[variv,])) - (qnorm(0.975)*sd(as.numeric(Model_AspectDum_r2Nagelkerke[variv,]))/sqrt(Nboot))
  Model_AspectDumAll[variv,21] = mean(as.numeric(Model_AspectDum_r2Nagelkerke[variv,])) + (qnorm(0.975)*sd(as.numeric(Model_AspectDum_r2Nagelkerke[variv,]))/sqrt(Nboot))
  Model_AspectDumAll[variv,22] = mean(as.numeric(Model_AspectDum_aic[variv,]))
  Model_AspectDumAll[variv,23] = mean(as.numeric(Model_AspectDum_aic[variv,])) - (qnorm(0.975)*((sd(as.numeric(Model_AspectDum_aic[variv,])))/sqrt(Nboot)))
  Model_AspectDumAll[variv,24] = mean(as.numeric(Model_AspectDum_aic[variv,])) + (qnorm(0.975)*((sd(as.numeric(Model_AspectDum_aic[variv,])))/sqrt(Nboot)))
  
  print(variv)
  
  
  file = paste("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_2010_Disc_AspectDum-",dirdum,".csv", sep="")
  write.csv2 (Model_AspectDumAll, file ) 
}




################################################ Estat - MM (Multivariable Model) ---------------------------------
rm(list=ls())
gc()

library(data.table)
library(stats) #aov test
library(lmtest) #lr test
library(pscl) #R2
library(ggplot2)
library(glmulti)
library(oddsratio)
# library(rms)
# library(survey) #regTermTest
# library(caret)
# library(car)
# library(rsq)


#rm(list=ls()[! ls() %in% c("pts_data")])
pts_data = read.csv2 ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/DataAll_Train_PTS.csv", header=T)

data = as.data.frame(pts_data)

data$Sbn2010 = as.factor(data$Sbn2010)

data$SubnChn [data$SubnChn==2] = 0
data$SubnChn = as.factor(data$SubnChn)

data$Suscep2e3 [data$Suscep==1] = 0
data$Suscep2e3 [data$Suscep==2] = 1
data$Suscep2e3 [data$Suscep==3] = 1
data$Suscep2e3 = as.factor(data$Suscep2e3)

data$Suscep3 [data$Suscep==1] = 0
data$Suscep3 [data$Suscep==2] = 0
data$Suscep3 [data$Suscep==3] = 1
data$Suscep3 = as.factor(data$Suscep3)

data$Suscep = as.factor(data$Suscep)
unique(data$Suscep)

tab_class = as.data.frame(data$Class)
names(tab_class) = c("Class")
tab_class$Cl = NA
tab_class$Cl[tab_class$Class=="yes"] = 1
tab_class$Cl[tab_class$Class=="no"] = 0
data$Class = as.factor(tab_class$Cl)
rm(tab_class)

data$AspectD = NA
data$AspectD [is.na(data$Aspect)] = "Flat"
data$AspectD [data$Aspect==0] = "N"
data$AspectD [data$Aspect>=(337.5)| data$Aspect<(22.5)] = "N"
data$AspectD [data$Aspect>=(22.5) & data$Aspect<(67.5)] = "NE"
data$AspectD [data$Aspect>=(67.5) & data$Aspect<(112.5)] = "E"
data$AspectD [data$Aspect>=(112.5) & data$Aspect<(157.5)] = "SE"
data$AspectD [data$Aspect>=(157.5) & data$Aspect<(202.5)] = "S"
data$AspectD [data$Aspect>=(202.5) & data$Aspect<(247.5)] = "SW"
data$AspectD [data$Aspect>=(247.5) & data$Aspect<(292.5)] = "W"
data$AspectD [data$Aspect>=(292.5) & data$Aspect<(337.5)] = "NW"
data$AspectD = as.factor(data$AspectD)
unique(data$AspectD)

data$AspectDN = 0
data$AspectDN [data$AspectD=="N"] = 1
data$AspectDN = as.factor(data$AspectDN)
data$AspectDNE = 0
data$AspectDNE [data$AspectD=="NE"] = 1
data$AspectDNE = as.factor(data$AspectDNE)
data$AspectDE = 0
data$AspectDE [data$AspectD=="E"] = 1
data$AspectDE = as.factor(data$AspectDE)
data$AspectDSE = 0
data$AspectDSE [data$AspectD=="SE"] = 1
data$AspectDSE = as.factor(data$AspectDSE)
data$AspectDS = 0
data$AspectDS [data$AspectD=="S"] = 1
data$AspectDS = as.factor(data$AspectDS)
data$AspectDSW = 0
data$AspectDSW [data$AspectD=="SW"] = 1
data$AspectDSW = as.factor(data$AspectDSW)
data$AspectDW = 0
data$AspectDW [data$AspectD=="W"] = 1
data$AspectDW = as.factor(data$AspectDW)
data$AspectDNW = 0
data$AspectDNW [data$AspectD=="NW"] = 1
data$AspectDNW = as.factor(data$AspectDNW)
data$AspectDFlat = 0
data$AspectDFlat [data$AspectD=="Flat"] = 1
data$AspectDFlat = as.factor(data$AspectDFlat)

data$AspectD4 = NA
data$AspectD4 [is.na(data$Aspect)] = "0" #"Flat"
data$AspectD4 [data$Aspect==0] =  "1" #"N"
data$AspectD4 [data$Aspect>=(315)| data$Aspect<(45)] =  "1" #"N"
data$AspectD4 [data$Aspect>=(45) & data$Aspect<(135)] =  "2" #"E"
data$AspectD4 [data$Aspect>=(135) & data$Aspect<(225)] =  "3" #"S"
data$AspectD4 [data$Aspect>=(225) & data$Aspect<(315)] =  "4" #"W"
data$AspectD4 = as.factor(data$AspectD4)
unique(data$AspectD4)

data$AspectD4Flat = 0
data$AspectD4Flat [data$AspectD=="Flat"] = 1
data$AspectD4Flat = as.factor(data$AspectD4Flat)
data$AspectD4N = 0
data$AspectD4N [data$AspectD=="N"] = 1
data$AspectD4N = as.factor(data$AspectD4N)
data$AspectD4E = 0
data$AspectD4E [data$AspectD=="E"] = 1
data$AspectD4E = as.factor(data$AspectD4E)
data$AspectD4S = 0
data$AspectD4S [data$AspectD=="S"] = 1
data$AspectD4S = as.factor(data$AspectD4S)
data$AspectD4W = 0
data$AspectD4W [data$AspectD=="W"] = 1
data$AspectD4W = as.factor(data$AspectD4W)

data = droplevels(data)

rm(list=ls()[! ls() %in% c("pts_data", "data")])

Ano = 2010
Nboot = 1000
Nsamples = 1000

Variav_M0 = c("ID", "Class",
              "Rn0d14d","Slope",
              "Suscep",
              "Veg2010","VegDif",
              "MnI2010","Sbn2010","SubnChn",
              "Dm2010","DmDif",
              "PSMF10")
nVariav_M0 = length(Variav_M0)-2
Variav_M0_Desag = c("Rn0d14d","Slope",
                    "Suscep","Suscep2","Suscep3",
                    "Veg2010","VegDif",
                    "MnI2010","Sbn2010","Sbn20101","SubnChn","SubnChn1",
                    "Dm2010","DmDif",
                    "PSMF10")
nVariav_M0_Desag = length(Variav_M0_Desag)

data_M0 = data.table(data[,Variav_M0])
setkeyv(data_M0,c("Class"))
table(data_M0$Class)




########################################### MM - M0 =================================
M0_Samples_train = data.frame(Boot=1:Nboot, Samples=NA)

M0_coef = data.frame(matrix(NA, ncol=Nboot, nrow=1+nVariav_M0_Desag), row.names=c("(Intercept)",Variav_M0_Desag))
colnames(M0_coef) = paste("Boot",1:Nboot, sep=" ")
M0_coef.se = M0_coef
M0_coef.pvalue = M0_coef

M0v_LogLik_lM0v = M0_coef
M0v_LogLik_l0 = M0_coef
M0v_LRchi2_M0vm0 = M0_coef
M0v_LRchi2_M0vm0.pvalue = M0_coef
M0v_aic = M0_coef

M0_LogLik_l0 = as.numeric() 
M0_LogLik_lM0 = as.numeric() 
M0_LRchi2 = as.numeric()
M0_LRchi2.pvalue = as.numeric()
M0_r2Nagelkerke = as.numeric()
M0_aic = as.numeric()

# topAIC = data.frame(matrix(NA, ncol=5, nrow=0))
# colnames(topAIC) = c("Boot","AIC_Rank","Model","AIC","Weights" )
# topBIC = data.frame(matrix(NA, ncol=5, nrow=0))
# colnames(topBIC) = c("Boot","BIC_Rank","Model","BIC","Weights" ) 

#b=1
#Nboot=2
for (b in (1:Nboot)) {
  tmp_M0 = rbind((data_M0[data_M0$Class==1,][sample(nrow(data_M0[data_M0$Class==1,]), Nsamples, replace=F),]),
                 (data_M0[data_M0$Class==0,][sample(nrow(data_M0[data_M0$Class==0,]), Nsamples, replace=F),]))
  tmp_M0 = na.omit(tmp_M0)
  Samples_train = as.vector(tmp_M0$ID)
  M0_Samples_train[b,2] = paste(Samples_train,collapse="-")
  tmp_M0 = tmp_M0[,-1]
  m_M0=glm(formula=Class ~ ., family=binomial(link="logit"), data=tmp_M0)
  tmp_M0_coef = as.data.frame(coef(summary(m_M0))[,1])
  for (v in (1:(nrow(tmp_M0_coef)))) {M0_coef[rownames(tmp_M0_coef)[v],b] = tmp_M0_coef[rownames(tmp_M0_coef)[v],1]}
  tmp_M0_coef.se = as.data.frame(coef(summary(m_M0))[,2])
  for (v in (1:(nrow(tmp_M0_coef.se)))) {M0_coef.se[rownames(tmp_M0_coef.se)[v],b] = tmp_M0_coef.se[rownames(tmp_M0_coef.se)[v],1]}
  tmp_M0_coef.pvalue = as.data.frame(coef(summary(m_M0))[,4])
  for (v in (1:nrow(tmp_M0_coef.pvalue))) {M0_coef.pvalue[rownames(tmp_M0_coef.pvalue)[v],b] = tmp_M0_coef.pvalue[rownames(tmp_M0_coef.pvalue)[v],1]}
  tmp_M0_lrtest = lrtest(m_M0) 
  M0_LogLik_lM0 [b] = as.numeric(tmp_M0_lrtest[1,2])
  M0_LogLik_l0 [b] = as.numeric(tmp_M0_lrtest[2,2])
  M0_LRchi2 [b] = as.numeric(tmp_M0_lrtest[2,4])
  M0_LRchi2.pvalue [b] = as.numeric(tmp_M0_lrtest[2,5])
  tmp_M0_pR2 = pR2(m_M0)
  M0_r2Nagelkerke[b] = as.numeric(tmp_M0_pR2[6])
  M0_aic[b] = as.numeric(m_M0$aic)
  for (v in (1:nVariav_M0)){
    variav = c("Class", Variav_M0[v+2])
    tmp_M0v = tmp_M0[,..variav]
    m_M0v=glm(formula=Class ~ ., family=binomial(link="logit"), data=tmp_M0v)  
    tmp_M0_lrtest_M0vm0 = lrtest(m_M0v)
    M0v_LogLik_lM0v[variav[-1],b] = as.numeric(tmp_M0_lrtest_M0vm0[1,2])
    M0v_LogLik_lM0v [variav[-1],b] = as.numeric(tmp_M0_lrtest_M0vm0[1,2])
    M0v_LogLik_l0 [variav[-1],b] = as.numeric(tmp_M0_lrtest_M0vm0[2,2])
    M0v_LRchi2_M0vm0 [variav[-1],b] = as.numeric(tmp_M0_lrtest_M0vm0[2,4])
    M0v_LRchi2_M0vm0.pvalue [variav[-1],b] = as.numeric(tmp_M0_lrtest_M0vm0[2,5])
    M0v_aic[variav[-1],b] = as.numeric(m_M0v$aic)
  }
  # AIC_f1 = glmulti(Class ~ ., data=tmp_M0, family=binomial, 
  #                  level=1, crit=aic, fitfunc=glm, method="h",
  #                  confsetsize=256, plotty=T, report=TRUE, name="AIC_glmulti")
  # tmp_M0_topAIC <- glmulti::weightable(AIC_f1)
  # tmp_M0_topAIC$Boot = b
  # tmp_M0_topAIC$AIC_Rank = c(1:nrow(tmp_M0_topAIC))
  # colnames(tmp_M0_topAIC) = c("Model","AIC","Weights","Boot","AIC_Rank")
  # topAIC = rbind(topAIC,tmp_M0_topAIC)
  # BIC_f1 = glmulti(Class ~ ., data=tmp_M0, family=binomial, 
  #                  level=1, crit=bic, fitfunc=glm, method="h",
  #                  confsetsize=256, plotty=T, report=TRUE, name="BIC_glmulti")
  # tmp_M0_topBIC <- glmulti::weightable(BIC_f1)
  # tmp_M0_topBIC$Boot = b
  # tmp_M0_topBIC$BIC_Rank = c(1:nrow(tmp_M0_topBIC))
  # topBIC = rbind(topBIC,tmp_M0_topBIC)
  print(paste("variav=", v, "Boot=", b))
}

write.csv2 (M0_Samples_train,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0_Samples_train.csv")
write.csv2 (M0_coef,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0_coef.csv")
write.csv2 (M0_coef.se,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0_coef.se.csv")
write.csv2 (M0_coef.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0_coef.pvalue.csv")
write.csv2 (M0_LogLik_lM0,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0_LogLik_lm.csv")
write.csv2 (M0_LogLik_l0,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0_LogLik_l0.csv")
write.csv2 (M0_LRchi2,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0_LRchi2.csv")
write.csv2 (M0_LRchi2.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0_LRchi2.pvalue.csv")
write.csv2 (M0_r2Nagelkerke,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0_r2Nagelkerke.csv")
write.csv2 (M0v_LogLik_lM0v,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0v_LogLik_lM0v.csv")
write.csv2 (M0v_LogLik_l0,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0v_LogLik_l0.csv")
write.csv2 (M0v_LRchi2_M0vm0,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0v_LRchi2_M0vm0.csv")
write.csv2 (M0v_LRchi2_M0vm0.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0v_LRchi2_M0vm0.pvalue.csv")
write.csv2 (M0_aic,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0_aic.csv")
write.csv2 (M0v_aic,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0v_aic.csv")
# write.csv2 (topAIC, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0_AICf1.csv")
# write.csv2 (topBIC, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0_BICf1.csv")

# M0_coef = read.csv2 ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0_coef.csv", header=T)
# M0_coef.pvalue = read.csv2 ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0_coef.pvalue.csv", header=T)
# M0_LogLik_l0 = read.csv2 ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0_LogLik_l0.csv", header=T)
# M0_LogLik_l0 = as.vector (M0_LogLik_l0[,2])
# M0_LogLik_lM0 = read.csv2 ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0_LogLik_lm.csv", header=T)
# M0_LogLik_lM0 = as.vector (M0_LogLik_lM0[,2])
# M0_LRchi2 = read.csv2 ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0_LRchi2.csv", header=T)
# M0_LRchi2 = as.vector (M0_LRchi2[,2])
# M0_LRchi2.pvalue = read.csv2 ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0_LRchi2.pvalue.csv", header=T)
# M0_LRchi2.pvalue = as.vector (M0_LRchi2.pvalue[,2])
# M0_r2Nagelkerke = read.csv2 ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0_r2Nagelkerke.csv", header=T)
# M0_r2Nagelkerke = as.vector (M0_r2Nagelkerke[,2])

VarEstatAll_M = c("mean.coef_param","CIinf mean.coef_param","CIsup coef_param",
                  "mean.coef.p-value","CIinf mean.coef.p-value","CIsup mean.coef.p-value",
                  "mean.LL0","CIinf mean.LL0","CIsup mean.LL0",
                  "mean.LLM0","CIinf mean.LLM0","CIsup mean.LLM0",
                  "mean.LLMx","CIinf mean.LLMx","CIsup mean.LLMx",
                  "mean.LRchi2_MXm0","CIinf mean.LRchi2_MXm0","CIsup mean_MXm0.LRchi2",
                  "mean.LRchi2_MXM0","CIinf mean.LRchi2_MXM0","CIsup mean.LRchi2_MXM0",
                  "mean.LRchi2_MXm0.pvalue","CIinf mean.LRchi2_MXm0.pvalue","CIsup mean_MXm0.LRchi2.pvalue",
                  "mean.LRchi2_MXM0.pvalue","CIinf mean.LRchi2_MXM0.pvalue","CIsup mean.LRchi2_MXM0.pvalue",
                  "mean.r2Nagelkerke","CIinf mean.r2Nagelkerke", "CIsup mean.r2Nagelkerke",
                  "mean.aic","CIinf mean.aic", "CIsup mean.aic")

M0_All = data.frame(matrix(NA, ncol=length(VarEstatAll_M), nrow=1+nVariav_M0_Desag+1))
rownames(M0_All) = c("(Intercept)",Variav_M0_Desag,"model M0")
colnames(M0_All) = c(VarEstatAll_M)

for (v in 1:nrow(M0_coef)) {
  M0_All[v,1] = mean(as.numeric(M0_coef[v,]))
  M0_All[v,2] = mean(as.numeric(M0_coef[v,])) - (qnorm(0.975)*((sd(as.numeric(M0_coef[v,])))/sqrt(Nboot)))
  M0_All[v,3] = mean(as.numeric(M0_coef[v,])) + (qnorm(0.975)*((sd(as.numeric(M0_coef[v,])))/sqrt(Nboot)))
  M0_All[v,4] = mean(as.numeric(M0_coef.pvalue[v,]))
  M0_All[v,5] = mean(as.numeric(M0_coef.pvalue[v,])) - (qnorm(0.975)*((sd(as.numeric(M0_coef.pvalue[v,])))/sqrt(Nboot)))
  M0_All[v,6] = mean(as.numeric(M0_coef.pvalue[v,])) + (qnorm(0.975)*((sd(as.numeric(M0_coef.pvalue[v,])))/sqrt(Nboot)))
  
  M0_All[v,7] = mean(as.numeric(M0v_LogLik_l0[v,]))
  M0_All[v,8] = mean(as.numeric(M0v_LogLik_l0[v,])) - (qnorm(0.975)*((sd(as.numeric(M0v_LogLik_l0[v,])))/sqrt(Nboot)))
  M0_All[v,9] = mean(as.numeric(M0v_LogLik_l0[v,])) + (qnorm(0.975)*((sd(as.numeric(M0v_LogLik_l0[v,])))/sqrt(Nboot)))
  M0_All[v,10] = mean(as.numeric(M0v_LogLik_lM0v[v,]))
  M0_All[v,11] = mean(as.numeric(M0v_LogLik_lM0v[v,])) - (qnorm(0.975)*((sd(as.numeric(M0v_LogLik_lM0v[v,])))/sqrt(Nboot)))
  M0_All[v,12] = mean(as.numeric(M0v_LogLik_lM0v[v,])) + (qnorm(0.975)*((sd(as.numeric(M0v_LogLik_lM0v[v,])))/sqrt(Nboot)))
  
  M0_All[v,16] = mean(as.numeric(M0v_LRchi2_M0vm0[v,]))
  M0_All[v,17] = mean(as.numeric(M0v_LRchi2_M0vm0[v,])) - (qnorm(0.975)*((sd(as.numeric(M0v_LRchi2_M0vm0[v,])))/sqrt(Nboot)))
  M0_All[v,18] = mean(as.numeric(M0v_LRchi2_M0vm0[v,])) + (qnorm(0.975)*((sd(as.numeric(M0v_LRchi2_M0vm0[v,])))/sqrt(Nboot)))
  
  M0_All[v,22] = mean(as.numeric(M0v_LRchi2_M0vm0.pvalue[v,]))
  M0_All[v,23] = mean(as.numeric(M0v_LRchi2_M0vm0.pvalue[v,])) - (qnorm(0.975)*((sd(as.numeric(M0v_LRchi2_M0vm0.pvalue[v,])))/sqrt(Nboot)))
  M0_All[v,24] = mean(as.numeric(M0v_LRchi2_M0vm0.pvalue[v,])) + (qnorm(0.975)*((sd(as.numeric(M0v_LRchi2_M0vm0.pvalue[v,])))/sqrt(Nboot)))
  
  M0_All[v,31] = mean(as.numeric(M0v_aic[v,]))
  M0_All[v,32] = mean(as.numeric(M0v_aic[v,])) - (qnorm(0.975)*((sd(as.numeric(M0v_aic[v,])))/sqrt(Nboot)))
  M0_All[v,33] = mean(as.numeric(M0v_aic[v,])) + (qnorm(0.975)*((sd(as.numeric(M0v_aic[v,])))/sqrt(Nboot)))

  print(v)
}

M0_All[1+nVariav_M0_Desag+1,7] = mean(M0_LogLik_l0)
M0_All[1+nVariav_M0_Desag+1,8] = mean(M0_LogLik_l0) - (qnorm(0.975)*((sd(as.numeric(M0_LogLik_l0)))/sqrt(Nboot)))
M0_All[1+nVariav_M0_Desag+1,9] = mean(M0_LogLik_l0) + (qnorm(0.975)*((sd(as.numeric(M0_LogLik_l0)))/sqrt(Nboot)))
M0_All[1+nVariav_M0_Desag+1,10] = mean(M0_LogLik_lM0)
M0_All[1+nVariav_M0_Desag+1,11] = mean(M0_LogLik_lM0) - (qnorm(0.975)*((sd(as.numeric(M0_LogLik_lM0)))/sqrt(Nboot)))
M0_All[1+nVariav_M0_Desag+1,12] = mean(M0_LogLik_lM0) + (qnorm(0.975)*((sd(as.numeric(M0_LogLik_lM0)))/sqrt(Nboot)))
M0_All[1+nVariav_M0_Desag+1,13] = NA
M0_All[1+nVariav_M0_Desag+1,14] = NA
M0_All[1+nVariav_M0_Desag+1,15] = NA

M0_All[1+nVariav_M0_Desag+1,16] = mean(M0_LRchi2)
M0_All[1+nVariav_M0_Desag+1,17] = mean(M0_LRchi2) - (qnorm(0.975)*((sd(as.numeric(M0_LRchi2)))/sqrt(Nboot)))
M0_All[1+nVariav_M0_Desag+1,18] = mean(M0_LRchi2) + (qnorm(0.975)*((sd(as.numeric(M0_LRchi2)))/sqrt(Nboot)))
M0_All[1+nVariav_M0_Desag+1,19] = NA
M0_All[1+nVariav_M0_Desag+1,20] = NA
M0_All[1+nVariav_M0_Desag+1,21] = NA

M0_All[1+nVariav_M0_Desag+1,22] = mean(M0_LRchi2.pvalue)
M0_All[1+nVariav_M0_Desag+1,23] = mean(M0_LRchi2.pvalue) - (qnorm(0.975)*((sd(as.numeric(M0_LRchi2.pvalue)))/sqrt(Nboot)))
M0_All[1+nVariav_M0_Desag+1,24] = mean(M0_LRchi2.pvalue) + (qnorm(0.975)*((sd(as.numeric(M0_LRchi2.pvalue)))/sqrt(Nboot)))
M0_All[1+nVariav_M0_Desag+1,25] = NA
M0_All[1+nVariav_M0_Desag+1,26] = NA
M0_All[1+nVariav_M0_Desag+1,27] = NA

M0_All[1+nVariav_M0_Desag+1,28] = mean(M0_r2Nagelkerke)
M0_All[1+nVariav_M0_Desag+1,29] = mean(M0_r2Nagelkerke) - (qnorm(0.975)*((sd(as.numeric(M0_r2Nagelkerke)))/sqrt(Nboot)))
M0_All[1+nVariav_M0_Desag+1,30] = mean(M0_r2Nagelkerke) + (qnorm(0.975)*((sd(as.numeric(M0_r2Nagelkerke)))/sqrt(Nboot)))

M0_All[1+nVariav_M0_Desag+1,31] = mean(M0_aic)
M0_All[1+nVariav_M0_Desag+1,32] = mean(M0_aic) - (qnorm(0.975)*((sd(as.numeric(M0_aic)))/sqrt(Nboot)))
M0_All[1+nVariav_M0_Desag+1,33] = mean(M0_aic) + (qnorm(0.975)*((sd(as.numeric(M0_aic)))/sqrt(Nboot)))


write.csv2 (M0_All,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_2010_M0.csv")





########################################### MM - M1 =================================
rm(list=ls()[! ls() %in% c("pts_data", "data")])

Ano = 2010
Nboot = 1000
#Nboot=2
Nsamples = 1000

Variav_M1 = c("ID", "Class",
              "Rn0d14d",
              "Slope","Veg2010",
              "MnI2010",
              "Sbn2010",
              "Dm2010")
nVariav_M1 = length(Variav_M1)-1
Variav_M1_Desag = c("Rn0d14d",
                    "Slope","Veg2010",
                    "MnI2010",
                    "Sbn2010","Sbn20101",
                    "Dm2010")
nVariav_M1_Desag = length(Variav_M1_Desag)

data_M1 = data.table(data[,Variav_M1])
setkeyv(data_M1,c("Class"))
data_M1 = na.omit(data_M1)
table(data_M1$Class)

M1_Samples_train = data.frame(Boot=1:Nboot, Samples=NA)

M1_coef = data.frame(matrix(NA, ncol=Nboot, nrow=1+nVariav_M1_Desag), row.names=c("(Intercept)",Variav_M1_Desag))
colnames(M1_coef) = paste("Boot",1:Nboot, sep=" ")
M1_coef.se = M1_coef
M1_coef.pvalue = M1_coef
M1_oddsratio = M1_coef

M1v_LogLik_lM1v = M1_coef
M1v_LogLik_l0 = M1_coef
M1v_LRchi2_M1vm0 = M1_coef
M1v_LRchi2_M1vm0.pvalue = M1_coef
M1v_aic = M1_coef

M1_LogLik_lm0 = as.numeric() 
M1_LogLik_lM1 = as.numeric() 
M1_LRchi2_M1m0 = as.numeric()
M1_LRchi2_M1m0.pvalue = as.numeric()
M1_r2Nagelkerke = as.numeric()
M1_aic = as.numeric()

topAIC = data.frame(matrix(NA, ncol=5, nrow=0))
colnames(topAIC) = c("Boot","AIC_Rank","Model","AIC","Weights" )
topBIC = data.frame(matrix(NA, ncol=5, nrow=0))
colnames(topBIC) = c("Boot","BIC_Rank","Model","BIC","Weights" ) 

incr = list(Rn0d14d=1,Slope=1,Veg2010=0.05,MnI2010=1000, Sbn20101=1, Dm2010=10)

#b=1
for (b in (1:Nboot)) {
  tmp_M1 = rbind((data_M1[data_M1$Class==1,][sample(nrow(data_M1[data_M1$Class==1,]), Nsamples, replace=F),]),
                 (data_M1[data_M1$Class==0,][sample(nrow(data_M1[data_M1$Class==0,]), Nsamples, replace=F),]))
  Samples_train = as.vector(tmp_M1$ID)
  M1_Samples_train[b,2] = paste(Samples_train,collapse="-")
  tmp_M1 = tmp_M1[,-1]
  m_M1=glm(formula=Class ~ ., family=binomial(link="logit"), data=tmp_M1)
  tmp_M1_coef = as.data.frame(coef(summary(m_M1))[,1])
  for (v in (1:(nrow(tmp_M1_coef)))) {M1_coef[rownames(tmp_M1_coef)[v],b] = tmp_M1_coef[rownames(tmp_M1_coef)[v],1]}
  tmp_M1_coef.se = as.data.frame(coef(summary(m_M1))[,2])
  for (v in (1:(nrow(tmp_M1_coef.se)))) {M1_coef.se[rownames(tmp_M1_coef.se)[v],b] = tmp_M1_coef.se[rownames(tmp_M1_coef.se)[v],1]}
  tmp_M1_coef.pvalue = as.data.frame(coef(summary(m_M1))[,4])
  for (v in (1:nrow(tmp_M1_coef.pvalue))) {M1_coef.pvalue[rownames(tmp_M1_coef.pvalue)[v],b] = tmp_M1_coef.pvalue[rownames(tmp_M1_coef.pvalue)[v],1]}
  tmp_M1_lrtest = lrtest(m_M1) 
  M1_LogLik_lM1 [b] = as.numeric(tmp_M1_lrtest[1,2])
  M1_LogLik_lm0 [b] = as.numeric(tmp_M1_lrtest[2,2])
  M1_LRchi2_M1m0 [b] = as.numeric(tmp_M1_lrtest[2,4])
  M1_LRchi2_M1m0.pvalue [b] = as.numeric(tmp_M1_lrtest[2,5])
  tmp_M1_pR2 = pR2(m_M1)
  M1_r2Nagelkerke[b] = as.numeric(tmp_M1_pR2[6])
  M1_aic[b] = as.numeric(m_M1$aic)
  or = or_glm(tmp_M1, m_M1, incr, CI = 0.95)
  or = as.data.frame(or)
  rownames(or) = or[,1]
  for (v in (1:nrow(or))) {M1_oddsratio[rownames(or)[v],b] = or[rownames(or)[v],"oddsratio"]}
  for (v in (1:(nVariav_M1-1))){
    variav = c("Class", Variav_M1[v+2])
    tmp_M1v = tmp_M1[,..variav]
    m_M1v=glm(formula=Class ~ ., family=binomial(link="logit"), data=tmp_M1v)  
    tmp_M1_lrtest_M1vm0 = lrtest(m_M1v)
    M1v_LogLik_lM1v [variav[-1],b] = as.numeric(tmp_M1_lrtest_M1vm0[1,2])
    M1v_LogLik_l0 [variav[-1],b] = as.numeric(tmp_M1_lrtest_M1vm0[2,2])
    M1v_LRchi2_M1vm0 [variav[-1],b] = as.numeric(tmp_M1_lrtest_M1vm0[2,4])
    M1v_LRchi2_M1vm0.pvalue [variav[-1],b] = as.numeric(tmp_M1_lrtest_M1vm0[2,5])
    M1v_aic[variav[-1],b] = as.numeric(m_M1v$aic)
  }
  AIC_f1 = glmulti(Class ~ ., data=tmp_M1, family=binomial, 
                        level=1, crit=aic, fitfunc=glm, method="h",
                        confsetsize=256, plotty=T, report=TRUE, name="AIC_glmulti")
  tmp_M1_topAIC <- glmulti::weightable(AIC_f1)
  tmp_M1_topAIC$Boot = b
  tmp_M1_topAIC$AIC_Rank = c(1:nrow(tmp_M1_topAIC))
  colnames(tmp_M1_topAIC) = c("Model","AIC","Weights","Boot","AIC_Rank")
  topAIC = rbind(topAIC,tmp_M1_topAIC)
  BIC_f1 = glmulti(Class ~ ., data=tmp_M1, family=binomial, 
                   level=1, crit=bic, fitfunc=glm, method="h",
                   confsetsize=256, plotty=T, report=TRUE, name="BIC_glmulti")
  tmp_M1_topBIC <- glmulti::weightable(BIC_f1)
  tmp_M1_topBIC$Boot = b
  tmp_M1_topBIC$BIC_Rank = c(1:nrow(tmp_M1_topBIC))
  topBIC = rbind(topBIC,tmp_M1_topBIC)
  print(paste("Boot=", b, " - Variav=", variav[-1]))
}

write.csv2 (M1_Samples_train,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1_Samples_train.csv")
write.csv2 (M1_coef,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1_coef.csv")
write.csv2 (M1_coef.se,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1_coef.se.csv")
write.csv2 (M1_coef.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1_coef.pvalue.csv")
write.csv2 (M1_LogLik_lm0,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1_LogLik_l0.csv")
write.csv2 (M1_LogLik_lM1,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1_LogLik_lM1.csv")
write.csv2 (M1_LRchi2_M1m0,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1_LRchi2_M1m0.csv")
write.csv2 (M1_LRchi2_M1m0.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1_LRchi2_M1m0.pvalue.csv")
write.csv2 (M1_r2Nagelkerke,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1_r2Nagelkerke.csv")
write.csv2 (M1v_LogLik_lM1v,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1v_LogLik_lM1v.csv")
write.csv2 (M1v_LogLik_l0,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1v_LogLik_l0.csv")
write.csv2 (M1v_LRchi2_M1vm0,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1v_LRchi2_M1vm0.csv")
write.csv2 (M1v_LRchi2_M1vm0.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1v_LRchi2_M1vm0.pvalue.csv")
write.csv2 (M1_aic,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1_aic.csv")
write.csv2 (M1v_aic,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1v_aic.csv")
write.csv2 (topAIC, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1_AICf1.csv")
write.csv2 (topBIC, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1_BICf1.csv")
write.csv2 (M1_oddsratio, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1_oddsratio.csv")

VarEstatAll_M = c("mean.coef_param","CIinf mean.coef_param","CIsup coef_param",
                  "mean.coef.p-value","CIinf mean.coef.p-value","CIsup mean.coef.p-value",
                  "mean.LL0","CIinf mean.LL0","CIsup mean.LL0",
                  "mean.LLM0","CIinf mean.LLM0","CIsup mean.LLM0",
                  "mean.LLMx","CIinf mean.LLMx","CIsup mean.LLMx",
                  "mean.LRchi2_MXm0","CIinf mean.LRchi2_MXm0","CIsup mean_MXm0.LRchi2",
                  "mean.LRchi2_MXM0","CIinf mean.LRchi2_MXM0","CIsup mean.LRchi2_MXM0",
                  "mean.LRchi2_MXm0.pvalue","CIinf mean.LRchi2_MXm0.pvalue","CIsup mean_MXm0.LRchi2.pvalue",
                  "mean.LRchi2_MXM0.pvalue","CIinf mean.LRchi2_MXM0.pvalue","CIsup mean.LRchi2_MXM0.pvalue",
                  "mean.r2Nagelkerke","CIinf mean.r2Nagelkerke", "CIsup mean.r2Nagelkerke",
                  "mean.aic","CIinf mean.aic", "CIsup mean.aic",
                  "mean.oddsratio","CIinf mean.oddsratio", "CIsup mean.oddsratio")

M1_All = data.frame(matrix(NA, ncol=length(VarEstatAll_M), nrow=1+nVariav_M1_Desag+1))
colnames(M1_All) = c(VarEstatAll_M)
rownames(M1_All) = c("(Intercept)",Variav_M1_Desag,"model M1")


for (v in 1:nrow(M1_coef)) {
  M1_All[v,1] = mean(as.numeric(M1_coef[v,]))
  M1_All[v,2] = mean(as.numeric(M1_coef[v,])) - (qnorm(0.975)*((sd(as.numeric(M1_coef[v,])))/sqrt(Nboot)))
  M1_All[v,3] = mean(as.numeric(M1_coef[v,])) + (qnorm(0.975)*((sd(as.numeric(M1_coef[v,])))/sqrt(Nboot)))
  M1_All[v,4] = mean(as.numeric(M1_coef.pvalue[v,]))
  M1_All[v,5] = mean(as.numeric(M1_coef.pvalue[v,])) - (qnorm(0.975)*((sd(as.numeric(M1_coef.pvalue[v,])))/sqrt(Nboot)))
  M1_All[v,6] = mean(as.numeric(M1_coef.pvalue[v,])) + (qnorm(0.975)*((sd(as.numeric(M1_coef.pvalue[v,])))/sqrt(Nboot)))
  
  M1_All[v,7] = mean(as.numeric(M1v_LogLik_l0[v,]))
  M1_All[v,8] = mean(as.numeric(M1v_LogLik_l0[v,])) - (qnorm(0.975)*((sd(as.numeric(M1v_LogLik_l0[v,])))/sqrt(Nboot)))
  M1_All[v,9] = mean(as.numeric(M1v_LogLik_l0[v,])) + (qnorm(0.975)*((sd(as.numeric(M1v_LogLik_l0[v,])))/sqrt(Nboot)))
  M1_All[v,13] = mean(as.numeric(M1v_LogLik_lM1v[v,]))
  M1_All[v,14] = mean(as.numeric(M1v_LogLik_lM1v[v,])) - (qnorm(0.975)*((sd(as.numeric(M1v_LogLik_lM1v[v,])))/sqrt(Nboot)))
  M1_All[v,15] = mean(as.numeric(M1v_LogLik_lM1v[v,])) + (qnorm(0.975)*((sd(as.numeric(M1v_LogLik_lM1v[v,])))/sqrt(Nboot)))
  
  M1_All[v,16] = mean(as.numeric(M1v_LRchi2_M1vm0[v,]))
  M1_All[v,17] = mean(as.numeric(M1v_LRchi2_M1vm0[v,])) - (qnorm(0.975)*((sd(as.numeric(M1v_LRchi2_M1vm0[v,])))/sqrt(Nboot)))
  M1_All[v,18] = mean(as.numeric(M1v_LRchi2_M1vm0[v,])) + (qnorm(0.975)*((sd(as.numeric(M1v_LRchi2_M1vm0[v,])))/sqrt(Nboot)))
  
  M1_All[v,22] = mean(as.numeric(M1v_LRchi2_M1vm0.pvalue[v,]))
  M1_All[v,23] = mean(as.numeric(M1v_LRchi2_M1vm0.pvalue[v,])) - (qnorm(0.975)*((sd(as.numeric(M1v_LRchi2_M1vm0.pvalue[v,])))/sqrt(Nboot)))
  M1_All[v,24] = mean(as.numeric(M1v_LRchi2_M1vm0.pvalue[v,])) + (qnorm(0.975)*((sd(as.numeric(M1v_LRchi2_M1vm0.pvalue[v,])))/sqrt(Nboot)))
  
  M1_All[v,31] = mean(as.numeric(M1v_aic[v,]))
  M1_All[v,32] = mean(as.numeric(M1v_aic[v,])) - (qnorm(0.975)*((sd(as.numeric(M1v_aic[v,])))/sqrt(Nboot)))
  M1_All[v,33] = mean(as.numeric(M1v_aic[v,])) + (qnorm(0.975)*((sd(as.numeric(M1v_aic[v,])))/sqrt(Nboot)))
  
  M1_All[v,34] = mean(as.numeric(M1_oddsratio[v,]))
  M1_All[v,35] = mean(as.numeric(M1_oddsratio[v,])) - (qnorm(0.975)*((sd(as.numeric(M1_oddsratio[v,])))/sqrt(Nboot)))
  M1_All[v,36] = mean(as.numeric(M1_oddsratio[v,])) + (qnorm(0.975)*((sd(as.numeric(M1_oddsratio[v,])))/sqrt(Nboot)))
  
  print(v)
}

M1_All[1+nVariav_M1_Desag+1,7] = mean(M1_LogLik_lm0)
M1_All[1+nVariav_M1_Desag+1,8] = mean(M1_LogLik_lm0) - (qnorm(0.975)*((sd(as.numeric(M1_LogLik_lm0)))/sqrt(Nboot)))
M1_All[1+nVariav_M1_Desag+1,9] = mean(M1_LogLik_lm0) + (qnorm(0.975)*((sd(as.numeric(M1_LogLik_lm0)))/sqrt(Nboot)))
M1_All[1+nVariav_M1_Desag+1,13] = mean(M1_LogLik_lM1)
M1_All[1+nVariav_M1_Desag+1,14] = mean(M1_LogLik_lM1) - (qnorm(0.975)*((sd(as.numeric(M1_LogLik_lM1)))/sqrt(Nboot)))
M1_All[1+nVariav_M1_Desag+1,15] = mean(M1_LogLik_lM1) + (qnorm(0.975)*((sd(as.numeric(M1_LogLik_lM1)))/sqrt(Nboot)))

M1_All[1+nVariav_M1_Desag+1,16] = mean(M1_LRchi2_M1m0)
M1_All[1+nVariav_M1_Desag+1,17] = mean(M1_LRchi2_M1m0) - (qnorm(0.975)*((sd(as.numeric(M1_LRchi2_M1m0)))/sqrt(Nboot)))
M1_All[1+nVariav_M1_Desag+1,18] = mean(M1_LRchi2_M1m0) + (qnorm(0.975)*((sd(as.numeric(M1_LRchi2_M1m0)))/sqrt(Nboot)))

M1_All[1+nVariav_M1_Desag+1,22] = mean(M1_LRchi2_M1m0.pvalue)
M1_All[1+nVariav_M1_Desag+1,23] = mean(M1_LRchi2_M1m0.pvalue) - (qnorm(0.975)*((sd(as.numeric(M1_LRchi2_M1m0.pvalue)))/sqrt(Nboot)))
M1_All[1+nVariav_M1_Desag+1,24] = mean(M1_LRchi2_M1m0.pvalue) + (qnorm(0.975)*((sd(as.numeric(M1_LRchi2_M1m0.pvalue)))/sqrt(Nboot)))

M1_All[1+nVariav_M1_Desag+1,28] = mean(M1_r2Nagelkerke)
M1_All[1+nVariav_M1_Desag+1,29] = mean(M1_r2Nagelkerke) - (qnorm(0.975)*((sd(as.numeric(M1_r2Nagelkerke)))/sqrt(Nboot)))
M1_All[1+nVariav_M1_Desag+1,30] = mean(M1_r2Nagelkerke) + (qnorm(0.975)*((sd(as.numeric(M1_r2Nagelkerke)))/sqrt(Nboot)))

M1_All[1+nVariav_M1_Desag+1,31] = mean(M1_aic)
M1_All[1+nVariav_M1_Desag+1,32] = mean(M1_aic) - (qnorm(0.975)*((sd(as.numeric(M1_aic)))/sqrt(Nboot)))
M1_All[1+nVariav_M1_Desag+1,33] = mean(M1_aic) + (qnorm(0.975)*((sd(as.numeric(M1_aic)))/sqrt(Nboot)))

write.csv2 (M1_All,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_2010_M1.csv")




########################################### MM - M2 =================================
Variav_M0 = c("ID", "Class",
              "Rn0d14d","Slope",
              "Suscep",
              "Veg2010","VegDif",
              "MnI2010","Sbn2010","SubnChn",
              "Dm2010","DmDif",
              "PSMF10","PSBL10","PEsgCA10", "PSEsg10")
nVariav_M0 = length(Variav_M0)-2
Variav_M0_Desag = c("Rn0d14d","Slope",
                    "Suscep","Suscep2","Suscep3",
                    "Veg2010","VegDif",
                    "MnI2010","Sbn2010","Sbn20101","SubnChn","SubnChn1",
                    "Dm2010","DmDif",
                    "PSMF10","PSBL10","PEsgCA10", "PSEsg10")
nVariav_M0_Desag = length(Variav_M0_Desag)

data_M0 = data.table(data[,Variav_M0])
setkeyv(data_M0,c("Class"))
table(data_M0$Class)

Variav_M2 = c("Class",
              "Rn0d14d","Slope","Veg2010",
              "MnI2010","Sbn2010",
              "Dm2010","PSEsg10","PSMF10")
nVariav_M2 = length(Variav_M2)-1
Variav_M2_Desag = c("Rn0d14d","Slope","Veg2010",
                    "MnI2010","Sbn20101",
                    "Dm2010","PSEsg10","PSMF10")
nVariav_M2_Desag = length(Variav_M2_Desag)

M2_Samples_train = data.frame(Boot=1:Nboot, Samples=NA)

M2_coef = data.frame(matrix(NA, ncol=Nboot, nrow=1+nVariav_M2_Desag), row.names=c("(Intercept)",Variav_M2_Desag))
colnames(M2_coef) = paste("Boot",1:Nboot, sep=" ")
M2_coef.se = M2_coef
M2_coef.pvalue = M2_coef

M2v_LogLik_lM2v = M2_coef
M2v_LogLik_l0 = M2_coef
M2v_LRchi2_M2vm0 = M2_coef
M2v_LRchi2_M2vm0.pvalue = M2_coef
M2v_aic = M2_coef

M2_LogLik_l0 = as.numeric() 
M2_LogLik_lM0 = as.numeric() 
M2_LogLik_lM2 = as.numeric() 
M2_LRchi2_M2m0 = as.numeric()
M2_LRchi2_M2m0.pvalue = as.numeric()
M2_LRchi2_M2 = as.numeric()
M2_LRchi2_M2.pvalue = as.numeric()
M2_r2Nagelkerke = as.numeric()
M2_aic = as.numeric()

for (b in (1:Nboot)) {
  tmp_M0 = rbind((data_M0[data_M0$Class==1,][sample(nrow(data_M0[data_M0$Class==1,]), Nsamples, replace=F),]),
                 (data_M0[data_M0$Class==0,][sample(nrow(data_M0[data_M0$Class==0,]), Nsamples, replace=F),]))
  tmp_M0 = na.omit(tmp_M0)
  Samples_train = as.vector(tmp_M0$ID)
  M2_Samples_train[b,2] = paste(Samples_train,collapse="-")
  tmp_M0 = tmp_M0[,-1]
  m_M0 = glm(formula=Class ~ ., family=binomial(link="logit"), data=tmp_M0)
  tmp_M2 = tmp_M0[,..Variav_M2]
  m_M2=glm(formula=Class ~ ., family=binomial(link="logit"), data=tmp_M2)
  tmp_M2_coef = as.data.frame(coef(summary(m_M2))[,1])
  for (v in (1:(nrow(tmp_M2_coef)))) {M2_coef[rownames(tmp_M2_coef)[v],b] = tmp_M2_coef[rownames(tmp_M2_coef)[v],1]}
  tmp_M2_coef.se = as.data.frame(coef(summary(m_M2))[,2])
  for (v in (1:(nrow(tmp_M2_coef.se)))) {M2_coef.se[rownames(tmp_M2_coef.se)[v],b] = tmp_M2_coef.se[rownames(tmp_M2_coef.se)[v],1]}
  tmp_M2_coef.pvalue = as.data.frame(coef(summary(m_M2))[,4])
  for (v in (1:nrow(tmp_M2_coef.pvalue))) {M2_coef.pvalue[rownames(tmp_M2_coef.pvalue)[v],b] = tmp_M2_coef.pvalue[rownames(tmp_M2_coef.pvalue)[v],1]}
  tmp_M2_lrtest_0Mm0 = lrtest(m_M2) 
  M2_LogLik_lM2 [b] = as.numeric(tmp_M2_lrtest_0Mm0[1,2])
  M2_LogLik_l0 [b] = as.numeric(tmp_M2_lrtest_0Mm0[2,2])
  M2_LRchi2_M2m0 [b] = as.numeric(tmp_M2_lrtest_0Mm0[2,4])
  M2_LRchi2_M2m0.pvalue [b] = as.numeric(tmp_M2_lrtest_0Mm0[2,5])
  tmp_M2_lrtest_M2 = lrtest(m_M0, m_M2) 
  M2_LogLik_lM0 [b] = as.numeric(tmp_M2_lrtest_M2[1,2])
  M2_LRchi2_M2 [b] = as.numeric(tmp_M2_lrtest_M2[2,4])
  M2_LRchi2_M2.pvalue [b] = as.numeric(tmp_M2_lrtest_M2[2,5])
  tmp_M2_pR2 = pR2(m_M2)
  M2_r2Nagelkerke[b] = as.numeric(tmp_M2_pR2[6])
  M2_aic[b] = as.numeric(m_M2$aic)
  for (v in (1:nVariav_M2)){
    variav = c("Class", Variav_M2[v+1])
    tmp_M2v = tmp_M2[,..variav]
    m_M2v=glm(formula=Class ~ ., family=binomial(link="logit"), data=tmp_M2v)  
    tmp_M2_lrtest_M2vm0 = lrtest(m_M2v)
    M2v_LogLik_lM2v [variav[-1],b] = as.numeric(tmp_M2_lrtest_M2vm0[1,2])
    M2v_LogLik_l0 [variav[-1],b] = as.numeric(tmp_M2_lrtest_M2vm0[2,2])
    M2v_LRchi2_M2vm0 [variav[-1],b] = as.numeric(tmp_M2_lrtest_M2vm0[2,4])
    M2v_LRchi2_M2vm0.pvalue [variav[-1],b] = as.numeric(tmp_M2_lrtest_M2vm0[2,5])
    M2v_aic[variav[-1],b] = as.numeric(m_M2v$aic)
  }
  print(paste("Boot=", b, " - Variav=", variav[-1]))
}

write.csv2 (M2_Samples_train,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM2_Samples_train.csv")
write.csv2 (M2_coef,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM2_coef.csv")
write.csv2 (M2_coef.se,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM2_coef.se.csv")
write.csv2 (M2_coef.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM2_coef.pvalue.csv")
write.csv2 (M2_LogLik_l0,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM2_LogLik_l0.csv")
write.csv2 (M2_LogLik_lM2,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM2_LogLik_lM2.csv")
write.csv2 (M2_LRchi2_M2m0,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM2_LRchi2_M2m0.csv")
write.csv2 (M2_LRchi2_M2m0.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM2_LRchi2_M2m0.pvalue.csv")
write.csv2 (M2_r2Nagelkerke,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM2_r2Nagelkerke.csv")
write.csv2 (M2v_LogLik_lM2v,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM2v_LogLik_lM2v.csv")
write.csv2 (M2v_LogLik_l0,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM2v_LogLik_l0.csv")
write.csv2 (M2v_LRchi2_M2vm0,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM2v_LRchi2_M2vm0.csv")
write.csv2 (M2v_LRchi2_M2vm0.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM2v_LRchi2_M2vm0.pvalue.csv")
write.csv2 (M2_aic,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM2_aic.csv")
write.csv2 (M2v_aic,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM2v_aic.csv")

VarEstatAll_M = c("mean.coef_param","CIinf mean.coef_param","CIsup coef_param",
                  "mean.coef.p-value","CIinf mean.coef.p-value","CIsup mean.coef.p-value",
                  "mean.LL0","CIinf mean.LL0","CIsup mean.LL0",
                  "mean.LLM0","CIinf mean.LLM0","CIsup mean.LLM0",
                  "mean.LLMx","CIinf mean.LLMx","CIsup mean.LLMx",
                  "mean.LRchi2_MXm0","CIinf mean.LRchi2_MXm0","CIsup mean_MXm0.LRchi2",
                  "mean.LRchi2_MXM0","CIinf mean.LRchi2_MXM0","CIsup mean.LRchi2_MXM0",
                  "mean.LRchi2_MXm0.pvalue","CIinf mean.LRchi2_MXm0.pvalue","CIsup mean_MXm0.LRchi2.pvalue",
                  "mean.LRchi2_MXM0.pvalue","CIinf mean.LRchi2_MXM0.pvalue","CIsup mean.LRchi2_MXM0.pvalue",
                  "mean.r2Nagelkerke","CIinf mean.r2Nagelkerke", "CIsup mean.r2Nagelkerke",
                  "mean.aic","CIinf mean.aic", "CIsup mean.aic")

M2_All = data.frame(matrix(NA, ncol=length(VarEstatAll_M), nrow=1+nVariav_M2_Desag+1))
colnames(M2_All) = c(VarEstatAll_M)
rownames(M2_All) = c("(Intercept)",Variav_M2_Desag,"model M2")

for (v in 1:nrow(M2_coef)) {
  M2_All[v,1] = mean(as.numeric(M2_coef[v,]))
  M2_All[v,2] = mean(as.numeric(M2_coef[v,])) - (qnorm(0.975)*((sd(as.numeric(M2_coef[v,])))/sqrt(Nboot)))
  M2_All[v,3] = mean(as.numeric(M2_coef[v,])) + (qnorm(0.975)*((sd(as.numeric(M2_coef[v,])))/sqrt(Nboot)))
  M2_All[v,4] = mean(as.numeric(M2_coef.pvalue[v,]))
  M2_All[v,5] = mean(as.numeric(M2_coef.pvalue[v,])) - (qnorm(0.975)*((sd(as.numeric(M2_coef.pvalue[v,])))/sqrt(Nboot)))
  M2_All[v,6] = mean(as.numeric(M2_coef.pvalue[v,])) + (qnorm(0.975)*((sd(as.numeric(M2_coef.pvalue[v,])))/sqrt(Nboot)))
  
  M2_All[v,7] = mean(as.numeric(M2v_LogLik_l0[v,]))
  M2_All[v,8] = mean(as.numeric(M2v_LogLik_l0[v,])) - (qnorm(0.975)*((sd(as.numeric(M2v_LogLik_l0[v,])))/sqrt(Nboot)))
  M2_All[v,9] = mean(as.numeric(M2v_LogLik_l0[v,])) + (qnorm(0.975)*((sd(as.numeric(M2v_LogLik_l0[v,])))/sqrt(Nboot)))
  M2_All[v,10] = mean(as.numeric(M2v_LogLik_lM2v[v,]))
  M2_All[v,11] = mean(as.numeric(M2v_LogLik_lM2v[v,])) - (qnorm(0.975)*((sd(as.numeric(M2v_LogLik_lM2v[v,])))/sqrt(Nboot)))
  M2_All[v,12] = mean(as.numeric(M2v_LogLik_lM2v[v,])) + (qnorm(0.975)*((sd(as.numeric(M2v_LogLik_lM2v[v,])))/sqrt(Nboot)))
  
  M2_All[v,16] = mean(as.numeric(M2v_LRchi2_M2vm0[v,]))
  M2_All[v,17] = mean(as.numeric(M2v_LRchi2_M2vm0[v,])) - (qnorm(0.975)*((sd(as.numeric(M2v_LRchi2_M2vm0[v,])))/sqrt(Nboot)))
  M2_All[v,18] = mean(as.numeric(M2v_LRchi2_M2vm0[v,])) + (qnorm(0.975)*((sd(as.numeric(M2v_LRchi2_M2vm0[v,])))/sqrt(Nboot)))
  
  M2_All[v,22] = mean(as.numeric(M2v_LRchi2_M2vm0.pvalue[v,]))
  M2_All[v,23] = mean(as.numeric(M2v_LRchi2_M2vm0.pvalue[v,])) - (qnorm(0.975)*((sd(as.numeric(M2v_LRchi2_M2vm0.pvalue[v,])))/sqrt(Nboot)))
  M2_All[v,24] = mean(as.numeric(M2v_LRchi2_M2vm0.pvalue[v,])) + (qnorm(0.975)*((sd(as.numeric(M2v_LRchi2_M2vm0.pvalue[v,])))/sqrt(Nboot)))
  
  M2_All[v,31] = mean(as.numeric(M2v_aic[v,]))
  M2_All[v,32] = mean(as.numeric(M2v_aic[v,])) - (qnorm(0.975)*((sd(as.numeric(M2v_aic[v,])))/sqrt(Nboot)))
  M2_All[v,33] = mean(as.numeric(M2v_aic[v,])) + (qnorm(0.975)*((sd(as.numeric(M2v_aic[v,])))/sqrt(Nboot)))
  
  print(v)
}

M2_All[1+nVariav_M2_Desag+1,7] = mean(M2_LogLik_l0)
M2_All[1+nVariav_M2_Desag+1,8] = mean(M2_LogLik_l0) - (qnorm(0.975)*((sd(as.numeric(M2_LogLik_l0)))/sqrt(Nboot)))
M2_All[1+nVariav_M2_Desag+1,9] = mean(M2_LogLik_l0) + (qnorm(0.975)*((sd(as.numeric(M2_LogLik_l0)))/sqrt(Nboot)))
M2_All[1+nVariav_M2_Desag+1,10] = mean(M2_LogLik_lM0)
M2_All[1+nVariav_M2_Desag+1,11] = mean(M2_LogLik_lM0) - (qnorm(0.975)*((sd(as.numeric(M2_LogLik_lM0)))/sqrt(Nboot)))
M2_All[1+nVariav_M2_Desag+1,12] = mean(M2_LogLik_lM0) + (qnorm(0.975)*((sd(as.numeric(M2_LogLik_lM0)))/sqrt(Nboot)))
M2_All[1+nVariav_M2_Desag+1,13] = mean(M2_LogLik_lM2)
M2_All[1+nVariav_M2_Desag+1,14] = mean(M2_LogLik_lM2) - (qnorm(0.975)*((sd(as.numeric(M2_LogLik_lM2)))/sqrt(Nboot)))
M2_All[1+nVariav_M2_Desag+1,15] = mean(M2_LogLik_lM2) + (qnorm(0.975)*((sd(as.numeric(M2_LogLik_lM2)))/sqrt(Nboot)))

M2_All[1+nVariav_M2_Desag+1,16] = mean(M2_LRchi2_M2m0)
M2_All[1+nVariav_M2_Desag+1,17] = mean(M2_LRchi2_M2m0) - (qnorm(0.975)*((sd(as.numeric(M2_LRchi2_M2m0)))/sqrt(Nboot)))
M2_All[1+nVariav_M2_Desag+1,18] = mean(M2_LRchi2_M2m0) + (qnorm(0.975)*((sd(as.numeric(M2_LRchi2_M2m0)))/sqrt(Nboot)))
M2_All[1+nVariav_M2_Desag+1,19] = mean(M2_LRchi2_M2)
M2_All[1+nVariav_M2_Desag+1,20] = mean(M2_LRchi2_M2) - (qnorm(0.975)*((sd(as.numeric(M2_LRchi2_M2)))/sqrt(Nboot)))
M2_All[1+nVariav_M2_Desag+1,21] = mean(M2_LRchi2_M2) + (qnorm(0.975)*((sd(as.numeric(M2_LRchi2_M2)))/sqrt(Nboot)))

M2_All[1+nVariav_M2_Desag+1,22] = mean(M2_LRchi2_M2m0.pvalue)
M2_All[1+nVariav_M2_Desag+1,23] = mean(M2_LRchi2_M2m0.pvalue) - (qnorm(0.975)*((sd(as.numeric(M2_LRchi2_M2m0.pvalue)))/sqrt(Nboot)))
M2_All[1+nVariav_M2_Desag+1,24] = mean(M2_LRchi2_M2m0.pvalue) + (qnorm(0.975)*((sd(as.numeric(M2_LRchi2_M2m0.pvalue)))/sqrt(Nboot)))
M2_All[1+nVariav_M2_Desag+1,25] = mean(M2_LRchi2_M2.pvalue)
M2_All[1+nVariav_M2_Desag+1,26] = mean(M2_LRchi2_M2.pvalue) - (qnorm(0.975)*((sd(as.numeric(M2_LRchi2_M2.pvalue)))/sqrt(Nboot)))
M2_All[1+nVariav_M2_Desag+1,27] = mean(M2_LRchi2_M2.pvalue) + (qnorm(0.975)*((sd(as.numeric(M2_LRchi2_M2.pvalue)))/sqrt(Nboot)))

M2_All[1+nVariav_M2_Desag+1,28] = mean(M2_r2Nagelkerke)
M2_All[1+nVariav_M2_Desag+1,29] = mean(M2_r2Nagelkerke) - (qnorm(0.975)*((sd(as.numeric(M2_r2Nagelkerke)))/sqrt(Nboot)))
M2_All[1+nVariav_M2_Desag+1,30] = mean(M2_r2Nagelkerke) + (qnorm(0.975)*((sd(as.numeric(M2_r2Nagelkerke)))/sqrt(Nboot)))

M2_All[1+nVariav_M2_Desag+1,31] = mean(M2_aic)
M2_All[1+nVariav_M2_Desag+1,32] = mean(M2_aic) - (qnorm(0.975)*((sd(as.numeric(M2_aic)))/sqrt(Nboot)))
M2_All[1+nVariav_M2_Desag+1,33] = mean(M2_aic) + (qnorm(0.975)*((sd(as.numeric(M2_aic)))/sqrt(Nboot)))

write.csv2 (M2_All,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_2010_M2.csv")




########################################### MM - M2 glmulti =================================
library(glmulti)
data_glmulti = data.table(data[,c("ID",Variav_M2)])
tmp_glmulti = rbind((data_glmulti[data_glmulti$Class==1,][sample(nrow(data_glmulti[data_glmulti$Class==1,]), Nsamples, replace=F),]),
                    (data_glmulti[data_glmulti$Class==0,][sample(nrow(data_glmulti[data_glmulti$Class==0,]), Nsamples, replace=F),]))
tmp_glmulti = na.omit(tmp_glmulti)
M1_glmulti_Samples_train = as.vector(tmp_glmulti$ID)
tmp_glmulti = tmp_glmulti [,-1]

AIC_diagn_f1 = glmulti(Class ~ ., data=tmp_glmulti, family=binomial, 
                       level=1, crit=aic, fitfunc=glm, method="d", minsize=2, marginality=T,
                       confsetsize=256, plotty=T, report=TRUE, name="AIC_glmulti")
AIC_best_f1 = glmulti(Class ~ ., data=tmp_glmulti, family=binomial, 
                      level=1, crit=aic, fitfunc=glm, method="h",
                      confsetsize=256, plotty=T, report=TRUE, name="AIC_glmulti")
summary(AIC_best_f1)
#weightable(AIC_best_f1)
#print(AIC_best_f1)
plot(AIC_best_f1)
top <- weightable(AIC_best_f1)
AIC_bestmodel_f1 = summary(AIC_best_f1@objects[[1]])
AIC_bestmodel_f1 
plot(AIC_best_f1, type="s")

write.csv2 (top, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_2010_M2_GLMulti_AICf1.csv")

BIC_diagn_f1 = glmulti(Class ~ ., data=tmp_glmulti, family=binomial, 
                       level=1, crit=bic, fitfunc=glm, method="d", minsize=2, marginality=T,
                       confsetsize=256, plotty=d, report=TRUE, name="BIC_glmulti")
BIC_best_f1 = glmulti(Class ~ ., data=tmp_glmulti, family=binomial, 
                      level=1, crit=bic, fitfunc=glm, method="h",
                      confsetsize=256, plotty=T, report=TRUE, name="BIC_glmulti")
summary(BIC_best_f1)
#weightable(BIC_best_f1)
#print(BIC_best_f1)
plot(BIC_best_f1)
top <- weightable(BIC_best_f1)
BIC_bestmodel_f1 = summary(BIC_best_f1@objects[[1]])
BIC_bestmodel_f1
plot(BIC_best_f1, type="s")

write.csv2 (top, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_2010_M2_GLMulti_BICf1.csv")

# AIC_best_f2 = glmulti(Class ~ ., data=tmp_glmulti, family=binomial, 
#                       level=2, crit=aic, fitfunc=glm, method="h",
#                       confsetsize=256, plotty=T, report=TRUE, name="AIC_glmulti")



########################################### MM - M3 =================================
Variav_M1 = c("ID", "Class",
              "Rn0d14d",
              "Slope","Veg2010",
              "MnI2010",
              "Sbn2010",
              "Dm2010")
nVariav_M1 = length(Variav_M1)-1
Variav_M1_Desag = c("Rn0d14d",
                    "Slope","Veg2010",
                    "MnI2010",
                    "Sbn2010","Sbn20101",
                    "Dm2010")
nVariav_M1_Desag = length(Variav_M1_Desag)

data_M1 = data.table(data[,Variav_M1])
setkeyv(data_M1,c("Class"))
table(data_M1$Class)

Variav_M3 = c("Class",
              "Rn0d14d",
              "Slope","Veg2010",
              "Sbn2010",
              "Dm2010")
nVariav_M3 = length(Variav_M3)-1
Variav_M3_Desag = c("Rn0d14d",
                    "Slope","Veg2010",
                    "Sbn2010","Sbn20101",
                    "Dm2010")
nVariav_M3_Desag = length(Variav_M3_Desag)

M3_Samples_train = data.frame(Boot=1:Nboot, Samples=NA)

M3_coef = data.frame(matrix(NA, ncol=Nboot, nrow=1+nVariav_M3_Desag), row.names=c("(Intercept)",Variav_M3_Desag))
colnames(M3_coef) = paste("Boot",1:Nboot, sep=" ")
M3_coef.se = M3_coef
M3_coef.pvalue = M3_coef

M3v_LogLik_lM3v = M3_coef
M3v_LogLik_l0 = M3_coef
M3v_LRchi2_M3vm0 = M3_coef
M3v_LRchi2_M3vm0.pvalue = M3_coef
M3v_aic = M3_coef

M3_LogLik_l0 = as.numeric() 
M3_LogLik_lM1 = as.numeric() 
M3_LogLik_lM3 = as.numeric() 
M3_LRchi2_M3m0 = as.numeric()
M3_LRchi2_M3m0.pvalue = as.numeric()
M3_LRchi2_M3 = as.numeric()
M3_LRchi2_M3.pvalue = as.numeric()
M3_r2Nagelkerke = as.numeric()
M3_aic = as.numeric()

for (b in (1:Nboot)) {
  tmp_M1 = rbind((data_M1[data_M1$Class==1,][sample(nrow(data_M1[data_M1$Class==1,]), Nsamples, replace=F),]),
                 (data_M1[data_M1$Class==0,][sample(nrow(data_M1[data_M1$Class==0,]), Nsamples, replace=F),]))
  tmp_M1 = na.omit(tmp_M1)
  Samples_train = as.vector(tmp_M1$ID)
  M3_Samples_train[b,2] = paste(Samples_train,collapse="-")
  tmp_M1 = tmp_M1[,-1]
  m_M1 = glm(formula=Class ~ ., family=binomial(link="logit"), data=tmp_M1)
  tmp_M3 = tmp_M1[,..Variav_M3]
  m_M3=glm(formula=Class ~ ., family=binomial(link="logit"), data=tmp_M3)
  tmp_M3_coef = as.data.frame(coef(summary(m_M3))[,1])
  for (v in (1:(nrow(tmp_M3_coef)))) {M3_coef[rownames(tmp_M3_coef)[v],b] = tmp_M3_coef[rownames(tmp_M3_coef)[v],1]}
  tmp_M3_coef.se = as.data.frame(coef(summary(m_M3))[,2])
  for (v in (1:(nrow(tmp_M3_coef.se)))) {M3_coef.se[rownames(tmp_M3_coef.se)[v],b] = tmp_M3_coef.se[rownames(tmp_M3_coef.se)[v],1]}
  tmp_M3_coef.pvalue = as.data.frame(coef(summary(m_M3))[,4])
  for (v in (1:nrow(tmp_M3_coef.pvalue))) {M3_coef.pvalue[rownames(tmp_M3_coef.pvalue)[v],b] = tmp_M3_coef.pvalue[rownames(tmp_M3_coef.pvalue)[v],1]}
  tmp_M3_lrtest_0Mm0 = lrtest(m_M3) 
  M3_LogLik_lM3 [b] = as.numeric(tmp_M3_lrtest_0Mm0[1,2])
  M3_LogLik_l0 [b] = as.numeric(tmp_M3_lrtest_0Mm0[2,2])
  M3_LRchi2_M3m0 [b] = as.numeric(tmp_M3_lrtest_0Mm0[2,4])
  M3_LRchi2_M3m0.pvalue [b] = as.numeric(tmp_M3_lrtest_0Mm0[2,5])
  tmp_M3_lrtest_M3 = lrtest(m_M1, m_M3) 
  M3_LogLik_lM1 [b] = as.numeric(tmp_M3_lrtest_M3[1,2])
  M3_LRchi2_M3 [b] = as.numeric(tmp_M3_lrtest_M3[2,4])
  M3_LRchi2_M3.pvalue [b] = as.numeric(tmp_M3_lrtest_M3[2,5])
  tmp_M3_pR2 = pR2(m_M3)
  M3_r2Nagelkerke[b] = as.numeric(tmp_M3_pR2[6])
  M3_aic[b] = as.numeric(m_M3$aic)
  for (v in (1:nVariav_M3)){
    variav = c("Class", Variav_M3[v+1])
    tmp_M3v = tmp_M3[,..variav]
    m_M3v=glm(formula=Class ~ ., family=binomial(link="logit"), data=tmp_M3v)  
    tmp_M3_lrtest_M3vm0 = lrtest(m_M3v)
    M3v_LogLik_lM3v [variav[-1],b] = as.numeric(tmp_M3_lrtest_M3vm0[1,2])
    M3v_LogLik_l0 [variav[-1],b] = as.numeric(tmp_M3_lrtest_M3vm0[2,2])
    M3v_LRchi2_M3vm0 [variav[-1],b] = as.numeric(tmp_M3_lrtest_M3vm0[2,4])
    M3v_LRchi2_M3vm0.pvalue [variav[-1],b] = as.numeric(tmp_M3_lrtest_M3vm0[2,5])
    M3v_aic[variav[-1],b] = as.numeric(m_M3v$aic)
  }
  print(paste("Boot=", b, " - Variav=", variav[-1]))
}

write.csv2 (M3_Samples_train,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM3_Samples_train.csv")
write.csv2 (M3_coef,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM3_coef.csv")
write.csv2 (M3_coef.se,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM3_coef.se.csv")
write.csv2 (M3_coef.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM3_coef.pvalue.csv")
write.csv2 (M3_LogLik_l0,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM3_LogLik_l0.csv")
write.csv2 (M3_LogLik_lM1,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM3_LogLik_lM1.csv")
write.csv2 (M3_LogLik_lM3,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM3_LogLik_lM3.csv")
write.csv2 (M3_LRchi2_M3m0,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM3_LRchi2_M3m0.csv")
write.csv2 (M3_LRchi2_M3m0.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM3_LRchi2_M3m0.pvalue.csv")
write.csv2 (M3_LRchi2_M3,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM3_LRchi2_M3.csv")
write.csv2 (M3_LRchi2_M3.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM3_LRchi2_M3.pvalue.csv")
write.csv2 (M3_r2Nagelkerke,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM3_r2Nagelkerke.csv")
write.csv2 (M3v_LogLik_lM3v,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM3v_LogLik_lM3v.csv")
write.csv2 (M3v_LogLik_l0,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM3v_LogLik_l0.csv")
write.csv2 (M3v_LRchi2_M3vm0,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM3v_LRchi2_M3vm0.csv")
write.csv2 (M3v_LRchi2_M3vm0.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM3v_LRchi2_M3vm0.pvalue.csv")
write.csv2 (M3_aic,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM3_aic.csv")
write.csv2 (M3v_aic,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM3v_aic.csv")

VarEstatAll_M = c("mean.coef_param","CIinf mean.coef_param","CIsup coef_param",
                  "mean.coef.p-value","CIinf mean.coef.p-value","CIsup mean.coef.p-value",
                  "mean.LL0","CIinf mean.LL0","CIsup mean.LL0",
                  "mean.LLM1","CIinf mean.LLM1","CIsup mean.LLM1",
                  "mean.LLMx","CIinf mean.LLMx","CIsup mean.LLMx",
                  "mean.LRchi2_MXm0","CIinf mean.LRchi2_MXm0","CIsup mean_MXm0.LRchi2",
                  "mean.LRchi2_MXM1","CIinf mean.LRchi2_MXM1","CIsup mean.LRchi2_MXM1",
                  "mean.LRchi2_MXm0.pvalue","CIinf mean.LRchi2_MXm0.pvalue","CIsup mean_MXm0.LRchi2.pvalue",
                  "mean.LRchi2_MXM1.pvalue","CIinf mean.LRchi2_MXM1.pvalue","CIsup mean.LRchi2_MXM1.pvalue",
                  "mean.r2Nagelkerke","CIinf mean.r2Nagelkerke", "CIsup mean.r2Nagelkerke",
                  "mean.aic","CIinf mean.aic", "CIsup mean.aic")

M3_All = data.frame(matrix(NA, ncol=length(VarEstatAll_M), nrow=1+nVariav_M3_Desag+1))
colnames(M3_All) = c(VarEstatAll_M)
rownames(M3_All) = c("(Intercept)",Variav_M3_Desag,"model M3")

for (v in 1:nrow(M3_coef)) {
  M3_All[v,1] = mean(as.numeric(M3_coef[v,]))
  M3_All[v,2] = mean(as.numeric(M3_coef[v,])) - (qnorm(0.975)*((sd(as.numeric(M3_coef[v,])))/sqrt(Nboot)))
  M3_All[v,3] = mean(as.numeric(M3_coef[v,])) + (qnorm(0.975)*((sd(as.numeric(M3_coef[v,])))/sqrt(Nboot)))
  M3_All[v,4] = mean(as.numeric(M3_coef.pvalue[v,]))
  M3_All[v,5] = mean(as.numeric(M3_coef.pvalue[v,])) - (qnorm(0.975)*((sd(as.numeric(M3_coef.pvalue[v,])))/sqrt(Nboot)))
  M3_All[v,6] = mean(as.numeric(M3_coef.pvalue[v,])) + (qnorm(0.975)*((sd(as.numeric(M3_coef.pvalue[v,])))/sqrt(Nboot)))
  
  M3_All[v,7] = mean(as.numeric(M3v_LogLik_l0[v,]))
  M3_All[v,8] = mean(as.numeric(M3v_LogLik_l0[v,])) - (qnorm(0.975)*((sd(as.numeric(M3v_LogLik_l0[v,])))/sqrt(Nboot)))
  M3_All[v,9] = mean(as.numeric(M3v_LogLik_l0[v,])) + (qnorm(0.975)*((sd(as.numeric(M3v_LogLik_l0[v,])))/sqrt(Nboot)))
  M3_All[v,13] = mean(as.numeric(M3v_LogLik_lM3v[v,]))
  M3_All[v,14] = mean(as.numeric(M3v_LogLik_lM3v[v,])) - (qnorm(0.975)*((sd(as.numeric(M3v_LogLik_lM3v[v,])))/sqrt(Nboot)))
  M3_All[v,15] = mean(as.numeric(M3v_LogLik_lM3v[v,])) + (qnorm(0.975)*((sd(as.numeric(M3v_LogLik_lM3v[v,])))/sqrt(Nboot)))
  
  M3_All[v,16] = mean(as.numeric(M3v_LRchi2_M3vm0[v,]))
  M3_All[v,17] = mean(as.numeric(M3v_LRchi2_M3vm0[v,])) - (qnorm(0.975)*((sd(as.numeric(M3v_LRchi2_M3vm0[v,])))/sqrt(Nboot)))
  M3_All[v,18] = mean(as.numeric(M3v_LRchi2_M3vm0[v,])) + (qnorm(0.975)*((sd(as.numeric(M3v_LRchi2_M3vm0[v,])))/sqrt(Nboot)))
  
  M3_All[v,22] = mean(as.numeric(M3v_LRchi2_M3vm0.pvalue[v,]))
  M3_All[v,23] = mean(as.numeric(M3v_LRchi2_M3vm0.pvalue[v,])) - (qnorm(0.975)*((sd(as.numeric(M3v_LRchi2_M3vm0.pvalue[v,])))/sqrt(Nboot)))
  M3_All[v,24] = mean(as.numeric(M3v_LRchi2_M3vm0.pvalue[v,])) + (qnorm(0.975)*((sd(as.numeric(M3v_LRchi2_M3vm0.pvalue[v,])))/sqrt(Nboot)))
  
  M3_All[v,31] = mean(as.numeric(M3v_aic[v,]))
  M3_All[v,32] = mean(as.numeric(M3v_aic[v,])) - (qnorm(0.975)*((sd(as.numeric(M3v_aic[v,])))/sqrt(Nboot)))
  M3_All[v,33] = mean(as.numeric(M3v_aic[v,])) + (qnorm(0.975)*((sd(as.numeric(M3v_aic[v,])))/sqrt(Nboot)))
  
  print(v)
}

M3_All[1+nVariav_M3_Desag+1,7] = mean(M3_LogLik_l0)
M3_All[1+nVariav_M3_Desag+1,8] = mean(M3_LogLik_l0) - (qnorm(0.975)*((sd(as.numeric(M3_LogLik_l0)))/sqrt(Nboot)))
M3_All[1+nVariav_M3_Desag+1,9] = mean(M3_LogLik_l0) + (qnorm(0.975)*((sd(as.numeric(M3_LogLik_l0)))/sqrt(Nboot)))
M3_All[1+nVariav_M3_Desag+1,10] = mean(M3_LogLik_lM1)
M3_All[1+nVariav_M3_Desag+1,11] = mean(M3_LogLik_lM1) - (qnorm(0.975)*((sd(as.numeric(M3_LogLik_lM1)))/sqrt(Nboot)))
M3_All[1+nVariav_M3_Desag+1,12] = mean(M3_LogLik_lM1) + (qnorm(0.975)*((sd(as.numeric(M3_LogLik_lM1)))/sqrt(Nboot)))
M3_All[1+nVariav_M3_Desag+1,13] = mean(M3_LogLik_lM3)
M3_All[1+nVariav_M3_Desag+1,14] = mean(M3_LogLik_lM3) - (qnorm(0.975)*((sd(as.numeric(M3_LogLik_lM3)))/sqrt(Nboot)))
M3_All[1+nVariav_M3_Desag+1,15] = mean(M3_LogLik_lM3) + (qnorm(0.975)*((sd(as.numeric(M3_LogLik_lM3)))/sqrt(Nboot)))

M3_All[1+nVariav_M3_Desag+1,16] = mean(M3_LRchi2_M3m0)
M3_All[1+nVariav_M3_Desag+1,17] = mean(M3_LRchi2_M3m0) - (qnorm(0.975)*((sd(as.numeric(M3_LRchi2_M3m0)))/sqrt(Nboot)))
M3_All[1+nVariav_M3_Desag+1,18] = mean(M3_LRchi2_M3m0) + (qnorm(0.975)*((sd(as.numeric(M3_LRchi2_M3m0)))/sqrt(Nboot)))
M3_All[1+nVariav_M3_Desag+1,19] = mean(M3_LRchi2_M3)
M3_All[1+nVariav_M3_Desag+1,20] = mean(M3_LRchi2_M3) - (qnorm(0.975)*((sd(as.numeric(M3_LRchi2_M3)))/sqrt(Nboot)))
M3_All[1+nVariav_M3_Desag+1,21] = mean(M3_LRchi2_M3) + (qnorm(0.975)*((sd(as.numeric(M3_LRchi2_M3)))/sqrt(Nboot)))

M3_All[1+nVariav_M3_Desag+1,22] = mean(M3_LRchi2_M3m0.pvalue)
M3_All[1+nVariav_M3_Desag+1,23] = mean(M3_LRchi2_M3m0.pvalue) - (qnorm(0.975)*((sd(as.numeric(M3_LRchi2_M3m0.pvalue)))/sqrt(Nboot)))
M3_All[1+nVariav_M3_Desag+1,24] = mean(M3_LRchi2_M3m0.pvalue) + (qnorm(0.975)*((sd(as.numeric(M3_LRchi2_M3m0.pvalue)))/sqrt(Nboot)))
M3_All[1+nVariav_M3_Desag+1,25] = mean(M3_LRchi2_M3.pvalue)
M3_All[1+nVariav_M3_Desag+1,26] = mean(M3_LRchi2_M3.pvalue) - (qnorm(0.975)*((sd(as.numeric(M3_LRchi2_M3.pvalue)))/sqrt(Nboot)))
M3_All[1+nVariav_M3_Desag+1,27] = mean(M3_LRchi2_M3.pvalue) + (qnorm(0.975)*((sd(as.numeric(M3_LRchi2_M3.pvalue)))/sqrt(Nboot)))

M3_All[1+nVariav_M3_Desag+1,28] = mean(M3_r2Nagelkerke)
M3_All[1+nVariav_M3_Desag+1,29] = mean(M3_r2Nagelkerke) - (qnorm(0.975)*((sd(as.numeric(M3_r2Nagelkerke)))/sqrt(Nboot)))
M3_All[1+nVariav_M3_Desag+1,30] = mean(M3_r2Nagelkerke) + (qnorm(0.975)*((sd(as.numeric(M3_r2Nagelkerke)))/sqrt(Nboot)))

M3_All[1+nVariav_M3_Desag+1,31] = mean(M3_aic)
M3_All[1+nVariav_M3_Desag+1,32] = mean(M3_aic) - (qnorm(0.975)*((sd(as.numeric(M3_aic)))/sqrt(Nboot)))
M3_All[1+nVariav_M3_Desag+1,33] = mean(M3_aic) + (qnorm(0.975)*((sd(as.numeric(M3_aic)))/sqrt(Nboot)))

write.csv2 (M3_All,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_2010_M3.csv")





########################################### EM (Evaluation Model) ---------------------------------
rm(list=ls())
gc()

library(ROCR)
library(data.table)

# rm(list=ls()[! ls() %in% c("pts_data_test")])
pts_data_test = read.csv2 ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/DataAll_Test_PTS.csv", header=T)

Ano = 2010

data_test = as.data.frame(pts_data_test)

data_test$Sbn2010 = as.factor(data_test$Sbn2010)

data_test$SubnChn [data_test$SubnChn==2] = 0
data_test$SubnChn = as.factor(data_test$SubnChn)

data_test$Suscep2e3 [data_test$Suscep==1] = 0
data_test$Suscep2e3 [data_test$Suscep==2] = 1
data_test$Suscep2e3 [data_test$Suscep==3] = 1
data_test$Suscep2e3 = as.factor(data_test$Suscep2e3)

data_test$Suscep3 [data_test$Suscep==1] = 0
data_test$Suscep3 [data_test$Suscep==2] = 0
data_test$Suscep3 [data_test$Suscep==3] = 1
data_test$Suscep3 = as.factor(data_test$Suscep3)

data_test$Suscep = as.factor(data_test$Suscep)
unique(data_test$Suscep)

tab_class = as.data.frame(data_test$Class)
names(tab_class) = c("Class")
tab_class$Cl = NA
tab_class$Cl[tab_class$Class=="yes"] = 1
tab_class$Cl[tab_class$Class=="no"] = 0
data_test$Class = as.factor(tab_class$Cl)
rm(tab_class)

data_test$AspectD = NA
data_test$AspectD [is.na(data_test$Aspect)] = "Flat"
data_test$AspectD [data_test$Aspect==0] = "N"
data_test$AspectD [data_test$Aspect>=(337.5)| data_test$Aspect<(22.5)] = "N"
data_test$AspectD [data_test$Aspect>=(22.5) & data_test$Aspect<(67.5)] = "NE"
data_test$AspectD [data_test$Aspect>=(67.5) & data_test$Aspect<(112.5)] = "E"
data_test$AspectD [data_test$Aspect>=(112.5) & data_test$Aspect<(157.5)] = "SE"
data_test$AspectD [data_test$Aspect>=(157.5) & data_test$Aspect<(202.5)] = "S"
data_test$AspectD [data_test$Aspect>=(202.5) & data_test$Aspect<(247.5)] = "SW"
data_test$AspectD [data_test$Aspect>=(247.5) & data_test$Aspect<(292.5)] = "W"
data_test$AspectD [data_test$Aspect>=(292.5) & data_test$Aspect<(337.5)] = "NW"
data_test$AspectD = as.factor(data_test$AspectD)
unique(data_test$AspectD)

data_test$AspectDN = 0
data_test$AspectDN [data_test$AspectD=="N"] = 1
data_test$AspectDN = as.factor(data_test$AspectDN)
data_test$AspectDNE = 0
data_test$AspectDNE [data_test$AspectD=="NE"] = 1
data_test$AspectDNE = as.factor(data_test$AspectDNE)
data_test$AspectDE = 0
data_test$AspectDE [data_test$AspectD=="E"] = 1
data_test$AspectDE = as.factor(data_test$AspectDE)
data_test$AspectDSE = 0
data_test$AspectDSE [data_test$AspectD=="SE"] = 1
data_test$AspectDSE = as.factor(data_test$AspectDSE)
data_test$AspectDS = 0
data_test$AspectDS [data_test$AspectD=="S"] = 1
data_test$AspectDS = as.factor(data_test$AspectDS)
data_test$AspectDSW = 0
data_test$AspectDSW [data_test$AspectD=="SW"] = 1
data_test$AspectDSW = as.factor(data_test$AspectDSW)
data_test$AspectDW = 0
data_test$AspectDW [data_test$AspectD=="W"] = 1
data_test$AspectDW = as.factor(data_test$AspectDW)
data_test$AspectDNW = 0
data_test$AspectDNW [data_test$AspectD=="NW"] = 1
data_test$AspectDNW = as.factor(data_test$AspectDNW)
data_test$AspectDFlat = 0
data_test$AspectDFlat [data_test$AspectD=="Flat"] = 1
data_test$AspectDFlat = as.factor(data_test$AspectDFlat)

data_test$AspectD4 = NA
data_test$AspectD4 [is.na(data_test$Aspect)] = "0" #"Flat"
data_test$AspectD4 [data_test$Aspect==0] =  "1" #"N"
data_test$AspectD4 [data_test$Aspect>=(315)| data_test$Aspect<(45)] =  "1" #"N"
data_test$AspectD4 [data_test$Aspect>=(45) & data_test$Aspect<(135)] =  "2" #"E"
data_test$AspectD4 [data_test$Aspect>=(135) & data_test$Aspect<(225)] =  "3" #"S"
data_test$AspectD4 [data_test$Aspect>=(225) & data_test$Aspect<(315)] =  "4" #"W"
data_test$AspectD4 = as.factor(data_test$AspectD4)
unique(data_test$AspectD4)

data_test$AspectD4N = 0
data_test$AspectD4N [data_test$AspectD=="N"] = 1
data_test$AspectD4N = as.factor(data_test$AspectD4N)
data_test$AspectD4E = 0
data_test$AspectD4E [data_test$AspectD=="E"] = 1
data_test$AspectD4E = as.factor(data_test$AspectD4E)
data_test$AspectD4S = 0
data_test$AspectD4S [data_test$AspectD=="S"] = 1
data_test$AspectD4S = as.factor(data_test$AspectD4S)
data_test$AspectD4W = 0
data_test$AspectD4W [data_test$AspectD=="W"] = 1
data_test$AspectD4W = as.factor(data_test$AspectD4W)

data_test = droplevels(data_test)

rm(list=ls()[! ls() %in% c("pts_data_test", "data_test")])



###################################### EM Roc - M1 ====
Variav_M1_test = c("ID", "Class",
              "MnI2010","Sbn2010",
              "Dm2010",
              "Veg2010","Slope",
              "Rn0d14d")
nVariav_M1_test = length(Variav_M1_test)-1
Variav_M1_test_Desag = c("MnI2010","Sbn2010","Sbn20101",
                    "Dm2010",
                    "Veg2010","Slope",
                    "Rn0d14d")
nVariav_M1_test_Desag = length(Variav_M1_test_Desag)

data_test_M1 = data.table(data_test[,Variav_M1_test])
setkeyv(data_test_M1,c("Class"))
table(data_test_M1$Class)

data_test_M1_s = data_test_M1
data_test_M1_s$Sbn2010 = as.numeric(as.character(data_test_M1_s$Sbn2010))

logits = -3.9717 - 
  0.000093*data_test_M1_s$MnI2010 + 
  1.3377*data_test_M1_s$Sbn2010 +
  0.0427*data_test_M1_s$Dm2010 + 
  1.2934*data_test_M1_s$Veg2010 + 
  0.0586*data_test_M1_s$Slope + 
  0.0164*data_test_M1_s$Rn0d14d

#https://sebastiansauer.github.io/convert_logit2prob/ 
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

prob_Logit = logit2prob(logits)
pred_Logit <- prediction(prob_Logit, data_test_M1$Class)
perf_Logit <- performance(pred_Logit, measure = "tpr", x.measure = "fpr")
plot(perf_Logit)
auc_Logit <- performance(pred_Logit, measure = "auc")
m.auc_Logit <- auc_Logit@y.values[[1]]
m.auc_Logit





###################################### EM 3d plot - M1 =====
MnI2010 = 2647
Sbn2010 = 1
Dm2010 = 13.323 
Veg2010 = 0.1004
Slope = 7.0921
Rn0d14d = 83.17

varx = c("Dm2010")
vary = c("Rn0d14d")

max_x = 400
max_y = 450
  
val_x = seq(1,max_x,1)
val_y = seq(1,max_y,(max_y/max_x))


Val_grid = expand.grid(val_x,val_y)
names(Val_grid) = c("val_x", "val_y")
Dm2010 = Val_grid$val_x
Rn0d14d = Val_grid$val_y
logits_Graf = -3.9717 - 
  0.000093*MnI2010 + 
  1.3377*Sbn2010 +
  0.0427*Dm2010 + 
  1.2934*Veg2010 + 
  0.0586*Slope + 
  0.0164*Rn0d14d
Val_grid$val_z = logit2prob(logits_Graf)
# plot(Val_grid$val_x, Val_grid$val_y, pch=19)
# plot(Val_grid$val_x, Val_grid$val_y, pch=19, cex=Val_grid$val_z)
x = Val_grid$val_x
y = Val_grid$val_y
z = Val_grid$val_z


f <- function(x, y) { 
  r = logit2prob(-3.9717 - 
    0.000093*MnI2010 + 
    1.3377*Sbn2010 +
    0.0427*x + 
    1.2934*Veg2010 + 
    0.0586*Slope + 
    0.0164*y)
}
Val_grid_2 = outer(val_x, val_y, f)

par(fg=NA, col="black")
p = filled.contour(x=val_x,y=val_y,z=Val_grid_2, 
               color.palette=colorRampPalette(c("Turquoise", "LightCoral")), 
               #color.palette=colorRampPalette(c("#00AFBB","#E7B800","#FC4E07")), 
               #color.palette=colorRampPalette(c("gray80","gray20")),
               plot.title=title(xlab="Dm2010", ylab="Rn0d14d"), 
               nlevels=200,
               plot.axes = {axis(side=1, at=val_y, labels=val_y)
                 axis(side = 2, at=val_x, labels=val_x) },
               key.title=title(main="prob"),
               key.axes = axis(4, seq(0, 1, by = 0.1)))
               

df <- as.data.frame(interp2xyz(interp(x=x, y=y, z=z)))#interp is from the akima package so you can work with non-square data in your contour
#plot3d(df) # What it looks like in 3d
ggplot(df, aes(x=x,y=y,z=z, fill=z)) + 
  geom_contour(binwidth=0.1, aes(color= ..level..)) + 
  theme_classic()#what it looks like projected into 2d

ggplot(df, aes(x=x,y=y)) + geom_point(aes(color = z), size = 7) +
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07"))
 

library(akima)
im <- with(Val_grid,interp(x,y,z))
with(im,image(x,y,z))



library(rgl)
persp3d(x, y, z, col="skyblue")
persp3d(Val_grid, col="skyblue")
persp3d(Val_grid_2, col="skyblue")

library(magrittr)
library(plotly)
plot_ly(x=x,y=y,z=z, type="surface")
p <- plot_ly(x=x, y=y, z=z) %>% add_surface()
p <- plot_ly(showscale = FALSE) %>%
  add_surface(z = ~z) %>%
  add_surface(z = ~z2, opacity = 0.98) %>%
  add_surface(z = ~z3, opacity = 0.98)

plot_ly(x=x, y=y, z=z,type="scatter3d")






###################################### EM Risk Map - M1 =====
library (raster)
Data_Dom2010_foc = raster("D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_Dom2010_foc.tif")
Data_MeanIncDif_foc = raster("D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_MeanIncDif_foc.tif")
Data_Subn2010_foc = raster("D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_Subn2010_foc.tif")
Data_Veg2010_foc = raster("D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_Veg2010_foc.tif")
Data_Slope_foc = raster("D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_Slope_foc.tif")
Rn0d14d = 83.17

logits_raster = -3.9717 - 
  0.000093*Data_MeanIncDif_foc + 
  1.3377*Data_Subn2010_foc +
  0.0427*Data_Dom2010_foc + 
  1.2934*Data_Veg2010_foc + 
  0.0586*Data_Slope_foc + 
  0.0164*Rn0d14d

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

prob_raster = logit2prob(logits_raster)
plot(prob_raster)
writeRaster (prob_raster, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1_ProbMap.tif", overwrite=F, datatype='FLT4S')





################################################ Estat - MM A (Multivariable Model) ---------------------------------
rm(list=ls())
gc()

library(data.table)
library(stats) #aov test
library(lmtest) #lr test
library(pscl) #R2
library(ggplot2)
library(glmulti)
library(oddsratio)

#rm(list=ls()[! ls() %in% c("pts_data")])
pts_data = read.csv2 ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/DataAll_Train_PTS.csv", header=T)

data = as.data.frame(pts_data)

data$Sbn2010 = as.factor(data$Sbn2010)

data$SubnChn [data$SubnChn==2] = 0
data$SubnChn = as.factor(data$SubnChn)

data$Suscep2e3 [data$Suscep==1] = 0
data$Suscep2e3 [data$Suscep==2] = 1
data$Suscep2e3 [data$Suscep==3] = 1
data$Suscep2e3 = as.factor(data$Suscep2e3)

data$Suscep3 [data$Suscep==1] = 0
data$Suscep3 [data$Suscep==2] = 0
data$Suscep3 [data$Suscep==3] = 1
data$Suscep3 = as.factor(data$Suscep3)

data$Suscep = as.factor(data$Suscep)
unique(data$Suscep)

tab_class = as.data.frame(data$Class)
names(tab_class) = c("Class")
tab_class$Cl = NA
tab_class$Cl[tab_class$Class=="yes"] = 1
tab_class$Cl[tab_class$Class=="no"] = 0
data$Class = as.factor(tab_class$Cl)
rm(tab_class)

data$AspectD = NA
data$AspectD [is.na(data$Aspect)] = "Flat"
data$AspectD [data$Aspect==0] = "N"
data$AspectD [data$Aspect>=(337.5)| data$Aspect<(22.5)] = "N"
data$AspectD [data$Aspect>=(22.5) & data$Aspect<(67.5)] = "NE"
data$AspectD [data$Aspect>=(67.5) & data$Aspect<(112.5)] = "E"
data$AspectD [data$Aspect>=(112.5) & data$Aspect<(157.5)] = "SE"
data$AspectD [data$Aspect>=(157.5) & data$Aspect<(202.5)] = "S"
data$AspectD [data$Aspect>=(202.5) & data$Aspect<(247.5)] = "SW"
data$AspectD [data$Aspect>=(247.5) & data$Aspect<(292.5)] = "W"
data$AspectD [data$Aspect>=(292.5) & data$Aspect<(337.5)] = "NW"
data$AspectD = as.factor(data$AspectD)
unique(data$AspectD)

data$AspectDN = 0
data$AspectDN [data$AspectD=="N"] = 1
data$AspectDN = as.factor(data$AspectDN)
data$AspectDNE = 0
data$AspectDNE [data$AspectD=="NE"] = 1
data$AspectDNE = as.factor(data$AspectDNE)
data$AspectDE = 0
data$AspectDE [data$AspectD=="E"] = 1
data$AspectDE = as.factor(data$AspectDE)
data$AspectDSE = 0
data$AspectDSE [data$AspectD=="SE"] = 1
data$AspectDSE = as.factor(data$AspectDSE)
data$AspectDS = 0
data$AspectDS [data$AspectD=="S"] = 1
data$AspectDS = as.factor(data$AspectDS)
data$AspectDSW = 0
data$AspectDSW [data$AspectD=="SW"] = 1
data$AspectDSW = as.factor(data$AspectDSW)
data$AspectDW = 0
data$AspectDW [data$AspectD=="W"] = 1
data$AspectDW = as.factor(data$AspectDW)
data$AspectDNW = 0
data$AspectDNW [data$AspectD=="NW"] = 1
data$AspectDNW = as.factor(data$AspectDNW)
data$AspectDFlat = 0
data$AspectDFlat [data$AspectD=="Flat"] = 1
data$AspectDFlat = as.factor(data$AspectDFlat)

data$AspectD4 = NA
data$AspectD4 [is.na(data$Aspect)] = "0" #"Flat"
data$AspectD4 [data$Aspect==0] =  "1" #"N"
data$AspectD4 [data$Aspect>=(315)| data$Aspect<(45)] =  "1" #"N"
data$AspectD4 [data$Aspect>=(45) & data$Aspect<(135)] =  "2" #"E"
data$AspectD4 [data$Aspect>=(135) & data$Aspect<(225)] =  "3" #"S"
data$AspectD4 [data$Aspect>=(225) & data$Aspect<(315)] =  "4" #"W"
data$AspectD4 = as.factor(data$AspectD4)
unique(data$AspectD4)

data$AspectD4Flat = 0
data$AspectD4Flat [data$AspectD=="Flat"] = 1
data$AspectD4Flat = as.factor(data$AspectD4Flat)
data$AspectD4N = 0
data$AspectD4N [data$AspectD=="N"] = 1
data$AspectD4N = as.factor(data$AspectD4N)
data$AspectD4E = 0
data$AspectD4E [data$AspectD=="E"] = 1
data$AspectD4E = as.factor(data$AspectD4E)
data$AspectD4S = 0
data$AspectD4S [data$AspectD=="S"] = 1
data$AspectD4S = as.factor(data$AspectD4S)
data$AspectD4W = 0
data$AspectD4W [data$AspectD=="W"] = 1
data$AspectD4W = as.factor(data$AspectD4W)

data = droplevels(data)




########################################### MM - M0A =================================
rm(list=ls()[! ls() %in% c("pts_data", "data")])

Ano = 2010
Nsamples = 1000
Nboot = 1000
#Nboot=2

Variav_M0A = c("ID", "Class",
               "Rn0d14d","Slope",
               "Suscep",
               "Veg2010","VegDif",
               "MnI2010","Sbn2010","SubnChn",
               "Dm2010","DmDif",
               "PSMF10","PSBL10","PEsgCA10","PSEsg10")
nVariav_M0A = length(Variav_M0A)-2
Variav_M0A_Desag = c("Rn0d14d","Slope",
                     "Suscep","Suscep2","Suscep3",
                     "Veg2010","VegDif",
                     "MnI2010","Sbn2010","Sbn20101","SubnChn","SubnChn1",
                     "Dm2010","DmDif",
                     "PSMF10","PSBL10","PEsgCA10","PSEsg10")
nVariav_M0A_Desag = length(Variav_M0A_Desag)

data_M0A = data.table(data[,Variav_M0A])
setkeyv(data_M0A,c("Class"))
table(data_M0A$Class)
data_M0A = na.omit(data_M0A)

M0A_Samples_train = data.frame(Boot=1:Nboot, Samples=NA)

M0A_coef = data.frame(matrix(NA, ncol=Nboot, nrow=1+nVariav_M0A_Desag), row.names=c("(Intercept)",Variav_M0A_Desag))
colnames(M0A_coef) = paste("Boot",1:Nboot, sep=" ")
M0A_coef.se = M0A_coef
M0A_coef.pvalue = M0A_coef

M0Av_LogLik_lM0Av = M0A_coef
M0Av_LogLik_l0 = M0A_coef
M0Av_LRchi2_M0Avm0 = M0A_coef
M0Av_LRchi2_M0Avm0.pvalue = M0A_coef
M0Av_aic = M0A_coef

M0A_LogLik_l0 = as.numeric() 
M0A_LogLik_lM0A = as.numeric() 
M0A_LRchi2 = as.numeric()
M0A_LRchi2.pvalue = as.numeric()
M0A_r2Nagelkerke = as.numeric()
M0A_aic = as.numeric()

#b=1
for (b in (1:Nboot)) {
  tmp_M0A = rbind((data_M0A[data_M0A$Class==1,][sample(nrow(data_M0A[data_M0A$Class==1,]), Nsamples, replace=F),]),
                 (data_M0A[data_M0A$Class==0,][sample(nrow(data_M0A[data_M0A$Class==0,]), Nsamples, replace=F),]))
  Samples_train = as.vector(tmp_M0A$ID)
  M0A_Samples_train[b,2] = paste(Samples_train,collapse="-")
  tmp_M0A = tmp_M0A[,-1]
  m_M0A=glm(formula=Class ~ ., family=binomial(link="logit"), data=tmp_M0A)
  tmp_M0A_coef = as.data.frame(coef(summary(m_M0A))[,1])
  for (v in (1:(nrow(tmp_M0A_coef)))) {M0A_coef[rownames(tmp_M0A_coef)[v],b] = tmp_M0A_coef[rownames(tmp_M0A_coef)[v],1]}
  tmp_M0A_coef.se = as.data.frame(coef(summary(m_M0A))[,2])
  for (v in (1:(nrow(tmp_M0A_coef.se)))) {M0A_coef.se[rownames(tmp_M0A_coef.se)[v],b] = tmp_M0A_coef.se[rownames(tmp_M0A_coef.se)[v],1]}
  tmp_M0A_coef.pvalue = as.data.frame(coef(summary(m_M0A))[,4])
  for (v in (1:nrow(tmp_M0A_coef.pvalue))) {M0A_coef.pvalue[rownames(tmp_M0A_coef.pvalue)[v],b] = tmp_M0A_coef.pvalue[rownames(tmp_M0A_coef.pvalue)[v],1]}
  tmp_M0A_lrtest = lrtest(m_M0A) 
  M0A_LogLik_lM0A [b] = as.numeric(tmp_M0A_lrtest[1,2])
  M0A_LogLik_l0 [b] = as.numeric(tmp_M0A_lrtest[2,2])
  M0A_LRchi2 [b] = as.numeric(tmp_M0A_lrtest[2,4])
  M0A_LRchi2.pvalue [b] = as.numeric(tmp_M0A_lrtest[2,5])
  tmp_M0A_pR2 = pR2(m_M0A)
  M0A_r2Nagelkerke[b] = as.numeric(tmp_M0A_pR2[6])
  M0A_aic[b] = as.numeric(m_M0A$aic)
  for (v in (1:nVariav_M0A)){
    variav = c("Class", Variav_M0A[v+2])
    tmp_M0Av = tmp_M0A[,..variav]
    m_M0Av=glm(formula=Class ~ ., family=binomial(link="logit"), data=tmp_M0Av)  
    tmp_M0A_lrtest_M0Avm0 = lrtest(m_M0Av)
    M0Av_LogLik_lM0Av[variav[-1],b] = as.numeric(tmp_M0A_lrtest_M0Avm0[1,2])
    M0Av_LogLik_l0 [variav[-1],b] = as.numeric(tmp_M0A_lrtest_M0Avm0[2,2])
    M0Av_LRchi2_M0Avm0 [variav[-1],b] = as.numeric(tmp_M0A_lrtest_M0Avm0[2,4])
    M0Av_LRchi2_M0Avm0.pvalue [variav[-1],b] = as.numeric(tmp_M0A_lrtest_M0Avm0[2,5])
    M0Av_aic[variav[-1],b] = as.numeric(m_M0Av$aic)
  }
  print(paste("Boot=", b))
}

write.csv2 (M0A_Samples_train,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0A_Samples_train.csv")
write.csv2 (M0A_coef,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0A_coef.csv")
write.csv2 (M0A_coef.se,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0A_coef.se.csv")
write.csv2 (M0A_coef.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0A_coef.pvalue.csv")
write.csv2 (M0A_LogLik_lM0A,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0A_LogLik_lm.csv")
write.csv2 (M0A_LogLik_l0,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0A_LogLik_l0.csv")
write.csv2 (M0A_LRchi2,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0A_LRchi2.csv")
write.csv2 (M0A_LRchi2.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0A_LRchi2.pvalue.csv")
write.csv2 (M0A_r2Nagelkerke,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0A_r2Nagelkerke.csv")
write.csv2 (M0Av_LogLik_lM0Av,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0Av_LogLik_lM0Av.csv")
write.csv2 (M0Av_LogLik_l0,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0Av_LogLik_l0.csv")
write.csv2 (M0Av_LRchi2_M0Avm0,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0Av_LRchi2_M0Avm0.csv")
write.csv2 (M0Av_LRchi2_M0Avm0.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0Av_LRchi2_M0Avm0.pvalue.csv")
write.csv2 (M0A_aic,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0A_aic.csv")
write.csv2 (M0Av_aic,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM0Av_aic.csv")

VarEstatAll_M = c("mean.coef_param","CIinf mean.coef_param","CIsup coef_param",
                  "mean.coef.p-value","CIinf mean.coef.p-value","CIsup mean.coef.p-value",
                  "mean.LL0","CIinf mean.LL0","CIsup mean.LL0",
                  "mean.LLMx","CIinf mean.LLMx","CIsup mean.LLMx",
                  "mean.LRchi2_MXm0","CIinf mean.LRchi2_MXm0","CIsup mean_MXm0.LRchi2",
                  "mean.LRchi2_MXm0.pvalue","CIinf mean.LRchi2_MXm0.pvalue","CIsup mean_MXm0.LRchi2.pvalue",
                  "mean.r2Nagelkerke","CIinf mean.r2Nagelkerke", "CIsup mean.r2Nagelkerke",
                  "mean.aic","CIinf mean.aic", "CIsup mean.aic")

M0A_All = data.frame(matrix(NA, ncol=length(VarEstatAll_M), nrow=1+nVariav_M0A_Desag+1))
rownames(M0A_All) = c("(Intercept)",Variav_M0A_Desag,"model M0A")
colnames(M0A_All) = c(VarEstatAll_M)

for (v in 1:nrow(M0A_coef)) {
  M0A_All[v,1] = mean(as.numeric(M0A_coef[v,]))
  M0A_All[v,2] = mean(as.numeric(M0A_coef[v,])) - (qnorm(0.975)*((sd(as.numeric(M0A_coef[v,])))/sqrt(Nboot)))
  M0A_All[v,3] = mean(as.numeric(M0A_coef[v,])) + (qnorm(0.975)*((sd(as.numeric(M0A_coef[v,])))/sqrt(Nboot)))
  
  M0A_All[v,4] = mean(as.numeric(M0A_coef.pvalue[v,]))
  M0A_All[v,5] = mean(as.numeric(M0A_coef.pvalue[v,])) - (qnorm(0.975)*((sd(as.numeric(M0A_coef.pvalue[v,])))/sqrt(Nboot)))
  M0A_All[v,6] = mean(as.numeric(M0A_coef.pvalue[v,])) + (qnorm(0.975)*((sd(as.numeric(M0A_coef.pvalue[v,])))/sqrt(Nboot)))
 
  M0A_All[v,10] = mean(as.numeric(M0Av_LogLik_lM0Av[v,]))
  M0A_All[v,11] = mean(as.numeric(M0Av_LogLik_lM0Av[v,])) - (qnorm(0.975)*((sd(as.numeric(M0Av_LogLik_lM0Av[v,])))/sqrt(Nboot)))
  M0A_All[v,12] = mean(as.numeric(M0Av_LogLik_lM0Av[v,])) + (qnorm(0.975)*((sd(as.numeric(M0Av_LogLik_lM0Av[v,])))/sqrt(Nboot)))
  
  M0A_All[v,13] = mean(as.numeric(M0Av_LRchi2_M0Avm0[v,]))
  M0A_All[v,14] = mean(as.numeric(M0Av_LRchi2_M0Avm0[v,])) - (qnorm(0.975)*((sd(as.numeric(M0Av_LRchi2_M0Avm0[v,])))/sqrt(Nboot)))
  M0A_All[v,15] = mean(as.numeric(M0Av_LRchi2_M0Avm0[v,])) + (qnorm(0.975)*((sd(as.numeric(M0Av_LRchi2_M0Avm0[v,])))/sqrt(Nboot)))
  
  M0A_All[v,16] = mean(as.numeric(M0Av_LRchi2_M0Avm0.pvalue[v,]))
  M0A_All[v,17] = mean(as.numeric(M0Av_LRchi2_M0Avm0.pvalue[v,])) - (qnorm(0.975)*((sd(as.numeric(M0Av_LRchi2_M0Avm0.pvalue[v,])))/sqrt(Nboot)))
  M0A_All[v,18] = mean(as.numeric(M0Av_LRchi2_M0Avm0.pvalue[v,])) + (qnorm(0.975)*((sd(as.numeric(M0Av_LRchi2_M0Avm0.pvalue[v,])))/sqrt(Nboot)))
  
  M0A_All[v,22] = mean(as.numeric(M0Av_aic[v,]))
  M0A_All[v,23] = mean(as.numeric(M0Av_aic[v,])) - (qnorm(0.975)*((sd(as.numeric(M0Av_aic[v,])))/sqrt(Nboot)))
  M0A_All[v,24] = mean(as.numeric(M0Av_aic[v,])) + (qnorm(0.975)*((sd(as.numeric(M0Av_aic[v,])))/sqrt(Nboot)))
  
  print(v)
}

M0A_All[1+nVariav_M0A_Desag+1,7] = mean(M0A_LogLik_l0)
M0A_All[1+nVariav_M0A_Desag+1,8] = mean(M0A_LogLik_l0) - (qnorm(0.975)*((sd(as.numeric(M0A_LogLik_l0)))/sqrt(Nboot)))
M0A_All[1+nVariav_M0A_Desag+1,9] = mean(M0A_LogLik_l0) + (qnorm(0.975)*((sd(as.numeric(M0A_LogLik_l0)))/sqrt(Nboot)))

M0A_All[1+nVariav_M0A_Desag+1,10] = mean(M0A_LogLik_lM0A)
M0A_All[1+nVariav_M0A_Desag+1,11] = mean(M0A_LogLik_lM0A) - (qnorm(0.975)*((sd(as.numeric(M0A_LogLik_lM0A)))/sqrt(Nboot)))
M0A_All[1+nVariav_M0A_Desag+1,12] = mean(M0A_LogLik_lM0A) + (qnorm(0.975)*((sd(as.numeric(M0A_LogLik_lM0A)))/sqrt(Nboot)))

M0A_All[1+nVariav_M0A_Desag+1,13] = mean(M0A_LRchi2)
M0A_All[1+nVariav_M0A_Desag+1,14] = mean(M0A_LRchi2) - (qnorm(0.975)*((sd(as.numeric(M0A_LRchi2)))/sqrt(Nboot)))
M0A_All[1+nVariav_M0A_Desag+1,15] = mean(M0A_LRchi2) + (qnorm(0.975)*((sd(as.numeric(M0A_LRchi2)))/sqrt(Nboot)))

M0A_All[1+nVariav_M0A_Desag+1,16] = mean(M0A_LRchi2.pvalue)
M0A_All[1+nVariav_M0A_Desag+1,17] = mean(M0A_LRchi2.pvalue) - (qnorm(0.975)*((sd(as.numeric(M0A_LRchi2.pvalue)))/sqrt(Nboot)))
M0A_All[1+nVariav_M0A_Desag+1,18] = mean(M0A_LRchi2.pvalue) + (qnorm(0.975)*((sd(as.numeric(M0A_LRchi2.pvalue)))/sqrt(Nboot)))

M0A_All[1+nVariav_M0A_Desag+1,19] = mean(M0A_r2Nagelkerke)
M0A_All[1+nVariav_M0A_Desag+1,20] = mean(M0A_r2Nagelkerke) - (qnorm(0.975)*((sd(as.numeric(M0A_r2Nagelkerke)))/sqrt(Nboot)))
M0A_All[1+nVariav_M0A_Desag+1,21] = mean(M0A_r2Nagelkerke) + (qnorm(0.975)*((sd(as.numeric(M0A_r2Nagelkerke)))/sqrt(Nboot)))

M0A_All[1+nVariav_M0A_Desag+1,22] = mean(M0A_aic)
M0A_All[1+nVariav_M0A_Desag+1,23] = mean(M0A_aic) - (qnorm(0.975)*((sd(as.numeric(M0A_aic)))/sqrt(Nboot)))
M0A_All[1+nVariav_M0A_Desag+1,24] = mean(M0A_aic) + (qnorm(0.975)*((sd(as.numeric(M0A_aic)))/sqrt(Nboot)))


write.csv2 (M0A_All,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_2010_M0A.csv")





########################################### MM - M1A =================================
rm(list=ls()[! ls() %in% c("pts_data", "data")])

Ano = 2010
Nsamples = 1000
Nboot = 1000
#Nboot=2

Variav_M1A = c("ID", "Class",
              "Rn0d14d",
              "Slope","Veg2010",
              "MnI2010",
              "Sbn2010",
              "Dm2010",
              "PSMF10","PSEsg10")
nVariav_M1A = length(Variav_M1A)-1
Variav_M1A_Desag = c("Rn0d14d",
                    "Slope","Veg2010",
                    "MnI2010",
                    "Sbn2010","Sbn20101",
                    "Dm2010",
                    "PSMF10","PSEsg10")
nVariav_M1A_Desag = length(Variav_M1A_Desag)

data_M1A = data.table(data[,Variav_M1A])
setkeyv(data_M1A,c("Class"))
data_M1A = na.omit(data_M1A)
table(data_M1A$Class)

M1A_Samples_train = data.frame(Boot=1:Nboot, Samples=NA)

M1A_coef = data.frame(matrix(NA, ncol=Nboot, nrow=1+nVariav_M1A_Desag), row.names=c("(Intercept)",Variav_M1A_Desag))
colnames(M1A_coef) = paste("Boot",1:Nboot, sep=" ")
M1A_coef.se = M1A_coef
M1A_coef.pvalue = M1A_coef
M1A_oddsratio = M1A_coef

M1Av_LogLik_lM1Av = M1A_coef
M1Av_LogLik_l0 = M1A_coef
M1Av_LRchi2_M1Avm0 = M1A_coef
M1Av_LRchi2_M1Avm0.pvalue = M1A_coef
M1Av_aic = M1A_coef

M1A_LogLik_lm0 = as.numeric() 
M1A_LogLik_lM1A = as.numeric() 
M1A_LRchi2_M1Am0 = as.numeric()
M1A_LRchi2_M1Am0.pvalue = as.numeric()
M1A_r2Nagelkerke = as.numeric()
M1A_aic = as.numeric()

topAIC = data.frame(matrix(NA, ncol=5, nrow=0))
colnames(topAIC) = c("Boot","AIC_Rank","Model","AIC","Weights" )
topBIC = data.frame(matrix(NA, ncol=5, nrow=0))
colnames(topBIC) = c("Boot","BIC_Rank","Model","BIC","Weights" ) 

incr = list(Rn0d14d=10,Slope=1,Veg2010=0.1,MnI2010=100, Sbn20101=1, Dm2010=1,PSMF10=0.1,PSEsg10=0.1)

#b=1
for (b in (1:Nboot)) {
  tmp_M1A = rbind((data_M1A[data_M1A$Class==1,][sample(nrow(data_M1A[data_M1A$Class==1,]), Nsamples, replace=F),]),
                 (data_M1A[data_M1A$Class==0,][sample(nrow(data_M1A[data_M1A$Class==0,]), Nsamples, replace=F),]))
  Samples_train = as.vector(tmp_M1A$ID)
  M1A_Samples_train[b,2] = paste(Samples_train,collapse="-")
  tmp_M1A = tmp_M1A[,-1]
  m_M1A=glm(formula=Class ~ ., family=binomial(link="logit"), data=tmp_M1A)
  tmp_M1A_coef = as.data.frame(coef(summary(m_M1A))[,1])
  for (v in (1:(nrow(tmp_M1A_coef)))) {M1A_coef[rownames(tmp_M1A_coef)[v],b] = tmp_M1A_coef[rownames(tmp_M1A_coef)[v],1]}
  tmp_M1A_coef.se = as.data.frame(coef(summary(m_M1A))[,2])
  for (v in (1:(nrow(tmp_M1A_coef.se)))) {M1A_coef.se[rownames(tmp_M1A_coef.se)[v],b] = tmp_M1A_coef.se[rownames(tmp_M1A_coef.se)[v],1]}
  tmp_M1A_coef.pvalue = as.data.frame(coef(summary(m_M1A))[,4])
  for (v in (1:nrow(tmp_M1A_coef.pvalue))) {M1A_coef.pvalue[rownames(tmp_M1A_coef.pvalue)[v],b] = tmp_M1A_coef.pvalue[rownames(tmp_M1A_coef.pvalue)[v],1]}
  tmp_M1A_lrtest = lrtest(m_M1A) 
  M1A_LogLik_lM1A [b] = as.numeric(tmp_M1A_lrtest[1,2])
  M1A_LogLik_lm0 [b] = as.numeric(tmp_M1A_lrtest[2,2])
  M1A_LRchi2_M1Am0 [b] = as.numeric(tmp_M1A_lrtest[2,4])
  M1A_LRchi2_M1Am0.pvalue [b] = as.numeric(tmp_M1A_lrtest[2,5])
  tmp_M1A_pR2 = pR2(m_M1A)
  M1A_r2Nagelkerke[b] = as.numeric(tmp_M1A_pR2[6])
  M1A_aic[b] = as.numeric(m_M1A$aic)
  or = or_glm(tmp_M1A, m_M1A, incr, CI = 0.95)
  or = as.data.frame(or)
  rownames(or) = or[,1]
  for (v in (1:nrow(or))) {M1A_oddsratio[rownames(or)[v],b] = or[rownames(or)[v],"oddsratio"]}
  for (v in (1:(nVariav_M1A-1))){
    variav = c("Class", Variav_M1A[v+2])
    tmp_M1Av = tmp_M1A[,..variav]
    m_M1Av=glm(formula=Class ~ ., family=binomial(link="logit"), data=tmp_M1Av)  
    tmp_M1A_lrtest_M1Avm0 = lrtest(m_M1Av)
    M1Av_LogLik_lM1Av [variav[-1],b] = as.numeric(tmp_M1A_lrtest_M1Avm0[1,2])
    M1Av_LogLik_l0 [variav[-1],b] = as.numeric(tmp_M1A_lrtest_M1Avm0[2,2])
    M1Av_LRchi2_M1Avm0 [variav[-1],b] = as.numeric(tmp_M1A_lrtest_M1Avm0[2,4])
    M1Av_LRchi2_M1Avm0.pvalue [variav[-1],b] = as.numeric(tmp_M1A_lrtest_M1Avm0[2,5])
    M1Av_aic[variav[-1],b] = as.numeric(m_M1Av$aic)
  }
  AIC_f1 = glmulti(Class ~ ., data=tmp_M1A, family=binomial, 
                   level=1, crit=aic, fitfunc=glm, method="h",
                   confsetsize=256, plotty=T, report=TRUE, name="AIC_glmulti")
  tmp_M1A_topAIC <- glmulti::weightable(AIC_f1)
  tmp_M1A_topAIC$Boot = b
  tmp_M1A_topAIC$AIC_Rank = c(1:nrow(tmp_M1A_topAIC))
  colnames(tmp_M1A_topAIC) = c("Model","AIC","Weights","Boot","AIC_Rank")
  topAIC = rbind(topAIC,tmp_M1A_topAIC)
  BIC_f1 = glmulti(Class ~ ., data=tmp_M1A, family=binomial, 
                   level=1, crit=bic, fitfunc=glm, method="h",
                   confsetsize=256, plotty=T, report=TRUE, name="BIC_glmulti")
  tmp_M1A_topBIC <- glmulti::weightable(BIC_f1)
  tmp_M1A_topBIC$Boot = b
  tmp_M1A_topBIC$BIC_Rank = c(1:nrow(tmp_M1A_topBIC))
  topBIC = rbind(topBIC,tmp_M1A_topBIC)
  print(paste("Boot=", b, " - Variav=", variav[-1]))
}

write.csv2 (M1A_Samples_train,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1A_Samples_train.csv")
write.csv2 (M1A_coef,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1A_coef.csv")
write.csv2 (M1A_coef.se,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1A_coef.se.csv")
write.csv2 (M1A_coef.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1A_coef.pvalue.csv")
write.csv2 (M1A_LogLik_lm0,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1A_LogLik_l0.csv")
write.csv2 (M1A_LogLik_lM1A,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1A_LogLik_lM1A.csv")
write.csv2 (M1A_LRchi2_M1Am0,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1A_LRchi2_M1Am0.csv")
write.csv2 (M1A_LRchi2_M1Am0.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1A_LRchi2_M1Am0.pvalue.csv")
write.csv2 (M1A_r2Nagelkerke,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1A_r2Nagelkerke.csv")
write.csv2 (M1Av_LogLik_lM1Av,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1Av_LogLik_lM1Av.csv")
write.csv2 (M1Av_LogLik_l0,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1Av_LogLik_l0.csv")
write.csv2 (M1Av_LRchi2_M1Avm0,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1Av_LRchi2_M1Avm0.csv")
write.csv2 (M1Av_LRchi2_M1Avm0.pvalue,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1Av_LRchi2_M1Avm0.pvalue.csv")
write.csv2 (M1A_aic,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1A_aic.csv")
write.csv2 (M1Av_aic,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1Av_aic.csv")
write.csv2 (topAIC, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1A_AICf1.csv")
write.csv2 (topBIC, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1A_BICf1.csv")
write.csv2 (M1A_oddsratio, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1A_oddsratio.csv")

VarEstatAll_M = c("mean.coef_param","CIinf mean.coef_param","CIsup coef_param",
                  "mean.coef.p-value","CIinf mean.coef.p-value","CIsup mean.coef.p-value",
                  "mean.LL0","CIinf mean.LL0","CIsup mean.LL0",
                  "mean.LLMx","CIinf mean.LLMx","CIsup mean.LLMx",
                  "mean.LRchi2_MXm0","CIinf mean.LRchi2_MXm0","CIsup mean_MXm0.LRchi2",
                  "mean.LRchi2_MXm0.pvalue","CIinf mean.LRchi2_MXm0.pvalue","CIsup mean_MXm0.LRchi2.pvalue",
                  "mean.r2Nagelkerke","CIinf mean.r2Nagelkerke", "CIsup mean.r2Nagelkerke",
                  "mean.aic","CIinf mean.aic", "CIsup mean.aic",
                  "mean.oddsratio","CIinf mean.oddsratio", "CIsup mean.oddsratio")

M1A_All = data.frame(matrix(NA, ncol=length(VarEstatAll_M), nrow=1+nVariav_M1A_Desag+1))
colnames(M1A_All) = c(VarEstatAll_M)
rownames(M1A_All) = c("(Intercept)",Variav_M1A_Desag,"model M1A")


for (v in 1:nrow(M1A_coef)) {
  M1A_All[v,1] = mean(as.numeric(M1A_coef[v,]))
  M1A_All[v,2] = mean(as.numeric(M1A_coef[v,])) - (qnorm(0.975)*((sd(as.numeric(M1A_coef[v,])))/sqrt(Nboot)))
  M1A_All[v,3] = mean(as.numeric(M1A_coef[v,])) + (qnorm(0.975)*((sd(as.numeric(M1A_coef[v,])))/sqrt(Nboot)))
  
  M1A_All[v,4] = mean(as.numeric(M1A_coef.pvalue[v,]))
  M1A_All[v,5] = mean(as.numeric(M1A_coef.pvalue[v,])) - (qnorm(0.975)*((sd(as.numeric(M1A_coef.pvalue[v,])))/sqrt(Nboot)))
  M1A_All[v,6] = mean(as.numeric(M1A_coef.pvalue[v,])) + (qnorm(0.975)*((sd(as.numeric(M1A_coef.pvalue[v,])))/sqrt(Nboot)))
  
  M1A_All[v,7] = mean(as.numeric(M1Av_LogLik_l0[v,]))
  M1A_All[v,8] = mean(as.numeric(M1Av_LogLik_l0[v,])) - (qnorm(0.975)*((sd(as.numeric(M1Av_LogLik_l0[v,])))/sqrt(Nboot)))
  M1A_All[v,9] = mean(as.numeric(M1Av_LogLik_l0[v,])) + (qnorm(0.975)*((sd(as.numeric(M1Av_LogLik_l0[v,])))/sqrt(Nboot)))
  
  M1A_All[v,10] = mean(as.numeric(M1Av_LogLik_lM1Av[v,]))
  M1A_All[v,11] = mean(as.numeric(M1Av_LogLik_lM1Av[v,])) - (qnorm(0.975)*((sd(as.numeric(M1Av_LogLik_lM1Av[v,])))/sqrt(Nboot)))
  M1A_All[v,12] = mean(as.numeric(M1Av_LogLik_lM1Av[v,])) + (qnorm(0.975)*((sd(as.numeric(M1Av_LogLik_lM1Av[v,])))/sqrt(Nboot)))
  
  M1A_All[v,13] = mean(as.numeric(M1Av_LRchi2_M1Avm0[v,]))
  M1A_All[v,14] = mean(as.numeric(M1Av_LRchi2_M1Avm0[v,])) - (qnorm(0.975)*((sd(as.numeric(M1Av_LRchi2_M1Avm0[v,])))/sqrt(Nboot)))
  M1A_All[v,15] = mean(as.numeric(M1Av_LRchi2_M1Avm0[v,])) + (qnorm(0.975)*((sd(as.numeric(M1Av_LRchi2_M1Avm0[v,])))/sqrt(Nboot)))
  
  M1A_All[v,16] = mean(as.numeric(M1Av_LRchi2_M1Avm0.pvalue[v,]))
  M1A_All[v,17] = mean(as.numeric(M1Av_LRchi2_M1Avm0.pvalue[v,])) - (qnorm(0.975)*((sd(as.numeric(M1Av_LRchi2_M1Avm0.pvalue[v,])))/sqrt(Nboot)))
  M1A_All[v,18] = mean(as.numeric(M1Av_LRchi2_M1Avm0.pvalue[v,])) + (qnorm(0.975)*((sd(as.numeric(M1Av_LRchi2_M1Avm0.pvalue[v,])))/sqrt(Nboot)))
  
  M1A_All[v,22] = mean(as.numeric(M1Av_aic[v,]))
  M1A_All[v,23] = mean(as.numeric(M1Av_aic[v,])) - (qnorm(0.975)*((sd(as.numeric(M1Av_aic[v,])))/sqrt(Nboot)))
  M1A_All[v,24] = mean(as.numeric(M1Av_aic[v,])) + (qnorm(0.975)*((sd(as.numeric(M1Av_aic[v,])))/sqrt(Nboot)))
  
  M1A_All[v,25] = mean(as.numeric(M1A_oddsratio[v,]))
  M1A_All[v,26] = mean(as.numeric(M1A_oddsratio[v,])) - (qnorm(0.975)*((sd(as.numeric(M1A_oddsratio[v,])))/sqrt(Nboot)))
  M1A_All[v,27] = mean(as.numeric(M1A_oddsratio[v,])) + (qnorm(0.975)*((sd(as.numeric(M1A_oddsratio[v,])))/sqrt(Nboot)))
  
  print(v)
}

M1A_All[1+nVariav_M1A_Desag+1,7] = mean(M1A_LogLik_lm0)
M1A_All[1+nVariav_M1A_Desag+1,8] = mean(M1A_LogLik_lm0) - (qnorm(0.975)*((sd(as.numeric(M1A_LogLik_lm0)))/sqrt(Nboot)))
M1A_All[1+nVariav_M1A_Desag+1,9] = mean(M1A_LogLik_lm0) + (qnorm(0.975)*((sd(as.numeric(M1A_LogLik_lm0)))/sqrt(Nboot)))

M1A_All[1+nVariav_M1A_Desag+1,10] = mean(M1A_LogLik_lM1A)
M1A_All[1+nVariav_M1A_Desag+1,11] = mean(M1A_LogLik_lM1A) - (qnorm(0.975)*((sd(as.numeric(M1A_LogLik_lM1A)))/sqrt(Nboot)))
M1A_All[1+nVariav_M1A_Desag+1,12] = mean(M1A_LogLik_lM1A) + (qnorm(0.975)*((sd(as.numeric(M1A_LogLik_lM1A)))/sqrt(Nboot)))

M1A_All[1+nVariav_M1A_Desag+1,13] = mean(M1A_LRchi2_M1Am0)
M1A_All[1+nVariav_M1A_Desag+1,14] = mean(M1A_LRchi2_M1Am0) - (qnorm(0.975)*((sd(as.numeric(M1A_LRchi2_M1Am0)))/sqrt(Nboot)))
M1A_All[1+nVariav_M1A_Desag+1,15] = mean(M1A_LRchi2_M1Am0) + (qnorm(0.975)*((sd(as.numeric(M1A_LRchi2_M1Am0)))/sqrt(Nboot)))

M1A_All[1+nVariav_M1A_Desag+1,16] = mean(M1A_LRchi2_M1Am0.pvalue)
M1A_All[1+nVariav_M1A_Desag+1,17] = mean(M1A_LRchi2_M1Am0.pvalue) - (qnorm(0.975)*((sd(as.numeric(M1A_LRchi2_M1Am0.pvalue)))/sqrt(Nboot)))
M1A_All[1+nVariav_M1A_Desag+1,18] = mean(M1A_LRchi2_M1Am0.pvalue) + (qnorm(0.975)*((sd(as.numeric(M1A_LRchi2_M1Am0.pvalue)))/sqrt(Nboot)))

M1A_All[1+nVariav_M1A_Desag+1,19] = mean(M1A_r2Nagelkerke)
M1A_All[1+nVariav_M1A_Desag+1,20] = mean(M1A_r2Nagelkerke) - (qnorm(0.975)*((sd(as.numeric(M1A_r2Nagelkerke)))/sqrt(Nboot)))
M1A_All[1+nVariav_M1A_Desag+1,21] = mean(M1A_r2Nagelkerke) + (qnorm(0.975)*((sd(as.numeric(M1A_r2Nagelkerke)))/sqrt(Nboot)))

M1A_All[1+nVariav_M1A_Desag+1,22] = mean(M1A_aic)
M1A_All[1+nVariav_M1A_Desag+1,23] = mean(M1A_aic) - (qnorm(0.975)*((sd(as.numeric(M1A_aic)))/sqrt(Nboot)))
M1A_All[1+nVariav_M1A_Desag+1,24] = mean(M1A_aic) + (qnorm(0.975)*((sd(as.numeric(M1A_aic)))/sqrt(Nboot)))

write.csv2 (M1A_All,"C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/Model_2010_M1A.csv")




###################################### EM 3d plot - M1A ====
library(ggplot2)

rm(list=ls()[! ls() %in% c("pts_data", "data")])

#Mean values for RMSP
Rn0d14d = 83.17
Slope = 7.0921
Veg2010 = 0.1004
MnI2010 = 2647
Sbn2010_0 = 0
Sbn2010_1 = 1
Dm2010 = 13.323 
PSMF10 = 0.09
PSEsg10 = 0.26

#Mean parameters
P_Intercept = -3.5869
P_Rn0d14d = 0.0163
P_Slope = 0.0621
P_Veg2010 = 1.7894
P_MnI2010 = -0.0001
P_Sbn2010 = 1.5335
P_Dm2010 = 0.0370
P_PSMF10 = 1.1494
P_PSEsg10 = -1.7180

varx = c("Dm2010")
max_x = 40
val_x = seq(1,max_x,1)

vary = c("Slope")
max_y = 80
val_y = seq(1,max_y,(max_y/max_x))

f0 <- function(x, y) { 
  r = exp(P_Intercept + 
            P_Rn0d14d*Rn0d14d + 
            P_Slope*y + 
            P_Veg2010*Veg2010 + 
            P_MnI2010*MnI2010 +
            P_Sbn2010*Sbn2010_0 + 
            P_Dm2010*x +
            P_PSMF10*PSMF10 + 
            P_PSEsg10*PSEsg10)
}
Val_grid_0 = outer(val_x, val_y, f0)
max(Val_grid_0)
par(fg=NA, col="black")
tit = paste("odds ratio", "\n Regular Settlements")
p = filled.contour(x=val_x,y=val_y,z=Val_grid_0, 
                   color.palette=colorRampPalette(c("Turquoise", "LightCoral")),
                   plot.title=title(xlab=varx, ylab=vary), 
                   nlevels=200,
                   plot.axes = {axis(side=1, at=val_y, labels=val_y)
                     axis(side=2, at=val_x, labels=val_x) },
                   key.title=title(main="odds ratio"),
                   key.axes = axis(4, seq(0, 200, by = 10)))


f1 <- function(x, y) { 
  r = exp(P_Intercept + 
            P_Rn0d14d*Rn0d14d + 
            P_Slope*y + 
            P_Veg2010*Veg2010 + 
            P_MnI2010*MnI2010 +
            P_Sbn2010*Sbn2010_1 + 
            P_Dm2010*x +
            P_PSMF10*PSMF10 + 
            P_PSEsg10*PSEsg10)
}
Val_grid_1 = outer(val_x, val_y, f1)
max(Val_grid_1)
par(fg=NA, col="black")
tit = paste("odds ratio", "\n Subnormal Settlements")
p = filled.contour(x=val_x,y=val_y,z=Val_grid_1, 
                   color.palette=colorRampPalette(c("Turquoise", "LightCoral")),
                   plot.title=title(xlab=varx, ylab=vary), 
                   nlevels=200,
                   plot.axes = {axis(side=1, at=val_y, labels=val_y)
                     axis(side=2, at=val_x, labels=val_x) },
                   key.title=title(main=tit),
                   key.axes = axis(4, seq(0, 200, by = 10)))



###################################### EM Risk Map - M1A ====
library (raster)

rm(list=ls()[! ls() %in% c("pts_data", "data")])

Data_Slope_foc = raster("D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_Slope_foc.tif")
Data_Veg2010_foc = raster("D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_Veg2010_foc.tif")
Data_MeanInc2010_foc = raster("D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_MeanInc2010_foc.tif")
Data_Subn2010_foc = raster("D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_Subn2010_foc.tif")
Data_Dom2010_foc = raster("D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_Dom2010_foc.tif")
Data_SMF10_foc = raster("D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_SMF10_foc.tif")
Data_SEsg10_foc = raster("D:/_Arquivo/Doutorado/ArtigoSETS/Estat_Inter/Data_SEsg10_foc.tif")

Rn0d14d = 83.17

logits_raster = -3.5869 + 
  0.0163*83.17 +
  0.0621*Data_Slope_foc + 
  1.7894*Data_Veg2010_foc +
  -0.0001*Data_MeanInc2010_foc + 
  1.5335*Data_Subn2010_foc +
  0.0370*Data_Dom2010_foc + 
  1.1494*Data_SMF10_foc + 
  -1.7180*Data_SEsg10_foc

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

prob_raster = logit2prob(logits_raster)
plot(prob_raster)
writeRaster (prob_raster, "C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/Output-Model/ModelM1A_ProbMap.tif", overwrite=F, datatype='FLT4S')






########################################### EM (Evaluation Model) =================================
rm(list=ls())
gc()

library(ROCR)
library(data.table)

# rm(list=ls()[! ls() %in% c("pts_data_test")])
pts_data_test = read.csv2 ("C:/ADriveSync/Andando/Publica/EmAndamento/SETS/Processa/Estat/DataAll_Test_PTS.csv", header=T)

Ano = 2010

data_test = as.data.frame(pts_data_test)

data_test$Sbn2010 = as.factor(data_test$Sbn2010)

data_test$SubnChn [data_test$SubnChn==2] = 0
data_test$SubnChn = as.factor(data_test$SubnChn)

data_test$Suscep2e3 [data_test$Suscep==1] = 0
data_test$Suscep2e3 [data_test$Suscep==2] = 1
data_test$Suscep2e3 [data_test$Suscep==3] = 1
data_test$Suscep2e3 = as.factor(data_test$Suscep2e3)

data_test$Suscep3 [data_test$Suscep==1] = 0
data_test$Suscep3 [data_test$Suscep==2] = 0
data_test$Suscep3 [data_test$Suscep==3] = 1
data_test$Suscep3 = as.factor(data_test$Suscep3)

data_test$Suscep = as.factor(data_test$Suscep)
unique(data_test$Suscep)

tab_class = as.data.frame(data_test$Class)
names(tab_class) = c("Class")
tab_class$Cl = NA
tab_class$Cl[tab_class$Class=="yes"] = 1
tab_class$Cl[tab_class$Class=="no"] = 0
data_test$Class = as.factor(tab_class$Cl)
rm(tab_class)

data_test$AspectD = NA
data_test$AspectD [is.na(data_test$Aspect)] = "Flat"
data_test$AspectD [data_test$Aspect==0] = "N"
data_test$AspectD [data_test$Aspect>=(337.5)| data_test$Aspect<(22.5)] = "N"
data_test$AspectD [data_test$Aspect>=(22.5) & data_test$Aspect<(67.5)] = "NE"
data_test$AspectD [data_test$Aspect>=(67.5) & data_test$Aspect<(112.5)] = "E"
data_test$AspectD [data_test$Aspect>=(112.5) & data_test$Aspect<(157.5)] = "SE"
data_test$AspectD [data_test$Aspect>=(157.5) & data_test$Aspect<(202.5)] = "S"
data_test$AspectD [data_test$Aspect>=(202.5) & data_test$Aspect<(247.5)] = "SW"
data_test$AspectD [data_test$Aspect>=(247.5) & data_test$Aspect<(292.5)] = "W"
data_test$AspectD [data_test$Aspect>=(292.5) & data_test$Aspect<(337.5)] = "NW"
data_test$AspectD = as.factor(data_test$AspectD)
unique(data_test$AspectD)

data_test$AspectDN = 0
data_test$AspectDN [data_test$AspectD=="N"] = 1
data_test$AspectDN = as.factor(data_test$AspectDN)
data_test$AspectDNE = 0
data_test$AspectDNE [data_test$AspectD=="NE"] = 1
data_test$AspectDNE = as.factor(data_test$AspectDNE)
data_test$AspectDE = 0
data_test$AspectDE [data_test$AspectD=="E"] = 1
data_test$AspectDE = as.factor(data_test$AspectDE)
data_test$AspectDSE = 0
data_test$AspectDSE [data_test$AspectD=="SE"] = 1
data_test$AspectDSE = as.factor(data_test$AspectDSE)
data_test$AspectDS = 0
data_test$AspectDS [data_test$AspectD=="S"] = 1
data_test$AspectDS = as.factor(data_test$AspectDS)
data_test$AspectDSW = 0
data_test$AspectDSW [data_test$AspectD=="SW"] = 1
data_test$AspectDSW = as.factor(data_test$AspectDSW)
data_test$AspectDW = 0
data_test$AspectDW [data_test$AspectD=="W"] = 1
data_test$AspectDW = as.factor(data_test$AspectDW)
data_test$AspectDNW = 0
data_test$AspectDNW [data_test$AspectD=="NW"] = 1
data_test$AspectDNW = as.factor(data_test$AspectDNW)
data_test$AspectDFlat = 0
data_test$AspectDFlat [data_test$AspectD=="Flat"] = 1
data_test$AspectDFlat = as.factor(data_test$AspectDFlat)

data_test$AspectD4 = NA
data_test$AspectD4 [is.na(data_test$Aspect)] = "0" #"Flat"
data_test$AspectD4 [data_test$Aspect==0] =  "1" #"N"
data_test$AspectD4 [data_test$Aspect>=(315)| data_test$Aspect<(45)] =  "1" #"N"
data_test$AspectD4 [data_test$Aspect>=(45) & data_test$Aspect<(135)] =  "2" #"E"
data_test$AspectD4 [data_test$Aspect>=(135) & data_test$Aspect<(225)] =  "3" #"S"
data_test$AspectD4 [data_test$Aspect>=(225) & data_test$Aspect<(315)] =  "4" #"W"
data_test$AspectD4 = as.factor(data_test$AspectD4)
unique(data_test$AspectD4)

data_test$AspectD4N = 0
data_test$AspectD4N [data_test$AspectD=="N"] = 1
data_test$AspectD4N = as.factor(data_test$AspectD4N)
data_test$AspectD4E = 0
data_test$AspectD4E [data_test$AspectD=="E"] = 1
data_test$AspectD4E = as.factor(data_test$AspectD4E)
data_test$AspectD4S = 0
data_test$AspectD4S [data_test$AspectD=="S"] = 1
data_test$AspectD4S = as.factor(data_test$AspectD4S)
data_test$AspectD4W = 0
data_test$AspectD4W [data_test$AspectD=="W"] = 1
data_test$AspectD4W = as.factor(data_test$AspectD4W)

data_test = droplevels(data_test)

rm(list=ls()[! ls() %in% c("pts_data_test", "data_test")])



###################################### EM Roc - M1A =================================
Variav_M1A_test = c("ID", "Class",
                    "Rn0d14d",
                    "Slope","Veg2010",
                    "MnI2010",
                    "Sbn2010",
                    "Dm2010",
                    "PSMF10","PSEsg10")
nVariav_M1A_test = length(Variav_M1A_test)-1
Variav_M1A_test_Desag = c("Rn0d14d",
                          "Slope","Veg2010",
                          "MnI2010",
                          "Sbn20101",
                          "Dm2010",
                          "PSMF10","PSEsg10")
nVariav_M1A_test_Desag = length(Variav_M1A_test_Desag)

data_test_M1A = data.table(data_test[,Variav_M1A_test])
setkeyv(data_test_M1A,c("Class"))
table(data_test_M1A$Class)

data_test_M1A_s = data_test_M1A
data_test_M1A_s$Sbn2010 = as.numeric(as.character(data_test_M1A_s$Sbn2010))

logits = -3.5869 + 
  0.0163*data_test_M1A_s$Rn0d14d +
  0.0621*data_test_M1A_s$Slope + 
  1.7894*data_test_M1A_s$Veg2010 +
  -0.0001*data_test_M1A_s$MnI2010 + 
  1.5335*data_test_M1A_s$Sbn2010 +
  0.0370*data_test_M1A_s$Dm2010 + 
  1.1494*data_test_M1A_s$PSMF10 + 
  -1.7180*data_test_M1A_s$PSEsg10

#https://sebastiansauer.github.io/convert_logit2prob/ 
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

prob_Logit = logit2prob(logits)
pred_Logit <- prediction(prob_Logit, data_test_M1A$Class)
perf_Logit <- performance(pred_Logit, measure = "tpr", x.measure = "fpr")
plot(perf_Logit)
auc_Logit <- performance(pred_Logit, measure = "auc")
m.auc_Logit <- auc_Logit@y.values[[1]]
m.auc_Logit

library(pROC)
plot(roc(data_test_M1A$Class, prob_Logit, direction="<"),
     col="gray", lwd=3)



################################################ Biblio ---------------------------------

# Creating two datasets
dataIndex = createDataPartition(data_regr$Class, p=0.8, list=FALSE)
data_regr_train = data_regr[dataIndex,]
table(data_regr_train$Class)
data_regr_test  = data_regr[-dataIndex,]
table(data_regr_test$Class)
Nsamples_train = nrow(data_regr_train[data_regr_train$Class==1,])
Nsamples_test = nrow(data_regr_test[data_regr_test$Class==1,])
tmp3 = rbind((data_regr_train[data_regr_train$Class==1,]), 
             (data_regr_train[data_regr_train$Class==0,][sample(nrow(data_regr_train[data_regr_train$Class==0,]), Nsamples_train, replace=F),])) 
table(tmp3$Class)

# Standar error vs standard deviation
#https://www.r-bloggers.com/standard-deviation-vs-standard-error/
sd(x)
#computation of the standard error of the mean
sem<-sd(x)/sqrt(length(x))
#95% confidence intervals of the mean
c(mean(x)-2*sem,mean(x)+2*sem)

# Calculating the Model
m=glm(formula=Class ~ ., family=binomial(link="logit"), data=tmp3)

# Statistical Tests for Individual Predictors -> Likelihood ratio test
#HOSMER, D. W.; LEMESHOW, S. Applied logistic regression. 2nd. Ed. ed. [S.l.]: John Wiley & Sons, Inc., 2000. 
#Case of a single independent variable, we first fit a model containing only the constant term. 
#We then fit a model containing only the constant term. This gives rise to a new log likelihood. 
#The likelihood ratio is obtained by multiplying the difference between these two values by -2.
#https://stats.stackexchange.com/questions/6505/likelihood-ratio-test-in-r
library(lmtest)
lrtest(m, m0)

# multicolinearity of variables -> VIF
#http://scg.sdsu.edu/logit_r/
#The VIF is commonly used to check multicollinearity of conditioning factors in landslide studies (Tien Bui et al. 2011; 2016) 
#On normal VIF in multiple regression, we attempt to eliminate variables with a VIF higher than 5
library(car)
vif(m_M0A)


# Statistical Tests for Individual Predictors -> Stepwise
#http://scg.sdsu.edu/logit_r/
#delete variables that do not significantly add to the fit
m_sw = step(m)
summary(m_sw)
m_sw$anova

# Statistical Tests for Individual Predictors -> Variable importance
#https://www.r-bloggers.com/evaluating-logistic-regression-models/
#relative importance of individual predictors in the model
varImp = varImp(m)
colSums(varImp)
estat = (sum.m$coefficients [-1,3] - sum.m$coefficients [-1,1]) / sum.m$coefficients [-1,2]

# Statistical Tests for Individual Predictors -> Wald Test
#https://www.r-bloggers.com/evaluating-logistic-regression-models/
#Evaluate the statistical significance of each coefficient in the model.
#Calculated by taking the ratio of the square of the regression coefficient to the square of the standard error of the coefficient. 
#Test the hypothesis that the coefficient of an independent variable in the model is significantly different from zero. 
#p-value for Wald-test less than 0.05: reject H0 or coefficient is significantly different from zero
library(survey)
regTermTest(m, ***VARIAVEL***)
#or
library(epiDisplay)
m.display = logistic.display(m)
m.display$table[,c("P(Wald's test)")]


# Goodness of Fit -> Likelihood Ratio Test (LR-test) -> Importance of any variable
#https://www.r-bloggers.com/evaluating-logistic-regression-models/
#compares the likelihood of the data under the full model against the likelihood of the data under a model with fewer predictors
#p-value for LR-test less than 0.05: model with this variable is better than without it.
#Or exclude variables with pvalue>0.05
library(epiDisplay)
m.display = logistic.display(m)
m.display$table[,c("P(LR-test)")]


# Goodness of Fit -> Pseudo R2 -> explains the proportion of variance in the dependent variable that is explained by the predictors
#https://www.r-bloggers.com/evaluating-logistic-regression-models/
#https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-what-are-pseudo-r-squareds/
#Most notable is McFadden's R2, which is defined as 1???[ln(LM)/ln(L0)] where ln(LM) is the log likelihood value 
#for the fitted model and ln(L0) is the log likelihood for the null model with only an intercept as a predictor.
library(pscl)
m.pR2 = pR2(m)
m.pR2.McFadden = as.numeric(m.pR2[4])
m.pR2.McFadden

# Goodness of Fit -> Likelihood-Ratio-Based R2
#calculate the likelihood-ratio-based R^2 for generalized linear models
library(rsq)
rsq.lr(m, adj=F)
rsq.lr(m, adj=T)

# Goodness of Fit -> Partial R2
#Calculate the coefficient of partial determination, aka partial R^2, for both linear and generalized linear models
library(rsq)
rsq.partial(m, adj=F, type='lr') #default: variance-function-based
rsq.partial(m, adj=T, type='lr') #default: variance-function-based


# Validation of Predicted Values -> ROC Curve -> classifier performance
#https://www.r-bloggers.com/evaluating-logistic-regression-models/
#This metric ranges from 0.50 to 1.00, and
#values above 0.80 indicate that the model does a good job
#in discriminating between the two categories which comprise our target variable. 
library(ROCR)
tmp3_test = rbind((data_regr_test[data_regr_test$Class==1,]),
                  (data_regr_test[data_regr_test$Class==0,][sample(nrow(data_regr_test[data_regr_test$Class==0,]), Nsamples_test, replace=F),])) 
prob <- predict(m, newdata=tmp3_test, type="response")
pred <- prediction(prob, tmp3_test$Class)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
m.auc <- auc@y.values[[1]]
m.auc


# #create a "fake" model and still use predict
# newmodel <- makeglm(Class ~  MnI2010+Sbn2010+Dm2010+PSEsg10+PSMF10+Veg2010+Slope+Rn0d14d, 
#                     family = binomial(link = "logit"), data = tmp_M1, 
#                     -3.661, MnI2010=-0.0001, Sbn2010=1.5431, 
#                     Dm2010=0.0363, PSEsg10=-1.7510, PSMF10=1.1608,
#                     Veg2010=1.85553, Slope=0.0623, Rn0d14d=0.0163)

library(pROC)
plot(roc(data_test_M1A$Class, prob_Logit, direction="<"),
     col="gray", lwd=3)

#https://www.r-bloggers.com/introduction-to-the-rms-package/
library(rms)
head(tmp_M1A)

dddtmp_M1A <- datadist(tmp_M1A)
options(datadist="dddtmp_M1A")
f   <- lrm(Class ~ ., data=tmp_M1A, x=TRUE, y=TRUE)
f
dddtmp_M0A <- datadist(tmp_M0A)
options(datadist="dddtmp_M0A")
f   <- lrm(Class ~ ., data=tmp_M0A, x=TRUE, y=TRUE)
f
summary(f)
plot(anova(f), what='proportion chisq') # relative importance
pred = Predict(f, fun=plogis)
plot(Predict(f, fun=plogis)) # predicted values
rms::validate(f, method="boot", B=500) # bootstrapped validation
my.calib <- rms::calibrate(f, method="boot", B=500) # model calibration
plot(my.calib, las=1)

s = summary(Class ~ MnI2010 + Sbn2010 + Dm2010 + PSEsg10 + PSMF10 + Veg2010 + Slope + Rn0d14d, data=data_M1_noNA)
plot(s, main= '' , xlim=c(0,1), subtitles=FALSE )

data_M1_noNA$Class = as.numeric(data_M1_noNA$Class)
s2 = summary(Class ~ MnI2010 + Sbn2010 + Dm2010 + PSEsg10 + PSMF10 + Veg2010 + Slope + Rn0d14d, data=data_M1_noNA)
s2
plot(s2, main= '' , xlim=c(1,1.002), subtitles=FALSE )

# Fig. 12.1 Univariable summaries of Titanic survival
latex (data_M1, file= '' )

#Fig. 12.5 Effects of predictors on probability of survival of Titanic passengers, esti- mated for zero siblings or spouses
p = Predict(f, age , sex , pclass , sibsp =0, fun=plogis ) 
ggplot (p)


# 3d Plotting
library(plotly)
p <- plot_ly(x = kd$x, y = kd$y, z = kd$z) %>% add_surface()
p <- plot_ly(showscale = FALSE) %>%
  add_surface(z = ~z) %>%
  add_surface(z = ~z2, opacity = 0.98) %>%
  add_surface(z = ~z3, opacity = 0.98)


# Difference between odds and risk
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4640017/
#Calculation of risk requires the use of "people at risk" as the denominator.
#OR is used as a measure of the strength of association between exposure and outcome
library(oddsratio)
incr = list(Rn0d14d=1,Slope=1,Veg2010=0.05,MnI2010=1000, Sbn20101=1, Dm2010=1)
incr = list(Rn0d14d=1,Slope=1,Veg2010=1,MnI2010=1, Sbn20101=1, Dm2010=1)

x = or_glm(tmp_M1, m_M1, incr, CI = 0.95)
exp(coef(m_M1))
#Odds ratio can only be calculate as exp(coef) when dichotomous variables are coded with 0 and 1
#para variáveis contínuas o próprio parâmetro é o odds ratio



## Plot 3d
#https://sebastiansauer.github.io/convert_logit2prob/ 
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

#Mean values for RMSP
Rn0d14d = 83.17
Slope = 7.0921
Veg2010 = 0.1004
MnI2010 = 2647
Sbn2010_0 = 0
Sbn2010_1 = 1
Dm2010 = 13.323 
PSMF10 = 0.09
PSEsg10 = 0.26

#Mean parameters
P_Intercept = -3.5869
P_Rn0d14d = 0.0163
P_Slope = 0.0621
P_Veg2010 = 1.7894
P_MnI2010 = -0.0001
P_Sbn2010 = 1.5335
P_Dm2010 = 0.0370
P_PSMF10 = 1.1494
P_PSEsg10 = -1.7180

varx = c("Dm2010")
max_x = 40
val_x = seq(1,max_x,1)

vary = c("Slope")
max_y = 40
val_y = seq(1,max_y,(max_y/max_x))

f0 <- function(x, y) { 
  r = exp(P_Intercept + 
            P_Rn0d14d*Rn0d14d + 
            P_Slope*y + 
            P_Veg2010*Veg2010 + 
            P_MnI2010*MnI2010 +
            P_Sbn2010*Sbn2010_0 + 
            P_Dm2010*x +
            P_PSMF10*PSMF10 + 
            P_PSEsg10*PSEsg10)
}
Val_matrix_0 = outer(val_x, val_y, f0)
max(Val_matrix_0)

f1 <- function(x, y) { 
  r = exp(P_Intercept + 
            P_Rn0d14d*Rn0d14d + 
            P_Slope*y + 
            P_Veg2010*Veg2010 + 
            P_MnI2010*MnI2010 +
            P_Sbn2010*Sbn2010_1 + 
            P_Dm2010*x +
            P_PSMF10*PSMF10 + 
            P_PSEsg10*PSEsg10)
}
Val_matrix_1 = outer(val_x, val_y, f1)
max(Val_matrix_1)



Val_df = expand.grid(val_x,val_y)
names(Val_df) = c("val_x", "val_y")

Val_df_1 = Val_df

Val_df_1$logits = (-3.5869 + 0.0163*Rn0d14d +
                     0.0621*Val_df_1$val_y + 
                     1.7894*Veg2010 - 0.0001*MnI2010 + 1.5335*Sbn2010_1 +
                     0.0370*Val_df_1$val_x + 
                     1.1494*PSMF10 - 1.7180*PSEsg10)
Val_df_1$val_z_prob = logit2prob(Val_df_1$logits)
Val_df_1$val_z_odds = exp(Val_df_1$val_z_prob)
# plot(Val_grid$val_x, Val_grid$val_y, pch=19)
# plot(Val_grid$val_x, Val_grid$val_y, pch=19, cex=Val_grid$val_z)
x = Val_df_1$val_x
y = Val_df_1$val_y
z = Val_df_1$val_z_odds








library(plotly)
library(magrittr)
plot_ly(x=x,y=y,z=z, type="surface")
p <- plot_ly(x=x, y=y, z=z) %>% add_surface()
p <- plot_ly(showscale = FALSE) %>%
  add_surface(z = ~z) %>%
  add_surface(z = ~z2, opacity = 0.98) %>%
  add_surface(z = ~z3, opacity = 0.98)

plot_ly(x=x, y=y, z=z,type="scatter3d")
plot_ly(x=x, y=y, z=z,type="surface")



library(rgl)
persp3d(Val_matrix_1, col="skyblue")
persp3d(Val_matrix_0, col="skyblue")



df = Val_df_1[,c("val_x", "val_y", "val_z_odds")]
#plot3d(df) # What it looks like in 3d
ggplot(df, aes(x=val_x,y=val_y,z=val_z_odds, fill=val_z_odds)) + 
  geom_contour(binwidth=0.1, aes(color= ..level..)) + 
  theme_classic()#what it looks like projected into 2d
ggplot(df, aes(x=val_x,y=val_y)) + geom_point(aes(color = val_z_odds), size = 7) +
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07"))


library(akima)
im <- with(df,interp(val_x,val_y,val_z_odds))
with(im,image(x,y,z))
