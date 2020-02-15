## Looad library

library(tidyverse)
library(raster)
library(ithir)
library(sp)
library(randomForest)
library(rpart)
library(gstat)
library(caret)
library(lattice)


## Importation de donnees

data<-read_csv2("ex.csv",col_types = cols(Horizon=col_factor(levels = c(1:7))))

data1<-read_csv2("donnees soter.csv",col_types = cols(Horizon=col_factor(levels = c(1:7))))


## Regard sur la normalite du carbone


(plot1<-data%>%ggplot()+geom_histogram(aes(x=log(total_carbone)),binwidth = 0.25))

(plot2<-data1%>%ggplot()+geom_histogram(aes(x=log(total_carbone)),binwidth = 0.25))

## Boxplot du teneur en carbone en fonction des Horizons

(plot3<-data1%>%ggplot(aes(x=reorder(Horizon,log(total_carbone),FUN = median),y=log(total_carbone),color=Horizon))+geom_boxplot()+geom_point()+xlab("Horizon")+ylim(0,5))

(plot3<-data%>%ggplot(aes(x=reorder(Horizon,log(total_carbone),FUN = median),y=log(total_carbone),color=Horizon))+geom_boxplot()+geom_point()+xlab("Horizon")+ylim(0,5))

## Conclusion ; Quand on avance en profondeur la teneur en carbone diminue

## Importons les fichiers raster

fichier<-list.files(path="C:/Users/pc/Desktop/soil Senegal", pattern = "\\.tif$",full.names = TRUE)
fichier

## Assignons les rasters 1 ? 1

dem<-raster(fichier[11])

vier<-raster(fichier[3])
## Reprojection et denommer

dem<-projectRaster(dem,crs = "+init=epsg:4326",method = "bilinear")

names(dem)<-"dem"

vier<-projectRaster(vier,crs = "+init=epsg:4326",method = "bilinear")

## Map Aspet and slope

elev.terr <- terrain(dem, opt = c("slope", "aspect"), unit ="radians")

## assignation e la pente et de l aspect et cartographie

slope<-elev.terr$slope

aspet<-elev.terr$aspect

plot(elev.terr$slope,main="Slope")

plot(elev.terr$aspect,main="Aspect")

## Regroupons les differents raters:

reg<-stack(dem,aspet,slope)

reg

## Assignons un nouveau nom au donné de base 

dat_mod<-data

names(dat_mod)[1:2]<-c("x","y")

## transform le data.frame en  spatial data

coordinates(dat_mod)<-~x+y

proj4string(dat_mod)<-crs("+init=epsg:4326")

da_ras<-raster::extract(reg,dat_mod,method="simple",sp=1) 

don_final<-as.data.frame(da_ras)

## Correlation entre les differents variable

cor(don_final[complete.cases(don_final[,c("CECS","EXNA","total_azote","sable_total","total_carbone","PH","EXCA","Argile")]),c("total_azote","slope","EXCA","EXMG","aspect","Argile","sable_total","CECS","Limon","EXNA","EXCK","PH")] ,don_final[complete.cases(don_final[,c("total_carbone","PH","EXCA","Argile","sable_total","total_azote","CECS","EXNA")]),"total_carbone"])

head(don_final,3)

## Modelisation TEST

model_test<-lm(log(total_carbone)~EXCA+dem+total_azote,data = model_data)

summary(model_test)

## Evaluation du modele

goof(log(model_data$total_carbone),model_test$fitted.values)

## K fold 

set.seed(123)

## vecteur pour entrainement du modele

training<-sample(nrow(model_data),0.7*nrow(model_data))

model1<-lm(log(total_carbone)~EXCA+total_azote+dem,data = model_data[training,])

pred1<-predict(model1,model_data[training,])

summary(model1)

## Evaluation du modele

goof(log(model_data$total_carbone[training]),pred1)

## Modele de Validation 

model1v<-lm(log(total_carbone)~EXCA+total_azote+dem,data = model_data[-training,])

summary(model1v)

goof(log(model_data$total_carbone[-training]),model1v$fitted.values)

## Random Forest

## Calibration du modele

model2<-randomForest(log(total_carbone)~total_azote+EXCA,data = model_data[training,],importance=TRUE,ntree=1000)

print(model2)

varImpPlot(model2)

goof(log(model_data$total_carbone[training]),model2$predicted)

## Modele de validation

model2v<-randomForest(log(total_carbone)~total_azote+EXCA,data = model_data[-training,],importance=TRUE,ntree=1000)

print(model2v)

varImpPlot(model2v)

goof(log(model_data$total_carbone[-training]),model2v$predicted)


## Arbre a decision modele calibration

model3<-rpart(log(total_carbone)~EXCA+total_azote,data = model_data[training,],control = rpart.control(minsplit = 30))

summary(model3)

printcp(model3)

plot(model3)

text(model3)

pred_tree<-predict(model3,model_data[training,])

## Evaluation du modele du modele de calibré

goof(log(model_data$total_carbone[training]),pred_tree)
  
## Modele de validation

model3v<-rpart(log(total_carbone)~EXCA+total_azote,data = model_data[-training,],control = rpart.control(minsplit = 30))

summary(model3v)

printcp(model3v)

plot(model3v)

text(model3v)

pred_tree<-predict(model3v,model_data[-training,])

## Evaluation du modele de validation

goof(log(model_data$total_carbone[-training]),pred_tree)



