library(highcharter)
library(tidyverse)
library(downloader)
library(rgdal)
library(sf)
library(ggplot2)
library(reshape2)
library(plotly)
library(rgdal)
library(stringr)
library(tmap)
library(readxl)
library(tidyr)
library(devtools)
library(hrbrthemes)
library(RColorBrewer)
library(janitor)
library(car)
library(geojsonio)
library(mapview)
library(crosstalk)
library(sp)
library(spdep)
library(fs)
library(nortest)
library(broom)
library(ggpubr)
library(tidypredict)
library(spatialreg)

##download boundary data
download.file("https://data.london.gov.uk/download/statistical-gis-boundary-files-london/9ba8c833-6370-4b11-abdc-314aa020d5e0/statistical-gis-boundaries-london.zip", 
              destfile="geo_data/statistical-gis-boundaries-london.zip")

##unzip boundary data
zipfile<-dir_info(here::here("geo_data")) %>%
  dplyr::filter(str_detect(path, ".zip")) %>%
  dplyr::select(path)%>%
  pull()%>%
  print()%>%
  as.character()%>%
  utils::unzip(exdir=here::here("geo_data"))

##read ward boundary data
Londonwards<-dir_info(here::here("geo_data", "statistical-gis-boundaries-london", "ESRI"))%>%
  filter(str_detect(path, "London_Ward_CityMerged.shp$"))%>%
  select(path)%>%
  pull()%>%
  st_read()
qtm(Londonwards)

##read attribute data
attribute_data <- read_excel(here::here("attribute_data","att_data.xlsx"),na = c("N/A"), col_names = TRUE)

##check type of attribute data
Typelist <- attribute_data %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="Features", 
               values_to="Feature_type")

##merge boundaries and geo data
joined_data <- Londonwards%>%
  left_join(.,
            attribute_data, 
            by = c("GSS_CODE" = "New Code"))

##obesity map
#tmap_mode("view")
Londonbb <- st_bbox(joined_data,
                    crs = st_crs(joined_data)) %>% 
  st_as_sfc()

##obesity visulization 
joined_data$`obesity rate` = joined_data$obesity_rate
obesity_map <- tm_shape(joined_data, bbbox = Londonbb) + 
  tm_polygons("obesity rate",
              breaks = c(5,15,25,35,45),
              palette="-RdYlBu",title="child obesity prevalence (%)")+
  tm_scale_bar(position = c("right", "bottom"), text.size = .8)+
  tm_layout(
    legend.position = c(0.7,0.3), 
    legend.text.size=.8, 
    legend.title.size = 1.1,
    #main.title = "child obesity prevalence rate (%)",
    main.title.position = "center",
    main.title.color = "black",
    main.title.size = 1.2,
    frame=FALSE)+
  tm_compass(north=0, position = c(0.8, 0.15),size =3) +
  
  #bottom left top right
  tm_layout(inner.margin=c(0.02,0.02,0.02,0.2))

obesity_map
##explore outliers (variable can be changed here)
obesity_rate <- boxplot(joined_data$obesity_rate, main="Boxplot of obesity rate",
                        ylab="obesity rate (%)",col = "#69b3a2")
obesity_rate

##plot histogram and density plot to explore distribution 
joined_data$`obesity rate` <- joined_data$obesity_rate
hist_obesity <- ggplot(data = joined_data, mapping = aes(
  x = `obesity rate`)) + 
  #ggtitle("obesity rate histogram and density")+
  theme(plot.title = element_text(hjust = 0.5),legend.position = c(0.85, 0.85))+
  geom_histogram(mapping = aes(y = stat(density)),bins = 10,fill="#69b3a2", color="#e9ecef") +
  geom_density(color = "red", size = 1,alpha = .2)+
  geom_vline(aes(xintercept=mean(obesity_rate, na.rm=T),color="mean"), 
             linetype="dashed", size=1)+
  geom_vline(aes(xintercept=median(obesity_rate, na.rm=T),color="median"),   
             linetype="dashed", size=1)+
  scale_color_manual(name = "mean and median", values = c(median = "blue", mean = "orange"))

hist_obesity

##plot qq plot
ggqqplot(joined_data$obesity_rate)

##test normality 
ad.test(joined_data$obesity_rate)#p-value = 1.895e-09
ad.test(joined_data$green_space)#p-value = < 2.2e-16
ad.test(joined_data$children_in_poverty)#p-value = 8.47e-08
ad.test(joined_data$no_qualifications)#p-value =0.1235
ad.test(joined_data$cars_per_household)#p-value = 2.422e-15
ad.test(joined_data$population_density)#p-value < 2.2e-16
ad.test(joined_data$no_employment)#p-value < 2.2e-16


## symbox() function in the car package is used to try a range of transfomations 
##choose proper method of normalization
symbox(~cars_per_household, 
       joined_data, 
       na.rm=T,
       powers=seq(-3,3,by=.5))

##data normalisation 
joined_data$obesity_rate_norm = (joined_data$obesity_rate)^2
joined_data$green_space_norm =(joined_data$green_space)^0.5
joined_data$children_in_poverty_norm = (joined_data$children_in_poverty)^1
joined_data$no_qualifications_norm = (joined_data$no_qualifications)^1
joined_data$cars_per_household_norm = (joined_data$cars_per_household)^0.5
joined_data$population_density_norm = (joined_data$population_density)^0.5
joined_data$no_employment_norm = (joined_data$no_employment)^0.5

#test normalised data
ad.test(joined_data$obesity_rate_norm)#p-value = 0.1205
ad.test(joined_data$green_space_norm)#p-value = 0.2191
ad.test(joined_data$children_in_poverty_norm)#p-value = 8.47e-08
ad.test(joined_data$no_qualifications_norm)#p-value = 0.1235
ad.test(joined_data$cars_per_household_norm)#p-value = 5.156e-05
ad.test(joined_data$population_density_norm)#p-value = 8.406e-05
ad.test(joined_data$population_density_norm)#p-value = 8.406e-05
ad.test(joined_data$no_employment_norm)#p-value = 4.302e-05

##scatter plot and linear fitting 
scatter <- qplot(x = `green_space_norm`, 
           y = `obesity_rate`, 
           data=joined_data)

scatter + stat_smooth(method="lm", se=FALSE, size=1) + 
  geom_jitter()

#run the linear regression model and store its outputs in an object called smodel
Modeldata<- joined_data%>%
  clean_names()%>%
  dplyr::select(obesity_rate,
                green_space)

#simple linear regression
smodel <- Modeldata %>%
  lm(obesity_rate ~
       green_space,
     data=.)

summary(smodel)
tidy(smodel)
glance(smodel)

Modeldata %>%
  tidypredict_to_column(smodel)

joined_data <- joined_data %>%
  clean_names()

##multiple linear regression
Modeldata2<- joined_data%>%
  clean_names()%>%
  dplyr::select(obesity_rate,green_space,children_in_poverty,no_qualifications,no_employment,cars_per_household,population_density)



test_model <- lm(obesity_rate  ~   children_in_poverty+ no_qualifications+no_employment+ cars_per_household + population_density + green_space, data = Modeldata2)

tidy(test_model)
summary(test_model)
vif(test_model)#threshold = 10

test_model_data <- test_model %>%
  augment(., Modeldata2)

hist(test_model_data$.resid)
ad.test(test_model_data$.resid) #normal distribution

#perform backward stepwise regression
backward <- step(test_model, direction='backward', scope=formula(test_model), trace=0)
backward$coefficients

final_model <-lm(obesity_rate ~ children_in_poverty + no_qualifications + no_employment+ cars_per_household+ population_density,data = Modeldata2)
summary(final_model)
vif(final_model)


##save residual 
final_model_data <- final_model %>%
  augment(., Modeldata2)

final_model_data%>%
  dplyr::select(.resid)%>%
  pull()%>%
  qplot()+ 
  geom_histogram() 

ad.test(final_model_data$.resid)##normal distrubuted 

## add to joined data
joined_data <- joined_data %>%
  mutate(final_model_resids = residuals(final_model))

##model diagnositcs. 
par(mfrow=c(2,2))  
plot(test_model)

##test autocorrelation
DW <- durbinWatsonTest(final_model)
tidy(DW)

##residual map for OLS final model 
joined_data$`residual value` <- joined_data$final_model_resids
OLS_residual_map <- tm_shape(joined_data, bbbox = Londonbb) + 
  tm_polygons("residual value",
              breaks = c(-10, -5, 0, 5, 10,15),
              palette="-RdYlBu",title="Residuals in OLS model")+
  tm_scale_bar(position = c("right", "bottom"), text.size = .8)+
  tm_layout(legend.position = c(0.73,0.3), 
            legend.text.size=.8, 
            legend.title.size = 1.1,
            #main.title = "Residuals in OLS model",
            main.title.position = "center",
            main.title.size = 1.2,
            main.title.color = "black",
            frame=FALSE)+
  tm_compass(north=0, position = c(0.8, 0.15)) +
  tm_layout(inner.margin=c(0.02,0.02,0.02,0.2))
OLS_residual_map

##calculate global moran I
#calculate the centroids of all Wards in London
coordsW <- joined_data%>%
  st_centroid()%>%
  st_geometry()

#plot(coordsW)

##spatial weights matrix
#a list of neighbours 
#LWard_nb <- joined_data %>%
#  poly2nb(., queen=T)

#nearest neighbours
knn_wards <-coordsW %>%
  knearneigh(., k=4)

LWard_knn <- knn_wards %>%
  knn2nb()

#plot(LWard_nb, st_geometry(coordsW), col="red")
plot(LWard_knn, st_geometry(coordsW), col="blue")

#spatial weights matrix
#Lward.queens_weight <- LWard_nb %>%
 # nb2listw(., style="C")

Lward.knn_4_weight <- LWard_knn %>%
  nb2listw(., style="C")

LWard_Local_obesity <- joined_data %>%
  pull(obesity_rate) %>%
  as.vector()%>%
  moran.test(., Lward.knn_4_weight)
LWard_Local_obesity

moran.plot(joined_data$obesity_rate, Lward.knn_4_weight)
#moran.plot(joined_data$obesity_rate, Lward.queens_weight)

#Queen <- joined_data %>%
#  st_drop_geometry()%>%
 # dplyr::select(final_model_resids)%>%
 # pull()%>%
 # moran.test(., Lward.queens_weight)%>%
#  tidy()
#Queen


Nearest_neighbour <- joined_data %>%
  st_drop_geometry()%>%
  dplyr::select(final_model_resids)%>%
  pull()%>%
  moran.test(., Lward.knn_4_weight)%>%
  tidy()

Nearest_neighbour


##local moran I
#use the localmoran function to generate I for each ward in the city

I_LWard_Local_obesity <- joined_data %>%
  pull(obesity_rate) %>%
  as.vector()%>%
  localmoran(., Lward.knn_4_weight)%>%
  as_tibble()

slice_head(I_LWard_Local_obesity, n=5)

joined_data <- joined_data %>%
  mutate(obesity_I = as.numeric(I_LWard_Local_obesity$Ii))%>%
  mutate(obesity_Iz =as.numeric(I_LWard_Local_obesity$Z.Ii))%>%
  mutate(obesity_p =as.numeric(I_LWard_Local_obesity$`Pr(z > 0)`))

Local_moran_map <- tm_shape(joined_data, bbbox = Londonbb) + 
  tm_polygons("obesity_Iz",
              breaks = c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000),
              palette=rev(brewer.pal(8, "RdGy")),title="Local Moran's I, Obesity rate")+
  tm_scale_bar(position = c("right", "bottom"), text.size = .8)+
  tm_layout(legend.position = c(0.73,0.3), 
            legend.text.size=.8, 
            legend.title.size = 1.1,
            #main.title = "Residuals in OLS model",
            main.title.position = "center",
            main.title.size = 1.2,
            main.title.color = "black",
            frame=FALSE)+
  tm_compass(north=0, position = c(0.8, 0.15)) +
  tm_layout(inner.margin=c(0.02,0.02,0.02,0.2))
Local_moran_map

joined_data$scale_obesity <- scale(joined_data$obesity_rate) %>% as.vector()
joined_data$lag_scale_obesity <- lag.listw(Lward.knn_4_weight, joined_data$scale_obesity)
summary(joined_data$lag_scale_obesity)
summary(joined_data$scale_obesity)
moran.plot(joined_data$lag_scale_obesity, Lward.knn_4_weight)

joined_data <- joined_data%>%
  mutate(sig_lisa = ifelse(joined_data$scale_obesity > 0 & joined_data$lag_scale_obesity > 0 & joined_data$obesity_p <= 0.05, "high-high",
                           ifelse(joined_data$scale_obesity <= 0 & joined_data$lag_scale_obesity <= 0 & joined_data$obesity_p <= 0.05, "low-low", 
                                  ifelse(joined_data$scale_obesity > 0 & joined_data$lag_scale_obesity <= 0 & joined_data$obesity_p <= 0.05, "high-low",
                                         ifelse(joined_data$scale_obesity <= 0 & joined_data$lag_scale_obesity > 0 & joined_data$obesity_p <= 0.05,"low-high", 
                                                "non-significant")))))

table(joined_data$sig_lisa)
#qtm(joined_data, fill="sig_lisa", fill.title="LISA")

lisa_map <- tm_shape(joined_data, bbbox = Londonbb) + 
  tm_polygons("sig_lisa",
              title="LISA, Obesity rate"
              )+
  tm_scale_bar(position = c("right", "bottom"), text.size = .8)+
  tm_layout(legend.position = c(0.73,0.3), 
            legend.text.size=.8, 
            legend.title.size = 1.1,
            #main.title = "Residuals in OLS model",
            #main.title.position = "center",
            #main.title.size = 1.2,
            #main.title.color = "black",
            frame=FALSE)+
  tm_compass(north=0, position = c(0.8, 0.15)) +
  tm_layout(inner.margin=c(0.02,0.02,0.02,0.2))
lisa_map

##spatial lag model
#slag_dv_final_model_queen <- lagsarlm(obesity_rate ~ children_in_poverty + no_qualifications + no_employment+ cars_per_household+ population_density,
     #                                 data = Modeldata2,
     #                                 nb2listw(LWard_nb, style="C"), 
     #                                 method = "eigen")


#tidy(slag_dv_final_model_queen)
#glance(slag_dv_final_model_queen)
#summary(slag_dv_final_model_queen)

#joined_data <- joined_data %>%
  #mutate(slag_dv_final_model_queen_resids = residuals(slag_dv_final_model_queen))

#run a spatially-lagged regression model
slag_dv_final_model_knn4 <- lagsarlm(obesity_rate ~ children_in_poverty + no_qualifications + no_employment+ cars_per_household+ population_density , data = Modeldata2, 
                                     nb2listw(LWard_knn, style="C"), 
                                     method = "eigen")


tidy(slag_dv_final_model_knn4)
summary(slag_dv_final_model_knn4)
glance(slag_dv_final_model_knn4)

joined_data <- joined_data %>%
  mutate(slag_dv_final_model_knn_resids = residuals(slag_dv_final_model_knn4))

##test the residual auto-correlation in spatial lag model
KNN4Moran <- joined_data %>%
  st_drop_geometry()%>%
  dplyr::select(slag_dv_final_model_knn_resids)%>%
  pull()%>%
  moran.test(., Lward.knn_4_weight)%>%
  tidy()

KNN4Moran

#joined_data <- joined_data %>%
  #mutate(slag_dv_final_model_queen_resids = residuals(slag_dv_final_model_queen))

#QEENMoran <- joined_data %>%
 # st_drop_geometry()%>%
 # dplyr::select(slag_dv_final_model_queen_resids)%>%
 # pull()%>%
 # moran.test(., Lward.knn_4_weight)%>%
  #tidy()

#QEENMoran

##spatial error model

semodel <- errorsarlm(obesity_rate ~ children_in_poverty + no_qualifications + no_employment+ cars_per_household+ population_density , data = Modeldata2,
                         nb2listw(LWard_knn, style="C"), 
                         method = "eigen")

tidy(semodel)
summary(semodel)
glance(semodel)
###choose spatial lag model with knn






