
p_median <- read.csv("data/Timeseries.FBI.Payout.median_shift.csv")
province_district <- read.csv("data/province district.csv")%>%
  rename(Province =1)
head(province_district)

### DROP no data and province polygons which failed to match
forecast_median=p_median%>%
  drop_na()%>%
  filter(Quantile == 100,mean_observed_SPI6 != 0)%>%
  left_join(y=province_district)%>%
  subset(Province != "NA") %>% # remove province "NA"
  dplyr::select(Year,Quantile,Province,Area,mean_observed_SPI6,mean_median_Forecasted_SPI6_belowQ)
head(forecast_median)
names(forecast_median)


forecast_median_class <- forecast_median %>%
  mutate(obs_mild_class = ifelse(mean_observed_SPI6 <= -0.5 & mean_observed_SPI6 >= -1, "drought", "no drought"),
         forecast_mild_class = ifelse(mean_median_Forecasted_SPI6_belowQ < -1 & mean_median_Forecasted_SPI6_belowQ >= -1.5, "drought", "no drought"),
       obs_moderate_class = ifelse(mean_observed_SPI6 <= -1 & mean_observed_SPI6 >= -1.5, "drought", "no drought"),
       forecast_moderate_class = ifelse(mean_median_Forecasted_SPI6_belowQ < -1 & mean_median_Forecasted_SPI6_belowQ >= -1.5, "drought", "no drought"),
       obs_severe_class = ifelse(mean_observed_SPI6 < -1.5 & mean_observed_SPI6 >= -2, "drought", "no drought"),
       forecast_severe_class = ifelse(mean_median_Forecasted_SPI6_belowQ < -1.5 & mean_median_Forecasted_SPI6_belowQ >= -2, "drought", "no drought"),
       obs_extreme_class = ifelse(mean_observed_SPI6 < -2, "drought", "no drought"),
       forecast_extreme_class = ifelse(mean_median_Forecasted_SPI6_belowQ < -2, "drought", "no drought"))


#write.csv(forecast_median_class, './output/csv/forecast_Q100_median_class_-1_-1.5_-2.csv')
############################

############ Perform accuracy assessment- National summaries

# first method provide more details
library(caret)
exp=as.factor(forecast_median_class$obs_mild_class)
pred=as.factor(forecast_median_class$forecast_mild_class)
# exp=as.factor(forecast_median_class$obs_extreme_class)
# pred=as.factor(forecast_median_class$forecast_extreme_class)
# exp=as.factor(forecast_median_class$obs_mild_class)
# pred=as.factor(forecast_median_class$forecast_mild_class)


conf_matrix <- caret::confusionMatrix(data=pred, reference = exp)
conf_matrix

#Using the confusionMatrix() function.
#confusionMatrix(table(pred, exp))
confusionMatrix(table(pred, exp), prevalence = 0.25)

# Reference
# #            Reference
# Prediction   drought no drought
# drought         48         39
# no drought      24         71
# 
# Accuracy : 0.6538          
# 95% CI : (0.5799, 0.7227)
# No Information Rate : 0.6044          
# P-Value [Acc > NIR] : 0.09809         
# 
# Kappa : 0.3013          
# 
# Mcnemar's Test P-Value : 0.07776         
#                                          
#             Sensitivity : 0.6667          
#             Specificity : 0.6455          
#          Pos Pred Value : 0.5517          
#          Neg Pred Value : 0.7474          
#              Prevalence : 0.3956          
#          Detection Rate : 0.2637          
#    Detection Prevalence : 0.4780          
#       Balanced Accuracy : 0.6561          
#                                           
#        'Positive' Class : drought   


# Use the second method to get the confusion matrix plot

library(cvms)
conf_matrix3=confusion_matrix(targets = forecast_median_class$obs_severe_class,
                              predictions = forecast_median_class$forecast_severe_class)
##Plot the confusion matrix
# first convert the matrix tod df/ tribble
conf_matrix4 = as_tibble(conf_matrix3)
conf_matrix <- plot_confusion_matrix(conf_matrix4$`Confusion Matrix`[[1]])
conf_matrix

########### Calculate manually on the data-frame ###############
########### Calculate manually on the data-frame ###############
national_accuracy <- subset(forecast_median_class, select = c(Area,obs_severe_class, forecast_severe_class) ) %>%
               mutate(
              class = case_when(
                obs_severe_class == "drought" & forecast_severe_class == "drought" ~ "True positive",
                obs_severe_class == "drought" & forecast_severe_class == "no drought" ~ "False positive",
                obs_severe_class ==  "no drought" & forecast_severe_class == "drought" ~ "False negative",
                    TRUE ~ "True negative" ),
              True_positive_count = sum(class== "True positive"),
              True_negative_count = sum(class== "True negative"),
              False_positive_count = sum(class== "False positive"),
              False_negative_count = sum(class== "False negative"),
              total_obser = n(),
              Sensitivity = percent(True_positive_count/(True_positive_count + False_negative_count), accuracy =1),
              Specificity = percent(True_negative_count/(True_negative_count + False_positive_count), accuracy = 1),
              Accuracy = percent((True_positive_count+True_negative_count)/total_obser, accuracy =1)
            )


############################## develop for individual polygon
polygon_accuracy <- subset(forecast_median_class, select = c(Area,obs_severe_class, forecast_severe_class) ) %>%
  group_by(Area)%>%
  mutate(
    class = case_when(
      obs_severe_class == "drought" & forecast_severe_class == "drought" ~ "True positive",
      obs_severe_class == "drought" & forecast_severe_class == "no drought" ~ "False positive",
      obs_severe_class ==  "no drought" & forecast_severe_class == "drought" ~ "False negative",
      TRUE ~ "True negative" ),
    True_positive_count = sum(class== "True positive"),
    True_negative_count = sum(class== "True negative"),
    False_positive_count = sum(class== "False positive"),
    False_negative_count = sum(class== "False negative"),
    total_obser = n(),
    Sensitivity = round((True_positive_count/(True_positive_count + False_negative_count))*100, digits = 0),
    Specificity = round((True_negative_count/(True_negative_count + False_positive_count))*100, digits = 0),
    Accuracy = round(((True_positive_count+True_negative_count)/total_obser)*100, digits = 0)
    # Sensitivity = percent(True_positive_count/(True_positive_count + False_negative_count), accuracy =1),
    # Specificity = percent(True_negative_count/(True_negative_count + False_positive_count), accuracy = 1),
    # Accuracy = percent((True_positive_count+True_negative_count)/total_obser, accuracy =1)
  ) %>%
   subset(select = -c(obs_severe_class,forecast_severe_class,class))

# Remove the duplicates
polygon_accuracy <- unique(polygon_accuracy)
# polygon_accuracy=  polygon_accuracy[!duplicated(polygon_accuracy), ]
write.csv(polygon_accuracy, "output/csv/polygon_accuracy_assesment_SPI_mild.csv")

######### generate a choropleth map of the accuracy assessment.
library(tmap)
moza<-shapefile("data/shp/vuln.shp")
qtm(moza) # plot shapefile

#Exploring the attributes table of shp
moza@data
# ID        Area Country
# 0  NA     Kaolack Senegal


#use of dplyr's left_join function to join the accuracy results to the shp.
library(dplyr)
moza@data <- left_join(moza@data, polygon_accuracy, by=c("Area" = "Area"))

writeOGR(moza, dsn = 'output/shp', layer = 'Senegal_accuracy_ass_SPI_mild', driver = "ESRI Shapefile")

##################### Use colour palletes to plot the shapefile ##################### 
tm_shape(moza) + 
  tm_polygons("Sensitivity", palette = "Reds") + 
  tm_legend(outside = TRUE)

#Use a quantile classification scheme with four classes (fill.n=4),
# qtm(moza, fill="Sensitivity", fill.style="quantile", 
#     fill.n=4,
#     fill.palette="Blues",
#     legend.text.size = 0.5,
#     layout.legend.position = c("right", "top"))
# 
# ###### Classify the map
# tm_shape(moza) + 
#   tm_polygons("Sensitivity", style = "fixed",palette = "Blues",
#               breaks = a) + 
#   tm_legend(outside = TRUE)

Sensitivity <- tm_shape(moza) + 
  tm_fill("Sensitivity", style="fixed", breaks=c(0, 50, 60, 70, 80, 90,100),
    labels=c("< 50", "50-60", "60-70", "70-80", "80-90", " > 90"),
          palette="Greens")  +
  tm_borders("grey") +
  tm_text("Sensitivity", size = "AREA") +
  #tm_legend(outside = TRUE, text.size = .5) +
  tm_layout(frame = TRUE)

Specificity <- tm_shape(moza) + 
  tm_fill("Specificity", style="fixed", breaks=c(0, 50, 60, 70, 80, 90,100),
          labels=c("< 50", "50-60", "60-70", "70-80", "80-90", " > 90"),
          palette="Greens")  +
  tm_borders("grey") +
  tm_text("Specificity", size = "AREA") +
  #tm_legend(outside = TRUE, text.size = .5) +
  tm_layout(frame = TRUE)

Accuracy <- tm_shape(moza) + 
  tm_fill("Accuracy", style="fixed", breaks=c(0, 50, 60, 70, 80, 90,100),
         labels=c("< 50", "50-60", "60-70", "70-80", "80-90", " > 90"),
          palette="Greens")  +
  tm_borders("grey") +
  tm_text("Accuracy", size = "AREA") +
  #tm_legend(outside = TRUE, text.size = .5) +
  tm_layout(frame = TRUE)

#Arrange all maps #
tmap_arrange(Sensitivity, Specificity,Accuracy)

###### Plot map for each polygon unit
tm_shape(moza) +
  tm_polygons("Accuracy", palette = "RdYlBu")+
  tm_facets(by = "Area")

############################# SAVE THE MAPS #################################
############################# SAVE THE MAPS #################################

## save an image ("plot" mode)
tmap_save(Sensitivity, filename = "output/png/Sensitivity_zscore-1.png")

## save as stand-alone HTML file ("view" mode)
tmap_save(Sensitivity, filename = "output/png/Sensitivity_zscore-1.html")

####################### ALTERNATIVE APPROACHES ############################
####################### ALTERNATIVE APPROACHES ############################

























