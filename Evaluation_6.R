install.packages("compositions")
install.packages("caret")
install.packages("randomForest")
install.packages("sf")
install.packages("ggmap")
install.packages("kknn")

install.packages("iterators")  
install.packages("caret") 
install.packages("caretEnsemble")

library(sf)
library(ggmap)
library(caret)
library(kknn)
library(tidyverse)
library(tidyr)
library(readr)
library(ggplot2)
library(plyr)
library(dplyr)
library(lattice)
library(compositions)
library(agridat)
library(expsmooth)
library(forecast)
library(tinytex)
library(kernlab)


setwd("C:/Users/Utilisateur/Dropbox/Analyse") 
banane <- read_csv("CodeR/_Evaluation6/banane.csv")


bananee <- banane %>% dplyr::select(-station_name)
id_tr <- createDataPartition(bananee$yield, p = 0.7, list = FALSE) 
banane_tr <- bananee[id_tr, ]
banane_te <- bananee[-id_tr, ]
ml_mod <- caret::train(yield ~ lon + lat + degree_days + cumul_precip + sdi,
                data = banane_tr,
                method = "kknn") 

kknn_grid <- expand.grid(kmax = 3:6, 
          distance = 1:2, 
          kernel = c("rectangular", "gaussian", "optimal"))

ctrl <- trainControl(method="repeatedcv", repeats = 5)

#set.seed()
clf  <- train(yield ~ .,
              data = banane_tr,
              method = "kknn",
              tuneGrid = kknn_grid,
              trainControl=ctrl)
clf

pred_tr <- predict(clf)
pred_te <- predict(clf, newdata = banane_te)

tablepred <- table(banane_tr$yield, pred_tr)


yield_sum <- bananee %>%
  sum(bananee$yield) 
 yield_total <- yield_sum*6000


banane_mult <- (data = bananee +
  bananee$degree_days*1.15 +
  bananee$cumul_precip*1.08 +
  bananee$sdi*0.88)


ml_yield <- caret::train(yield ~ lon + lat + degree_days + cumul_precip + sdi,
                       data = banane_mult,
                       method = "kknn")


yield_predict_sum <- (data = banane_mult +
                       sum(banane_mult$yield))
yield_predict_total <- yield_predict_sum*6000



banane_geo <- bananee %>%
st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
st_transform("+proj=longlat +datumNAD83")


banane_geo %>% st_coordinates() %>% summary()

banane_map <- get_stamenmap(bbox=c(left = -80.50, right = -61.70, bottom = 42, top = 51), 
              zoom=7, maptype="terrain")

banane_coord <- banane_geo %>%
  st_coordinates()%>%
  as_tibble() %>%
  mutate(cumul_precip = banane_geo$cumul_precip,
         degree_days = banane_geo$degree_days)

ggmap(banane_map) +
  geom_point(data = banane_coord, aes(x=X, y=Y, colour = cumul_precip)) +
  scale_colour_viridis_c(option = "inferno", direction = -1) +
  theme_bw()



library("caret")
banane_dd <- bananee %>% 
  dplyr::select(lon, lat, cumul_precip) %>% 
  drop_na()
banane_dd_sc <- banane_dd %>% 
  mutate(cumul_precip = (cumul_precip - mean(cumul_precip))/sd(cumul_precip))
train_id_cumul <- createDataPartition(y = banane_dd_sc$cumul_precip, p = 0.7, list = FALSE)

library("kernlab")

dd_gp <- gausspr(x = banane_dd_sc[train_id_cumul, c("lon", "lat")],
                 y = banane_dd_sc[train_id_cumul, "cumul_precip"],
                 kernel = "rbfdot",
                 #kpar = list(sigma = 01), # laisser optimiser
                 variance.model = TRUE,
                 scale = TRUE,
                 var = 0.1,
                 cross = 5)


pred_dd_tr <- predict(dd_gp)
pred_dd_te <- predict(dd_gp, newdata =  banane_dd_sc[-train_id_cumul, c("lon", "lat")])

par(mfrow = c(1, 2))
plot(banane_dd_sc$cumul_precip[train_id_cumul], pred_dd_tr, main = "Train prediction", xlab = "mesuré", ylab = "prédit")
abline(0, 1, col="red")
plot(banane_dd_sc$cumul_precip[-train_id_cumul], pred_dd_te, main = "Test prediction", xlab = "mesuré", ylab = "prédit")
abline(0, 1, col="red")

grid <- expand.grid(lon = seq(from = -80.50, to = -61.70, by = 0.25),
                    lat = seq(from = 42, to = 51, by = 0.25))

grid <- grid %>% 
  mutate(pred_dd_mean = predict(dd_gp, newdata = ., type = "response") * sd(banane_dd$cumul_precip) + mean(banane_dd$cumul_precip),
         pred_dd_sd = predict(dd_gp, newdata = ., type = "sdeviation") * sd(banane_dd$cumul_precip))
head(grid)



grid_geo <- banane_dd_sc %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform("+proj=longlat +datumNAD83")

grid_geo %>% st_coordinates() %>% summary()

grid_map <- get_stamenmap(bbox=c(left = -80.50, right = -61.70, bottom = 42, top = 51), 
                          zoom=6, maptype="terrain")

grid_coord <- grid_geo %>%
  st_coordinates()%>%
  as_tibble() %>%
  mutate(cumul_precip = banane_dd_sc$cumul_precip)

grid_nogeo <- grid_geo %>%
  st_set_geometry(NULL)
grid_pred <- bind_cols(grid_nogeo, grid_coord)

ggmap(grid_map) +
  geom_point(data= grid_pred , aes(x=X, y=Y, colour = cumul_precip)) +
  geom_point(data = grid_coord, aes(x=X, y=Y, size = cumul_precip), shape = 21) +
  scale_colour_viridis_c(option = "inferno", direction = -1) +
  theme_bw()
