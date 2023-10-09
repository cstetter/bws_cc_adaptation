library(tidyverse)
library(sf)

df_model <- readRDS("df_model.rds")
df_grid <- readRDS("grid_10km.rds")


res_bws_mixl_bio_cor <- readRDS("model_outputs/res_bws_mixl_bio_cor.rds")


df_grid$mrain <- df_grid$rain / mean(df_model$rain) / 100
df_grid$mtemp <- df_grid$temp / mean(df_model$temp)
df_grid$msqi <- df_grid$sqi / mean(df_model$sqi)
df_grid$mnfkw <- df_grid$nfkw / mean(df_model$nfkw)
df_grid$msqr <- df_grid$sqr / mean(df_model$sqr)


# No krinksky Robb
calc_marg_effect <- function(model=res_bws_mixl_bio_cor,
                             temp=0,
                             rain=0,
                             soil=0){
  
  coef_main <- coef(model)[1:12]
  coef_rain <- coef(model)[str_detect(names(coef(model)), "rain")]
  coef_temp <- coef(model)[str_detect(names(coef(model)), "temp")]
  # coef_soil <- coef(model)[str_detect(names(coef(model)), "nfkw")]
  coef_soil <- coef(model)[str_detect(names(coef(model)), "sqi")]
  # coef_soil <- coef(model)[str_detect(names(coef(model)), "sqr")]
  
  
  numerator <- c(exp( coef_main + 
                        coef_rain * rain + 
                        coef_temp * temp + 
                        coef_soil * soil),1)
  denominator <- sum(numerator)
  perc_imp<- (numerator/denominator)*100
  
  return(perc_imp)                     
}

# Project grid based preferences
a <- pbapply::pbsapply(1:nrow(df_grid), function(i)
  calc_marg_effect(model = res_bws_mixl_bio_cor2000, 
                   temp = df_grid$mtemp[i],
                   rain = df_grid$mrain[i], 
                   soil = df_grid$msqi[i]
                   # soil = df_grid$mnfkw[i]
                   # soil = df_grid$msqr[i]
  )
)

# Assign to spatial data frame
rownames(a)[13] <- "insurance"

cbind(df_grid, t(a)) %>% 
  pivot_longer(prec_farming:insurance, names_to = "measure", values_to = "sp_score") %>% 
  ggplot() +
  geom_sf(aes(fill=sp_score), col=NA) +
  facet_wrap(vars(measure), nrow = 3) +
  theme_void() +
  scale_fill_stepsn(n.breaks = 15, colours = terrain.colors(15))
