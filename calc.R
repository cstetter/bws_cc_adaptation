set.seed(4)


## Load libraries ----
library(tidyverse)
library(support.BWS)
library(mlogit)
library(gmnl)
library(units)
library(stargazer)
# 
# 
# data <- readRDS("data700.rds")
# #### Prepare data for analysis ####
# 
# ### Create suitable data frame
# ### Load BIBD
bibd <- readRDS("data/bibd_field_phase.rds")
# stargazer(bibd, type="text", out="bibd.htm")
# 
# ### function to transform data format
# formatDF <- function(subDF, numQes=1){
#   df <-data.frame(b=rep(NA, nrow(data)))
#   
#   best <- sapply(1:nrow(df), function(i) 
#     colnames(data[subDF])[which(data[i,subDF]==1)])
#   
#   worst <- sapply(1:nrow(df), function(i) 
#     colnames(data[subDF])[which(data[i,subDF]==2)])
#   
#   df <- df %>% 
#     dplyr::mutate(b =  dplyr::case_when( 
#       startsWith(best, "prec_farming") ~ 1, 
#       startsWith(best, "manag_rhythm") ~ 2,
#       startsWith(best, "plant_protect") ~ 3,
#       startsWith(best, "rotation") ~ 4,
#       startsWith(best, "con_tillage") ~ 5, 
#       startsWith(best, "fertilizer") ~ 6,
#       startsWith(best, "res_crops") ~ 7,
#       startsWith(best, "res_varieties") ~ 8,
#       startsWith(best, "catch_crops") ~ 9, 
#       startsWith(best, "mixed_crops") ~ 10,
#       startsWith(best, "irrigation") ~ 11,
#       startsWith(best, "insurance") ~ 12,
#       startsWith(best, "business_div") ~ 13),
#       
#       w = dplyr::case_when( 
#         startsWith(worst, "prec_farming") ~ 1, 
#         startsWith(worst, "manag_rhythm") ~ 2,
#         startsWith(worst, "plant_protect") ~ 3,
#         startsWith(worst, "rotation") ~ 4,
#         startsWith(worst, "con_tillage") ~ 5, 
#         startsWith(worst, "fertilizer") ~ 6,
#         startsWith(worst, "res_crops") ~ 7,
#         startsWith(worst, "res_varieties") ~ 8,
#         startsWith(worst, "catch_crops") ~ 9, 
#         startsWith(worst, "mixed_crops") ~ 10,
#         startsWith(worst, "irrigation") ~ 11,
#         startsWith(worst, "insurance") ~ 12,
#         startsWith(worst, "business_div") ~ 13))
#   
#   colnames(df) <- c(paste0("b", numQes), paste0("w", numQes))
#   return(df)
# }
# 
# bw1 <- formatDF(subDF=2:5,numQes = 1)
# bw2 <- formatDF(subDF=6:9,numQes = 2)
# bw3 <- formatDF(subDF=10:13,numQes = 3)
# bw4 <- formatDF(subDF=14:17,numQes = 4)
# bw5 <- formatDF(subDF=18:21,numQes = 5)
# bw6 <- formatDF(subDF=22:25,numQes = 6)
# bw7 <- formatDF(subDF=26:29,numQes = 7)
# bw8 <- formatDF(subDF=30:33,numQes = 8)
# bw9 <- formatDF(subDF=34:37,numQes = 9)
# bw10 <- formatDF(subDF=38:41,numQes = 10)
# bw11 <- formatDF(subDF=42:45,numQes = 11)
# bw12 <- formatDF(subDF=46:49,numQes = 12)
# bw13 <- formatDF(subDF=50:53,numQes = 13)
# 
# df_bw<-cbind(data$ID, bw1, bw2, bw3, bw4, bw5, bw6, 
#              bw7, bw8, bw9, bw10, bw11, bw12, bw13)
# 
# # df_bw<-rename(df_bw,c("data$ID"="ID"))
# colnames(df_bw)[1] <- "ID"
# #deleting bws from data frame, so that I can replace it with bw.
# data_no_bws <- data[,-(2:53)] 
# 
# # join data
# df_final<-left_join(df_bw, data_no_bws, by = "ID")
# 
# 
# east <-readRDS("data/plz_east_attribution.rds")
# df_final$location <- parse_number(df_final$location) %>% as.character()
# df_final$adapt_friends_d <- ifelse(df_final$adapt_friends==3, 1, -1)
# df_final$climate_impact_d <- ifelse(df_final$climate_impact==2, 1, -1)
# df_final$high_risk_d <- ifelse(df_final$risk_willingness>3, 1, -1)
# df_final$perception_others_imp <- ifelse(df_final$perception_others<3, 1, -1)
# df_final$organic <- ifelse(df_final$management_type==2, 1, -1)
# df_final$loc_east <- ifelse(
#   startsWith(df_final$location, "0") |
#     startsWith(df_final$location, "1") |
#     startsWith(df_final$location, "99") |
#     startsWith(df_final$location, "98") |
#     startsWith(df_final$location, "39") |
#     df_final$location %in% east, 1, -1)
# df_final$arable_d <- with(df_final, ifelse(business_type<3|business_type==7, 1,-1))
# 
# saveRDS(df_final, "df_final.rds")
### Combining the design and the responses

### Storing names of response variable


df_final <- readRDS("df_finaL_spatial.rds")

response <- colnames(df_final)[2:27]
response

#### Analysis ####
#Notes:
#-The variable RES corresponds to the dependent variable
#-Although the item variables correspond to the independent variables, 
#an arbitrary item variable must be omitted from the model because all the item 
#variables are dummy variables -> selection via the counting approach

### Dataset for the model
adapt<-c("prec_farming", "manag_rhythm", "plant_protect", "rotation",
        "con_tillage", "fertilizer", "res_crops", "res_varieties",
         "catch_crops", "mixed_crops", "irrigation", "insurance", "business_div")

df_model <- df_final %>% 
  dplyr::select(1:27,64, adapt_friends_d, climate_impact_d, high_risk_d, 
         perception_others_imp, organic,loc_east, arable_d, rain, temp,
         sqi) %>% na.omit()


df_model$mtemp <- with(df_model, temp-mean(temp))
df_model$mrain <- with(df_model, rain-mean(rain))/100
df_model$msqi <- with(df_model, sqi-mean(sqi))


saveRDS(df_model, "df_model.rds")


model_data <- bws.dataset(
  respondent.dataset = df_model, # data frame containing response dataset
  response.type = 2, # format of response variables: 1 = row number format 
  #2= item number format
  choice.sets = bibd, # choice sets
  design.type = 2, # 2 if a BIBD is assgined to choice.sets
  item.names = adapt, # the names of items
  id = "ID", # the name of respondent id variable
  response = response,# the names of response variables
  model = "maxdiff") # the type of dataset created by the function is the maxdiff model


model_data_count <- model_data
saveRDS(model_data_count, "model_outputs/model_data_count.rds")


model_data <- mlogit.data(data = model_data, choice = "RES", shape = "long",
                          alt.var = "PAIR", chid.var = "STR", id.var = "ID")



## MNL
res_bws_mnl <- gmnl(
  formula = RES ~ prec_farming + manag_rhythm + plant_protect + rotation +
    con_tillage + fertilizer + res_crops + res_varieties +
    catch_crops + mixed_crops + irrigation + business_div - 1,
  data = model_data)
saveRDS(res_bws_mnl, "model_outputs/res_bws_mnl.rds")


## MIXL cor
res_bws_mixl_cor <- gmnl(
  formula = RES ~ prec_farming + manag_rhythm + plant_protect + rotation +
    con_tillage + fertilizer + res_crops + res_varieties +
    catch_crops + mixed_crops + irrigation + business_div -1,
  data = model_data,
  model = 'mixl', 
  panel = TRUE,
  correlation = TRUE,
  method = "bhhh",
  ranp = c(prec_farming = "n", manag_rhythm = "n",
           plant_protect = "n", rotation = "n",  con_tillage = "n",
           fertilizer = "n", res_crops = "n", res_varieties = "n",
           catch_crops = "n", mixed_crops = "n",
           irrigation = "n",
           business_div = "n"),
  R = 2000, halton=NA)
saveRDS(res_bws_mixl_cor, "model_outputs/res_bws_mxl_cor.rds")


## MIXL
res_bws_mixl <- gmnl(
  formula = RES ~ prec_farming + manag_rhythm + plant_protect + rotation +
    con_tillage + fertilizer + res_crops + res_varieties +
    catch_crops + mixed_crops + irrigation + business_div -1,
  data = model_data,
  model = 'mixl', 
  panel = TRUE,
  method = "bhhh",
  ranp = c(prec_farming = "n", manag_rhythm = "n",
           plant_protect = "n", rotation = "n",  con_tillage = "n",
           fertilizer = "n", res_crops = "n", res_varieties = "n",
           catch_crops = "n", mixed_crops = "n",
           irrigation = "n",
           business_div = "n"),
  R = 2000, halton=NA)

saveRDS(res_bws_mixl, "model_outputs/res_bws_mxl.rds")


# Heterogenous model biophysical
het_bio <- c("mtemp", "mrain", "msqi")

# res_bws_mixl_bio_cor <- gmnl(
#   formula = RES ~ prec_farming + manag_rhythm + plant_protect + rotation +
#     con_tillage + fertilizer + res_crops + res_varieties +
#     catch_crops + mixed_crops + irrigation + business_div | 0 | 0 |
#     mtemp + mrain + msqi - 1,
#   data = model_data,
#   model = 'mixl', 
#   panel = TRUE,
#   correlation = TRUE,
#   method = "bhhh",
#   ranp = c(prec_farming = "n", manag_rhythm = "n",
#            plant_protect = "n", rotation = "n",  con_tillage = "n",
#            fertilizer = "n", res_crops = "n", res_varieties = "n",
#            catch_crops = "n", mixed_crops = "n",
#            irrigation = "n",
#            business_div = "n"),
#   mvar= list(prec_farming = het_bio, 
#              manag_rhythm = het_bio,  
#              plant_protect = het_bio, 
#              rotation = het_bio,      
#              con_tillage = het_bio,  
#              fertilizer = het_bio,    
#              res_crops = het_bio,  
#              res_varieties = het_bio, 
#              catch_crops = het_bio,  
#              mixed_crops = het_bio,   
#              irrigation = het_bio,      
#              business_div = het_bio),
#   R = 1000, 
#   halton=NA)

# saveRDS(res_bws_mixl_bio_cor, "model_outputs/res_bws_mixl_bio_cor1000.rds")




# het_bio <- c("mtemp", "mrain", "msqr")
# res_bws_mixl_bio_cor_sqr <- gmnl(
#   formula = RES ~ prec_farming + manag_rhythm + plant_protect + rotation +
#     con_tillage + fertilizer + res_crops + res_varieties +
#     catch_crops + mixed_crops + irrigation + business_div | 0 | 0 |
#     mtemp + mrain + msqr - 1,
#   data = model_data,
#   model = 'mixl', 
#   panel = TRUE,
#   correlation = TRUE,
#   method = "bhhh",
#   ranp = c(prec_farming = "n", manag_rhythm = "n",
#            plant_protect = "n", rotation = "n",  con_tillage = "n",
#            fertilizer = "n", res_crops = "n", res_varieties = "n",
#            catch_crops = "n", mixed_crops = "n",
#            irrigation = "n",
#            business_div = "n"),
#   mvar= list(prec_farming = het_bio, 
#              manag_rhythm = het_bio,  
#              plant_protect = het_bio, 
#              rotation = het_bio,      
#              con_tillage = het_bio,  
#              fertilizer = het_bio,    
#              res_crops = het_bio,  
#              res_varieties = het_bio, 
#              catch_crops = het_bio,  
#              mixed_crops = het_bio,   
#              irrigation = het_bio,      
#              business_div = het_bio),
#   R = 1000, 
#   halton=NA)
# 
# saveRDS(res_bws_mixl_bio_cor_sqr, "model_outputs/res_bws_mixl_bio_cor_sqr.rds")
# 
# het_bio <- c("mtemp", "mrain", "mnfkw")
# res_bws_mixl_bio_cor_nfkw <- gmnl(
#   formula = RES ~ prec_farming + manag_rhythm + plant_protect + rotation +
#     con_tillage + fertilizer + res_crops + res_varieties +
#     catch_crops + mixed_crops + irrigation + business_div | 0 | 0 |
#     mtemp + mrain + mnfkw - 1,
#   data = model_data,
#   model = 'mixl', 
#   panel = TRUE,
#   correlation = TRUE,
#   method = "bhhh",
#   ranp = c(prec_farming = "n", manag_rhythm = "n",
#            plant_protect = "n", rotation = "n",  con_tillage = "n",
#            fertilizer = "n", res_crops = "n", res_varieties = "n",
#            catch_crops = "n", mixed_crops = "n",
#            irrigation = "n",
#            business_div = "n"),
#   mvar= list(prec_farming = het_bio, 
#              manag_rhythm = het_bio,  
#              plant_protect = het_bio, 
#              rotation = het_bio,      
#              con_tillage = het_bio,  
#              fertilizer = het_bio,    
#              res_crops = het_bio,  
#              res_varieties = het_bio, 
#              catch_crops = het_bio,  
#              mixed_crops = het_bio,   
#              irrigation = het_bio,      
#              business_div = het_bio),
#   R = 1000, 
#   halton=NA)
# 
# saveRDS(res_bws_mixl_bio_cor_nfkw, "model_outputs/res_bws_mixl_bio_cor_nfkw.rds")


# het_bio <- c("mtemp", "mrain", "msqi")
res_bws_mixl_bio_cor <- gmnl(
  formula = RES ~ prec_farming + manag_rhythm + plant_protect + rotation +
    con_tillage + fertilizer + res_crops + res_varieties +
    catch_crops + mixed_crops + irrigation + business_div | 0 | 0 |
    mtemp + mrain + msqi - 1,
  data = model_data,
  model = 'mixl', 
  panel = TRUE,
  correlation = TRUE,
  method = "bhhh",
  ranp = c(prec_farming = "n", manag_rhythm = "n",
           plant_protect = "n", rotation = "n",  con_tillage = "n",
           fertilizer = "n", res_crops = "n", res_varieties = "n",
           catch_crops = "n", mixed_crops = "n",
           irrigation = "n",
           business_div = "n"),
  mvar= list(prec_farming = het_bio, 
             manag_rhythm = het_bio,  
             plant_protect = het_bio, 
             rotation = het_bio,      
             con_tillage = het_bio,  
             fertilizer = het_bio,    
             res_crops = het_bio,  
             res_varieties = het_bio, 
             catch_crops = het_bio,  
             mixed_crops = het_bio,   
             irrigation = het_bio,      
             business_div = het_bio),
  R = 2000, 
  halton=NA)

saveRDS(res_bws_mixl_bio_cor, "model_outputs/res_bws_mixl_bio_cor.rds")



res_bws_mixl_bio_uncor <- gmnl(
  formula = RES ~ prec_farming + manag_rhythm + plant_protect + rotation +
    con_tillage + fertilizer + res_crops + res_varieties +
    catch_crops + mixed_crops + irrigation + business_div | 0 | 0 |
    mtemp + mrain + msqi - 1,
  data = model_data,
  model = 'mixl', 
  panel = TRUE,
  correlation = F,
  method = "bhhh",
  ranp = c(prec_farming = "n", manag_rhythm = "n",
           plant_protect = "n", rotation = "n",  con_tillage = "n",
           fertilizer = "n", res_crops = "n", res_varieties = "n",
           catch_crops = "n", mixed_crops = "n",
           irrigation = "n",
           business_div = "n"),
  mvar= list(prec_farming = het_bio, 
             manag_rhythm = het_bio,  
             plant_protect = het_bio, 
             rotation = het_bio,      
             con_tillage = het_bio,  
             fertilizer = het_bio,    
             res_crops = het_bio,  
             res_varieties = het_bio, 
             catch_crops = het_bio,  
             mixed_crops = het_bio,   
             irrigation = het_bio,      
             business_div = het_bio),
  R = 2000, 
  halton=NA)

saveRDS(res_bws_mixl_bio_uncor, "model_outputs/res_bws_mixl_bio_uncor.rds")


#### MIXL weather tests ####
het_bio <- c("mtemp")

res_bws_mixl_bio_cor_temp <- gmnl(
  formula = RES ~ prec_farming + manag_rhythm + plant_protect + rotation +
    con_tillage + fertilizer + res_crops + res_varieties +
    catch_crops + mixed_crops + irrigation + business_div | 0 | 0 |
    mtemp + mrain + msqi - 1,
  data = model_data,
  model = 'mixl', 
  panel = TRUE,
  correlation = TRUE,
  method = "bhhh",
  ranp = c(prec_farming = "n", manag_rhythm = "n",
           plant_protect = "n", rotation = "n",  con_tillage = "n",
           fertilizer = "n", res_crops = "n", res_varieties = "n",
           catch_crops = "n", mixed_crops = "n",
           irrigation = "n",
           business_div = "n"),
  mvar= list(prec_farming = het_bio, 
             manag_rhythm = het_bio,  
             plant_protect = het_bio, 
             rotation = het_bio,      
             con_tillage = het_bio,  
             fertilizer = het_bio,    
             res_crops = het_bio,  
             res_varieties = het_bio, 
             catch_crops = het_bio,  
             mixed_crops = het_bio,   
             irrigation = het_bio,      
             business_div = het_bio),
  R = 2000, 
  halton=NA)

saveRDS(res_bws_mixl_bio_cor_temp, "model_outputs/res_bws_mixl_bio_cor_temp.rds")


## rain
het_bio <- c("mrain")

res_bws_mixl_bio_cor_rain <- gmnl(
  formula = RES ~ prec_farming + manag_rhythm + plant_protect + rotation +
    con_tillage + fertilizer + res_crops + res_varieties +
    catch_crops + mixed_crops + irrigation + business_div | 0 | 0 |
    mtemp + mrain + msqi - 1,
  data = model_data,
  model = 'mixl', 
  panel = TRUE,
  correlation = TRUE,
  method = "bhhh",
  ranp = c(prec_farming = "n", manag_rhythm = "n",
           plant_protect = "n", rotation = "n",  con_tillage = "n",
           fertilizer = "n", res_crops = "n", res_varieties = "n",
           catch_crops = "n", mixed_crops = "n",
           irrigation = "n",
           business_div = "n"),
  mvar= list(prec_farming = het_bio, 
             manag_rhythm = het_bio,  
             plant_protect = het_bio, 
             rotation = het_bio,      
             con_tillage = het_bio,  
             fertilizer = het_bio,    
             res_crops = het_bio,  
             res_varieties = het_bio, 
             catch_crops = het_bio,  
             mixed_crops = het_bio,   
             irrigation = het_bio,      
             business_div = het_bio),
  R = 2000, 
  halton=NA)

saveRDS(res_bws_mixl_bio_cor_rain, "model_outputs/res_bws_mixl_bio_cor_rain.rds")



## rain
het_bio <- c("msqi")

res_bws_mixl_bio_cor_sqi <- gmnl(
  formula = RES ~ prec_farming + manag_rhythm + plant_protect + rotation +
    con_tillage + fertilizer + res_crops + res_varieties +
    catch_crops + mixed_crops + irrigation + business_div | 0 | 0 |
    mtemp + mrain + msqi - 1,
  data = model_data,
  model = 'mixl', 
  panel = TRUE,
  correlation = TRUE,
  method = "bhhh",
  ranp = c(prec_farming = "n", manag_rhythm = "n",
           plant_protect = "n", rotation = "n",  con_tillage = "n",
           fertilizer = "n", res_crops = "n", res_varieties = "n",
           catch_crops = "n", mixed_crops = "n",
           irrigation = "n",
           business_div = "n"),
  mvar= list(prec_farming = het_bio, 
             manag_rhythm = het_bio,  
             plant_protect = het_bio, 
             rotation = het_bio,      
             con_tillage = het_bio,  
             fertilizer = het_bio,    
             res_crops = het_bio,  
             res_varieties = het_bio, 
             catch_crops = het_bio,  
             mixed_crops = het_bio,   
             irrigation = het_bio,      
             business_div = het_bio),
  R = 2000, 
  halton=NA)

saveRDS(res_bws_mixl_bio_cor_sqi, "model_outputs/res_bws_mixl_bio_cor_sqi.rds")



# MIXL + explainables

## Farm production structure
het <- c("loc_east", "organic", "arable_d")
res_bws_farm_structure <- gmnl(
  formula = RES ~ prec_farming + manag_rhythm + plant_protect + rotation +
    con_tillage + fertilizer + res_crops + res_varieties +
    catch_crops + mixed_crops + irrigation + business_div | 0 | 0 |
    loc_east + organic + arable_d - 1,
  data = model_data,
  correlation = T,
  model = 'mixl', 
  panel = TRUE,
  method = "bhhh",
  ranp = c(prec_farming = "n", manag_rhythm = "n",
           plant_protect = "n", rotation = "n",  con_tillage = "n",
           fertilizer = "n", res_crops = "n", res_varieties = "n",
           catch_crops = "n", mixed_crops = "n",
           irrigation = "n",
           business_div = "n"),
  mvar= list(prec_farming = het, 
             manag_rhythm = het,  
             plant_protect = het, 
             rotation = het,      
             con_tillage = het,  
             fertilizer = het,    
             res_crops = het,  
             res_varieties = het, 
             catch_crops = het,  
             mixed_crops = het,   
             irrigation = het,      
             business_div = het),
  R = 2000, halton=NA)
saveRDS(res_bws_farm_structure, "model_outputs/res_bws_farm_structure.rds")



## Behavioral factors
het <- c("adapt_friends_d", "climate_impact_d", "high_risk_d", 
         "perception_others_imp")
res_bws_behavior <- gmnl(
  formula = RES ~ prec_farming + manag_rhythm + plant_protect + rotation +
    con_tillage + fertilizer + res_crops + res_varieties +
    catch_crops + mixed_crops + irrigation + business_div | 0 | 0 |
    adapt_friends_d + climate_impact_d + 
    high_risk_d + perception_others_imp - 1,
  data = model_data,
  model = 'mixl', 
  panel = TRUE,
  correlation = T,
  method = "bhhh",
  ranp = c(prec_farming = "n", manag_rhythm = "n",
           plant_protect = "n", rotation = "n",  con_tillage = "n",
           fertilizer = "n", res_crops = "n", res_varieties = "n",
           catch_crops = "n", mixed_crops = "n",
           irrigation = "n",
           business_div = "n"),
  mvar= list(prec_farming = het, 
             manag_rhythm = het,  
             plant_protect = het, 
             rotation = het,      
             con_tillage = het,  
             fertilizer = het,    
             res_crops = het,  
             res_varieties = het, 
             catch_crops = het,  
             mixed_crops = het,   
             irrigation = het,      
             business_div = het),
  R = 2000, halton=NA)

saveRDS(res_bws_behavior, "model_outputs/res_bws_behavior.rds")




