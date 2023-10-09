calc_marg_effect <- function(model=res_bws_mixl_bio_cor,
                             temp=0,
                             rain=0,
                             soil=0){
  Ncoef <- length(coef(model))
  
  ## Kinsky Robb
  R <- 10000
  a <- t(coef(model) + chol(vcov(model)) %*%
           matrix(rnorm(R*Ncoef, mean = 0, sd = 1), nrow = Ncoef, ncol = R))
  a <- as.data.frame(a)
  
  # numerator <- cbind(exp(a[1:12]), rep(1, R))
  numerator <- cbind(exp(a[1:12] +
                           select(a, contains("mtemp")) * temp +
                           select(a, contains("mrain")) * rain +
                           select(a, contains("msqi")) * soil),
                     rep(1, R))
  denominator <- apply(numerator, 1, sum)
  perc_imp<- (numerator/denominator)*100
  
  return(perc_imp)                     
}

# colMeans(calc_marg_effect(res_bws_mixl_bio_cor2000, temp=-2))
# colMedians(calc_marg_effect(res_bws_mixl_bio_cor2000, temp=-2))
# 
# lapply(quantile(res_bws_mixl_bio_cor$mf$mrain, seq(0,1,.1)), function(j)
#   sapply(quantile(res_bws_mixl_bio_cor$mf$mtemp, seq(0,1,.1)), function(i) 
#   rank(colMeans(calc_marg_effect(res_bws_mixl_bio_cor, temp=i, rain=j)))))




## Temperature share of preference effect
bounds95_temp <- lapply(quantile(res_bws_mixl_bio_cor$mf$mtemp, seq(0,1,.1)), function(i)
  t(apply(calc_marg_effect(res_bws_mixl_bio_cor2000, temp=i), 2, quantile, c(0.025, .975))))

mean_temp <- lapply(quantile(res_bws_mixl_bio_cor$mf$mtemp, seq(0,1,.1)), function(i)
  colMeans(calc_marg_effect(res_bws_mixl_bio_cor2000, temp=i)))

# Combine the results 
sp_temp_list <- lapply(1:11, function(i){
  cbind(bounds95_temp[[i]], mean_temp[[i]])
})

# Make dataframe
df_sp_temp <- as.data.frame(do.call(rbind, sp_temp_list)) 
df_sp_temp$adapt_measure <- rep(cc_adapt_names, 11)
df_sp_temp$temperature <- rep(quantile(res_bws_mixl_bio_cor$mf$mtemp, seq(0,1,.1)), each=13)

colnames(df_sp_temp)[1:3] <- c("lb", "ub", "pt")

df_sp_temp %>% 
  ggplot(aes(x=temperature)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.4) +
  geom_line(aes(y=pt)) +
  facet_wrap(vars(adapt_measure), scales = "free") +
  theme_bw(base_size = 16)
  


## Rain share of preference effect
bounds95_rain <- lapply(quantile(res_bws_mixl_bio_cor_adj$mf$mrain100, seq(0,1,.1)), function(i)
  t(apply(calc_marg_effect(res_bws_mixl_bio_cor_adj, rain=i), 2, quantile, c(0.025, .975))))

mean_rain <- lapply(quantile(res_bws_mixl_bio_cor_adj$mf$mrain100, seq(0,1,.1)), function(i)
  colMeans(calc_marg_effect(res_bws_mixl_bio_cor_adj, rain=i)))

# Combine the results 
sp_rain_list <- lapply(1:11, function(i){
  cbind(bounds95_rain[[i]], mean_rain[[i]])
})

# Make dataframe
df_sp_rain <- as.data.frame(do.call(rbind, sp_rain_list)) 
df_sp_rain$adapt_measure <- rep(names(mean_rain[[1]]), 11)
df_sp_rain$rain <- rep(quantile(res_bws_mixl_bio_cor_adj$mf$mrain100, seq(0,1,.1)), each=13)

colnames(df_sp_rain)[1:3] <- c("lb", "ub", "pt")

df_sp_rain %>% 
  ggplot(aes(x=rain)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.4) +
  geom_line(aes(y=pt)) +
  facet_wrap(vars(adapt_measure), scales = "free") +
  theme_bw(base_size = 16)


## Soil share of preference effect
bounds95_sqi <- lapply(quantile(res_bws_mixl_bio_cor_adj$mf$msqi, seq(0,1,.1)), function(i)
  t(apply(calc_marg_effect(res_bws_mixl_bio_cor_adj, soil=i), 2, quantile, c(0.025, .975))))

mean_sqi <- lapply(quantile(res_bws_mixl_bio_cor_adj$mf$msqi, seq(0,1,.1)), function(i)
  colMeans(calc_marg_effect(res_bws_mixl_bio_cor_adj, soil=i)))

# Combine the results 
sp_sqi_list <- lapply(1:11, function(i){
  cbind(bounds95_sqi[[i]], mean_sqi[[i]])
})

# Make dataframe
df_sp_sqi <- as.data.frame(do.call(rbind, sp_sqi_list)) 
df_sp_sqi$adapt_measure <- rep(names(mean_sqi[[1]]), 11)
df_sp_sqi$sqi <- rep(quantile(res_bws_mixl_bio_cor_adj$mf$msqi, seq(0,1,.1)), each=13)

colnames(df_sp_sqi)[1:3] <- c("lb", "ub", "pt")

df_sp_sqi %>% 
  ggplot(aes(x=sqi)) +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.4) +
  geom_line(aes(y=pt)) +
  facet_wrap(vars(adapt_measure)) +
  theme_bw(base_size = 16)



## Interaction plots




a <- lapply(quantile(res_bws_mixl_bio_cor_adj$mf$mrain, seq(0,1,.05)), function(j)
  t(
    sapply(
      quantile(res_bws_mixl_bio_cor_adj$mf$msqi, seq(0,1,.05)), function(i)
        # rank(calc_marg_effect_simple(res_bws_mixl_bio_cor_adj, soil=i, rain=j))
        calc_marg_effect_simple(res_bws_mixl_bio_cor_adj, soil=i, rain=j)
    )
  )
)


b <- as.data.frame(do.call(rbind, a))

b$value_sqi <- rep(
  seq(0,10,.5), 21
  )

b$value_rain <- rep(
  seq(0,10,.5), each = 21
)

colnames(b)[13] <- "insurance"

df_gg <- res_bws_mixl_bio_cor2000$mf

b %>% 
  pivot_longer(1:13) %>%
  mutate(value=as.factor(value)) %>%
  ggplot() +
  geom_raster(aes(x=value_rain, y=value_sqi, fill=value), col="black") +
  # geom_rug(data = df_gg, 
  #          aes(x = scales::rescale(mrain,to = c(0,10)), 
  #              y = scales::rescale(msqi,to = c(0,10)))) +
  facet_wrap(vars(name), nrow = 2) +
  theme_bw() +
  # scale_fill_viridis_b(direction = -1, n.breaks=25 )
  scale_fill_viridis_d(direction = -1)
