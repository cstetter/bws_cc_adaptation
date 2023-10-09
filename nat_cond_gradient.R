# Temperature

#### Make breaks along gradient ####
breaks <- scales::pretty_breaks(10)(res_bws_mixl_bio_cor$mf$mtemp)
breaks_temp <- breaks + round(mean(df_model$temp), 1)

#### Calculate preference shares & KI CIs ####
  mean_temp <- lapply(breaks, function(i)
    colMeans(calc_marg_effect(res_bws_mixl_bio_cor, temp=i))
  )
  
  bounds95_temp <- lapply(breaks, function(i)
    round(
      t(
        apply(calc_marg_effect(res_bws_mixl_bio_cor, temp=i), 
              2, 
              quantile, c(0.025, .975))),
      2)
  )

#### Make dataframe w/ results ####
sp_temp_list <- lapply(1:length(breaks), function(i){
  cbind(bounds95_temp[[i]], mean_temp[[i]])
})
df_sp_temp <- as.data.frame(do.call(rbind, sp_temp_list))



# Rainfall
#### Make breaks along gradient ####
breaks <- scales::pretty_breaks(10)(res_bws_mixl_bio_cor$mf$mrain)
breaks_rain <- breaks + round(mean(df_model$rain/100), 1)

#### Calculate preference shares & KI CIs ####
mean_rain <- lapply(breaks, function(i)
  colMeans(calc_marg_effect(res_bws_mixl_bio_cor, rain=i))
)

bounds95_rain <- lapply(breaks, function(i)
  round(
    t(
      apply(calc_marg_effect(res_bws_mixl_bio_cor, rain=i), 
            2, 
            quantile, c(0.025, .975))),
    2)
)

#### Make dataframe w/ results ####
sp_rain_list <- lapply(1:length(breaks), function(i){
  cbind(bounds95_rain[[i]], mean_rain[[i]])
})

df_sp_rain <- as.data.frame(do.call(rbind, sp_rain_list))


# Soil quality
#### Make breaks along gradient ####
breaks <- scales::pretty_breaks(10)(res_bws_mixl_bio_cor$mf$msqi)
breaks_soil <- breaks + round(mean(df_model$sqi), 1)

#### Calculate preference shares & KI CIs ####
mean_soil <- lapply(breaks, function(i)
  colMeans(calc_marg_effect(res_bws_mixl_bio_cor, soil=i))
)

bounds95_soil <- lapply(breaks, function(i)
  round(
    t(
      apply(calc_marg_effect(res_bws_mixl_bio_cor, soil=i), 
            2, 
            quantile, c(0.025, .975))),
    2)
)

#### Make dataframe w/ results ####
sp_soil_list <- lapply(1:length(breaks), function(i){
  cbind(bounds95_soil[[i]], mean_soil[[i]])
})

df_sp_soil <- as.data.frame(do.call(rbind, sp_soil_list))
























process_df_sp <- function(df = df_sp_temp, breaks=breaks_temp,
                          xlabel= "Temperature (°C)") {
  
  #### Order and rename adaptation measures ####
colnames(df)[1:3] <- c("lb", "ub", "pt")
df$adapt_measure <- rep(rownames(df[1:13,]), length(breaks))


df$xVar <- rep(breaks, each=13)
df$pt2 <- round(df$pt, 2)
df$label <- with(df, paste0(pt2, "\n[", lb, ",", ub, "]"))


df <- df %>% 
  group_by(xVar) %>% 
  mutate(rank = rank(desc(pt)))

order_adapt <- df[1:13,] %>% 
  arrange(pt) %>% 
  mutate(real_names =
           case_when(
             adapt_measure == "rep.1..R."     ~ "Insurance",
             adapt_measure == "prec_farming"  ~ "Precision farming",
             adapt_measure == "manag_rhythm"  ~ "Adjusted crop\nmanagement rhythm",
             adapt_measure == "plant_protect" ~ "Adjusted plant\nprotection management",
             adapt_measure == "rotation"      ~ "Crop rotation\ndiversification",
             adapt_measure == "con_tillage"   ~ "Conservation tillage",
             adapt_measure == "fertilizer"    ~ "Adjusted ferti-\nlization management",
             adapt_measure == "res_crops"     ~ "Cultivation of\nresilient crops",
             adapt_measure == "res_varieties" ~ "Cultivation of\nresilient crop varieties",
             adapt_measure == "catch_crops"   ~ "Cultivation of\ncover crops",
             adapt_measure == "mixed_crops"   ~ "Mixed cropping",
             adapt_measure == "irrigation"    ~ "Irrigation",
             adapt_measure == "business_div"  ~ "Business\ndiversification"))

df$adapt_measure<- 
  factor(df$adapt_measure, 
         levels = order_adapt$adapt_measure,
         labels =  order_adapt$real_names)


theme_set(theme_minimal(base_size = 25))


#### Make plot table ####
p <- ggplot2::ggplot(
  data = df,
  mapping = aes(x = xVar, y = adapt_measure)
)  +
  ggplot2::geom_tile(
    aes(fill = pt),
    col="white", size=1.2, alpha=1
  ) +
  ggplot2::geom_text(
    aes(x = xVar, y = adapt_measure,
        label = label), 
    size=5
  ) +
  scale_fill_viridis_b(n.breaks=20, direction=-1, begin = 0.1, 
                       name= "Preference shares in %") +
  scale_x_continuous(breaks=breaks) +
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        plot.background = element_rect(color = NA),
        plot.margin = margin(t = 2, l = 4, unit = 'mm'),
        # plot.title = element_text(face = 'bold'),
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(face = 'bold'),
        legend.key.width=unit(5,"cm"),
        legend.text=element_text(size=14)
  ) +
  coord_cartesian( ylim = c( 0.5, 13.5),
                   expand = F) +
  scale_y_discrete(position="right") +
  ggtitle("Preference Shares") +
  xlab(xlabel)



#### Ranking ####
col_sp <- Polychrome::alphabet.colors(13)
names(col_sp) <- order_adapt$real_names

gg1 <- df %>% 
  ggplot(mapping=aes(x=xVar, y=rank, col = adapt_measure)) + 
  geom_point(shape = '|', stroke =7) +
  ggbump::geom_bump( lwd = 2) +
  scale_x_continuous(breaks=breaks) +
  coord_cartesian(xlim = c(min(breaks), max(breaks)), ylim = c(13.5, 0.5),
                  expand = F) +
  theme(
    legend.position = 'none',
    plot.background = element_rect(color = NA),
    plot.margin = margin(t = 2, l = 0, r=2, unit = 'mm'),
    plot.title = element_text(
      hjust = 0.5
      # face = 'bold'
    ),
    axis.title.y = element_blank(),
    axis.text.y = element_text(face = 'bold'),
    panel.grid = element_blank()
  ) +
  ggtitle(label = "Preference Ranking") +
  scale_y_continuous(position="right", breaks=1:13)+
  scale_color_discrete(type = col_sp) +
  xlab(xlabel)

ggTemp <- p +gg1 + patchwork::plot_layout(widths = c(2,1))

return(ggTemp)

}
