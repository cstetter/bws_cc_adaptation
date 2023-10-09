```{r}
perc_bws <- function(context="organic"){
  Ncoef <- length(coef(res_bws_farm_structure))
  ## Kinsky Robb
  R <- 100000
  a <- t(coef(res_bws_farm_structure) + chol(vcov(res_bws_farm_structure)) %*%
           matrix(rnorm(R*Ncoef, mean = 0, sd = 1), nrow = Ncoef, ncol = R))
  a <- as.data.frame(a)
  ## Results
  if (context=="organic") {
    numerator <- cbind(exp(a[1:12]+select(a, contains("organic"))), rep(1, R))
  }
  else if(context=="conventional"){
    numerator <- cbind(exp(a[1:12]-select(a, contains("organic"))), rep(1, R))
  }
  else if(context=="east"){
    numerator <- cbind(exp(a[1:12]+select(a, contains("loc"))), rep(1, R))
  }
  else if(context=="west"){
    numerator <- cbind(exp(a[1:12]-select(a, contains("loc"))), rep(1, R))
  }
  else if(context=="arable"){
    numerator <- cbind(exp(a[1:12]+select(a, contains("arable"))), rep(1, R))
  }
  else if(context=="livestock"){
    numerator <- cbind(exp(a[1:12]-select(a, contains("arable"))), rep(1, R))
  }
  else if(context=="all"){
    numerator <- cbind(exp(a[1:12]), rep(1, R))
  }
  # 2-way-domain
  ## loc arable
  else if(context=="east_livestock"){
    numerator <- cbind(exp(a[1:12] +
                             select(a, contains("loc")) -
                             select(a, contains("arable"))), rep(1, R))
  }
  else if(context=="west_livestock"){
    numerator <- cbind(exp(a[1:12] -
                             select(a, contains("loc")) -
                             select(a, contains("arable"))), rep(1, R))
  }
  else if(context=="east_crop"){
    numerator <- cbind(exp(a[1:12] +
                             select(a, contains("loc")) +
                             select(a, contains("arable"))), rep(1, R))
  }
  else if(context=="west_crop"){
    numerator <- cbind(exp(a[1:12] -
                             select(a, contains("loc")) +
                             select(a, contains("arable"))), rep(1, R))
  }
  ## arable organic
  else if(context=="livestock_organic"){
    numerator <- cbind(exp(a[1:12] -
                             select(a, contains("arable")) +
                             select(a, contains("organic"))), rep(1, R))
  }
  else if(context=="crop_organic"){
    numerator <- cbind(exp(a[1:12] +
                             select(a, contains("arable")) +
                             select(a, contains("organic"))), rep(1, R))
  }
  else if(context=="livestock_conventional"){
    numerator <- cbind(exp(a[1:12] -
                             select(a, contains("arable")) -
                             select(a, contains("organic"))), rep(1, R))
  }
  else if(context=="crop_conventional"){
    numerator <- cbind(exp(a[1:12] +
                             select(a, contains("arable")) -
                             select(a, contains("organic"))), rep(1, R))
  }
  ## loc organic
  else if(context=="east_organic"){
    numerator <- cbind(exp(a[1:12] +
                             select(a, contains("loc")) +
                             select(a, contains("organic"))), rep(1, R))
  }
  else if(context=="west_organic"){
    numerator <- cbind(exp(a[1:12] -
                             select(a, contains("loc")) +
                             select(a, contains("organic"))), rep(1, R))
  }
  else if(context=="east_conventional"){
    numerator <- cbind(exp(a[1:12] +
                             select(a, contains("loc")) -
                             select(a, contains("organic"))), rep(1, R))
  }
  else if(context=="west_conventional"){
    numerator <- cbind(exp(a[1:12] -
                             select(a, contains("loc")) -
                             select(a, contains("organic"))), rep(1, R))
  }
  # 3-way-domain
  else if(context=="east_livestock_organic"){
    numerator <- cbind(exp(a[1:12] +
                             select(a, contains("loc")) -
                             select(a, contains("arable"))+
                             select(a, contains("organic"))), rep(1, R))
  }
  else if(context=="west_livestock_organic"){
    numerator <- cbind(exp(a[1:12] -
                             select(a, contains("loc")) -
                             select(a, contains("arable"))+
                             select(a, contains("organic"))), rep(1, R))
  }
  else if(context=="east_crop_organic"){
    numerator <- cbind(exp(a[1:12] +
                             select(a, contains("loc")) +
                             select(a, contains("arable"))+
                             select(a, contains("organic"))), rep(1, R))
  }
  else if(context=="west_crop_organic"){
    numerator <- cbind(exp(a[1:12] -
                             select(a, contains("loc")) +
                             select(a, contains("arable"))+
                             select(a, contains("organic"))), rep(1, R))
  }
  else if(context=="east_livestock_conventional"){
    numerator <- cbind(exp(a[1:12] +
                             select(a, contains("loc")) -
                             select(a, contains("arable"))-
                             select(a, contains("organic"))), rep(1, R))
  }
  else if(context=="west_livestock_conventional"){
    numerator <- cbind(exp(a[1:12] -
                             select(a, contains("loc")) -
                             select(a, contains("arable"))-
                             select(a, contains("organic"))), rep(1, R))
  }
  else if(context=="east_crop_conventional"){
    numerator <- cbind(exp(a[1:12] +
                             select(a, contains("loc")) +
                             select(a, contains("arable"))-
                             select(a, contains("organic"))), rep(1, R))
  }
  else if(context=="west_crop_conventional"){
    numerator <- cbind(exp(a[1:12] -
                             select(a, contains("loc")) +
                             select(a, contains("arable"))-
                             select(a, contains("organic"))), rep(1, R))
  }
  denominator <- apply(numerator, 1, sum)
  perc_imp_org <- (numerator/denominator)*100
  # perc_imp_org$group <-context
  return(perc_imp_org)
}



prep_sp_gg <- function(context="east_livestock_organic"){
  a <- data.frame(
    cbind(
      colMeans(perc_bws(context)),
      sapply(perc_bws(context), quantile, probs=0.025),
      sapply(perc_bws(context), quantile, probs=0.975),
      sapply(perc_bws(context), quantile, probs=0.05),
      sapply(perc_bws(context), quantile, probs=0.95))) %>% 
    rename(c("point"=1, "lb"=2, "ub"=3, "lb90"=4, "ub90"=5)) %>%
    rownames_to_column("var_name") %>%
    mutate(model=context,
           rank=as.character(rank(-point))) %>%
    arrange(desc(point))
  return(a)
}

```










# SP plots
```{r}
dp <- lapply(list(c("all"),
                  c("organic", "conventional"),
                  c("east", "west"),
                  c("arable", "livestock")), function(x)
                    do.call(rbind,
                            lapply(x, function(i)
                              prep_sp_gg(context=i)
                            ))
)

dp[[1]]$facet <- "Average"
dp[[2]]$facet <- "Production system"
dp[[3]]$facet <- "Location"
dp[[4]]$facet <- "Management focus"

order_y <- as.vector(
  with(
    dp[[1]],
    rev(
      plyr::revalue(factor(var_name), cc_adapt_names)
    )
  )
)

gg_sp_hetero <- lapply(dp, function(i)
  i %>% 
    mutate(model=str_replace(model, "livestock", "Non-Specialized Crop")) %>%
    mutate(model=str_replace(model, "arable", "Specialized Crop")) %>%
    mutate(model=str_to_title(model)) %>% 
    # mutate(var_name=str_wrap(plyr::revalue(factor(var_name, ordered = T), cc_adapt_names), 18)) %>%
    mutate(var_name=plyr::revalue(factor(var_name), cc_adapt_names)) %>%
    ggplot(aes(y=fct_relevel(var_name, order_y), 
               group = model, 
               col=model))  +
    geom_linerange(aes(xmin = lb, xmax = ub), 
                   size=.6, 
                   position=position_dodge2(padding = 0.1,width = 0.8, reverse = T)) +
    geom_linerange(aes(xmin = lb90, xmax = ub90), 
                   size=1, 
                   position=position_dodge2(padding = 0.1,width = 0.8, reverse = T)) +
    geom_point(aes(x=point), 
               size=1.5, 
               position=position_dodge2(padding = 0.1,width = 0.8, reverse = T)) +
    scale_shape_manual(values=1:8) +
    scale_y_discrete(labels= str_wrap(order_y, 25)) +
    ylab(NULL) +
    xlim(0,45) +
    xlab(" ") +
    theme_bw(base_size = 22) +
    guides(col=guide_legend("Model", byrow = T),
           shape=guide_legend("Model", byrow = T)) +
    theme(#legend.spacing.y = unit(1.5, 'cm'),
      legend.position = "bottom",
      legend.direction="vertical",
      legend.justification = c(0.5, 1),
      legend.title = element_blank(),
      legend.box.margin=margin(-20,0,0,0)) +
    facet_grid( ~ facet)
)


gg_patch_hetero <- 
  (
    gg_sp_hetero[[1]]  +
      scale_color_manual(values=unname(graf_palettes$safe[1])) + 
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        plot.tag.position  = c(.45, .9)
      ) |
      
      gg_sp_hetero[[2]] +
      scale_color_manual(values=unname(graf_palettes$safe[2:3])) + 
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        plot.tag.position  = c(.08, .9)
      )
  ) / 
  (
    gg_sp_hetero[[3]] +
      scale_color_manual(values=unname(graf_palettes$safe[4:5])) + 
      theme(
        # axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),
        # axis.line.y = element_blank(),
        # axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.tag.position  = c(.45, .9)
      ) |
      gg_sp_hetero[[4]] +
      scale_color_manual(values=unname(graf_palettes$safe[6:7])) + 
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.tag.position  = c(.08, .9))
  ) /
  plot_spacer() +
  plot_layout(
    heights = c(20, 20, 1),
    guides = "collect"
  ) &
  theme(legend.position = "bottom",
        legend.justification='right',
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm")) & 
  plot_annotation(tag_levels = "a",tag_suffix = ")")

gg_SP_hetero <- ggdraw(add_sub(gg_patch_hetero, "Importance assigned by farmers (%)", 
                               vpadding=grid::unit(0, "lines"),
                               y = 9, x = 0.45, hjust = 0, size = 18))

ggsave(filename = paste0("img/gg_SP_hetero1.png"), 
       plot = gg_SP_hetero, dpi="print",
       width = 13, height = 18)
```

# SP plot conference presentation
```{r}
gg_sp_avg <- dp_farm_structure %>% 
  mutate(var_name=plyr::revalue(factor(var_name), cc_adapt_names),
         var_name=str_to_title(var_name)) %>% 
  ggplot(aes(y=fct_reorder(var_name, point)))  +
  geom_linerange(aes(xmin = lb, xmax = ub), size=1, position=position_dodge2(padding = 3,width = .7, reverse = T)) +
  geom_point(aes(x=point, shape=model), size=2, position=position_dodge2(padding = 3,width = .7, reverse = T)) +
  ylab(NULL) +
  xlab("Importance assigned by farmers (%)") +
  theme_bw(base_size = 22) +
  theme(legend.position = "none")


ggsave(filename = paste0("../02_presentation/img/gg_SP_avg.png"), 
       plot = gg_sp_avg, dpi="print",
       width = 11, height = 8)



gg_sp_hetero_conf <- lapply(dp[2:4], function(i)
  i %>% 
    mutate(model=str_replace(model, "livestock", "Non-Specialized Crop")) %>%
    mutate(model=str_replace(model, "arable", "Specialized Crop")) %>%
    mutate(model=str_to_title(model)) %>% 
    # mutate(var_name=str_wrap(plyr::revalue(factor(var_name, ordered = T), cc_adapt_names), 18)) %>%
    mutate(var_name=plyr::revalue(factor(var_name), cc_adapt_names)) %>%
    ggplot(aes(y=fct_relevel(var_name, order_y), 
               group = model, 
               col=model))  +
    geom_linerange(aes(xmin = lb, xmax = ub), 
                   size=.6, 
                   position=position_dodge2(padding = 0.1,width = 0.8, reverse = T)) +
    geom_linerange(aes(xmin = lb90, xmax = ub90), 
                   size=1, 
                   position=position_dodge2(padding = 0.1,width = 0.8, reverse = T)) +
    geom_point(aes(x=point), 
               size=1.5, 
               position=position_dodge2(padding = 0.1,width = 0.8, reverse = T)) +
    scale_shape_manual(values=1:8) +
    scale_y_discrete(labels= str_wrap(order_y, 25)) +
    ylab(NULL) +
    xlim(0,45) +
    xlab(" ") +
    theme_bw(base_size = 22) +
    guides(col=guide_legend("Model", byrow = T),
           shape=guide_legend("Model", byrow = T)) +
    theme(#legend.spacing.y = unit(1.5, 'cm'),
      legend.position = "bottom",
      legend.direction="vertical",
      legend.justification = c(0.5, 1),
      legend.title = element_blank(),
      legend.box.margin=margin(-20,0,0,0)) +
    facet_grid( ~ facet)
)


gg_patch_hetero_conf <- 
  (
    gg_sp_hetero_conf[[1]] +
      scale_color_manual(values=unname(graf_palettes$safe[1:2])) + 
      theme(
        # axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),
        # axis.line.y = element_blank(),
        # axis.title.y = element_blank(),
        axis.title.x = element_blank()
      )
    | 
      gg_sp_hetero_conf[[2]] +
      scale_color_manual(values=unname(graf_palettes$safe[4:5])) + 
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()
      )    |
      gg_sp_hetero_conf[[3]] +
      scale_color_manual(values=unname(graf_palettes$safe[6:7])) + 
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()
      )
  ) /
  plot_spacer() +
  plot_layout(
    heights = c(20, 1),
    guides = "collect"
  ) &
  theme(legend.position = "bottom",
        legend.justification='right',
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"))

gg_SP_hetero_conf <- ggdraw(add_sub(gg_patch_hetero_conf, "Importance assigned by farmers (%)", 
                                    vpadding=grid::unit(0, "lines"),
                                    y = 9, x = 0.45, hjust = 0, size = 18))

ggsave(filename = paste0("../02_presentation/img/gg_SP_hetero.png"), 
       plot = gg_SP_hetero_conf, dpi="print",
       width = 15, height = 10)
```


# SP Multi-way interaction plots
```{r}
live_org <- apply( expand.grid(c("livestock", "crop"), c("organic", "conventional")) , 1 , paste , collapse = "_" )
loc_org <- apply( expand.grid(c("east", "west"), c("organic", "conventional")) , 1 , paste , collapse = "_" )
loc_live <- apply( expand.grid(c("east", "west"), c("livestock", "crop")) , 1 , paste , collapse = "_" )
threeway <- apply( expand.grid(c("east", "west"), 
                               c("livestock", "crop"), 
                               c("organic", "conventional")), 1 , paste , collapse = "_" )

dp_3way <- do.call(rbind,
                   lapply(threeway, function(i)
                     prep_sp_gg(context=i)
                   ))


dp_2way <- lapply(list(live_org, loc_org, loc_live), function(x)
  do.call(rbind,
          lapply(x, function(i)
            prep_sp_gg(context=i)
          ))
)

dp_2way[[1]]$facet <- "Management focus X\n Production system"
dp_2way[[2]]$facet <- "Location X\n Production system"
dp_2way[[3]]$facet <- "Location X\n Management focus "


order_y <- 
  as.vector(
    with(
      dp[[1]],
      rev(
        plyr::revalue(factor(var_name), cc_adapt_names)
      )
    )
  )


gg_2way <- lapply(dp_2way, function(i)
  i %>% 
    mutate(var_name=plyr::revalue(factor(var_name), cc_adapt_names)) %>% 
    mutate(model=str_replace_all(model, "_", " x ")) %>% 
    mutate(model=str_replace(model, "livestock", "Non-Specialized Crop")) %>%
    mutate(model=str_replace(model, "crop", "Specialized Crop")) %>%
    mutate(model=str_to_title(model)) %>%
    mutate(model=str_wrap(model, 15)) %>%
    ggplot(aes(y=fct_relevel(var_name, order_y), 
               group = model, 
               col=model))  +
    geom_linerange(aes(xmin = lb, xmax = ub), 
                   size=.6, 
                   position=position_dodge2(padding = 3,width = .5, reverse = T)) +
    geom_linerange(aes(xmin = lb90, xmax = ub90), 
                   size=1, 
                   position=position_dodge2(padding = 3,width = .5, reverse = T)) +
    geom_point(aes(x=point, shape=model), 
               size=1.5, 
               position=position_dodge2(padding = 3,width = .5, reverse = T)) +
    scale_shape_manual(values=1:8) +
    scale_y_discrete(labels= str_wrap(order_y, 18)) +
    ylab(NULL) +
    xlim(0,50) +
    xlab(" ") +
    theme_bw(base_size = 26) +
    guides(col=guide_legend("Model", byrow = T),
           shape=guide_legend("Model", byrow = T)) +
    theme(#legend.spacing.y = unit(1.5, 'cm'),
      # legend.position = "bottom",
      legend.direction="vertical",
      legend.justification = c(0.5, 1),
      # legend.title = element_blank(),
      legend.box.margin=margin(-20,0,0,0))  +
    facet_grid( ~ facet) 
)



dp_3way$facet <- "Location X Management \nfocus X Production system"

gg_3way <- dp_3way %>% 
  mutate(var_name=plyr::revalue(factor(var_name), cc_adapt_names)) %>% 
  mutate(model=str_replace_all(model, "_", " x ")) %>% 
  mutate(model=str_replace(model, "livestock", "Non-Specialized Crop")) %>%
  mutate(model=str_replace(model, "crop", "Specialized Crop")) %>%
  mutate(model=str_to_title(model)) %>%
  mutate(model=str_wrap(model, 15)) %>%
  ggplot(aes(y=fct_relevel(var_name, order_y), 
             group = model, 
             col=model))  +
  geom_linerange(aes(xmin = lb, xmax = ub), 
                 size=.6, 
                 position=position_dodge2(padding = 3,width = .7, reverse = T)) +
  geom_linerange(aes(xmin = lb90, xmax = ub90), 
                 size=1, 
                 position=position_dodge2(padding = 3,width = .7, reverse = T)) +
  geom_point(aes(x=point, shape=model), 
             size=1.5, 
             position=position_dodge2(padding = 3,width = .7, reverse = T)) +
  scale_shape_manual(values=1:8) +
  scale_y_discrete(labels= str_wrap(order_y, 18)) +
  ylab(NULL) +
  xlim(0,50) +
  xlab(" ") +
  theme_bw(base_size = 26) +
  guides(col=guide_legend("d)", byrow = T),
         shape=guide_legend("d)", byrow = T)) +
  theme(#legend.spacing.y = unit(1.5, 'cm'),
    # legend.position = "bottom",
    legend.direction="vertical",
    legend.justification = c(0.5, 1),
    # legend.title = element_blank(),
    legend.box.margin=margin(-20,0,0,0)) +  
  facet_grid( ~ facet)


gg_2way[[1]]  +
  scale_color_manual(values=unname(graf_palettes$kelly[1:4])) +
  xlab("Preference share (%)") +
  guides(col=guide_legend("", byrow = T),
         shape=guide_legend("", byrow = T)) 
ggsave(filename = paste0("img/gg_SP_man_prod.png"), 
       dpi="print",
       width = 12, height = 16) 

gg_2way[[2]]  +
  scale_color_manual(values=unname(graf_palettes$kelly[5:8])) +
  xlab("Preference share (%)") +
  guides(col=guide_legend("", byrow = T),
         shape=guide_legend("", byrow = T)) 
ggsave(filename = paste0("img/gg_SP_loc_prod.png"), 
       dpi="print",
       width = 12, height = 16) 

gg_2way[[3]]  +
  scale_color_manual(values=unname(graf_palettes$kelly[9:12])) +
  xlab("Preference share (%)") +
  guides(col=guide_legend("", byrow = T),
         shape=guide_legend("", byrow = T)) 
ggsave(filename = paste0("img/gg_SP_loc_man.png"), 
       dpi="print",
       width = 12, height = 16) 


gg_3way  +
  scale_color_manual(values=unname(graf_palettes$kelly[13:20])) +
  xlab("Preference share (%)") +
  guides(col=guide_legend("", byrow = T),
         shape=guide_legend("", byrow = T)) 
ggsave(filename = paste0("img/gg_SP_3way.png"), 
       dpi="print",
       width = 12, height = 16) 

# gg_patch_hetero2 <- 
#   (
#     gg_2way[[1]]  +
#       scale_color_manual(values=unname(graf_palettes$kelly[1:4])) +
#       guides(col=guide_legend("a)", byrow = T),
#              shape=guide_legend("a)", byrow = T)) +
#       theme(
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.line.x = element_blank(),
#         axis.title.x = element_blank(),
#         plot.tag.position  = c(.4, .88)
#       ) |
#       
#       gg_2way[[2]] +
#       scale_color_manual(values=unname(graf_palettes$kelly[5:8])) + 
#       guides(col=guide_legend("b)", byrow = T),
#              shape=guide_legend("b)", byrow = T)) +
#       theme(
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         axis.line = element_blank(),
#         axis.title = element_blank(),
#         plot.tag.position  = c(.08, .88)
#       )
#   ) / 
#   (
#     gg_2way[[3]] +
#       scale_color_manual(values=unname(graf_palettes$kelly[9:12])) + 
#       guides(col=guide_legend("c)", byrow = T),
#              shape=guide_legend("c)", byrow = T)) +
#       theme(
#         # axis.text.y = element_blank(),
#         # axis.ticks.y = element_blank(),
#         # axis.line.y = element_blank(),
#         # axis.title.y = element_blank(),
#         axis.title.x = element_blank(),
#         plot.tag.position  = c(.4, .88)
#       ) |
#       gg_3way +
#       scale_color_manual(values=unname(graf_palettes$kelly[13:20])) + 
#       theme(
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.line.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.title.x = element_blank(),
#         plot.tag.position  = c(.08, .88))
#   ) /
#   plot_spacer() +
#   plot_layout(
#     heights = c(100, 100, 1),
#     guides = "collect"
#   ) &
#   theme(legend.position = "right",
#         legend.justification='right',
#         plot.margin = unit(c(0,0,0,0), "cm")) & 
#   plot_annotation(tag_levels = "a",tag_suffix = ")")
# 
# gg_SP_hetero2 <- ggdraw(add_sub(gg_patch_hetero2, "Importance assigned by farmers (%)", 
#                                vpadding=grid::unit(0, "lines"),
#                                y = 0.9, x = 0.4, hjust = 0, size = 22))
# 
# 
# ggsave(filename = paste0("img/gg_SP_hetero2.png"), 
#        plot = gg_SP_hetero2, dpi="print",
#        width = 16, height = 25)
```

# Save SP tables
```{r}
df_sp_hetero <- do.call(rbind, c(dp, dp_2way, list(dp_3way)))
saveRDS(df_sp_hetero, "model_outputs/df_sp_hetero.rds")

df_sp_hetero <- df_sp_hetero %>% 
  # mutate(var_name=str_wrap(recode(var_name, 
  #                                 "business_div"='Business diversification'  ,
  #                                 "catch_crops"='Use of catch crops'   ,
  #                                 "con_tillage"='Conservation tillage'   ,
  #                                 "fertilizer"='Adjust fertilizer'    ,
  #                                 "irrigation"='Use of irrigation'    ,
  #                                 "manag_rhythm"='Adjust management rhythm'  ,
  #                                 "mixed_crops"='Use of mixed crops'   ,
  #                                 "plant_protect"='Adjustment of farm chemicals use' ,
  #                                 "prec_farming"='Use of precision farming'  ,
  #                                 "rep(1, R)"='Insurance'     ,
#                                 "res_crops"='Use of drought-resistant crops',
#                                 "rotation"='Diversify crop rotation',
#                                 "res_varieties"='Use of drought-resistant crop varieties'), width = 18)) %>%
mutate(var_name=revalue(factor(var_name), cc_adapt_names))
mutate(model=str_replace_all(model, "_", " x ")) %>% 
  mutate(model=str_to_title(model),
         facet=str_count(model, pattern = ' X '))  


df_sp_hetero %>%   
  ggplot(aes(y=as.integer(rank), x=factor(model, 
                                          levels = unique(model)), col=var_name)) +
  # geom_point(size=5, shape=15) +
  geom_line(aes(group=var_name), lwd=3.5) +
  facet_grid (.~ facet, scales = "free_x", space = "free_x") +
  theme_bw(base_size=24) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        legend.key.size = unit(3, 'lines'),
        legend.title.align=0.5,
        legend.justification = c(0.5, 1),
        legend.text=element_text(size=14),
        legend.title=element_text(size=18),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  ylab("Preference rank") +
  xlab("Farm characterization") +
  scale_y_reverse(breaks = 1:13) +
  scale_colour_grafify(name= "Adaptation\nstrategy", palette = "kelly", breaks=unique(df_sp_hetero$var_name)) 
```



```{r}
sapply(unique(dp[[3]]$var_name), function(i)
  min(dp[[3]]$ub[dp[[3]]$var_name==i]) <= max(dp[[2]]$lb[dp[[2]]$var_name==i])
)
```