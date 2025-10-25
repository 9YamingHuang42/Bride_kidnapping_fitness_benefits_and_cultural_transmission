library( dagitty )
library( readxl )
library( dplyr )
library( tidyr )
library( rethinking )
library( ggplot2 )
library( ggdist )
library( gt )
library( gto )
library( officer )
library( cmdstanr )

#3 Fertility ----
##3.1 DAG ----
Dag_RS <- 
  dagitty( 'dag {
bb="-0.5,-0.5,0.5,0.5"
Age [pos="-0.150,-0.330"]
Brideprice [pos="-0.200,0.100"]
Cur.wealth [pos="0.200,0.100"]
Fertility [outcome,pos="0.000,0.200"]
His.wealth [latent,pos="-0.270,-0.160"]
Marriage [exposure,pos="0.150,-0.330"]
Religiosity [pos="0.270,-0.160"]
U_village [latent,pos="0.120,0.200"]
Age -> Brideprice
Age -> Fertility
Age -> His.wealth
Age -> Marriage
Age -> Religiosity
Brideprice -> Cur.wealth
His.wealth -> Brideprice
His.wealth -> Cur.wealth
His.wealth -> Fertility
Marriage -> Brideprice
Marriage -> Fertility
Religiosity -> Fertility
Religiosity -> Marriage
U_village -> Fertility
}')

adjustmentSets( Dag_RS , 
                exposure = "Marriage" , 
                outcome = "Fertility" ,
                effect = "total" )

##3.2 Data preparation ----
load( file = "Bride_kidnapping_fertility_analyses.RData" )

##3.3 Models ----
Model_list_RS <- with( RS.data , list(
  Fertility = No.children ,
  VID = as.integer( VID ) , 
  Kidnap = as.integer( Kidnap ) ,
  Age = as.integer( Age.c ) , 
  Pray = as.integer( Pray ) ) )

###3.3.1 Marriage type ----
{set.seed(123)
  Model_M_RS <- ulam(
    alist(
      Fertility ~ normal( mu , sigma ),
      mu <- bM[Kidnap] + 
        bA[Age] + bR[Pray] + # confounders
        V[VID] , # village intercepts 
      
      bM[Kidnap] ~ normal( 0 , 0.5 ) ,
      
      bA[Age] ~ normal( 0 , 0.6 ) ,
      bR[Pray] ~ normal( 0 , 0.3 ) ,
      
      sigma ~ exponential( 1 ) ,
      
      # define effects using other parameters
      transpars> vector[VID]: V <<- V_bar + z_V*sigma_V ,
      
      V_bar ~ normal( 1 , 0.3 ) ,
      z_V[VID] ~ normal( 0 , 0.5 ) ,
      sigma_V ~ exponential( 1 ) 
    ) , data = Model_list_RS , 
    iter = 1500 , warmup = 500 , chains = 6 , cores = 6 , 
    log_lik = TRUE , control = list( adapt_delta = 0.99 ) 
  ) }
Pre_model_M_RS <- precis( Model_M_RS , depth = 3 , prob = 0.90 , 
                          pars = c( "bM" , "bA" , "bR" , "V" ) )
Pre_model_M_RS

###3.3.2 Marriage type stratified by village ----
{set.seed(123)
  Model_MV_RS <- ulam(
    alist(
      Fertility ~ normal( mu , sigma ),
      mu <- bMV[Kidnap,VID] + 
        bA[Age] + bR[Pray] + # confounders
        V[VID] , # village intercepts  
      
      matrix[Kidnap,VID]: bMV ~ normal( 0 , 0.5 ) ,
      
      bA[Age] ~ normal( 0 , 0.6 ) ,
      bR[Pray] ~ normal( 0 , 0.3 ) ,
      
      sigma ~ exponential( 1 ) ,
      
      # define effects using other parameters
      transpars> vector[VID]: V <<- V_bar + z_V*sigma_V ,
      
      V_bar ~ normal( 1 , 0.3 ) ,
      z_V[VID] ~ normal( 0 , 0.5 ) ,
      sigma_V ~ exponential( 1 ) 
    ) , data = Model_list_RS , 
    iter = 1500 , warmup = 500 , chains = 6 , cores = 6 , 
    log_lik = TRUE , control = list( adapt_delta = 0.99 ) 
  ) }
Pre_model_MV_RS <- precis( Model_MV_RS , depth = 3 , prob = 0.90 , 
                           pars = c( "bMV" , "bA" , "bR" , "V" ) )
Pre_model_MV_RS

##3.4 Figures ----
Post_model_M_RS <- extract.samples( Model_M_RS )
Post_model_MV_RS <- extract.samples( Model_MV_RS )

###3.4.1 Marriage type ----
####3.4.1.1 Posterior distribution of estimates and differences ----
{
  par( mfrow = c( 1 , 1 ) , oma = c( 1.2 , 1.2 , 0.2 , 0.2 ) , mar = c( 0 , 0 , 0 , 0 ) )
  dens( Post_model_M_RS$bM[,1] , 
        adj = 1 , 
        xlim = c( -1.8 , 1.8 ) ,
        ylim = c( 0 , 1.6 ) ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#e47178" , cex.axis = 1.2 )
  dens( Post_model_M_RS$bM[,2] ,
        adj = 1 , 
        lwd = 3 , col = "#e47178" , 
        add = T )
  mtext( "Density" , side = 2 , line = 2.2 , cex = 1.5 )
  mtext( "Posterior distribution of estimates" , side = 1 , line = 2.4 , cex = 1.5 )
  
  legend( x = -0.1 , y = 1.7 , 
          box.col = "white",
          legend = c( "Non-kidnapped female" , 
                      "Kidnapped female" ) , 
          lty = c( 3 , 1 ) ,  
          col = c( "#e47178" , "#e47178" ) , 
          lwd = 2 ,
          cex = 1 , 
          bty = "n" ,
          y.intersp = 1.2 ,
          x.intersp = 0.4 ,
          seg.len = 1.5  )
}

dev.off()

####3.4.1.2 Estimate difference of kidnapping and non-kidnapping ----
bM_diff <- tibble( Value = c( Post_model_M_RS$bM[,2] - Post_model_M_RS$bM[,1] ) )
bM_diff_summary <- bM_diff %>% 
  summarise( Mean = mean( Value ),
             Ci_low = quantile( Value , 0.05 ),
             Ci_high = quantile( Value , 0.95 ) )

ggplot( ) +
  geom_density( data = bM_diff , 
                aes( x = Value ) , 
                fill = "#e47178" , 
                alpha = 0.3 , 
                color = NA , 
                adjust = 1.5 ) +
  geom_vline( xintercept = 0 , 
              linetype = 2 , 
              linewidth = 1 , 
              color = "dimgray") + 
  geom_point( data = bM_diff_summary , 
              aes( x = Mean , y = 0 ) ,
              color = "red" , size = 2 ) +
  geom_errorbarh( data = bM_diff_summary ,
                  aes( xmin = Ci_low , 
                       xmax = Ci_high , 
                       y = 0 ) , 
                  color = "red" ,
                  linewidth = 1 ) +
  scale_x_continuous( limits = c( -0.5 , 1.5 ) , breaks = seq( -0.5 , 1.5 , 0.5 ) ) +
  scale_y_continuous( limits = c( 0 , 2 ) , breaks = seq( 0 , 2 , 1 ) ) +
  xlab( "Posterior distribution of mean difference" ) +
  ylab( "Density" ) +
  theme(plot.margin = margin(10, 10, 10, 20),
        strip.background = element_rect(color = "black", fill = "white") ,
        strip.text.x = element_text(size = 16),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black",fill = NA),
        panel.background = element_blank(),
        plot.title = element_text(size = 16),
        legend.position = "none",
        axis.title.x = element_text(size = 16,
                                    margin = margin(t = 0.3, r = 0, b = 0, l = 0,unit = "cm")),
        axis.text.x = element_text(colour = "black",size = 14,
                                   margin = margin(t = 0.1, r = 0, b = 0, l = 0,unit = "cm")),
        axis.title.y = element_text(size = 16,
                                    margin = margin(t = 0, r = 0.5, b = 0, l = 0,unit = "cm")),
        axis.text.y = element_text(colour = "black",size = 14,
                                   margin = margin(t = 0, r = 0.2, b = 0, l = 0,unit = "cm")),
        legend.title = element_text( size = 12,face = "bold"),
        legend.text = element_text( size = 12,face = "bold") )

###3.4.2 Marriage type stratified by village ----
####3.4.2.1 Posterior distribution of estimates ----
{
  par( mfrow = c( 1 , 2 ) , oma = c( 2.2 , 3.2 , 0.2 , 0.2 ) , mar = c( 1.4 , 0.4 , 0 , 0 ) )
  # village 1
  dens( Post_model_MV_RS$bMV[,1,1] ,
        adj = 1 , 
        xlim = c( -3 , 3 ) ,
        ylim = c( 0 , 1.5 ) ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#e47178" , cex.axis = 1.2 )
  dens( Post_model_MV_RS$bMV[,2,1] ,
        adj = 1 , 
        lwd = 3 , col = "#e47178" , 
        add = T )
  mtext( "Village 1" , side = 1 , line = 2.2 , cex = 1.2 )
  mtext( "Density" , side = 2 , line = 2.2 , cex = 1.2 )
  
  # village 2
  dens( Post_model_MV_RS$bMV[,1,2] ,
        adj = 1 , 
        xlim = c( -3 , 3 ) ,
        ylim = c( 0 , 1.5 ) ,
        xaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#e47178" , cex.axis = 1.2 )
  dens( Post_model_MV_RS$bMV[,2,2] ,
        adj = 1 , 
        lwd = 3 , col = "#e47178" , 
        add = T )
  mtext( "Village 2" , side = 1 , line = 2.2 , cex = 1.2 )
  legend( x = 0.1 , y = 1.6 , 
          box.col = "white",
          legend = c( "Non-kidnapped female" , 
                      "Kidnapped female" ) , 
          lty = c( 3 , 1 ) ,  
          col = c( "#e47178" , "#e47178" ) , 
          lwd = 2 ,
          cex = 1.0 , 
          bty = "n" ,
          y.intersp = 1.0 ,
          x.intersp = 0.4 ,
          seg.len = 1.5  )
  
}

dev.off()

####3.4.2.2 Estimate difference of kidnapping and non-kidnapping ----
tibble( Value = c( Post_model_MV_RS$bMV[,2,1] - Post_model_MV_RS$bMV[,1,1] , 
                   Post_model_MV_RS$bMV[,2,2] - Post_model_MV_RS$bMV[,1,2] ) ,
        Type = c( rep( "V1:Female" , 6000 ) ,
                  rep( "V2:Female" , 6000 ) ) ) %>% 
  mutate( Type = factor( Type , 
                         levels = c( "V2:Female" , "V1:Female" ) , 
                         labels = c( "V2:Female" , "V1:Female" ) ) ) %>% 
  ggplot( aes( x = Value , y = Type , 
               fill = Type , color = Type) ) +
  geom_vline( xintercept = 0, linetype = 2 , linewidth = 1 ,
              color = "dimgray" ) +
  stat_halfeye( .width = .90, height = 0.8 ) + 
  scale_fill_manual( values = alpha( c( "#e47178" , "#e47178" ) , 0.3 ) ) +
  scale_color_manual( values = c( "#e47178" , "#e47178" ) ) +
  scale_y_discrete( labels = c( "V2" , "V1" ) ) +
  xlab( "Posterior distribution of mean difference" ) +
  ylab( "Village" ) +
  coord_cartesian( ylim = c( 1.5, 2.3 ) ) +
  scale_x_continuous( limits = c( -1.5 , 2.5 ) , breaks = c( -1 , 1 , 0 , 1 , 2 ) ) +
  theme(plot.margin = margin(10, 10, 10, 20),
        strip.background = element_rect(color = "black", fill = "white") ,
        strip.text.x = element_text(size = 16),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "Gainsboro"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black",fill = NA),
        panel.background = element_blank(),
        plot.title = element_text(size = 16),
        legend.position = "none",
        axis.title.x = element_text(size = 16,
                                    margin = margin(t = 0.3, r = 0, b = 0, l = 0,unit = "cm")),
        axis.text.x = element_text(colour = "black",size = 14,
                                   margin = margin(t = 0.1, r = 0, b = 0, l = 0,unit = "cm")),
        axis.title.y = element_text(size = 16,
                                    margin = margin(t = 0, r = 0.5, b = 0, l = 0,unit = "cm")),
        axis.text.y = element_text(colour = "black",size = 14,
                                   margin = margin(t = 0, r = 0.2, b = 0, l = 0,unit = "cm")),
        legend.title = element_text( size = 12,face = "bold"),
        legend.text = element_text( size = 12,face = "bold") )

###3.4.3 Figure2: Estimate difference of kidnapping and non-kidnapping (marriage age + fertility) ----
tibble( Value = c( Post_model_MG_MA$bMG[,2,1] - Post_model_MG_MA$bMG[,1,1] , 
                   Post_model_MG_MA$bMG[,2,2] - Post_model_MG_MA$bMG[,1,2] ,
                   Post_model_M_RS$bM[,2] - Post_model_M_RS$bM[,1] ) ,
        Gender = c( rep( "Female" , 6000 ) ,
                    rep( "Male" , 6000 ) , 
                    rep( "Female" , 6000 ) ) , 
        Type = c( rep( "Age at first marriage-f" , 6000 ) ,
                  rep( "Age at first marriage-m" , 6000 ) , 
                  rep( "Number of births" , 6000 ) ) ) %>% 
  mutate( Gender = factor( Gender , 
                           levels = c( "Male" , "Female" ) , 
                           labels = c( "Male" , "Female" ) ) , 
          Type = factor( Type , 
                         levels = c( "Number of births" , "Age at first marriage-m" , "Age at first marriage-f" ) , 
                         labels = c( "Number of births" , "Age at first marriage-m" , "Age at first marriage-f" ) ) ) %>% 
  ggplot( aes( x = Value , y = Type , 
               fill = Gender , color = Gender) ) +
  geom_vline( xintercept = 0, linetype = 2 , linewidth = 1 ,
              color = "dimgray" ) +
  stat_halfeye( .width = .90, height = 0.8 ) + 
  scale_fill_manual( values = alpha( c( "#a0add0" , "#e47178" ) , 0.3 ) ) +
  scale_color_manual( values = c( "#a0add0" , "#e47178" ) ) +
  scale_y_discrete( labels = c( "Number of births" , "Age at first marriage" , "Age at first marriage" ) ) +
  xlab( "Posterior distribution of mean difference" ) +
  ylab( "Models" ) +
  coord_cartesian( ylim = c( 1.5, 3.4 ) ) +
  scale_x_continuous( limits = c( -1 , 1.2 ) , breaks = c( -1 , 0 , 1 ) ) +
  theme(plot.margin = margin(10, 10, 10, 20),
        strip.background = element_rect(color = "black", fill = "white") ,
        strip.text.x = element_text(size = 16),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "Gainsboro"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black",fill = NA),
        panel.background = element_blank(),
        plot.title = element_text(size = 16),
        legend.position = c(0.99,0.99),
        legend.justification = c(0.99,0.99),
        axis.title.x = element_text(size = 16,
                                    margin = margin(t = 0.2, r = 0, b = 0, l = 0,unit = "cm")),
        axis.text.x = element_text(colour = "black",size = 14,
                                   margin = margin(t = 0.1, r = 0, b = 0, l = 0,unit = "cm")),
        axis.title.y = element_text(size = 16,
                                    margin = margin(t = 0, r = 0.2, b = 0, l = 0,unit = "cm")),
        axis.text.y = element_text(colour = "black",size = 14,
                                   margin = margin(t = 0, r = 0.1, b = 0, l = 0,unit = "cm")),
        legend.title = element_text( size = 12),
        legend.text = element_text( size = 12) )

##3.5 Outputs ----
###3.5.1 Marriage type ----
####3.5.1.1 Estimates ----
Pre_M_RS_output <- data.frame( Mean = Pre_model_M_RS$mean ,
                               CI5 = Pre_model_M_RS$`5%` ,
                               CI95 = Pre_model_M_RS$`95%` ,
                               Variable = c( "Non-kidnapped" , 
                                             "Kidnapped" , 
                                             "Age: <40" , "Age: 40-49" , 
                                             "Age: 50-59" , "Age: >=60" , 
                                             "Pray: seldom" , "Pray: often" , 
                                             "Village 1" , "Village 2" ) ) %>% 
  mutate( Variable = factor( Variable , 
                             levels = c( "Non-kidnapped" , 
                                         "Kidnapped" , 
                                         "Age: <40" , "Age: 40-49" , 
                                         "Age: 50-59" , "Age: >=60" , 
                                         "Pray: seldom" , "Pray: often" , 
                                         "Village 1" , "Village 2" ) ,
                             labels = c( "Non-kidnapped" , 
                                         "Kidnapped" , 
                                         "Age: <40" , "Age: 40-49" , 
                                         "Age: 50-59" , "Age: >=60" , 
                                         "Pray: seldom" , "Pray: often" , 
                                         "Village 1" , "Village 2" ) ) ,
          Model = "Fertility" , 
          "Mean [CI]" = paste( format( round( Mean , 2 ), nsmall = 2 ) , 
                               " [" , 
                               format( round( CI5 , 2 ), nsmall = 2 ) , 
                               ", " , 
                               format( round( CI95 , 2 ), nsmall = 2 ) , 
                               "]", 
                               sep = "") )

M_RS_output <- Pre_M_RS_output %>% 
  select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( "Variable" ~ px( 170 ) , 
              everything() ~ px( 150 ) )

M_RS_output

####3.5.1.2 Estimate differences ----
tibble( Gender = c( "Woman" ) ,
        bM_diff = c( sprintf("%0.2f" , mean( Post_model_M_RS$bM[,2] - Post_model_M_RS$bM[,1] ) ) ) ,
        
        bM_diff_L = c( sprintf("%0.2f" , PI( Post_model_M_RS$bM[,2] - Post_model_M_RS$bM[,1] , prob = 0.90 )[1] ) ) ,
        
        bM_diff_H = c( sprintf("%0.2f" , PI( Post_model_M_RS$bM[,2] - Post_model_M_RS$bM[,1] , prob = 0.90 )[2] ) ) ,
        `bM_diff[90%CI]` = paste( bM_diff , "[" , bM_diff_L , ", " , bM_diff_H , "]") , 
        Pro_bM_diff = c( sprintf( "%0.4f" , 
                                  length( which( ( Post_model_M_RS$bM[,2] - Post_model_M_RS$bM[,1] ) > 0 ) ) / 6000 ) ) ) %>% 
  mutate( Pro_bM_diff = paste( round( as.numeric( Pro_bM_diff ) * 100 , 2 ) , "%" ) ) %>% 
  select( Gender , `bM_diff[90%CI]` , Pro_bM_diff ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels(everything() ) ) %>% 
  tab_style( style = list( cell_text(font = "Times New Roman") ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "center",
              columns = c( Gender , `bM_diff[90%CI]` , Pro_bM_diff ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( everything() ~ px( 150 ) )

###3.5.2 Marriage type stratified by village ----
####3.5.2.1 Estimates ----
Pre_MV_RS_output <- data.frame( Mean = Pre_model_MV_RS$mean ,
                                CI5 = Pre_model_MV_RS$`5%` ,
                                CI95 = Pre_model_MV_RS$`95%` ,
                                Variable = c( "Non-kidnapped: V1 female" , 
                                              "Kidnapped: V1 female" , 
                                              "Non-kidnapped: V2 female" , 
                                              "Kidnapped: V2 female" , 
                                              
                                              "Age: <40" , "Age: 40-49" , 
                                              "Age: 50-59" , "Age: >=60" , 
                                              "Pray: seldom" , "Pray: often" ,
                                              "Village 1" , "Village 2" ) ) %>% 
  mutate( Variable = factor( Variable , 
                             levels = c( "Non-kidnapped: V1 female" , 
                                         "Kidnapped: V1 female" , 
                                         "Non-kidnapped: V2 female" , 
                                         "Kidnapped: V2 female" , 
                                         
                                         "Age: <40" , "Age: 40-49" , 
                                         "Age: 50-59" , "Age: >=60" , 
                                         "Pray: seldom" , "Pray: often" ,
                                         "Village 1" , "Village 2" ) ,
                             labels = c( "Non-kidnapped: V1 female" , 
                                         "Kidnapped: V1 female" , 
                                         "Non-kidnapped: V2 female" , 
                                         "Kidnapped: V2 female" , 
                                         
                                         "Age: <40" , "Age: 40-49" , 
                                         "Age: 50-59" , "Age: >=60" , 
                                         "Pray: seldom" , "Pray: often" ,
                                         "Village 1" , "Village 2" ) ) ,
          Model = "Fertility" , 
          "Mean [CI]" = paste( format( round( Mean , 2 ), nsmall = 2 ) , 
                               " [" , 
                               format( round( CI5 , 2 ), nsmall = 2 ) , 
                               ", " , 
                               format( round( CI95 , 2 ), nsmall = 2 ) , 
                               "]", 
                               sep = "") )

MV_RS_output <- Pre_MV_RS_output %>% 
  select( Variable , "Mean [CI]" ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels( everything() ) ) %>% 
  tab_style( style = list( cell_text( font = "Times New Roman" ) ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "left",
              columns = "Variable" ) %>%
  cols_align( align = "center",
              columns = c( "Mean [CI]" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( "Variable" ~ px( 250 ) , 
              everything() ~ px( 150 ) )

MV_RS_output

####3.5.2.2 Estimate differences ----
tibble( Village = c( "V1" , "V2" ) ,
        bMV = c( sprintf("%0.2f" , mean( Post_model_MV_RS$bMV[,2,1] - Post_model_MV_RS$bMV[,1,1] ) ) , 
                 sprintf("%0.2f" , mean( Post_model_MV_RS$bMV[,2,2] - Post_model_MV_RS$bMV[,1,2] ) ) ) ,
        
        bMV_L = c( sprintf("%0.2f" , PI( Post_model_MV_RS$bMV[,2,1] - Post_model_MV_RS$bMV[,1,1] , prob = 0.90 )[1] ) , 
                   sprintf("%0.2f" , PI( Post_model_MV_RS$bMV[,2,2] - Post_model_MV_RS$bMV[,1,2] , prob = 0.90 )[1] ) ) ,
        
        bMV_H = c( sprintf("%0.2f" , PI( Post_model_MV_RS$bMV[,2,1] - Post_model_MV_RS$bMV[,1,1] , prob = 0.90 )[2] ) , 
                   sprintf("%0.2f" , PI( Post_model_MV_RS$bMV[,2,2] - Post_model_MV_RS$bMV[,1,2] , prob = 0.90 )[2] ) ) ,
        `bMV[90%CI]` = paste( bMV , "[" , bMV_L , ", " , bMV_H , "]") ,
        Pro_bMV = c( sprintf( "%0.4f" ,  length( which( Post_model_MV_RS$bMV[,2,1] - Post_model_MV_RS$bMV[,1,1] > 0 ) ) / 6000 ) , 
                     sprintf( "%0.4f" ,  length( which( Post_model_MV_RS$bMV[,2,2] - Post_model_MV_RS$bMV[,1,2] > 0 ) ) / 6000 ) ) ) %>% 
  mutate( Pro_bMV = paste( round( as.numeric( Pro_bMV ) * 100 , 2 ) , "%" ) ) %>% 
  select( Village , `bMV[90%CI]` , Pro_bMV ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels(everything() ) ) %>% 
  tab_style( style = list( cell_text(font = "Times New Roman") ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "center",
              columns = c( Village , `bMV[90%CI]` , Pro_bMV ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( everything() ~ px( 150 ) )