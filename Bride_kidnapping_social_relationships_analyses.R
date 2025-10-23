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

#1 Be nominated for various social relationships ----
##1.1 DAG ----
Dag_BN <- 
  dagitty( 'dag {
bb="-0.5,-0.5,0.5,0.5"
Age.cohort [pos="-0.290,-0.140"]
Be.nominated [outcome,pos="0.000,0.320"]
Cur.wealth [pos="0.290,-0.140"]
Gender [pos="-0.150,-0.315"]
Marriage [exposure,pos="0.150,-0.315"]
No.nomination [pos="-0.230,0.190"]
Religiosity [pos="0.230,0.190"]
U_village [latent,pos="0.180,0.320"]
Age.cohort -> Be.nominated
Age.cohort -> Marriage
Age.cohort -> No.nomination
Age.cohort -> Religiosity
Cur.wealth -> Be.nominated
Cur.wealth -> No.nomination
Gender -> Be.nominated
Gender -> No.nomination
Marriage -> Be.nominated
Marriage -> Cur.wealth
Marriage -> No.nomination
No.nomination -> Be.nominated
Religiosity -> Be.nominated
Religiosity -> Marriage
Religiosity -> No.nomination
U_village -> Be.nominated
}
')

adjustmentSets( Dag_BN , 
                exposure = "Marriage" , 
                outcome = "Be.nominated" ,
                effect = "total" )

##1.2 Data loading ----
load( file = "Bride_kidnapping_social_relationships_analyses.RData" )

##1.3 Models ----
###1.3.1 The number of being nominated including all types ----
Model_M_list_all <- with( BN.data , list(
  Nominated = as.integer( No.be.nominated ) ,
  VID = as.integer( VID ) , 
  Gender = as.integer( Gender ) ,
  Kidnap = as.integer( Kidnap ) ,
  Age = as.integer( Age.c ) ,
  Pray = as.integer( Pray ) ) )

####1.3.1.1 Marriage type stratified by gender ----
{set.seed(123)
  Model_MG_all <- ulam(
    alist(
      Nominated ~ dpois( lambda ),
      log( lambda ) <- bMG[Kidnap,Gender] + 
        bA[Age] + bR[Pray] + # confounders
        V[VID] , # village intercepts 
      
      matrix[Kidnap,Gender]: bMG ~ normal( 0 , 0.5 ) ,
      
      bA[Age] ~ normal( 0 , 0.2 ) ,
      bR[Pray] ~ normal( 0 , 0.2 ) ,
      
      # define effects using other parameters
      transpars> vector[VID]: V <<- V_bar + z_V*sigma_V ,
      
      V_bar ~ normal( 1 , 0.3 ) ,
      z_V[VID] ~ normal( 0.5 , 0.6 ) ,
      sigma_V ~ exponential( 2 ) 
    ) , data = Model_M_list_all , 
    iter = 1500 , warmup = 500 , chains = 6 , cores = 6 , 
    log_lik = TRUE , control = list( adapt_delta = 0.99 ) 
  ) }
Pre_model_MG_all <- precis( Model_MG_all , depth = 3 , prob = 0.90 , 
                            pars = c( "bMG" , "bA" , "bR" , "V" ) )
Pre_model_MG_all

####1.3.1.2 Marriage type stratified by gender and village ----
{set.seed(123)
  Model_MVG_all <- ulam(
    alist(
      Nominated ~ dpois( lambda ),
      log( lambda ) <- bGV[Gender,VID] + bGVM[Gender,VID] * ( Kidnap - 1 ) + 
        bA[Age] + bR[Pray] + # confounders
        V[VID] , # village intercepts  
      
      matrix[Gender,VID]: bGV ~ normal( 0 , 0.5 ) ,
      matrix[Gender,VID]: bGVM ~ normal( 0 , 0.5 ) ,
      
      bA[Age] ~ normal( 0 , 0.2 ) ,
      bR[Pray] ~ normal( 0 , 0.2 ) ,
      
      # define effects using other parameters
      transpars> vector[VID]: V <<- V_bar + z_V*sigma_V ,
      
      V_bar ~ normal( 1 , 0.3 ) ,
      z_V[VID] ~ normal( 0.5 , 0.6 ) ,
      sigma_V ~ exponential( 2 )
    ) , data = Model_M_list_all , 
    iter = 1500 , warmup = 500 , chains = 6 , cores = 6 , 
    log_lik = TRUE , control = list( adapt_delta = 0.99 ) 
  ) }
Pre_model_MVG_all <- precis( Model_MVG_all , depth = 3 , prob = 0.90 , 
                             pars = c( "bGV" , "bGVM" , "bA" , "bR" , "V" ) )
Pre_model_MVG_all

###1.3.2 The number of being nominated for prestige ----
Model_M_list_pre <- with( BN.data , list(
  Nominated = as.integer( No.be.nominated.prestige ) ,
  VID = as.integer( VID ) , 
  Gender = as.integer( Gender ) ,
  Kidnap = as.integer( Kidnap ) ,
  Age = as.integer( Age.c ) , 
  Pray = as.integer( Pray ) ) )

####1.3.2.1 Marriage type stratified by gender ----
{set.seed(123)
  Model_MG_pre <- ulam(
    alist(
      Nominated ~ dpois( lambda ),
      log( lambda ) <- bMG[Kidnap,Gender] + 
        bA[Age] + bR[Pray] + # confounders
        V[VID] , # village intercepts 
      
      matrix[Kidnap,Gender]: bMG ~ normal( 0 , 0.5 ) ,
      
      bA[Age] ~ normal( 0 , 0.2 ) ,
      bR[Pray] ~ normal( 0 , 0.2 ) ,
      
      # define effects using other parameters
      transpars> vector[VID]: V <<- V_bar + z_V*sigma_V ,
      
      V_bar ~ normal( 1 , 0.3 ) ,
      z_V[VID] ~ normal( 0.5 , 0.6 ) ,
      sigma_V ~ exponential( 2 ) 
    ) , data = Model_M_list_pre , 
    iter = 1500 , warmup = 500 , chains = 6 , cores = 6 , 
    log_lik = TRUE , control = list( adapt_delta = 0.99 ) 
  ) }
Pre_model_MG_pre <- precis( Model_MG_pre , depth = 3 , prob = 0.90 , 
                            pars = c( "bMG" , "bA" , "bR" , "V" ) )
Pre_model_MG_pre

####1.3.2.2 Marriage type stratified by gender and village ----
{set.seed(123)
  Model_MVG_pre <- ulam(
    alist(
      Nominated ~ dpois( lambda ),
      log( lambda ) <- bGV[Gender,VID] + bGVM[Gender,VID] * ( Kidnap - 1 ) + 
        bA[Age] + bR[Pray] + # confounders
        V[VID] , # village intercepts 
      
      matrix[Gender,VID]: bGV ~ normal( 0 , 0.5 ) ,
      matrix[Gender,VID]: bGVM ~ normal( 0 , 0.5 ) ,
      
      bA[Age] ~ normal( 0 , 0.2 ) ,
      bR[Pray] ~ normal( 0 , 0.2 ) ,
      
      # define effects using other parameters
      transpars> vector[VID]: V <<- V_bar + z_V*sigma_V ,
      
      V_bar ~ normal( 1 , 0.3 ) ,
      z_V[VID] ~ normal( 0.5 , 0.6 ) ,
      sigma_V ~ exponential( 2 ) 
    ) , data = Model_M_list_pre , 
    iter = 1500 , warmup = 500 , chains = 6 , cores = 6 , 
    log_lik = TRUE , control = list( adapt_delta = 0.99 ) 
  ) }
Pre_model_MVG_pre <- precis( Model_MVG_pre , depth = 3 , prob = 0.90 , 
                             pars = c( "bGV" , "bGVM" , "bA" , "bR" , "V" ) )
Pre_model_MVG_pre

###1.3.3 The number of being nominated for financial support ----
Model_M_list_fin <- with( BN.data , list(
  Nominated = as.integer( No.be.nominated.financial ) ,
  VID = as.integer( VID ) , 
  Gender = as.integer( Gender ) ,
  Kidnap = as.integer( Kidnap ) ,
  Age = as.integer( Age.c ) , 
  Pray = as.integer( Pray ) ) )

####1.3.3.1 Marriage type stratified by gender ----
{set.seed(123)
  Model_MG_fin <- ulam(
    alist(
      Nominated ~ dpois( lambda ),
      log( lambda ) <- bMG[Kidnap,Gender] + 
        bA[Age] + bR[Pray] + # confounders
        V[VID] , # village intercepts  
      
      matrix[Kidnap,Gender]: bMG ~ normal( 0 , 0.5 ) ,
      
      bA[Age] ~ normal( 0 , 0.2 ) ,
      bR[Pray] ~ normal( 0 , 0.2 ) ,
      
      # define effects using other parameters
      transpars> vector[VID]: V <<- V_bar + z_V*sigma_V ,
      
      V_bar ~ normal( 1 , 0.3 ) ,
      z_V[VID] ~ normal( 0.5 , 0.6 ) ,
      sigma_V ~ exponential( 2 )  
    ) , data = Model_M_list_fin , 
    iter = 1500 , warmup = 500 , chains = 6 , cores = 6 , 
    log_lik = TRUE , control = list( adapt_delta = 0.99 ) 
  ) }
Pre_model_MG_fin <- precis( Model_MG_fin , depth = 3 , prob = 0.90 , 
                            pars = c( "bMG" , "bA" , "bR" , "V" ) )
Pre_model_MG_fin

####1.3.3.2 Marriage type stratified by gender and village ----
{set.seed(123)
  Model_MVG_fin <- ulam(
    alist(
      Nominated ~ dpois( lambda ),
      log( lambda ) <- bGV[Gender,VID] + bGVM[Gender,VID] * ( Kidnap - 1 ) + 
        bA[Age] + bR[Pray] + # confounders
        V[VID] , # village intercepts 
      
      matrix[Gender,VID]: bGV ~ normal( 0 , 0.5 ) ,
      matrix[Gender,VID]: bGVM ~ normal( 0 , 0.5 ) ,
      
      bA[Age] ~ normal( 0 , 0.2 ) ,
      bR[Pray] ~ normal( 0 , 0.2 ) ,
      
      # define effects using other parameters
      transpars> vector[VID]: V <<- V_bar + z_V*sigma_V ,
      
      V_bar ~ normal( 1 , 0.3 ) ,
      z_V[VID] ~ normal( 0.5 , 0.6 ) ,
      sigma_V ~ exponential( 2 ) 
    ) , data = Model_M_list_fin , 
    iter = 1500 , warmup = 500 , chains = 6 , cores = 6 , 
    log_lik = TRUE , control = list( adapt_delta = 0.99 ) 
  ) }
Pre_model_MVG_fin <- precis( Model_MVG_fin , depth = 3 , prob = 0.90 , 
                             pars = c( "bGV" , "bGVM" , "bA" , "bR" , "V" ) )
Pre_model_MVG_fin

###1.3.4 The number of being nominated for friendships ----
Model_M_list_fri <- with( BN.data , list(
  Nominated = as.integer( No.be.nominated.friend ) ,
  VID = as.integer( VID ) , 
  Gender = as.integer( Gender ) ,
  Kidnap = as.integer( Kidnap ) ,
  Age = as.integer( Age.c ) , 
  Pray = as.integer( Pray ) ) )

####1.3.4.1 Marriage type stratified by gender ----
{set.seed(123)
  Model_MG_fri <- ulam(
    alist(
      Nominated ~ dpois( lambda ),
      log( lambda ) <- bMG[Kidnap,Gender] + 
        bA[Age] + bR[Pray] + # confounders
        V[VID] , # village intercepts  
      
      matrix[Kidnap,Gender]: bMG ~ normal( 0 , 0.5 ) ,
      
      bA[Age] ~ normal( 0 , 0.2 ) ,
      bR[Pray] ~ normal( 0 , 0.2 ) ,
      
      # define effects using other parameters
      transpars> vector[VID]: V <<- V_bar + z_V*sigma_V ,
      
      V_bar ~ normal( 1 , 0.3 ) ,
      z_V[VID] ~ normal( 0.5 , 0.6 ) ,
      sigma_V ~ exponential( 2 )  
    ) , data = Model_M_list_fri , 
    iter = 1500 , warmup = 500 , chains = 6 , cores = 6 , 
    log_lik = TRUE , control = list( adapt_delta = 0.99 ) 
  ) }
Pre_model_MG_fri <- precis( Model_MG_fri , depth = 3 , prob = 0.90 , 
                            pars = c( "bMG" , "bA" , "bR" , "V" ) )
Pre_model_MG_fri

####1.3.4.2 Marriage type stratified by gender and village ----
{set.seed(123)
  Model_MVG_fri <- ulam(
    alist(
      Nominated ~ dpois( lambda ),
      log( lambda ) <- bGV[Gender,VID] + bGVM[Gender,VID] * ( Kidnap - 1 ) + 
        bA[Age] + bR[Pray] + # confounders
        V[VID] , # village intercepts  
      
      matrix[Gender,VID]: bGV ~ normal( 0 , 0.5 ) ,
      matrix[Gender,VID]: bGVM ~ normal( 0 , 0.5 ) ,
      
      bA[Age] ~ normal( 0 , 0.2 ) ,
      bR[Pray] ~ normal( 0 , 0.2 ) ,
      
      # define effects using other parameters
      transpars> vector[VID]: V <<- V_bar + z_V*sigma_V ,
      
      V_bar ~ normal( 1 , 0.3 ) ,
      z_V[VID] ~ normal( 0.5 , 0.6 ) ,
      sigma_V ~ exponential( 2 )  
    ) , data = Model_M_list_fri , 
    iter = 1500 , warmup = 500 , chains = 6 , cores = 6 , 
    log_lik = TRUE , control = list( adapt_delta = 0.99 ) 
  ) }
Pre_model_MVG_fri <- precis( Model_MVG_fri , depth = 3 , prob = 0.90 , 
                             pars = c( "bGV" , "bGVM" , "bA" , "bR" , "V" ) )
Pre_model_MVG_fri

##1.4 Figures ----
Post_model_MG_all <- extract.samples( Model_MG_all )
Post_model_MG_pre <- extract.samples( Model_MG_pre )
Post_model_MG_fin <- extract.samples( Model_MG_fin )
Post_model_MG_fri <- extract.samples( Model_MG_fri )

Post_model_MVG_all <- extract.samples( Model_MVG_all )
Post_model_MVG_pre <- extract.samples( Model_MVG_pre )
Post_model_MVG_fin <- extract.samples( Model_MVG_fin )
Post_model_MVG_fri <- extract.samples( Model_MVG_fri )

###1.4.1 All types: marriage type ----
####1.4.1.1 Posterior distribution of estimates ----
{
  par( mfrow = c( 1 , 2 ) , oma = c( 2.2 , 2.2 , 0.2 , 0 ) , mar = c( 1.4 , 1.4 , 0 , 0 ) )
  dens( Post_model_MG_all$bMG[,1,1] , 
        adj = 1 , 
        xlim = c( -1.5 , 1.5 ) ,
        ylim = c( 0 , 3 ) ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#df562c" , cex.axis = 1.2 )
  dens( Post_model_MG_all$bMG[,2,1] ,
        adj = 1 , 
        lwd = 3 , col = "#df562c" , 
        add = T )
  mtext( "Density" , side = 2 , line = 2.4 , cex = 1.5 )
  mtext( "Females" , side = 1 , line = 2.4 , cex = 1.5 )
  legend( x = -0.3 , y = 3.1 , 
          box.col = "white",
          legend = c( "Non-kidnapped female" , 
                      "Kidnapped female" , 
                      "Non-kidnapping male" , 
                      "Kidnapping male" ) , 
          lty = c( 3 , 1 , 3 , 1 ) ,  
          col = c( "#df562c" , "#df562c" , "#038766" , "#038766" ) , 
          lwd = 2 ,
          cex = 1.2 , 
          bty = "n" ,
          y.intersp = 0.8 ,
          x.intersp = 0.4 ,
          seg.len = 1.5  )
  
  dens( Post_model_MG_all$bMG[,1,2] ,
        adj = 1 , 
        xlim = c( -1.5 , 1.5 ) ,
        ylim = c( 0 , 3 ) ,
        yaxt = "n" , 
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#038766" , cex.axis = 1.2 )
  dens( Post_model_MG_all$bMG[,2,2] ,
        adj = 1 , 
        lwd = 3 , col = "#038766" , 
        add = T )
  mtext( "Males" , side = 1 , line = 2.4 , cex = 1.5 )
}

dev.off()

###1.4.2 All types: marriage type and village ----
####1.4.2.1 Posterior distribution of estimates ----
{
  par( mfrow = c( 2 , 2 ) , oma = c( 2.2 , 2.2 , 0 , 0 ) , mar = c( 1.4 , 1.4 , 0 , 0 ) )
  # village 1
  dens( Post_model_MVG_all$bGV[,1,1] ,
        adj = 1 , 
        xlim = c( -2 , 2 ) ,
        ylim = c( 0 , 2.5 ) ,
        xaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#df562c" , cex.axis = 1.2 )
  dens( Post_model_MVG_all$bGV[,1,1] + Post_model_MVG_all$bGVM[,1,1] ,
        adj = 1 , 
        lwd = 3 , col = "#df562c" , 
        add = T )
  mtext( "Village 1" , side = 2 , line = 2.4 , cex = 1.2 )
  
  dens( Post_model_MVG_all$bGV[,2,1] ,
        adj = 1 , 
        xlim = c( -2 , 2 ) ,
        ylim = c( 0 , 2.5 ) ,
        xaxt = "n" ,
        yaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#038766" , cex.axis = 1.2 )
  dens( Post_model_MVG_all$bGV[,2,1] + Post_model_MVG_all$bGVM[,2,1] ,
        adj = 1 , 
        lwd = 3 , col = "#038766" , 
        add = T )
  legend( x = -0.3 , y = 2.6 , 
          box.col = "white",
          legend = c( "Non-kidnapped female" , 
                      "Kidnapped female" , 
                      "Non-kidnapping male" , 
                      "Kidnapping male" ) , 
          lty = c( 3 , 1 , 3 , 1 ) ,  
          col = c( "#df562c" , "#df562c" , "#038766" , "#038766" ) , 
          lwd = 2 ,
          cex = 1.1 , 
          bty = "n" ,
          y.intersp = 1.0 ,
          x.intersp = 0.4 ,
          seg.len = 1.5  )
  
  # village 2
  dens( Post_model_MVG_all$bGV[,1,2] ,
        adj = 1 , 
        xlim = c( -2 , 2 ) ,
        ylim = c( 0 , 2.5 ) ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#df562c" , cex.axis = 1.2 )
  dens( Post_model_MVG_all$bGV[,1,2] + Post_model_MVG_all$bGVM[,1,2] ,
        adj = 1 , 
        lwd = 3 , col = "#df562c" , 
        add = T )
  mtext( "Village 2" , side = 2 , line = 2.4 , cex = 1.2 )
  mtext( "Females" , side = 1 , line = 2.4 , cex = 1.2 )
  
  dens( Post_model_MVG_all$bGV[,2,2] ,
        adj = 1 , 
        xlim = c( -2 , 2 ) ,
        ylim = c( 0 , 2.5 ) ,
        yaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#038766" , cex.axis = 1.2 )
  dens( Post_model_MVG_all$bGV[,2,2] + Post_model_MVG_all$bGVM[,2,2] ,
        adj = 1 , 
        lwd = 3 , col = "#038766" , 
        add = T )
  mtext( "Males" , side = 1 , line = 2.4 , cex = 1.2 )
}

dev.off()

####1.4.2.2 Estimate difference of kidnapping and non-kidnapping ----
tibble( Value = c( Post_model_MVG_all$bGVM[,1,1] , 
                   Post_model_MVG_all$bGVM[,2,1] ,
                   Post_model_MVG_all$bGVM[,1,2] , 
                   Post_model_MVG_all$bGVM[,2,2] ) ,
        Type = c( rep( "V1:Female" , 6000 ) ,
                  rep( "V1:Male" , 6000 ) ,
                  rep( "V2:Female" , 6000 ) ,
                  rep( "V2:Male" , 6000 ) ) ) %>% 
  mutate( Type = factor( Type , 
                         levels = c( "V2:Male" , "V2:Female" , "V1:Male" , "V1:Female" ) , 
                         labels = c( "V2:Male" , "V2:Female" , "V1:Male" , "V1:Female" ) ) ) %>% 
  ggplot( aes( x = Value , y = Type , 
               fill = Type , color = Type) ) +
  geom_vline( xintercept = 0, linetype = 2 , linewidth = 1 ,
              color = "dimgray" ) +
  stat_halfeye( .width = .90, height = 0.8 ) + 
  scale_fill_manual( values = alpha( c( "#038766" , "#df562c" , "#038766" , "#df562c" ) , 0.3 ) ) +
  scale_color_manual( values = c( "#038766" , "#df562c" , "#038766" , "#df562c" ) ) +
  scale_y_discrete( labels = c( "V2:Male" , "V2:Female" , "V1:Male" , "V1:Female" ) ) +
  xlab( "Posterior distribution of mean difference" ) +
  ylab( "Village:gender" ) +
  coord_cartesian( ylim = c( 1.5, 4.2 ) ) +
  scale_x_continuous( limits = c( -0.8 , 0.8 ) , breaks = c( -0.5 , 0 , 0.5 ) ) +
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

ggsave( filename = "BN_bMVG_diff_all.jpeg" , 
        width = 150 , height = 120 , units = "mm" , dpi = 300 )

###1.4.3 Marriage type across three types ----
####1.4.3.1 Posterior distribution of estimates ----
{
  par( mfrow = c( 3 , 2 ) , oma = c( 3.5 , 3.5 , 0 , 0 ) , mar = c( 0.3 , 0.3 , 0 , 0 ) )
  # Prestige
  dens( Post_model_MG_pre$bMG[,1,1] ,
        xlim = c( -2 , 2 ) ,
        ylim = c( 0 , 2.5 ) ,
        xaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#df562c" , cex.axis = 1.2 )
  dens( Post_model_MG_pre$bMG[,2,1] ,
        lwd = 3 , col = "#df562c" , 
        add = T )
  mtext( "Prestige" , side = 2 , line = 2.5 , cex = 1 )
  legend( x = -0.3 , y = 2.6 , 
          box.col = "white",
          legend = c( "Non-kidnapped female" , 
                      "Kidnapped female" , 
                      "Non-kidnapping male" , 
                      "Kidnapping male" ) , 
          lty = c( 3 , 1 , 3 , 1 ) ,  
          col = c( "#df562c" , "#df562c" , "#038766" , "#038766" ) , 
          lwd = 2 ,
          cex = 1.1 , 
          bty = "n" ,
          y.intersp = 0.8 ,
          x.intersp = 0.4 ,
          seg.len = 1.5  )
  
  dens( Post_model_MG_pre$bMG[,1,2] ,
        xlim = c( -2 , 2 ) ,
        ylim = c( 0 , 2.5 ) ,
        xaxt = "n" ,
        yaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#038766" , cex.axis = 1.2 )
  dens( Post_model_MG_pre$bMG[,2,2] ,
        lwd = 3 , col = "#038766" , 
        add = T )
  
  # Financial support
  dens( Post_model_MG_fin$bMG[,1,1] ,
        xlim = c( -2 , 2 ) ,
        ylim = c( 0 , 2.5 ) ,
        xaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#df562c" , cex.axis = 1.2 )
  dens( Post_model_MG_fin$bMG[,2,1] ,
        lwd = 3 , col = "#df562c" , 
        add = T )
  mtext( "Financial support" , side = 2 , line = 2.5 , cex = 1 )
  
  dens( Post_model_MG_fin$bMG[,1,2] ,
        xlim = c( -2 , 2 ) ,
        ylim = c( 0 , 2.5 ) ,
        xaxt = "n" ,
        yaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#038766" , cex.axis = 1.2 )
  dens( Post_model_MG_fin$bMG[,2,2] ,
        lwd = 3 , col = "#038766" , 
        add = T )
  
  # Friendships
  dens( Post_model_MG_fri$bMG[,1,1] ,
        xlim = c( -2 , 2 ) ,
        ylim = c( 0 , 2.5 ) ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#df562c" , cex.axis = 1.2 )
  dens( Post_model_MG_fri$bMG[,2,1] ,
        lwd = 3 , col = "#df562c" , 
        add = T )
  mtext( "Friendship" , side = 2 , line = 2.5 , cex = 1 )
  mtext( "Females" , side = 1 , line = 2.5 , cex = 1 )
  
  dens( Post_model_MG_fri$bMG[,1,2] ,
        xlim = c( -2 , 2 ) ,
        ylim = c( 0 , 2.5 ) ,
        yaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#038766" , cex.axis = 1.2 )
  dens( Post_model_MG_fri$bMG[,2,2] ,
        lwd = 3 , col = "#038766" , 
        add = T )
  mtext( "Males" , side = 1 , line = 2.5 , cex = 1 )
}

dev.off()

###1.4.4 Marriage type across three types and villages ----
####1.4.4.1 Posterior distribution of estimates ----
{
  par( mfrow = c( 3 , 4 ) , oma = c( 3.5 , 3.5 , 2.5 , 0.2 ) , mar = c( 0.3 , 0.3 , 0 , 0 ) )
  # Prestige 
  dens( Post_model_MVG_pre$bGV[,1,1] ,
        xlim = c( -2.5 , 2 ) ,
        ylim = c( 0 , 2 ) ,
        xaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#df562c" , cex.axis = 1.2 )
  dens( Post_model_MVG_pre$bGV[,1,1] + Post_model_MVG_pre$bGVM[,1,1] ,
        lwd = 3 , col = "#df562c" , 
        add = T )
  mtext( "Prestige" , side = 2 , line = 2.2 , cex = 1.2 )
  mtext( "Village 1" , side = 3 , line = 0.5 , cex = 1.2 )
  
  dens( Post_model_MVG_pre$bGV[,2,1] ,
        xlim = c( -2.5 , 2 ) ,
        ylim = c( 0 , 2 ) ,
        xaxt = "n" ,
        yaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#038766" , cex.axis = 1.2 )
  dens( Post_model_MVG_pre$bGV[,2,1] + Post_model_MVG_pre$bGVM[,2,1] ,
        lwd = 3 , col = "#038766" , 
        add = T )
  mtext( "Village 1" , side = 3 , line = 0.5 , cex = 1.2 )
  
  dens( Post_model_MVG_pre$bGV[,1,2] ,
        xlim = c( -2.5 , 2 ) ,
        ylim = c( 0 , 2 ) ,
        xaxt = "n" ,
        yaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#df562c" , cex.axis = 1.2 )
  dens( Post_model_MVG_pre$bGV[,1,2] + Post_model_MVG_pre$bGVM[,1,2] ,
        lwd = 3 , col = "#df562c" , 
        add = T )
  mtext( "Village 2" , side = 3 , line = 0.5 , cex = 1.2 )
  
  dens( Post_model_MVG_pre$bGV[,2,2] ,
        xlim = c( -2.5 , 2 ) ,
        ylim = c( 0 , 2 ) ,
        xaxt = "n" ,
        yaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#038766" , cex.axis = 1.2 )
  dens( Post_model_MVG_pre$bGV[,2,2] + Post_model_MVG_pre$bGVM[,2,2] ,
        lwd = 3 , col = "#038766" , 
        add = T )
  mtext( "Village 2" , side = 3 , line = 0.5 , cex = 1.2 )
  legend( x = -0.9 , y = 2.10 , 
          box.col = "white",
          legend = c( "Non-kidnapped female" , 
                      "Kidnapped female" , 
                      "Non-kidnapping male" , 
                      "Kidnapping male" ) , 
          lty = c( 3 , 1 , 3 , 1 ) ,  
          col = c( "#df562c" , "#df562c" , "#038766" , "#038766" ) , 
          lwd = 2 ,
          cex = 1 , 
          bty = "n" ,
          y.intersp = 0.8 ,
          x.intersp = 0.4 ,
          seg.len = 1.2  )
  
  # Financial support 
  dens( Post_model_MVG_fin$bGV[,1,1] ,
        xlim = c( -2.5 , 2 ) ,
        ylim = c( 0 , 2 ) ,
        xaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#df562c" , cex.axis = 1.2 )
  dens( Post_model_MVG_fin$bGV[,1,1] + Post_model_MVG_fin$bGVM[,1,1] ,
        lwd = 3 , col = "#df562c" , 
        add = T )
  mtext( "Financial support" , side = 2 , line = 2.4 , cex = 1.2 )
  
  dens( Post_model_MVG_fin$bGV[,2,1] ,
        xlim = c( -2.5 , 2 ) ,
        ylim = c( 0 , 2 ) ,
        xaxt = "n" ,
        yaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#038766" , cex.axis = 1.2 )
  dens( Post_model_MVG_fin$bGV[,2,1] + Post_model_MVG_fin$bGVM[,2,1] ,
        lwd = 3 , col = "#038766" , 
        add = T )
  
  dens( Post_model_MVG_fin$bGV[,1,2] ,
        xlim = c( -2.5 , 2 ) ,
        ylim = c( 0 , 2 ) ,
        xaxt = "n" ,
        yaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#df562c" , cex.axis = 1.2 )
  dens( Post_model_MVG_fin$bGV[,1,2] + Post_model_MVG_fin$bGVM[,1,2] ,
        lwd = 3 , col = "#df562c" , 
        add = T )
  
  dens( Post_model_MVG_fin$bGV[,2,2] ,
        xlim = c( -2.5 , 2 ) ,
        ylim = c( 0 , 2 ) ,
        xaxt = "n" ,
        yaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#038766" , cex.axis = 1.2 )
  dens( Post_model_MVG_fin$bGV[,2,2] + Post_model_MVG_fin$bGVM[,2,2] ,
        lwd = 3 , col = "#038766" , 
        add = T )
  
  # Friendship 
  dens( Post_model_MVG_fri$bGV[,1,1] ,
        xlim = c( -2.5 , 2 ) ,
        ylim = c( 0 , 2 ) ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#df562c" , cex.axis = 1.2 )
  dens( Post_model_MVG_fri$bGV[,1,1] + Post_model_MVG_fri$bGVM[,1,1] ,
        lwd = 3 , col = "#df562c" , 
        add = T )
  mtext( "Friendship" , side = 2 , line = 2.4 , cex = 1.2 )
  mtext( "Females" , side = 1 , line = 2.4 , cex = 1.2 )
  
  dens( Post_model_MVG_fri$bGV[,2,1] ,
        xlim = c( -2.5 , 2 ) ,
        ylim = c( 0 , 2 ) ,
        yaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#038766" , cex.axis = 1.2 )
  dens( Post_model_MVG_fri$bGV[,2,1] + Post_model_MVG_fri$bGVM[,2,1] ,
        lwd = 3 , col = "#038766" , 
        add = T )
  mtext( "Males" , side = 1 , line = 2.4 , cex = 1.2 )
  
  dens( Post_model_MVG_fri$bGV[,1,2] ,
        xlim = c( -2.5 , 2 ) ,
        ylim = c( 0 , 2 ) ,
        yaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#df562c" , cex.axis = 1.2 )
  dens( Post_model_MVG_fri$bGV[,1,2] + Post_model_MVG_fri$bGVM[,1,2] ,
        lwd = 3 , col = "#df562c" , 
        add = T )
  mtext( "Females" , side = 1 , line = 2.4 , cex = 1.2 )
  
  dens( Post_model_MVG_fri$bGV[,2,2] ,
        xlim = c( -2.5 , 2 ) ,
        ylim = c( 0 , 2 ) ,
        yaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#038766" , cex.axis = 1.2 )
  dens( Post_model_MVG_fri$bGV[,2,2] + Post_model_MVG_fri$bGVM[,2,2] ,
        lwd = 3 , col = "#038766" , 
        add = T )
  mtext( "Males" , side = 1 , line = 2.4 , cex = 1.2 )
  
}

dev.off()

####1.4.4.2 Estimate difference of kidnapping and non-kidnapping ----
tibble( Value = c( Post_model_MVG_pre$bGVM[,1,1] , 
                   Post_model_MVG_pre$bGVM[,2,1] ,
                   Post_model_MVG_pre$bGVM[,1,2] , 
                   Post_model_MVG_pre$bGVM[,2,2] , 
                   
                   Post_model_MVG_fin$bGVM[,1,1] , 
                   Post_model_MVG_fin$bGVM[,2,1] ,
                   Post_model_MVG_fin$bGVM[,1,2] , 
                   Post_model_MVG_fin$bGVM[,2,2] ,
                   
                   Post_model_MVG_fri$bGVM[,1,1] , 
                   Post_model_MVG_fri$bGVM[,2,1] ,
                   Post_model_MVG_fri$bGVM[,1,2] , 
                   Post_model_MVG_fri$bGVM[,2,2] ) , 
        Type = c( rep( "Prestige" , 6000 ) ,
                  rep( "Prestige" , 6000 ) , 
                  rep( "Prestige" , 6000 ) ,
                  rep( "Prestige" , 6000 ) , 
                  
                  rep( "Financial support" , 6000 ) ,
                  rep( "Financial support" , 6000 ) , 
                  rep( "Financial support" , 6000 ) ,
                  rep( "Financial support" , 6000 ) , 
                  
                  rep( "Friendship" , 6000 ) ,
                  rep( "Friendship" , 6000 ) ,
                  rep( "Friendship" , 6000 ) ,
                  rep( "Friendship" , 6000 ) ) ,
        Gender = c( rep( "V1:Female" , 6000 ) ,
                    rep( "V1:Male" , 6000 ) ,
                    rep( "V2:Female" , 6000 ) ,
                    rep( "V2:Male" , 6000 ) ,
                    
                    rep( "V1:Female" , 6000 ) ,
                    rep( "V1:Male" , 6000 ) ,
                    rep( "V2:Female" , 6000 ) ,
                    rep( "V2:Male" , 6000 ) ,
                    
                    rep( "V1:Female" , 6000 ) ,
                    rep( "V1:Male" , 6000 ) ,
                    rep( "V2:Female" , 6000 ) ,
                    rep( "V2:Male" , 6000 ) ) ) %>% 
  mutate( Type = factor( Type , 
                         levels = c( "Prestige" , "Financial support" , "Friendship" ) , 
                         labels = c( "Prestige" , "Financial support" , "Friendship" ) ) , 
          Gender = factor( Gender , 
                           levels = c( "V2:Male" , "V2:Female" , "V1:Male" , "V1:Female" ) , 
                           labels = c( "V2:Male" , "V2:Female" , "V1:Male" , "V1:Female" ) ) ) %>% 
  ggplot( aes( x = Value , y = Gender , 
               fill = Gender , color = Gender) ) +
  facet_wrap( ~ Type ) +
  geom_vline( xintercept = 0, linetype = 2 , linewidth = 1 ,
              color = "dimgray" ) +
  stat_halfeye( .width = .90, height = 0.8 ) + 
  scale_fill_manual( values = alpha( c( "#038766" , "#df562c" , "#038766" , "#df562c" ) , 0.3 ) ) +
  scale_color_manual( values = c( "#038766" , "#df562c" , "#038766" , "#df562c" ) ) +
  scale_y_discrete( labels = c( "V2:Male" , "V2:Female" , "V1:Male" , "V1:Female" ) ) +
  xlab( "Posterior distribution of mean difference" ) +
  ylab( "Village:gender" ) +
  coord_cartesian( ylim = c( 1.5, 4.2 ) ) +
  scale_x_continuous( limits = c( -1.2 , 1.2 ) , breaks = c( -1 , 0 , 1 ) ) +
  theme(plot.margin = margin(10, 10, 10, 20),
        strip.background = element_rect(color = "black", fill = "white") ,
        strip.text.x = element_text(size = 14),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "Gainsboro"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black",fill = NA),
        panel.background = element_blank(),
        plot.title = element_text(size = 16),
        legend.position = "none",
        axis.title.x = element_text(size = 14,
                                    margin = margin(t = 0.3, r = 0, b = 0, l = 0,unit = "cm")),
        axis.text.x = element_text(colour = "black",size = 12,
                                   margin = margin(t = 0.1, r = 0, b = 0, l = 0,unit = "cm")),
        axis.title.y = element_text(size = 14,
                                    margin = margin(t = 0, r = 0.5, b = 0, l = 0,unit = "cm")),
        axis.text.y = element_text(colour = "black",size = 12,
                                   margin = margin(t = 0, r = 0.2, b = 0, l = 0,unit = "cm")),
        legend.title = element_text( size = 12,face = "bold"),
        legend.text = element_text( size = 12,face = "bold") )

ggsave( filename = "BN_bMVG_diff_three_types.jpeg" , 
        width = 200 , height = 160 , units = "mm" , dpi = 300 )

###1.4.5 Figure 1: Estimate difference of kidnapping and non-kidnapping (all + three types) ----
tibble( Value = c( Post_model_MG_all$bMG[,2,1] - Post_model_MG_all$bMG[,1,1] , 
                   Post_model_MG_all$bMG[,2,2] - Post_model_MG_all$bMG[,1,2] ,
                   
                   Post_model_MG_pre$bMG[,2,1] - Post_model_MG_pre$bMG[,1,1] , 
                   Post_model_MG_pre$bMG[,2,2] - Post_model_MG_pre$bMG[,1,2] , 
                   
                   Post_model_MG_fin$bMG[,2,1] - Post_model_MG_fin$bMG[,1,1] , 
                   Post_model_MG_fin$bMG[,2,2] - Post_model_MG_fin$bMG[,1,2] ,
                   
                   Post_model_MG_fri$bMG[,2,1] - Post_model_MG_fri$bMG[,1,1] , 
                   Post_model_MG_fri$bMG[,2,2] - Post_model_MG_fri$bMG[,1,2] ) , 
        Type = c( rep( "a. all" , 6000 ) ,
                  rep( "a. all" , 6000 ) , 
                  rep( "b. prestige" , 6000 ) ,
                  rep( "b. prestige" , 6000 ) , 
                  rep( "c. financial support" , 6000 ) ,
                  rep( "c. financial support" , 6000 ) , 
                  rep( "d. friendship" , 6000 ) ,
                  rep( "d. friendship" , 6000 ) ) ,
        Gender = c( rep( "Female" , 6000 ) ,
                    rep( "Male" , 6000 ) , 
                    rep( "Female" , 6000 ) ,
                    rep( "Male" , 6000 ) , 
                    rep( "Female" , 6000 ) ,
                    rep( "Male" , 6000 ) , 
                    rep( "Female" , 6000 ) ,
                    rep( "Male" , 6000 ) ) ) %>% 
  mutate( Type = factor( Type , 
                         levels = c( "a. all" , "b. prestige" , "c. financial support" , "d. friendship" ) , 
                         labels = c( "a. all" , "b. prestige" , "c. financial support" , "d. friendship" ) ) , 
          Gender = factor( Gender , 
                           levels = c( "Male" , "Female" ) , 
                           labels = c( "Male" , "Female" ) ) ) %>% 
  ggplot( aes( x = Value , y = Gender , 
               fill = Gender , color = Gender) ) +
  facet_wrap( ~ Type , nrow = 1 ) +
  geom_vline( xintercept = 0, linetype = 2 , linewidth = 1 ,
              color = "dimgray" ) +
  stat_halfeye( .width = .90, height = 0.8 ) + 
  scale_fill_manual( values = alpha( c( "#038766" , "#df562c" ) , 0.3 ) ) +
  scale_color_manual( values = c( "#038766" , "#df562c" ) ) +
  scale_y_discrete( labels = c( "Male" , "Female" ) ) +
  xlab( "Posterior distribution of mean difference" ) +
  ylab( "Gender" ) +
  coord_cartesian( ylim = c( 1.5, 2.2 ) ) +
  scale_x_continuous( limits = c( -0.5 , 1.1 ) , breaks = c( -0.5 , 0 , 0.5 , 1 ) ) +
  theme(plot.margin = margin(10, 10, 10, 20),
        strip.background = element_rect(color = "black", fill = "white") ,
        strip.text.x = element_text(size = 16),
        #axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "Gainsboro"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black",fill = NA),
        panel.background = element_blank(),
        plot.title = element_text(size = 16),
        legend.position = "none",
        axis.title.x = element_text(size = 18,
                                    margin = margin(t = 0.2, r = 0, b = 0, l = 0,unit = "cm")),
        axis.text.x = element_text(colour = "black",size = 14,
                                   margin = margin(t = 0.1, r = 0, b = 0, l = 0,unit = "cm")),
        axis.title.y = element_text(size = 16,
                                    margin = margin(t = 0, r = 0.2, b = 0, l = 0,unit = "cm")),
        axis.text.y = element_text(colour = "black",size = 14,
                                   margin = margin(t = 0, r = 0.1, b = 0, l = 0,unit = "cm")),
        legend.title = element_text( size = 12,face = "bold"),
        legend.text = element_text( size = 12,face = "bold") )

ggsave( filename = "BN_bMG_diff_all&three_types.jpeg" , 
        width = 250 , height = 70 , units = "mm" , dpi = 300 )

##1.5 Outputs ----
###1.5.1 Marriage type stratified by gender ----
####1.5.1.1 Estimates ----
Pre_MG_BN_output <- data.frame( Mean = c( Pre_model_MG_all$mean , 
                                          Pre_model_MG_pre$mean , 
                                          Pre_model_MG_fin$mean ,
                                          Pre_model_MG_fri$mean ) ,
                                CI5 = c( Pre_model_MG_all$`5%` , 
                                         Pre_model_MG_pre$`5%` ,
                                         Pre_model_MG_fin$`5%` ,
                                         Pre_model_MG_fri$`5%` ) ,
                                CI95 = c( Pre_model_MG_all$`95%` , 
                                          Pre_model_MG_pre$`95%` ,
                                          Pre_model_MG_fin$`95%` ,
                                          Pre_model_MG_fri$`95%` ) ,
                                Type = rep( c( "All" , "Prestige" , "Financial support" , "Friendship" ) , each = 12 ) ,
                                Variable = rep( c( "Non-kidnapped female" , 
                                                   "Kidnapped female" , 
                                                   "Non-kidnapping male" , 
                                                   "Kidnapping male" , 
                                                   "Age: <40" , "Age: 40-49" , 
                                                   "Age: 50-59" , "Age: >=60" , 
                                                   "Pray: seldom" , "Pray: often" , 
                                                   "Village 1" , "Village 2" ) , 4 ) ) %>% 
  mutate( Type = factor( Type , 
                         levels = c( "All" , "Prestige" , "Financial support" , "Friendship" ) ,
                         labels = c( "All" , "Prestige" , "Financial support" , "Friendship" ) ) ,
          Variable = factor( Variable , 
                             levels = c( "Non-kidnapped female" , 
                                         "Kidnapped female" , 
                                         "Non-kidnapping male" , 
                                         "Kidnapping male" , 
                                         "Age: <40" , "Age: 40-49" , 
                                         "Age: 50-59" , "Age: >=60" , 
                                         "Pray: seldom" , "Pray: often" ,
                                         "Village 1" , "Village 2" ) ,
                             labels = c( "Non-kidnapped female" , 
                                         "Kidnapped female" , 
                                         "Non-kidnapping male" , 
                                         "Kidnapping male" , 
                                         "Age: <40" , "Age: 40-49" , 
                                         "Age: 50-59" , "Age: >=60" , 
                                         "Pray: seldom" , "Pray: often" ,
                                         "Village 1" , "Village 2" ) ) ,
          Model = "Social relationship nomination" , 
          "Mean [CI]" = paste( format( round( Mean , 2 ), nsmall = 2 ) , 
                               " [" , 
                               format( round( CI5 , 2 ), nsmall = 2 ) , 
                               ", " , 
                               format( round( CI95 , 2 ), nsmall = 2 ) , 
                               "]", 
                               sep = "") )

MG_all_output <- tibble( Variable =  c( "Non-kidnapped female" , 
                                        "Kidnapped female" , 
                                        "Non-kidnapping male" , 
                                        "Kidnapping male" , 
                                        "Age: <40" , "Age: 40-49" , 
                                        "Age: 50-59" , "Age: >=60" , 
                                        "Pray: seldom" , "Pray: often" ,
                                        "Village 1" , "Village 2" ) ,
                         Type = "All" ) %>% 
  left_join( Pre_MG_BN_output[ , c( 4 , 5 , 7 ) ] ,
             by = c( "Type" , "Variable" ) ) %>% 
  dplyr::rename( `All` = "Mean [CI]" ) %>% 
  mutate( Type = "Prestige" ) %>% 
  left_join( Pre_MG_BN_output[ , c( 4 , 5 , 7 ) ] ,
             by = c( "Type" , "Variable" ) ) %>% 
  dplyr::rename( `Prestige` = "Mean [CI]" ) %>% 
  mutate( Type = "Financial support" ) %>% 
  left_join( Pre_MG_BN_output[ , c( 4 , 5 , 7 ) ] ,
             by = c( "Type" , "Variable" ) ) %>% 
  dplyr::rename( `Financial support` = "Mean [CI]" ) %>% 
  mutate( Type = "Friendship" ) %>% 
  left_join( Pre_MG_BN_output[ , c( 4 , 5 , 7 ) ] ,
             by = c( "Type" , "Variable" ) ) %>% 
  dplyr::rename( `Friendship` = "Mean [CI]" ) %>% 
  select( -Type ) %>% 
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
              columns = c( "All" , "Prestige" , "Financial support" , "Friendship" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( "Variable" ~ px( 170 ) , 
              everything() ~ px( 150 ) )

MG_all_output

####1.5.1.2 Estimate differences ----
tibble( Type = c( rep( "All" , 2 ) ,
                  rep( "Prestige" , 2 ) ,
                  rep( "Financial support" , 2 ) ,
                  rep( "Friendship"  , 2 ) ) ,
        Gender = c( rep( c( "Woman" , "Man" ) , 4 ) ) ,
        bM_diff = c( sprintf("%0.2f" , mean( Post_model_MG_all$bMG[,2,1] - Post_model_MG_all$bMG[,1,1] ) ) , 
                     sprintf("%0.2f" , mean( Post_model_MG_all$bMG[,2,2] - Post_model_MG_all$bMG[,1,2] ) ) ,
                     
                     sprintf("%0.2f" , mean( Post_model_MG_pre$bMG[,2,1] - Post_model_MG_pre$bMG[,1,1] ) ) , 
                     sprintf("%0.2f" , mean( Post_model_MG_pre$bMG[,2,2] - Post_model_MG_pre$bMG[,1,2] ) ) , 
                     
                     sprintf("%0.2f" , mean( Post_model_MG_fin$bMG[,2,1] - Post_model_MG_fin$bMG[,1,1] ) ) , 
                     sprintf("%0.2f" , mean( Post_model_MG_fin$bMG[,2,2] - Post_model_MG_fin$bMG[,1,2] ) ) , 
                     
                     sprintf("%0.2f" , mean( Post_model_MG_fri$bMG[,2,1] - Post_model_MG_fri$bMG[,1,1] ) ) , 
                     sprintf("%0.2f" , mean( Post_model_MG_fri$bMG[,2,2] - Post_model_MG_fri$bMG[,1,2] ) ) ) ,
        
        bM_diff_L = c( sprintf("%0.2f" , PI( Post_model_MG_all$bMG[,2,1] - Post_model_MG_all$bMG[,1,1] , prob = 0.90 )[1] ) , 
                       sprintf("%0.2f" , PI( Post_model_MG_all$bMG[,2,2] - Post_model_MG_all$bMG[,1,2] , prob = 0.90 )[1] ) ,  
                       
                       sprintf("%0.2f" , PI( Post_model_MG_pre$bMG[,2,1] - Post_model_MG_pre$bMG[,1,1] , prob = 0.90 )[1] ) , 
                       sprintf("%0.2f" , PI( Post_model_MG_pre$bMG[,2,2] - Post_model_MG_pre$bMG[,1,2] , prob = 0.90 )[1] ) , 
                       
                       sprintf("%0.2f" , PI( Post_model_MG_fin$bMG[,2,1] - Post_model_MG_fin$bMG[,1,1] , prob = 0.90 )[1] ) , 
                       sprintf("%0.2f" , PI( Post_model_MG_fin$bMG[,2,2] - Post_model_MG_fin$bMG[,1,2] , prob = 0.90 )[1] ) , 
                       
                       sprintf("%0.2f" , PI( Post_model_MG_fri$bMG[,2,1] - Post_model_MG_fri$bMG[,1,1] , prob = 0.90 )[1] ) , 
                       sprintf("%0.2f" , PI( Post_model_MG_fri$bMG[,2,2] - Post_model_MG_fri$bMG[,1,2] , prob = 0.90 )[1] ) ) ,
        
        bM_diff_H = c( sprintf("%0.2f" , PI( Post_model_MG_all$bMG[,2,1] - Post_model_MG_all$bMG[,1,1] , prob = 0.90 )[2] ) , 
                       sprintf("%0.2f" , PI( Post_model_MG_all$bMG[,2,2] - Post_model_MG_all$bMG[,1,2] , prob = 0.90 )[2] ) ,  
                       
                       sprintf("%0.2f" , PI( Post_model_MG_pre$bMG[,2,1] - Post_model_MG_pre$bMG[,1,1] , prob = 0.90 )[2] ) , 
                       sprintf("%0.2f" , PI( Post_model_MG_pre$bMG[,2,2] - Post_model_MG_pre$bMG[,1,2] , prob = 0.90 )[2] ) , 
                       
                       sprintf("%0.2f" , PI( Post_model_MG_fin$bMG[,2,1] - Post_model_MG_fin$bMG[,1,1] , prob = 0.90 )[2] ) , 
                       sprintf("%0.2f" , PI( Post_model_MG_fin$bMG[,2,2] - Post_model_MG_fin$bMG[,1,2] , prob = 0.90 )[2] ) , 
                       
                       sprintf("%0.2f" , PI( Post_model_MG_fri$bMG[,2,1] - Post_model_MG_fri$bMG[,1,1] , prob = 0.90 )[2] ) , 
                       sprintf("%0.2f" , PI( Post_model_MG_fri$bMG[,2,2] - Post_model_MG_fri$bMG[,1,2] , prob = 0.90 )[2] ) ) ,
        `bM_diff[90%CI]` = paste( bM_diff , "[" , bM_diff_L , ", " , bM_diff_H , "]") , 
        Pro_bM_diff = c( sprintf( "%0.4f" , length( which( ( Post_model_MG_all$bMG[,2,1] - Post_model_MG_all$bMG[,1,1] ) > 0 ) ) / 6000 ) , 
                         sprintf( "%0.4f" , length( which( ( Post_model_MG_all$bMG[,2,2] - Post_model_MG_all$bMG[,1,2] ) > 0 ) ) / 6000 ) , 
                         
                         sprintf( "%0.4f" , length( which( ( Post_model_MG_pre$bMG[,2,1] - Post_model_MG_pre$bMG[,1,1] ) > 0 ) ) / 6000 ) , 
                         sprintf( "%0.4f" , length( which( ( Post_model_MG_pre$bMG[,2,2] - Post_model_MG_pre$bMG[,1,2] ) > 0 ) ) / 6000 ) ,
                         
                         sprintf( "%0.4f" , length( which( ( Post_model_MG_fin$bMG[,2,1] - Post_model_MG_fin$bMG[,1,1] ) > 0 ) ) / 6000 ) , 
                         sprintf( "%0.4f" , length( which( ( Post_model_MG_fin$bMG[,2,2] - Post_model_MG_fin$bMG[,1,2] ) > 0 ) ) / 6000 ) ,
                         
                         sprintf( "%0.4f" , length( which( ( Post_model_MG_fri$bMG[,2,1] - Post_model_MG_fri$bMG[,1,1] ) > 0 ) ) / 6000 ) , 
                         sprintf( "%0.4f" , length( which( ( Post_model_MG_fri$bMG[,2,2] - Post_model_MG_fri$bMG[,1,2] ) > 0 ) ) / 6000 ) ) ) %>% 
  mutate( Pro_bM_diff = paste( round( as.numeric( Pro_bM_diff ) * 100 , 2 ) , "%" ) ) %>% 
  select( Type , Gender , `bM_diff[90%CI]` , Pro_bM_diff ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels(everything() ) ) %>% 
  tab_style( style = list( cell_text(font = "Times New Roman") ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "center",
              columns = c( Type , Gender , `bM_diff[90%CI]` , Pro_bM_diff ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( everything() ~ px( 150 ) )

###1.5.2 Marriage type stratified by gender and village ----
####1.5.2.1 Estimates ----
Pre_MVG_BN_output <- data.frame( Mean = c( Pre_model_MVG_all$mean , 
                                           Pre_model_MVG_pre$mean , 
                                           Pre_model_MVG_fin$mean ,
                                           Pre_model_MVG_fri$mean ) ,
                                 CI5 = c( Pre_model_MVG_all$`5%` , 
                                          Pre_model_MVG_pre$`5%` ,
                                          Pre_model_MVG_fin$`5%` ,
                                          Pre_model_MVG_fri$`5%` ) ,
                                 CI95 = c( Pre_model_MVG_all$`95%` , 
                                           Pre_model_MVG_pre$`95%` ,
                                           Pre_model_MVG_fin$`95%` ,
                                           Pre_model_MVG_fri$`95%` ) ,
                                 Type = rep( c( "All" , "Prestige" , "Financial support" , "Friendship" ) , each = 16 ) ,
                                 Variable = rep( c( "Non-kidnapped: V1 female" , 
                                                    "Non-kidnapping: V1 male" , 
                                                    "Non-kidnapped: V2 female" , 
                                                    "Non-kidnapping: V2 male" , 
                                                    
                                                    "Marriage difference: V1 female" , 
                                                    "Marriage difference: V1 male" , 
                                                    "Marriage difference: V2 female" , 
                                                    "Marriage difference: V2 male" , 
                                                    
                                                    "Age: <40" , "Age: 40-49" , 
                                                    "Age: 50-59" , "Age: >=60" , 
                                                    "Pray: seldom" , "Pray: often" ,
                                                    "Village 1" , "Village 2" ) , 4 ) ) %>% 
  mutate( Type = factor( Type , 
                         levels = c( "All" , "Prestige" , "Financial support" , "Friendship" ) ,
                         labels = c( "All" , "Prestige" , "Financial support" , "Friendship" ) ) ,
          Variable = factor( Variable , 
                             levels = c( "Non-kidnapped: V1 female" , 
                                         "Non-kidnapping: V1 male" , 
                                         "Non-kidnapped: V2 female" , 
                                         "Non-kidnapping: V2 male" , 
                                         
                                         "Marriage difference: V1 female" , 
                                         "Marriage difference: V1 male" , 
                                         "Marriage difference: V2 female" , 
                                         "Marriage difference: V2 male" , 
                                         
                                         "Age: <40" , "Age: 40-49" , 
                                         "Age: 50-59" , "Age: >=60" , 
                                         "Pray: seldom" , "Pray: often" ,
                                         "Village 1" , "Village 2" ) ,
                             labels = c( "Non-kidnapped: V1 female" , 
                                         "Non-kidnapping: V1 male" , 
                                         "Non-kidnapped: V2 female" , 
                                         "Non-kidnapping: V2 male" , 
                                         
                                         "Marriage difference: V1 female" , 
                                         "Marriage difference: V1 male" , 
                                         "Marriage difference: V2 female" , 
                                         "Marriage difference: V2 male" ,
                                         
                                         "Age: <40" , "Age: 40-49" , 
                                         "Age: 50-59" , "Age: >=60" , 
                                         "Pray: seldom" , "Pray: often" ,
                                         "Village 1" , "Village 2" ) ) ,
          Model = "Social relationship nomination" , 
          "Mean [CI]" = paste( format( round( Mean , 2 ), nsmall = 2 ) , 
                               " [" , 
                               format( round( CI5 , 2 ), nsmall = 2 ) , 
                               ", " , 
                               format( round( CI95 , 2 ), nsmall = 2 ) , 
                               "]", 
                               sep = "") )

MVG_all_output <- tibble( Variable =  c( "Non-kidnapped: V1 female" , 
                                         "Non-kidnapping: V1 male" , 
                                         "Non-kidnapped: V2 female" , 
                                         "Non-kidnapping: V2 male" , 
                                         
                                         "Marriage difference: V1 female" , 
                                         "Marriage difference: V1 male" , 
                                         "Marriage difference: V2 female" , 
                                         "Marriage difference: V2 male" , 
                                         
                                         "Age: <40" , "Age: 40-49" , 
                                         "Age: 50-59" , "Age: >=60" , 
                                         "Pray: seldom" , "Pray: often" ,
                                         "Village 1" , "Village 2" ) ,
                          Type = "All" ) %>% 
  left_join( Pre_MVG_BN_output[ , c( 4 , 5 , 7 ) ] ,
             by = c( "Type" , "Variable" ) ) %>% 
  dplyr::rename( `All` = "Mean [CI]" ) %>% 
  mutate( Type = "Prestige" ) %>% 
  left_join( Pre_MVG_BN_output[ , c( 4 , 5 , 7 ) ] ,
             by = c( "Type" , "Variable" ) ) %>% 
  dplyr::rename( `Prestige` = "Mean [CI]" ) %>% 
  mutate( Type = "Financial support" ) %>% 
  left_join( Pre_MVG_BN_output[ , c( 4 , 5 , 7 ) ] ,
             by = c( "Type" , "Variable" ) ) %>% 
  dplyr::rename( `Financial support` = "Mean [CI]" ) %>% 
  mutate( Type = "Friendship" ) %>% 
  left_join( Pre_MVG_BN_output[ , c( 4 , 5 , 7 ) ] ,
             by = c( "Type" , "Variable" ) ) %>% 
  dplyr::rename( `Friendship` = "Mean [CI]" ) %>% 
  select( -Type ) %>% 
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
              columns = c( "All" , "Prestige" , "Financial support" , "Friendship" ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( "Variable" ~ px( 170 ) , 
              everything() ~ px( 150 ) )

MVG_all_output

####1.5.2.2 Estimate differences ----
tibble( Type = c( rep( "All" , 4 ) ,
                  rep( "Prestige" , 4 ) ,
                  rep( "Financial support" , 4 ) ,
                  rep( "Friendship"  , 4 ) ) ,
        Village = c( rep( c( "V1" , "V1" , "V2" , "V2" ) , 4 ) ) ,
        Gender = c( rep( c( "Female" , "Male" ) , 8 ) ) ,
        bGVM = c( sprintf("%0.2f" , mean( Post_model_MVG_all$bGVM[,1,1] ) ) , 
                  sprintf("%0.2f" , mean( Post_model_MVG_all$bGVM[,2,1] ) ) ,
                  sprintf("%0.2f" , mean( Post_model_MVG_all$bGVM[,1,2] ) ) , 
                  sprintf("%0.2f" , mean( Post_model_MVG_all$bGVM[,2,2] ) ) ,
                  
                  sprintf("%0.2f" , mean( Post_model_MVG_pre$bGVM[,1,1] ) ) , 
                  sprintf("%0.2f" , mean( Post_model_MVG_pre$bGVM[,2,1] ) ) ,
                  sprintf("%0.2f" , mean( Post_model_MVG_pre$bGVM[,1,2] ) ) , 
                  sprintf("%0.2f" , mean( Post_model_MVG_pre$bGVM[,2,2] ) ) ,
                  
                  sprintf("%0.2f" , mean( Post_model_MVG_fin$bGVM[,1,1] ) ) , 
                  sprintf("%0.2f" , mean( Post_model_MVG_fin$bGVM[,2,1] ) ) ,
                  sprintf("%0.2f" , mean( Post_model_MVG_fin$bGVM[,1,2] ) ) , 
                  sprintf("%0.2f" , mean( Post_model_MVG_fin$bGVM[,2,2] ) ) ,
                  
                  sprintf("%0.2f" , mean( Post_model_MVG_fri$bGVM[,1,1] ) ) , 
                  sprintf("%0.2f" , mean( Post_model_MVG_fri$bGVM[,2,1] ) ) ,
                  sprintf("%0.2f" , mean( Post_model_MVG_fri$bGVM[,1,2] ) ) , 
                  sprintf("%0.2f" , mean( Post_model_MVG_fri$bGVM[,2,2] ) ) ) ,
        
        bGVM_L = c( sprintf("%0.2f" , PI( Post_model_MVG_all$bGVM[,1,1] , prob = 0.90 )[1] ) , 
                    sprintf("%0.2f" , PI( Post_model_MVG_all$bGVM[,2,1] , prob = 0.90 )[1] ) ,  
                    sprintf("%0.2f" , PI( Post_model_MVG_all$bGVM[,1,2] , prob = 0.90 )[1] ) , 
                    sprintf("%0.2f" , PI( Post_model_MVG_all$bGVM[,2,2] , prob = 0.90 )[1] ) ,  
                    
                    sprintf("%0.2f" , PI( Post_model_MVG_pre$bGVM[,1,1] , prob = 0.90 )[1] ) , 
                    sprintf("%0.2f" , PI( Post_model_MVG_pre$bGVM[,2,1] , prob = 0.90 )[1] ) ,  
                    sprintf("%0.2f" , PI( Post_model_MVG_pre$bGVM[,1,2] , prob = 0.90 )[1] ) , 
                    sprintf("%0.2f" , PI( Post_model_MVG_pre$bGVM[,2,2] , prob = 0.90 )[1] ) ,  
                    
                    sprintf("%0.2f" , PI( Post_model_MVG_fin$bGVM[,1,1] , prob = 0.90 )[1] ) , 
                    sprintf("%0.2f" , PI( Post_model_MVG_fin$bGVM[,2,1] , prob = 0.90 )[1] ) ,  
                    sprintf("%0.2f" , PI( Post_model_MVG_fin$bGVM[,1,2] , prob = 0.90 )[1] ) , 
                    sprintf("%0.2f" , PI( Post_model_MVG_fin$bGVM[,2,2] , prob = 0.90 )[1] ) ,  
                    
                    sprintf("%0.2f" , PI( Post_model_MVG_fri$bGVM[,1,1] , prob = 0.90 )[1] ) , 
                    sprintf("%0.2f" , PI( Post_model_MVG_fri$bGVM[,2,1] , prob = 0.90 )[1] ) ,  
                    sprintf("%0.2f" , PI( Post_model_MVG_fri$bGVM[,1,2] , prob = 0.90 )[1] ) , 
                    sprintf("%0.2f" , PI( Post_model_MVG_fri$bGVM[,2,2] , prob = 0.90 )[1] ) ) ,
        
        bGVM_H = c( sprintf("%0.2f" , PI( Post_model_MVG_all$bGVM[,1,1] , prob = 0.90 )[2] ) , 
                    sprintf("%0.2f" , PI( Post_model_MVG_all$bGVM[,2,1] , prob = 0.90 )[2] ) ,  
                    sprintf("%0.2f" , PI( Post_model_MVG_all$bGVM[,1,2] , prob = 0.90 )[2] ) , 
                    sprintf("%0.2f" , PI( Post_model_MVG_all$bGVM[,2,2] , prob = 0.90 )[2] ) ,  
                    
                    sprintf("%0.2f" , PI( Post_model_MVG_pre$bGVM[,1,1] , prob = 0.90 )[2] ) , 
                    sprintf("%0.2f" , PI( Post_model_MVG_pre$bGVM[,2,1] , prob = 0.90 )[2] ) ,  
                    sprintf("%0.2f" , PI( Post_model_MVG_pre$bGVM[,1,2] , prob = 0.90 )[2] ) , 
                    sprintf("%0.2f" , PI( Post_model_MVG_pre$bGVM[,2,2] , prob = 0.90 )[2] ) ,  
                    
                    sprintf("%0.2f" , PI( Post_model_MVG_fin$bGVM[,1,1] , prob = 0.90 )[2] ) , 
                    sprintf("%0.2f" , PI( Post_model_MVG_fin$bGVM[,2,1] , prob = 0.90 )[2] ) ,  
                    sprintf("%0.2f" , PI( Post_model_MVG_fin$bGVM[,1,2] , prob = 0.90 )[2] ) , 
                    sprintf("%0.2f" , PI( Post_model_MVG_fin$bGVM[,2,2] , prob = 0.90 )[2] ) ,  
                    
                    sprintf("%0.2f" , PI( Post_model_MVG_fri$bGVM[,1,1] , prob = 0.90 )[2] ) , 
                    sprintf("%0.2f" , PI( Post_model_MVG_fri$bGVM[,2,1] , prob = 0.90 )[2] ) ,  
                    sprintf("%0.2f" , PI( Post_model_MVG_fri$bGVM[,1,2] , prob = 0.90 )[2] ) , 
                    sprintf("%0.2f" , PI( Post_model_MVG_fri$bGVM[,2,2] , prob = 0.90 )[2] ) ) ,
        `bGVM[90%CI]` = paste( bGVM , "[" , bGVM_L , ", " , bGVM_H , "]") , 
        Pro_bGVM = c( sprintf( "%0.4f" , length( which( Post_model_MVG_all$bGVM[,1,1] > 0 ) ) / 6000 ) , 
                      sprintf( "%0.4f" , length( which( Post_model_MVG_all$bGVM[,2,1] > 0 ) ) / 6000 ) , 
                      sprintf( "%0.4f" , length( which( Post_model_MVG_all$bGVM[,1,2] > 0 ) ) / 6000 ) , 
                      sprintf( "%0.4f" , length( which( Post_model_MVG_all$bGVM[,2,2] > 0 ) ) / 6000 ) , 
                      
                      sprintf( "%0.4f" , length( which( Post_model_MVG_pre$bGVM[,1,1] > 0 ) ) / 6000 ) , 
                      sprintf( "%0.4f" , length( which( Post_model_MVG_pre$bGVM[,2,1] > 0 ) ) / 6000 ) , 
                      sprintf( "%0.4f" , length( which( Post_model_MVG_pre$bGVM[,1,2] > 0 ) ) / 6000 ) , 
                      sprintf( "%0.4f" , length( which( Post_model_MVG_pre$bGVM[,2,2] > 0 ) ) / 6000 ) , 
                      
                      sprintf( "%0.4f" , length( which( Post_model_MVG_fin$bGVM[,1,1] > 0 ) ) / 6000 ) , 
                      sprintf( "%0.4f" , length( which( Post_model_MVG_fin$bGVM[,2,1] > 0 ) ) / 6000 ) , 
                      sprintf( "%0.4f" , length( which( Post_model_MVG_fin$bGVM[,1,2] > 0 ) ) / 6000 ) , 
                      sprintf( "%0.4f" , length( which( Post_model_MVG_fin$bGVM[,2,2] > 0 ) ) / 6000 ) , 
                      
                      sprintf( "%0.4f" , length( which( Post_model_MVG_fri$bGVM[,1,1] > 0 ) ) / 6000 ) , 
                      sprintf( "%0.4f" , length( which( Post_model_MVG_fri$bGVM[,2,1] > 0 ) ) / 6000 ) , 
                      sprintf( "%0.4f" , length( which( Post_model_MVG_fri$bGVM[,1,2] > 0 ) ) / 6000 ) , 
                      sprintf( "%0.4f" , length( which( Post_model_MVG_fri$bGVM[,2,2] > 0 ) ) / 6000 ) ) ) %>% 
  mutate( Pro_bGVM = paste( round( as.numeric( Pro_bGVM ) * 100 , 2 ) , "%" ) ) %>% 
  select( Type , Village , Gender , `bGVM[90%CI]` , Pro_bGVM ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels(everything() ) ) %>% 
  tab_style( style = list( cell_text(font = "Times New Roman") ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "center",
              columns = c( Type , Village , Gender , `bGVM[90%CI]` , Pro_bGVM ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( everything() ~ px( 150 ) )