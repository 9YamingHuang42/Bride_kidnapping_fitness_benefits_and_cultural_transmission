library( readxl )
library( dplyr )
library( tidyr )
library( dagitty )
library( plyr )
library( ggplot2 )
library( gt )
library( gto )
library( officer )
library( cmdstanr )
library( rethinking )

#1 DAG ----
Dag <- 
  dagitty( 'dag {
bb="-0.5,-0.5,0.5,0.5"
"Age cohort" [pos="-0.300,0.090"]
"Marriage of older siblings" [pos="-0.130,-0.390"]
"Parental marriage" [pos="0.130,-0.390"]
Marriage [outcome,pos="0.000,0.333"]
Religiosity [pos="0.300,-0.190"]
Sibship [pos="-0.300,-0.190"]
U_sibship [latent,pos="0.184,0.268"]
U_village [latent,pos="-0.170,0.279"]
"Age cohort" -> "Marriage of older siblings"
"Age cohort" -> "Parental marriage"
"Age cohort" -> Marriage
"Age cohort" -> Religiosity
"Age cohort" -> Sibship
"Marriage of older siblings" -> Marriage
"Parental marriage" -> "Marriage of older siblings"
"Parental marriage" -> Marriage
"Parental marriage" -> Sibship
Religiosity -> Marriage
Sibship -> "Marriage of older siblings"
Sibship -> Marriage
U_sibship -> Marriage
U_village -> Marriage
}
')

#2 Data preparation ----
load( file = "Bride_kidnapping_parental_and_older's_siblings'_marriage_type_analyses.RData" )

#3 Analyses ----
##3.1 Parental marriage type ----
adjustmentSets( Dag , 
                exposure = "Parental marriage" , 
                outcome = "Marriage" ,
                effect = "total" )

Model_P_list <- with( Transmit.data , list(
  Kidnapping = as.integer( Kidnapping ) , 
  Gender = as.integer( Gender ) , 
  Cohort = as.integer( BY.5c ) ,
  P_kidnapping = as.integer( P.kidnapping ) + 1 ,
  SibID = as.integer( SibID ) ,
  VID = as.integer( VID ) ) )

###3.1.1 Parental marriage type stratified by gender ----
{set.seed(123)
  Model_PG <- ulam(
    alist(
      Kidnapping ~ bernoulli( p ) ,
      logit( p ) <- bPG[P_kidnapping,Gender] + 
        bC[Cohort] + 
        bV[VID] + # village intercepts
        bSib[SibID] , # sibship intercepts
      
      matrix[P_kidnapping,Gender]: bPG ~ normal( 0 , 0.6 ) ,
      bC[Cohort] ~ normal( 0 , 1 ) ,
      
      # define effects of villages using other parameters
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V ,
      transpars> vector[SibID]: bSib <<- z_sib*sigma_sib ,
      
      V_bar ~ normal( 0 , 0.8 ) ,
      z_V[VID] ~ normal( 1 , 0.5 ) ,
      sigma_V ~ exponential( 1 ) ,
      z_sib[SibID] ~ normal( 0 , 0.8 ) ,
      sigma_sib ~ exponential( 1 ) 
    ) , data = Model_P_list , 
    iter = 1500 , warmup = 500 , chains = 6 , cores = 6 , 
    log_lik = TRUE , control = list( adapt_delta = 0.99 ) 
  ) }
Pre_model_PG <- precis( Model_PG , depth = 3 , prob = 0.90 , 
                        pars = c( "bPG" , "bC" , "bV" ) )
Pre_model_PG

###3.1.2 Parental marriage type stratified by village and gender ----
{set.seed(123)
  Model_PGV <- ulam(
    alist(
      Kidnapping ~ bernoulli( p ) ,
      logit( p ) <- bGV[Gender,VID] + bPGV[Gender,VID] * ( P_kidnapping - 1 ) + 
        bC[Cohort] + 
        bV[VID] + # village intercepts
        bSib[SibID] , # sibship intercepts 
      
      matrix[Gender,VID]: bGV ~ normal(0, 0.6) ,
      matrix[Gender,VID]: bPGV ~ normal(0, 0.6) ,
      bC[Cohort] ~ normal( 0 , 1 ) ,
      
      # define effects of villages using other parameters
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V ,
      transpars> vector[SibID]: bSib <<- z_sib*sigma_sib ,
      
      V_bar ~ normal( 0 , 0.8 ) ,
      z_V[VID] ~ normal( 1 , 0.5 ) ,
      sigma_V ~ exponential( 1 ) ,
      z_sib[SibID] ~ normal( 0 , 0.8 ) ,
      sigma_sib ~ exponential( 1 ) 
    ) , data = Model_P_list , 
    iter = 1500 , warmup = 500 , chains = 6 , cores = 6 , 
    log_lik = TRUE , control = list( adapt_delta = 0.99 ) 
  ) }
Pre_model_PGV <- precis( Model_PGV , depth = 3 , prob = 0.90 , 
                         pars = c( "bGV" , "bPGV" , "bC" , "bV" ) )
Pre_model_PGV

###3.1.3 Parental marriage type stratified by cohort ----
{set.seed(123)
  Model_PC <- ulam(
    alist(
      Kidnapping ~ bernoulli( p ) ,
      logit( p ) <- bPC[P_kidnapping,Cohort] + 
        bV[VID] + # village intercepts
        bSib[SibID] , # sibship intercepts
      
      matrix[P_kidnapping,Cohort]: bPC ~ normal( 0 , 0.6 ) ,
      
      # define effects of villages using other parameters
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V ,
      transpars> vector[SibID]: bSib <<- z_sib*sigma_sib ,
      
      V_bar ~ normal( 0 , 0.8 ) ,
      z_V[VID] ~ normal( 1 , 0.5 ) ,
      sigma_V ~ exponential( 1 ) ,
      z_sib[SibID] ~ normal( 0 , 0.8 ) ,
      sigma_sib ~ exponential( 1 ) 
    ) , data = Model_P_list , 
    iter = 1500 , warmup = 500 , chains = 6 , cores = 6 , 
    log_lik = TRUE , control = list( adapt_delta = 0.99 ) 
  ) }
Pre_model_PC <- precis( Model_PC , depth = 3 , prob = 0.90 , 
                        pars = c( "bPC" , "bV" ) )
Pre_model_PC

###3.1.4 Parental marriage type stratified by village and cohort ----
{set.seed(123)
  Model_PCV <- ulam(
    alist(
      Kidnapping ~ bernoulli( p ) ,
      logit( p ) <- bCV[Cohort,VID] + bPCV[Cohort,VID] * ( P_kidnapping - 1 ) + 
        bV[VID] + # village intercepts
        bSib[SibID] , # sibship intercepts
      
      matrix[Cohort,VID]: bCV ~ normal( 0 , 0.6 ) ,
      matrix[Cohort,VID]: bPCV ~ normal( 0 , 0.6 ) ,
      
      # define effects of villages using other parameters
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V ,
      transpars> vector[SibID]: bSib <<- z_sib*sigma_sib ,
      
      V_bar ~ normal( 0 , 0.8 ) ,
      z_V[VID] ~ normal( 1 , 0.5 ) ,
      sigma_V ~ exponential( 1 ) ,
      z_sib[SibID] ~ normal( 0 , 0.8 ) ,
      sigma_sib ~ exponential( 1 ) 
    ) , data = Model_P_list , 
    iter = 1500 , warmup = 500 , chains = 6 , cores = 6 , 
    log_lik = TRUE , control = list( adapt_delta = 0.99 ) 
  ) }
Pre_model_PCV <- precis( Model_PCV , depth = 3 , prob = 0.90 , 
                         pars = c( "bCV" , "bPCV" , "bV" ) )
Pre_model_PCV

###3.1.5 Figures ----
Post_model_PG <- extract.samples( Model_PG )
Post_model_PGV <- extract.samples( Model_PGV )

Post_model_PC <- extract.samples( Model_PC )
Post_model_PCV <- extract.samples( Model_PCV )

####3.1.5.1 Parental marriage type stratified by gender ----
#####3.1.5.1.1 Posterior distribution of estimates ----
{
  par( mfrow = c( 1 , 2 ) , oma = c( 2.2 , 2.2 , 0.2 , 0 ) , mar = c( 1.4 , 1.4 , 0 , 0 ) )
  dens( Post_model_PG$bPG[,1,1] , 
        adj = 1 , 
        xlim = c( -2.5 , 2 ) ,
        ylim = c( 0 , 2 ) ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#c82423" , cex.axis = 1.2 )
  dens( Post_model_PG$bPG[,2,1] ,
        adj = 1 , 
        lwd = 3 , col = "#c82423" , 
        add = T )
  mtext( "Density" , side = 2 , line = 2.4 , cex = 1.5 )
  mtext( "Females" , side = 1 , line = 2.4 , cex = 1.5 )
  
  dens( Post_model_PG$bPG[,1,2] ,
        adj = 1 , 
        xlim = c( -2.5 , 2 ) ,
        ylim = c( 0 , 2 ) ,
        yaxt = "n" , 
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#2878b5" , cex.axis = 1.2 )
  dens( Post_model_PG$bPG[,2,2] ,
        adj = 1 , 
        lwd = 3 , col = "#2878b5" , 
        add = T )
  mtext( "Males" , side = 1 , line = 2.4 , cex = 1.5 )
  legend( x = -2.3 , y = 2.1 , 
          box.col = "white",
          legend = c( "Parents without kidnap marriage : female" , 
                      "Parents with kidnap marriage : female" , 
                      "Parents without kidnap marriage : male" , 
                      "Parents with kidnap marriage : male" ) , 
          lty = c( 3 , 1 , 3 , 1 ) ,  
          col = c( "#c82423" , "#c82423" , "#2878b5" , "#2878b5" ) , 
          lwd = 2 ,
          cex = 1 , 
          bty = "n" ,
          y.intersp = 1 ,
          x.intersp = 0.4 ,
          seg.len = 1.5  )
}

dev.off()

#####3.1.5.1.2 Estimate difference ----
tibble( Value = c( Post_model_PG$bPG[,2,1] - Post_model_PG$bPG[,1,1] , 
                   Post_model_PG$bPG[,2,2] - Post_model_PG$bPG[,1,2] ) ,
        Gender = c( rep( "Female" , 6000 ) ,
                    rep( "Male" , 6000 ) ) ) %>% 
  mutate( Gender = factor( Gender , 
                           levels = c( "Female" , "Male" ) , 
                           labels = c( "Female" , "Male" ) ) ) %>% 
  ggplot( aes( x = Value , y = Gender , 
               fill = Gender , color = Gender) ) +
  geom_vline( xintercept = 0, linetype = 2 , linewidth = 1 ,
              color = "dimgray" ) +
  stat_halfeye( .width = .90, height = 0.8 ) + 
  scale_fill_manual( values = alpha( c( "#c82423" , "#2878b5" ) , 0.3 ) ) +
  scale_color_manual( values = c( "#c82423" , "#2878b5" ) ) +
  scale_y_discrete( NULL, labels = ggplot2:::parse_safe ) +
  xlab( "Posterior distribution of estimate differences" ) +
  scale_x_continuous( limits = c( -0.5 , 2 ) , breaks = c( -1 , 0 , 1 , 2 ) ) +
  theme(plot.margin = margin(10, 10, 10, 20),
        strip.background = element_rect(color = "black", fill = "white") ,
        strip.text.x = element_text(size = 16),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "Gainsboro"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour="black",fill=NA),
        panel.background = element_blank(),
        plot.title = element_text(size=16),
        legend.position = "none",
        axis.title.x = element_text(size = 18,
                                    margin = margin(t = 0.3, r = 0, b = 0, l = 0,unit = "cm")),
        axis.text.x = element_text(colour="black",size=16,
                                   margin = margin(t = 0.1, r = 0, b = 0, l = 0,unit = "cm")),
        axis.title.y = element_text(size = 18,
                                    margin = margin(t = 0, r = 0.5, b = 0, l = 0,unit = "cm")),
        axis.text.y = element_text(colour="black",size=16,
                                   margin = margin(t = 0, r = 0.2, b = 0, l = 0,unit = "cm")),
        legend.title=element_text(size=14,face = "bold"),
        legend.text=element_text(size=12,face = "bold"))

####3.1.5.2 Parental marriage type stratified by village and gender ----
#####3.1.5.2.1 Posterior distribution of estimates ----
{
  par( mfrow = c( 2 , 2 ) , oma = c( 2.2 , 2.2 , 0.2 , 0.2 ) , mar = c( 1.4 , 1.4 , 0 , 0 ) )
  # village 1
  dens( Post_model_PGV$bGV[,1,1] ,
        adj = 1 , 
        xlim = c( -2.5 , 2.5 ) ,
        ylim = c( 0 , 1.5 ) ,
        xaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#c82423" , cex.axis = 1.2 )
  dens( Post_model_PGV$bGV[,1,1] + Post_model_PGV$bPGV[,1,1] ,
        adj = 1 , 
        lwd = 3 , col = "#c82423" , 
        add = T )
  mtext( "Village 1" , side = 2 , line = 2.4 , cex = 1.2 )
  
  dens( Post_model_PGV$bGV[,2,1] ,
        adj = 1 , 
        xlim = c( -2.5 , 2.5 ) ,
        ylim = c( 0 , 1.5 ) ,
        xaxt = "n" ,
        yaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#2878b5" , cex.axis = 1.2 )
  dens( Post_model_PGV$bGV[,2,1] + Post_model_PGV$bPGV[,2,1] ,
        adj = 1 , 
        lwd = 3 , col = "#2878b5" , 
        add = T )
  legend( x = -2 , y = 1.6 , 
          box.col = "white",
          legend = c( "Parents without kidnap marriage : female" , 
                      "Parents with kidnap marriage : female" , 
                      "Parents without kidnap marriage : male" , 
                      "Parents with kidnap marriage : male" ) , 
          lty = c( 3 , 1 , 3 , 1 ) ,  
          col = c( "#c82423" , "#c82423" , "#2878b5" , "#2878b5" ) , 
          lwd = 2 ,
          cex = 1.2 , 
          bty = "n" ,
          y.intersp = 1.0 ,
          x.intersp = 0.4 ,
          seg.len = 1.5  )
  
  # village 2
  dens( Post_model_PGV$bGV[,1,2] ,
        adj = 1 , 
        xlim = c( -2.5 , 2.5 ) ,
        ylim = c( 0 , 1.5 ) ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#c82423" , cex.axis = 1.2 )
  dens( Post_model_PGV$bGV[,1,2] + Post_model_PGV$bPGV[,1,2] ,
        adj = 1 , 
        lwd = 3 , col = "#c82423" , 
        add = T )
  mtext( "Village 2" , side = 2 , line = 2.4 , cex = 1.2 )
  mtext( "Females" , side = 1 , line = 2.4 , cex = 1.2 )
  
  dens( Post_model_PGV$bGV[,2,2] ,
        adj = 1 , 
        xlim = c( -2.5 , 2.5 ) ,
        ylim = c( 0 , 1.5 ) ,
        yaxt = "n" ,
        xlab = "" , 
        ylab = "" ,
        lty = 3 , lwd = 3 , col = "#2878b5" , cex.axis = 1.2 )
  dens( Post_model_PGV$bGV[,2,2] + Post_model_PGV$bPGV[,2,2] ,
        adj = 1 , 
        lwd = 3 , col = "#2878b5" , 
        add = T )
  mtext( "Males" , side = 1 , line = 2.4 , cex = 1.2 )
}

dev.off()

#####3.1.5.2.2 Estimate difference ----
tibble( Value = c( Post_model_PGV$bPGV[,1,1] , 
                   Post_model_PGV$bPGV[,2,1] ,
                   Post_model_PGV$bPGV[,1,2] , 
                   Post_model_PGV$bPGV[,2,2] ) ,
        Type = c( rep( "V1:female" , 6000 ) ,
                  rep( "V1:male" , 6000 ) ,
                  rep( "V2:female" , 6000 ) ,
                  rep( "V2:male" , 6000 ) ) ) %>% 
  mutate( Type = factor( Type , 
                         levels = c( "V2:male" , "V2:female" ,  
                                     "V1:male" , "V1:female" ) ,  
                         labels = c( "V2:male" , "V2:female" ,  
                                     "V1:male" , "V1:female" ) ) ) %>% 
  ggplot( aes( x = Value , y = Type , 
               fill = Type , color = Type) ) +
  geom_vline( xintercept = 0, linetype = 2 , linewidth = 1 ,
              color = "dimgray" ) +
  stat_halfeye( .width = .90, height = 0.8 ) + 
  scale_fill_manual( values = alpha( c( "#2878b5" , "#c82423" , "#2878b5" , "#c82423" ) , 0.3 ) ) +
  scale_color_manual( values = c( "#2878b5" , "#c82423" , "#2878b5" , "#c82423" ) ) +
  scale_y_discrete( labels = ggplot2:::parse_safe ) +
  xlab( "Posterior distribution of mean differences" ) +
  ylab( "Village : gender" ) +
  scale_x_continuous( limits = c( -1 , 1.5 ) , breaks = c( -1 , 0 , 1 ) ) +
  theme(plot.margin = margin(10, 10, 10, 20),
        strip.background = element_rect(color = "black", fill = "white") ,
        strip.text.x = element_text(size = 16),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "Gainsboro"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour="black",fill=NA),
        panel.background = element_blank(),
        plot.title = element_text(size=16),
        legend.position = "none",
        axis.title.x = element_text(size = 18,
                                    margin = margin(t = 0.3, r = 0, b = 0, l = 0,unit = "cm")),
        axis.text.x = element_text(colour="black",size=16,
                                   margin = margin(t = 0.1, r = 0, b = 0, l = 0,unit = "cm")),
        axis.title.y = element_text(size = 18,
                                    margin = margin(t = 0, r = 0.5, b = 0, l = 0,unit = "cm")),
        axis.text.y = element_text(colour="black",size=16,
                                   margin = margin(t = 0, r = 0.2, b = 0, l = 0,unit = "cm")),
        legend.title=element_text(size=14,face = "bold"),
        legend.text=element_text(size=12,face = "bold"))

####3.1.5.3 Parental marriage type stratified by cohort----
#####3.1.5.3.1 Figure 3a: Posterior distribution of estimates ----
tibble( Value = c( Post_model_PC$bPC[,1,1] , 
                   Post_model_PC$bPC[,1,2] , 
                   Post_model_PC$bPC[,1,3] , 
                   Post_model_PC$bPC[,1,4] , 
                   Post_model_PC$bPC[,1,5] , 
                   Post_model_PC$bPC[,1,6] , 
                   Post_model_PC$bPC[,1,7] , 
                   Post_model_PC$bPC[,1,8] ,  
                   
                   Post_model_PC$bPC[,2,1] , 
                   Post_model_PC$bPC[,2,2] , 
                   Post_model_PC$bPC[,2,3] , 
                   Post_model_PC$bPC[,2,4] , 
                   Post_model_PC$bPC[,2,5] , 
                   Post_model_PC$bPC[,2,6] , 
                   Post_model_PC$bPC[,2,7] , 
                   Post_model_PC$bPC[,2,8] ) ,
        Parent = c( rep( "Parents without kidnap marriage" , 48000 ) ,
                    rep( "Parents with kidnap marriage" , 48000 ) ) ,
        Cohort = c( rep( "<=1960" , 6000 ) ,
                    rep( "1961-1965" , 6000 ) , 
                    rep( "1966-1970" , 6000 ) , 
                    rep( "1971-1975" , 6000 ) ,
                    rep( "1976-1980" , 6000 ) , 
                    rep( "1981-1985" , 6000 ) , 
                    rep( "1986-1990" , 6000 ) , 
                    rep( ">1990" , 6000 ) ,
                    rep( "<=1960" , 6000 ) ,
                    rep( "1961-1965" , 6000 ) , 
                    rep( "1966-1970" , 6000 ) , 
                    rep( "1971-1975" , 6000 ) ,
                    rep( "1976-1980" , 6000 ) , 
                    rep( "1981-1985" , 6000 ) , 
                    rep( "1986-1990" , 6000 ) , 
                    rep( ">1990" , 6000 ) ) ) %>% 
  mutate( Parent = factor( Parent , 
                           levels = c( "Parents without kidnap marriage" , "Parents with kidnap marriage" ) , 
                           labels = c( "Parents without kidnap marriage" , "Parents with kidnap marriage" ) ) ,
          Cohort = factor( Cohort ,  
                           levels = c( ">1990" , "1986-1990" , "1981-1985" , 
                                       "1976-1980" , "1971-1975" , "1966-1970" , 
                                       "1961-1965" , "<=1960" ) , 
                           labels = c( ">1990" , "1986-1990" , "1981-1985" , 
                                       "1976-1980" , "1971-1975" , "1966-1970" , 
                                       "1961-1965" , "<=1960" ) ) ) %>% 
  ggplot( aes( x = Value , y = Cohort , 
               color = Parent , fill = Parent ) ) +
  geom_vline( xintercept = 0, linetype = 2 , linewidth = 1 ,
              color = "dimgray" ) +
  stat_halfeye( .width = .90, height = 0.8 ) + 
  scale_fill_manual( values = alpha( c( "#8e7fbb" , "#a2c9ae" ) , 0.4 ) ) +
  scale_color_manual( values = c( "#8e7fbb" , "#a2c9ae" ) ) +
  scale_y_discrete( breaks = c( ">1990" , "1986-1990" , "1981-1985" , 
                                "1976-1980" , "1971-1975" , "1966-1970" , 
                                "1961-1965" , "<=1960" ) ,
                    labels = c( ">1990" , "1986-1990" , "1981-1985" , 
                                "1976-1980" , "1971-1975" , "1966-1970" , 
                                "1961-1965" , "<=1960" ) )+
  labs( x = "Posterior distribution of estimates" ,
        y = "Cohorts" ,
        color = NULL ,
        fill = NULL ) +
  coord_cartesian( ylim = c( 1.5, 9.0 ) ) +
  scale_x_continuous( limits = c( -2.5 , 2 ) , breaks = c( -2 , -1 , 0 , 1 , 2 ) ) +
  theme(strip.background = element_rect(color = "black", fill = "white") ,
        strip.text.x = element_text(size = 16),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "Gainsboro"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour="black",fill=NA),
        panel.background = element_blank(),
        plot.title = element_text(size=16),
        legend.margin = margin(0, 0, -5, 0) ,
        legend.position = c(0.99,0.99),
        legend.justification = c(0.99,0.99), 
        axis.title.x = element_text(size = 18,
                                    margin = margin(t = 0.2, r = 0, b = 0, l = 0,unit = "cm")),
        axis.text.x = element_text(colour="black",size=16,
                                   margin = margin(t = 0.1, r = 0, b = 0, l = 0,unit = "cm")),
        axis.title.y = element_text(size = 18,
                                    margin = margin(t = 0, r = 0.2, b = 0, l = 0,unit = "cm")),
        axis.text.y = element_text(colour="black",size=16,
                                   margin = margin(t = 0, r = 0.1, b = 0, l = 0,unit = "cm")),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14))

#####3.1.5.3.2 Estimate difference ----
tibble( Value = c( Post_model_PC$bPC[,2,1] - Post_model_PC$bPC[,1,1] ,
                   Post_model_PC$bPC[,2,2] - Post_model_PC$bPC[,1,2] , 
                   Post_model_PC$bPC[,2,3] - Post_model_PC$bPC[,1,3] , 
                   Post_model_PC$bPC[,2,4] - Post_model_PC$bPC[,1,4] , 
                   Post_model_PC$bPC[,2,5] - Post_model_PC$bPC[,1,5] , 
                   Post_model_PC$bPC[,2,6] - Post_model_PC$bPC[,1,6] , 
                   Post_model_PC$bPC[,2,7] - Post_model_PC$bPC[,1,7] , 
                   Post_model_PC$bPC[,2,8] - Post_model_PC$bPC[,1,8] ) ,
        Cohort = c( rep( "<=1960" , 6000 ) ,
                    rep( "1961-1965" , 6000 ) , 
                    rep( "1966-1970" , 6000 ) , 
                    rep( "1971-1975" , 6000 ) ,
                    rep( "1976-1980" , 6000 ) , 
                    rep( "1981-1985" , 6000 ) , 
                    rep( "1986-1990" , 6000 ) , 
                    rep( ">1990" , 6000 ) ) ) %>% 
  mutate( Cohort = factor( Cohort ,  
                           levels = c( ">1990" , "1986-1990" , "1981-1985" , 
                                       "1976-1980" , "1971-1975" , "1966-1970" , 
                                       "1961-1965" , "<=1960" ) , 
                           labels = c( ">1990" , "1986-1990" , "1981-1985" , 
                                       "1976-1980" , "1971-1975" , "1966-1970" , 
                                       "1961-1965" , "<=1960" ) ) ) %>% 
  ggplot( aes( x = Value , y = Cohort , 
               fill = Cohort , color = Cohort) ) +
  geom_vline( xintercept = 0, linetype = 2 , linewidth = 1 ,
              color = "dimgray" ) +
  stat_halfeye( .width = .90, height = 0.8 ) + 
  scale_fill_manual( values = alpha( c( "#9e9e9e" , "#9e9e9e" , "#9e9e9e" ,
                                        "#9e9e9e" , "#9e9e9e" , "#9e9e9e" ,
                                        "#9e9e9e" , "#9e9e9e" ) , 0.4 ) ) +
  scale_color_manual( values = c( "#9e9e9e" , "#9e9e9e" , "#9e9e9e" ,
                                  "#9e9e9e" , "#9e9e9e" , "#9e9e9e" ,
                                  "#9e9e9e" , "#9e9e9e"  ) ) +
  scale_y_discrete( breaks = c( ">1990" , "1986-1990" , "1981-1985" , 
                                "1976-1980" , "1971-1975" , "1966-1970" , 
                                "1961-1965" , "<=1960" ) ,
                    labels = c( ">1990" , "1986-1990" , "1981-1985" , 
                                "1976-1980" , "1971-1975" , "1966-1970" , 
                                "1961-1965" , "<=1960" ) )+
  xlab( "Posterior distribution of mean differences" ) +
  ylab( "Cohorts" ) +
  coord_cartesian( ylim = c( 1.5, 9.0 ) ) +
  scale_x_continuous( limits = c( -2.5 , 2 ) , breaks = c( -2 , -1 , 0 , 1 , 2 ) ) +
  theme(plot.margin = margin(10, 10, 10, 20),
        strip.background = element_rect(color = "black", fill = "white") ,
        strip.text.x = element_text(size = 16),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "Gainsboro"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour="black",fill=NA),
        panel.background = element_blank(),
        plot.title = element_text(size=16),
        legend.position = "none",
        axis.title.x = element_text(size = 18,
                                    margin = margin(t = 0.3, r = 0, b = 0, l = 0,unit = "cm")),
        axis.text.x = element_text(colour="black",size=16,
                                   margin = margin(t = 0.1, r = 0, b = 0, l = 0,unit = "cm")),
        axis.title.y = element_text(size = 18,
                                    margin = margin(t = 0, r = 0.5, b = 0, l = 0,unit = "cm")),
        axis.text.y = element_text(colour="black",size=16,
                                   margin = margin(t = 0, r = 0.2, b = 0, l = 0,unit = "cm")),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12))

####3.1.5.4 Parental marriage type stratified by village and cohort----
#####3.1.5.4.1 Posterior distribution of estimates ----
tibble( Value = c( Post_model_PCV$bCV[,1,1] , 
                   Post_model_PCV$bCV[,2,1] , 
                   Post_model_PCV$bCV[,3,1] , 
                   Post_model_PCV$bCV[,4,1] , 
                   Post_model_PCV$bCV[,5,1] , 
                   Post_model_PCV$bCV[,6,1] , 
                   Post_model_PCV$bCV[,7,1] , 
                   Post_model_PCV$bCV[,8,1] ,  
                   
                   Post_model_PCV$bCV[,1,1] + Post_model_PCV$bPCV[,1,1] , 
                   Post_model_PCV$bCV[,2,1] + Post_model_PCV$bPCV[,2,1] , 
                   Post_model_PCV$bCV[,3,1] + Post_model_PCV$bPCV[,3,1] , 
                   Post_model_PCV$bCV[,4,1] + Post_model_PCV$bPCV[,4,1] , 
                   Post_model_PCV$bCV[,5,1] + Post_model_PCV$bPCV[,5,1] , 
                   Post_model_PCV$bCV[,6,1] + Post_model_PCV$bPCV[,6,1] , 
                   Post_model_PCV$bCV[,7,1] + Post_model_PCV$bPCV[,7,1] , 
                   Post_model_PCV$bCV[,8,1] + Post_model_PCV$bPCV[,8,1] ,
                   
                   Post_model_PCV$bCV[,1,2] , 
                   Post_model_PCV$bCV[,2,2] , 
                   Post_model_PCV$bCV[,3,2] , 
                   Post_model_PCV$bCV[,4,2] , 
                   Post_model_PCV$bCV[,5,2] , 
                   Post_model_PCV$bCV[,6,2] , 
                   Post_model_PCV$bCV[,7,2] , 
                   Post_model_PCV$bCV[,8,2] ,  
                   
                   Post_model_PCV$bCV[,1,2] + Post_model_PCV$bPCV[,1,2] , 
                   Post_model_PCV$bCV[,2,2] + Post_model_PCV$bPCV[,2,2] , 
                   Post_model_PCV$bCV[,3,2] + Post_model_PCV$bPCV[,3,2] , 
                   Post_model_PCV$bCV[,4,2] + Post_model_PCV$bPCV[,4,2] , 
                   Post_model_PCV$bCV[,5,2] + Post_model_PCV$bPCV[,5,2] , 
                   Post_model_PCV$bCV[,6,2] + Post_model_PCV$bPCV[,6,2] , 
                   Post_model_PCV$bCV[,7,2] + Post_model_PCV$bPCV[,7,2] , 
                   Post_model_PCV$bCV[,8,2] + Post_model_PCV$bPCV[,8,2] ) ,
        Village = c( rep( "Village 1" , 96000 ) ,
                     rep( "Village 2" , 96000 ) ) ,
        Parent = c( rep( "Parents without kidnap marriage" , 48000 ) ,
                    rep( "Parents with kidnap marriage" , 48000 ) , 
                    rep( "Parents without kidnap marriage" , 48000 ) ,
                    rep( "Parents with kidnap marriage" , 48000 ) ) ,
        Cohort = c( rep( c( rep( "<=1960" , 6000 ) ,
                            rep( "1961-1965" , 6000 ) , 
                            rep( "1966-1970" , 6000 ) , 
                            rep( "1971-1975" , 6000 ) ,
                            rep( "1976-1980" , 6000 ) , 
                            rep( "1981-1985" , 6000 ) , 
                            rep( "1986-1990" , 6000 ) , 
                            rep( ">1990" , 6000 ) ) , 4 ) ) ) %>% 
  mutate( Village = factor( Village , 
                            levels = c( "Village 1" , "Village 2" ) , 
                            labels = c( "Village 1" , "Village 2" ) ) ,
          Parent = factor( Parent , 
                           levels = c( "Parents without kidnap marriage" , "Parents with kidnap marriage" ) , 
                           labels = c( "Parents without kidnap marriage" , "Parents with kidnap marriage" ) ) ,
          Cohort = factor( Cohort ,  
                           levels = c( ">1990" , "1986-1990" , "1981-1985" , 
                                       "1976-1980" , "1971-1975" , "1966-1970" , 
                                       "1961-1965" , "<=1960" ) , 
                           labels = c( ">1990" , "1986-1990" , "1981-1985" , 
                                       "1976-1980" , "1971-1975" , "1966-1970" , 
                                       "1961-1965" , "<=1960" ) ) ) %>% 
  ggplot( aes( x = Value , y = Cohort , 
               color = Parent , fill = Parent ) ) +
  facet_wrap( ~ Village , nrow = 1 ) + 
  geom_vline( xintercept = 0, linetype = 2 , linewidth = 1 ,
              color = "dimgray" ) +
  stat_halfeye( .width = .90, height = 0.8 ) + 
  scale_fill_manual( values = alpha( c( "#8e7fbb" , "#a2c9ae" ) , 0.4 ) ) +
  scale_color_manual( values = c( "#8e7fbb" , "#a2c9ae" ) ) +
  scale_y_discrete( breaks = c( ">1990" , "1986-1990" , "1981-1985" , 
                                "1976-1980" , "1971-1975" , "1966-1970" , 
                                "1961-1965" , "<=1960" ) ,
                    labels = c( ">1990" , "1986-1990" , "1981-1985" , 
                                "1976-1980" , "1971-1975" , "1966-1970" , 
                                "1961-1965" , "<=1960" ) )+
  labs( x = "Posterior distribution of estimates" ,
        y = "Cohorts" ,
        color = "" ,
        fill = "" ) +
  coord_cartesian( ylim = c( 1.5, 9.0 ) ) +
  scale_x_continuous( limits = c( -3 , 3 ) , breaks = c( -3 , -2 , -1 , 0 , 1 , 2 , 3 ) ) +
  theme(strip.background = element_rect(color = "black", fill = "white") ,
        strip.text.x = element_text(size = 16),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "Gainsboro"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour="black",fill=NA),
        panel.background = element_blank(),
        plot.title = element_text(size=16),
        legend.margin = margin(0, 0, -5, 0) ,
        legend.position = "top",
        legend.justification = "center", 
        legend.box = "horizontal",  
        axis.title.x = element_text(size = 18,
                                    margin = margin(t = 0.3, r = 0, b = 0, l = 0,unit = "cm")),
        axis.text.x = element_text(colour="black",size=16,
                                   margin = margin(t = 0.1, r = 0, b = 0, l = 0,unit = "cm")),
        axis.title.y = element_text(size = 18,
                                    margin = margin(t = 0, r = 0.5, b = 0, l = 0,unit = "cm")),
        axis.text.y = element_text(colour="black",size=16,
                                   margin = margin(t = 0, r = 0.2, b = 0, l = 0,unit = "cm")),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14))

#####3.1.5.4.2 Estimate difference ----
tibble( Value = c( Post_model_PCV$bPCV[,1,1] ,
                   Post_model_PCV$bPCV[,2,1] , 
                   Post_model_PCV$bPCV[,3,1] , 
                   Post_model_PCV$bPCV[,4,1] , 
                   Post_model_PCV$bPCV[,5,1] , 
                   Post_model_PCV$bPCV[,6,1] , 
                   Post_model_PCV$bPCV[,7,1] , 
                   Post_model_PCV$bPCV[,8,1] , 
                   
                   Post_model_PCV$bPCV[,1,2] ,
                   Post_model_PCV$bPCV[,2,2] , 
                   Post_model_PCV$bPCV[,3,2] , 
                   Post_model_PCV$bPCV[,4,2] , 
                   Post_model_PCV$bPCV[,5,2] , 
                   Post_model_PCV$bPCV[,6,2] , 
                   Post_model_PCV$bPCV[,7,2] , 
                   Post_model_PCV$bPCV[,8,2] ) ,
        Village = c( rep( "Village 1" , 48000 ) ,
                     rep( "Village 2" , 48000 ) ) ,
        Cohort = c( rep( c( rep( "<=1960" , 6000 ) ,
                            rep( "1961-1965" , 6000 ) , 
                            rep( "1966-1970" , 6000 ) , 
                            rep( "1971-1975" , 6000 ) ,
                            rep( "1976-1980" , 6000 ) , 
                            rep( "1981-1985" , 6000 ) , 
                            rep( "1986-1990" , 6000 ) , 
                            rep( ">1990" , 6000 ) ) , 2 ) ) ) %>% 
  mutate( Village = factor( Village , 
                            levels = c( "Village 1" , "Village 2" ) , 
                            labels = c( "Village 1" , "Village 2" ) ) ,
          Cohort = factor( Cohort ,  
                           levels = c( ">1990" , "1986-1990" , "1981-1985" , 
                                       "1976-1980" , "1971-1975" , "1966-1970" , 
                                       "1961-1965" , "<=1960" ) , 
                           labels = c( ">1990" , "1986-1990" , "1981-1985" , 
                                       "1976-1980" , "1971-1975" , "1966-1970" , 
                                       "1961-1965" , "<=1960" ) ) ) %>% 
  ggplot( aes( x = Value , y = Cohort , 
               fill = Cohort , color = Cohort) ) +
  facet_wrap( ~ Village , nrow = 1 ) + 
  geom_vline( xintercept = 0, linetype = 2 , linewidth = 1 ,
              color = "dimgray" ) +
  stat_halfeye( .width = .90, height = 0.8 ) + 
  scale_fill_manual( values = alpha( c( "#9e9e9e" , "#9e9e9e" , "#9e9e9e" ,
                                        "#9e9e9e" , "#9e9e9e" , "#9e9e9e" ,
                                        "#9e9e9e" , "#9e9e9e" ) , 0.4 ) ) +
  scale_color_manual( values = c( "#9e9e9e" , "#9e9e9e" , "#9e9e9e" ,
                                  "#9e9e9e" , "#9e9e9e" , "#9e9e9e" ,
                                  "#9e9e9e" , "#9e9e9e"  ) ) +
  scale_y_discrete( breaks = c( ">1990" , "1986-1990" , "1981-1985" , 
                                "1976-1980" , "1971-1975" , "1966-1970" , 
                                "1961-1965" , "<=1960" ) ,
                    labels = c( ">1990" , "1986-1990" , "1981-1985" , 
                                "1976-1980" , "1971-1975" , "1966-1970" , 
                                "1961-1965" , "<=1960" ) )+
  xlab( "Posterior distribution of mean differences" ) +
  ylab( "Cohorts" ) +
  coord_cartesian( ylim = c( 1.5, 9.0 ) ) +
  scale_x_continuous( limits = c( -2.5 , 2 ) , breaks = c( -2 , -1 , 0 , 1 , 2 ) ) +
  theme(plot.margin = margin(10, 10, 10, 20),
        strip.background = element_rect(color = "black", fill = "white") ,
        strip.text.x = element_text(size = 16),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "Gainsboro"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour="black",fill=NA),
        panel.background = element_blank(),
        plot.title = element_text(size=16),
        legend.position = "none",
        axis.title.x = element_text(size = 18,
                                    margin = margin(t = 0.3, r = 0, b = 0, l = 0,unit = "cm")),
        axis.text.x = element_text(colour="black",size=16,
                                   margin = margin(t = 0.1, r = 0, b = 0, l = 0,unit = "cm")),
        axis.title.y = element_text(size = 18,
                                    margin = margin(t = 0, r = 0.5, b = 0, l = 0,unit = "cm")),
        axis.text.y = element_text(colour="black",size=16,
                                   margin = margin(t = 0, r = 0.2, b = 0, l = 0,unit = "cm")),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12))

###3.1.6 Outputs ----
####3.1.6.1 Parental marriage type stratified by gender ----
#####3.1.6.1.1 Estimates ----
Pre_PG_output <- data.frame( Mean = c( Pre_model_PG$mean ) ,
                             CI5 = c( Pre_model_PG$`5%` ) ,
                             CI95 = c( Pre_model_PG$`95%` ) ,
                             Variable = c( "Parents without kidnap marriage: female" , 
                                           "Parents with kidnap marriage: female" ,
                                           "Parents without kidnap marriage: male" , 
                                           "Parents with kidnap marriage: male" ,
                                           
                                           "<=1960" , "1961-1965" , 
                                           "1966-1970" , "1971-1975" , "1976-1980" , 
                                           "1981-1985" , "1986-1990" , ">1990" ,
                                           
                                           "Village 1" , "Village 2" ) ) %>% 
  mutate( Variable = factor( Variable , 
                             levels = c( "Parents without kidnap marriage: female" , 
                                         "Parents with kidnap marriage: female" ,
                                         "Parents without kidnap marriage: male" , 
                                         "Parents with kidnap marriage: male" ,
                                         
                                         "<=1960" , "1961-1965" , 
                                         "1966-1970" , "1971-1975" , "1976-1980" , 
                                         "1981-1985" , "1986-1990" , ">1990" ,
                                         
                                         "Village 1" , "Village 2" ) ,
                             labels = c( "Parents without kidnap marriage: female" , 
                                         "Parents with kidnap marriage: female" ,
                                         "Parents without kidnap marriage: male" , 
                                         "Parents with kidnap marriage: male" ,
                                         
                                         "<=1960" , "1961-1965" , 
                                         "1966-1970" , "1971-1975" , "1976-1980" , 
                                         "1981-1985" , "1986-1990" , ">1990" , 
                                         
                                         "Village 1" , "Village 2" ) ) ,
          Model = "Parental marriage type stratified by gender" , 
          "Mean [CI]" = paste( format( round( Mean , 2 ), nsmall = 2 ) , 
                               " [" , 
                               format( round( CI5 , 2 ), nsmall = 2 ) , 
                               ", " , 
                               format( round( CI95 , 2 ), nsmall = 2 ) , 
                               "]", 
                               sep = "") )

PG_output <- Pre_PG_output %>% 
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
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

PG_output

#####3.1.6.1.2 Estimate differences ----
tibble( Gender = c( "Female" , "Male" ) ,
        bPG_diff = c( sprintf("%0.2f" , mean( Post_model_PG$bPG[,2,1] - Post_model_PG$bPG[,1,1] ) ) , 
                      sprintf("%0.2f" , mean( Post_model_PG$bPG[,2,2] - Post_model_PG$bPG[,1,2] ) ) ) ,
        bPG_diff_L = c( sprintf("%0.2f" , PI( Post_model_PG$bPG[,2,1] - Post_model_PG$bPG[,1,1] , prob = 0.90 )[1] ) , 
                        sprintf("%0.2f" , PI( Post_model_PG$bPG[,2,2] - Post_model_PG$bPG[,1,2] , prob = 0.90 )[1] ) ) ,
        
        bPG_diff_H = c( sprintf("%0.2f" , PI( Post_model_PG$bPG[,2,1] - Post_model_PG$bPG[,1,1] , prob = 0.90 )[2] ) , 
                        sprintf("%0.2f" , PI( Post_model_PG$bPG[,2,2] - Post_model_PG$bPG[,1,2] , prob = 0.90 )[2] ) ) ,
        `bPG_diff[90%CI]` = paste( bPG_diff , "[" , bPG_diff_L , ", " , bPG_diff_H , "]") , 
        Pro_bPG_diff = c( sprintf( "%0.4f" , length( which( ( Post_model_PG$bPG[,2,1] - Post_model_PG$bPG[,1,1] ) > 0 ) ) / 6000 ) , 
                          sprintf( "%0.4f" , length( which( ( Post_model_PG$bPG[,2,2] - Post_model_PG$bPG[,1,2] ) > 0 ) ) / 6000 ) ) ) %>% 
  mutate( Pro_bPG_diff = paste( round( as.numeric( Pro_bPG_diff ) * 100 , 2 ) , "%" ) ) %>% 
  select( Gender , `bPG_diff[90%CI]` , Pro_bPG_diff ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels(everything() ) ) %>% 
  tab_style( style = list( cell_text(font = "Times New Roman") ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "center",
              columns = c( Gender , `bPG_diff[90%CI]` , Pro_bPG_diff ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( everything() ~ px( 150 ) )

####3.1.6.2 Parental marriage type stratified by village and gender ----
#####3.1.6.2.1 Estimates ----
Pre_PGV_output <- data.frame( Mean = c( Pre_model_PGV$mean ) ,
                              CI5 = c( Pre_model_PGV$`5%` ) ,
                              CI95 = c( Pre_model_PGV$`95%` ) ,
                              Variable = c( "Parents without kidnap marriage: V1 female" , 
                                            "Parents without kidnap marriage: V1 male" , 
                                            "Parents without kidnap marriage: V2 female" , 
                                            "Parents without kidnap marriage: V2 male" , 
                                            "Kidnapping difference: V1 female" ,
                                            "Kidnapping difference: V1 male" ,
                                            "Kidnapping difference: V2 female" ,
                                            "Kidnapping difference: V2 male" ,
                                            
                                            "<=1960" , "1961-1965" , 
                                            "1966-1970" , "1971-1975" , "1976-1980" , 
                                            "1981-1985" , "1986-1990" , ">1990" ,
                                            
                                            "Village 1" , "Village 2" ) ) %>% 
  mutate( Variable = factor( Variable , 
                             levels = c( "Parents without kidnap marriage: V1 female" , 
                                         "Parents without kidnap marriage: V1 male" , 
                                         "Parents without kidnap marriage: V2 female" , 
                                         "Parents without kidnap marriage: V2 male" , 
                                         "Kidnapping difference: V1 female" ,
                                         "Kidnapping difference: V1 male" ,
                                         "Kidnapping difference: V2 female" ,
                                         "Kidnapping difference: V2 male" ,
                                         
                                         "<=1960" , "1961-1965" , 
                                         "1966-1970" , "1971-1975" , "1976-1980" , 
                                         "1981-1985" , "1986-1990" , ">1990" ,
                                         
                                         "Village 1" , "Village 2" ) ,
                             labels = c( "Parents without kidnap marriage: V1 female" , 
                                         "Parents without kidnap marriage: V1 male" , 
                                         "Parents without kidnap marriage: V2 female" , 
                                         "Parents without kidnap marriage: V2 male" , 
                                         "Kidnapping difference: V1 female" ,
                                         "Kidnapping difference: V1 male" ,
                                         "Kidnapping difference: V2 female" ,
                                         "Kidnapping difference: V2 male" ,
                                         
                                         "<=1960" , "1961-1965" , 
                                         "1966-1970" , "1971-1975" , "1976-1980" , 
                                         "1981-1985" , "1986-1990" , ">1990" ,
                                         
                                         "Village 1" , "Village 2" ) ) ,
          Model = "Parental marriage type stratified by village and gender" , 
          "Mean [CI]" = paste( format( round( Mean , 2 ), nsmall = 2 ) , 
                               " [" , 
                               format( round( CI5 , 2 ), nsmall = 2 ) , 
                               ", " , 
                               format( round( CI95 , 2 ), nsmall = 2 ) , 
                               "]", 
                               sep = "") )

PGV_output <- Pre_PGV_output %>% 
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
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

PGV_output

#####3.1.6.2.2 Estimate differences ----
tibble( Village = c( "V1" , "V1" , "V2" , "V2"  ) ,
        Gender = c( "Female" , "Male" , "Female" , "Male"  ) ,
        bPGV = c( sprintf("%0.2f" , mean( Post_model_PGV$bPGV[,1,1] ) ) , 
                  sprintf("%0.2f" , mean( Post_model_PGV$bPGV[,2,1] ) ) , 
                  sprintf("%0.2f" , mean( Post_model_PGV$bPGV[,1,2] ) ) , 
                  sprintf("%0.2f" , mean( Post_model_PGV$bPGV[,2,2] ) ) ) ,
        bPGV_L = c( sprintf("%0.2f" , PI( Post_model_PGV$bPGV[,1,1] , prob = 0.90 )[1] ) , 
                    sprintf("%0.2f" , PI( Post_model_PGV$bPGV[,2,1] , prob = 0.90 )[1] ) ,  
                    sprintf("%0.2f" , PI( Post_model_PGV$bPGV[,1,2] , prob = 0.90 )[1] ) , 
                    sprintf("%0.2f" , PI( Post_model_PGV$bPGV[,2,2] , prob = 0.90 )[1] ) ) ,
        
        bPGV_H = c( sprintf("%0.2f" , PI( Post_model_PGV$bPGV[,1,1] , prob = 0.90 )[2] ) , 
                    sprintf("%0.2f" , PI( Post_model_PGV$bPGV[,2,1] , prob = 0.90 )[2] ) ,  
                    sprintf("%0.2f" , PI( Post_model_PGV$bPGV[,1,2] , prob = 0.90 )[2] ) , 
                    sprintf("%0.2f" , PI( Post_model_PGV$bPGV[,2,2] , prob = 0.90 )[2] ) ) ,
        `bPGV[90%CI]` = paste( bPGV , "[" , bPGV_L , ", " , bPGV_H , "]") ,
        Pro_bPGV = c( sprintf( "%0.4f" , length( which( Post_model_PGV$bPGV[,1,1] > 0 ) ) / 6000 ) , 
                      sprintf( "%0.4f" , length( which( Post_model_PGV$bPGV[,2,1] > 0 ) ) / 6000 ) , 
                      sprintf( "%0.4f" , length( which( Post_model_PGV$bPGV[,1,2] > 0 ) ) / 6000 ) , 
                      sprintf( "%0.4f" , length( which( Post_model_PGV$bPGV[,2,2] > 0 ) ) / 6000 ) ) ) %>% 
  mutate( Pro_bPGV = paste( round( as.numeric( Pro_bPGV ) * 100 , 2 ) , "%" ) ) %>% 
  select( Village , Gender , `bPGV[90%CI]` , Pro_bPGV ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels(everything() ) ) %>% 
  tab_style( style = list( cell_text(font = "Times New Roman") ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "center",
              columns = c( Village , Gender , `bPGV[90%CI]` , Pro_bPGV ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( everything() ~ px( 150 ) )

####3.1.6.3 Parental marriage type stratified by cohort ----
#####3.1.6.3.1 Estimates ----
Pre_PC_output <- data.frame( Mean = c( Pre_model_PC$mean ) ,
                             CI5 = c( Pre_model_PC$`5%` ) ,
                             CI95 = c( Pre_model_PC$`95%` ) ,
                             Variable = c( "Parents without kidnap marriage: <=1960" , 
                                           "Parents with kidnap marriage: <=1960" , 
                                           
                                           "Parents without kidnap marriage: 1961-1965" , 
                                           "Parents with kidnap marriage: 1961-1965" , 
                                           
                                           "Parents without kidnap marriage: 1966-1970" , 
                                           "Parents with kidnap marriage: 1966-1970" , 
                                           
                                           "Parents without kidnap marriage: 1971-1975" , 
                                           "Parents with kidnap marriage: 1971-1975" , 
                                           
                                           "Parents without kidnap marriage: 1976-1980" , 
                                           "Parents with kidnap marriage: 1976-1980" , 
                                           
                                           "Parents without kidnap marriage: 1981-1985" , 
                                           "Parents with kidnap marriage: 1981-1985" , 
                                           
                                           "Parents without kidnap marriage: 1986-1990" , 
                                           "Parents with kidnap marriage: 1986-1990" , 
                                           
                                           "Parents without kidnap marriage: >1990" , 
                                           "Parents with kidnap marriage: >1990" , 
                                           
                                           "Village 1" , "Village 2" ) ) %>% 
  mutate( Variable = factor( Variable , 
                             levels = c( "Parents without kidnap marriage: <=1960" , 
                                         "Parents without kidnap marriage: 1961-1965" , 
                                         "Parents without kidnap marriage: 1966-1970" , 
                                         "Parents without kidnap marriage: 1971-1975" , 
                                         "Parents without kidnap marriage: 1976-1980" , 
                                         "Parents without kidnap marriage: 1981-1985" , 
                                         "Parents without kidnap marriage: 1986-1990" , 
                                         "Parents without kidnap marriage: >1990" , 
                                         
                                         "Parents with kidnap marriage: <=1960" , 
                                         "Parents with kidnap marriage: 1961-1965" , 
                                         "Parents with kidnap marriage: 1966-1970" , 
                                         "Parents with kidnap marriage: 1971-1975" , 
                                         "Parents with kidnap marriage: 1976-1980" , 
                                         "Parents with kidnap marriage: 1981-1985" , 
                                         "Parents with kidnap marriage: 1986-1990" , 
                                         "Parents with kidnap marriage: >1990" , 
                                         
                                         "Village 1" , "Village 2" ) ,
                             labels = c( "Parents without kidnap marriage: <=1960" , 
                                         "Parents without kidnap marriage: 1961-1965" , 
                                         "Parents without kidnap marriage: 1966-1970" , 
                                         "Parents without kidnap marriage: 1971-1975" , 
                                         "Parents without kidnap marriage: 1976-1980" , 
                                         "Parents without kidnap marriage: 1981-1985" , 
                                         "Parents without kidnap marriage: 1986-1990" , 
                                         "Parents without kidnap marriage: >1990" , 
                                         
                                         "Parents with kidnap marriage: <=1960" , 
                                         "Parents with kidnap marriage: 1961-1965" , 
                                         "Parents with kidnap marriage: 1966-1970" , 
                                         "Parents with kidnap marriage: 1971-1975" , 
                                         "Parents with kidnap marriage: 1976-1980" , 
                                         "Parents with kidnap marriage: 1981-1985" , 
                                         "Parents with kidnap marriage: 1986-1990" , 
                                         "Parents with kidnap marriage: >1990" , 
                                         
                                         "Village 1" , "Village 2" ) ) ,
          Model = "Parental marriage type stratified by cohort" , 
          "Mean [CI]" = paste( format( round( Mean , 2 ), nsmall = 2 ) , 
                               " [" , 
                               format( round( CI5 , 2 ), nsmall = 2 ) , 
                               ", " , 
                               format( round( CI95 , 2 ), nsmall = 2 ) , 
                               "]", 
                               sep = "") )

PC_output <- Pre_PC_output %>% 
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
  cols_width( "Variable" ~ px( 300 ) , 
              everything() ~ px( 150 ) )

PC_output

#####3.1.6.3.2 Estimate differences ----
tibble( Cohort = c( "<=1960" , "1961-1965" , "1966-1970" , "1971-1975" , 
                    "1976-1980" , "1981-1985" , "1986-1990" , ">1990" ) ,
        bPC_diff = c( sprintf("%0.2f" , mean( Post_model_PC$bPC[,2,1] - Post_model_PC$bPC[,1,1] ) ) ,
                      sprintf("%0.2f" , mean( Post_model_PC$bPC[,2,2] - Post_model_PC$bPC[,1,2] ) ) ,
                      sprintf("%0.2f" , mean( Post_model_PC$bPC[,2,3] - Post_model_PC$bPC[,1,3] ) ) ,
                      sprintf("%0.2f" , mean( Post_model_PC$bPC[,2,4] - Post_model_PC$bPC[,1,4] ) ) ,
                      sprintf("%0.2f" , mean( Post_model_PC$bPC[,2,5] - Post_model_PC$bPC[,1,5] ) ) ,
                      sprintf("%0.2f" , mean( Post_model_PC$bPC[,2,6] - Post_model_PC$bPC[,1,6] ) ) ,
                      sprintf("%0.2f" , mean( Post_model_PC$bPC[,2,7] - Post_model_PC$bPC[,1,7] ) ) ,
                      sprintf("%0.2f" , mean( Post_model_PC$bPC[,2,8] - Post_model_PC$bPC[,1,8] ) ) ) ,
        
        bPC_diff_L = c( sprintf("%0.2f" , PI( Post_model_PC$bPC[,2,1] - Post_model_PC$bPC[,1,1] , prob = 0.90 )[1] ) ,
                        sprintf("%0.2f" , PI( Post_model_PC$bPC[,2,2] - Post_model_PC$bPC[,1,2] , prob = 0.90 )[1] ) ,
                        sprintf("%0.2f" , PI( Post_model_PC$bPC[,2,3] - Post_model_PC$bPC[,1,3] , prob = 0.90 )[1] ) ,
                        sprintf("%0.2f" , PI( Post_model_PC$bPC[,2,4] - Post_model_PC$bPC[,1,4] , prob = 0.90 )[1] ) ,
                        sprintf("%0.2f" , PI( Post_model_PC$bPC[,2,5] - Post_model_PC$bPC[,1,5] , prob = 0.90 )[1] ) ,
                        sprintf("%0.2f" , PI( Post_model_PC$bPC[,2,6] - Post_model_PC$bPC[,1,6] , prob = 0.90 )[1] ) ,
                        sprintf("%0.2f" , PI( Post_model_PC$bPC[,2,7] - Post_model_PC$bPC[,1,7] , prob = 0.90 )[1] ) ,
                        sprintf("%0.2f" , PI( Post_model_PC$bPC[,2,8] - Post_model_PC$bPC[,1,8] , prob = 0.90 )[1] ) ) ,
        
        bPC_diff_H = c( sprintf("%0.2f" , PI( Post_model_PC$bPC[,2,1] - Post_model_PC$bPC[,1,1] , prob = 0.90 )[2] ) ,
                        sprintf("%0.2f" , PI( Post_model_PC$bPC[,2,2] - Post_model_PC$bPC[,1,2] , prob = 0.90 )[2] ) ,
                        sprintf("%0.2f" , PI( Post_model_PC$bPC[,2,3] - Post_model_PC$bPC[,1,3] , prob = 0.90 )[2] ) ,
                        sprintf("%0.2f" , PI( Post_model_PC$bPC[,2,4] - Post_model_PC$bPC[,1,4] , prob = 0.90 )[2] ) ,
                        sprintf("%0.2f" , PI( Post_model_PC$bPC[,2,5] - Post_model_PC$bPC[,1,5] , prob = 0.90 )[2] ) ,
                        sprintf("%0.2f" , PI( Post_model_PC$bPC[,2,6] - Post_model_PC$bPC[,1,6] , prob = 0.90 )[2] ) ,
                        sprintf("%0.2f" , PI( Post_model_PC$bPC[,2,7] - Post_model_PC$bPC[,1,7] , prob = 0.90 )[2] ) ,
                        sprintf("%0.2f" , PI( Post_model_PC$bPC[,2,8] - Post_model_PC$bPC[,1,8] , prob = 0.90 )[2] ) ) ,
        `bPC_diff[90%CI]` = paste( bPC_diff , "[" , bPC_diff_L , ", " , bPC_diff_H , "]") , 
        Pro_bPC_diff = c( sprintf( "%0.4f" , length( which( ( Post_model_PC$bPC[,2,1] - Post_model_PC$bPC[,1,1] ) > 0 ) ) / 6000 ) , 
                          sprintf( "%0.4f" , length( which( ( Post_model_PC$bPC[,2,2] - Post_model_PC$bPC[,1,2] ) > 0 ) ) / 6000 ) , 
                          sprintf( "%0.4f" , length( which( ( Post_model_PC$bPC[,2,3] - Post_model_PC$bPC[,1,3] ) > 0 ) ) / 6000 ) , 
                          sprintf( "%0.4f" , length( which( ( Post_model_PC$bPC[,2,4] - Post_model_PC$bPC[,1,4] ) > 0 ) ) / 6000 ) , 
                          sprintf( "%0.4f" , length( which( ( Post_model_PC$bPC[,2,5] - Post_model_PC$bPC[,1,5] ) > 0 ) ) / 6000 ) , 
                          sprintf( "%0.4f" , length( which( ( Post_model_PC$bPC[,2,6] - Post_model_PC$bPC[,1,6] ) > 0 ) ) / 6000 ) , 
                          sprintf( "%0.4f" , length( which( ( Post_model_PC$bPC[,2,7] - Post_model_PC$bPC[,1,7] ) > 0 ) ) / 6000 ) , 
                          sprintf( "%0.4f" , length( which( ( Post_model_PC$bPC[,2,8] - Post_model_PC$bPC[,1,8] ) > 0 ) ) / 6000 ) ) ) %>% 
  mutate( Pro_bPC_diff = paste( round( as.numeric( Pro_bPC_diff ) * 100 , 2 ) , "%" ) ) %>% 
  select( Cohort , `bPC_diff[90%CI]` , Pro_bPC_diff ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels(everything() ) ) %>% 
  tab_style( style = list( cell_text(font = "Times New Roman") ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "center",
              columns = c( Cohort , `bPC_diff[90%CI]` , Pro_bPC_diff ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( everything() ~ px( 150 ) )

####3.1.6.4 Parental marriage type stratified by village and cohort ----
#####3.1.6.4.1 Estimates ----
Pre_PCV_output <- data.frame( Mean = c( Pre_model_PCV$mean ) ,
                              CI5 = c( Pre_model_PCV$`5%` ) ,
                              CI95 = c( Pre_model_PCV$`95%` ) ,
                              Variable = c( "Parents without kidnap marriage: V1 <=1960" , 
                                            "Parents without kidnap marriage: V1 1961-1965" , 
                                            "Parents without kidnap marriage: V1 1966-1970" , 
                                            "Parents without kidnap marriage: V1 1971-1975" , 
                                            "Parents without kidnap marriage: V1 1976-1980" , 
                                            "Parents without kidnap marriage: V1 1981-1985" , 
                                            "Parents without kidnap marriage: V1 1986-1990" , 
                                            "Parents without kidnap marriage: V1 >1990" , 
                                            
                                            "Parents without kidnap marriage: V2 <=1960" , 
                                            "Parents without kidnap marriage: V2 1961-1965" , 
                                            "Parents without kidnap marriage: V2 1966-1970" , 
                                            "Parents without kidnap marriage: V2 1971-1975" , 
                                            "Parents without kidnap marriage: V2 1976-1980" , 
                                            "Parents without kidnap marriage: V2 1981-1985" , 
                                            "Parents without kidnap marriage: V2 1986-1990" , 
                                            "Parents without kidnap marriage: V2 >1990" , 
                                            
                                            "Kidnapping difference: V1 <=1960" , 
                                            "Kidnapping difference: V1 1961-1965" , 
                                            "Kidnapping difference: V1 1966-1970" ,
                                            "Kidnapping difference: V1 1971-1975" , 
                                            "Kidnapping difference: V1 1976-1980" , 
                                            "Kidnapping difference: V1 1981-1985" , 
                                            "Kidnapping difference: V1 1986-1990" , 
                                            "Kidnapping difference: V1 >1990" , 
                                            
                                            "Kidnapping difference: V2 <=1960" , 
                                            "Kidnapping difference: V2 1961-1965" , 
                                            "Kidnapping difference: V2 1966-1970" , 
                                            "Kidnapping difference: V2 1971-1975" , 
                                            "Kidnapping difference: V2 1976-1980" , 
                                            "Kidnapping difference: V2 1981-1985" , 
                                            "Kidnapping difference: V2 1986-1990" , 
                                            "Kidnapping difference: V2 >1990" , 
                                            
                                            "Village 1" , "Village 2" ) ) %>% 
  mutate( Variable = factor( Variable , 
                             levels = c( "Parents without kidnap marriage: V1 <=1960" , 
                                         "Parents without kidnap marriage: V1 1961-1965" , 
                                         "Parents without kidnap marriage: V1 1966-1970" , 
                                         "Parents without kidnap marriage: V1 1971-1975" , 
                                         "Parents without kidnap marriage: V1 1976-1980" , 
                                         "Parents without kidnap marriage: V1 1981-1985" , 
                                         "Parents without kidnap marriage: V1 1986-1990" , 
                                         "Parents without kidnap marriage: V1 >1990" , 
                                         
                                         "Parents without kidnap marriage: V2 <=1960" , 
                                         "Parents without kidnap marriage: V2 1961-1965" , 
                                         "Parents without kidnap marriage: V2 1966-1970" , 
                                         "Parents without kidnap marriage: V2 1971-1975" , 
                                         "Parents without kidnap marriage: V2 1976-1980" , 
                                         "Parents without kidnap marriage: V2 1981-1985" , 
                                         "Parents without kidnap marriage: V2 1986-1990" , 
                                         "Parents without kidnap marriage: V2 >1990" , 
                                         
                                         "Kidnapping difference: V1 <=1960" , 
                                         "Kidnapping difference: V1 1961-1965" , 
                                         "Kidnapping difference: V1 1966-1970" ,
                                         "Kidnapping difference: V1 1971-1975" , 
                                         "Kidnapping difference: V1 1976-1980" , 
                                         "Kidnapping difference: V1 1981-1985" , 
                                         "Kidnapping difference: V1 1986-1990" , 
                                         "Kidnapping difference: V1 >1990" , 
                                         
                                         "Kidnapping difference: V2 <=1960" , 
                                         "Kidnapping difference: V2 1961-1965" , 
                                         "Kidnapping difference: V2 1966-1970" , 
                                         "Kidnapping difference: V2 1971-1975" , 
                                         "Kidnapping difference: V2 1976-1980" , 
                                         "Kidnapping difference: V2 1981-1985" , 
                                         "Kidnapping difference: V2 1986-1990" , 
                                         "Kidnapping difference: V2 >1990" , 
                                         
                                         "Village 1" , "Village 2" ) ,
                             labels = c( "Parents without kidnap marriage: V1 <=1960" , 
                                         "Parents without kidnap marriage: V1 1961-1965" , 
                                         "Parents without kidnap marriage: V1 1966-1970" , 
                                         "Parents without kidnap marriage: V1 1971-1975" , 
                                         "Parents without kidnap marriage: V1 1976-1980" , 
                                         "Parents without kidnap marriage: V1 1981-1985" , 
                                         "Parents without kidnap marriage: V1 1986-1990" , 
                                         "Parents without kidnap marriage: V1 >1990" , 
                                         
                                         "Parents without kidnap marriage: V2 <=1960" , 
                                         "Parents without kidnap marriage: V2 1961-1965" , 
                                         "Parents without kidnap marriage: V2 1966-1970" , 
                                         "Parents without kidnap marriage: V2 1971-1975" , 
                                         "Parents without kidnap marriage: V2 1976-1980" , 
                                         "Parents without kidnap marriage: V2 1981-1985" , 
                                         "Parents without kidnap marriage: V2 1986-1990" , 
                                         "Parents without kidnap marriage: V2 >1990" , 
                                         
                                         "Kidnapping difference: V1 <=1960" , 
                                         "Kidnapping difference: V1 1961-1965" , 
                                         "Kidnapping difference: V1 1966-1970" ,
                                         "Kidnapping difference: V1 1971-1975" , 
                                         "Kidnapping difference: V1 1976-1980" , 
                                         "Kidnapping difference: V1 1981-1985" , 
                                         "Kidnapping difference: V1 1986-1990" , 
                                         "Kidnapping difference: V1 >1990" , 
                                         
                                         "Kidnapping difference: V2 <=1960" , 
                                         "Kidnapping difference: V2 1961-1965" , 
                                         "Kidnapping difference: V2 1966-1970" , 
                                         "Kidnapping difference: V2 1971-1975" , 
                                         "Kidnapping difference: V2 1976-1980" , 
                                         "Kidnapping difference: V2 1981-1985" , 
                                         "Kidnapping difference: V2 1986-1990" , 
                                         "Kidnapping difference: V2 >1990" , 
                                         
                                         "Village 1" , "Village 2" ) ) ,
          Model = "Parental marriage type stratified by village and cohort" , 
          "Mean [CI]" = paste( format( round( Mean , 2 ), nsmall = 2 ) , 
                               " [" , 
                               format( round( CI5 , 2 ), nsmall = 2 ) , 
                               ", " , 
                               format( round( CI95 , 2 ), nsmall = 2 ) , 
                               "]", 
                               sep = "") )

PCV_output <- Pre_PCV_output %>% 
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
  cols_width( "Variable" ~ px( 400 ) , 
              everything() ~ px( 150 ) )

PCV_output

#####3.1.6.4.2 Estimate differences ----
tibble( Village = rep( c( "V1" , "V2" ) , each = 8 ) ,
        Cohort = rep( c( "<=1960" , "1961-1965" , "1966-1970" , "1971-1975" , 
                         "1976-1980" , "1981-1985" , "1986-1990" , ">1990" ) , 2 ) ,
        bPCV = c( sprintf("%0.2f" , mean( Post_model_PCV$bPCV[,1,1] ) ) , 
                  sprintf("%0.2f" , mean( Post_model_PCV$bPCV[,2,1] ) ) ,  
                  sprintf("%0.2f" , mean( Post_model_PCV$bPCV[,3,1] ) ) , 
                  sprintf("%0.2f" , mean( Post_model_PCV$bPCV[,4,1] ) ) ,  
                  sprintf("%0.2f" , mean( Post_model_PCV$bPCV[,5,1] ) ) , 
                  sprintf("%0.2f" , mean( Post_model_PCV$bPCV[,6,1] ) ) ,  
                  sprintf("%0.2f" , mean( Post_model_PCV$bPCV[,7,1] ) ) , 
                  sprintf("%0.2f" , mean( Post_model_PCV$bPCV[,8,1] ) ) ,  
                  
                  sprintf("%0.2f" , mean( Post_model_PCV$bPCV[,1,2] ) ) , 
                  sprintf("%0.2f" , mean( Post_model_PCV$bPCV[,2,2] ) ) ,
                  sprintf("%0.2f" , mean( Post_model_PCV$bPCV[,3,2] ) ) , 
                  sprintf("%0.2f" , mean( Post_model_PCV$bPCV[,4,2] ) ) ,
                  sprintf("%0.2f" , mean( Post_model_PCV$bPCV[,5,2] ) ) , 
                  sprintf("%0.2f" , mean( Post_model_PCV$bPCV[,6,2] ) ) ,
                  sprintf("%0.2f" , mean( Post_model_PCV$bPCV[,7,2] ) ) , 
                  sprintf("%0.2f" , mean( Post_model_PCV$bPCV[,8,2] ) ) ) ,
        
        bPCV_L = c( sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,1,1] , prob = 0.90 )[1] ) , 
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,2,1] , prob = 0.90 )[1] ) ,  
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,3,1] , prob = 0.90 )[1] ) , 
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,4,1] , prob = 0.90 )[1] ) ,  
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,5,1] , prob = 0.90 )[1] ) , 
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,6,1] , prob = 0.90 )[1] ) ,  
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,7,1] , prob = 0.90 )[1] ) , 
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,8,1] , prob = 0.90 )[1] ) ,  
                    
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,1,2] , prob = 0.90 )[1] ) , 
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,2,2] , prob = 0.90 )[1] ) ,
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,3,2] , prob = 0.90 )[1] ) , 
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,4,2] , prob = 0.90 )[1] ) ,
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,5,2] , prob = 0.90 )[1] ) , 
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,6,2] , prob = 0.90 )[1] ) ,
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,7,2] , prob = 0.90 )[1] ) , 
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,8,2] , prob = 0.90 )[1] ) ) ,
        
        bPCV_H = c( sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,1,1] , prob = 0.90 )[2] ) , 
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,2,1] , prob = 0.90 )[2] ) ,  
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,3,1] , prob = 0.90 )[2] ) , 
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,4,1] , prob = 0.90 )[2] ) ,  
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,5,1] , prob = 0.90 )[2] ) , 
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,6,1] , prob = 0.90 )[2] ) ,  
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,7,1] , prob = 0.90 )[2] ) , 
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,8,1] , prob = 0.90 )[2] ) ,  
                    
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,1,2] , prob = 0.90 )[2] ) , 
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,2,2] , prob = 0.90 )[2] ) ,
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,3,2] , prob = 0.90 )[2] ) , 
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,4,2] , prob = 0.90 )[2] ) ,
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,5,2] , prob = 0.90 )[2] ) , 
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,6,2] , prob = 0.90 )[2] ) ,
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,7,2] , prob = 0.90 )[2] ) , 
                    sprintf("%0.2f" , PI( Post_model_PCV$bPCV[,8,2] , prob = 0.90 )[2] ) ) ,
        `bPCV[90%CI]` = paste( bPCV , "[" , bPCV_L , ", " , bPCV_H , "]") ,
        Pro_bPCV = c( sprintf( "%0.4f" , length( which( Post_model_PCV$bPCV[,1,1] > 0 ) ) / 6000 ) , 
                      sprintf( "%0.4f" , length( which( Post_model_PCV$bPCV[,2,1] > 0 ) ) / 6000 ) , 
                      sprintf( "%0.4f" , length( which( Post_model_PCV$bPCV[,3,1] > 0 ) ) / 6000 ) , 
                      sprintf( "%0.4f" , length( which( Post_model_PCV$bPCV[,4,1] > 0 ) ) / 6000 ) , 
                      sprintf( "%0.4f" , length( which( Post_model_PCV$bPCV[,5,1] > 0 ) ) / 6000 ) , 
                      sprintf( "%0.4f" , length( which( Post_model_PCV$bPCV[,6,1] > 0 ) ) / 6000 ) , 
                      sprintf( "%0.4f" , length( which( Post_model_PCV$bPCV[,7,1] > 0 ) ) / 6000 ) , 
                      sprintf( "%0.4f" , length( which( Post_model_PCV$bPCV[,8,1] > 0 ) ) / 6000 ) , 
                      
                      sprintf( "%0.4f" , length( which( Post_model_PCV$bPCV[,1,2] > 0 ) ) / 6000 ) , 
                      sprintf( "%0.4f" , length( which( Post_model_PCV$bPCV[,2,2] > 0 ) ) / 6000 ) ,
                      sprintf( "%0.4f" , length( which( Post_model_PCV$bPCV[,3,2] > 0 ) ) / 6000 ) , 
                      sprintf( "%0.4f" , length( which( Post_model_PCV$bPCV[,4,2] > 0 ) ) / 6000 ) ,
                      sprintf( "%0.4f" , length( which( Post_model_PCV$bPCV[,5,2] > 0 ) ) / 6000 ) , 
                      sprintf( "%0.4f" , length( which( Post_model_PCV$bPCV[,6,2] > 0 ) ) / 6000 ) ,
                      sprintf( "%0.4f" , length( which( Post_model_PCV$bPCV[,7,2] > 0 ) ) / 6000 ) , 
                      sprintf( "%0.4f" , length( which( Post_model_PCV$bPCV[,8,2] > 0 ) ) / 6000 ) ) ) %>% 
  mutate( Pro_bPCV = paste( round( as.numeric( Pro_bPCV ) * 100 , 2 ) , "%" ) ) %>% 
  select( Village , Cohort , `bPCV[90%CI]` , Pro_bPCV ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels(everything() ) ) %>% 
  tab_style( style = list( cell_text(font = "Times New Roman") ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "center",
              columns = c( Village , Cohort , `bPCV[90%CI]` , Pro_bPCV ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( everything() ~ px( 150 ) )

##3.2 Siblings ----
adjustmentSets( Dag , 
                exposure = "Sibship" , 
                outcome = "Marriage" ,
                effect = "total" )
adjustmentSets( Dag , 
                exposure = "Marriage of older siblings" , 
                outcome = "Marriage" ,
                effect = "total" )

Model_Sib_list <- with( Transmit.data , list(
  Kidnapping = as.integer( Kidnapping ) , 
  Gender = as.integer( Gender ) , 
  Cohort = as.integer( BY.5c ) ,
  P_kidnapping = as.integer( P.kidnapping ) + 1 ,
  Bro = as.numeric( No.bro.std ) ,
  Sis = as.numeric( No.sis.std ) ,
  Older_bro_kidnap = as.numeric( No.older.bro.kidnap.std ) ,
  Older_sis_kidnap = as.numeric( No.older.sis.kidnap.std ) ,
  SibID = as.integer( SibID ) ,
  VID = as.integer( VID ) ) )

###3.2.1 Number of siblings stratified by gender ----
{set.seed(123)
  Model_SibG <- ulam(
    alist(
      Kidnapping ~ bernoulli( p ) ,
      logit( p ) <- bSisG[Gender] * Sis + bBroG[Gender] * Bro + 
        bPG[P_kidnapping,Gender] + bC[Cohort] + 
        bV[VID] + bSib[SibID] , # random effects at village- and sibship-level
      
      bSisG[Gender] ~ normal( 0 , 0.5 ) ,
      bBroG[Gender] ~ normal( 0 , 0.5 ) ,
      
      bC[Cohort] ~ normal( 0 , 1 ) ,
      matrix[P_kidnapping,Gender]: bPG ~ normal( 0 , 0.6 ) ,
      
      # define effects of villages using other parameters
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V ,
      transpars> vector[SibID]: bSib <<- z_sib*sigma_sib ,
      
      V_bar ~ normal( 0 , 0.8 ) ,
      z_V[VID] ~ normal( 1 , 0.5 ) ,
      sigma_V ~ exponential( 1 ) ,
      z_sib[SibID] ~ normal( 0 , 0.8 ) ,
      sigma_sib ~ exponential( 1 ) 
    ) , data = Model_Sib_list , 
    iter = 1500 , warmup = 500 , chains = 6 , cores = 6 , 
    log_lik = TRUE , control = list( adapt_delta = 0.99 ) 
  ) }
Pre_model_SibG <- precis( Model_SibG , depth = 3 , prob = 0.90 , 
                          pars = c( "bSisG" , "bBroG" , "bPG" , "bC" , "bV" ) )
Pre_model_SibG

###3.2.2 Number of siblings stratified by gender and village ----
{set.seed(123)
  Model_SibGV <- ulam(
    alist(
      Kidnapping ~ bernoulli( p ) ,
      logit( p ) <- bSisGV[Gender,VID] * Sis + bBroGV[Gender,VID] * Bro + 
        bPG[P_kidnapping,Gender] + bC[Cohort] + 
        bV[VID] + bSib[SibID] , # random effects at village- and sibship-level
      
      matrix[Gender,VID]: bSisGV ~ normal( 0 , 0.5 ) ,
      matrix[Gender,VID]: bBroGV ~ normal( 0 , 0.5 ) ,
      
      bC[Cohort] ~ normal( 0 , 1 ) ,
      matrix[P_kidnapping,Gender]: bPG ~ normal( 0 , 0.6 ) ,
      
      # define effects of villages using other parameters
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V ,
      transpars> vector[SibID]: bSib <<- z_sib*sigma_sib ,
      
      V_bar ~ normal( 0 , 0.8 ) ,
      z_V[VID] ~ normal( 1 , 0.5 ) ,
      sigma_V ~ exponential( 1 ) ,
      z_sib[SibID] ~ normal( 0 , 0.8 ) ,
      sigma_sib ~ exponential( 1 ) 
    ) , data = Model_Sib_list , 
    iter = 1500 , warmup = 500 , chains = 6 , cores = 6 , 
    log_lik = TRUE , control = list( adapt_delta = 0.99 ) 
  ) }
Pre_model_SibGV <- precis( Model_SibGV , depth = 3 , prob = 0.90 , 
                           pars = c( "bSisGV" , "bBroGV" , "bPG" , "bC" , "bV" ) )
Pre_model_SibGV

###3.2.3 Older siblings' marriage type stratified by gender ----
{set.seed(123)
  Model_SibMG <- ulam(
    alist(
      Kidnapping ~ bernoulli( p ) ,
      logit( p ) <- bOSisMG[Gender] * Older_sis_kidnap + 
        bOBroMG[Gender] * Older_bro_kidnap +
        bSisG[Gender] * Sis + bBroG[Gender] * Bro + 
        bPG[P_kidnapping,Gender] + bC[Cohort] + 
        bV[VID] + bSib[SibID] , # random effects at village- and sibship-level
      
      bOSisMG[Gender] ~ normal( 0 , 0.5 ) ,
      bOBroMG[Gender] ~ normal( 0 , 0.5 ) ,
      
      bSisG[Gender] ~ normal( 0 , 0.5 ) ,
      bBroG[Gender] ~ normal( 0 , 0.5 ) ,
      
      bC[Cohort] ~ normal( 0 , 1 ) ,
      matrix[P_kidnapping,Gender]: bPG ~ normal( 0 , 0.6 ) ,
      
      # define effects of villages using other parameters
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V ,
      transpars> vector[SibID]: bSib <<- z_sib*sigma_sib ,
      
      V_bar ~ normal( 0 , 0.8 ) ,
      z_V[VID] ~ normal( 1 , 0.5 ) ,
      sigma_V ~ exponential( 1 ) ,
      z_sib[SibID] ~ normal( 0 , 0.8 ) ,
      sigma_sib ~ exponential( 1 ) 
    ) , data = Model_Sib_list , 
    iter = 1500 , warmup = 500 , chains = 6 , cores = 6 , 
    log_lik = TRUE , control = list( adapt_delta = 0.99 ) 
  ) }
Pre_model_SibMG <- precis( Model_SibMG , depth = 3 , prob = 0.90 , 
                           pars = c( "bOSisMG" , "bOBroMG" , 
                                     "bSisG" , "bBroG" , "bPG" , "bC" , "bV" ) )
Pre_model_SibMG

###3.2.4 Older siblings' marriage type stratified by village and gender ----
{set.seed(123)
  Model_SibMGV <- ulam(
    alist(
      Kidnapping ~ bernoulli( p ) ,
      logit( p ) <- bOSisMGV[Gender,VID] * Older_sis_kidnap + 
        bOBroMGV[Gender,VID] * Older_bro_kidnap +
        bSisG[Gender] * Sis + bBroG[Gender] * Bro + 
        bPG[P_kidnapping,Gender] + bC[Cohort] + 
        bV[VID] + bSib[SibID] , # random effects at village- and sibship-level
      
      matrix[Gender,VID]: bOSisMGV ~ normal( 0 , 0.5 ) ,
      matrix[Gender,VID]: bOBroMGV ~ normal( 0 , 0.5 ) ,
      
      bSisG[Gender] ~ normal( 0 , 0.5 ) ,
      bBroG[Gender] ~ normal( 0 , 0.5 ) ,
      
      bC[Cohort] ~ normal( 0 , 1 ) ,
      matrix[P_kidnapping,Gender]: bPG ~ normal( 0 , 0.6 ) ,
      
      # define effects of villages using other parameters
      transpars> vector[VID]: bV <<- V_bar + z_V*sigma_V ,
      transpars> vector[SibID]: bSib <<- z_sib*sigma_sib ,
      
      V_bar ~ normal( 0 , 0.8 ) ,
      z_V[VID] ~ normal( 1 , 0.5 ) ,
      sigma_V ~ exponential( 1 ) ,
      z_sib[SibID] ~ normal( 0 , 0.8 ) ,
      sigma_sib ~ exponential( 1 ) 
    ) , data = Model_Sib_list , 
    iter = 1500 , warmup = 500 , chains = 6 , cores = 6 , 
    log_lik = TRUE , control = list( adapt_delta = 0.99 ) 
  ) }
Pre_model_SibMGV <- precis( Model_SibMGV , depth = 3 , prob = 0.90 , 
                            pars = c( "bOSisMGV" , "bOBroMGV" , 
                                      "bSisG" , "bBroG" , "bPG" , "bC" , "bV" ) )
Pre_model_SibMGV

###3.2.4 Figures ----
Post_model_SibG <- extract.samples( Model_SibG )
Post_model_SibGV <- extract.samples( Model_SibGV )

Post_model_SibMG <- extract.samples( Model_SibMG )
Post_model_SibMGV <- extract.samples( Model_SibMGV )

####3.2.4.1 Number of siblings stratified by gender ----
#####3.2.4.1.1 Posterior distribution of estimates ----
tibble( Value = c( Post_model_SibG$bSisG[,1] , 
                   Post_model_SibG$bSisG[,2] , 
                   Post_model_SibG$bBroG[,1] , 
                   Post_model_SibG$bBroG[,2] ) ,
        Sib_gender = c( rep( "Sisters : female" , 6000 ) ,
                        rep( "Sisters : male" , 6000 ) , 
                        rep( "Brothers : female" , 6000 ) ,
                        rep( "Brothers : male" , 6000 ) ) ) %>% 
  mutate( Sib_gender = factor( Sib_gender , 
                               levels = c( "Brothers : male" , "Brothers : female" , 
                                           "Sisters : male" , "Sisters : female" ) , 
                               labels = c( "Brothers : male" , "Brothers : female" , 
                                           "Sisters : male" , "Sisters : female" ) ) ) %>% 
  ggplot( aes( x = Value , y = Sib_gender , 
               color = Sib_gender , fill = Sib_gender ) ) +
  geom_vline( xintercept = 0, linetype = 2 , linewidth = 1 ,
              color = "dimgray" ) +
  stat_halfeye( .width = .90, height = 0.8 ) + 
  scale_fill_manual( values = alpha( c( "#193E8F", "#E53528" , "#193E8F" , "#E53528" ) , 0.3 ) ) +
  scale_color_manual( values = c( "#193E8F", "#E53528" , "#193E8F" , "#E53528" ) ) +
  scale_y_discrete( breaks = c( "Brothers : male" , "Brothers : female" , 
                                "Sisters : male" , "Sisters : female" ) ,
                    labels = c( "Brothers : male" , "Brothers : female" , 
                                "Sisters : male" , "Sisters : female" ) ) +
  labs( x = "Posterior distribution of estimates" ,
        y = "Sibling type : gender" ) +
  coord_cartesian( ylim = c( 1.5, 4.5 ) ) +
  scale_x_continuous( limits = c( -0.8 , 0.6 ) , breaks = c( -0.5 , 0 , 0.5 ) ) +
  theme(plot.margin = margin(10, 10, 10, 20),
        strip.background = element_rect(color = "black", fill = "white") ,
        strip.text.x = element_text(size = 16),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "Gainsboro"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour="black",fill=NA),
        panel.background = element_blank(),
        plot.title = element_text(size=16),
        legend.position = "none",
        axis.title.x = element_text(size = 18,
                                    margin = margin(t = 0.3, r = 0, b = 0, l = 0,unit = "cm")),
        axis.text.x = element_text(colour="black",size=16,
                                   margin = margin(t = 0.1, r = 0, b = 0, l = 0,unit = "cm")),
        axis.title.y = element_text(size = 18,
                                    margin = margin(t = 0, r = 0.5, b = 0, l = 0,unit = "cm")),
        axis.text.y = element_text(colour="black",size=16,
                                   margin = margin(t = 0, r = 0.2, b = 0, l = 0,unit = "cm")),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12))

#####3.2.4.1.2 Predicted probability ----
# Function to generate new data allowing to specify different values from models of sibling configuration
generate_and_predict_p_matrix_SisG <- function( post_samples , Gender_value = 1, n_samples = 6000 ) {
  
  # Generate new data
  new_data <- data.frame(
    Gender = as.integer( rep( Gender_value , 1296 ) ) ,
    P_kidnapping = as.integer( rep( 1:2 , 648 ) ) ,
    Cohort = as.integer( rep( 1:8 , 162 ) ) ,
    Sis_o = as.integer( rep( 0:8 , each = 144 ) ) ,
    Sis = as.numeric( sprintf( "%0.4f" , ( as.integer( rep( 0:8 , each = 144 ) ) - 3.272835 )/ 1.845436 ) )  ,
    Bro_o = as.integer( rep( 0:8 , 144 ) ) ,
    Bro = as.numeric( sprintf( "%0.4f" , ( as.integer( rep( 0:8 , 144 ) ) - 2.67497 )/ 1.61629 ) ) 
  )
  
  # Initialize p_matrix
  n_Sis <- length( unique( new_data$Sis_o ) )
  p_matrix <- matrix( NA , nrow = n_samples , ncol = n_Sis )
  
  # Loop over posterior samples
  for ( s in 1:n_samples ) {
    
    # Extract posterior sample for current iteration
    bSisG <- post_samples$bSisG[s, ]
    bBroG <- post_samples$bBroG[s, ]
    bPG <- post_samples$bPG[s, , ]
    bC <- post_samples$bC[s, ]
    V_bar <- post_samples$V_bar[s, 1]
    
    # Loop over each Sis
    for ( m in 1:n_Sis ) {
      Sis_subset <- new_data[ new_data$Sis_o == ( m - 1 ) , ]  # Subset data for Sis_o
      logit_p_vals <- with( Sis_subset,
                            bSisG[Gender] * Sis + 
                              bBroG[Gender] * Bro + 
                              bPG[cbind(P_kidnapping, Gender)] +
                              bC[Cohort] + 
                              V_bar )
      
      # Convert to probabilities and average over other variables
      p_matrix[s, m] <- mean(rethinking::inv_logit( logit_p_vals ) , na.rm = TRUE )
    }
  }
  
  return(p_matrix)
}

Predicted_p_SibG_sis_F <- generate_and_predict_p_matrix_SisG( post_samples = Post_model_SibG , 
                                                              Gender_value = 1 )
Predicted_p_SibG_sis_M <- generate_and_predict_p_matrix_SisG( post_samples = Post_model_SibG , 
                                                              Gender_value = 2 )

generate_and_predict_p_matrix_BroG <- function( post_samples , Gender_value = 1, n_samples = 6000 ) {
  
  # Generate new data
  new_data <- data.frame(
    Gender = as.integer( rep( Gender_value , 1296 ) ) ,
    P_kidnapping = as.integer( rep( 1:2 , 648 ) ) ,
    Cohort = as.integer( rep( 1:8 , 162 ) ) ,
    Sis_o = as.integer( rep( 0:8 , 144 ) ) ,
    Sis = as.numeric( sprintf( "%0.4f" , ( as.integer( rep( 0:8 , 144 ) ) - 3.272835 )/ 1.845436 ) )  ,
    Bro_o = as.integer( rep( 0:8 , each = 144 ) ) ,
    Bro = as.numeric( sprintf( "%0.4f" , ( as.integer( rep( 0:8 , each = 144 ) ) - 2.67497 )/ 1.61629 ) ) 
  )
  
  # Initialize p_matrix
  n_bro <- length( unique( new_data$Bro_o ) )
  p_matrix <- matrix( NA , nrow = n_samples , ncol = n_bro )
  
  # Loop over posterior samples
  for ( s in 1:n_samples ) {
    
    # Extract posterior sample for current iteration
    bSisG <- post_samples$bSisG[s, ]
    bBroG <- post_samples$bBroG[s, ]
    bPG <- post_samples$bPG[s, , ]
    bC <- post_samples$bC[s, ]
    V_bar <- post_samples$V_bar[s, 1]
    
    # Loop over each Bro
    for ( m in 1:n_bro ) {
      bro_subset <- new_data[ new_data$Bro_o == ( m - 1 ) , ]  # Subset data for Bro_o
      logit_p_vals <- with( bro_subset,
                            bSisG[Gender] * Sis + 
                              bBroG[Gender] * Bro + 
                              bPG[cbind(P_kidnapping, Gender)] +
                              bC[Cohort] + 
                              V_bar )
      
      # Convert to probabilities and average over other variables
      p_matrix[s, m] <- mean(rethinking::inv_logit( logit_p_vals ) , na.rm = TRUE )
    }
  }
  
  return(p_matrix)
}

Predicted_p_SibG_bro_F <- generate_and_predict_p_matrix_BroG( post_samples = Post_model_SibG , 
                                                              Gender_value = 1 )
Predicted_p_SibG_bro_M <- generate_and_predict_p_matrix_BroG( post_samples = Post_model_SibG , 
                                                              Gender_value = 2 )

####3.2.4.2 Number of siblings stratified by gender and village ----
#####3.2.4.2.1 Posterior distribution of estimates ----
tibble( Value = c( Post_model_SibGV$bSisGV[,1,1] , 
                   Post_model_SibGV$bSisGV[,2,1] , 
                   Post_model_SibGV$bSisGV[,1,2] , 
                   Post_model_SibGV$bSisGV[,2,2] , 
                   Post_model_SibGV$bBroGV[,1,1] , 
                   Post_model_SibGV$bBroGV[,2,1] ,
                   Post_model_SibGV$bBroGV[,1,2] , 
                   Post_model_SibGV$bBroGV[,2,2] ) ,
        Sib_gender = c( rep( "Sisters : female" , 6000 ) ,
                        rep( "Sisters : male" , 6000 ) , 
                        rep( "Sisters : female" , 6000 ) ,
                        rep( "Sisters : male" , 6000 ) , 
                        rep( "Brothers : female" , 6000 ) ,
                        rep( "Brothers : male" , 6000 ) , 
                        rep( "Brothers : female" , 6000 ) ,
                        rep( "Brothers : male" , 6000 ) ) , 
        Village = c( rep( "Village 1" , 12000 ) ,
                     rep( "Village 2" , 12000 ) , 
                     rep( "Village 1" , 12000 ) ,
                     rep( "Village 2" , 12000 ) ) ) %>% 
  mutate( Sib_gender = factor( Sib_gender , 
                               levels = c( "Brothers : male" , "Brothers : female" , 
                                           "Sisters : male" , "Sisters : female" ) , 
                               labels = c( "Brothers : male" , "Brothers : female" , 
                                           "Sisters : male" , "Sisters : female" ) ) , 
          Village = factor( Village , 
                            levels = c( "Village 1" , "Village 2" ) , 
                            labels = c( "Village 1" , "Village 2" ) ) ) %>% 
  ggplot( aes( x = Value , y = Sib_gender , 
               color = Sib_gender , fill = Sib_gender ) ) +
  facet_wrap( ~ Village , nrow = 1 ) + 
  geom_vline( xintercept = 0, linetype = 2 , linewidth = 1 ,
              color = "dimgray" ) +
  stat_halfeye( .width = .90, height = 0.8 ) + 
  scale_fill_manual( values = alpha( c( "#193E8F", "#E53528" , "#193E8F" , "#E53528" ) , 0.3 ) ) +
  scale_color_manual( values = c( "#193E8F", "#E53528" , "#193E8F" , "#E53528" ) ) +
  scale_y_discrete( breaks = c( "Brothers : male" , "Brothers : female" , 
                                "Sisters : male" , "Sisters : female" ) ,
                    labels = c( "Brothers : male" , "Brothers : female" , 
                                "Sisters : male" , "Sisters : female" ) ) +
  labs( x = "Posterior distribution of estimates" ,
        y = "Sibling type : gender" ) +
  coord_cartesian( ylim = c( 1.5, 4.5 ) ) +
  scale_x_continuous( limits = c( -1 , 1 ) , breaks = c( -1 , 0 , 1 ) ) +
  theme(plot.margin = margin(10, 10, 10, 20),
        strip.background = element_rect(color = "black", fill = "white") ,
        strip.text.x = element_text(size = 16),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "Gainsboro"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour="black",fill=NA),
        panel.background = element_blank(),
        plot.title = element_text(size=16),
        legend.position = "none",
        axis.title.x = element_text(size = 18,
                                    margin = margin(t = 0.3, r = 0, b = 0, l = 0,unit = "cm")),
        axis.text.x = element_text(colour="black",size=16,
                                   margin = margin(t = 0.1, r = 0, b = 0, l = 0,unit = "cm")),
        axis.title.y = element_text(size = 18,
                                    margin = margin(t = 0, r = 0.5, b = 0, l = 0,unit = "cm")),
        axis.text.y = element_text(colour="black",size=16,
                                   margin = margin(t = 0, r = 0.2, b = 0, l = 0,unit = "cm")),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12))

#####3.2.4.2.2 Predicted probability ----
# Function to generate new data allowing to specify different values from models of sibling configuration
generate_and_predict_p_matrix_SisGV <- function( post_samples , Gender_value = 1, V_value = 1, n_samples = 6000 ) {
  
  # Generate new data
  new_data <- data.frame(
    Gender = as.integer( rep( Gender_value , 1296 ) ) ,
    Village = as.integer( rep( V_value , 1296 ) ) ,
    P_kidnapping = as.integer( rep( 1:2 , 648 ) ) ,
    Cohort = as.integer( rep( 1:8 , 162 ) ) ,
    Sis_o = as.integer( rep( 0:8 , each = 144 ) ) ,
    Sis = as.numeric( sprintf( "%0.4f" , ( as.integer( rep( 0:8 , each = 144 ) ) - 3.272835 )/ 1.845436 ) )  ,
    Bro_o = as.integer( rep( 0:8 , 144 ) ) ,
    Bro = as.numeric( sprintf( "%0.4f" , ( as.integer( rep( 0:8 , 144 ) ) - 2.67497 )/ 1.61629 ) ) 
  )
  
  # Initialize p_matrix
  n_Sis <- length( unique( new_data$Sis_o ) )
  p_matrix <- matrix( NA , nrow = n_samples , ncol = n_Sis )
  
  # Loop over posterior samples
  for ( s in 1:n_samples ) {
    
    # Extract posterior sample for current iteration
    bSisGV <- post_samples$bSisGV[s, , ]
    bBroGV <- post_samples$bBroGV[s, , ]
    bPG <- post_samples$bPG[s, , ]
    bC <- post_samples$bC[s, ]
    V_bar <- post_samples$V_bar[s, 1]
    
    # Loop over each Sis
    for ( m in 1:n_Sis ) {
      Sis_subset <- new_data[ new_data$Sis_o == ( m - 1 ) , ]  # Subset data for Sis_o
      logit_p_vals <- with( Sis_subset,
                            bSisGV[cbind(Gender,Village)] * Sis + 
                              bBroGV[cbind(Gender,Village)] * Bro + 
                              bPG[cbind(P_kidnapping, Gender)] +
                              bC[Cohort] + 
                              V_bar )
      
      # Convert to probabilities and average over other variables
      p_matrix[s, m] <- mean(rethinking::inv_logit( logit_p_vals ) , na.rm = TRUE )
    }
  }
  
  return(p_matrix)
}

Predicted_p_SibGV_sis_FV1 <- generate_and_predict_p_matrix_SisGV( post_samples = Post_model_SibGV , 
                                                                  Gender_value = 1 , V_value = 1 )
Predicted_p_SibGV_sis_MV1 <- generate_and_predict_p_matrix_SisGV( post_samples = Post_model_SibGV , 
                                                                  Gender_value = 2 , V_value = 1 )
Predicted_p_SibGV_sis_FV2 <- generate_and_predict_p_matrix_SisGV( post_samples = Post_model_SibGV , 
                                                                  Gender_value = 1 , V_value = 2 )
Predicted_p_SibGV_sis_MV2 <- generate_and_predict_p_matrix_SisGV( post_samples = Post_model_SibGV , 
                                                                  Gender_value = 2 , V_value = 2 )

generate_and_predict_p_matrix_BroGV <- function( post_samples , Gender_value = 1 , V_value = 1 , n_samples = 6000 ) {
  
  # Generate new data
  new_data <- data.frame(
    Gender = as.integer( rep( Gender_value , 1296 ) ) ,
    Village = as.integer( rep( V_value , 1296 ) ) ,
    P_kidnapping = as.integer( rep( 1:2 , 648 ) ) ,
    Cohort = as.integer( rep( 1:8 , 162 ) ) ,
    Sis_o = as.integer( rep( 0:8 , 144 ) ) ,
    Sis = as.numeric( sprintf( "%0.4f" , ( as.integer( rep( 0:8 , 144 ) ) - 3.272835 )/ 1.845436 ) )  ,
    Bro_o = as.integer( rep( 0:8 , each = 144 ) ) ,
    Bro = as.numeric( sprintf( "%0.4f" , ( as.integer( rep( 0:8 , each = 144 ) ) - 2.67497 )/ 1.61629 ) ) 
  )
  
  # Initialize p_matrix
  n_bro <- length( unique( new_data$Bro_o ) )
  p_matrix <- matrix( NA , nrow = n_samples , ncol = n_bro )
  
  # Loop over posterior samples
  for ( s in 1:n_samples ) {
    
    # Extract posterior sample for current iteration
    bSisGV <- post_samples$bSisGV[s, , ]
    bBroGV <- post_samples$bBroGV[s, , ]
    bPG <- post_samples$bPG[s, , ]
    bC <- post_samples$bC[s, ]
    V_bar <- post_samples$V_bar[s, 1]
    
    # Loop over each Bro
    for ( m in 1:n_bro ) {
      bro_subset <- new_data[ new_data$Bro_o == ( m - 1 ) , ]  # Subset data for Bro_o
      logit_p_vals <- with( bro_subset,
                            bSisGV[cbind(Gender,Village)] * Sis + 
                              bBroGV[cbind(Gender,Village)] * Bro + 
                              bPG[cbind(P_kidnapping, Gender)] +
                              bC[Cohort] + 
                              V_bar )
      
      # Convert to probabilities and average over other variables
      p_matrix[s, m] <- mean(rethinking::inv_logit( logit_p_vals ) , na.rm = TRUE )
    }
  }
  
  return(p_matrix)
}

Predicted_p_SibGV_bro_FV1 <- generate_and_predict_p_matrix_BroGV( post_samples = Post_model_SibGV , 
                                                                  Gender_value = 1 , V_value = 1 )
Predicted_p_SibGV_bro_MV1 <- generate_and_predict_p_matrix_BroGV( post_samples = Post_model_SibGV , 
                                                                  Gender_value = 2 , V_value = 1 )
Predicted_p_SibGV_bro_FV2 <- generate_and_predict_p_matrix_BroGV( post_samples = Post_model_SibGV , 
                                                                  Gender_value = 1 , V_value = 2 )
Predicted_p_SibGV_bro_MV2 <- generate_and_predict_p_matrix_BroGV( post_samples = Post_model_SibGV , 
                                                                  Gender_value = 2 , V_value = 2 )

{ par( mfrow = c( 2 , 2 ) , oma = c( 4 , 4 , 0 , 0 ) , mar = c( 0.4 , 0.4 , 0.4 , 0.4 ) )
  # Village 1
  # Number of sisters
  plot( NULL,
        xlim = c(0, 8),
        ylim = c(0,1),
        xaxt = "n" , 
        xlab = "" , 
        ylab = "" ,
        cex.axis = 1.2 ,
        cex.lab = 1.5 ,
        cex.main = 1.5 , 
        mgp = c( 2.5 , 0.8 , 0 ) )
  mtext( "Village 1" , side = 2 , line = 2.4 , cex = 1.5 )
  
  # female
  lines( c(0:8) , 
         apply( Predicted_p_SibGV_sis_FV1 , 2 , mean ) ,
         lwd = 2 , col = "#E53528" )
  shade( apply( Predicted_p_SibGV_sis_FV1 , 2 , PI , prob = 0.90 ) , 
         c(0:8) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  shade( apply( Predicted_p_SibGV_sis_FV1 , 2 , PI , prob = 0.60 ) , 
         c(0:8) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  shade( apply( Predicted_p_SibGV_sis_FV1 , 2 , PI , prob = 0.30 ) , 
         c(0:8) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  # male
  lines( c(0:8) , 
         apply( Predicted_p_SibGV_sis_MV1 , 2 , mean ) ,
         lwd = 2 , col = "#193E8F" )
  shade( apply( Predicted_p_SibGV_sis_MV1 , 2 , PI , prob = 0.90 ) , 
         c(0:8) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  shade( apply( Predicted_p_SibGV_sis_MV1 , 2 , PI , prob = 0.60 ) , 
         c(0:8) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  shade( apply( Predicted_p_SibGV_sis_MV1 , 2 , PI , prob = 0.30 ) , 
         c(0:8) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  
  # Number of brothers
  plot( NULL,
        xlim = c(0, 8),
        ylim = c(0,1),
        xaxt = "n" , 
        yaxt = "n" , 
        xlab = "" , 
        ylab = "" ,
        cex.axis = 1.2 ,
        cex.lab = 1.5 ,
        cex.main = 1.5 , 
        mgp = c( 2.5 , 0.8 , 0 ) )
  
  # female
  lines( c(0:8) , 
         apply( Predicted_p_SibGV_bro_FV1 , 2 , mean ) ,
         lwd = 2 , col = "#E53528" )
  shade( apply( Predicted_p_SibGV_bro_FV1 , 2 , PI , prob = 0.90 ) , 
         c(0:8) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  shade( apply( Predicted_p_SibGV_bro_FV1 , 2 , PI , prob = 0.60 ) , 
         c(0:8) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  shade( apply( Predicted_p_SibGV_bro_FV1 , 2 , PI , prob = 0.30 ) , 
         c(0:8) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  # male
  lines( c(0:8) , 
         apply( Predicted_p_SibGV_bro_MV1 , 2 , mean ) ,
         lwd = 2 , col = "#193E8F" )
  shade( apply( Predicted_p_SibGV_bro_MV1 , 2 , PI , prob = 0.90 ) , 
         c(0:8) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  shade( apply( Predicted_p_SibGV_bro_MV1 , 2 , PI , prob = 0.60 ) , 
         c(0:8) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  shade( apply( Predicted_p_SibGV_bro_MV1 , 2 , PI , prob = 0.30 ) , 
         c(0:8) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  legend( x = 6.2 , y = 1.05 , 
          box.col = "white",
          legend = c( "Female" , "Male" ) , 
          lty = c( 1 , 1 ) ,  
          col = c( "#E53528" , "#193E8F" ) , 
          lwd = 2 ,
          cex = 1.2 , 
          bty = "n" ,
          y.intersp = 1.1 ,
          x.intersp = 0.3 ,
          seg.len = 1.2 )
  
  # Village 2
  # Number of sisters
  plot( NULL,
        xlim = c(0, 8),
        ylim = c(0,1),
        xlab = "" , 
        ylab = "" ,
        cex.axis = 1.2 ,
        cex.lab = 1.5 ,
        cex.main = 1.5 , 
        mgp = c( 2.5 , 0.8 , 0 ) )
  mtext( "Number of sisters" , side = 1 , line = 2.4 , cex = 1.5 )
  mtext( "Village 2" , side = 2 , line = 2.4 , cex = 1.5 )
  
  # female
  lines( c(0:8) , 
         apply( Predicted_p_SibGV_sis_FV2 , 2 , mean ) ,
         lwd = 2 , col = "#E53528" )
  shade( apply( Predicted_p_SibGV_sis_FV2 , 2 , PI , prob = 0.90 ) , 
         c(0:8) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  shade( apply( Predicted_p_SibGV_sis_FV2 , 2 , PI , prob = 0.60 ) , 
         c(0:8) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  shade( apply( Predicted_p_SibGV_sis_FV2 , 2 , PI , prob = 0.30 ) , 
         c(0:8) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  # male
  lines( c(0:8) , 
         apply( Predicted_p_SibGV_sis_MV2 , 2 , mean ) ,
         lwd = 2 , col = "#193E8F" )
  shade( apply( Predicted_p_SibGV_sis_MV2 , 2 , PI , prob = 0.90 ) , 
         c(0:8) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  shade( apply( Predicted_p_SibGV_sis_MV2 , 2 , PI , prob = 0.60 ) , 
         c(0:8) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  shade( apply( Predicted_p_SibGV_sis_MV2 , 2 , PI , prob = 0.30 ) , 
         c(0:8) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  
  # Number of brothers
  plot( NULL,
        xlim = c(0, 8),
        ylim = c(0,1),
        yaxt = "n" , 
        xlab = "" , 
        ylab = "" ,
        cex.axis = 1.2 ,
        cex.lab = 1.5 ,
        cex.main = 1.5 , 
        mgp = c( 2.5 , 0.8 , 0 ) )
  mtext( "Number of brothers" , side = 1 , line = 2.4 , cex = 1.5 )
  
  # female
  lines( c(0:8) , 
         apply( Predicted_p_SibGV_bro_FV2 , 2 , mean ) ,
         lwd = 2 , col = "#E53528" )
  shade( apply( Predicted_p_SibGV_bro_FV2 , 2 , PI , prob = 0.90 ) , 
         c(0:8) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  shade( apply( Predicted_p_SibGV_bro_FV2 , 2 , PI , prob = 0.60 ) , 
         c(0:8) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  shade( apply( Predicted_p_SibGV_bro_FV2 , 2 , PI , prob = 0.30 ) , 
         c(0:8) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  # male
  lines( c(0:8) , 
         apply( Predicted_p_SibGV_bro_MV2 , 2 , mean ) ,
         lwd = 2 , col = "#193E8F" )
  shade( apply( Predicted_p_SibGV_bro_MV2 , 2 , PI , prob = 0.90 ) , 
         c(0:8) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  shade( apply( Predicted_p_SibGV_bro_MV2 , 2 , PI , prob = 0.60 ) , 
         c(0:8) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  shade( apply( Predicted_p_SibGV_bro_MV2 , 2 , PI , prob = 0.30 ) , 
         c(0:8) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
}

dev.off()

####3.2.4.3 Older siblings' marriage type stratified by gender ----
#####3.2.4.3.1 Posterior distribution of estimates ----
tibble( Value = c( Post_model_SibMG$bOSisMG[,1] , 
                   Post_model_SibMG$bOSisMG[,2] , 
                   Post_model_SibMG$bOBroMG[,1] , 
                   Post_model_SibMG$bOBroMG[,2] ) ,
        SibM_gender = c( rep( "Older sisters with kidnap marriage : female" , 6000 ) ,
                         rep( "Older sisters with kidnap marriage : male" , 6000 ) , 
                         rep( "Older brothers with kidnap marriage : female" , 6000 ) ,
                         rep( "Older brothers with kidnap marriage : male" , 6000 ) ) ) %>% 
  mutate( SibM_gender = factor( SibM_gender , 
                                levels = c( "Older brothers with kidnap marriage : male" , 
                                            "Older brothers with kidnap marriage : female" , 
                                            "Older sisters with kidnap marriage : male" , 
                                            "Older sisters with kidnap marriage : female" ) , 
                                labels = c( "Older brothers with kidnap marriage : male" , 
                                            "Older brothers with kidnap marriage : female" , 
                                            "Older sisters with kidnap marriage : male" , 
                                            "Older sisters with kidnap marriage : female" ) ) ) %>% 
  ggplot( aes( x = Value , y = SibM_gender , 
               color = SibM_gender , fill = SibM_gender ) ) +
  geom_vline( xintercept = 0, linetype = 2 , linewidth = 1 ,
              color = "dimgray" ) +
  stat_halfeye( .width = .90, height = 0.8 ) + 
  scale_fill_manual( values = alpha( c( "#193E8F", "#E53528" , "#193E8F" , "#E53528" ) , 0.3 ) ) +
  scale_color_manual( values = c( "#193E8F", "#E53528" , "#193E8F" , "#E53528" ) ) +
  scale_y_discrete( breaks = c( "Older brothers with kidnap marriage : male" , 
                                "Older brothers with kidnap marriage : female" , 
                                "Older sisters with kidnap marriage : male" , 
                                "Older sisters with kidnap marriage : female" ) ,
                    labels = c( "Older brothers with kidnap marriage : male" , 
                                "Older brothers with kidnap marriage : female" , 
                                "Older sisters with kidnap marriage : male" , 
                                "Older sisters with kidnap marriage : female" ) ) +
  labs( x = "Posterior distribution of estimates" ,
        y = "Older siblings with kidnap marriage : gender" ) +
  coord_cartesian( ylim = c( 1.5, 4.5 ) ) +
  scale_x_continuous( limits = c( -0.8 , 0.8 ) , breaks = c( -0.5 , 0 , 0.5 ) ) +
  theme(plot.margin = margin(10, 10, 10, 20),
        strip.background = element_rect(color = "black", fill = "white") ,
        strip.text.x = element_text(size = 16),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "Gainsboro"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour="black",fill=NA),
        panel.background = element_blank(),
        plot.title = element_text(size=16),
        legend.position = "none",
        axis.title.x = element_text(size = 18,
                                    margin = margin(t = 0.3, r = 0, b = 0, l = 0,unit = "cm")),
        axis.text.x = element_text(colour="black",size=16,
                                   margin = margin(t = 0.1, r = 0, b = 0, l = 0,unit = "cm")),
        axis.title.y = element_text(size = 18,
                                    margin = margin(t = 0, r = 0.5, b = 0, l = 0,unit = "cm")),
        axis.text.y = element_text(colour="black",size=16,
                                   margin = margin(t = 0, r = 0.2, b = 0, l = 0,unit = "cm")),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12))

#####3.2.4.3.2 Predicted probability ----
# Function to generate new data allowing to specify different values from models of marriage type of older sisters
generate_and_predict_p_matrix_OSisMG <- function( post_samples , Gender_value = 1, n_samples = 6000 ) {
  
  # Generate new data
  new_data <- data.frame(
    Gender = as.integer( rep( Gender_value , 7056 ) ) ,
    P_kidnapping = as.integer( rep( 1:2 , 3528 ) ) ,
    Cohort = as.integer( rep( 1:8 , 882 ) ) ,
    Sis_o = as.integer( rep( 0:8 , 784 ) ) ,
    Sis = as.numeric( sprintf( "%0.4f" , ( as.integer( rep( 0:8 , 784 ) ) - 3.272835 )/ 1.845436 ) )  ,
    Bro_o = as.integer( rep( 0:8 , 784 ) ) ,
    Bro = as.numeric( sprintf( "%0.4f" , ( as.integer( rep( 0:8 , 784 ) ) - 2.67497 )/ 1.61629 ) ) ,
    Older_sis_kidnap_o = as.integer( rep( 0:6 , each = 1008 ) ) ,
    Older_sis_kidnap = as.numeric( sprintf( "%0.4f" , ( as.integer( rep( 0:6 , each = 1008 ) ) - 0.8807829 )/ 1.146026 ) )  ,
    Older_bro_kidnap_o = as.integer( rep( 0:6 , 1008 ) ) ,
    Older_bro_kidnap = as.numeric( sprintf( "%0.4f" , ( as.integer( rep( 0:6 , 1008 ) ) - 0.6079478 )/ 0.9804781 ) )
  )
  
  # Initialize p_matrix
  n_OSisM <- length( unique( new_data$Older_sis_kidnap_o ) )
  p_matrix <- matrix( NA , nrow = n_samples , ncol = n_OSisM )
  
  # Loop over posterior samples
  for ( s in 1:n_samples ) {
    
    # Extract posterior sample for current iteration
    bOSisMG <- post_samples$bOSisMG[s, ]
    bOBroMG <- post_samples$bOBroMG[s, ]
    bSisG <- post_samples$bSisG[s, ]
    bBroG <- post_samples$bBroG[s, ]
    bPG <- post_samples$bPG[s, , ]
    bC <- post_samples$bC[s, ]
    V_bar <- post_samples$V_bar[s, 1]
    
    # Loop over each Older_sis_kidnap
    for ( m in 1:n_OSisM ) {
      OSis_kidnap_subset <- new_data[ new_data$Older_sis_kidnap_o == ( m - 1 ) , ]  # Subset data for Older_sis_kidnap
      logit_p_vals <- with( OSis_kidnap_subset,
                            bOSisMG[Gender] * Older_sis_kidnap + 
                              bOBroMG[Gender] * Older_bro_kidnap + 
                              bSisG[Gender] * Sis + 
                              bBroG[Gender] * Bro + 
                              bPG[cbind(P_kidnapping, Gender)] +
                              bC[Cohort] + 
                              V_bar )
      
      # Convert to probabilities and average over other variables
      p_matrix[s, m] <- mean(rethinking::inv_logit( logit_p_vals ) , na.rm = TRUE )
    }
  }
  
  return(p_matrix)
}

Predicted_p_SibMG_sis_F <- generate_and_predict_p_matrix_OSisMG( post_samples = Post_model_SibMG , 
                                                                 Gender_value = 1 )
Predicted_p_SibMG_sis_M <- generate_and_predict_p_matrix_OSisMG( post_samples = Post_model_SibMG , 
                                                                 Gender_value = 2 )

generate_and_predict_p_matrix_OBroMG <- function( post_samples , Gender_value = 1, n_samples = 6000 ) {
  
  # Generate new data
  new_data <- data.frame(
    Gender = as.integer( rep( Gender_value , 7056 ) ) ,
    P_kidnapping = as.integer( rep( 1:2 , 3528 ) ) ,
    Cohort = as.integer( rep( 1:8 , 882 ) ) ,
    Sis_o = as.integer( rep( 0:8 , 784 ) ) ,
    Sis = as.numeric( sprintf( "%0.4f" , ( as.integer( rep( 0:8 , 784 ) ) - 3.272835 )/ 1.845436 ) )  ,
    Bro_o = as.integer( rep( 0:8 , 784 ) ) ,
    Bro = as.numeric( sprintf( "%0.4f" , ( as.integer( rep( 0:8 , 784 ) ) - 2.67497 )/ 1.61629 ) ) ,
    Older_sis_kidnap_o = as.integer( rep( 0:6 , 1008 ) ) ,
    Older_sis_kidnap = as.numeric( sprintf( "%0.4f" , ( as.integer( rep( 0:6 , 1008 ) ) - 0.8807829 )/ 1.146026 ) )  ,
    Older_bro_kidnap_o = as.integer( rep( 0:6 , each = 1008 ) ) ,
    Older_bro_kidnap = as.numeric( sprintf( "%0.4f" , ( as.integer( rep( 0:6 , each = 1008 ) ) - 0.6079478 )/ 0.9804781 ) )
  )
  
  # Initialize p_matrix
  n_OBroM <- length( unique( new_data$Older_bro_kidnap_o ) )
  p_matrix <- matrix( NA , nrow = n_samples , ncol = n_OBroM )
  
  # Loop over posterior samples
  for ( s in 1:n_samples ) {
    
    # Extract posterior sample for current iteration
    bOSisMG <- post_samples$bOSisMG[s, ]
    bOBroMG <- post_samples$bOBroMG[s, ]
    bSisG <- post_samples$bSisG[s, ]
    bBroG <- post_samples$bBroG[s, ]
    bPG <- post_samples$bPG[s, , ]
    bC <- post_samples$bC[s, ]
    V_bar <- post_samples$V_bar[s, 1]
    
    # Loop over each Older_bro_kidnap
    for ( m in 1:n_OBroM ) {
      OBro_kidnap_subset <- new_data[ new_data$Older_bro_kidnap_o == ( m - 1 ) , ]  # Subset data for Older_bro_kidnap
      logit_p_vals <- with( OBro_kidnap_subset,
                            bOSisMG[Gender] * Older_sis_kidnap + 
                              bOBroMG[Gender] * Older_bro_kidnap + 
                              bSisG[Gender] * Sis + 
                              bBroG[Gender] * Bro + 
                              bPG[cbind(P_kidnapping, Gender)] +
                              bC[Cohort] + 
                              V_bar )
      
      # Convert to probabilities and average over other variables
      p_matrix[s, m] <- mean(rethinking::inv_logit( logit_p_vals ) , na.rm = TRUE )
    }
  }
  
  return(p_matrix)
}

Predicted_p_SibMG_bro_F <- generate_and_predict_p_matrix_OBroMG( post_samples = Post_model_SibMG , 
                                                                 Gender_value = 1 )
Predicted_p_SibMG_bro_M <- generate_and_predict_p_matrix_OBroMG( post_samples = Post_model_SibMG , 
                                                                 Gender_value = 2 )

####3.2.4.4 Figure 3b-e: Predicted figure (sibling + sibling marriage type)---- 
{ par( mfrow = c( 2 , 2 ) , oma = c( 3.5 , 2 , 0 , 0 ) , mar = c( 0.4 , 0.4 , 0.4 , 0.4 ) )
  # Number of sisters
  plot( NULL,
        xlim = c(0, 8),
        ylim = c(0,1),
        xaxt = "n" , 
        yaxt = "n" , 
        xlab = "" , 
        ylab = "" ,
        cex.axis = 1.2 ,
        cex.lab = 1.5 ,
        cex.main = 1.5 , 
        mgp = c( 2.5 , 0.8 , 0 ) )
  mtext( "Sister" , side = 2 , line = 0.4 , cex = 1.5 )
  
  # female
  lines( c(0:8) , 
         apply( Predicted_p_SibG_sis_F , 2 , mean ) ,
         lwd = 2 , col = "#E53528" )
  shade( apply( Predicted_p_SibG_sis_F , 2 , PI , prob = 0.90 ) , 
         c(0:8) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  shade( apply( Predicted_p_SibG_sis_F , 2 , PI , prob = 0.60 ) , 
         c(0:8) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  shade( apply( Predicted_p_SibG_sis_F , 2 , PI , prob = 0.30 ) , 
         c(0:8) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  # male
  lines( c(0:8) , 
         apply( Predicted_p_SibG_sis_M , 2 , mean ) ,
         lwd = 2 , col = "#193E8F" )
  shade( apply( Predicted_p_SibG_sis_M , 2 , PI , prob = 0.90 ) , 
         c(0:8) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  shade( apply( Predicted_p_SibG_sis_M , 2 , PI , prob = 0.60 ) , 
         c(0:8) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  shade( apply( Predicted_p_SibG_sis_M , 2 , PI , prob = 0.30 ) , 
         c(0:8) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  
  # Number of kidnapping older sisters
  plot( NULL,
        xlim = c(0, 6),
        ylim = c(0,1),
        xaxt = "n" , 
        yaxt = "n" , 
        xlab = "" , 
        ylab = "" ,
        cex.axis = 1.2 ,
        cex.lab = 1.5 ,
        cex.main = 1.5 , 
        mgp = c( 2.5 , 0.8 , 0 ) )
  
  # female
  lines( c(0:6) , 
         apply( Predicted_p_SibMG_sis_F , 2 , mean ) ,
         lwd = 2 , col = "#E53528" )
  shade( apply( Predicted_p_SibMG_sis_F , 2 , PI , prob = 0.90 ) , 
         c(0:6) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  shade( apply( Predicted_p_SibMG_sis_F , 2 , PI , prob = 0.60 ) , 
         c(0:6) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  shade( apply( Predicted_p_SibMG_sis_F , 2 , PI , prob = 0.30 ) , 
         c(0:6) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  # male
  lines( c(0:6) , 
         apply( Predicted_p_SibMG_sis_M , 2 , mean ) ,
         lwd = 2 , col = "#193E8F" )
  shade( apply( Predicted_p_SibMG_sis_M , 2 , PI , prob = 0.90 ) , 
         c(0:6) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  shade( apply( Predicted_p_SibMG_sis_M , 2 , PI , prob = 0.60 ) , 
         c(0:6) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  shade( apply( Predicted_p_SibMG_sis_M , 2 , PI , prob = 0.30 ) , 
         c(0:6) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  legend( x = 4 , y = 1.05 , 
          box.col = "white",
          legend = c( "Female" , "Male" ) , 
          lty = c( 1 , 1 ) ,  
          col = c( "#E53528" , "#193E8F" ) , 
          lwd = 2 ,
          cex = 1.5 , 
          bty = "n" ,
          y.intersp = 1.0 ,
          x.intersp = 0.3 ,
          seg.len = 1.2 )
  
  # Number of brothers
  plot( NULL,
        xlim = c(0, 8),
        ylim = c(0,1),
        yaxt = "n" , 
        xlab = "" , 
        ylab = "" ,
        cex.axis = 1.2 ,
        cex.lab = 1.5 ,
        cex.main = 1.5 , 
        mgp = c( 2.5 , 0.8 , 0 ) )
  mtext( "Brother" , side = 2 , line = 0.4 , cex = 1.5 )
  mtext( "Number of siblings" , side = 1 , line = 2.2 , cex = 1.4 )
  
  # female
  lines( c(0:8) , 
         apply( Predicted_p_SibG_bro_F , 2 , mean ) ,
         lwd = 2 , col = "#E53528" )
  shade( apply( Predicted_p_SibG_bro_F , 2 , PI , prob = 0.90 ) , 
         c(0:8) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  shade( apply( Predicted_p_SibG_bro_F , 2 , PI , prob = 0.60 ) , 
         c(0:8) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  shade( apply( Predicted_p_SibG_bro_F , 2 , PI , prob = 0.30 ) , 
         c(0:8) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  # male
  lines( c(0:8) , 
         apply( Predicted_p_SibG_bro_M , 2 , mean ) ,
         lwd = 2 , col = "#193E8F" )
  shade( apply( Predicted_p_SibG_bro_M , 2 , PI , prob = 0.90 ) , 
         c(0:8) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  shade( apply( Predicted_p_SibG_bro_M , 2 , PI , prob = 0.60 ) , 
         c(0:8) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  shade( apply( Predicted_p_SibG_bro_M , 2 , PI , prob = 0.30 ) , 
         c(0:8) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  
  # Number of kidnapping older brothers
  plot( NULL,
        xlim = c(0, 6),
        ylim = c(0,1),
        yaxt = "n" , 
        xlab = "" , 
        ylab = "" ,
        cex.axis = 1.2 ,
        cex.lab = 1.5 ,
        cex.main = 1.5 , 
        mgp = c( 2.5 , 0.8 , 0 ) )
  mtext( "Number of siblings with kidnap marriage" , side = 1 , line = 2.2 , cex = 1.4 )
  
  # female
  lines( c(0:6) , 
         apply( Predicted_p_SibMG_bro_F , 2 , mean ) ,
         lwd = 2 , col = "#E53528" )
  shade( apply( Predicted_p_SibMG_bro_F , 2 , PI , prob = 0.90 ) , 
         c(0:6) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  shade( apply( Predicted_p_SibMG_bro_F , 2 , PI , prob = 0.60 ) , 
         c(0:6) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  shade( apply( Predicted_p_SibMG_bro_F , 2 , PI , prob = 0.30 ) , 
         c(0:6) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  # male
  lines( c(0:6) , 
         apply( Predicted_p_SibMG_bro_M , 2 , mean ) ,
         lwd = 2 , col = "#193E8F" )
  shade( apply( Predicted_p_SibMG_bro_M , 2 , PI , prob = 0.90 ) , 
         c(0:6) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  shade( apply( Predicted_p_SibMG_bro_M , 2 , PI , prob = 0.60 ) , 
         c(0:6) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  shade( apply( Predicted_p_SibMG_bro_M , 2 , PI , prob = 0.30 ) , 
         c(0:6) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
}

dev.off()

####3.2.4.4 Older siblings' marriage type stratified by village and gender ----
#####3.2.4.4.1 Posterior distribution of estimates ----
tibble( Value = c( Post_model_SibMGV$bOSisMGV[,1,1] , 
                   Post_model_SibMGV$bOSisMGV[,2,1] , 
                   Post_model_SibMGV$bOSisMGV[,1,2] , 
                   Post_model_SibMGV$bOSisMGV[,2,2] , 
                   Post_model_SibMGV$bOBroMGV[,1,1] , 
                   Post_model_SibMGV$bOBroMGV[,2,1] , 
                   Post_model_SibMGV$bOBroMGV[,1,2] , 
                   Post_model_SibMGV$bOBroMGV[,2,2] ) ,
        SibM_gender = c( rep( "Older sisters with kidnap marriage : female" , 6000 ) ,
                         rep( "Older sisters with kidnap marriage : male" , 6000 ) , 
                         rep( "Older sisters with kidnap marriage : female" , 6000 ) ,
                         rep( "Older sisters with kidnap marriage : male" , 6000 ) , 
                         rep( "Older brothers with kidnap marriage : female" , 6000 ) ,
                         rep( "Older brothers with kidnap marriage : male" , 6000 ) , 
                         rep( "Older brothers with kidnap marriage : female" , 6000 ) ,
                         rep( "Older brothers with kidnap marriage : male" , 6000 ) ) , 
        Village = c( rep( "Village 1" , 12000 ) ,
                     rep( "Village 2" , 12000 ) , 
                     rep( "Village 1" , 12000 ) ,
                     rep( "Village 2" , 12000 ) ) ) %>% 
  mutate( SibM_gender = factor( SibM_gender , 
                                levels = c( "Older brothers with kidnap marriage : male" , 
                                            "Older brothers with kidnap marriage : female" , 
                                            "Older sisters with kidnap marriage : male" , 
                                            "Older sisters with kidnap marriage : female" ) , 
                                labels = c( "Older brothers with kidnap marriage : male" , 
                                            "Older brothers with kidnap marriage : female" , 
                                            "Older sisters with kidnap marriage : male" , 
                                            "Older sisters with kidnap marriage : female" ) ) , 
          Village = factor( Village , 
                            levels = c( "Village 1" , "Village 2" ) , 
                            labels = c( "Village 1" , "Village 2" ) ) ) %>% 
  ggplot( aes( x = Value , y = SibM_gender , 
               color = SibM_gender , fill = SibM_gender ) ) +
  facet_wrap( ~ Village , nrow = 1 ) + 
  geom_vline( xintercept = 0, linetype = 2 , linewidth = 1 ,
              color = "dimgray" ) +
  stat_halfeye( .width = .90, height = 0.8 ) + 
  scale_fill_manual( values = alpha( c( "#193E8F", "#E53528" , "#193E8F" , "#E53528" ) , 0.3 ) ) +
  scale_color_manual( values = c( "#193E8F", "#E53528" , "#193E8F" , "#E53528" ) ) +
  scale_y_discrete( breaks = c( "Older brothers with kidnap marriage : male" , 
                                "Older brothers with kidnap marriage : female" , 
                                "Older sisters with kidnap marriage : male" , 
                                "Older sisters with kidnap marriage : female" ) ,
                    labels = c( "Older brothers with kidnap marriage : male" , 
                                "Older brothers with kidnap marriage : female" , 
                                "Older sisters with kidnap marriage : male" , 
                                "Older sisters with kidnap marriage : female" ) ) +
  labs( x = "Posterior distribution of estimates" ,
        y = "Older sibling with kidnap marriage : gender" ) +
  coord_cartesian( ylim = c( 1.5, 4.5 ) ) +
  scale_x_continuous( limits = c( -0.8 , 0.8 ) , breaks = c( -0.5 , 0 , 0.5 ) ) +
  theme(plot.margin = margin(10, 10, 10, 20),
        strip.background = element_rect(color = "black", fill = "white") ,
        strip.text.x = element_text(size = 16),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = "Gainsboro"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour="black",fill=NA),
        panel.background = element_blank(),
        plot.title = element_text(size=16),
        legend.position = "none",
        axis.title.x = element_text(size = 18,
                                    margin = margin(t = 0.3, r = 0, b = 0, l = 0,unit = "cm")),
        axis.text.x = element_text(colour="black",size=16,
                                   margin = margin(t = 0.1, r = 0, b = 0, l = 0,unit = "cm")),
        axis.title.y = element_text(size = 18,
                                    margin = margin(t = 0, r = 0.5, b = 0, l = 0,unit = "cm")),
        axis.text.y = element_text(colour="black",size=16,
                                   margin = margin(t = 0, r = 0.2, b = 0, l = 0,unit = "cm")),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12))

#####3.2.4.4.2 Predicted probability ----
# Function to generate new data allowing to specify different values from models of marriage type of older sisters
generate_and_predict_p_matrix_OSisMGV <- function( post_samples , Gender_value = 1, V_value = 1 , n_samples = 6000 ) {
  
  # Generate new data
  new_data <- data.frame(
    Gender = as.integer( rep( Gender_value , 7056 ) ) ,
    Village = as.integer( rep( V_value , 7056 ) ) ,
    P_kidnapping = as.integer( rep( 1:2 , 3528 ) ) ,
    Cohort = as.integer( rep( 1:8 , 882 ) ) ,
    Sis_o = as.integer( rep( 0:8 , 784 ) ) ,
    Sis = as.numeric( sprintf( "%0.4f" , ( as.integer( rep( 0:8 , 784 ) ) - 3.272835 )/ 1.845436 ) )  ,
    Bro_o = as.integer( rep( 0:8 , 784 ) ) ,
    Bro = as.numeric( sprintf( "%0.4f" , ( as.integer( rep( 0:8 , 784 ) ) - 2.67497 )/ 1.61629 ) ) ,
    Older_sis_kidnap_o = as.integer( rep( 0:6 , each = 1008 ) ) ,
    Older_sis_kidnap = as.numeric( sprintf( "%0.4f" , ( as.integer( rep( 0:6 , each = 1008 ) ) - 0.8807829 )/ 1.146026 ) )  ,
    Older_bro_kidnap_o = as.integer( rep( 0:6 , 1008 ) ) ,
    Older_bro_kidnap = as.numeric( sprintf( "%0.4f" , ( as.integer( rep( 0:6 , 1008 ) ) - 0.6079478 )/ 0.9804781 ) )
  )
  
  # Initialize p_matrix
  n_OSisMV <- length( unique( new_data$Older_sis_kidnap_o ) )
  p_matrix <- matrix( NA , nrow = n_samples , ncol = n_OSisMV )
  
  # Loop over posterior samples
  for ( s in 1:n_samples ) {
    
    # Extract posterior sample for current iteration
    bOSisMGV <- post_samples$bOSisMGV[s, , ]
    bOBroMGV <- post_samples$bOBroMGV[s, , ]
    bSisG <- post_samples$bSisG[s, ]
    bBroG <- post_samples$bBroG[s, ]
    bPG <- post_samples$bPG[s, , ]
    bC <- post_samples$bC[s, ]
    V_bar <- post_samples$V_bar[s, 1]
    
    # Loop over each Older_sis_kidnap
    for ( m in 1:n_OSisMV ) {
      OSis_kidnap_subset <- new_data[ new_data$Older_sis_kidnap_o == ( m - 1 ) , ]  # Subset data for Older_sis_kidnap
      logit_p_vals <- with( OSis_kidnap_subset,
                            bOSisMGV[cbind(Gender,Village)] * Older_sis_kidnap + 
                              bOBroMGV[cbind(Gender,Village)] * Older_bro_kidnap + 
                              bSisG[Gender] * Sis + 
                              bBroG[Gender] * Bro + 
                              bPG[cbind(P_kidnapping, Gender)] +
                              bC[Cohort] + 
                              V_bar )
      
      # Convert to probabilities and average over other variables
      p_matrix[s, m] <- mean(rethinking::inv_logit( logit_p_vals ) , na.rm = TRUE )
    }
  }
  
  return(p_matrix)
}

Predicted_p_SibMGV_sis_FV1 <- generate_and_predict_p_matrix_OSisMGV( post_samples = Post_model_SibMGV , 
                                                                     Gender_value = 1 , V_value = 1 )
Predicted_p_SibMGV_sis_MV1 <- generate_and_predict_p_matrix_OSisMGV( post_samples = Post_model_SibMGV , 
                                                                     Gender_value = 2 , V_value = 1 )
Predicted_p_SibMGV_sis_FV2 <- generate_and_predict_p_matrix_OSisMGV( post_samples = Post_model_SibMGV , 
                                                                     Gender_value = 1 , V_value = 2 )
Predicted_p_SibMGV_sis_MV2 <- generate_and_predict_p_matrix_OSisMGV( post_samples = Post_model_SibMGV , 
                                                                     Gender_value = 2 , V_value = 2 )

generate_and_predict_p_matrix_OBroMGV <- function( post_samples , Gender_value = 1 , V_value = 1 , n_samples = 6000 ) {
  
  # Generate new data
  new_data <- data.frame(
    Gender = as.integer( rep( Gender_value , 7056 ) ) ,
    Village = as.integer( rep( V_value , 7056 ) ) ,
    P_kidnapping = as.integer( rep( 1:2 , 3528 ) ) ,
    Cohort = as.integer( rep( 1:8 , 882 ) ) ,
    Sis_o = as.integer( rep( 0:8 , 784 ) ) ,
    Sis = as.numeric( sprintf( "%0.4f" , ( as.integer( rep( 0:8 , 784 ) ) - 3.272835 )/ 1.845436 ) )  ,
    Bro_o = as.integer( rep( 0:8 , 784 ) ) ,
    Bro = as.numeric( sprintf( "%0.4f" , ( as.integer( rep( 0:8 , 784 ) ) - 2.67497 )/ 1.61629 ) ) ,
    Older_sis_kidnap_o = as.integer( rep( 0:6 , 1008 ) ) ,
    Older_sis_kidnap = as.numeric( sprintf( "%0.4f" , ( as.integer( rep( 0:6 , 1008 ) ) - 0.8807829 )/ 1.146026 ) )  ,
    Older_bro_kidnap_o = as.integer( rep( 0:6 , each = 1008 ) ) ,
    Older_bro_kidnap = as.numeric( sprintf( "%0.4f" , ( as.integer( rep( 0:6 , each = 1008 ) ) - 0.6079478 )/ 0.9804781 ) )
  )
  
  # Initialize p_matrix
  n_OBroMV <- length( unique( new_data$Older_bro_kidnap_o ) )
  p_matrix <- matrix( NA , nrow = n_samples , ncol = n_OBroMV )
  
  # Loop over posterior samples
  for ( s in 1:n_samples ) {
    
    # Extract posterior sample for current iteration
    bOSisMGV <- post_samples$bOSisMGV[s, , ]
    bOBroMGV <- post_samples$bOBroMGV[s, , ]
    bSisG <- post_samples$bSisG[s, ]
    bBroG <- post_samples$bBroG[s, ]
    bPG <- post_samples$bPG[s, , ]
    bC <- post_samples$bC[s, ]
    V_bar <- post_samples$V_bar[s, 1]
    
    # Loop over each Older_bro_kidnap
    for ( m in 1:n_OBroMV ) {
      OBro_kidnap_subset <- new_data[ new_data$Older_bro_kidnap_o == ( m - 1 ) , ]  # Subset data for Older_bro_kidnap
      logit_p_vals <- with( OBro_kidnap_subset,
                            bOSisMGV[cbind(Gender,Village)] * Older_sis_kidnap + 
                              bOBroMGV[cbind(Gender,Village)] * Older_bro_kidnap + 
                              bSisG[Gender] * Sis + 
                              bBroG[Gender] * Bro + 
                              bPG[cbind(P_kidnapping, Gender)] +
                              bC[Cohort] + 
                              V_bar )
      
      # Convert to probabilities and average over other variables
      p_matrix[s, m] <- mean(rethinking::inv_logit( logit_p_vals ) , na.rm = TRUE )
    }
  }
  
  return(p_matrix)
}

Predicted_p_SibMGV_bro_FV1 <- generate_and_predict_p_matrix_OBroMGV( post_samples = Post_model_SibMGV , 
                                                                     Gender_value = 1 , V_value = 1 )
Predicted_p_SibMGV_bro_MV1 <- generate_and_predict_p_matrix_OBroMGV( post_samples = Post_model_SibMGV , 
                                                                     Gender_value = 2 , V_value = 1 )
Predicted_p_SibMGV_bro_FV2 <- generate_and_predict_p_matrix_OBroMGV( post_samples = Post_model_SibMGV , 
                                                                     Gender_value = 1 , V_value = 2 )
Predicted_p_SibMGV_bro_MV2 <- generate_and_predict_p_matrix_OBroMGV( post_samples = Post_model_SibMGV , 
                                                                     Gender_value = 2 , V_value = 2 )

{ par( mfrow = c( 2 , 2 ) , oma = c( 4 , 4 , 0 , 0 ) , mar = c( 0.4 , 0.4 , 0.4 , 0.4 ) )
  # Village 1
  # Number of kidnapping older sisters
  plot( NULL,
        xlim = c(0, 6),
        ylim = c(0,1),
        xaxt = "n" , 
        xlab = "" , 
        ylab = "" ,
        cex.axis = 1.2 ,
        cex.lab = 1.5 ,
        cex.main = 1.5 , 
        mgp = c( 2.5 , 0.8 , 0 ) )
  mtext( "Village 1" , side = 2 , line = 2.4 , cex = 1.5 )
  
  # female
  lines( c(0:6) , 
         apply( Predicted_p_SibMGV_sis_FV1 , 2 , mean ) ,
         lwd = 2 , col = "#E53528" )
  shade( apply( Predicted_p_SibMGV_sis_FV1 , 2 , PI , prob = 0.90 ) , 
         c(0:6) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  shade( apply( Predicted_p_SibMGV_sis_FV1 , 2 , PI , prob = 0.60 ) , 
         c(0:6) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  shade( apply( Predicted_p_SibMGV_sis_FV1 , 2 , PI , prob = 0.30 ) , 
         c(0:6) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  # male
  lines( c(0:6) , 
         apply( Predicted_p_SibMGV_sis_MV1 , 2 , mean ) ,
         lwd = 2 , col = "#193E8F" )
  shade( apply( Predicted_p_SibMGV_sis_MV1 , 2 , PI , prob = 0.90 ) , 
         c(0:6) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  shade( apply( Predicted_p_SibMGV_sis_MV1 , 2 , PI , prob = 0.60 ) , 
         c(0:6) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  shade( apply( Predicted_p_SibMGV_sis_MV1 , 2 , PI , prob = 0.30 ) , 
         c(0:6) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  
  # Number of kidnapping older brothers
  plot( NULL,
        xlim = c(0, 6),
        ylim = c(0,1),
        xaxt = "n" , 
        yaxt = "n" , 
        xlab = "" , 
        ylab = "" ,
        cex.axis = 1.2 ,
        cex.lab = 1.5 ,
        cex.main = 1.5 , 
        mgp = c( 2.5 , 0.8 , 0 ) )
  
  # female
  lines( c(0:6) , 
         apply( Predicted_p_SibMGV_bro_FV1 , 2 , mean ) ,
         lwd = 2 , col = "#E53528" )
  shade( apply( Predicted_p_SibMGV_bro_FV1 , 2 , PI , prob = 0.90 ) , 
         c(0:6) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  shade( apply( Predicted_p_SibMGV_bro_FV1 , 2 , PI , prob = 0.60 ) , 
         c(0:6) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  shade( apply( Predicted_p_SibMGV_bro_FV1 , 2 , PI , prob = 0.30 ) , 
         c(0:6) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  # male
  lines( c(0:6) , 
         apply( Predicted_p_SibMGV_bro_MV1 , 2 , mean ) ,
         lwd = 2 , col = "#193E8F" )
  shade( apply( Predicted_p_SibMGV_bro_MV1 , 2 , PI , prob = 0.90 ) , 
         c(0:6) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  shade( apply( Predicted_p_SibMGV_bro_MV1 , 2 , PI , prob = 0.60 ) , 
         c(0:6) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  shade( apply( Predicted_p_SibMGV_bro_MV1 , 2 , PI , prob = 0.30 ) , 
         c(0:6) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  legend( x = 4.6 , y = 1.05 , 
          box.col = "white",
          legend = c( "Female" , "Male" ) , 
          lty = c( 1 , 1 ) ,  
          col = c( "#E53528" , "#193E8F" ) , 
          lwd = 2 ,
          cex = 1.3 , 
          bty = "n" ,
          y.intersp = 1.1 ,
          x.intersp = 0.3 ,
          seg.len = 1.2 )
  
  # Village 2
  # Number of kidnapping older sisters
  plot( NULL,
        xlim = c(0, 6),
        ylim = c(0,1),
        xlab = "" , 
        ylab = "" ,
        cex.axis = 1.2 ,
        cex.lab = 1.5 ,
        cex.main = 1.5 , 
        mgp = c( 2.5 , 0.8 , 0 ) )
  mtext( "Number of older sisters with kidnap marriage" , side = 1 , line = 2.4 , cex = 1.3 )
  mtext( "Village 2" , side = 2 , line = 2.4 , cex = 1.5 )
  
  # female
  lines( c(0:6) , 
         apply( Predicted_p_SibMGV_sis_FV2 , 2 , mean ) ,
         lwd = 2 , col = "#E53528" )
  shade( apply( Predicted_p_SibMGV_sis_FV2 , 2 , PI , prob = 0.90 ) , 
         c(0:6) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  shade( apply( Predicted_p_SibMGV_sis_FV2 , 2 , PI , prob = 0.60 ) , 
         c(0:6) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  shade( apply( Predicted_p_SibMGV_sis_FV2 , 2 , PI , prob = 0.30 ) , 
         c(0:6) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  # male
  lines( c(0:6) , 
         apply( Predicted_p_SibMGV_sis_MV2 , 2 , mean ) ,
         lwd = 2 , col = "#193E8F" )
  shade( apply( Predicted_p_SibMGV_sis_MV2 , 2 , PI , prob = 0.90 ) , 
         c(0:6) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  shade( apply( Predicted_p_SibMGV_sis_MV2 , 2 , PI , prob = 0.60 ) , 
         c(0:6) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  shade( apply( Predicted_p_SibMGV_sis_MV2 , 2 , PI , prob = 0.30 ) , 
         c(0:6) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  
  # Number of kidnapping older brothers
  plot( NULL,
        xlim = c(0, 6),
        ylim = c(0,1),
        yaxt = "n" , 
        xlab = "" , 
        ylab = "" ,
        cex.axis = 1.2 ,
        cex.lab = 1.5 ,
        cex.main = 1.5 , 
        mgp = c( 2.5 , 0.8 , 0 ) )
  mtext( "Number of older brothers with kidnap marriage" , side = 1 , line = 2.4 , cex = 1.3 )
  
  # female
  lines( c(0:6) , 
         apply( Predicted_p_SibMGV_bro_FV2 , 2 , mean ) ,
         lwd = 2 , col = "#E53528" )
  shade( apply( Predicted_p_SibMGV_bro_FV2 , 2 , PI , prob = 0.90 ) , 
         c(0:6) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  shade( apply( Predicted_p_SibMGV_bro_FV2 , 2 , PI , prob = 0.60 ) , 
         c(0:6) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  shade( apply( Predicted_p_SibMGV_bro_FV2 , 2 , PI , prob = 0.30 ) , 
         c(0:6) , 
         col = col.alpha( "#E53528" , 0.2 ) )
  # male
  lines( c(0:6) , 
         apply( Predicted_p_SibMGV_bro_MV2 , 2 , mean ) ,
         lwd = 2 , col = "#193E8F" )
  shade( apply( Predicted_p_SibMGV_bro_MV2 , 2 , PI , prob = 0.90 ) , 
         c(0:6) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  shade( apply( Predicted_p_SibMGV_bro_MV2 , 2 , PI , prob = 0.60 ) , 
         c(0:6) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
  shade( apply( Predicted_p_SibMGV_bro_MV2 , 2 , PI , prob = 0.30 ) , 
         c(0:6) , 
         col = col.alpha( "#193E8F" , 0.2 ) )
}

dev.off()

###3.2.5 Outputs ----
####3.2.5.1 Number of siblings stratified by gender ----
#####3.2.5.1.1 Estimates ----
Pre_SibG_output <- data.frame( Mean = c( Pre_model_SibG$mean ) ,
                               CI5 = c( Pre_model_SibG$`5%` ) ,
                               CI95 = c( Pre_model_SibG$`95%` ) ,
                               Variable = c( "Sisters: female" , 
                                             "Sisters: male" ,
                                             "Brothers: female" , 
                                             "Brothers: male" ,
                                             
                                             "Non-kidnapping parents: female" , 
                                             "Kidnapping parents: female" ,
                                             "Non-kidnapping parents: male" , 
                                             "Kidnapping parents: male" ,
                                             
                                             "<=1960" , "1961-1965" , 
                                             "1966-1970" , "1971-1975" , "1976-1980" , 
                                             "1981-1985" , "1986-1990" , ">1990" ,
                                             
                                             "Village 1" , "Village 2" ) ) %>% 
  mutate( Variable = factor( Variable , 
                             levels = c( "Sisters: female" , 
                                         "Sisters: male" ,
                                         "Brothers: female" , 
                                         "Brothers: male" ,
                                         
                                         "Non-kidnapping parents: female" , 
                                         "Kidnapping parents: female" ,
                                         "Non-kidnapping parents: male" , 
                                         "Kidnapping parents: male" ,
                                         
                                         "<=1960" , "1961-1965" , 
                                         "1966-1970" , "1971-1975" , "1976-1980" , 
                                         "1981-1985" , "1986-1990" , ">1990" ,
                                         
                                         "Village 1" , "Village 2" ) ,
                             labels = c( "Sisters: female" , 
                                         "Sisters: male" ,
                                         "Brothers: female" , 
                                         "Brothers: male" ,
                                         
                                         "Non-kidnapping parents: female" , 
                                         "Kidnapping parents: female" ,
                                         "Non-kidnapping parents: male" , 
                                         "Kidnapping parents: male" ,
                                         
                                         "<=1960" , "1961-1965" , 
                                         "1966-1970" , "1971-1975" , "1976-1980" , 
                                         "1981-1985" , "1986-1990" , ">1990" ,
                                         
                                         "Village 1" , "Village 2" ) ) ,
          Model = "Number of siblings stratified by gender" , 
          "Mean [CI]" = paste( format( round( Mean , 2 ), nsmall = 2 ) , 
                               " [" , 
                               format( round( CI5 , 2 ), nsmall = 2 ) , 
                               ", " , 
                               format( round( CI95 , 2 ), nsmall = 2 ) , 
                               "]", 
                               sep = "") )

SibG_output <- Pre_SibG_output %>% 
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
SibG_output

#####3.2.5.1.2 90% proportions ----
tibble( BType = c( "bSis" , "bSis" , "bBro" , "bBro" ) ,
        Gender = c( "Female" , "Male" , "Female" , "Male" ) ,
        bSibG_diff = c( sprintf("%0.2f" , mean( Post_model_SibG$bSisG[,1] ) ) , 
                        sprintf("%0.2f" , mean( Post_model_SibG$bSisG[,2] ) ) , 
                        sprintf("%0.2f" , mean( Post_model_SibG$bBroG[,1] ) ) , 
                        sprintf("%0.2f" , mean( Post_model_SibG$bBroG[,2] ) ) ) ,
        
        bSibG_diff_L = c( sprintf("%0.2f" , PI( Post_model_SibG$bSisG[,1] , prob = 0.90 )[1] ) , 
                          sprintf("%0.2f" , PI( Post_model_SibG$bSisG[,2] , prob = 0.90 )[1] ) , 
                          sprintf("%0.2f" , PI( Post_model_SibG$bBroG[,1] , prob = 0.90 )[1] ) , 
                          sprintf("%0.2f" , PI( Post_model_SibG$bBroG[,2] , prob = 0.90 )[1] ) ) ,
        
        bSibG_diff_H = c( sprintf("%0.2f" , PI( Post_model_SibG$bSisG[,1] , prob = 0.90 )[2] ) , 
                          sprintf("%0.2f" , PI( Post_model_SibG$bSisG[,2] , prob = 0.90 )[2] ) , 
                          sprintf("%0.2f" , PI( Post_model_SibG$bBroG[,1] , prob = 0.90 )[2] ) , 
                          sprintf("%0.2f" , PI( Post_model_SibG$bBroG[,2] , prob = 0.90 )[2] ) ) ,
        `bSibG_diff[90%CI]` = paste( bSibG_diff , "[" , bSibG_diff_L , ", " , bSibG_diff_H , "]") , 
        Pro_bSibG_diff = c( sprintf( "%0.4f" , length( which( ( Post_model_SibG$bSisG[,1] ) > 0 ) ) / 6000 ) , 
                            sprintf( "%0.4f" , length( which( ( Post_model_SibG$bSisG[,2] ) > 0 ) ) / 6000 ) , 
                            sprintf( "%0.4f" , length( which( ( Post_model_SibG$bBroG[,1] ) > 0 ) ) / 6000 ) , 
                            sprintf( "%0.4f" , length( which( ( Post_model_SibG$bBroG[,2] ) > 0 ) ) / 6000 ) ) ) %>% 
  mutate( Pro_bSibG_diff = paste( round( as.numeric( Pro_bSibG_diff ) * 100 , 2 ) , "%" ) ) %>% 
  select( BType , Gender , `bSibG_diff[90%CI]` , Pro_bSibG_diff ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels(everything() ) ) %>% 
  tab_style( style = list( cell_text(font = "Times New Roman") ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "center",
              columns = c( BType , Gender , `bSibG_diff[90%CI]` , Pro_bSibG_diff ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( everything() ~ px( 150 ) )

####3.2.5.2 Number of siblings stratified by gender and village----
#####3.2.5.2.1 Estimates ----
Pre_SibGV_output <- data.frame( Mean = c( Pre_model_SibGV$mean ) ,
                                CI5 = c( Pre_model_SibGV$`5%` ) ,
                                CI95 = c( Pre_model_SibGV$`95%` ) ,
                                Variable = c( "Sisters: V1 female" , 
                                              "Sisters: V1 male" ,
                                              "Sisters: V2 female" , 
                                              "Sisters: V2 male" ,
                                              "Brothers: V1 female" , 
                                              "Brothers: V1 male" ,
                                              "Brothers: V2 female" , 
                                              "Brothers: V2 male" ,
                                              
                                              "Non-kidnapping parents: female" , 
                                              "Kidnapping parents: female" ,
                                              "Non-kidnapping parents: male" , 
                                              "Kidnapping parents: male" ,
                                              
                                              "<=1960" , "1961-1965" , 
                                              "1966-1970" , "1971-1975" , "1976-1980" , 
                                              "1981-1985" , "1986-1990" , ">1990" ,
                                              
                                              "Village 1" , "Village 2" ) ) %>% 
  mutate( Variable = factor( Variable , 
                             levels = c( "Sisters: V1 female" , 
                                         "Sisters: V1 male" ,
                                         "Sisters: V2 female" , 
                                         "Sisters: V2 male" ,
                                         "Brothers: V1 female" , 
                                         "Brothers: V1 male" ,
                                         "Brothers: V2 female" , 
                                         "Brothers: V2 male" ,
                                         
                                         "Non-kidnapping parents: female" , 
                                         "Kidnapping parents: female" ,
                                         "Non-kidnapping parents: male" , 
                                         "Kidnapping parents: male" ,
                                         
                                         "<=1960" , "1961-1965" , 
                                         "1966-1970" , "1971-1975" , "1976-1980" , 
                                         "1981-1985" , "1986-1990" , ">1990" ,
                                         
                                         "Village 1" , "Village 2" ) ,
                             labels = c( "Sisters: V1 female" , 
                                         "Sisters: V1 male" ,
                                         "Sisters: V2 female" , 
                                         "Sisters: V2 male" ,
                                         "Brothers: V1 female" , 
                                         "Brothers: V1 male" ,
                                         "Brothers: V2 female" , 
                                         "Brothers: V2 male" ,
                                         
                                         "Non-kidnapping parents: female" , 
                                         "Kidnapping parents: female" ,
                                         "Non-kidnapping parents: male" , 
                                         "Kidnapping parents: male" ,
                                         
                                         "<=1960" , "1961-1965" , 
                                         "1966-1970" , "1971-1975" , "1976-1980" , 
                                         "1981-1985" , "1986-1990" , ">1990" ,
                                         
                                         "Village 1" , "Village 2" ) ) ,
          Model = "Number of siblings stratified by gender and village" , 
          "Mean [CI]" = paste( format( round( Mean , 2 ), nsmall = 2 ) , 
                               " [" , 
                               format( round( CI5 , 2 ), nsmall = 2 ) , 
                               ", " , 
                               format( round( CI95 , 2 ), nsmall = 2 ) , 
                               "]", 
                               sep = "") )

SibGV_output <- Pre_SibGV_output %>% 
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
SibGV_output

#####3.2.5.2.2 90% proportions ----
tibble( BType = c( "bSis" , "bSis" , "bSis" , "bSis" , 
                   "bBro" , "bBro" , "bBro" , "bBro" ) ,
        Village = c( "V1" , "V1" , "V2" , "V2" , 
                     "V1" , "V1" , "V2" , "V2" ) ,
        Gender = c( "Female" , "Male" , "Female" , "Male" , 
                    "Female" , "Male" , "Female" , "Male" ) ,
        bSibGV_diff = c( sprintf("%0.2f" , mean( Post_model_SibGV$bSisGV[,1,1] ) ) , 
                         sprintf("%0.2f" , mean( Post_model_SibGV$bSisGV[,2,1] ) ) , 
                         sprintf("%0.2f" , mean( Post_model_SibGV$bSisGV[,1,2] ) ) , 
                         sprintf("%0.2f" , mean( Post_model_SibGV$bSisGV[,2,2] ) ) , 
                         sprintf("%0.2f" , mean( Post_model_SibGV$bBroGV[,1,1] ) ) , 
                         sprintf("%0.2f" , mean( Post_model_SibGV$bBroGV[,2,1] ) ) ,
                         sprintf("%0.2f" , mean( Post_model_SibGV$bBroGV[,1,2] ) ) , 
                         sprintf("%0.2f" , mean( Post_model_SibGV$bBroGV[,2,2] ) ) ) ,
        
        bSibGV_diff_L = c( sprintf("%0.2f" , PI( Post_model_SibGV$bSisGV[,1,1] , prob = 0.90 )[1] ) , 
                           sprintf("%0.2f" , PI( Post_model_SibGV$bSisGV[,2,1] , prob = 0.90 )[1] ) , 
                           sprintf("%0.2f" , PI( Post_model_SibGV$bSisGV[,1,2] , prob = 0.90 )[1] ) , 
                           sprintf("%0.2f" , PI( Post_model_SibGV$bSisGV[,2,2] , prob = 0.90 )[1] ) , 
                           sprintf("%0.2f" , PI( Post_model_SibGV$bBroGV[,1,1] , prob = 0.90 )[1] ) , 
                           sprintf("%0.2f" , PI( Post_model_SibGV$bBroGV[,2,1] , prob = 0.90 )[1] ) , 
                           sprintf("%0.2f" , PI( Post_model_SibGV$bBroGV[,1,2] , prob = 0.90 )[1] ) , 
                           sprintf("%0.2f" , PI( Post_model_SibGV$bBroGV[,2,2] , prob = 0.90 )[1] ) ) ,
        
        bSibGV_diff_H = c( sprintf("%0.2f" , PI( Post_model_SibGV$bSisGV[,1,1] , prob = 0.90 )[2] ) , 
                           sprintf("%0.2f" , PI( Post_model_SibGV$bSisGV[,2,1] , prob = 0.90 )[2] ) , 
                           sprintf("%0.2f" , PI( Post_model_SibGV$bSisGV[,1,2] , prob = 0.90 )[2] ) , 
                           sprintf("%0.2f" , PI( Post_model_SibGV$bSisGV[,2,2] , prob = 0.90 )[2] ) , 
                           sprintf("%0.2f" , PI( Post_model_SibGV$bBroGV[,1,1] , prob = 0.90 )[2] ) , 
                           sprintf("%0.2f" , PI( Post_model_SibGV$bBroGV[,2,1] , prob = 0.90 )[2] ) , 
                           sprintf("%0.2f" , PI( Post_model_SibGV$bBroGV[,1,2] , prob = 0.90 )[2] ) , 
                           sprintf("%0.2f" , PI( Post_model_SibGV$bBroGV[,2,2] , prob = 0.90 )[2] ) ) ,
        `bSibGV_diff[90%CI]` = paste( bSibGV_diff , "[" , bSibGV_diff_L , ", " , bSibGV_diff_H , "]") , 
        Pro_bSibGV_diff = c( sprintf( "%0.4f" , length( which( ( Post_model_SibGV$bSisGV[,1,1] ) > 0 ) ) / 6000 ) , 
                             sprintf( "%0.4f" , length( which( ( Post_model_SibGV$bSisGV[,2,1] ) > 0 ) ) / 6000 ) , 
                             sprintf( "%0.4f" , length( which( ( Post_model_SibGV$bSisGV[,1,2] ) > 0 ) ) / 6000 ) , 
                             sprintf( "%0.4f" , length( which( ( Post_model_SibGV$bSisGV[,2,2] ) > 0 ) ) / 6000 ) , 
                             sprintf( "%0.4f" , length( which( ( Post_model_SibGV$bBroGV[,1,1] ) > 0 ) ) / 6000 ) , 
                             sprintf( "%0.4f" , length( which( ( Post_model_SibGV$bBroGV[,2,1] ) > 0 ) ) / 6000 ) , 
                             sprintf( "%0.4f" , length( which( ( Post_model_SibGV$bBroGV[,1,2] ) > 0 ) ) / 6000 ) , 
                             sprintf( "%0.4f" , length( which( ( Post_model_SibGV$bBroGV[,2,2] ) > 0 ) ) / 6000 ) ) ) %>% 
  mutate( Pro_bSibGV_diff = paste( round( as.numeric( Pro_bSibGV_diff ) * 100 , 2 ) , "%" ) ) %>% 
  select( BType , Village , Gender , `bSibGV_diff[90%CI]` , Pro_bSibGV_diff ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels(everything() ) ) %>% 
  tab_style( style = list( cell_text(font = "Times New Roman") ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "center",
              columns = c( BType , Village , Gender , `bSibGV_diff[90%CI]` , Pro_bSibGV_diff ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( everything() ~ px( 150 ) )

####3.2.5.3 Older siblings' marriage type stratified by gender ----
#####3.2.5.3.1 Estimates ----
Pre_SibMG_output <- data.frame( Mean = c( Pre_model_SibMG$mean ) ,
                                CI5 = c( Pre_model_SibMG$`5%` ) ,
                                CI95 = c( Pre_model_SibMG$`95%` ) ,
                                Variable = c( "Kidnapping older sisters: female" , 
                                              "Kidnapping older sisters: male" ,
                                              "Kidnapping older brothers: female" , 
                                              "Kidnapping older brothers: male" ,
                                              
                                              "Sisters: female" , 
                                              "Sisters: male" ,
                                              "Brothers: female" , 
                                              "Brothers: male" ,
                                              
                                              "Non-kidnapping parents: female" , 
                                              "Kidnapping parents: female" ,
                                              "Non-kidnapping parents: male" , 
                                              "Kidnapping parents: male" ,
                                              
                                              "<=1960" , "1961-1965" , 
                                              "1966-1970" , "1971-1975" , "1976-1980" , 
                                              "1981-1985" , "1986-1990" , ">1990" ,
                                              
                                              "Village 1" , "Village 2" ) ) %>% 
  mutate( Variable = factor( Variable , 
                             levels = c( "Kidnapping older sisters: female" , 
                                         "Kidnapping older sisters: male" ,
                                         "Kidnapping older brothers: female" , 
                                         "Kidnapping older brothers: male" ,
                                         
                                         "Sisters: female" , 
                                         "Sisters: male" ,
                                         "Brothers: female" , 
                                         "Brothers: male" ,
                                         
                                         "Non-kidnapping parents: female" , 
                                         "Kidnapping parents: female" ,
                                         "Non-kidnapping parents: male" , 
                                         "Kidnapping parents: male" ,
                                         
                                         "<=1960" , "1961-1965" , 
                                         "1966-1970" , "1971-1975" , "1976-1980" , 
                                         "1981-1985" , "1986-1990" , ">1990" ,
                                         
                                         "Village 1" , "Village 2" ) ,
                             labels = c( "Kidnapping older sisters: female" , 
                                         "Kidnapping older sisters: male" ,
                                         "Kidnapping older brothers: female" , 
                                         "Kidnapping older brothers: male" ,
                                         
                                         "Sisters: female" , 
                                         "Sisters: male" ,
                                         "Brothers: female" , 
                                         "Brothers: male" ,
                                         
                                         "Non-kidnapping parents: female" , 
                                         "Kidnapping parents: female" ,
                                         "Non-kidnapping parents: male" , 
                                         "Kidnapping parents: male" ,
                                         
                                         "<=1960" , "1961-1965" , 
                                         "1966-1970" , "1971-1975" , "1976-1980" , 
                                         "1981-1985" , "1986-1990" , ">1990" ,
                                         
                                         "Village 1" , "Village 2" ) ) ,
          Model = "Kidnapping older siblings stratified by gender" , 
          "Mean [CI]" = paste( format( round( Mean , 2 ), nsmall = 2 ) , 
                               " [" , 
                               format( round( CI5 , 2 ), nsmall = 2 ) , 
                               ", " , 
                               format( round( CI95 , 2 ), nsmall = 2 ) , 
                               "]", 
                               sep = "") )

SibMG_output <- Pre_SibMG_output %>% 
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
SibMG_output

#####3.2.5.3.2 90% proportions ----
tibble( BType = c( "bOSisMG" , "bOSisMG" , "bOBroMG" , "bOBroMG" ) ,
        Gender = c( "Female" , "Male" , "Female" , "Male" ) ,
        bOSibMG_diff = c( sprintf("%0.2f" , mean( Post_model_SibMG$bOSisMG[,1] ) ) , 
                          sprintf("%0.2f" , mean( Post_model_SibMG$bOSisMG[,2] ) ) , 
                          sprintf("%0.2f" , mean( Post_model_SibMG$bOBroMG[,1] ) ) , 
                          sprintf("%0.2f" , mean( Post_model_SibMG$bOBroMG[,2] ) ) ) ,
        
        bOSibMG_diff_L = c( sprintf("%0.2f" , PI( Post_model_SibMG$bOSisMG[,1] , prob = 0.90 )[1] ) , 
                            sprintf("%0.2f" , PI( Post_model_SibMG$bOSisMG[,2] , prob = 0.90 )[1] ) , 
                            sprintf("%0.2f" , PI( Post_model_SibMG$bOBroMG[,1] , prob = 0.90 )[1] ) , 
                            sprintf("%0.2f" , PI( Post_model_SibMG$bOBroMG[,2] , prob = 0.90 )[1] ) ) ,
        
        bOSibMG_diff_H = c( sprintf("%0.2f" , PI( Post_model_SibMG$bOSisMG[,1] , prob = 0.90 )[2] ) , 
                            sprintf("%0.2f" , PI( Post_model_SibMG$bOSisMG[,2] , prob = 0.90 )[2] ) , 
                            sprintf("%0.2f" , PI( Post_model_SibMG$bOBroMG[,1] , prob = 0.90 )[2] ) , 
                            sprintf("%0.2f" , PI( Post_model_SibMG$bOBroMG[,2] , prob = 0.90 )[2] ) ) ,
        `bOSibMG_diff[90%CI]` = paste( bOSibMG_diff , "[" , bOSibMG_diff_L , ", " , bOSibMG_diff_H , "]") , 
        Pro_bOSibMG_diff = c( sprintf( "%0.4f" , length( which( ( Post_model_SibMG$bOSisMG[,1] ) > 0 ) ) / 6000 ) , 
                              sprintf( "%0.4f" , length( which( ( Post_model_SibMG$bOSisMG[,2] ) > 0 ) ) / 6000 ) , 
                              sprintf( "%0.4f" , length( which( ( Post_model_SibMG$bOBroMG[,1] ) > 0 ) ) / 6000 ) , 
                              sprintf( "%0.4f" , length( which( ( Post_model_SibMG$bOBroMG[,2] ) > 0 ) ) / 6000 ) ) ) %>% 
  mutate( Pro_bOSibMG_diff = paste( round( as.numeric( Pro_bOSibMG_diff ) * 100 , 2 ) , "%" ) ) %>% 
  select( BType , Gender , `bOSibMG_diff[90%CI]` , Pro_bOSibMG_diff ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels(everything() ) ) %>% 
  tab_style( style = list( cell_text(font = "Times New Roman") ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "center",
              columns = c( BType , Gender , `bOSibMG_diff[90%CI]` , Pro_bOSibMG_diff ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( everything() ~ px( 150 ) )

####3.2.5.4 Older siblings' marriage type stratified by village and gender ----
#####3.2.5.4.1 Estimates ----
Pre_SibMGV_output <- data.frame( Mean = c( Pre_model_SibMGV$mean ) ,
                                 CI5 = c( Pre_model_SibMGV$`5%` ) ,
                                 CI95 = c( Pre_model_SibMGV$`95%` ) ,
                                 Variable = c( "Kidnapping older sisters: V1 female" , 
                                               "Kidnapping older sisters: V1 male" ,
                                               "Kidnapping older sisters: V2 female" , 
                                               "Kidnapping older sisters: V2 male" ,
                                               
                                               "Kidnapping older brothers: V1 female" , 
                                               "Kidnapping older brothers: V1 male" ,
                                               "Kidnapping older brothers: V2 female" , 
                                               "Kidnapping older brothers: V2 male" ,
                                               
                                               "Sisters: female" , 
                                               "Sisters: male" ,
                                               "Brothers: female" , 
                                               "Brothers: male" ,
                                               
                                               "Non-kidnapping parents-Female" , 
                                               "Kidnapping parents-Female" ,
                                               "Non-kidnapping parents-Male" , 
                                               "Kidnapping parents-Male" ,
                                               
                                               "<=1960" , "1961-1965" , 
                                               "1966-1970" , "1971-1975" , "1976-1980" , 
                                               "1981-1985" , "1986-1990" , ">1990" ,
                                               
                                               "Village 1" , "Village 2" ) ) %>% 
  mutate( Variable = factor( Variable , 
                             levels = c( "Kidnapping older sisters: V1 female" , 
                                         "Kidnapping older sisters: V1 male" ,
                                         "Kidnapping older sisters: V2 female" , 
                                         "Kidnapping older sisters: V2 male" ,
                                         
                                         "Kidnapping older brothers: V1 female" , 
                                         "Kidnapping older brothers: V1 male" ,
                                         "Kidnapping older brothers: V2 female" , 
                                         "Kidnapping older brothers: V2 male" ,
                                         
                                         "Sisters: female" , 
                                         "Sisters: male" ,
                                         "Brothers: female" , 
                                         "Brothers: male" ,
                                         
                                         "Non-kidnapping parents-Female" , 
                                         "Kidnapping parents-Female" ,
                                         "Non-kidnapping parents-Male" , 
                                         "Kidnapping parents-Male" ,
                                         
                                         "<=1960" , "1961-1965" , 
                                         "1966-1970" , "1971-1975" , "1976-1980" , 
                                         "1981-1985" , "1986-1990" , ">1990" ,
                                         
                                         "Village 1" , "Village 2" ) ,
                             labels = c( "Kidnapping older sisters: V1 female" , 
                                         "Kidnapping older sisters: V1 male" ,
                                         "Kidnapping older sisters: V2 female" , 
                                         "Kidnapping older sisters: V2 male" ,
                                         
                                         "Kidnapping older brothers: V1 female" , 
                                         "Kidnapping older brothers: V1 male" ,
                                         "Kidnapping older brothers: V2 female" , 
                                         "Kidnapping older brothers: V2 male" ,
                                         
                                         "Sisters: female" , 
                                         "Sisters: male" ,
                                         "Brothers: female" , 
                                         "Brothers: male" ,
                                         
                                         "Non-kidnapping parents-Female" , 
                                         "Kidnapping parents-Female" ,
                                         "Non-kidnapping parents-Male" , 
                                         "Kidnapping parents-Male" ,
                                         
                                         "<=1960" , "1961-1965" , 
                                         "1966-1970" , "1971-1975" , "1976-1980" , 
                                         "1981-1985" , "1986-1990" , ">1990" ,
                                         
                                         "Village 1" , "Village 2" ) ) ,
          Model = "Kidnapping older siblings stratified by village and gender" , 
          "Mean [CI]" = paste( format( round( Mean , 2 ), nsmall = 2 ) , 
                               " [" , 
                               format( round( CI5 , 2 ), nsmall = 2 ) , 
                               ", " , 
                               format( round( CI95 , 2 ), nsmall = 2 ) , 
                               "]" , 
                               sep = "") )

SibMGV_output <- Pre_SibMGV_output %>% 
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
SibMGV_output

#####3.2.5.4.2 90% proportions ----
tibble( BType = c( "bOSisMG" , "bOSisMG" , "bOSisMG" , "bOSisMG" , 
                   "bOBroMG" , "bOBroMG" , "bOBroMG" , "bOBroMG" ) ,
        Village = c( "V1" , "V1" , "V2" , "V2" , 
                     "V1" , "V1" , "V2" , "V2" ) ,
        Gender = c( "Female" , "Male" , "Female" , "Male" , 
                    "Female" , "Male" , "Female" , "Male" ) ,
        bSibMGV_diff = c( sprintf("%0.2f" , mean( Post_model_SibMGV$bOSisMGV[,1,1] ) ) , 
                          sprintf("%0.2f" , mean( Post_model_SibMGV$bOSisMGV[,2,1] ) ) , 
                          sprintf("%0.2f" , mean( Post_model_SibMGV$bOSisMGV[,1,2] ) ) , 
                          sprintf("%0.2f" , mean( Post_model_SibMGV$bOSisMGV[,2,2] ) ) , 
                          sprintf("%0.2f" , mean( Post_model_SibMGV$bOBroMGV[,1,1] ) ) , 
                          sprintf("%0.2f" , mean( Post_model_SibMGV$bOBroMGV[,2,1] ) ) ,
                          sprintf("%0.2f" , mean( Post_model_SibMGV$bOBroMGV[,1,2] ) ) , 
                          sprintf("%0.2f" , mean( Post_model_SibMGV$bOBroMGV[,2,2] ) ) ) ,
        
        bSibMGV_diff_L = c( sprintf("%0.2f" , PI( Post_model_SibMGV$bOSisMGV[,1,1] , prob = 0.90 )[1] ) , 
                            sprintf("%0.2f" , PI( Post_model_SibMGV$bOSisMGV[,2,1] , prob = 0.90 )[1] ) , 
                            sprintf("%0.2f" , PI( Post_model_SibMGV$bOSisMGV[,1,2] , prob = 0.90 )[1] ) , 
                            sprintf("%0.2f" , PI( Post_model_SibMGV$bOSisMGV[,2,2] , prob = 0.90 )[1] ) , 
                            sprintf("%0.2f" , PI( Post_model_SibMGV$bOBroMGV[,1,1] , prob = 0.90 )[1] ) , 
                            sprintf("%0.2f" , PI( Post_model_SibMGV$bOBroMGV[,2,1] , prob = 0.90 )[1] ) , 
                            sprintf("%0.2f" , PI( Post_model_SibMGV$bOBroMGV[,1,2] , prob = 0.90 )[1] ) , 
                            sprintf("%0.2f" , PI( Post_model_SibMGV$bOBroMGV[,2,2] , prob = 0.90 )[1] ) ) ,
        
        bSibMGV_diff_H = c( sprintf("%0.2f" , PI( Post_model_SibMGV$bOSisMGV[,1,1] , prob = 0.90 )[2] ) , 
                            sprintf("%0.2f" , PI( Post_model_SibMGV$bOSisMGV[,2,1] , prob = 0.90 )[2] ) , 
                            sprintf("%0.2f" , PI( Post_model_SibMGV$bOSisMGV[,1,2] , prob = 0.90 )[2] ) , 
                            sprintf("%0.2f" , PI( Post_model_SibMGV$bOSisMGV[,2,2] , prob = 0.90 )[2] ) , 
                            sprintf("%0.2f" , PI( Post_model_SibMGV$bOBroMGV[,1,1] , prob = 0.90 )[2] ) , 
                            sprintf("%0.2f" , PI( Post_model_SibMGV$bOBroMGV[,2,1] , prob = 0.90 )[2] ) , 
                            sprintf("%0.2f" , PI( Post_model_SibMGV$bOBroMGV[,1,2] , prob = 0.90 )[2] ) , 
                            sprintf("%0.2f" , PI( Post_model_SibMGV$bOBroMGV[,2,2] , prob = 0.90 )[2] ) ) ,
        
        `bSibMGV_diff[90%CI]` = paste( bSibMGV_diff , "[" , bSibMGV_diff_L , ", " , bSibMGV_diff_H , "]") , 
        Pro_bSibMGV_diff = c( sprintf( "%0.4f" , length( which( ( Post_model_SibMGV$bOSisMGV[,1,1] ) > 0 ) ) / 6000 ) , 
                              sprintf( "%0.4f" , length( which( ( Post_model_SibMGV$bOSisMGV[,2,1] ) > 0 ) ) / 6000 ) , 
                              sprintf( "%0.4f" , length( which( ( Post_model_SibMGV$bOSisMGV[,1,2] ) > 0 ) ) / 6000 ) , 
                              sprintf( "%0.4f" , length( which( ( Post_model_SibMGV$bOSisMGV[,2,2] ) > 0 ) ) / 6000 ) , 
                              sprintf( "%0.4f" , length( which( ( Post_model_SibMGV$bOBroMGV[,1,1] ) > 0 ) ) / 6000 ) , 
                              sprintf( "%0.4f" , length( which( ( Post_model_SibMGV$bOBroMGV[,2,1] ) > 0 ) ) / 6000 ) , 
                              sprintf( "%0.4f" , length( which( ( Post_model_SibMGV$bOBroMGV[,1,2] ) > 0 ) ) / 6000 ) , 
                              sprintf( "%0.4f" , length( which( ( Post_model_SibMGV$bOBroMGV[,2,2] ) > 0 ) ) / 6000 ) ) ) %>% 
  mutate( Pro_bSibMGV_diff = paste( round( as.numeric( Pro_bSibMGV_diff ) * 100 , 2 ) , "%" ) ) %>% 
  select( BType , Village , Gender , `bSibMGV_diff[90%CI]` , Pro_bSibMGV_diff ) %>% 
  gt() %>% 
  fmt_number( ) %>% 
  sub_missing( columns = everything() , 
               missing_text = " " ) %>% 
  tab_style( style = list( cell_text( weight = "bold" ) ) ,
             locations = cells_column_labels(everything() ) ) %>% 
  tab_style( style = list( cell_text(font = "Times New Roman") ) ,
             locations = cells_body() ) %>% 
  cols_align( align = "center",
              columns = c( BType , Village , Gender , `bSibMGV_diff[90%CI]` , Pro_bSibMGV_diff ) ) %>% 
  opt_table_lines( extent = "default" ) %>%
  tab_options( column_labels.border.top.color = "black" ,
               column_labels.border.top.width = px( 3 ) ,
               column_labels.border.bottom.color = "black" ,
               table_body.hlines.color = "lightgrey" ,
               table.border.bottom.color = "black" ,
               table.border.bottom.width = px( 3 ) ) %>% 
  cols_width( everything() ~ px( 150 ) )
