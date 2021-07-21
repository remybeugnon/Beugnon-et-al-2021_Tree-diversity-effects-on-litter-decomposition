#---------------------------------------------
# platform       x86_64-w64-mingw32          
# arch           x86_64                      
# os             mingw32                     
# system         x86_64, mingw32             
# status                                     
# major          4                           
# minor          1.0                         
# year           2021                        
# month          05                          
# day            18                          
# svn rev        80317                       
# language       R                           
# version.string R version 4.1.0 (2021-05-18)
# nickname       Camp Pontanezen 
#---------------------------------------------
# Packages:
#   - tidyverse_1.3.0
#     - ggplot2_3.3.3
#     - dplyr_1.0.4
#     - purrr_0.3.4
#     - tidyr_1.1.3
#     - stringr_1.4.0
#   - ggeffects_1.0.2
#   - ggforce_0.3.2
#   - ggpubr_0.4.0
#---------------------------------------------
#### > 0 Start ####
# Cleaning environment
rm(list = ls())

# Fixing random structure
set.seed(1232)

# Loading packages
libs <- c('tidyverse', 
          'ggeffects',
          'ggforce', 
          'ggpubr')

invisible(lapply(libs, library, character.only = T))

# Extract significance from model output
extract.signif = function(signif){
  signif = if(signif < 0.001){
    'p-value < 0.001 ***'
  } else {
    paste0('p-value = ',round(signif, 3), if(signif < 0.01){
      ' **'
    } else if(signif >= 0.01 & signif < 0.05){
      " *"
    } else if(signif >= 0.05 & signif < 0.1){
      " ."
    } else {' '})
  }
  return(signif)
}

#### > 1 Data ####
df = read.csv(file = "df.csv")

#### > 2 Statistical analyses ####
#### >> 2.1 Tree diversity effects on C and N loss ####
mod.c = lm(data = df, formula = "C.loss_Ma1 ~ log(neigh.sp.rich)")
mod.n = lm(data = df, formula = "N.loss_Ma1 ~ log(neigh.sp.rich)")

#### >> 2.2 Tree diversity effects on microbial C and N loss ####
mod.c.micro = lm(data = df, formula = "C.loss_Mi1 ~ log(neigh.sp.rich)")
mod.n.micro = lm(data = df, formula = "N.loss_Mi1 ~ log(neigh.sp.rich)")

#### >> 2.3 Tree diversity effects on litter decomposability ####
mod.c.cg = lm(data = df, formula = "C.loss_CG ~ log(neigh.sp.rich)")
mod.n.cg = lm(data = df, formula = "N.loss_CG ~ log(neigh.sp.rich)")

#### > 3 Plot ####
# Set theme
thm = theme() + 
  theme_bw()

#### >> 3.1 C loss ####
# Extract model output
pred.mod.c = ggpredict(mod.c, terms = 'neigh.sp.rich')
# Extract variable significance
signif = summary(mod.c)$coefficients[2,4] 
signif.code = extract.signif(signif = signif)

p.c =
  ggplot(data = pred.mod.c, 
         aes(x = x, y = predicted)) + 
  geom_ribbon(data = pred.mod.c, 
              aes(x = x, 
                  ymin = conf.low, 
                  ymax = conf.high), 
              color = 'gray', 
              alpha = 0.2) + 
  geom_line(data = pred.mod.c, 
            aes(x = x, 
                y = predicted)) + 
  geom_jitter(data = df, 
              aes(y = C.loss_Ma1, 
                  x = neigh.sp.rich)) + 
  annotate(geom = 'text', 
           x = 1, y = 100, 
           label = signif.code, 
           hjust = 0) +
  labs(x = " ", y = " C loss [%]", 
       title = "Decomposition") + 
  scale_x_continuous(trans = 'log', breaks = c(1,2,4,8)) + 
  thm

#### >> 3.2 N loss ####
pred.mod.n = ggpredict(mod.n, terms = 'neigh.sp.rich')
signif = summary(mod.n)$coefficients[2,4] 
signif.code = extract.signif(signif = signif)

p.n =
  ggplot(data = pred.mod.n, 
         aes(x = x , y = predicted)) + 
  geom_ribbon(data = pred.mod.n, 
              aes(x = x , 
                  ymin = conf.low,
                  ymax = conf.high), 
              color = 'gray',
              alpha = 0.2) + 
  geom_line(data = pred.mod.n, 
            aes(x = x , y = predicted)) + 
  geom_jitter(data = df, 
              aes(y = N.loss_Ma1, x = neigh.sp.rich)) + 
  annotate(geom = 'text', 
           x = 1, y = 110, 
           label = signif.code, 
           hjust = 0) +
  labs(x = " ", y = " N loss [%]") + 
  scale_x_continuous(trans = 'log', breaks = c(1,2,4,8)) +  
  thm

#### >> 3.3 Microbial C loss ####
pred.mod.c.micro = ggpredict(mod.c.micro, terms = 'neigh.sp.rich')
signif = summary(mod.c.micro)$coefficients[2,4] 
signif.code = extract.signif(signif = signif)

p.c.micro =
  ggplot(data = pred.mod.c.micro, 
         aes(x = x , y = predicted)) + 
  geom_ribbon(data = pred.mod.c.micro, 
              aes(x = x , 
                  ymin = conf.low, 
                  ymax = conf.high), 
              color = 'gray', 
              alpha = 0.2) + 
  geom_line(data = pred.mod.c.micro, 
            aes(x = x , y = predicted),
            color = 'gray') + 
  geom_jitter(data = df, 
              aes(y = C.loss_Mi1, x = neigh.sp.rich),
              color = 'gray') + 
  annotate(geom = 'text', x = 1, y = 110, label = signif.code, hjust = 0) +
  labs(x = " ", y = " Microbial C loss [%]", 
       title = "Microbial decomposition") + 
  scale_x_continuous(trans = 'log', breaks = c(1,2,4,8)) + 
  thm

#### >> 3.4 Microbial N loss ####
pred.mod.n.micro = ggpredict(mod.n.micro, terms = 'neigh.sp.rich')
signif = summary(mod.n.micro)$coefficients[2,4] 
signif.code = extract.signif(signif = signif)

p.n.micro =
  ggplot(data = pred.mod.n.micro, 
         aes(x = x , y = predicted)) + 
  geom_ribbon(data = pred.mod.n.micro, 
              aes(x = x , 
                  ymin = conf.low, 
                  ymax = conf.high), 
              color = 'gray', 
              alpha = 0.2) + 
  geom_line(data = pred.mod.n.micro, 
            aes(x = x , y = predicted)) + 
  geom_jitter(data = df, 
              aes(y = N.loss_Mi1, x = neigh.sp.rich)) + 
  annotate(geom = 'text', 
           x = 1, y = 110, 
           label = signif.code, 
           hjust = 0) +
  labs(x = " ", y = "Microbial N loss [%]") + 
  scale_x_continuous(trans = 'log', breaks = c(1,2,4,8)) +
  thm

#### >> 3.5 Decomposability C loss ####
pred.mod.c.cg = ggpredict(mod.c.cg, terms = 'neigh.sp.rich')
signif = summary(mod.c.cg)$coefficients[2,4] 
signif.code = extract.signif(signif = signif)

p.c.cg =
  ggplot(data = pred.mod.c.cg, 
         aes(x = x , y = predicted)) + 
  geom_ribbon(data = pred.mod.c.cg, 
              aes(x = x , 
                  ymin = conf.low, 
                  ymax = conf.high), 
              color = 'gray', 
              alpha = 0.2) + 
  geom_line(data = pred.mod.c.cg, 
            aes(x = x , y = predicted),
            color = 'gray') + 
  geom_jitter(data = df, 
              aes(y = C.loss_CG, x = neigh.sp.rich),
              color = 'gray') + 
  annotate(geom = 'text', 
           x = 1, y = 110, 
           label = signif.code, 
           hjust = 0) +
  labs(x = "Neighborhood species richness", 
       y = "Decomposability in C loss [%]", 
       title = "Decomposability") + 
  scale_x_continuous(trans = 'log', breaks = c(1,2,4,8)) +  
  thm

#### >> 3.6 Decomposability N loss ####
pred.mod.n.cg = ggpredict(mod.n.cg, terms = 'neigh.sp.rich')
signif = summary(mod.n.cg)$coefficients[2,4] 
signif.code = extract.signif(signif = signif)

p.n.cg =
  ggplot(data = pred.mod.n.cg, 
         aes(x = x , y = predicted)) + 
  geom_ribbon(data = pred.mod.n.cg, 
              aes(x = x , 
                  ymin = conf.low, 
                  ymax = conf.high), 
              color = 'gray', 
              alpha = 0.2) + 
  geom_line(data = pred.mod.n.cg, 
            aes(x = x , y = predicted)) + 
  geom_jitter(data = df, 
              aes(y = N.loss_CG, x = neigh.sp.rich)) + 
  annotate(geom = 'text', x = 1, y = 90, 
           label = signif.code, hjust = 0) +
  labs(x = "Neighborhood species richness", 
       y = "Decomposability in N loss [%]") + 
  scale_x_continuous(trans = 'log', breaks = c(1,2,4,8)) + 
  thm

#### >> 3.7 Figure 1 ####
fig2 =
  ggarrange(
    p.c, p.n,
    p.c.micro, p.n.micro,
    p.c.cg, p.n.cg,
    nrow = 3, ncol = 2,
    align = 'hv', 
    labels = LETTERS[1:6]
  )

# Save
ggsave(plot = fig2, 
       filename = "figure2.png",
       height = 10, width = 7)