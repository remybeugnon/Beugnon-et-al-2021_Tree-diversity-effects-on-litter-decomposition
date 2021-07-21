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
#   - vegan_2.5-7 
#   - lavaan_0.6-7
#   - ggeffects_1.0.2
#   - ggforce_0.3.2
#   - ggplotify_0.0.5
#   - ggpubr_0.4.0
#   - ggimage_0.2.8
#---------------------------------------------
#### > 0. Start ####
# Cleaning environment
rm(list = ls())

# Fixing random structure
set.seed(1232)

# Loading packages
libs <- c('tidyverse',
          'vegan', 'lavaan', 
          'ggeffects',  
          'ggforce',
          'ggplotify',
          'ggpubr', 
          'ggimage')

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
#### >> 2.2. Tree species richness effect on litterfall ####
#### >> 2.2.1 Tree species richness effect on the amount of litterfall ####
mod.fall = lm(data = df, formula = 'fall ~ log(neigh.sp.rich)')

#### >> 2.2.2 Tree species richness effect on litter species richness ####
mod.lit.rich = lm(data = df, formula = 'log(lit.rich) ~ log(neigh.sp.rich)')

#### >> 2.3 SEM model ####
df.2 = df %>% 
  select(neigh.sp.rich, fall, lit.rich, 
         C.loss_Ma1, C.loss_Mi1, C.loss_CG,
         N.loss_Ma1, N.loss_Mi1, N.loss_CG) %>%
  mutate(log.neigh.sp.rich = log(neigh.sp.rich)) %>%
  mutate(log.lit.rich = log(lit.rich)) %>%
  apply(., 2, scale) %>%
  data.frame()

form.sem = 
  '
  C.loss_Ma1 ~ C.loss_Mi1 + fall + log.lit.rich
  N.loss_Ma1 ~ N.loss_Mi1 + fall + log.lit.rich
  C.loss_Ma1 ~~ N.loss_Ma1
  
  C.loss_Mi1 ~ C.loss_CG + fall + log.lit.rich
  N.loss_Mi1 ~ N.loss_CG + fall + log.lit.rich
  C.loss_Mi1 ~~ N.loss_Mi1
  
  C.loss_CG ~ log.lit.rich
  N.loss_CG ~ log.lit.rich
  C.loss_CG ~~ N.loss_CG
  
  fall ~ log.neigh.sp.rich
  log.lit.rich ~ log.neigh.sp.rich
  fall ~~ log.lit.rich
'
mod.sem = sem(model = form.sem, 
              data = df.2)

#### > 3 Plot ####
# Define ggplot theme 
thm = theme(axis.title.x = element_blank()) + 
  theme_bw()

#### >> 3.1 A. Litterfall ####
#### >>> 3.1.1 Amount of litterfall ####
pred.mod.fall = ggpredict(mod.fall, terms = 'neigh.sp.rich')
signif = summary(mod.fall)$coefficients[2,4] 
signif.code = extract.signif(signif = signif)

p.fall =
  ggplot(data = pred.mod.fall, 
         aes(x = x , y = predicted)) + 
  geom_ribbon(data = pred.mod.fall, 
              aes(x = x , 
                  ymin = conf.low, 
                  ymax = conf.high), 
              color = 'gray', 
              alpha = 0.2) + 
  geom_line(data = pred.mod.fall, 
            aes(x = x , y = predicted)) + 
  geom_jitter(data = df, 
              aes(y = fall, x = neigh.sp.rich)) + 
  annotate(geom = 'text', 
           x = 1, y = 300, 
           label = signif.code, hjust = 0) +
  labs(x = "", y = "Litterfall [g/m2]") + 
  scale_x_continuous(trans = 'log', breaks = c(1,2,4,8)) + 
  thm

#### >>> 3.1.2 Litter species richness ####
pred.mod.lit.rich = ggpredict(mod.lit.rich, terms = 'neigh.sp.rich')
signif = summary(mod.lit.rich)$coefficients[2,4] 
signif.code = extract.signif(signif = signif)

p.lit.rich =
  ggplot(data = pred.mod.lit.rich, 
         aes(x = x , y = predicted)) + 
  geom_abline(intercept = 0, slope = 1, 
              lty = 2 , color = 'gray') + 
  geom_ribbon(data = pred.mod.lit.rich, 
              aes(x = x , 
                  ymin = conf.low, 
                  ymax = conf.high), 
              color = 'gray', 
              alpha = 0.2) + 
  geom_line(data = pred.mod.lit.rich, 
            aes(x = x , y = predicted)) + 
  geom_jitter(data = df, 
              aes(y = lit.rich, x = neigh.sp.rich)) + 
  annotate(geom = 'text', 
           x = 1, y = 9.5, 
           label = signif.code, 
           hjust = 0) +
  labs(x = "", y = "Litterfall species richness") + 
  scale_x_continuous(trans = 'log', breaks = c(1,2,4,8)) + 
  scale_y_continuous(trans = 'log', breaks = c(1,2,4,8)) + 
  thm

#### >> 3.2 B. Part of microbial decomposition in total decomposition ####
p.part.micro =
  ggplot(data = NULL) + 
  # C loss 
  geom_violin(data = df, fill = 'gray', aes(x = 'C.loss', y = (100*(C.loss_Mi1/C.loss_Ma1)))) +
  geom_boxplot(data = df, width = .25, aes(x = 'C.loss', y = (100*(C.loss_Mi1/C.loss_Ma1)))) +
  # N loss
  geom_violin(data = df, fill = 'gray', aes(x = 'N.loss', y = (100*(N.loss_Mi1/N.loss_Ma1)))) + 
  geom_boxplot(data = df, width = .25, aes(x = 'N.loss', y = (100*(N.loss_Mi1/N.loss_Ma1)))) + 
  
  geom_hline(yintercept = 50, lty = 2) + 
  geom_hline(yintercept = 100, lty = 2) + 
  labs(x = '', y = expression(paste(frac('Microbial decomposition', 'Total decomposition'), '   (%)'))) +
  scale_x_discrete(labels = c("C loss", "N loss")) + 
  lims(y = c(0 , 130)) + 
  theme_bw()

#### >> 3.3 C. SEM plot ####
# The SEM output have been built manually with Inkscape (1.0.2-2 (e86c870879, 2021-01-15)) 
# Loading figure
d = data.frame(x=0, 
               y=0, 
               image="sem.svg")

# Incorporation to ggplot
p.sem = ggplot(d, aes(x, y, 
                      image = image)) + 
  geom_image(size = 1) + 
  coord_fixed() + 
  theme(plot.background = element_blank(),
        panel.background = element_rect(fill = 'white')) +
  theme_void()

#### >> 3.4 Figure 3 ####
fig3 =
  ggarrange(
    ggarrange(
      # Panel A
      ggarrange(
        p.fall + theme(plot.margin = margin(.5,.2,0,0, "cm")), 
        p.lit.rich + theme(plot.margin = margin(.5,0,0,.2, "cm")),
        ncol = 2,
        align = 'v'
      ) %>% 
        annotate_figure(.,
                        bottom = text_grob("Neighborhood species richness")) + 
        theme(plot.margin = margin(.5,.7,.1,.6, "cm")), 
    
      # Panel B 
      p.part.micro + theme(plot.margin = margin(1,1,1,1, "cm")), 
      heights = c(.5,.5), 
      nrow = 2,
      labels = c("A", "B")) ,
    
    # Panel C
    p.sem,
    
    # Plot sizing 
    ncol = 2,
    labels = c('','C'),
    widths = c(.4,.5))

# Save
ggsave(plot = fig3,
       filename = "Figure3.png",
       height = 6, width = 10)