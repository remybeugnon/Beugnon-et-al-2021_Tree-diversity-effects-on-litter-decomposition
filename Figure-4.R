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
#---------------------------------------------
#### > 0 Start ####
# Cleaning environment
rm(list = ls())

# Fixing random structure
set.seed(1232)

# Loading packages
libs <- c('tidyverse')

invisible(lapply(libs, library, character.only = T))

#### > 1 Data ####
#### >> 1.1 Decomposition data ####
df = read.csv(file = "df.csv")
# Removing 1 ourlier
df = df %>% 
  filter(C.loss > 50)
# Scaling explanatory variables
exp = c('ini.SLA', 'ini.LDMC', 'ini.C', 'CN',  'CP', 'var.x')
df[, exp] = df[, exp] %>% 
  apply(., 2, scale)

#### >> 1.2 Litterfall ####
df.2 = read.csv(file =  "df-2.csv")

exp = c("N", "C", "SLA", "LDMC", "dist", "log.biomass")
df.2[, exp] = df.2[, exp] %>% 
  apply(., 2, scale)

#### > 2 Statistical analyses ####
#### >> 2.1 Decomposability drivers ####
#### >>> 2.1.1 C loss ####
mod.c = lm(C.loss ~ ini.SLA + ini.LDMC + ini.C + CN +  CP + var.x + log(lit.rich), 
           data = df) %>% 
  step(direction = 'both', trace = F)

#### >>> 2.1.2 N loss ####
mod.n = lm(N.loss ~ ini.SLA + ini.LDMC + ini.C + CN + CP + var.x + log(lit.rich), 
           data = df) %>% 
  step(direction = 'both', trace = F)

#### >>> 2.1.3 Summary extraction ####
df.decomp.drivers = data.frame(
  var = factor(c('ini.SLA','ini.LDMC', 'ini.C','CN','CP','var.x', 'log(lit.rich)')))  %>%
  # C loss drivers
  left_join(., 
            summary(mod.c)$coefficients %>%
              data.frame() %>%
              select(est.c = Estimate, se.c = `Std..Error`, p.c = `Pr...t..`) %>%
              mutate(var = row.names(.)) %>%
              # Confidence interval
              mutate(int.pos.c = est.c + 1.96 * se.c) %>%
              mutate(int.neg.c = est.c - 1.96 * se.c),
            by = 'var') %>%
  # N loss divers
  left_join(., 
            summary(mod.n)$coefficients %>%
              data.frame() %>%
              select(est.n = Estimate, se.n = `Std..Error`, p.n = `Pr...t..`) %>%
              mutate(var = row.names(.)) %>%
              # Confidence interval
              mutate(int.pos.n = est.n + 1.96 * se.n) %>%
              mutate(int.neg.n = est.n - 1.96 * se.n),
            by = 'var')

df.decomp.drivers$var = df.decomp.driverss$var %>% 
  factor(., levels =  c('log(lit.rich)','var.x','CP','CN','ini.C', 'ini.LDMC','ini.SLA'))

# Significance
df.decomp.drivers$sign.c = ifelse(df.decomp.drivers$p.c < 0.05, 'sign', 'sign-')
df.decomp.drivers$sign.n = ifelse(df.decomp.drivers$p.n < 0.05, 'sign', 'sign-')

#### >> 2.2 Litterfall drivers ####
#### >>> 2.2.1 Model ####
mod.lit = "log.litter.biomass.area ~ log.biomass + dist + LDMC + SLA + C + N" %>%
  as.formula() %>%
  lm(data = df.2) %>%
  step(direction = 'both', trace = F)

#### >>> 2.2.2 Summary extraction ####
df.fall.drivers = summary(mod.lit)$coefficients %>% 
  data.frame() %>%
  filter(row.names(.) != "(Intercept)") %>%
  mutate(variable = row.names(.) %>% factor(., levels = c("N", "C", "SLA", "LDMC", "dist", "log.biomass"))) %>%
  mutate(lty = if_else(condition = Pr...t..<0.05,true = '1',false = '2'))

#### > 3 Plot ####
#### >> 3.1 Plot decomposability drivers ####
p.decomp = 
  ggplot(data = df.decomp.drivers, 
         aes(x = 1:8, y = est.c)) +
  geom_hline(aes(yintercept = 0), lty = 2) + 
  geom_point(data = df.results, 
             aes(x = as.numeric(var) + .15, y = est.c)) + 
  geom_errorbar(data = df.decomp.drivers, 
                aes(x = as.numeric(var) + .15, 
                    ymin = int.neg.c, 
                    ymax = int.pos.c, 
                    lty = sign.c), 
                width = .1) + 
  geom_point(data = df.decomp.drivers, 
             aes(x = as.numeric(var) - .15, 
                 y = est.n), 
             color = 'red') + 
  geom_errorbar(data = decomp.drivers, 
                aes(x = as.numeric(var) - .15, 
                    ymin = int.neg.n, 
                    ymax = int.pos.n, 
                    lty = sign.n), 
                width = .1, 
                color = 'red') +
  scale_x_continuous(breaks = 1:7,
                     labels = c('1' = 'Litter spe. richnness', 
                                '2' = 'Chemistry coupling', 
                                '3' = '[C]/[P]', 
                                '4' = '[C]/[N]' ,
                                '5' = '[C]', 
                                '6' = "LDMC", 
                                "7" = 'SLA')) +
  labs(y = 'Estimate', x = '') + 
  lims(y = c(-4,4)) +
  coord_flip() + 
  theme_classic() + 
  theme(legend.position = 'none', panel.grid = element_blank()) 

ggsave(p.decomp, 
       filename = "decomposability-drivers-trait.png",
       height = 12, width = 7.5, 
       unit = 'cm')

#### >> 3.2 Plot litterfall drivers ####
p.fall = 
  ggplot(data = df.fall.driver, 
         aes(x = as.numeric(variable), y = Estimate)) + 
  geom_hline(yintercept = 0) + 
  geom_point() + 
  geom_errorbar(aes(ymin = Estimate - 1.96 * Std..Error, 
                    ymax = Estimate + 1.96 * Std..Error, 
                    lty = lty), 
                width = .1) + 
  labs(y = "Estimate", x = "") +
  scale_x_continuous(breaks = c(1:6), 
                     labels = c('1' = '[N]', 
                                '2' = '[C]', 
                                '3' = 'SLA', 
                                '4' = "LDMC" ,
                                '5' = "1/dist", 
                                '6' = "log(biomass)")) + 
  lims(y = c(-1,1)) + 
  coord_flip() + 
  theme_classic() + 
  theme(legend.position = 'none', 
        plot.background = element_rect(fill = NA),
        panel.background = element_rect(fill = NA))

ggsave(p.fall, 
       filename = "litterfall-drivers-trait.png",
       height = 10, width = 7.5, unit = 'cm')

#### >> 3.3 Final figure ####
# The figure 4 have been assembled manually with Inkscape (1.0.2-2 (e86c870879, 2021-01-15)) 