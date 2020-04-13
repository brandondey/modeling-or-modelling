library(dplyr)
library(magrittr)
library(ggplot2)
library(purrr)
library(stringr)
library(tidyr)

l <- read.csv("modeling_output.csv")
ll <- read.csv("modelling_output.csv")

l$one_l <- 1
ll$one_l <- 0
all  <- rbind(l, ll)

all %>%  
  mutate(created = lubridate::ymd(created)) %>% 
  arrange(created) -> all


summary(all)

cats = c('astro-ph', 'cond-mat', 'gr-qc', 'hep-ex', 'hep-lat', 'hep-ph', 'hep-th',
        'math-ph', 'nlin', 'nucl-ex', 'nucl-th', 'physics', 'quant-ph', 'math', 
        'CoRR', 'q-bio', 'q-fin', 'stat')


detection <- map(cats, function(cat) grepl(cat, 
                              as.character(all$categories)))
names(detection) <- cats

cat_n <- map_int(detection, sum)
names(cat_n) <- cats
all <- cbind(all, as.data.frame(detection)) 


stat_sub_cats <- c('stat.AP', 'stat.CO', 'stat.ML','stat.ME', 'stat.OT', 'stat.TH') %>%  tolower()
stat_det <- map(stat_sub_cats, function(cat) grepl(cat, 
                                           as.character(all$categories)))
names(stat_det) <- stat_sub_cats
stat_cat_n <- map_int(stat_det, sum)
names(stat_cat_n) <- stat_sub_cats

all <- cbind(all, as.data.frame(stat_det)) 

all %>%  
  pivot_longer(contains("stat."), names_to = "stat_sub_cat", 
               values_to = "on") -> stat_long  

all %>%  
  pivot_longer(contains(cats), names_to = "cat", 
               values_to = "on") -> all_long

all_long %>%    
  group_by(cat) %>%  
  summarize(n = sum(on), 
            one_l = sum(on == T & one_l == 1), 
            prop_l = one_l / n) %>% 
  arrange(prop_l) -> all_sum

stat_long %>%    
  group_by(stat_sub_cat) %>%  
  summarize(n = sum(on), 
            one_l = sum(on == T & one_l == 1), 
            prop_l = one_l / n) %>% 
  arrange(prop_l) -> stat_sum


stat_sum %>%  
  ggplot() + 
  geom_bar(aes(reorder(stat_sub_cat, -prop_l), prop_l), stat = "identity") + 
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "Title", 
       y = "% of Abstracts using 'modeling' (instead of 'modelling')", 
       x = "", 
       caption = "") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

## timeseries
stat_long %>%  
  mutate(yr = lubridate::year(created), 
         stat_sub_cat = case_when(stat_sub_cat == "stat.co" ~ "Computation, Statistics", 
                                  stat_sub_cat == "stat.me" ~ "Methodology, Statistics", 
                                  stat_sub_cat == "stat.ap" ~ "Applications, Statistics", 
                                  stat_sub_cat == "stat.ml" ~ "Machine Learning, Statistics", 
                                  stat_sub_cat == "stat.ot" ~ "Other, Statistics", 
                                  stat_sub_cat == "stat.th" ~ "Theory, Statistics")) %>%  
  group_by(stat_sub_cat, yr) %>%  
  summarize(n = sum(on), 
            one_l = sum(on == T & one_l == 1), 
            prop_l = one_l / n)  %>%  
  arrange(yr) -> stat_long_sum 
  
stat_long_sum %>% 
  ggplot(aes(yr, prop_l, col = as.factor(stat_sub_cat))) + 
  geom_point(stat = "identity") + 
  geom_line() +
  scale_y_continuous(labels = scales::percent) + 
  # scale_color_brewer(type = "seq", "BuGn") + 
  labs(title = "'Modeling' is/has been Preferred to 'Modelling' in Statistics Literature on arXiv",
       subtitle = "Since at least 2007",
       y = "% of Abstracts using 'modeling' (instead of 'modelling')", 
       x = "", 
       caption = "Only abstracts containing 'modeling' or 'modelling' were scraped from Cornell's arXiv.org with Mahdi Sadjadi's arxivscraper for Python on 4/12/20
       
       Mahdi Sadjadi (2017). arxivscraper: Zenodo. http://doi.org/10.5281/zenodo.889853") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  facet_wrap(~reorder(stat_sub_cat, prop_l)) + 
  guides(col = F)

ggsave("Modeling Freq by Statistics Topics.jpeg", 
       dpi = 1000, 
       height = 8, width = 8)

# Save data
write.csv(all, "clean_stat_abstracts.csv")
