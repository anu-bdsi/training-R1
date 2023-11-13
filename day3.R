#load packages ----
library(readxl)
library(janitor)
library(stringr)
library(tidyr)
library(skimr)
library(ggplot2)
library(dplyr)


df_nicotine <- read_excel(path='data/brief-access-lick-data.xlsx', 
           sheet = 'Nicotine Ave Response Latency')


#data exploration ----
View(df_nicotine)
dim(df_nicotine)
nrow(df_nicotine)
ncol(df_nicotine)
colnames(df_nicotine)

str(df_nicotine)
glimpse(df_nicotine)

head(df_nicotine, 10)
tail(df_nicotine)

summary(df_nicotine)

skim(df_nicotine)

#pipe vs nested parentheses ----
#example
max(range(sort(df_nicotine$WEIGHT)))

df_nicotine$WEIGHT %>% 
  sort() %>% 
  range() %>% 
  max()


#clean column names: string manipulation
df_nicotine$WEIGHT

#these names are not R friendly:
df_nicotine$`AVERAGE LATENCY TO FIRST LICK (SEC) (WATER)`

clean_names(df_nicotine) %>% colnames()

#apply changes
df_nicotine2 <- clean_names(df_nicotine)

#compare with previous names
df_nicotine2$average_latency_to_first_lick_sec_water

df_nicotine2 <- df_nicotine2 %>% rename_with(~str_replace(., 
                                        'average_latency_to_first_lick_sec', 
                                        'avg'))

#dplyr ----
df_nicotine2 %>% arrange(weight)
df_nicotine2 %>% arrange(desc(weight))
df_nicotine2 %>% arrange(-weight)


#count ----
df_nicotine2 %>% count(sex, prenatal_exposure)
table(df_nicotine2$sex, df_nicotine2$prenatal_exposure)


#summarise ----
df_nicotine2 %>% summarise(m1=mean(weight))
df_nicotine2 %>% 
  group_by(sex) %>% 
  summarise(meam.weight=mean(weight), 
            std=sd(weight), 
            median.weight=median(weight))


#filter ----
# == , !=, >, <, >=, =<
df_nicotine2 %>% filter(sex != 'male')
df_nicotine2 %>% filter(sex == 'male', prenatal_exposure == 'alcohol', weight > 75.1)


#select ----
#helper functions: https://tidyselect.r-lib.org/reference/starts_with.html

df_nicotine2 %>% select(animal_id, prenatal_exposure)
df_nicotine2 %>% select(starts_with('avg'))
df_nicotine2 %>% select(contains('0m'))

#mutate ----
#rename variables/ create new columns:
df_nicotine2 %>% select(animal_id:sex) %>%
  mutate(treatment=case_match(prenatal_exposure, 
                                'free choice liquid diet' ~ 'water', 
                                .default = prenatal_exposure)) %>% tail()

#apply functions to multiple columns
df_nicotine2 %>% mutate(across(avg_water:avg_6_0m_m, round))

df_nicotine2 %>% rowwise() %>%
  mutate(mean_row = mean(c_across(avg_0_1m_m:avg_6_0m_m)))


#pivot ----
#making the data tidy, i.e., ggplot2 ready!!! ----
df_pivot <- df_nicotine2 %>% 
  pivot_longer(cols = starts_with('avg_'), 
               names_to = 'concentration',
               values_to = 'latency',
               names_prefix = 'avg_')


#saving new dataframe as csv ----
write.csv(df_pivot, 'out/df_nicotine_pivot.csv', quote = F, row.names = F)


#ggplot2 ----
#https://ggplot2.tidyverse.org/reference/index.html

#plot the data: 
ggplot(data=df_pivot, aes(x=concentration, y=latency, fill=prenatal_exposure)) + 
  geom_boxplot(position = position_dodge(width = 1), outlier.shape = NA) +
  geom_point(position = position_dodge(width = 1), alpha=0.5) +
  scale_fill_brewer(palette = 'Dark2') +
  scale_y_log10() +
  facet_wrap(~sex)


#save figure
ggsave('figures/fig1.pdf', width = 10, height = 4)
       



