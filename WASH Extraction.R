library(tidyverse)
library(readxl)
library(vroom)

## setting the directory 
setwd("D:/R workshop/WASH Datasets/Data")


## extracting required variables from different levels 
lvl01 = vroom_fwf(file = "R76120L01.TXT",
                  fwf_cols(fsu=c(4,8),
                           sss=c(30,30),
                           hhno=c(31,32),
                           sctr=c(15,15),
                           state=c(16,17),
                           dist=c(19,20),
                           nsc=c(127,129),
                           mlt=c(130,139)
                           ))

lvl02 = vroom_fwf(file = "R76120L02.TXT",
                  fwf_cols(fsu=c(4,8),
                           sss=c(30,30),
                           hhno=c(31,32),
                           psn=c(38,39),
                           gender=c(41,41),
                           edu=c(46,47),
                           usage=c(54,54),
                           type=c(55,56),
                           rsn=c(58,58)
                           ))

lvl03 = vroom_fwf(file = "R76120L03.TXT",
                  fwf_cols(fsu=c(4,8),
                           sss=c(30,30),
                           hhno=c(31,32),
                           hhsize=c(40,41),
                           rlgn=c(42,42),
                           grp=c(43,43),
                           mce=c(86,95)
                           ))

lvl05 = vroom_fwf(file = "R76120L05.TXT",
                  fwf_cols(fsu=c(4,8),
                           sss=c(30,30),
                           hhno=c(31,32),
                           source=c(40,41),
                           cover=c(72,72),
                           use=c(94,94),
                           typ=c(95,96),
                           child=c(110,110)
                           ))

lvl06 = vroom_fwf(file = "R76120L06.TXT",
                  fwf_cols(fsu=c(4,8),
                           sss=c(30,30),
                           hhno=c(31,32),
                           drain=c(58,58),
                           dispose=c(59,59),
                           place=c(60,60),
                           stag=c(65,65),
                           faeces=c(66,66),
                           flies=c(69,69)
                           ))



## getting the name of the states from the excel in data frame format

setwd("D:/R workshop/WASH Datasets")
st.code= read_xlsx("state_code 76round.xlsx")

st.code= st.code %>% mutate(state = as.numeric(state))

## conversion of state from character to numeric
lvl01=lvl01 %>% mutate(state= as.numeric(state))

##joining st.code with lvl01
lvl01= lvl01 %>%  left_join(st.code, by = "state") 


joined = lvl01 %>% 
  #left_join(lvl02) %>% 
  left_join(lvl03) %>% 
  left_join(lvl05) %>% 
  right_join(lvl06)


hh.hd=lvl02 %>% filter(psn=="01") %>% 
  select(fsu, sss, hhno, psn, gender, edu)

joined = joined %>% left_join(hh.hd)

joined = joined %>% mutate(wt=mlt/100,
                           mce=as.numeric(mce))

joined = joined %>% mutate(sctr = case_when(sctr == 1 ~ "Rural",
                                            sctr == 2 ~ "Urban"))


#creating dummy variable for stagnant water
joined = joined %>% mutate(stag01=case_when(stag==1 ~ 1,
                                            stag==2 ~ 0,
                                            TRUE ~ NA_real_))

#creating dummy variable for human faeces
joined = joined %>% mutate(fecs01=case_when(faeces==1 ~ 1,
                                            faeces==2 ~ 0,
                                            TRUE ~ NA_real_))

#master variable
joined = joined %>% mutate(club=pmax(stag01, fecs01, na.rm = T))

unique(joined$fecs01)
unique(joined$club)
table(joined$stag)
table(joined$club)

library(statar)

# creating indwt - indv level weight
joined = joined %>% mutate(indwt=wt*hhsize)

# creating National level quintile on individual basis (per capita)
joined = joined %>% 
  mutate(quintile_nt=xtile(mce, n=5, wt=wt*hhsize))

# creating Sector Level quintile 
joined = joined %>% group_by(sctr) %>% 
  mutate(quintile_sctr = xtile(mce, n=5, wt=wt*hhsize)) %>%
  ungroup()

# creating State Level quintile 
joined = joined %>% group_by(stnm) %>% 
  mutate(quintile_stnm = xtile(mce, n=5, wt=wt*hhsize)) %>%
  ungroup()


# creating State-Sector Level quintile
joined = joined %>% group_by(stnm, sctr) %>% 
  mutate(quintile_stsc=xtile(mce, n=5, wt=wt*hhsize)) %>%
  ungroup()



joined = joined %>% mutate(rlgn.sh=case_when(rlgn==1 ~ 1, 
                                             rlgn==2 ~ 2,
                                             rlgn %in% 3:9 ~ 3))

table(joined$grp)

# creating a two digit code for- 1st digit representing religion and second for caste
joined = joined %>% mutate(rlgn.grp=rlgn.sh*10+grp)

table(joined$rlgn.grp)

# assigning categories to the codes to socio-religious group
joined = joined %>% mutate(srg=case_when(rlgn.grp %in% c(11, 21, 31) ~ "ST", 
                                         rlgn.grp %in% c(12, 22, 32) ~ "SC", 
                                         rlgn.grp == 13 ~ "H-OBC", 
                                         rlgn.grp == 23 ~ "M-OBC", 
                                         rlgn.grp == 33 ~ "O-OBC",
                                         rlgn.grp == 19 ~ "H-GEN", 
                                         rlgn.grp == 29 ~ "M-GEN", 
                                         rlgn.grp == 39 ~ "O-GEN"))

table(joined$srg)



## data analysis using survey mode

library(srvyr)
svy.joined = joined %>% as_survey_design(weights = indwt)

### Checking quintile created correctly or not 

# National Level
svy.joined %>% group_by(quintile_nt) %>% summarise(survey_prop())

# State Level
svy.joined %>% group_by(quintile_stnm) %>% summarise(survey_prop())

# Sector Level
svy.joined %>% group_by(quintile_sctr) %>% summarise(survey_prop())

# State-Sector Level
svy.joined %>% group_by(quintile_stsc) %>% summarise(survey_prop())

## National level quintile when use at more disaggregated level 
xx=svy.joined %>% group_by(stnm, sctr, quintile_nt) %>% 
  summarise(survey_prop())

### it is inappropriate to use national level quintile for state level analysis

## Checking State-Sector level quintile 
zz=svy.joined %>% group_by(stnm, sctr, quintile_stsc) %>% 
  summarise(survey_prop()) %>%
  ungroup()

## Overall Exposure to the problem stagnant water and faeces 
svy.joined %>% summarise(survey_mean(club, vartype = "ci"))
## 0.206 

## Exposure to the problem stagnant water and faeces by sector
svy.joined %>% group_by(sctr) %>% 
  summarise(prop_ind = survey_mean(club)) %>%
  ungroup()

## Rural    0.224     0.00219
## Urban    0.166     0.00278


table(joined$stnm)

library(Hmisc)    # used for %nin% function

## Exposure to the problem stagnant water and faeces by state & sector 
tab1 = svy.joined %>% 
  group_by(stnm, sctr) %>% 
  filter(stnm %nin% c("UTT", "SKM", "ARP", "NGL", "MNP","ANI","MIZ","TRP", "JMK", "POD", "LKD", "GOA", "HMP", "CHN","MEG", "DDU", "DNH")) %>%
  summarise(prop_ind = survey_mean(club)) %>%
  #arrange(desc(prop_ind)) %>%
  ungroup()

## Exposure to the problem stagnant water and faeces by state, sector and economic status
tab2 = svy.joined %>% group_by(stnm, sctr, quintile_stsc) %>% 
  summarise(prop_ind = survey_mean(club)) %>%
  ungroup()

## Exposure to the problem stagnant water and faeces by state
tab3 = svy.joined %>% group_by(stnm) %>% 
  summarise(prop_ind = survey_mean(club)) %>%
  arrange(desc(prop_ind)) %>%
  ungroup()

## checking the population share of each SRGs 
svy.joined %>% group_by(srg) %>% 
  summarise(prop_max = survey_prop()) %>% 
  ungroup()


## Exposure to the problem stagnant water and faeces by Socio-religious group
tab4 = svy.joined %>% filter(srg %nin% c("O-GEN", "O-OBC")) %>%
  ## dropping these categories bcoz of very low population share
       group_by(srg) %>% 
       summarise(prop_ind = survey_mean(club)) %>% 
       arrange(desc(prop_ind)) %>%
       ungroup()

## Exposure to the problem stagnant water and faeces by Socio-religious group and state
tab5 = svy.joined %>% filter(srg %nin% c("O-GEN", "O-OBC")) %>%
  ## dropping these categories bcoz of very low population share
  group_by(stnm, srg) %>% 
  summarise(prop_ind = survey_mean(club)) %>% 
  ungroup()

library(forcats)  # for re-ordering in ggplot

## graph based on sector-state
graph01 = ggplot(tab1, aes(x = fct_reorder(stnm, prop_ind, .desc = TRUE), y = prop_ind * 100, fill = as_factor(sctr))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.5) +  # Grouped bars
  labs(x = "State", 
       y = "Individual Proportions (%)",  # Y-axis label with percentage symbol
       fill = "Sectors") +  
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +  # Set color palette
  geom_errorbar(
    aes(ymin = (prop_ind - 1.96 * prop_ind_se) * 100, 
        ymax = (prop_ind + 1.96 * prop_ind_se) * 100), 
    position = position_dodge(width = 0.5),  # Same dodge position as the bars
    color = "black", 
    width = 0.25) + # Error bars for each group
  #geom_text(
  #aes(label = round(prop_ind * 100, 2), y = 0.01), # Position text at the bottom of each bar
  #position = position_dodge(width = 0.5),
  #vjust = -0.5, # Adjust vertical position 
  #size = 2, # Adjust text size
  #color = "black") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center and style the title
    axis.text.x = element_text(size = 10),  # Style x-axis labels
    axis.text.y = element_text(size = 12),  # Style y-axis labels
    axis.title.x = element_text(size = 14, face = "bold"),  # Style x-axis title
    axis.title.y = element_text(size = 14, face = "bold"),  # Style y-axis title
    legend.position = c(0.9, 0.9)  # Adjust legend position
  )+
  coord_flip()  # Add this line to flip axes

graph01 = graph01 + ggtitle("Figure 1: State-Sector Wise Exposure to Faeces & Stagnant Water")
graph01


## graph based on socio-religious group
graph02 = ggplot(tab4, aes(x = fct_reorder(srg, prop_ind, .desc = TRUE), y = prop_ind * 100)) +
  geom_bar(stat = "identity", #position = position_dodge(width = 0.5), 
           width = 0.7, fill = "purple", color = "black", alpha = 0.5) +  # Grouped bars
  labs(x = "Socio-Religious Groups", 
       y = "Individual Proportions (%)") +  # Y-axis label with percentage symbol
  theme_minimal() +
  #scale_fill_brewer(palette = "Set1") +  # Set color palette
  geom_errorbar(
    aes(ymin = (prop_ind - 1.96 * prop_ind_se) * 100, 
        ymax = (prop_ind + 1.96 * prop_ind_se) * 100), 
    #position = position_dodge(width = 0.5),  # Same dodge position as the bars
    color = "black", 
    width = 0.25) + # Error bars for each group
  geom_text(
    aes(label = round(prop_ind * 100, 2), y = 0.1), # Position text at the bottom of each bar
    #position = position_dodge(width = 0.5),
    vjust = -5, # Adjust vertical position 
    size = 4, # Adjust text size
    color = "black") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),  # Center and style the title
    axis.text.x = element_text(size = 10),  # Style x-axis labels
    axis.text.y = element_text(size = 12),  # Style y-axis labels
    axis.title.x = element_text(size = 10, face = "bold"),  # Style x-axis title
    axis.title.y = element_text(size = 10, face = "bold"),  # Style y-axis title
    #legend.position = c(0.10, 0.9)  # Adjust legend position
  )
graph02 = graph02 + ggtitle("Figure 2: Socio-Religious Disparities in Faecal & Stagnant Water Exposure")
graph02

## Exposure to the problem of stagnant water and faeces by national economic status
tab6 = svy.joined %>% group_by(quintile_nt) %>% 
  summarise(prop_ind = survey_mean(club)) %>%
  ungroup()

## Exposure to the problem of stagnant water and faeces by sector wise economic status
tab7 = svy.joined %>% group_by(sctr, quintile_sctr) %>% 
  summarise(prop_ind = survey_mean(club)) %>%
  ungroup()

## graph based on national quintiles
graph03 = ggplot(tab6, aes(x = quintile_nt, y = prop_ind * 100)) +
  geom_bar(stat = "identity", #position = position_dodge(width = 0.5), 
           width = 0.5, fill = "darkgreen", color = "black", alpha = 0.5) +  # Grouped bars
  labs(title = "Exposure to Faeces based on National Economic Status", 
       x = "Quintiles", 
       y = "Individual Proportions (%)") +  # Y-axis label with percentage symbol
  theme_minimal() +
  #scale_fill_brewer(palette = "Set1") +  # Set color palette
  geom_errorbar(
    aes(ymin = (prop_ind - 1.96 * prop_ind_se) * 100, 
        ymax = (prop_ind + 1.96 * prop_ind_se) * 100), 
    #position = position_dodge(width = 0.5),  # Same dodge position as the bars
    color = "black", 
    width = 0.25) + # Error bars for each group
  geom_text(
    aes(label = round(prop_ind * 100, 2), y = 0.01), # Position text at the bottom of each bar
    #position = position_dodge(width = 0.5),
    vjust = -5, # Adjust vertical position 
    size = 4, # Adjust text size
    color = "black") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center and style the title
    axis.text.x = element_text(size = 10),  # Style x-axis labels
    axis.text.y = element_text(size = 12),  # Style y-axis labels
    axis.title.x = element_text(size = 14, face = "bold"),  # Style x-axis title
    axis.title.y = element_text(size = 14, face = "bold"),  # Style y-axis title
    #legend.position = c(0.10, 0.9)  # Adjust legend position
  )

graph03

## graph based on sectoral quintiles
graph04 = ggplot(tab7, aes(x = sctr, y = prop_ind * 100, fill = as_factor(quintile_sctr))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7, alpha=0.5)+ # Grouped bars
  labs(x = "Sectors", 
       y = "Individual Proportions (%)",  # Y-axis label with percentage symbol
       fill = "Quintiles") +  
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +  # Set color palette
  geom_errorbar(
    aes(ymin = (prop_ind - 1.96 * prop_ind_se) * 100, 
        ymax = (prop_ind + 1.96 * prop_ind_se) * 100), 
    position = position_dodge(width = 0.7),  # Same dodge position as the bars
    color = "black", 
    width = 0.25) + # Error bars for each group
  geom_text(
  aes(label = round(prop_ind * 100, 2), y = 0.01), # Position text at the bottom of each bar
  position = position_dodge(width = 0.7),
  vjust = -8, # Adjust vertical position 
  size = 4, # Adjust text size
  color = "black") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center and style the title
    axis.text.x = element_text(size = 12),  # Style x-axis labels
    axis.text.y = element_text(size = 12),  # Style y-axis labels
    axis.title.x = element_text(size = 14, face = "bold"),  # Style x-axis title
    axis.title.y = element_text(size = 14, face = "bold"),  # Style y-axis title
    legend.position = c(0.85, 0.9)  # Adjust legend position
  )
graph04 = graph04 + ggtitle("Figure 3: Sectoral Economic Disparities in Faecal & Stagnant Water Exposure")
graph04

## Saving graphs in the directory

tiff("sctr_state.tif", width = 12, height = 5, units = "in", res = 600)
print(graph01)
dev.off()

tiff("srg.tif", width = 10, height = 5, units = "in", res = 600)
print(graph02)
dev.off()

tiff("stratums.tif", width = 15, height = 5, units = "in", res = 600)
print(graph04)
dev.off()


### End of Code



