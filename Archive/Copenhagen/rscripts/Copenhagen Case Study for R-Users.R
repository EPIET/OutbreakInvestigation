#Outbreak module 2022
#Part 1

#Load packages----
install.packages("pacman")
library(pacman)

pacman::p_load(
  devtools,
  tidyverse,
  here,
  lubridate,
  skimr,
  here,
  rio,
  janitor,
  lubridate, 
  epiR, 
  apyramid,
  gtsummary,
  EpiStats,
  fextable
)

here()

#Load dataset----
cph <- import(here("data","Copenhagen_raw.csv"))
dim(cph)
str(cph)
skim(cph)
glimpse(cph)

names(cph)

#Cleaning data----
class(cph)
class(cph$age)

table(cph$age)
table(cph$age[cph$group == 0])
table(cph$age[cph$group == 1])

cph %>% 
  filter(group == 1) %>% 
  select(age) %>% 
  table()

cph %>% 
  tabyl(starthour)

cph %>% 
  tabyl(meal, tuna, show_na=T)

cph %>% 
  filter((diarrhoea!=1 | is.na(diarrhoea)) & (bloody!=1 | is.na(bloody)) & (vomiting!=1 | is.na(vomiting))) %>% 
  select(dayonset) %>% 
  table()

cph <- cph %>% 
  mutate(sex = ifelse(sex == "male", 1, 0) %>% as.double()) %>% 
  mutate(age=ifelse(age==8 | age==180, 18, age)) %>% 
  mutate(age=ifelse(age==16 & group==0, 61, age))

summary(cph$age[cph$group==0])

cph <- cph %>% 
  mutate(dayonset=ifelse(
                (diarrhoea == 0 | is.na(diarrhoea)) & 
                (vomiting == 0 | is.na(vomiting)) & 
                  (bloody == 0 | is.na(bloody)), NA_character_, dayonset))  
 

tabyl(cph, dayonset)

cph <- cph %>% 
  mutate(dayonset=na_if(cph$dayonset, "")) #to transform the empty values to NAs

cph <- cph %>% 
  mutate(dayonset=dmy(dayonset)) 

  class(cph$dayonset)

cph <- cph %>% 
    mutate(diarrhoea=as.logical(diarrhoea))

tabyl(cph$diarrhoea) 

 cph <- cph %>% 
   mutate(across(.cols = c("diarrhoea", "bloody", "vomiting", "meal"),
                  .fns=as.logical))

 class(cph$meal)          

 #case definition
 cph <- cph %>% 
   mutate(case = 
            case_when(
              if_all(c(diarrhoea, vomiting, bloody), ~ . == FALSE) ~ FALSE, # has no symptoms
              if_any(c(diarrhoea, vomiting, bloody), ~ . == TRUE) ~ TRUE, # has any symptoms
              dayonset == "2006-11-11" & starthour < 18 ~ FALSE, # onset before
              dayonset == "2006-11-13" & starthour > 18 ~ FALSE, # onset after
              meal  == FALSE ~ NA # not exposed to any food 
            )
   )
tabyl(cph, case)

cph %>% 
  filter(if_all(c(diarrhoea, vomiting, bloody), 
                ~ is.na(.))) %>% 
  count()

cph %>% 
  filter(if_all(c(diarrhoea, vomiting, bloody), 
                ~ (is.na(.) & meal == TRUE))
  ) %>% 
  count()

cph <- cph %>% 
  mutate(
    case = ifelse(if_all(c(diarrhoea, vomiting, bloody), ~ is.na(.)) & meal == TRUE,
                  FALSE, 
                  case))
cph %>% 
  filter(if_all(.cols =c(vomiting, diarrhoea, bloody), .fns = ~ . %in% c(FALSE, NA) & case == TRUE)) %>% 
  count()

#Part 2-----
cph <- import(here("data", "Copenhagen_analysis_clean.rds"))

#Descriptive analysis-----
tabyl(cph, case)
tabyl(cph, case, sex)

cph %>% tabyl(diarrhoea, case) %>%
  # add additional info:
  # row and col totals      
  adorn_totals(where = "both") %>% 
  # turn into percentages      
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 1) %>% 
  # add counts, again
  adorn_ns(position = "front") %>% 
  # change titles
  adorn_title(col_name = "case", row_name = "diarrhoea") %>% 
  flextable::qflextable()

summary(cph$age)
skim(cph$age)
ggplot(data=cph,
       mapping=aes(x=age))+
  geom_histogram()+
  theme_minimal()

cph <- cph %>% 
  mutate(age_cat = cut(age, breaks = c(-Inf, 10, 16, 20, 50, Inf),
                       labels = c("0-10", "11-16", "17-20", "21-50",">50"),
                       right = TRUE,
                       ordered_result = TRUE)
  )

#age pyramid
apyramid::age_pyramid(cph, 
                      age_group = age_cat,
                      split_by = sex) +
  # to adapt the scale of the plot to better fit our data:
  scale_y_continuous(limits = c(-150,150),
                     breaks = seq(from = -150, to = 150, by = 25),
                     labels = as.character(abs(seq(from = -150, to = 150, by = 25))))

#epicurve
cph %>% 
  filter(case == TRUE) %>% 
  mutate(onset_day = factor(onset_day)) %>% 
  ggplot(mapping=aes(x = onset_day)) + 
  geom_bar() +
  # adapt scale for better readability
  scale_x_discrete(guide = guide_axis(angle = 90)) + 
  # apply a leaner esthetic to the graph
  theme_minimal()

any(!is.na(cph$onset_day) & is.na(cph$onset_hour))

# create new variable 'onset_dayhour'
cph <- cph %>% 
  mutate(onset_dayhour = onset_day + hours(onset_hour))

cph %>% 
  filter(case == TRUE) %>% 
  mutate(onset_dayhour = factor(onset_dayhour)) %>% 
  ggplot(mapping=aes(x = onset_dayhour)) + 
  geom_bar() +
  # adapt scale for better readability
  scale_x_discrete(guide = guide_axis(angle = 90)) + 
  # apply a leaner esthetic to the graph
  theme_minimal()

cph %>% 
  filter(case == TRUE) %>% 
  ggplot(aes(x = factor(onset_dayhour), fill = factor(group))) + 
  geom_bar() +
  facet_wrap(~sex) +
  # adapt scale for better readability
  scale_x_discrete(guide = guide_axis(angle = 90)) + 
  # change titles and labels
  labs(fill = "group",
       x = "day and hour of onset",
       y = "number of cases",
       title = "Epi-Curve of the outbreak, divided by sex",
       subtitle = "Copenhagen, November 2006, N(total) = 215 cases")

#Incubation period

cph <- cph %>% 
  mutate(
    # Create variable storing the time of the dinner (equal for all participants)    
    dinner_dayhour = dmy_h("11.11.06 - 18"),
    # in case you haven´t done it above:
    onset_dayhour = onset_day + hours(onset_hour),
    # calculate incubation period
    incubation = onset_dayhour - dinner_dayhour
  )

cph %>% 
  filter(!is.na(incubation)) %>% 
ggplot()+
  geom_histogram(mapping =aes(x = incubation), binwidth = 6) +
  # adapt scale to better fit data
  scale_x_continuous(breaks = seq(from=0, to=48, by=6)) + 
  labs(x = "incubation period in 6-hour bins",
       y = "number of cases")

#Attack rates
tabyl(cph, case) %>% 
  adorn_totals(where = "row") %>% 
  adorn_pct_formatting(digits = 1)

tabyl(cph, group, case) %>% 
  adorn_totals(where = "col") %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 1) %>% 
  adorn_ns(position = "front") %>% 
  adorn_title(col_name = "case")

cph %>% 
  select(case, group, class, sex, age_cat, meal, pasta, pastaD, veal) %>% 
  tbl_summary(by=case) %>% 
  add_p()

#Univariable analysis----
cph <- cph %>% 
  mutate(case=factor(cph$case, levels = c("TRUE", "FALSE")))
         
cph <- cph %>% 
  mutate(across(.cols = c("case", "pasta", "veal", "champagne", "sauce", "shrimps"),
                .fns = ~factor(., levels = c("TRUE", "FALSE")))
  )

cph %>% 
  select (shrimps, case) %>%
  mutate(across(.cols = c(shrimps, case),
                .fns = ~factor(., levels=(c("TRUE", "FALSE")))
  )) %>% table() %>% 
  epi.2by2(method = "cohort.count")

#cstable 
Table_1 <- EpiStats::cstable(as.data.frame(cph), 
                             cases = "case", 
                             exposure = "shrimps")
Table_1

cph %>% 
  select(pasta, case) %>% 
  table() %>% 
  epi.2by2(method = "cohort.count")
         
Table_2 <- EpiStats::cstable(as.data.frame(cph), 
                             cases = "case", 
                             exposure = "pasta")
Table_2    

#Tests
chisq.test(cph$sex, cph$case)
wilcox.test(cph$class ~ cph$case)
wilcox.test(cph$age ~ cph$sex)
shapiro.test(cph$age)
t.test(cph$age ~ cph$sex)

#dose response
wilcox.test(cph$pastaD~cph$case)


#Stratified analysis

mh <- cph %>%
  select(veal, case, pasta) %>% 
  table() %>% 
  epi.2by2(method = "cohort.count")

mh
names(mh)
names(mh$massoc.detail)
mh$massoc.detail$RR.crude.wald 
mh$massoc.detail$RR.strata.wald
mh$massoc.detail$RR.mh.wald
diff <-round(((mh$massoc.detail$RR.crude.wald-mh$massoc.detail$RR.mh.wald)/mh$massoc.detail$RR.crude.wald)*100, digits=0)


results <- bind_rows(mh$massoc.detail$RR.crude.wald, 
                     mh$massoc.detail$RR.strata.wald, 
                     mh$massoc.detail$RR.mh.wald, 
                     diff)

rownames(results) <- c("Crude", "Stratum 1", "Stratum 0", "Adjusted", "Difference")

results


