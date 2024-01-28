## ----error = F, message = F, warning = F----------------------------------------

rm(list=ls())
options(digits = 2)

## Install "pacman" package if not installed
# (remove the # symbol from the line below):
# install.packages("pacman")

## Load R packages:
pacman::p_load(data.table, tidyverse, haven, labelled, vtable, 
               psych, scales, weights, clipr, forcats,
               stargazer, ggthemes, ggcharts, geomtextpath,
               corrplot, tm, gt, lme4, car, lmerTest, 
               ggeffects, magrittr, broom, broom.mixed)

## Import BCL dataset:
ds <- read_sav("~/Desktop/oxford/data/BCL/BCL Dataset.sav")
ds <- as.data.table(ds)
## Remove cases with no country:
ds <- ds[!is.na(ds$Country),]

ds$Country <- as.character(haven::as_factor(ds$Country))

ds <- ds[!(ds$Country == "USA"), ]
table(ds$Country)

ds$CountryID <- ifelse(ds$Country == "Bangladesh", "01",
                ifelse(ds$Country == "Ghana", "02",
                ifelse(ds$Country == "Malawi", "03",
                ifelse(ds$Country == "Pakistan", "04",
                ifelse(ds$Country == "Sierra Leone", "05",
                ifelse(ds$Country == "Tanzania", "06",
                ifelse(ds$Country == "Uganda", "07", NA)))))))



## ----error = F, message = F, warning = F----------------------------------------

tbl01 <- table(ds$Country, ds$UserLanguage)
tbl02 <- addmargins(tbl01, c(1, 2))

## Table of user language by country:
tbl02

## Sample size by country:

lp01 <- ds %>% 
  # drop_na(Country) %>%
  lollipop_chart(x = Country,
               line_color = "black",
               point_color = "black")+
  labs(y = "Frequency",
       x = "",
       title = "Sample size by country")+
  theme_bw()

lp01



## ----error = F, message = F, warning = F----------------------------------------

ds$seconds <- as.numeric(ds$Duration__in_seconds_)
ds$minutes <- (ds$seconds/60)
ds$hours <- (ds$minutes/60)

summary(ds$seconds)
summary(ds$minutes)
summary(ds$hours)

ds %>% 
#  drop_na(Country)%>%
ggplot(aes(x = hours))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 200)+
  geom_textvline(label = "Mean = 1.00", 
                 xintercept = mean(ds$hours), 
                 vjust = 1.3, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Hours", 
       y = "Frequency", 
       title = "Survey duration (full sample)")+
  theme_bw()


ds %>% 
  # drop_na(Country)%>%
ggplot(aes(x = hours))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 100)+
  labs(x = "Hours", 
       y = "Frequency", 
       title = "Survey duration by country")+
    facet_wrap(vars(Country), nrow = 2)+
  theme_bw()

ds %>% 
  # drop_na(Country)%>%
ggplot(aes(x = hours, 
           y = Country, 
           color = Country))+
  geom_point(show.legend = FALSE)+
  labs(x = "Hours",
       title = "Survey duration by country")+
  scale_color_colorblind()+
  theme_bw()




## ----error = F, message = F, warning = F----------------------------------------

ds$age <- as.numeric(ds$age)

summary(ds$age)

ds %>% 
  # drop_na(Country)%>%
ggplot(aes(x = age, 
           y = Country))+
  geom_boxplot(show.legend = FALSE)+
  labs(x = "Age",
       title = "Age distribution by country")+
  scale_color_colorblind()+
  theme_bw()

ds %>% 
  # drop_na(Country)%>%
ggplot(aes(x = age, 
           y = Country, 
           color = Country))+
  geom_point(show.legend = FALSE)+
  labs(x = "Age",
       title = "Age distribution by country")+
  scale_color_colorblind()+
  theme_bw()




## ----error = F, message = F, warning = F----------------------------------------

ds$gend1 <- as_factor(ds$gend)
ds$gend2 <- as_factor(ds$gend_6_TEXT)

ds$gend3 <- ifelse(ds$gend2=="Female", "Female",
            ifelse(ds$gend2=="IM FEMAIL", "Female",
            ifelse(ds$gend2=="Male", "Male",
            ifelse(ds$gend2=="male", "Male", ""))))

ds$gender <- ifelse(ds$gend1 == "Male", "Male",
             ifelse(ds$gend3 == "Male", "Male",
             ifelse(ds$gend1 == "Female", "Female",
             ifelse(ds$gend3 == "Female", "Female", NA))))

## Gender distribution (full sample):
lp02 <- ds %>% 
  drop_na(gender) %>%
lollipop_chart(x = gender,
               line_color = "black",
               point_color = "black")+
  labs(x = "Gender", 
       y = "Frequency",
       title = "Gender distribution (full sample)")+
  theme_bw()

lp02

## Gender distribution by country
barp01 <- ds %>% 
  drop_na(gender) %>%
  ggplot(aes(x = gender, 
             fill = gender))+
  geom_bar()+
  labs(x = "",
       y = "Frequency",
       title = "Gender distribution by country", 
       fill = "")+
  scale_fill_manual(values=c("green", "purple"))+
  facet_wrap(~Country, ncol = 1)+
  coord_flip()+
  theme_bw()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

barp01



## ----error = F, message = F, warning = F----------------------------------------

ds$wealth_level <- as.numeric(ds$wealth_4)
summary(ds$wealth_level)

ds %>% 
  drop_na(wealth_level, gender) %>%
  ggplot(aes(y = wealth_level, 
             x = Country))+
  geom_boxplot(fill = "white")+
  facet_wrap(~gender, 
             ncol = 2)+
  labs(y = "Self-reported relative wealth status", 
       x = "")+
  coord_flip()+
  theme_bw()




## ----error = F, message = F, warning = F----------------------------------------

head(table(ds$income), n = 15)



## ----error = F, message = F, warning = F----------------------------------------

head(table(ds$occupation), n = 15)



## ----error = F, message = F, warning = F----------------------------------------

ds$religion2 <- as_factor(ds$religion)
ds$religion2 <- as.character(ds$religion2)

ds$religion2 <- str_replace_all(ds$religion2, " - ", ": ")

ds$religion2 <- ifelse(ds$religion2 == "Other (please specify)", "Other", 
                       ds$religion2)

## Religion distribution:

lp06 <- ds %>% 
  drop_na(religion2) %>%
lollipop_chart(x = religion2,
               line_color = "black",
               point_color = "black")+
  labs(x = "",
       y = "Frequency",
       title = "Religion distribution (full sample)")+
  theme_bw()

lp06


## Religion distribution by country:

tbl04 <- table(ds$religion2, ds$Country)
tbl05 <- addmargins(tbl04, c(1, 2))

## Table of user language by country:
tbl05


ds$religion3 <- ifelse(ds$religion2=="Other (please specify)", "Other",
                ifelse(ds$religion2=="Atheist", "Atheist/Agnostic/None",
                ifelse(ds$religion2=="Agnostic", "Atheist/Agnostic/None",
                ifelse(ds$religion2=="Spiritual not Religious", "Atheist/Agnostic/None",
                ifelse(ds$religion2=="None", "Atheist/Agnostic/None",
                ifelse(ds$religion2=="Sikh", "Other",
                ifelse(ds$religion2=="Jewish", "Other",
                ifelse(ds$religion2=="Buddhist", "Other",
                       ds$religion2))))))))

## Religion distribution by country:

lp07 <- ds %>% 
  drop_na(religion3) %>%
  group_by(religion3, Country) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(religion3, count)) + 
  geom_segment(aes(x=religion3, xend=religion3, 
                   y=0, yend=count))+ 
  geom_point()+
  labs(x = "", 
       y = "Frequency", 
       title = "Religion distribution by country")+
    facet_wrap(vars(Country), nrow = 2)+
  coord_flip()+
  theme_bw()

lp07



## ----error = F, message = F, warning = F----------------------------------------

ds$marriage01 <- as.character(haven::as_factor(ds$marriage))

table(ds$marriage01)

ds$married <- ifelse(ds$marriage01 == "Single", "Unmarried",
              ifelse(ds$marriage01 == "Married", "Married", "Other"))

bp01 <- ds %>% drop_na(married) %>%
  ggplot(aes(x = fct_rev(fct_infreq(married))))+
  geom_bar(fill = "black")+
  labs(x = "",
       y = "Frequency",
       title = "Marital status")+
  coord_flip()+
  theme_bw()

# bp01

## Marital status (full sample):

lp04 <- ds %>% 
  drop_na(married) %>%
  group_by(married) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(married, count)) + 
  geom_segment(aes(x=married, xend=married, 
                   y=0, yend=count))+ 
  geom_point()+
  labs(x = "", 
       y = "Frequency", 
       title = "Marital status (full sample)")+
  coord_flip()+
  theme_bw()

lp04

## Marital status by country:

lp05 <- ds %>% 
  drop_na(married) %>%
  group_by(married, Country) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(married, count)) + 
  geom_segment(aes(x=married, xend=married, 
                   y=0, yend=count))+ 
  geom_point()+
  labs(x = "", 
       y = "Frequency", 
       title = "Marital status by country")+
    facet_wrap(vars(Country), nrow = 2)+
  coord_flip()+
  theme_bw()

lp05



## ----error = F, message = F, warning = F----------------------------------------

ds$children <- as.numeric(gsub("\\D", "", ds$children))

summary(ds$children)

ds %>% drop_na(children)%>%
ggplot(aes(x = children))+
  geom_bar(color = "black",
                 fill = "gray",
                 width = 0.75)+
  geom_textvline(label = "Mean = 0.7", 
                 xintercept = 0.7, 
                 vjust = 1.3, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Number of children", 
       y = "Frequency", 
       title = "Number of children: Full sample")+
  theme_bw()



ds %>% drop_na(children)%>%
ggplot(aes(x = children))+
  geom_histogram(color = "black",
                 fill = "gray", 
                 bins = 10)+
  labs(x = "Number of children", 
       y = "Frequency", 
       title = "Number of children by country")+
    facet_wrap(vars(Country), nrow = 2)+
#  coord_flip()+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

head(table(ds$ethnic), n = 15)



## ----error = F, message = F, warning = F----------------------------------------

ds$edu1 <- as_factor(ds$Education)

ds$edu2 <- ifelse(ds$edu1 == "No schooling completed", "No schooling",
           ifelse(ds$edu1 == "Nursery school to 8th grade", "8th grade or less",
           ifelse(ds$edu1 == "Some high school, no diploma", "Some high school",
           ifelse(ds$edu1 == "High school graduate, diploma or the equivalent (for example: GED)", 
                  "High school or equivalent",
           ifelse(ds$edu1 == "Some college credit, no degree", "Some college",
           ifelse(ds$edu1 == "Trade/technical/vocational training", "Vocational school",
           ifelse(ds$edu1 == "Associate degree", "Associate's",
           ifelse(ds$edu1 == "Bachelor’s degree", "Bachelor's",
           ifelse(ds$edu1 == "Master’s degree", "Master's",
           ifelse(ds$edu1 == "Professional degree", "Professional",
           ifelse(ds$edu1 == "Doctorate degree", "Doctorate", NA)))))))))))

ds$edu2 <- factor(ds$edu2, 
                  levels = c("No schooling", "8th grade or less", 
                             "Some high school", "High school or equivalent",
                             "Some college", "Vocational school",
                             "Associate's", "Bachelor's", "Master's",
                             "Professional", "Doctorate"))

# table(ds$edu2)

## Education distribution (full sample):

lp04 <- ds %>% 
  drop_na(edu2) %>%
  group_by(edu2) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(edu2, count)) + 
  geom_segment(aes(x=edu2, xend=edu2, 
                   y=0, yend=count))+ 
  geom_point()+
  labs(x = "", 
       y = "Frequency", 
       title = "Education distribution (full sample)")+
  coord_flip()+
  theme_bw()

lp04

## Education distribution by country:

lp05 <- ds %>% 
  drop_na(edu2) %>%
  group_by(edu2, Country) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(edu2, count)) + 
  geom_segment(aes(x=edu2, xend=edu2, 
                   y=0, yend=count))+ 
  geom_point()+
  labs(x = "", 
       y = "Frequency", 
       title = "Education distribution by country")+
    facet_wrap(vars(Country), nrow = 2)+
  coord_flip()+
  theme_bw()

lp05



## ----error = F, message = F, warning = F----------------------------------------

## Four ingroup fusion items:

# I have a deep emotional bond with the [ingroup].
ds$IGF01 <- as.numeric(ds$Fus.IG1)

# I am strong because of the [ingroup].
ds$IGF02 <- as.numeric(ds$Fus.IG2)

# I make the [ingroup] strong.	
ds$IGF03 <- as.numeric(ds$Fus.IG3)

# I am one with the [ingroup]
ds$IGF04 <- as.numeric(ds$Fus.IG4)

## Four outgroup fusion items:

# I have a deep emotional bond with the [outgroup].
ds$OGF01 <- as.numeric(ds$FUS.OG1)

# I am strong because of the [outgroup].
ds$OGF02 <- as.numeric(ds$FUS.OG2)

# I make the [outgroup] strong.	
ds$OGF03 <- as.numeric(ds$FUS.OG3)

# I am one with the [outgroup].
ds$OGF04 <- as.numeric(ds$FUS.OG4)


## Four ingroup identification items:

# I identify with the [ingroup].
ds$IGI01 <- as.numeric(ds$IDT.IG1)

# I have a lot in common with the [ingroup].
ds$IGI02 <- as.numeric(ds$IDT.IG2)

# I connect with the values of the [ingroup].
ds$IGI03 <- as.numeric(ds$IDT.IG3)

# I feel a sense of belonging with the [ingroup].
ds$IGI04 <- as.numeric(ds$IDT.IG4)


## Four outgroup identification items:

# I identify with the [outgroup].	
ds$OGI01 <- as.numeric(ds$IDNT.OG1)

# I have a lot in common with the [outgroup].	
ds$OGI02 <- as.numeric(ds$IDNT.OG2)

# I connect with the values of the [outgroup].	
ds$OGI03 <- as.numeric(ds$IDNT.OG3)

# I feel a sense of belonging with the [outgroup].	
ds$OGI04 <- as.numeric(ds$IDNT.OG4)



## ----error=F, message=F, warning=F----------------------------------------------

## Bonds dataframe:
bonds <- cbind.data.frame(ds$IGF01, ds$IGF02, ds$IGF03, ds$IGF04,
                          ds$IGI01, ds$IGI02, ds$IGI03, ds$IGI04,
                          ds$OGF01, ds$OGF02, ds$OGF03, ds$OGF04,
                          ds$OGI01, ds$OGI02, ds$OGI03, ds$OGI04)

names(bonds) <- sub('ds\\$', '', names(bonds))

bonds <- na.omit(bonds)
mtx1 <- cor(bonds[, c(1:16)])



## ----error = F, message = F, warning = F----------------------------------------

corrplot(mtx1, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))+
    facet_wrap(vars(Country), nrow = 2)



## ----warning=F, message=F-------------------------------------------------------

KMO(r=cor(bonds))



## ----warning=F, message=F-------------------------------------------------------

cortest.bartlett(bonds)



## ----warning=F, message=F-------------------------------------------------------
# # parallel <- fa.parallel(bonds)



## ----warning=F, message=F-------------------------------------------------------
fit02 <- factanal(bonds, 2, rotation="promax")
fit02



## ----warning=F, message=F-------------------------------------------------------

fit03 <- factanal(bonds, 3, rotation="promax")
fit03



## ----warning=F, message=F-------------------------------------------------------

fit04 <- factanal(bonds, 4, rotation="promax")
fit04


## ----warning=F, message=F-------------------------------------------------------

## Remove 4th item from all subscales and retry:

## New dataframe:
bonds <- cbind.data.frame(ds$IGF01, ds$IGF02, ds$IGF03,
                          ds$IGI01, ds$IGI02, ds$IGI03,
                          ds$OGF01, ds$OGF02, ds$OGF03,
                          ds$OGI01, ds$OGI02, ds$OGI03)

names(bonds) <- sub('ds\\$', '', names(bonds))

bonds <- na.omit(bonds)
mtx1 <- cor(bonds[, c(1:12)])

corrplot(mtx1, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))

# # parallel <- fa.parallel(bonds)



## ----error = F, message = F, warning = F----------------------------------------

# Two factor model
fit04 <- factanal(bonds, 2, rotation="promax")
fit04



## ----error = F, message = F, warning = F----------------------------------------

# Three factor model:
fit05 <- factanal(bonds, 3, rotation="promax")
fit05



## ----error = F, message = F, warning = F----------------------------------------

# Four factor model:
fit06 <- factanal(bonds, 4, rotation="promax")
fit06



## ----error = F, message = F, warning=F------------------------------------------

igfusion <- cbind.data.frame(ds$IGF01, ds$IGF02, ds$IGF03)

names(igfusion) <- sub('ds\\$', '', names(igfusion))

igfusion <- na.omit(igfusion)
mtx2 <- cor(igfusion[, c(1:3)])

corrplot(mtx2, method = "number", number.cex = 0.7,
         col=c("darkred", "red",
               "darkgrey", "blue", "darkblue"))

## Reliability:
alpha02 <- psych::alpha(igfusion)
alpha02



## ----warning=F, message=F-------------------------------------------------------

igidentification <- cbind.data.frame(ds$IGI01, ds$IGI02, ds$IGI03)

names(igidentification) <- sub('ds\\$', '', names(igidentification))

igidentification <- na.omit(igidentification)
mtx3 <- cor(igidentification[, c(1:3)])

corrplot(mtx3, method = "number", number.cex = 0.7,
         col=c("darkred", "red",
               "darkgrey", "blue", "darkblue"))

## Reliability:
alpha03 <- psych::alpha(igidentification)
alpha03



## ----warning=F, message=F-------------------------------------------------------

ogbonds <- cbind.data.frame(ds$OGF01, ds$OGF02, ds$OGF03,
                            ds$OGI01, ds$OGI02, ds$OGI03)

names(ogbonds) <- sub('ds\\$', '', names(ogbonds))

ogbonds <- na.omit(ogbonds)
mtx3 <- cor(ogbonds[, c(1:6)])

corrplot(mtx3, method = "number", number.cex = 0.7,
         col=c("darkred", "red",
               "darkgrey", "blue", "darkblue"))

## Reliability:
alpha04 <- psych::alpha(ogbonds)
alpha04



## ----warning=F, message=F-------------------------------------------------------

ogfusion <- cbind.data.frame(ds$OGF01, ds$OGF02, ds$OGF03)

names(ogfusion) <- sub('ds\\$', '', names(ogfusion))

ogfusion <- na.omit(ogfusion)
mtx3 <- cor(ogfusion[, c(1:3)])

corrplot(mtx3, method = "number", number.cex = 0.7,
         col=c("darkred", "red",
               "darkgrey", "blue", "darkblue"))

## Reliability:
alpha04 <- psych::alpha(ogfusion)
alpha04



## ----warning=F, message=F-------------------------------------------------------

ogidentification <- cbind.data.frame(ds$OGI01, ds$OGI02, ds$OGI03)

names(ogidentification) <- sub('ds\\$', '', names(ogidentification))

ogidentification <- na.omit(ogidentification)
mtx3 <- cor(ogidentification[, c(1:3)])

corrplot(mtx3, method = "number", number.cex = 0.7,
         col=c("darkred", "red",
               "darkgrey", "blue", "darkblue"))

## Reliability:
alpha04 <- psych::alpha(ogidentification)
alpha04



## ----error = F, message = F, warning=F------------------------------------------

## BCL and BBL items:

# BCL_01:
# Seek out opportunities to bridge social divisions with their opponents, enemies, opposition groups, or other outgroups.   
# Variables: endorse_style_1	and exp_style_1

# BCL_02:
# Demonstrate willingness to compromise with their opponents, enemies, opposition groups, or other outgroups. 
# Variables: endorse_style_2	and exp_style_2

# BCL_03:
# Try to understand and empathize with their opponents, enemies, opposition groups, or other outgroups.  
# Variables: endorse_style_3	and exp_style_3

# BBL_01:
# Represent the interests of the communities and groups that they belong to even at the cost of other groups.
# Variables: endorse_style_4	and exp_style_4

# BBL_02:

## OLD QUESTION (individual datasets):
# Focus on building stronger connections within the communities and groups they belong to rather than building stronger relationships with other groups across boundaries.

## NEW QUESTION (this dataset):
# Seek out opportunities to build stronger connections with the communities and groups they belong to.

## Are the above same?? I'm assuming they are the same item. 

# Variables: endorse_style_5	and exp_style_5

# BBL_03:
# Try to gain benefits for the communities and groups they belong to even at the expense of other groups.
# Variables: endorse_style_6	and exp_style_6

ds$ENDBCL01 <- as.numeric(ds$endorse_style_1)
ds$ENDBCL02 <- as.numeric(ds$endorse_style_2)
ds$ENDBCL03 <- as.numeric(ds$endorse_style_3)
ds$ENDBBL01 <- as.numeric(ds$endorse_style_4)
ds$ENDBBL02 <- as.numeric(ds$endorse_style_5)
ds$ENDBBL03 <- as.numeric(ds$endorse_style_6)

ds$EXPBCL01 <- as.numeric(ds$exp_style_1)
ds$EXPBCL02 <- as.numeric(ds$exp_style_2)
ds$EXPBCL03 <- as.numeric(ds$exp_style_3)
ds$EXPBBL01 <- as.numeric(ds$exp_style_4)
ds$EXPBBL02 <- as.numeric(ds$exp_style_5)
ds$EXPBBL03 <- as.numeric(ds$exp_style_6)

leadership <- cbind.data.frame(ds$ENDBCL01, ds$ENDBCL02, ds$ENDBCL03, 
                               ds$ENDBBL01, ds$ENDBBL02, ds$ENDBBL03,
                               ds$EXPBCL01, ds$EXPBCL02, ds$EXPBCL03,
                               ds$EXPBBL01, ds$EXPBBL02, ds$EXPBBL03)

names(leadership) <- sub('ds\\$', '', names(leadership))

leadership <- na.omit(leadership)
mtx1 <- cor(leadership[, c(1:12)])



## ----error = F, message = F, warning = F----------------------------------------

corrplot(mtx1, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))


## ----error = F, message = F, warning = F----------------------------------------

## Kaiser-Meyer-Olkin (KMO) test of factorability
KMO(r=cor(leadership))

## Bartlett's test of sphericity
cortest.bartlett(leadership)



## ----error = F, message = F, warning = F----------------------------------------

# # parallel <- fa.parallel(leadership)



## ----error = F, warning = F, message = F----------------------------------------

fit03 <- factanal(leadership, 3, rotation="promax")
fit03



## ----error = F, warning = F, message = F----------------------------------------

fit04 <- factanal(leadership, 4, rotation="promax")
fit04



## ----error = F, message = F, warning=F------------------------------------------

end_bcl <- cbind.data.frame(ds$ENDBCL01, ds$ENDBCL02, ds$ENDBCL03)
end_bbl <- cbind.data.frame(ds$ENDBBL01, ds$ENDBBL02, ds$ENDBBL03)
exp_bcl <- cbind.data.frame(ds$EXPBCL01, ds$EXPBCL02, ds$EXPBCL03)
exp_bbl <- cbind.data.frame(ds$EXPBBL01, ds$EXPBBL02, ds$EXPBBL03)

names(end_bcl) <- sub('ds\\$', '', names(end_bcl))
names(end_bbl) <- sub('ds\\$', '', names(end_bbl))
names(exp_bcl) <- sub('ds\\$', '', names(exp_bcl))
names(exp_bbl) <- sub('ds\\$', '', names(exp_bbl))

end_bcl <- na.omit(end_bcl)
end_bbl <- na.omit(end_bbl)
exp_bcl <- na.omit(exp_bcl)
exp_bbl <- na.omit(exp_bbl)



## ----error = F, message = F, warning=F------------------------------------------

mtx1 <- cor(end_bcl[, c(1:3)])

corrplot(mtx1, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))

alph01 <- psych::alpha(end_bcl)
alph01



## ----error = F, message = F, warning=F------------------------------------------

mtx2 <- cor(end_bbl[, c(1:3)])

corrplot(mtx2, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))

alph02 <- psych::alpha(end_bbl)
alph02



## ----error = F, message = F, warning=F------------------------------------------

mtx3 <- cor(exp_bcl[, c(1:3)])

corrplot(mtx3, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))

alph03 <- psych::alpha(exp_bcl)
alph03



## ----error = F, message = F, warning=F------------------------------------------

mtx4 <- cor(exp_bbl[, c(1:3)])

corrplot(mtx4, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))

alph04 <- psych::alpha(exp_bbl)
alph04



## ----error = F, message = F, warning = F----------------------------------------

## Proper (usable) variables in the model:

ds$Age <- as.numeric(ds$age)

ds$Female <- ifelse(ds$gender == "Female", 1, 0)
ds$Married <- ifelse(ds$married == "Married", 1, 0)
ds$Wealth_level <- ds$wealth_level

ds$Endorse_BCL <- as.numeric((ds$ENDBCL01+ds$ENDBCL02+ds$ENDBCL03)/3)
ds$Endorse_BBL <- as.numeric((ds$ENDBBL01+ds$ENDBBL02+ds$ENDBBL03)/3)
ds$Experience_BCL <- as.numeric((ds$EXPBCL01+ds$EXPBCL02+ds$EXPBCL03)/3)
ds$Experience_BBL <- as.numeric((ds$EXPBBL01+ds$EXPBBL02+ds$EXPBBL03)/3)

ds$IG_Fusion <- as.numeric((ds$IGF01+ds$IGF02+ds$IGF03)/3)
ds$IG_Identification <- as.numeric((ds$IGI01+ds$IGI02+ds$IGI03)/3)
ds$OG_Bonds <- as.numeric((ds$OGF01+ds$OGF02+ds$OGF03+
                             ds$OGI01+ds$OGI02+ds$OGI03)/6)


summary(ds$Age)



## ----error = F, message = F, warning = F----------------------------------------

## Four regression models predicting endorsement and experience of BCL / BBL:

lm01 <- lm(Endorse_BCL~IG_Fusion+IG_Identification+OG_Bonds+Age+Female+Married+Wealth_level,
           data = ds)

lm02 <- lm(Experience_BCL~IG_Fusion+IG_Identification+OG_Bonds+Age+Female+Married+Wealth_level, 
           data = ds)

lm03 <- lm(Endorse_BBL~IG_Fusion+IG_Identification+OG_Bonds+Age+Female+Married+Wealth_level, 
           data = ds)

lm04 <- lm(Experience_BBL~IG_Fusion+IG_Identification+OG_Bonds+Age+Female+Married+Wealth_level, 
           data = ds)


## ----error = F, message = F, warning = F, results = "hide"----------------------

## Tabulated results:

stargazer(lm01, lm02,
          lm03, lm04, 
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html")



## ----results='asis'-------------------------------------------------------------
htmltools::includeHTML("table1.html")



## ----error = F, message = F, warning = F----------------------------------------

## Empathic concern

ds$empathic_concern_01 <- as.numeric(ds$empathy_1)

ds$empathic_concern_02a <- as.numeric(ds$empathy_2)
ds$empathic_concern_02 <- as.numeric(8 - ds$empathic_concern_02a)

ds$empathic_concern_03 <- as.numeric(ds$empathy_3)

ds$empathic_concern_04a <- as.numeric(ds$empathy_4)
ds$empathic_concern_04 <- as.numeric(8 - ds$empathic_concern_04a)

ds$empathic_concern_05a <- as.numeric(ds$empathy_5)
ds$empathic_concern_05 <- as.numeric(8 - ds$empathic_concern_05a)

ds$empathic_concern_06 <- as.numeric(ds$empathy_6)

ds$empathic_concern_07 <- as.numeric(ds$empathy_7)

ds$empathic_concern <- (ds$empathic_concern_01+ds$empathic_concern_02+
                        ds$empathic_concern_03+ds$empathic_concern_04+
                        ds$empathic_concern_05+ds$empathic_concern_06+
                        ds$empathic_concern_07)/7

summary(ds$empathic_concern)

ds %>% drop_na(empathic_concern)%>%
ggplot(aes(x = empathic_concern))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 30)+
  geom_textvline(label = "Mean = 4.00", 
                 xintercept = 4.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Empathic concern score", 
       y = "Frequency", 
       title = "Empathic concern (full sample)")+
  theme_bw()


ds %>% 
   drop_na(empathic_concern)%>%
ggplot(aes(x = empathic_concern))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 30)+
  labs(x = "Empathic concern score", 
       y = "Frequency", 
       title = "Empathic concern by country")+
    facet_wrap(vars(Country), nrow = 2)+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

ds$perspective_taking_01a <- as.numeric(ds$empathy_8)
ds$perspective_taking_01 <- (8 - ds$perspective_taking_01a)

ds$perspective_taking_02 <- as.numeric(ds$empathy_9)

ds$perspective_taking_03 <- as.numeric(ds$empathy_10)

ds$perspective_taking_04a <- as.numeric(ds$empathy_11)
ds$perspective_taking_04 <- (8 - ds$perspective_taking_04a)

ds$perspective_taking_05 <- as.numeric(ds$empathy_12)

ds$perspective_taking_06 <- as.numeric(ds$empathy_13)

ds$perspective_taking_07 <- as.numeric(ds$empathy_14)

ds$perspective_taking <- (ds$perspective_taking_01+ds$perspective_taking_02+
                          ds$perspective_taking_03+ds$perspective_taking_04+
                          ds$perspective_taking_05+ds$perspective_taking_06+
                          ds$perspective_taking_07)/7

summary(ds$perspective_taking)

ds %>% drop_na(perspective_taking)%>%
ggplot(aes(x = perspective_taking))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 30)+
  geom_textvline(label = "Mean = 4.00", 
                 xintercept = 4.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Perspective taking score", 
       y = "Frequency", 
       title = "Perspective taking (full sample)")+
  theme_bw()


ds %>% 
   drop_na(perspective_taking)%>%
ggplot(aes(x = perspective_taking))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 30)+
  labs(x = "Perspective taking score", 
       y = "Frequency", 
       title = "Perspective taking by country")+
    facet_wrap(vars(Country), nrow = 2)+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

ds3 <- cbind.data.frame(ds$empathic_concern, ds$perspective_taking)
ds3 <- na.omit(ds3)

names(ds3) <- sub('ds\\$', '', names(ds3))

mtx <- cor(ds3[, c(1:2)])

corrplot(mtx, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))



## ----error = F, message = F, warning = F----------------------------------------

## Four regression models predicting endorsement and experience of BCL / BBL:

lm01 <- lm(Endorse_BCL~IG_Fusion+IG_Identification+OG_Bonds+empathic_concern+perspective_taking+Age+Female+Married+Wealth_level,
           data = ds)

lm02 <- lm(Experience_BCL~IG_Fusion+IG_Identification+OG_Bonds+empathic_concern+perspective_taking+Age+Female+Married+Wealth_level, 
           data = ds)

lm03 <- lm(Endorse_BBL~IG_Fusion+IG_Identification+OG_Bonds+empathic_concern+perspective_taking+Age+Female+Married+Wealth_level, 
           data = ds)

lm04 <- lm(Experience_BBL~IG_Fusion+IG_Identification+OG_Bonds+empathic_concern+perspective_taking+Age+Female+Married+Wealth_level, 
           data = ds)




## ----error = F, message = F, warning = F, results = "hide"----------------------

## Tabulated results:

stargazer(lm01, lm02,
          lm03, lm04, 
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html")



## -------------------------------------------------------------------------------
htmltools::includeHTML("table1.html")



## ----error = F, message = F, warning = F----------------------------------------

ds$religious_freedom_per_01 <- as.numeric(ds$FORB1)

ds$religious_freedom_per_02a <- as.numeric(ds$FORB2)
ds$religious_freedom_per_02 <- (8 - ds$religious_freedom_per_02a)

ds$religious_freedom_per_03 <- as.numeric(ds$FORB3)
ds$religious_freedom_per_04 <- as.numeric(ds$FORB4)
ds$religious_freedom_per_05 <- as.numeric(ds$FORB5)
ds$religious_freedom_per_06 <- as.numeric(ds$FORB6)
ds$religious_freedom_per_07 <- as.numeric(ds$FORB7)
ds$religious_freedom_per_08 <- as.numeric(ds$FORB8)
ds$religious_freedom_per_09 <- as.numeric(ds$FORB9)

ds$sprf <- (ds$religious_freedom_per_01+ds$religious_freedom_per_02+
            ds$religious_freedom_per_03+ds$religious_freedom_per_04+
            ds$religious_freedom_per_05+ds$religious_freedom_per_06+
            ds$religious_freedom_per_07+ds$religious_freedom_per_08+
            ds$religious_freedom_per_09)/9

summary(ds$sprf)

ds %>% drop_na(sprf)%>%
ggplot(aes(x = sprf))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 30)+
  geom_textvline(label = "Mean = 6.00", 
                 xintercept = 6.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "SPRF score", 
       y = "Frequency", 
       title = "Social perception of religious freedom (full sample)")+
  theme_bw()


ds %>% 
   drop_na(sprf)%>%
ggplot(aes(x = sprf))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 30)+
  labs(x = "SPRF score", 
       y = "Frequency", 
       title = "Social perception of religious freedom by country")+
    facet_wrap(vars(Country), nrow = 2)+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

ds$religious_freedom_exp_01a <- as.numeric(ds$REL.PERS_1)
ds$religious_freedom_exp_01 <- (8 - ds$religious_freedom_exp_01a)

ds$religious_freedom_exp_02a <- as.numeric(ds$REL.PERS_2)
ds$religious_freedom_exp_02 <- (8 - ds$religious_freedom_exp_02a)

ds$religious_freedom_exp_03a <- as.numeric(ds$REL.PERS_3)
ds$religious_freedom_exp_03 <- (8 - ds$religious_freedom_exp_03a)

ds$religious_freedom_exp_04a <- as.numeric(ds$REL.PERS_4)
ds$religious_freedom_exp_04 <- (8 - ds$religious_freedom_exp_04a)

ds$religious_freedom_exp_05a <- as.numeric(ds$REL.PERS_5)
ds$religious_freedom_exp_05 <- (8 - ds$religious_freedom_exp_05a)

ds$religious_freedom_exp_06a <- as.numeric(ds$REL.PERS_6)
ds$religious_freedom_exp_06 <- (8 - ds$religious_freedom_exp_06a)

ds$religious_freedom_exp_07a <- as.numeric(ds$REL.PERS7)
ds$religious_freedom_exp_07 <- (8 - ds$religious_freedom_exp_07a)

ds$religious_freedom_exp_08a <- as.numeric(ds$REL.PERS8)
ds$religious_freedom_exp_08 <- (8 - ds$religious_freedom_exp_08a)

ds$religious_freedom_exp_09a <- as.numeric(ds$REL.PERS9)
ds$religious_freedom_exp_09 <- (9 - ds$religious_freedom_exp_09a)

ds$religious_freedom_exp_10a <- as.numeric(ds$REL.PERS10)
ds$religious_freedom_exp_10 <- (10 - ds$religious_freedom_exp_10a)

ds$religious_freedom_exp_11a <- as.numeric(ds$REL.PERS11)
ds$religious_freedom_exp_11 <- (11 - ds$religious_freedom_exp_11a)

ds$exp_religious_freedom <- (ds$religious_freedom_exp_01+ds$religious_freedom_exp_02+
                             ds$religious_freedom_exp_03+ds$religious_freedom_exp_04+
                             ds$religious_freedom_exp_05+ds$religious_freedom_exp_06+
                             ds$religious_freedom_exp_07+ds$religious_freedom_exp_08+
                             ds$religious_freedom_exp_09+ds$religious_freedom_exp_10+
                             ds$religious_freedom_exp_11)/11

summary(ds$exp_religious_freedom)

ds %>% drop_na(exp_religious_freedom)%>%
ggplot(aes(x = exp_religious_freedom))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 30)+
  geom_textvline(label = "Mean = 6.00", 
                 xintercept = 6.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Experience of religious freedom score", 
       y = "Frequency", 
       title = "Experience of religious freedom (full sample)")+
  theme_bw()


ds %>% 
   drop_na(exp_religious_freedom)%>%
ggplot(aes(x = exp_religious_freedom))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 30)+
  labs(x = "Experience of religious freedom score", 
       y = "Frequency", 
       title = "Experience of religious freedom by country")+
    facet_wrap(vars(Country), nrow = 2)+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

ds$Religion <- ds$religion2

ds2 <- ds %>% 
drop_na(Religion, exp_religious_freedom)%>%
 group_by(Religion) %>% 
 summarise(Exp_religious_freedom=mean(exp_religious_freedom),.groups = 'drop') %>%
  as.data.frame()

ds2

ds %>% drop_na(religion, exp_religious_freedom)%>%
ggplot(aes(y = exp_religious_freedom, 
           x = Religion))+
  geom_boxplot()+
  labs(x = "", 
       y = "Experience of religious freedom score", 
       title = "Experience of religious freedom (full sample)")+
  coord_flip()+
  theme_bw()


ds %>% drop_na(religion, exp_religious_freedom)%>%
ggplot(aes(y = exp_religious_freedom, 
           x = Religion))+
  geom_boxplot()+
  labs(x = "", 
       y = "Experience of religious freedom score", 
       title = "Experience of religious freedom by country and religion")+
  coord_flip()+
    facet_wrap(vars(Country), nrow = 2)+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

ds$event_negative_affect <- as.numeric(ds$NEG.AFF)
table(ds$event_negative_affect)

ds$event_positive_affect_01 <- (8 - ds$event_negative_affect)
table(ds$event_positive_affect_01)

ds$event_positive_affect_02 <- as.numeric(ds$POS.AFF)
table(ds$event_positive_affect_02)

ds$event_positive_affect <- ds$event_positive_affect_02

ds$event_episodic_recall_01 <- as.numeric(ds$VIV.MEM)
table(ds$event_episodic_recall_01)

ds$event_episodic_recall_02 <- as.numeric(ds$REL.MEM)
table(ds$event_episodic_recall_02)

ds$event_shared_perception_01 <- as.numeric(ds$SHRD.EXP)
table(ds$event_shared_perception_01)

ds$event_shared_perception_02 <- as.numeric(ds$SHRD.MEM)
table(ds$event_shared_perception_02)

ds$event_event_reflection_01 <- as.numeric(ds$REFL1)
table(ds$event_event_reflection_01)

ds$event_event_reflection_02 <- as.numeric(ds$REFL2)
table(ds$event_event_reflection_02)

ds$event_transformative_indiv_01 <- as.numeric(ds$PERSIG)
table(ds$event_transformative_indiv_01)

ds$event_transformative_indiv_02 <- as.numeric(ds$TRNSFM)
table(ds$event_transformative_indiv_02)

ds$event_transformative_group_01 <- as.numeric(ds$IMPTGRP1)
table(ds$event_transformative_group_01)

ds$event_transformative_group_02 <- as.numeric(ds$IMPTGRP2)
table(ds$event_transformative_group_02)

imagistic <- cbind.data.frame(ds$event_negative_affect, ds$event_positive_affect, 
                              ds$event_episodic_recall_01, ds$event_episodic_recall_02,
                              ds$event_shared_perception_01, ds$event_shared_perception_02,
                              ds$event_event_reflection_01, ds$event_event_reflection_02,
                              ds$event_transformative_indiv_01, ds$event_transformative_indiv_02,
                              ds$event_transformative_group_01, ds$event_transformative_group_02)

imagistic <- na.omit(imagistic)

names(imagistic) <- sub('ds\\$event\\_', '', names(imagistic))

mtx <- cor(imagistic[, c(1:12)])

corrplot(mtx, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))



## ----error = F, message = F, warning = F----------------------------------------

## Kaiser-Meyer-Olkin (KMO) test of factorability
KMO(r=cor(imagistic))

## Bartlett's test of sphericity
cortest.bartlett(imagistic)

## Parallel test
# parallel <- fa.parallel(imagistic)



## ----error = F, message = F, warning = F----------------------------------------

fit02 <- factanal(imagistic, 2, rotation="promax")
fit02



## ----error = F, message = F, warning = F----------------------------------------

fit06 <- factanal(imagistic, 6, rotation="promax")
fit06



## ----error = F, message = F, warning = F----------------------------------------

imagistic <- cbind.data.frame(ds$event_negative_affect, ds$event_positive_affect, 
                              ds$event_episodic_recall_01, ds$event_episodic_recall_02,
                              ds$event_shared_perception_01, ds$event_shared_perception_02,
                              ds$event_event_reflection_01, ds$event_event_reflection_02,
                              ds$event_transformative_indiv_01, ds$event_transformative_indiv_02,
                              ds$event_transformative_group_01, ds$event_transformative_group_02)


## Reliability:
alph01 <- psych::alpha(imagistic)
alph01



## ----error = F, message = F, warning = F----------------------------------------

ds$imagistic <- (((ds$event_positive_affect)+
                 (ds$event_negative_affect)+ 
                ((ds$event_episodic_recall_01+ds$event_episodic_recall_02)/2)+
                ((ds$event_shared_perception_01+ds$event_shared_perception_02)/2)+
                ((ds$event_event_reflection_01+ds$event_event_reflection_02)/2)+
                ((ds$event_transformative_indiv_01+ds$event_transformative_indiv_02)/2)+
                ((ds$event_transformative_group_01+ds$event_transformative_group_02)/2)))

summary(ds$imagistic)

ds %>% drop_na(imagistic)%>%
ggplot(aes(x = imagistic))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 50)+
  geom_textvline(label = "Mean = 32.00", 
                 xintercept = 32.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Imagistic experience score", 
       y = "Frequency", 
       title = "Imagistic experience (full sample)")+
  theme_bw()


ds %>% 
  drop_na(imagistic)%>%
ggplot(aes(x = imagistic))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 50)+
  labs(x = "Imagistic experience score", 
       y = "Frequency", 
       title = "Imagistic experience by country")+
    facet_wrap(vars(Country), nrow = 2)+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

## Positive affect:
negative_affect <- cbind.data.frame(ds$event_negative_affect)
negative_affect <- na.omit(negative_affect)

## Visualization:
ds$event_negative_affect <- (ds$event_negative_affect)
summary(ds$event_negative_affect)

ds %>% drop_na(event_negative_affect)%>%
ggplot(aes(x = event_negative_affect))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 10)+
  geom_textvline(label = "Mean = 4.00", 
                 xintercept = 4.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Negative affect about event", 
       y = "Frequency", 
       title = "Negative affect about event (full sample)")+
  theme_bw()


ds %>% 
  drop_na(event_negative_affect)%>%
ggplot(aes(x = event_negative_affect))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 10)+
  labs(x = "Negative affect about event", 
       y = "Frequency", 
       title = "Negative affect about event by country")+
    facet_wrap(vars(Country), nrow = 2)+
  theme_bw()




## ----error = F, message = F, warning = F----------------------------------------

## Positive affect:
positive_affect <- cbind.data.frame(ds$event_positive_affect)
positive_affect <- na.omit(positive_affect)

## Visualization:
ds$event_positive_affect <- (ds$event_positive_affect)
summary(ds$event_positive_affect)

ds %>% drop_na(event_positive_affect)%>%
ggplot(aes(x = event_positive_affect))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 10)+
  geom_textvline(label = "Mean = 5.00", 
                 xintercept = 5.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Positive affect about event", 
       y = "Frequency", 
       title = "Positive affect about event: Gambia")+
  theme_bw()


ds %>% 
  drop_na(event_positive_affect)%>%
ggplot(aes(x = event_positive_affect))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 10)+
  labs(x = "Positive affect about event", 
       y = "Frequency", 
       title = "Positive affect about event by country")+
    facet_wrap(vars(Country), nrow = 2)+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

## Episodic recall:
episodic_recall <- cbind.data.frame(ds$event_episodic_recall_01, ds$event_episodic_recall_02)
episodic_recall <- na.omit(episodic_recall)

names(episodic_recall) <- sub('ds\\$event\\_', '', names(episodic_recall))

mtx <- cor(episodic_recall[, c(1:2)])

## Correlation plot:
corrplot(mtx, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))

## Reliability:
alph01 <- psych::alpha(episodic_recall)
summary(alph01)

## Visualization:
ds$event_episodic_recall <- ((ds$event_episodic_recall_01+ds$event_episodic_recall_02)/2)
summary(ds$event_episodic_recall)

ds %>% drop_na(event_episodic_recall)%>%
ggplot(aes(x = event_episodic_recall))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 10)+
  geom_textvline(label = "Mean = 5.00", 
                 xintercept = 5.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Episodic recall of event", 
       y = "Frequency", 
       title = "Episodic recall of event (full sample)")+
  theme_bw()


ds %>% drop_na(event_episodic_recall)%>%
ggplot(aes(x = event_episodic_recall))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 10)+
  labs(x = "Episodic recall of event", 
       y = "Frequency", 
       title = "Episodic recall of event by country")+
    facet_wrap(vars(Country), nrow = 2)+
  theme_bw()
  
  


## ----error = F, message = F, warning = F----------------------------------------

## Shared perception:
shared_perception <- cbind.data.frame(ds$event_shared_perception_01, ds$event_shared_perception_02)
shared_perception <- na.omit(shared_perception)

names(shared_perception) <- sub('ds\\$event\\_', '', names(shared_perception))

mtx <- cor(shared_perception[, c(1:2)])

## Correlation plot:
corrplot(mtx, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))

## Reliability:
alph01 <- psych::alpha(shared_perception)
summary(alph01)

## Visualization:
ds$event_shared_perception <- ((ds$event_shared_perception_01+ds$event_shared_perception_02)/2)
summary(ds$event_shared_perception)

ds %>% drop_na(event_shared_perception)%>%
ggplot(aes(x = event_shared_perception))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 10)+
  geom_textvline(label = "Mean = 5.00", 
                 xintercept = 5.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Shared perception of event", 
       y = "Frequency", 
       title = "Shared perception of event (full sample)")+
  theme_bw()



ds %>% drop_na(event_shared_perception)%>%
ggplot(aes(x = event_shared_perception))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 10)+
  labs(x = "Shared perception of event", 
       y = "Frequency", 
       title = "Shared perception of event by country")+
    facet_wrap(vars(Country), nrow = 2)+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

## Reflection:
event_reflection <- cbind.data.frame(ds$event_event_reflection_01, ds$event_event_reflection_02)
event_reflection <- na.omit(event_reflection)

names(event_reflection) <- sub('ds\\$event\\_', '', names(event_reflection))

mtx <- cor(event_reflection[, c(1:2)])

## Correlation plot:
corrplot(mtx, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))

## Reliability:
alph01 <- psych::alpha(event_reflection)
summary(alph01)

## Visualization:
ds$event_event_reflection <- ((ds$event_event_reflection_01+ds$event_event_reflection_02)/2)
summary(ds$event_event_reflection)

ds %>% drop_na(event_event_reflection)%>%
ggplot(aes(x = event_event_reflection))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 10)+
  geom_textvline(label = "Mean = 5.00", 
                 xintercept = 5.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Reflection of event", 
       y = "Frequency", 
       title = "Reflection of event (full sample)")+
  theme_bw()


ds %>% drop_na(event_event_reflection)%>%
ggplot(aes(x = event_event_reflection))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 10)+
  labs(x = "Reflection of event", 
       y = "Frequency", 
       title = "Reflection of event by country")+
    facet_wrap(vars(Country), nrow = 2)+
  theme_bw()




## ----error = F, message = F, warning = F----------------------------------------

## Reflection:
event_transformative_indiv <- cbind.data.frame(ds$event_transformative_indiv_01, ds$event_transformative_indiv_02)

event_transformative_indiv <- na.omit(event_transformative_indiv)

names(event_transformative_indiv) <- sub('ds\\$event\\_', '', names(event_transformative_indiv))

mtx <- cor(event_transformative_indiv[, c(1:2)])

## Correlation plot:
corrplot(mtx, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))

## Reliability:
psych::alpha(event_transformative_indiv)

## Visualization:
ds$event_transformative_indiv <- ((ds$event_transformative_indiv_01+ds$event_transformative_indiv_02)/2)
summary(ds$event_transformative_indiv)

ds %>% drop_na(event_transformative_indiv)%>%
ggplot(aes(x = event_transformative_indiv))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 10)+
  geom_textvline(label = "Mean = 5.00", 
                 xintercept = 5.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Transformative event score", 
       y = "Frequency", 
       title = "Transformative event for individual (full sample)")+
  theme_bw()

ds %>% drop_na(event_transformative_indiv)%>%
ggplot(aes(x = event_transformative_indiv))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 10)+
  labs(x = "Transformative event score", 
       y = "Frequency", 
       title = "Transformative event for individual by country")+
    facet_wrap(vars(Country), nrow = 2)+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

event_transformative_group <- cbind.data.frame(ds$event_transformative_group_01, ds$event_transformative_group_02)
event_transformative_group <- na.omit(event_transformative_group)

names(event_transformative_group) <- sub('ds\\$event\\_', '', names(event_transformative_group))

mtx <- cor(event_transformative_group[, c(1:2)])

## Correlation plot:
corrplot(mtx, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))

## Reliability:
alph01 <- psych::alpha(event_transformative_group)
summary(alph01)

## Visualization:
ds$event_transformative_group <- ((ds$event_transformative_group_01+ds$event_transformative_group_02)/2)
summary(ds$event_transformative_group)

ds %>% drop_na(event_transformative_group)%>%
ggplot(aes(x = event_transformative_group))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 10)+
  geom_textvline(label = "Mean = 5.00", 
                 xintercept = 5.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Transformative event score", 
       y = "Frequency", 
       title = "Transformative event for group (full sample)")+
  theme_bw()



ds %>% drop_na(event_transformative_group)%>%
ggplot(aes(x = event_transformative_group))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 10)+
  labs(x = "Transformative event score", 
       y = "Frequency", 
       title = "Transformative event for group by country")+
    facet_wrap(vars(Country), nrow = 2)+
  theme_bw()



## -------------------------------------------------------------------------------

imagistic_subscales <- cbind.data.frame(ds$event_positive_affect, ds$event_negative_affect,
                                        ds$event_episodic_recall, 
                                        ds$event_shared_perception, ds$event_event_reflection, 
                                        ds$event_transformative_indiv, ds$event_transformative_group)


imagistic_subscales <- na.omit(imagistic_subscales)

names(imagistic_subscales) <- sub('ds\\$event\\_', '', names(imagistic_subscales))

mtx <- cor(imagistic_subscales[, c(1:6)])

## Correlation plot:
corrplot(mtx, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))



## ----error = F, message = F, warning = F----------------------------------------

## Four regression models predicting IG/OG Fusion/Identification:

lm01 <- lm(IG_Fusion~event_positive_affect+event_negative_affect+event_episodic_recall+event_shared_perception+event_event_reflection+
event_transformative_indiv+event_transformative_group+Age+Female+Married+Wealth_level,
           data = ds)

lm02 <- lm(IG_Identification~event_positive_affect+event_negative_affect+event_episodic_recall+event_shared_perception+event_event_reflection+
event_transformative_indiv+event_transformative_group+Age+Female+Married+Wealth_level,
           data = ds)

lm03 <- lm(OG_Bonds~event_positive_affect+event_negative_affect+event_episodic_recall+event_shared_perception+event_event_reflection+
event_transformative_indiv+event_transformative_group+Age+Female+Married+Wealth_level,
           data = ds)



## ----error = F, message = F, warning = F, results = "hide"----------------------

## Tabulated results:

stargazer(lm01, lm02,
          lm03, 
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html")



## -------------------------------------------------------------------------------
htmltools::includeHTML("table1.html")


## -------------------------------------------------------------------------------
## Save the latest version of data:
# fwrite(ds, file = "~/Desktop/oxford/data/BCL/BCL01.csv")


