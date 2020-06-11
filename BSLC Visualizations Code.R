library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggalt)
library(grid)
library(gridExtra) 
library(patchwork)


# Plot 1: Siblings Comprehension and Speaking Scores
Ratings_3 <- tribble(
  ~'Subject', ~'Sibling Order',    ~'Family',   ~'Score Type',    ~'Score',
  'Daniel (O)',      'Older',              '1',         'Comprehend',             10,
  'Daniel (O)',      'Older',              '1',         'Speak',             10,
  'Mia (Y)',      'Younger',               '1',         'Comprehend',             10,
  'Mia (Y)',      'Younger',               '1',         'Speak',             10,
  'Brent (O)',     'Older',                '2',         'Comprehend',             10,
  'Brent (O)',     'Older',                '2',         'Speak',             10,
  'Kyle (Y)',       'Younger',             '2',        'Comprehend',              9,
  'Kyle (Y)',       'Younger',             '2',        'Speak',              7,
  'Benito (O)',     'Older',               '3',         'Comprehend',             10,
  'Benito (O)',     'Older',               '3',         'Speak',             7,
  'Oscar (Y)',       'Younger',            '3',         'Comprehend',              6,
  'Oscar (Y)',       'Younger',            '3',         'Speak',              5,
  'Samuel (O)',       'Older',             '4',        'Comprehend',              10,
  'Samuel (O)',       'Older',             '4',        'Speak',              10,
  'Kendra (Y)',      'Younger',            '4',        'Comprehend',              10,
  'Kendra (Y)',      'Younger',            '4',        'Speak',              5,
  'Lani (O)',        'Older',              '5',        'Comprehend',              10,
  'Lani (O)',        'Older',              '5',        'Speak',              8,
  'Leo (Y)',         'Younger',            '5',        'Comprehend',               9,
  'Leo (Y)',         'Younger',            '5',        'Speak',               2,
  'Bryce (O)',      'Older',               '6',        'Comprehend',               8,
  'Bryce (O)',      'Older',               '6',        'Speak',               8,
  'Melvin (Y)',     'Younger',             '6',        'Comprehend',               9,
  'Melvin (Y)',     'Younger',             '6',        'Speak',               9
)

Ratings_3 <- Ratings_3 %>% mutate(Family_ScoreType = factor(paste(Family, `Score Type`, sep="_")))

#Separate to multiple dataframes based on family
Uniq22<-data.frame(table(Ratings_3$Family))

for(i in 1:nrow(Uniq22)){
  assign(paste0(Uniq22[i,1],"_family"), data.frame(Ratings_3[ which(Ratings_3$Family==Uniq22[i,1]), ]))
  
}

# Stacked Bar Graph (Family/ Score Type is on y-axis)
ONES<-ggplot2::ggplot(`1_family`, aes(fill=`Score.Type`, y=`Score`, x=`Subject`)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Family 1") + 
  coord_flip() +
  scale_x_discrete(limits = rev(levels(`1_family`$Subject))) +
  geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5), linetype="solid", color = "black", size=1)+
  scale_fill_manual(values = c("Black","Grey"))+theme(legend.position = "none")+
  theme(plot.title = element_text(size=12, hjust = 0.5))

# TWO FAMILY STACK
TWOS<-ggplot2::ggplot(`2_family`, aes(fill=`Score.Type`, y=`Score`, x=`Subject`)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Family 2") + 
  coord_flip() +
  scale_x_discrete(limits = rev(levels(`2_family`$Subject))) +
  geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5), linetype="solid", color = "black", size=1)+
  scale_fill_manual(values = c("Black","Grey"))+
  theme(plot.title = element_text(size=12, hjust = 0.5))

# THREE FAMILY STACK
THREES<-ggplot2::ggplot(`3_family`, aes(fill=`Score.Type`, y=`Score`, x=`Subject`)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Family 3") + 
  coord_flip() +
  scale_x_discrete(limits = rev(levels(`3_family`$Subject))) +
  geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5), linetype="solid", color = "black", size=1)+
  scale_fill_manual(values = c("Black","Grey"))+theme(legend.position = "none")+
  theme(plot.title = element_text(size=12, hjust = 0.5))

# FOUR FAMILY STACK
FOURS<-ggplot2::ggplot(`4_family`, aes(fill=`Score.Type`, y=`Score`, x=`Subject`)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Family 4") + 
  coord_flip() +
  scale_x_discrete(limits = rev(levels(`4_family`$Subject))) +
  geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5), linetype="solid", color = "black", size=1)+
  scale_fill_manual(values = c("Black","Grey"))+theme(legend.position = "none")+
  theme(plot.title = element_text(size=12, hjust = 0.5))+
  scale_x_discrete(limits = rev(levels(factor(`4_family`$Subject))))

# FIVE FAMILY STACK
FIVES<-ggplot2::ggplot(`5_family`, aes(fill=`Score.Type`, y=`Score`, x=`Subject`)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Family 5") + 
  coord_flip() +
  scale_x_discrete(limits = rev(levels(`5_family`$Subject))) +
  geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5), linetype="solid", color = "black", size=1)+
  scale_fill_manual(values = c("Black","Grey"))+theme(legend.position = "none")+
  theme(plot.title = element_text(size=12, hjust = 0.5))

# SIX FAMILY STACK
SIXS<-ggplot2::ggplot(`6_family`, aes(fill=`Score.Type`, y=`Score`, x=`Subject`)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Family 6") + 
  coord_flip() +
  scale_x_discrete(limits = rev(levels(`6_family`$Subject))) +
  geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5), linetype="solid", color = "black", size=1)+
  scale_fill_manual(values = c("Black","Grey"))+theme(legend.position = "none")+
  theme(plot.title = element_text(size=12, hjust = 0.5))

scores <- ((ONES + TWOS)/
             (THREES + FOURS)/
             (FIVES + SIXS)) + 
  plot_layout(guides = 'collect')


ggsave("test.png", plot = scores, device = "png", width = 7 , height = 5, 
       units = "in", dpi = 300)


################## 
# Plot 2: Expressive Vocabularly Scores
# ENGLISH vs SPANISH
Vocab2 <- tribble(
  ~'Subject',    ~'Siblings.Order',     ~'English', ~'Spanish',
  'Daniel (O)',   'Older',                75,       70,
  'Mia (Y)',        'Younger',              25,       39,
  'Brent (O)',       'Older',              13,        3,
  'Kyle (Y)',          'Younger',            18,      1,
  'Benito (O)',        'Older',            25,         1,
  'Oscar (Y)',        'Younger' ,        37,           1,
  'Samuel (O)',         'Older',          61,          4,
  'Kendra (Y)',         'Younger',     45,            4,
  'Lani (O)',          'Older',         95,      4,
  'Leo (Y)',           'Younger',       99,     1,
  'Bryce (O)',         'Older',       62,      77,
  'Melvin (Y)',        'Younger',     96,     92,
) 

Vocab2$Subject <- factor(Vocab2$Subject, levels = as.character(Vocab2$Subject))

Vocab$Subject<- factor(Vocab$Subject, levels = levels(Vocab$Subject))

vocab_chart <- ggplot(Vocab2, aes(y = Subject)) + 
  geom_point(data = Vocab, aes(x = Score, color = Language), size = 2.5) +
  geom_dumbbell(aes(x = Spanish, xend = English), size=2.5, color="darkgrey", 
                colour_x = "red3", colour_xend = "deepskyblue",
                dot_guide=TRUE, dot_guide_size=0.25) +
  scale_color_manual(name = "", values = c("deepskyblue", "red3") )+
  labs(x=NULL, y=NULL, 
       title="Dumbbell Chart", 
       subtitle="Spanish vs English Score")+
  geom_text(color="black", size=2,vjust= 1.8,hjust=1.3,
            aes(x=Spanish, label=paste(Spanish)))+
  geom_text(aes(x=English, label=paste(English)), 
            color="black", size=2,vjust= 1.8,hjust=0.1)+
  scale_y_discrete(limits = rev(levels(as.factor(Vocab2$Subject))))
#scale_x_continuous(labels = function(x) paste0(x*1, "%")) # Multiply by 100 & add % 


ggsave("Vocab_chart.png", plot = vocab_chart, device = "png", width = 7 , height = 5, 
       units = "in", dpi = 300)

###########
# Plot 3: TMA Distribution between older and younger siblings

TMA_total <- tribble(
  ~'TMA', ~'Younger siblings', ~'Older siblings',
  "Present",       18.72,           35.98,
  "Preterite",     55.07,           41.08,
  "Imperfect",     24.89,           18.98,
  "Subjunctive",  0.44,             1.70, 
  "Other",        0.88,             2.22
) 

# Geom dumbbell plot
TMA_total$TMA <- factor(TMA_total$TMA, levels = as.character(TMA_total$TMA))

TMA_total2=tidyr::gather(TMA_total, group, value, -TMA)
TMA_total2$TMA <- factor(TMA_total2$TMA, levels = levels(TMA_total2$TMA))

TMA <- ggplot(TMA_total, aes(y = TMA)) + 
  geom_point(data = TMA_total2, aes(x = value, color = group), size = 2.5) +
  geom_dumbbell(aes(x = `Younger siblings`, xend = `Older siblings`), size=2.5, color="darkgrey", 
                colour_x = "deepskyblue", colour_xend = "black",
                dot_guide=TRUE, dot_guide_size=0.25) +
  scale_color_manual(name = "", values = c("black", "deepskyblue") )+
  labs(x=NULL, y=NULL, 
       title="Dumbbell Chart", 
       subtitle="Younger vs Older siblings")+
  geom_text(color="black", size=2,vjust= 1.8,hjust=1,
            aes(x=`Younger siblings`, label=paste(`Younger siblings`,"%")))+
  geom_text(aes(x=`Older siblings`, label=paste(`Older siblings`,"%")), 
            color="black", size=2,vjust= 1.8,hjust=0.5)+
  scale_y_discrete(limits = rev(levels(as.factor(TMA_total$TMA))))+
  scale_x_continuous(labels = function(x) paste0(x*1, "%")) # Multiply by 100 & add % 

ggsave("TMA.png", plot = TMA, device = "png", width = 7, height = 4, 
       units = "in", dpi = 300)

######################################
# Present 

English_insertions2 <- tribble(
  ~'Family',   ~'Younger',~'Older',
  '1',       31.88 ,               7.14,
  '2',       15.52,              37.70,
  '3',       46.77,              47.54,
  '4',       5.97,              85.96,
  '5',       26.00,               6.56, 
  '6',       20.97,                28.57
)      

English_insertions3 <- tribble( 
  ~'Subject', ~'Sibling order', ~'Family', ~'Presence',
  'Daniel (O)', 'Older', '1', 7.14,
  'Mia (Y)', 'Younger', '1', 31.88,
  'Brent (O)', 'Older', '2', 37.70,
  'Kyle (Y)', 'Younger', '2', 15.52,
  'Benito (O)', 'Older', '3', 47.54,
  'Oscar (Y)', 'Younger', '3', 46.77,
  'Samuel (O)', 'Older', '4', 85.96,
  'Kendra (Y)', 'Younger', '4', 5.97,
  'Lani (O)', 'Older', '5', 6.56,
  'Leo (Y)', 'Younger', '5', 26.00,
  'Bryce (O)', 'Older', '6', 28.57,
  'Melvin (Y)', 'Younger', '6', 20.97 
)


# Geom dumbbell plot
English_insertions2$Family <- factor(English_insertions2$Family, levels = as.character(English_insertions2$Family))

English_insertions3$Family<- factor(English_insertions3$Family, levels = levels(English_insertions3$Family))

Present <- ggplot(English_insertions2, aes(y = Family)) + 
  geom_point(data = English_insertions3, aes(x = Presence, color = `Sibling order`), size = 2.5) +
  geom_dumbbell(aes(x = `Younger`, xend = `Older`), size=2.5, color="darkgrey", 
                colour_x = "deepskyblue", colour_xend = "black",
                dot_guide=TRUE, dot_guide_size=0.25) +
  scale_color_manual(name = "", values = c("black", "deepskyblue") )+
  labs(x=NULL, y=NULL, 
       title="Present Tense", 
       subtitle="Younger Versus Older siblings")+
  geom_text(color="black", size=2,vjust= 1.8,hjust=1.3,
            aes(x=`Younger`, label=paste(`Younger`,"%")))+
  geom_text(aes(x=`Older`, label=paste(`Older`,"%")), 
            color="black", size=2,vjust= 1.8,hjust=0.1)+
  scale_y_discrete(limits = rev(levels(as.factor(English_insertions2$Family))))+
  scale_x_continuous(labels = function(x) paste0(x*1, "%")) # Multiply by 100 & add % 

ggsave("present.png", plot = Present, device = "png", width = 7, height = 5, 
       units = "in", dpi = 300)

##############################
# Preterite
English_insertions2 <- tribble(
  ~'Family',   ~'Younger',~'Older',
  '1',        33.33,               60.71,
  '2',       44.83,              36.07,
  '3',       37.10,              40.98,
  '4',       49.25,              5.26,
  '5',       48.00,                 55.74, 
  '6',       58.06,                48.21
)      


English_insertions3 <- tribble(
  ~'Subject', ~'Sibling order', ~'Family', ~'Presence',
  'Daniel (O)', 'Older', '1', 60.71,
  'Mia (Y)', 'Younger', '1', 33.33,
  'Brent (O)', 'Older', '2', 36.07,
  'Kyle (Y)', 'Younger', '2', 44.83,
  'Benito (O)', 'Older', '3', 40.98,
  'Oscar (Y)', 'Younger', '3', 37.10,
  'Samuel (O)', 'Older', '4', 5.26,
  'Kendra (Y)', 'Younger', '4', 49.25,
  'Lani (O)', 'Older', '5', 55.74,
  'Leo (Y)', 'Younger', '5', 48.00,
  'Bryce (O)', 'Older', '6', 48.21,
  'Melvin (Y)', 'Younger', '6', 58.06 
  
)
# Geom dumbbell plot of Preterite Distribution
English_insertions2$Family <- factor(English_insertions2$Family, levels = as.character(English_insertions2$Family))

English_insertions3$Family<- factor(English_insertions3$Family, levels = levels(English_insertions3$Family))

Preterite <- ggplot(English_insertions2, aes(y = Family)) + 
  geom_point(data = English_insertions3, aes(x = Presence, color = `Sibling order`), size = 2.5) +
  geom_dumbbell(aes(x = `Younger`, xend = `Older`), size=2.5, color="darkgrey", 
                colour_x = "deepskyblue", colour_xend = "black",
                dot_guide=TRUE, dot_guide_size=0.25) +
  scale_color_manual(name = "", values = c("black", "deepskyblue") )+
  labs(x=NULL, y=NULL, 
       title="Dumbbell Chart", 
       subtitle="Younger vs Older siblings")+
  geom_text(color="black", size=2,vjust= 1.8,hjust=1.3,
            aes(x=`Younger`, label=paste(`Younger`,"%")))+
  geom_text(aes(x=`Older`, label=paste(`Older`,"%")), 
            color="black", size=2,vjust= 1.8,hjust=0.1)+
  scale_y_discrete(limits = rev(levels(as.factor(English_insertions2$Family))))+
  scale_x_continuous(labels = function(x) paste0(x*1, "%")) # Multiply by 100 & add % 

ggsave("Preterite.png", plot = Preterite, device = "png", width = 7, height = 5, 
       units = "in", dpi = 300)
###################################
# Imperfect Distribution

English_insertions2 <- tribble(
  ~'Family',   ~'Younger',~'Older',
  '1',       31.88 ,              0.36,
  '2',       41.38,            22.95,
  '3',       16.13,             8.20,
  '4',       43.28,             1.75,
  '5',       24.00,               31.15, 
  '6',       25.81,                19.64
)      

English_insertions3 <- tribble(
  ~'Subject', ~'Sibling order', ~'Family', ~'Presence',
  'Daniel (O)', 'Older', '1', 30.36,
  'Mia (Y)', 'Younger', '1', 31.88,
  'Brent (O)', 'Older', '2', 22.95,
  'Kyle (Y)', 'Younger', '2', 41.38,
  'Benito (O)', 'Older', '3', 8.20,
  'Oscar (Y)', 'Younger', '3', 16.13,
  'Samuel (O)', 'Older', '4', 1.75,
  'Kendra (Y)', 'Younger', '4', 43.28,
  'Lani (O)', 'Older', '5', 31.15,
  'Leo (Y)', 'Younger', '5', 24.00,
  'Bryce (O)', 'Older', '6', 19.64,
  'Melvin (Y)', 'Younger', '6', 25.81
  
)

# Geom dumbbell plot of Imperfect distribution
English_insertions2$Family <- factor(English_insertions2$Family, levels = as.character(English_insertions2$Family))

English_insertions3$Family<- factor(English_insertions3$Family, levels = levels(English_insertions3$Family))

Imperfect <- ggplot(English_insertions2, aes(y = Family)) + 
  geom_point(data = English_insertions3, aes(x = Presence, color = `Sibling order`), size = 2.5) +
  geom_dumbbell(aes(x = `Younger`, xend = `Older`), size=2.5, color="darkgrey", 
                colour_x = "deepskyblue", colour_xend = "black",
                dot_guide=TRUE, dot_guide_size=0.25) +
  scale_color_manual(name = "", values = c("black", "deepskyblue") )+
  labs(x=NULL, y=NULL, 
       title="Dumbbell Chart", 
       subtitle="Younger vs Older siblings")+
  geom_text(color="black", size=2,vjust= 1.8,hjust=1.3,
            aes(x=`Younger`, label=paste(`Younger`,"%")))+
  geom_text(aes(x=`Older`, label=paste(`Older`,"%")), 
            color="black", size=2,vjust= 1.8,hjust=0.1)+
  scale_y_discrete(limits = rev(levels(as.factor(English_insertions2$Family))))+
  scale_x_continuous(labels = function(x) paste0(x*1, "%")) # Multiply by 100 & add % 

ggsave("Imperfect.png", plot = Imperfect, device = "png", width = 7, height = 5, 
       units = "in", dpi = 300)

######################################################
# Spanish Lexical and Linguistic Features

# Plot 3: Distribution of Target vs non-target instances
gender_subject <- tribble(
  ~'Subject',    ~'Family',   ~'Target',    ~'Non-Target',
  'Daniel (O)',      '1',       100,         0,
  'Mia (Y)',         '1',       100,         0,
  'Brent (O)',       '2',       100,         0,
  'Kyle (Y)',        '2',       90.91,        9.09,  
  'Benito (O)',      '3',       97.06,      2.94,
  'Oscar (Y)',       '3',       70,         30,
  'Samuel (O)',      '4',       100,        0,
  'Kendra (Y)',      '4',      87.23,      12.77,
  'Lani (O)',        '5',      100,        0,
  'Leo (Y)',         '5',       16.67,     83.33,
  'Bryce (O)',       '6',       100,       0,
  'Melvin (Y)',      '6',       100,       0
) # Stacked bar chart. Will use as model for other similar plots.

gender_subject2=tidyr::gather(gender_subject, group, value, -c(Subject,Family))

#Separate to multiple dataframes based on family
Uniq23<-data.frame(table(gender_subject2$Family))

for(i in 1:nrow(Uniq23)){
  assign(paste0(Uniq23[i,1],"_Family"), data.frame(gender_subject2[ which(gender_subject2$Family==Uniq23[i,1]), ]))
  
}


# Stacked Bar Graph (Family1)
ONE3<-ggplot2::ggplot(`1_Family`, aes(fill=`group`, y=`value`, x=`Subject`)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Family 1") + 
  theme(axis.text.x = element_text(angle=0)) +
  geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5), linetype="solid", color = "black", size=1)+
  scale_fill_manual(values = c("Black","Grey"))+theme(legend.position = "none")+
  theme(plot.title = element_text(size=12, hjust = 0.5)) + ylab("Distribution")+
  scale_x_discrete(limits = rev(levels(factor(`1_Family`$Subject))))+
  scale_y_continuous(labels = function(x) paste0(x*1, "%")) # Multiply by 100 & add % 


# Stacked Bar Graph (Family2)
TWO3<-ggplot2::ggplot(`2_Family`, aes(fill=`group`, y=`value`, x=`Subject`)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Family 2") + 
  theme(axis.text.x = element_text(angle=0)) +
  geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5), linetype="solid", color = "black", size=1)+
  scale_fill_manual(values = c("Black","Grey"))+
  theme(plot.title = element_text(size=12, hjust = 0.5)) + ylab("Distribution")+
  scale_x_discrete(limits = rev(levels(factor(`2_Family`$Subject))))+
  scale_y_continuous(labels = function(x) paste0(x*1, "%")) # Multiply by 100 & add % 


# Stacked Bar Graph (Family3)
THREE3<-ggplot2::ggplot(`3_Family`, aes(fill=`group`, y=`value`, x=`Subject`)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Family 3") + 
  theme(axis.text.x = element_text(angle=0)) +
  geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5), linetype="solid", color = "black", size=1)+
  scale_fill_manual(values = c("Black","Grey"))+
  theme(plot.title = element_text(size=12, hjust = 0.5)) + ylab("Distribution")+
  scale_x_discrete(limits = rev(levels(factor(`3_Family`$Subject))))+
  scale_y_continuous(labels = function(x) paste0(x*1, "%")) # Multiply by 100 & add % 

# Stacked Bar Graph (Family4)
FOUR3<-ggplot2::ggplot(`4_Family`, aes(fill=`group`, y=`value`, x=`Subject`)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Family 4") + 
  theme(axis.text.x = element_text(angle=0)) +
  geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5), linetype="solid", color = "black", size=1)+
  scale_fill_manual(values = c("Black","Grey"))+
  theme(plot.title = element_text(size=12, hjust = 0.5)) + ylab("Distribution")+
  #scale_x_discrete(limits = rev(levels(factor(`4_Family`$Subject))))+
  scale_y_continuous(labels = function(x) paste0(x*1, "%")) # Multiply by 100 & add % 

# Stacked Bar Graph (Family5)
FIVE3<-ggplot2::ggplot(`5_Family`, aes(fill=`group`, y=`value`, x=`Subject`)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Family 5") + 
  theme(axis.text.x = element_text(angle=0)) +
  geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5), linetype="solid", color = "black", size=1)+
  scale_fill_manual(values = c("Black","Grey"))+
  theme(plot.title = element_text(size=12, hjust = 0.5)) + ylab("Distribution")+
  scale_x_discrete(limits = rev(levels(factor(`5_Family`$Subject))))+
  scale_y_continuous(labels = function(x) paste0(x*1, "%")) # Multiply by 100 & add % 

# Stacked Bar Graph (Family6)
SIX3<-ggplot2::ggplot(`6_Family`, aes(fill=`group`, y=`value`, x=`Subject`)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Family 6") + 
  theme(axis.text.x = element_text(angle=0)) +
  geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5), linetype="solid", color = "black", size=1)+
  scale_fill_manual(values = c("Black","Grey"))+
  theme(plot.title = element_text(size=12, hjust = 0.5)) + ylab("Distribution")+
  scale_x_discrete(limits = rev(levels(factor(`6_Family`$Subject))))+
  scale_y_continuous(labels = function(x) paste0(x*1, "%")) # Multiply by 100 & add % 

#ALL TOGETHER
subject <- ((ONE3 + TWO3)/
              (THREE3 + FOUR3)/
              (FIVE3 + SIX3)) +  
  plot_layout(guides = 'collect')

ggsave("test.png", plot = subject, device = "png", width = 7 , height = 5, 
       units = "in", dpi = 300)

##########
## Object position

#Distribution of Target vs non-target instances
gender_subject <- tribble(
  ~'Subject',    ~'Family',   ~'Target',    ~'Non-Target',
  'Daniel (O)',      '1',       100,         0,
  'Mia (Y)',         '1',       92.59,         7.41,
  'Brent (O)',       '2',       100,         0,
  'Kyle (Y)',        '2',       60,         40,  
  'Benito (O)',      '3',       87.50,      12.50,
  'Oscar (Y)',       '3',       82.35,         17.65,
  'Samuel (O)',      '4',       93.1,        6.9,
  'Kendra (Y)',      '4',      58.82,      41.18,
  'Lani (O)',        '5',      100,        0,
  'Leo (Y)',         '5',       50,     50,
  'Bryce (O)',       '6',       100,       0,
  'Melvin (Y)',      '6',       96.15,       3.85
) # Stacked bar chart. Will use as model for other similar plots.

gender_subject2=tidyr::gather(gender_subject, group, value, -c(Subject,Family))

#Separate to multiple dataframes based on family
Uniq23<-data.frame(table(gender_subject2$Family))

for(i in 1:nrow(Uniq23)){
  assign(paste0(Uniq23[i,1],"_Family"), data.frame(gender_subject2[ which(gender_subject2$Family==Uniq23[i,1]), ]))
  
}

# Stacked Bar Graph (Family1)
ONE3<-ggplot2::ggplot(`1_Family`, aes(fill=`group`, y=`value`, x=`Subject`)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Family 1") + 
  theme(axis.text.x = element_text(angle=0)) +
  geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5), linetype="solid", color = "black", size=1)+
  scale_fill_manual(values = c("Black","Grey"))+theme(legend.position = "none")+
  theme(plot.title = element_text(size=12, hjust = 0.5)) + ylab("Distribution")+
  scale_x_discrete(limits = rev(levels(factor(`1_Family`$Subject))))+
  scale_y_continuous(labels = function(x) paste0(x*1, "%")) # Multiply by 100 & add % 


# Stacked Bar Graph (Family2)
TWO3<-ggplot2::ggplot(`2_Family`, aes(fill=`group`, y=`value`, x=`Subject`)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Family 2") + 
  theme(axis.text.x = element_text(angle=0)) +
  geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5), linetype="solid", color = "black", size=1)+
  scale_fill_manual(values = c("Black","Grey"))+
  theme(plot.title = element_text(size=12, hjust = 0.5)) + ylab("Distribution")+
  scale_x_discrete(limits = rev(levels(factor(`2_Family`$Subject))))+
  scale_y_continuous(labels = function(x) paste0(x*1, "%")) # Multiply by 100 & add % 


# Stacked Bar Graph (Family3)
THREE3<-ggplot2::ggplot(`3_Family`, aes(fill=`group`, y=`value`, x=`Subject`)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Family 3") + 
  theme(axis.text.x = element_text(angle=0)) +
  geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5), linetype="solid", color = "black", size=1)+
  scale_fill_manual(values = c("Black","Grey"))+
  theme(plot.title = element_text(size=12, hjust = 0.5)) + ylab("Distribution")+
  scale_x_discrete(limits = rev(levels(factor(`3_Family`$Subject))))+
  scale_y_continuous(labels = function(x) paste0(x*1, "%")) # Multiply by 100 & add % 

# Stacked Bar Graph (Family4)
FOUR3<-ggplot2::ggplot(`4_Family`, aes(fill=`group`, y=`value`, x=`Subject`)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Family 4") + 
  theme(axis.text.x = element_text(angle=0)) +
  geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5), linetype="solid", color = "black", size=1)+
  scale_fill_manual(values = c("Black","Grey"))+
  theme(plot.title = element_text(size=12, hjust = 0.5)) + ylab("Distribution")+
  #scale_x_discrete(limits = rev(levels(factor(`4_Family`$Subject))))+
  scale_y_continuous(labels = function(x) paste0(x*1, "%")) # Multiply by 100 & add % 

# Stacked Bar Graph (Family5)
FIVE3<-ggplot2::ggplot(`5_Family`, aes(fill=`group`, y=`value`, x=`Subject`)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Family 5") + 
  theme(axis.text.x = element_text(angle=0)) +
  geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5), linetype="solid", color = "black", size=1)+
  scale_fill_manual(values = c("Black","Grey"))+
  theme(plot.title = element_text(size=12, hjust = 0.5)) + ylab("Distribution")+
  scale_x_discrete(limits = rev(levels(factor(`5_Family`$Subject))))+
  scale_y_continuous(labels = function(x) paste0(x*1, "%")) # Multiply by 100 & add % 

# Stacked Bar Graph (Family6)
SIX3<-ggplot2::ggplot(`6_Family`, aes(fill=`group`, y=`value`, x=`Subject`)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Family 6") + 
  theme(axis.text.x = element_text(angle=0)) +
  geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5), linetype="solid", color = "black", size=1)+
  scale_fill_manual(values = c("Black","Grey"))+
  theme(plot.title = element_text(size=12, hjust = 0.5)) + ylab("Distribution")+
  scale_x_discrete(limits = rev(levels(factor(`6_Family`$Subject))))+
  scale_y_continuous(labels = function(x) paste0(x*1, "%")) # Multiply by 100 & add % 

#ALL TOGETHER
object <- ((ONE3 + TWO3)/
             (THREE3 + FOUR3)/
             (FIVE3 + SIX3)) +  
  plot_layout(guides = 'collect')

ggsave("test.png", plot = object, device = "png", width = 7 , height = 5, 
       units = "in", dpi = 300)


###########
##########
## Verb production

#Distribution of Target vs non-target instances of Gender Subject
gender_subject <- tribble(
  ~'Subject',    ~'Family',   ~'Target',    ~'Non-Target',
  'Daniel (O)',      '1',       100,         0,
  'Mia (Y)',         '1',       91.3,         8.7,
  'Brent (O)',       '2',       100,         0,
  'Kyle (Y)',        '2',       84.48,         15.51,  
  'Benito (O)',      '3',       100,      0,
  'Oscar (Y)',       '3',       83.87,         16.13,
  'Samuel (O)',      '4',       100,        0,
  'Kendra (Y)',      '4',      65.67,      34.33,
  'Lani (O)',        '5',      95.08,        4.92,
  'Leo (Y)',         '5',       70,     30,
  'Bryce (O)',       '6',       100,       0,
  'Melvin (Y)',      '6',       90.32,       9.68
) # Stacked bar chart. Will use as model for other similar plots.

gender_subject2=tidyr::gather(gender_subject, group, value, -c(Subject,Family))

#Separate to multiple dataframes based on family
Uniq23<-data.frame(table(gender_subject2$Family))

for(i in 1:nrow(Uniq23)){
  assign(paste0(Uniq23[i,1],"_Family"), data.frame(gender_subject2[ which(gender_subject2$Family==Uniq23[i,1]), ]))
  
}

# Stacked Bar Graph (Family1)
ONE3<-ggplot2::ggplot(`1_Family`, aes(fill=`group`, y=`value`, x=`Subject`)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Family 1") + 
  theme(axis.text.x = element_text(angle=0)) +
  geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5), linetype="solid", color = "black", size=1)+
  scale_fill_manual(values = c("Black","Grey"))+theme(legend.position = "none")+
  theme(plot.title = element_text(size=12, hjust = 0.5)) + ylab("Distribution")+
  scale_x_discrete(limits = rev(levels(factor(`1_Family`$Subject))))+
  scale_y_continuous(labels = function(x) paste0(x*1, "%")) # Multiply by 100 & add % 


# Stacked Bar Graph (Family2)
TWO3<-ggplot2::ggplot(`2_Family`, aes(fill=`group`, y=`value`, x=`Subject`)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Family 2") + 
  theme(axis.text.x = element_text(angle=0)) +
  geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5), linetype="solid", color = "black", size=1)+
  scale_fill_manual(values = c("Black","Grey"))+
  theme(plot.title = element_text(size=12, hjust = 0.5)) + ylab("Distribution")+
  scale_x_discrete(limits = rev(levels(factor(`2_Family`$Subject))))+
  scale_y_continuous(labels = function(x) paste0(x*1, "%")) # Multiply by 100 & add % 


# Stacked Bar Graph (Family3)
THREE3<-ggplot2::ggplot(`3_Family`, aes(fill=`group`, y=`value`, x=`Subject`)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Family 3") + 
  theme(axis.text.x = element_text(angle=0)) +
  geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5), linetype="solid", color = "black", size=1)+
  scale_fill_manual(values = c("Black","Grey"))+
  theme(plot.title = element_text(size=12, hjust = 0.5)) + ylab("Distribution")+
  scale_x_discrete(limits = rev(levels(factor(`3_Family`$Subject))))+
  scale_y_continuous(labels = function(x) paste0(x*1, "%")) # Multiply by 100 & add % 

# Stacked Bar Graph (Family4)
FOUR3<-ggplot2::ggplot(`4_Family`, aes(fill=`group`, y=`value`, x=`Subject`)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Family 4") + 
  theme(axis.text.x = element_text(angle=0)) +
  geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5), linetype="solid", color = "black", size=1)+
  scale_fill_manual(values = c("Black","Grey"))+
  theme(plot.title = element_text(size=12, hjust = 0.5)) + ylab("Distribution")+
  #scale_x_discrete(limits = rev(levels(factor(`4_Family`$Subject))))+
  scale_y_continuous(labels = function(x) paste0(x*1, "%")) # Multiply by 100 & add % 

# Stacked Bar Graph (Family5)
FIVE3<-ggplot2::ggplot(`5_Family`, aes(fill=`group`, y=`value`, x=`Subject`)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Family 5") + 
  theme(axis.text.x = element_text(angle=0)) +
  geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5), linetype="solid", color = "black", size=1)+
  scale_fill_manual(values = c("Black","Grey"))+
  theme(plot.title = element_text(size=12, hjust = 0.5)) + ylab("Distribution")+
  scale_x_discrete(limits = rev(levels(factor(`5_Family`$Subject))))+
  scale_y_continuous(labels = function(x) paste0(x*1, "%")) # Multiply by 100 & add % 

# Stacked Bar Graph (Family6)
SIX3<-ggplot2::ggplot(`6_Family`, aes(fill=`group`, y=`value`, x=`Subject`)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Family 6") + 
  theme(axis.text.x = element_text(angle=0)) +
  geom_vline(xintercept=c(2.5, 4.5, 6.5, 8.5, 10.5), linetype="solid", color = "black", size=1)+
  scale_fill_manual(values = c("Black","Grey"))+
  theme(plot.title = element_text(size=12, hjust = 0.5)) + ylab("Distribution")+
  scale_x_discrete(limits = rev(levels(factor(`6_Family`$Subject))))+
  scale_y_continuous(labels = function(x) paste0(x*1, "%")) # Multiply by 100 & add % 

#ALL TOGETHER
verb <- ((ONE3 + TWO3)/
           (THREE3 + FOUR3)/
           (FIVE3 + SIX3)) +  
  plot_layout(guides = 'collect')

ggsave("test.png", plot = verb, device = "png", width = 7 , height = 5, 
       units = "in", dpi = 300)

#ALL TOGETHER
((ONE3 + TWO3)/
    (THREE3 + FOUR3)/
    (FIVE3 + SIX3)) +  
  plot_layout(guides = 'collect')


##########
# Plot 10: English insertions
English_insertions <- tribble(
  ~'Subject',    ~'Family',   ~'Presence',
  'Daniel (O)',      '1',       7.14,        
  'Mia (Y)',         '1',       4.35,  
  'Brent (O)',       '2',       9.84,        
  'Kyle (Y)',        '2',       16.95,        
  'Benito (O)',      '3',       14.75,    
  'Oscar (Y)',       '3',       51.61,       
  'Samuel (O)',      '4',       7.02,
  'Kendra (Y)',      '4',      14.93,      
  'Lani (O)',        '5',      6.56,       
  'Leo (Y)',         '5',       58,     
  'Bryce (O)',       '6',       7.14,
  'Melvin (Y)',      '6',       6.45
)      


English_insertions2 <- tribble(
  ~'Family',   ~'Younger',~'Older',
  '1',        4.35,               7.14,
  '2',       16.95,              9.84,
  '3',       51.61,              14.75,
  '4',       14.93,              7.02,
  '5',       58,                 6.56, 
  '6',       6.45,                7.14
)      

# Plot 4: Presence of English Lexical Insertions
English_insertions3 <- tribble(
  ~'Subject', ~'Sibling order', ~'Family', ~'Presence',
  'Daniel (O)', 'Older', '1', 7.14,
  'Mia (Y)', 'Younger', '1', 4.35,
  'Brent (O)', 'Older', '2', 9.84,
  'Kyle (Y)', 'Younger', '2', 16.95,
  'Benito (O)', 'Older', '3', 14.75,
  'Oscar (Y)', 'Younger', '3', 51.61,
  'Samuel (O)', 'Older', '4', 7.02,
  'Kendra (Y)', 'Younger', '4', 14.93,
  'Lani (O)', 'Older', '5', 6.56,
  'Leo (Y)', 'Younger', '5', 58,
  'Bryce (O)', 'Older', '6', 7.14,
  'Melvin (Y)', 'Younger', '6', 6.45
  
)


# Geom dumbbell plot of English Lexical Insertions
English_insertions2$Family <- factor(English_insertions2$Family, levels = as.character(English_insertions2$Family))

English_insertions3$Family<- factor(English_insertions3$Family, levels = levels(English_insertions3$Family))

Lexical <- ggplot(English_insertions2, aes(y = Family)) + 
  geom_point(data = English_insertions3, aes(x = Presence, color = `Sibling order`), size = 2.5) +
  geom_dumbbell(aes(x = `Younger`, xend = `Older`), size=2.5, color="darkgrey", 
                colour_x = "deepskyblue", colour_xend = "black",
                dot_guide=TRUE, dot_guide_size=0.25) +
  scale_color_manual(name = "", values = c("black", "deepskyblue") )+
  labs(x=NULL, y=NULL, 
       title="Presence of English Lexical Insertions", 
       subtitle="Younger versus Older siblings")+
  geom_text(color="black", size=2,vjust= 1.8,hjust=1.3,
            aes(x=`Younger`, label=paste(`Younger`,"%")))+
  geom_text(aes(x=`Older`, label=paste(`Older`,"%")), 
            color="black", size=2,vjust= 1.8,hjust=0.1)+
  scale_y_discrete(limits = rev(levels(as.factor(English_insertions2$Family))))+
  scale_x_continuous(labels = function(x) paste0(x*1, "%")) # Multiply by 100 & add % 


ggsave("Lexical.png", plot = Lexical, device = "png", width = 7 , height = 5, 
       units = "in", dpi = 300)






