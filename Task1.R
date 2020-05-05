library(ggplot2)
library(magrittr)
library(dplyr)
library(forcats)
library(viridis)
library(tidyr)

emperors <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")

emperors %>% 
  count(cause) %>% 
  arrange(n) %>% 
  mutate(assassinated = ifelse(cause=="Assassination", 
                             TRUE, FALSE),
         cause=fct_inorder(cause)) %>% 
  ggplot(aes(x=n, y=cause, fill=assassinated))+
  geom_col()+
  geom_text(aes(label=n, x= n - .25),
            color="white",
            size=5,
            hjust=1)+
  xlab("number")+
  scale_fill_manual(name=NULL, reigns=c("lightgray", "steelblue"))+
  theme(axis.line.y = element_blank(),
        legend.position = 0,
        panel.grid = element_blank(),
        plot.title = element_text(size=18, lineheight=0.9))+
ggtitle("Roman Emperor's", subtitle=paste0("(data of", count(emperors), "emperors)"))


###killer

emperors %>% 
  count(killer) %>% 
  arrange(n) %>% 
  mutate(Other_Emperor = ifelse(killer=="Other Emperor", 
                               TRUE, FALSE),
         killer=fct_inorder(killer)) %>% 
  ggplot(aes(x=n, y=killer, fill=Other_Emperor))+
  geom_col()+
  geom_text(aes(label=n, x= n - .25),
            color="white",
            size=5,
            hjust=1)+
  xlab("number")+
  scale_fill_manual(name=NULL, reigns=c("lightgray", "steelblue"))+
  theme(axis.line.y = element_blank(),
        legend.position = 0,
        panel.grid = element_blank(),
        plot.title = element_text(size=18, lineheight=0.9))+
  ggtitle("Roman Emperor's", subtitle=paste0("(data of", count(emperors), " emperors)"))
  

####own idea - age of the emperors - problem BC/AD

zero <- lubridate::as_date(lubridate::ymd_hms("0000-01-01-00-00-00"))


emperors$reign <- ifelse(emperors$reign_end<emperors$reign_start, 
                         as.integer(lubridate::time_length(difftime(emperors$reign_start, zero), "days")+lubridate::time_length(difftime(emperors$reign_end, zero), "days")), 
                         as.integer(lubridate::time_length(difftime(emperors$reign_end, emperors$reign_start), "days")))

emperors$age <- ifelse(emperors$death<emperors$birth, 
                         as.integer(lubridate::time_length(difftime(emperors$birth, zero), "days")+lubridate::time_length(difftime(emperors$death, zero), "days")), 
                         as.integer(lubridate::time_length(difftime(emperors$death, emperors$birth), "days")))
#1.
ggplot(emperors,aes(x=reign, y=name, fill=dynasty)) +geom_col()

#2.
ggplot(emperors,aes(x=reign, y=reorder(name, reign), fill=dynasty)) +geom_col(stat=identity)

#3. 
ggplot(emperors %>% arrange(dynasty, desc(reign)) %>%
         mutate(name=factor(name, levels=name)), 
       aes(x=reign,y=name, fill = dynasty)) +
  geom_col(show.legend = FALSE)

#4
ggplot(emperors %>% arrange(dynasty, desc(reign)) %>%
         mutate(name=factor(name, levels=name)), 
       aes(x=reign,y=name, fill = dynasty)) +
  geom_col(show.legend = FALSE)+
  geom_vline(aes(xintercept = mean(emperors$reign)))+
  scale_fill_viridis_d()

#5
ggplot(emperors %>% arrange(dynasty, desc(reign)) %>%
         mutate(name=factor(name, levels=name)), 
       aes(x=reign,y=name, fill = dynasty)) +
  geom_col(show.legend = TRUE)+
  scale_x_continuous(expand = c(0, 0),limits = c(0, 16000), breaks=c(0, 2500, 5000,7500, 10000, 12500, 15000))+
  geom_vline(aes(xintercept = mean(emperors$reign)))+
  scale_fill_viridis_d()+
  geom_text(aes(label=reign),
          color="black",
          size=4,
          hjust=0)+
  xlab("Period of reign in Days")+
  ylab("Name of the Emperor")+
  theme(axis.line.y = element_blank(),
        legend.position = 1,
        panel.grid = element_blank(),
        plot.title = element_text(size=18, lineheight=0.9))+
  labs(title = "How long did a Roman emperor rule?", subtitle=paste0("(Average reign periode:", round(mean(emperors$reign)), " days)"), 
       fill="Dynasty")+
  theme(legend.position="bottom")


####Pie Chart Task

#####Data and old Visualization
data <- data.frame(
  percent1994=c(35,25,20,10,5,5),
  percent2014=c(41,21,17,8,4,9),
  music=c("hard rock", "samba", "hiphop","reggea","country","classic")
)
 

pie1 <- ggplot2::ggplot(data, aes(x="",y=percent1994, fill=music))+
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)+
  ggtitle("music preferences 1944")

pie2 <- ggplot2::ggplot(data,aes(x="",y=percent2014, fill=music))+
  geom_bar(stat="identity", width=1)+
  coord_polar("y",start=0)+
  ggtitle("music preferences 2014")

library(patchwork)

pie1+pie2

ggsave("graphic_example.jpg", width=15, height=7)

#####New Version

data %>%
  gather("Type", "Value",-music) %>%
  ggplot(aes(music, Value, fill = Type)) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d()+
  theme_bw()+
  facet_wrap(~music,scales = "free_x") +
  labs(title = "Music Preferences in percent",
       subtitle = "comparison between 1994 and 2014| source: Martin",
       x = "Music Genre",
       fill = c("Year"))






