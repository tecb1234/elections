library(dplyr)
library(ggplot2)
library(tidyr)

elections2010Raw <- read.csv("Election2010.csv")

seats2010<-elections2010Raw %>%
  select(Seat,Win10) %>%
  group_by(Win10) %>%
  summarise(NumSeats=length(Win10)) %>%
  rename(Party = Win10) %>%
  ungroup() %>%
  transform(Party = factor(Party,levels=Party[rank(-NumSeats)]))


p<- ggplot(seats2010,aes(Party,NumSeats)) + geom_bar(stat = "identity")
p <- p + geom_hline(yintercept=325)
p <- p + scale_y_continuous(limits=c(0,400))
p <- p + theme_bw()
p <- p + theme(
  plot.background = element_blank()
  ,panel.grid.major.x = element_blank()
  ,panel.grid.minor= element_blank()
  ,panel.border = element_blank()
  ,axis.ticks = element_blank()
  ,axis.title.y = element_text(angle=0,vjust=1,hjust=5)
  ,axis.title.x = element_blank()
)
p <- p + geom_hline(yintercept=0)
p <- p + geom_segment(aes(x = 0, y = 0, xend = 0, yend = 400))
p <- p+ ylab("Number of Seats")
p 


elections2010Tidy<-elections2010Raw %>%
  select(1:3,Win10 ,Con10:UKIP10) %>%
  gather(Party, VoteShare, Con10:UKIP10, Votes, Convt10:UKIPvt10) %>%
  arrange(Seat)





