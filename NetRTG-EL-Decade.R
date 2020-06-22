library(tidyverse)
library(cowplot)
library(ggrepel)
library(ggimage)

year <- 2011:2020
BAR <- c(8.3, 28.2, 19.4, 8.1, 16.5, 10.7, -4.8, 2.4, 3.7, 8)
RMD <- c(10.4, 14.3, 13.0, 32.6, 13.1, 6.2, 10.6, 8.8, 10.8, 10.2)
PAO <- c(13.8, 13.0, 3.6, 4.5, 3.5, 6.4, 4.3, 2.0, 1.5, 0.3)
OLY <- c(10.1, 3.4, 7.1, 11.0, 5.2, 9.6, 5.2, 1.5, 1.4, -2.1)
MAC <- c(13.5, 8.0, 14.3, 7.4, 1.9, -5.9, -7.6, -4.1, 1.4, 6.8)
CSKA <- c(-8.4, 19.0, 10.3, 8.1, 22.8, 17.4, 10.3, 13.5, 9.0, 8.7)
EFES <- c(-1.7, -4.1, -0.3, -3.6, 3.7, 7.6, 0.4, -9.9, 7.2, 13.4)
EAM <- c(-4.2, 0.6, -1.0, -2.8, -2.8, -8.4, -8.9, -5.8, 0.1, -3.9)
FNB <- c(10.1, 3.7, -1.6, 14.1, 7.8, 8.9, 2.0, 8.1, 12.4, -1.6)
ZAL <- c(-0.4, -6.6, 16.0, -3.5, 0.7, -4.8, -2.0, 1.4, 1.7, 3.2)

net1 <- data.frame(year,BAR,RMD,PAO,OLY,MAC,CSKA,EFES,EAM,FNB,ZAL)
net1$image <- c(rep("C:\\logos\\mac.png",7),"C:\\logos\\oly.png",
                "C:\\logos\\pao.png","C:\\logos\\el.png")

# this example is for Maccabi. The same steps can be followed for the rest of the teams


# Base R

plot(MAC~year, data=net1, ylim=c(-15,15), xaxt="n",
     pch=20, col="forestgreen", type="b", 
     main = "PAO BC, 2011-20 Regular Seasons", xlab="Season", ylab="Net Rating")
text(net1$BAR~net1$year, labels=BAR ,data=net1, cex=0.9, pos=3)
text(x=2018.7, y=-29, "source: @acbbstatscy", font=2)
axis(1, at=seq(2011,2020,1))
abline(h=0)


# GGPlot

p <- ggplot(data=net1, aes(x=as.integer(year), y=MAC, group=1)) +
  geom_line(color="darkgoldenrod1")+
  geom_point(col="darkgoldenrod1", size=2)+
  geom_hline(yintercept=0, linetype=2, color="grey60") +
  labs(title = "Maccabi Tel Aviv, 2011-20 Regular Seasons",
       subtitle = "Team logo shows championships",
       y = "Net Rating", x = "Year",
       caption = "Source: @acbbstatscy") +
  ylim(-15,15) + scale_x_continuous(breaks = year) +
  geom_image(data = net1 %>% filter(year==2014),aes(image=image), 
             size=0.05) +
  geom_text(data = net1, aes(x = year, y = MAC, label = MAC), 
            inherit.aes = F,
            color = "black", nudge_y = -1.7, size=5.2) +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        plot.title=element_text(face="bold"),
        axis.line = element_line(colour = "grey19", size = 1))

png("mac_test.png", units="in", width=8, height=5, res=500)
ggdraw(p) + 
  draw_image(net1$image[10], x = 1, y = 1, hjust = 1, vjust = 1, width = 0.13, height = 0.2)

dev.off()