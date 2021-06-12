#Cumulative scaled rings = largest scales represnet the greatest observation 

library(tidyverse)
library(wesanderson)
library(patchwork)
library(ggimage)
fishing <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/fishing.csv')

#Variability of Erie data
da<-fishing%>%filter(lake=="Erie") %>% mutate(row=row_number())
cumulative<- ave(rep(1,nrow(da)), da[c("species")], FUN=cumsum)
da<-cbind(da,cumulative)
da<-da%>% mutate(probability=cumulative/row)
da<-da%>% mutate(surprisal=log(1/probability))
circleFun <- function(center = c(0,0),diameter = 1, npoints = 14634){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
dat <- circleFun(c(1,-1),2.3,npoints = 14634)
da<-cbind(dat,da)
pal <- wes_palette("Darjeeling2", 21, type = "continuous")
t<-ggplot(da,aes(x,y)) +geom_point(aes(colour=surprisal),size=19)+ scale_colour_gradientn(colours = pal)+ theme_void() + theme(legend.position = "none") + theme(plot.background = element_rect(fill = "papayawhip", colour=NA)) + scale_size_continuous(range = c(10, 100)) + annotate("text", x = 1.2, y = 0.6, hjust = 1, vjust = 1, label = " Erie", size= 10, family= "Comic Sans MS",color = "black") + annotate("path", x = c(-0.1,1), y = c(-1,-0.5),size = 1,colour="tan")  + geom_image(aes(x = 1, y = -0.5, image = "http://huntfishmanitoba.ca/sites/default/files/lake-whitefish-600.png"), size = 0.2, by = "height")
#Variability of superior data
set<-fishing%>%filter(lake=="Superior") %>% mutate(row=row_number())
cumulative<- ave(rep(1,nrow(set)), set[c("species")], FUN=cumsum)
set<-cbind(set,cumulative)
set<-set%>% mutate(probability=cumulative/row)
set<-set%>% mutate(surprisal=log(1/probability))
circleFun <- function(center = c(0,0),diameter = 1, npoints = 7905){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
dat <- circleFun(c(1,-1),2.3,npoints = 7905)
set<-cbind(dat,set)
pal <- wes_palette("Darjeeling2", 21, type = "continuous")
p<-ggplot(set,aes(x,y)) +geom_point(aes(colour=surprisal),size=19)+ scale_colour_gradientn(colours = pal)+ theme_void() + theme(legend.position = "none") + theme(plot.background = element_rect(fill = "papayawhip", colour=NA))  + scale_size_continuous(range = c(10, 100)) +
annotate("text", x = 1.4, y = 0.6, hjust = 1, vjust = 1, label = " Superior", size= 10, family= "Comic Sans MS",color = "black") + annotate("path", x = c(0.45,1), y = c(0.01,-0.5),size = 1,colour="tan") + annotate("path", x = c(-0.20,1), y = c(-0.8,-0.8),size = 1,colour="tan") + geom_image(aes(x = 1, y = -0.5, image = "https://cdn.pixabay.com/photo/2021/05/06/12/06/lake-trout-6233286_1280.png"), size = 0.2, by = "height")+ geom_image(aes(x = 1, y = -0.8, image = "http://huntfishmanitoba.ca/sites/default/files/lake-whitefish-600.png"), size = 0.2, by = "height")

#Variability of huron data
df<-fishing%>%filter(lake=="Huron") %>% mutate(row=row_number())
cumulative<- ave(rep(1,nrow(df)), df[c("species")], FUN=cumsum)
df<-cbind(df,cumulative)
df<-df%>% mutate(probability=cumulative/row)
df<-df%>% mutate(surprisal=log(1/probability))
circleFun <- function(center = c(0,0),diameter = 1, npoints = 17115){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
dat <- circleFun(c(1,-1),2.3,npoints = 17115)
df<-cbind(dat,df)
pal <- wes_palette("Darjeeling2", 21, type = "continuous")
s<-ggplot(df,aes(x,y)) +geom_point(aes(colour=surprisal),size=19)+ annotate("text", x = 1.4, y = 0.6, hjust = 1, vjust = 1, label = " Huron", size= 10, family= "Comic Sans MS",color = "black")+scale_colour_gradientn(colours = pal)+ theme_void() + theme(legend.position = "none") + theme(plot.background = element_rect(fill = "papayawhip", colour=NA))  + scale_size_continuous(range = c(10, 100)) + annotate("path", x = c(-0.15,1), y = c(-0.6,-0.5),size = 1,colour="tan") + annotate("path", x = c(-0.20,1), y = c(-0.8,-1),size = 1,colour="tan") + annotate("path", x = c(-0.15,1), y = c(-0.6,-0.5),size = 1,colour="tan") + annotate("path", x = c(1,2), y = c(-1.5,-1.8),size = 1,colour="tan")  + geom_image(aes(x = 1, y = -1.5, image = "/Users/thivina/Documents/TidyTuesday06-08/images/walleye.png"), size = 0.2, by = "height") + geom_image(aes(x = 1, y = -0.5, image = "https://cdn.pixabay.com/photo/2021/05/06/12/06/lake-trout-6233286_1280.png"), size = 0.2, by = "height")+ geom_image(aes(x = 1, y = -1, image = "http://huntfishmanitoba.ca/sites/default/files/lake-whitefish-600.png"), size = 0.2, by = "height") 
#Variability of ontario data
data<-fishing%>%filter(lake=="Ontario") %>% mutate(row=row_number())
cumulative<- ave(rep(1,nrow(data)), data[c("species")], FUN=cumsum)
data<-cbind(data,cumulative)
data<-data%>% mutate(probability=cumulative/row)
data<-data%>% mutate(surprisal=log(1/probability))
circleFun <- function(center = c(0,0),diameter = 1, npoints = 4576){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
dat <- circleFun(c(1,-1),2.3,npoints = 4576)
data<-cbind(dat,data)
pal <- wes_palette("Darjeeling2", 21, type = "continuous")
r<-ggplot(data,aes(x,y)) +geom_point(aes(colour=surprisal),size=19)+ scale_colour_gradientn(colours = pal)+ theme_void() + theme(legend.position = "none") + theme(plot.background = element_rect(fill = "papayawhip", colour=NA)) + scale_size_continuous(range = c(10, 100)) + annotate("text", x = 1.4, y = 0.6, hjust = 1, vjust = 1, label = " Ontario", size= 10, family= "Comic Sans MS",color = "black")  + geom_image(aes(x = 1, y = -0.5, image = "http://huntfishmanitoba.ca/sites/default/files/lake-whitefish-600.png"), size = 0.2, by = "height")+ annotate("path", x = c(-0.1,1), y = c(-1.4,-0.5),size = 1,colour="tan")
#Variability of michigan data
dt<-fishing%>%filter(lake=="Michigan") %>% mutate(row=row_number())
cumulative<- ave(rep(1,nrow(dt)), dt[c("species")], FUN=cumsum)
dt<-cbind(dt,cumulative)
dt<-dt%>% mutate(probability=cumulative/row)
dt<-dt%>% mutate(surprisal=log(1/probability))
circleFun <- function(center = c(0,0),diameter = 1, npoints = 19134){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
dat <- circleFun(c(1,-1),2.3,npoints = 19134)
dt<-cbind(dat,dt)
pal <- wes_palette("Darjeeling2", 21, type = "continuous")
m<-ggplot(dt,aes(x,y)) +geom_point(aes(colour=surprisal),size=19)+ scale_colour_gradientn(colours = pal)+ theme_void() + theme(legend.position = "none") + theme(plot.background = element_rect(fill = "papayawhip", colour=NA)) + scale_size_continuous(range = c(10, 100)) + annotate("text", x = 1.4, y = 0.6, hjust = 1, vjust = 1, label = " Michigan", size= 10, family= "Comic Sans MS",color = "black")  + geom_image(aes(x = 1, y = -0.5, image = "http://huntfishmanitoba.ca/sites/default/files/lake-whitefish-600.png"), size = 0.2, by = "height")+ annotate("path", x = c(-0.1,1), y = c(-1.2,-0.5),size = 1,colour="tan") + annotate("path", x = c(-0.1,1), y = c(-1.6,-1),size = 1,colour="tan")  + geom_image(aes(x = 1, y = -1.1, image = "https://cdn.pixabay.com/photo/2021/05/06/12/06/lake-trout-6233286_1280.png"), size = 0.2, by = "height")
#Variability of st clair data
d<-fishing%>%filter(lake=="Saint Clair") %>% mutate(row=row_number())

cumulative<- ave(rep(1,nrow(d)), d[c("species")], FUN=cumsum)
d<-cbind(d,cumulative)
d<-d%>% mutate(probability=cumulative/row)
d<-d%>% mutate(surprisal=log(1/probability))
circleFun <- function(center = c(0,0),diameter = 1, npoints = 2342){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
dat <- circleFun(c(1,-1),2.3,npoints = 2342)
d<-cbind(dat,d)
pal <- wes_palette("Darjeeling2", 21, type = "continuous")
c<-ggplot(d,aes(x,y)) +geom_point(aes(colour=surprisal),size=19)+ scale_colour_gradientn(colours = pal)+ theme_void() + theme(legend.position = "none") + theme(plot.background = element_rect(fill = "papayawhip", colour=NA)) + scale_size_continuous(range = c(10, 100)) + annotate("text", x = 1.4, y = 0.6, hjust = 1, vjust = 1, label = " Saint Clair", size= 10, family= "Comic Sans MS",color = "black") 
#ggplot() + geom_image(aes(x = 5, y = -1.5, image = "/Users/thivina/Documents/TidyTuesday06-08/images/pike.png"), size = 0.2, by = "height")

a<-p+t+s+m+ggsave(path = "images", filename = "test.png", dpi = 128, width = 15, height =8.4 )
x<-ggplot() + theme_void()+  theme(plot.background = element_rect(fill = "tan", colour=NA)) + draw_image( image = "/Users/thivina/Documents/TidyTuesday06-08/images/test.png") + draw_image(image = "/Users/thivina/Documents/TidyTuesday06-08/images/tape.png",x = -0.06, y = -0.07, width=0.2,height=0.2)+ draw_image(image = "/Users/thivina/Documents/TidyTuesday06-08/images/tape.png",x = -0.06, y = 0.9, width=0.2,height=0.2)+ draw_image(image = "/Users/thivina/Documents/TidyTuesday06-08/images/tape.png",x = 0.85, y = 0.9, width=0.2,height=0.2) + draw_image(image = "/Users/thivina/Documents/TidyTuesday06-08/images/tape.png",x = 0.85, y = -0.07, width=0.2,height=0.2) 
x<-x+ggsave(path = "images", filename = "test2.png", dpi = 128, width = 15, height =8.4 )
