file <- file.choose()
volcano<- read.csv(file=file, sep= ',' , header = TRUE, stringsAsFactors = FALSE)
View(volcano)

library(ggplot2)
library(grid)
#install.packages("maps")
library(maps)
#Retrieve world map from the ggplot2 package. See ?map_data for examples
world.map <- map_data("world")


# Plot world map with dots for earthquakes. The size of the dots and their color are according to magnitude of earthquake
ggplot()+
  geom_polygon(data = world.map, aes(x = long, y = lat, group = group),fill = "white",alpha=0.2)+
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(),axis.ticks = element_blank(),plot.margin=unit(c(3, 0, 0, 0),"mm"),legend.text = element_text(size = 6),legend.title = element_text(size = 8, face = "plain"),panel.background = element_rect(fill='black'))+ 
  #sets the theme. Background color is black so the world map now appears (white on the black background).
  geom_point(aes(x=Longitude,y=Latitude,size=volcano$Elevation..m., color=volcano$Elevation..m.),data=volcano)+ #Adds the earthquake points, with the size and color according to "mag" variable (magnitude).
  coord_fixed(ylim = c(-82.5, 87.5), xlim = c(-185, 185))+
  scale_size_continuous(range = c(0.25, 1))+ #size gradient for points
  scale_color_continuous(low="yellow",high="red")+ #color gradient for points
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank())
#+
  #geom_text(aes(x=35,y=-75),label=paste("Earthquakes recorded since ",EQ_min_year),color="white",hjust=0,size=3.5)

us.map<-map_data("state")
volcanoesus<-volcano[volcano$Country=="United States",]
ggplot()+
  geom_polygon(data = us.map, aes(x = long, y = lat, group = group),fill = "white",alpha=0.2)+
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(),axis.ticks = element_blank(),plot.margin=unit(c(3, 0, 0, 0),"mm"),
        legend.text = element_text(size = 6),legend.title = element_text(size = 8, face = "plain"),panel.background = element_rect(fill='black'))+ 
  #sets the theme. Background color is black so the world map now appears (white on the black background).
  geom_point(aes(x=Longitude,y=Latitude,size=Elevation..m., color=Elevation..m.),data=volcanoesus)+ 
  #coord_fixed(ylim = c(-82.5, 87.5), xlim = c(-185, 185))+
  scale_size_continuous(range = c(0.25, 1))+ #size gradient for points
  scale_color_continuous(low="yellow",high="red")+ #color gradient for points
  theme(legend.position="none",axis.title.y=element_blank(),axis.title.x=element_blank())
#+

mostvolcanoes<-data.frame(table(volcano$Country))
mostvolcanoes[order(-mostvolcanoes$Freq),]
top6countries<-volcano[volcano$Country=="United States"|volcano$Country=="Russia"|
                         volcano$Country=="Indonesia"|volcano$Country=="Japan"|volcano$Country=="Chile"|volcano$Country=="Ethiopia",]

ggplot()+
  geom_polygon(data = world.map, aes(x = long, y = lat, group = group),fill = "white",alpha=0.2)+
  theme_classic()+
  theme(axis.line = element_blank(), axis.text = element_blank(),axis.ticks = element_blank(),plot.margin=unit(c(3, 0, 0, 0),"mm"),legend.text = element_text(size = 6),legend.title = element_text(size = 8, face = "plain"),panel.background = element_rect(fill='black'))+ 
  #sets the theme. Background color is black so the world map now appears (white on the black background).
  geom_point(aes(x=Longitude,y=Latitude,size=Elevation..m., color=Elevation..m.),data=top6countries)+ #Adds the earthquake points, with the size and color according to "mag" variable (magnitude).
  coord_fixed(ylim = c(-82.5, 87.5), xlim = c(-185, 185))+
  scale_size_continuous(range = c(0.25, 1.1))+ #size gradient for points
  scale_color_continuous(low="yellow",high="red")+ #color gradient for points
  guides(fill  = guide_legend) + 
  theme(legend.position="bottom")#theme(legend.position="bottomleft",axis.title.y=element_blank(),axis.title.x=element_blank())

#2nd
#install.packages("treemap")
library(treemap)
commomrocks<-data.frame(table(volcano$Dominant.Rock.Type))
View(commomrocks)
commomrocks$Freq<-as.numeric(commomrocks$Freq)
str(commomrocks)
treemap(dtf = commomrocks,index = c("Var1"),vSize = "Freq",vColor = "Freq",
        palette="Greys",type="value", 
        border.col ="white", title="Dominant Rock Types")
RColorBrewer::display.brewer.all()

#3rd
commontype<-data.frame(table(volcano$Activity.Evidence))
headofcommon<-commontype[order(-commontype$Freq),]
barplot(headofcommon$Freq[1:5], horiz = TRUE, xlim = c(0,600),
        col = c("lightblue","turquoise","steelblue","blue","navyblue"),
        xlab = "Number of occurences",
        legend.text = c("Eruption Observed","Evidence Credible","Eruption Dated","Evidence Uncertain","Unrest / Holocene"),
                main= "Number of eruptions by activity evidence")

#4th    
filteredvolcano<-volcano[volcano$Country=="United States"|
                           volcano$Country=="Russia"| volcano$Country=="Indonesia"|
                           volcano$Country=="Chile"|volcano$Country=="Japan"| volcano$Country=="Ethiopia",]
View(filteredvolcano)


for(i in 1:nrow(filteredvolcano))
{
  if (filteredvolcano[i,4]=="Stratovolcano(es)"|filteredvolcano[i,4]=="Stratovolcano?")
  {
    filteredvolcano[i,4]="Stratovolcano"
  }
  if (filteredvolcano[i,4]=="Pyroclastic cone(s)")
  {
    filteredvolcano[i,4]="Pyroclastic cone"
  }
  if (filteredvolcano[i,4]=="Shield(s)")
  {
    filteredvolcano[i,4]="Shield"
  }
  if (filteredvolcano[i,4]=="Caldera(s)")
  {
    filteredvolcano[i,4]="Caldera"
  }
  if (filteredvolcano[i,4]=="Lava dome(s)")
  {
    filteredvolcano[i,4]="Lava dome"
  }
  
}

xyz<-table(filteredvolcano$Primary.Volcano.Type)
xyz<-data.frame(xyz)
View(xyz[order(-xyz$Freq),])

xyz1<-table(filteredvolcano$Primary.Volcano.Type,filteredvolcano$Country)
xyz1<-data.frame(xyz1)

toplot<-xyz1[xyz1$Var1=="Stratovolcano"|xyz1$Var1=="Shield"|xyz1$Var1=="Caldera"|
               xyz1$Var1=="Pyroclastic cone"| xyz1$Var1=="Submarine"| xyz1$Var1=="Volcanic field"|
               xyz1$Var1=="Complex"| xyz1$Var1=="Lava dome",]


View(toplot)
colnames(toplot)<-c("Volcano Type","Country","No of occurences")
par(mfrow=c(2,4))

#firstquad
one<-toplot[toplot$`Volcano Type`=="Stratovolcano",]
barplot(one$`No of occurences`, main = "Stratovolcano",
        xlab = "Country", ylab = "No of occurence",
        col = c("yellow","yellow3","orange","salmon","red","red3"))

#secondquad
two<-toplot[toplot$`Volcano Type`=="Shield",]
barplot(two$`No of occurences`, main = "Shield Volcano",
        xlab = "Country", ylab = "No of occurence",
        col = c("yellow","yellow3","orange","salmon","red","red3"))

#third
three<-toplot[toplot$`Volcano Type`=="Caldera",]
barplot(three$`No of occurences`, main = "Pyroclastic cone Volcano",
        xlab = "Country", ylab = "No of occurence", 
        #legend.text = c("US", "Russia","Indonesia","Japan","Chile","Ethiopia"),
        args.legend = list(x="center"),col = c("yellow","yellow3","orange","salmon","red","red3"))


#fourth
four<-toplot[toplot$`Volcano Type`=="Pyroclastic cone",]
barplot(five$`No of occurences`, main = "Pyroclastic cone",
        xlab = "Country", ylab = "No of occurence",
        col = c("yellow","yellow3","orange","salmon","red","red3"))

#fifth
five<-toplot[toplot$`Volcano Type`=="Submarine",]
barplot(six$`No of occurences`, main = "Submarine Volcano",
        xlab = "Country", ylab = "No of occurence",
        col = c("yellow","yellow3","orange","salmon","red","red3"))

#sixth
six<-toplot[toplot$`Volcano Type`=="Volcanic field",]
barplot(four$`No of occurences`, main = "Volcanic field",
        xlab = "Country", ylab = "No of occurence",
        col = c("yellow","yellow3","orange","salmon","red","red3"))

#seventh
six<-toplot[toplot$`Volcano Type`=="Complex",]
barplot(six$`No of occurences`, main = "Complex Volcano",
        xlab = "Country", ylab = "No of occurence",
        col = c("yellow","yellow3","orange","salmon","red","red3"))

#eighth
six<-toplot[toplot$`Volcano Type`=="Lava dome",]
barplot(six$`No of occurences`, main = "Lava dome Volcano",
        xlab = "Country", ylab = "No of occurence",
        col = c("yellow","yellow3","orange","salmon","red","red3"))


#5th
#statemapofUS
us.map<-map_data("state")
volcanoesus<-volcano[volcano$Country=="United States" & volcano$Latitude<50 & volcano$Longitude< -100 & volcano$Longitude>-130,]
#Total <- merge(all_states, Prison, by="")
ggplot(data = us.map)+
  geom_polygon(data = us.map, aes(x = long, 
                                  y = lat, group = group, fill=region),color = "gray",alpha=0.1)+guides(fill = FALSE)+
  geom_point(aes(x=Longitude,y=Latitude,size=Elevation..m., color=Elevation..m.),data=volcanoesus)+scale_color_gradient(low="orange", high="red")
  #coord_fixed(ylim = c(-82.5, 87.5), xlim = c(-185, 185))+
  

#6th
#donut graph
dat<- data.frame(table(volcano$Region))
colnames(dat)=c("category","count")
dat$fraction = dat$count / sum(dat$count)
dat = dat[order(dat$fraction), ]
dat$ymax = cumsum(dat$fraction)
dat$ymin = c(0, head(dat$ymax, n=-1))

# Make the plot
ggplot(dat, aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  annotate("text", x = 0, y = 0, label = "Volcano prone Regions") +
  labs(title="")#+geom_label(aes(label=paste(round(fraction*100,2),"%"),
                #    x=3.5,y=(ymin+ymax)/2),inherit.aes = TRUE, show.legend = FALSE)

