
library(XML)
library(stringr)
library(tidyverse)
library(viridis)
library(gridExtra)
library(cowplot)
library(magick)
library(shadowtext)

# Get current list of US salamanders
#download.file("https://amphibiaweb.org/cgi/amphib_ws_locality?where-isocc=us&rel-isocc=like","USspecies_list.xml",sep="")
doc<-xmlParse('USspecies_list.xml')
order<- xmlToDataFrame(node=getNodeSet(doc,'//amphibiaweb/amphibian/order'))[[1]]
family <- xmlToDataFrame(node=getNodeSet(doc,'//amphibiaweb/amphibian/family'))[[1]]
myspecies <- xmlToDataFrame(node=getNodeSet(doc,'//amphibiaweb/amphibian/scientificname'))[[1]]
caudatelist_a<-subset(myspecies,order=='Caudata')
caudatelist_b<-caudatelist_a[! caudatelist_a %in% 'Ensatina eschscholtzii']
ensatina<-c('Ensatina eschscholtzii oregonensis','Ensatina eschscholtzii picta','Ensatina eschscholtzii platensis','Ensatina eschscholtzii xanthoptica','Ensatina eschscholtzii eschscholtzii','Ensatina eschscholtzii croceator','Ensatina eschscholtzii klauberi')
caudatelist<-c(caudatelist_b,ensatina)

# Load Lifelist
lifelist<-read.csv('MyLifeList.csv')
spseen<-nrow(lifelist)
sptotal<-length(caudatelist)

# Last New Species
last_sp<-tail(lifelist$species,n=1)
last_cn<-tail(lifelist$common_name,n=1)
last_lo<-tail(lifelist$location,n=1)
last_yr<-tail(lifelist$year,n=1)

#compare genus seen to total 
genusALL <- word(caudatelist,1)
genusSEEN <- word(lifelist$species,1)
genusALLtab<-cbind(names(table(genusALL)),table(genusALL))
genusSEENtab<-cbind(names(table(genusSEEN)),table(genusSEEN))
colnames(genusALLtab)<-colnames(genusSEENtab)<-c('genus','number')
merged<-merge(x= genusALLtab,y= genusSEENtab,by=0,all.x=T)
merged$number.y[is.na(merged$number.y)]<-0
#merged$number.y[8]<-1
merged$notseen<- as.numeric(merged$number.x)-as.numeric(merged$number.y)
merged$seen<-as.numeric(merged$number.y)
merged = merged %>% arrange(desc(notseen+seen)) %>% mutate(genus.x=factor(genus.x,levels=genus.x))
merged$id<-seq(1:nrow(merged))
data <- merged[,c(2,8,6,7)]

# Transform data into a tidy format (long format)
data <- data %>% gather(key = "observation", value="value", -c(1,2)) 

# Get the name and the y position of each label
label_data <- data %>% group_by(id, genus.x) %>% summarize(tot=sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# Find X/Y species seen
label_data<-merge(x=label_data,y=merged,by.x='genus.x',by.y='genus.x',all.x=T)
label_data$genusnamecolors<-c('black','darkturquoise','black','black','darkturquoise','black','black','black','black','black','deeppink','darkturquoise','darkturquoise','black','darkturquoise','black','darkturquoise','black','black','darkturquoise','darkturquoise','black','darkturquoise')


# prepare a data frame for base lines
base_data <- data %>% 
  summarize(start=min(id), end=max(id) ) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]



# Make the plot
p <- ggplot(data) +      
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=observation), stat="identity", alpha=1) +
  scale_fill_manual(values=c('darkturquoise','deeppink')) +
  
  ylim(-50,max(label_data$tot+40, na.rm=T)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id.x, y=tot+5, label=genus.x, hjust= hjust), color= label_data$genusnamecolors, fontface="bold",alpha=1, size=5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add X/Y seen labels under bars
  geom_text(data=label_data, aes(x=id.x, y=-12, label=paste(seen,'/',tot,sep=''), hjust= hjust), color= 'black', fontface="bold",alpha=1, size=3, angle= label_data$angle, inherit.aes = FALSE )

# accumulation curve
accumulation<-c(0,cumsum(table(lifelist$year)),max(cumsum(table(lifelist$year))))
newyears<-as.numeric(c(as.numeric(min(names(table(lifelist$year))))-1,names(table(lifelist$year)),format(Sys.Date(), "%Y")))
accurve.df<-data.frame(accumulation, newyears)

q<-ggplot(data=accurve.df,aes(x = newyears, y = accumulation, group = 1))+geom_step()+geom_point()+xlab('Year')+ylab('New Species Seen')+theme(axis.title.x = element_text(face='bold'),axis.title.y = element_text(face='bold'),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_rect(fill = "transparent",colour = NA),        plot.background = element_rect(fill = "transparent",colour = NA),axis.line = element_line(colour = "black"))


graph<-ggdraw()+theme(plot.background = element_rect(fill="white", color = NA))+draw_plot(q,0.05,0.77,0.3,0.2) + draw_plot(p,0.05,-0.05,1,1)
finalgraph<- graph + draw_text('Yet to See', x=0.8,y=0.94,size=22,color='darkturquoise',fontface='bold',hjust=0)+ draw_text('Seen', x=0.8,y=0.90,size=22,color='deeppink',fontface='bold',hjust=0)+ draw_text(paste(spseen,'of',sptotal), x=0.23,y=0.84,size=18,color='black',fontface='bold',hjust=0)+ 
  
  draw_text('Latest Addition', x=0.02,y=0.15,size=18,hjust=0,fontface='bold')+ 
  draw_text(last_sp, x=0.02,y=0.12,size=15,hjust=0,fontface='italic')+ 
  draw_text(last_cn, x=0.02,y=0.09,size=15,hjust=0)+ 
  draw_text(last_lo, x=0.02,y=0.06,size=15,hjust=0)+ 
  draw_text(last_yr, x=0.02,y=0.03,size=15,hjust=0)+ 
  
  draw_image("Ambystoma cingulatum.png",x=0.42, y=0.08, scale=0.1)+draw_image("Batrachoseps.png",x=0.4, y=0.2, scale=0.1)+draw_image("Pseudotriton.png",x=-0.3, y=-0.04, scale=0.1)+draw_image("Desmognathus_ocoee.png",x=0.22, y=0.35, scale=0.1)+draw_image("Gyrinophilus porphyriticus dunni.png",x=0.18, y=-0.4, scale=0.2)+draw_image("Necturus lodingi.png",x=0.36, y=-0.19, scale=0.15)+draw_image('Pseudobranchus striatus.png',x=-0.37, y=-0.13, scale=0.2)+draw_image("Plethodon shermani.png",x=0.1, y=0.4, scale=0.15)+draw_image("Stereochilus marginatus.png",x=-0.1, y=0.25, scale=0.1)+draw_image("Dicamptodon.png",x=0.29, y=-0.35, scale=0.15)+draw_image("Eurycea_lucifuga.png",x=0.3, y=0.26, scale=0.1)+draw_image("urspelerpes.png",x=0.01, y=0.28, scale=0.07)+draw_image("Siren.png",x=-0.04, y=-0.39, scale=0.15)+draw_image("Notophthalmus.png",x=-0.4, y=-0.22, scale=0.15)+draw_image("Amphiuma_means.png",x=-0.24, y=-0.27, scale=0.15)+draw_image("Taricha.png",x=-0.14, y=-0.34, scale=0.15)+draw_image("Rhyacotriton.png",x=0.05, y=-0.44, scale=0.1)+draw_image("Aneides.png",x=0.42, y=-0.02, scale=0.17)+draw_image("Ensatina.png",x=0.42, y=-0.11, scale=0.13)+draw_image("Cryptobranchus.png",x=-0.38, y=0.05, scale=0.22)+draw_image("Hemidactylium.png",x=-0.27, y=0.14, scale=0.1)+draw_image("phaeognathus.png",x=-0.27, y=0.2, scale=0.2)+draw_image("Hydromantes.png",x=0.34, y=-0.25, scale=0.1)

ggsave(finalgraph, file="SalamanderLifeList.png", width=10, height=10)

