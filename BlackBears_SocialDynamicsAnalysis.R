getwd()
bears <- read.csv("bears2.csv")
#replace NA with 0
bears[is.na(bears)] <- 0

#graph one: bar graph of broad positive and negative interactions between bears (disregarding directionality of + or -)-----

#make new df without any interactions with humans
bears_rmh <- bears[bears$other_individ !='4',]

#JUST SEPARATED INTO POSITIVE AND NEGATIVE INTERACTIONS
bears_sim <- bears_rmh
#add together focal individ and other to get unique number for interactions with eachother 
#Bear 1=c, 2=B, 3=A

library(dplyr)

bears_sim <- bears_rmh  %>%
  mutate(Group=case_when(
    focal._individ=="1" & other_individ=="2" ~ "B-C",
    focal._individ=="1" & other_individ=="3" ~ "A-C", 
    focal._individ=="2" & other_individ=="1" ~ "B-C",
    focal._individ=="2" & other_individ=="3" ~ "A-B",
    focal._individ=="3" & other_individ=="1" ~ "A-C",
    focal._individ=="3" & other_individ=="2" ~ "A-B"))


#add together positive interactions
bears_sim$Positive=bears_sim$Approach+bears_sim$Approach_by+bears_sim$Tongue.licking+bears_sim$Paw_Swat+
  bears_sim$Paw_Swat_R+bears_sim$Playing_I+bears_sim$Playing_Accept+bears_sim$Interest+bears_sim$Sniff+
  bears_sim$Pacing_A+bears_sim$Digging_together
#add together negative interactions
bears_sim$Negative=bears_sim$Chasing+bears_sim$Being_Chased+bears_sim$avoided+bears_sim$Avoid+
  bears_sim$Displaced_by+bears_sim$Displace+bears_sim$Food_T+bears_sim$Food_G+bears_sim$Treeing+
  bears_sim$follow_Treeing+bears_sim$AgressiveVocal_HHH+bears_sim$Fighting+bears_sim$Push+bears_sim$Push_by
#get rid of all the other columns
bears_sim <- bears_sim[,32:34]

#transform data for graphing
bears_sim <- pivot_longer(bears_sim, c(2:3),names_to = "Impact", values_to = "Number")
bears_simP <- subset.data.frame(bears_sim,bears_sim$Impact=="Positive")
bears_simN <- subset.data.frame(bears_sim,bears_sim$Impact=="Negative")

bears_simP <- bears_simP[,-2]
bears_simP <- aggregate(.~Group,data=bears_simP,FUN=sum)
bears_simP$Impact <- "Positive"

bears_simN <- bears_simN[,-2]
bears_simN <- aggregate(.~Group,data=bears_simN,FUN=sum)
bears_simN$Impact <- "Negative"

bears_sim <- rbind(bears_simP,bears_simN)

#try to make a graph
library(ggplot2)
library(tidyr)

ggplot(bears_sim,aes(x=Group, y=Number,fill=Impact))+
  geom_bar(position="dodge",stat="identity")+
  ggtitle("Total Observed Positive and Negative interactions between bears")+
  labs(x="Bears",y="Number of Observed Events")

#bar graph MINUS PACING (stereotypy)-----
library(dplyr)
library(ggplot2)
library(tidyr)

bears_rmh2 <- bears[bears$other_individ !='4',]
bears_rmstpy <- bears_rmh2

bears_rmstpy <- bears_rmh2  %>%
  mutate(Group=case_when(
    focal._individ=="1" & other_individ=="2" ~ "B-C",
    focal._individ=="1" & other_individ=="3" ~ "A-C", 
    focal._individ=="2" & other_individ=="1" ~ "B-C",
    focal._individ=="2" & other_individ=="3" ~ "A-B",
    focal._individ=="3" & other_individ=="1" ~ "A-C",
    focal._individ=="3" & other_individ=="2" ~ "A-B"))

#add together positive interactions
bears_rmstpy$Positive=bears_rmstpy$Approach+bears_rmstpy$Approach_by+bears_rmstpy$Tongue.licking+bears_rmstpy$Paw_Swat+
  bears_rmstpy$Paw_Swat_R+bears_rmstpy$Playing_I+bears_rmstpy$Playing_Accept+bears_rmstpy$Interest+bears_rmstpy$Sniff+
  bears_rmstpy$Digging_together
#add together negative interactions
bears_rmstpy$Negative=bears_rmstpy$Chasing+bears_rmstpy$Being_Chased+bears_rmstpy$avoided+bears_rmstpy$Avoid+
  bears_rmstpy$Displaced_by+bears_rmstpy$Displace+bears_rmstpy$Food_T+bears_rmstpy$Food_G+bears_rmstpy$Treeing+
  bears_rmstpy$follow_Treeing+bears_rmstpy$AgressiveVocal_HHH+bears_rmstpy$Fighting+bears_rmstpy$Push+bears_rmstpy$Push_by

bears_rmstpy <- bears_rmstpy[,32:34]
bears_rmstpy <- pivot_longer(bears_rmstpy, c(2:3),names_to = "Impact", values_to = "Number")
bears_rmstpyP <- subset.data.frame(bears_rmstpy,bears_rmstpy$Impact=="Positive")
bears_rmstpyN <- subset.data.frame(bears_rmstpy,bears_rmstpy$Impact=="Negative")
bears_rmstpyP <- bears_rmstpyP[,-2]
bears_rmstpyP <- aggregate(.~Group,data=bears_rmstpyP,FUN=sum)
bears_rmstpyP$Impact <- "Positive"
bears_rmstpyN <- bears_rmstpyN[,-2]
bears_rmstpyN <- aggregate(.~Group,data=bears_rmstpyN,FUN=sum)
bears_rmstpyN$Impact <- "Negative"
bears_rmstpy <- rbind(bears_rmstpyP,bears_rmstpyN)

ggplot(bears_rmstpy,aes(x=Group, y=Number,fill=Impact))+
  geom_bar(position="dodge",stat="identity")+
  ggtitle("Total Observed Positive and Negative interactions between bears without stereotypy")+
  labs(x="Bears",y="Number of Observed Events")


#Bar graph: just poisitve with and without stereotypy-----

library(dplyr)
library(ggplot2)
library(tidyr)

bears_rmh2 <- bears[bears$other_individ !='4',]
bears_poscomp <- bears_rmh2

bears_poscomp <- bears_rmh2  %>%
  mutate(Group=case_when(
    focal._individ=="1" & other_individ=="2" ~ "B-C",
    focal._individ=="1" & other_individ=="3" ~ "A-C", 
    focal._individ=="2" & other_individ=="1" ~ "B-C",
    focal._individ=="2" & other_individ=="3" ~ "A-B",
    focal._individ=="3" & other_individ=="1" ~ "A-C",
    focal._individ=="3" & other_individ=="2" ~ "A-B"))


#positive interactions NO STEREOTYPY
bears_poscomp$PositiveNOP=bears_poscomp$Approach+bears_poscomp$Approach_by+bears_poscomp$Tongue.licking+bears_poscomp$Paw_Swat+
  bears_poscomp$Paw_Swat_R+bears_poscomp$Playing_I+bears_poscomp$Playing_Accept+bears_poscomp$Interest+bears_poscomp$Sniff+
  +bears_poscomp$Digging_together

#add together all positive
bears_poscomp$Positive=bears_poscomp$Approach+bears_poscomp$Approach_by+bears_poscomp$Tongue.licking+bears_poscomp$Paw_Swat+
  bears_poscomp$Paw_Swat_R+bears_poscomp$Playing_I+bears_poscomp$Playing_Accept+bears_poscomp$Interest+bears_poscomp$Sniff+
  bears_poscomp$Digging_together+bears_poscomp$Pacing_A

bears_poscomp <- bears_poscomp[,32:34]
bears_poscomp <- pivot_longer(bears_poscomp, c(2:3),names_to = "Impact", values_to = "Number")
bears_poscompP <- subset.data.frame(bears_poscomp,bears_poscomp$Impact=="Positive")
bears_poscompPNS <- subset.data.frame(bears_poscomp,bears_poscomp$Impact=="PositiveNOP")
bears_poscompP <- bears_poscompP[,-2]
bears_poscompP <- aggregate(.~Group,data=bears_poscompP,FUN=sum)
bears_poscompP$Impact <- "Positive"
bears_poscompPNS <- bears_poscompPNS[,-2]
bears_poscompPNS <- aggregate(.~Group,data=bears_poscompPNS,FUN=sum)
bears_poscompPNS$Impact <- "Positive, No Stereotypy"
bears_poscomp <- rbind(bears_poscompP,bears_poscompPNS)

ggplot(bears_poscomp,aes(x=Group, y=Number,fill=Impact))+
  geom_bar(position="dodge",stat="identity")+
  ggtitle("Total Observed Positive interactions between bears with and without stereotypy")+
  labs(x="Bears",y="Number of Observed Events")

#graph just of positive without sterotypic pacing
ggplot(bears_poscompPNS,aes(x=Group, y=Number,fill=Group))+
  geom_bar(stat="identity", fill="lightblue")+
  ggtitle("Total Observed Positive interactions between bears Excluding Affiliative Pacing")+
  labs(x="Bears",y="Number of Observed Events")










#graph two: pie chart about interaction with humans------

#make dataframe with only interactions with humans
bears_h <- subset.data.frame(bears,bears$other_individ=="4")
bears_h <- bears_h[,1:13]
bears_h = select(bears_h, -1:-2,-4)
bears_h = select(bears_h, -3:-9)

#combine approach and interest into interaction column
bears_h$Interaction=bears_h$Approach+bears_h$Interest
bears_h = select(bears_h, -2:-3)

#change numvers into letters
bears_h <- bears_h  %>%
  mutate(Bear=case_when(
    focal._individ=="1"  ~ "Bear C",
    focal._individ=="2"  ~ "Bear B", 
    focal._individ=="3"  ~ "Bear A"))
bears_h = select(bears_h, -1)

#simplify and sum
library(dplyr)

bears_h <- bears_h %>%
  group_by(Bear) %>%
  summarise(totalinteraction = sum(Interaction))

#pie chart ...FIX TITLE MAYBE BUT GOOD ENOUGH FOR NOW
bears_h <- bears_h %>% 
  arrange(desc(Bear)) %>%
  mutate(prop = totalinteraction / sum(bears_h$totalinteraction) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(bears_h, aes(x="", y=prop, fill=Bear)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  
  geom_text(aes(y = ypos, label = Bear), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")+
  ggtitle('Percent of Human Interaction Engaged by Each Bear')

#stats about pie chart: t-test
bears_hstat <- bears
bears_hstat <- subset.data.frame(bears,bears$other_individ=="4")
bears_hstat <- bears_hstat[,1:13]
bears_hstat = select(bears_hstat, -1:-2,-4)
bears_hstat = select(bears_hstat, -3:-9)

#combine approach and interest into interaction column
bears_hstat$Interaction=bears_hstat$Approach+bears_hstat$Interest
bears_hstat = select(bears_hstat, -2:-3)
bears_hstat <- bears_hstat  %>%
  mutate(Bear=case_when(
    focal._individ=="1"  ~ "Bear C",
    focal._individ=="2"  ~ "Bear B", 
    focal._individ=="3"  ~ "Bear A"))
bears_hstat = select(bears_hstat, -1)

#try anova now
Humananova <- aov(Interaction~Bear,data=bears_hstat)
summary(Humananova)
TukeyHSD(Humananova)




#graph 3: network analysis...oh boy---- 
#what we need: a df with 2 columns, Give and Recieve, only for + or - behaviors with bears in them

#FIRST: making it simple: basic affiliative groupings, get rid of directional


#what if we separate into different df for each column, repeat the rows based on



#isolate directional variables, use those to make network analysis
#start with negative
bears_negative <- bears_rmh
#remove all affiliative columns and anything not directional
bears_negative = select(bears_negative, -5:-15)
bears_negative = select(bears_negative, -7)
bears_negative = select(bears_negative, -19)

#combine colums into Give and Recieve
bears_negative$Give=bears_negative$Chasing+bears_negative$avoided+bears_negative$Displace+bears_negative$Food_T+bears_negative$Treeing+
  bears_negative$follow_Treeing+bears_negative$AgressiveVocal_HHH+bears_negative$Push+bears_negative$Fighting

bears_negative$Recieve=bears_negative$Being_Chased+bears_negative$Avoid+bears_negative$Displaced_by+bears_negative$Food_G+bears_negative$Push_by

bears_negative = select(bears_negative, -5:-18)
bears_negative = select(bears_negative, -1:-2)

#to make final dataframe, separate into different df to repeat rows, and then recombine
#just giving
bears_non_giv <- bears_negative
bears_non_giv=select(bears_non_giv,-4)
#remove any rows where no giving has occurred
bears_non_giv<-bears_non_giv[!(bears_non_giv$Give=="0"),]

#repeat rows
bears_non_giv <- bears_non_giv |> uncount(Give)
#change column names
colnames(bears_non_giv) <- c("Give", "Receive")

#just recieving
bears_non_rec <- bears_negative
bears_non_rec=select(bears_non_rec,-3)
#remove any rows where no giving has occurred
bears_non_rec<-bears_non_rec[!(bears_non_rec$Recieve=="0"),]

#repeat rows
bears_non_rec <- bears_non_rec |> uncount(Recieve)
#IMPORTANT: CHANGE COLUMN ORDER (because receiving, not giving)
bears_non_rec <- bears_non_rec[,c(2,1)]
#change column names
colnames(bears_non_rec) <- c("Give", "Receive")

#Now combine df
bears_nn <- rbind(bears_non_giv,bears_non_rec)
#replace numbers w letters
bears_nn2 <- bears_nn
bears_nn2$Give[bears_nn2$Give=="1"]<-"C"
bears_nn2$Give[bears_nn2$Give=="2"]<-"B"
bears_nn2$Give[bears_nn2$Give=="3"]<-"A"
bears_nn2$Receive[bears_nn2$Receive=="1"]<-"C"
bears_nn2$Receive[bears_nn2$Receive=="2"]<-"B"
bears_nn2$Receive[bears_nn2$Receive=="3"]<-"A"
#now to network analysis
install.packages("igraph")

library('igraph')



#unweighted network

ig=graph_from_data_frame(bears_nn2, directed=T)
plot(ig)


#weighted
E(ig)$weight=0.5 #make all edge weights = 0.5
ig2=simplify(ig, remove.multiple=T, edge.attr.comb=list(weight=sum))
plot(ig2, edge.width=E(ig2)$weight)
plot(ig2, edge.width=E(ig2)$weight/4, edge.curved=T, layout=layout_in_circle(ig2))

plot(ig2, edge.width=E(ig2)$weight/4, edge.curved=T, layout=layout_in_circle(ig2))
#SUCCESSS!!!!! Dont know how to make super pretty

#now do positive associations-----

bears_positive <- bears_rmh
#remove all affiliative columns and anything not directional
bears_positive = select(bears_positive, -1:-2)
bears_positive = select(bears_positive, -9)
bears_positive = select(bears_positive, -13:-27)
#removing TL, Pacing and digging ... not directional
bears_positive = select(bears_positive, -5,-12,-13)


#combine colums into Give and Recieve
bears_positive$Give=bears_positive$Approach+bears_positive$Paw_Swat+bears_positive$Playing_I+bears_positive$Interest+bears_positive$Sniff
bears_positive$Recieve=bears_positive$Approach_by+bears_positive$Paw_Swat_R+bears_positive$Playing_Accept

bears_positive = select(bears_positive, -3:-10)


#to make final dataframe, separate into different df to repeat rows, and then recombine
#just giving
bears_pos_giv <- bears_positive
bears_pos_giv=select(bears_pos_giv,-4)
#remove any rows where no giving has occurred
bears_pos_giv<-bears_pos_giv[!(bears_pos_giv$Give=="0"),]

#repeat rows
bears_pos_giv <- bears_pos_giv |> uncount(Give)
#change column names
colnames(bears_pos_giv) <- c("Give", "Receive")

#just recieving
bears_pos_rec <- bears_positive
bears_pos_rec=select(bears_pos_rec,-3)
#remove any rows where no giving has occurred
bears_pos_rec<-bears_pos_rec[!(bears_pos_rec$Recieve=="0"),]

#repeat rows
bears_pos_rec <- bears_pos_rec |> uncount(Recieve)
#IMPORTANT: CHANGE COLUMN ORDER (because receiving, not giving)
bears_pos_rec <- bears_pos_rec[,c(2,1)]
#change column names
colnames(bears_pos_rec) <- c("Give", "Receive")

#Now combine df
bears_np <- rbind(bears_pos_giv,bears_pos_rec)
#replace numbers w letters
bears_np2 <- bears_np
bears_np2$Give[bears_np2$Give=="1"]<-"C"
bears_np2$Give[bears_np2$Give=="2"]<-"B"
bears_np2$Give[bears_np2$Give=="3"]<-"A"
bears_np2$Receive[bears_np2$Receive=="1"]<-"C"
bears_np2$Receive[bears_np2$Receive=="2"]<-"B"
bears_np2$Receive[bears_np2$Receive=="3"]<-"A"

#unweighted network
igP=graph_from_data_frame(bears_np2, directed=T)
plot(igP)

#weighted
E(igP)$weight=0.5 #make all edge weights = 0.25
ig2P=simplify(igP, remove.multiple=T, edge.attr.comb=list(weight=sum))
plot(ig2P, edge.width=E(ig2P)$weight)
plot(ig2P, edge.width=E(ig2P)$weight/4, edge.curved=T, layout=layout_in_circle(ig2P))

plot(ig2P, edge.width=E(ig2P)$weight/4, edge.curved=T, layout=layout_in_circle(ig2P))
#SUCCESSS!!!!! Dont know how to make super pretty


#NETWORK WITH PACING, TL and Digging WEIGHTED AS GIVE,-----

bears_positiveGive <- bears_rmh
#remove all affiliative columns and anything not directional
bears_positiveGive = select(bears_positiveGive, -1:-2)
bears_positiveGive = select(bears_positiveGive, -9)
bears_positiveGive = select(bears_positiveGive, -13:-27)


#combine colums into Give and Recieve
bears_positiveGive$Give=bears_positiveGive$Approach+bears_positiveGive$Paw_Swat+bears_positiveGive$Playing_I+bears_positiveGive$Interest+bears_positiveGive$Sniff+
  bears_positiveGive$Pacing_A+bears_positiveGive$Digging_together+bears_positiveGive$Tongue.licking
bears_positiveGive$Recieve=bears_positiveGive$Approach_by+bears_positiveGive$Paw_Swat_R+bears_positiveGive$Playing_Accept

bears_positiveGive = select(bears_positiveGive, -3:-13)


#to make final dataframe, separate into different df to repeat rows, and then recombine
#just giving
bears_pos_giv2 <- bears_positiveGive
bears_pos_giv2=select(bears_pos_giv2,-4)
#remove any rows where no giving has occurred
bears_pos_giv2<-bears_pos_giv2[!(bears_pos_giv2$Give=="0"),]

#repeat rows
bears_pos_giv2 <- bears_pos_giv2 |> uncount(Give)
#change column names
colnames(bears_pos_giv2) <- c("Give", "Receive")

#just recieving
bears_pos_rec2 <- bears_positiveGive
bears_pos_rec2=select(bears_pos_rec2,-3)
#remove any rows where no giving has occurred
bears_pos_rec2<-bears_pos_rec2[!(bears_pos_rec2$Recieve=="0"),]

#repeat rows
bears_pos_rec2 <- bears_pos_rec2 |> uncount(Recieve)
#IMPORTANT: CHANGE COLUMN ORDER (because receiving, not giving)
bears_pos_rec2 <- bears_pos_rec2[,c(2,1)]
#change column names
colnames(bears_pos_rec2) <- c("Give", "Receive")

#Now combine df
bears_np3 <- rbind(bears_pos_giv2,bears_pos_rec2)
#replace numbers w letters
bears_np4 <- bears_np3
bears_np4$Give[bears_np4$Give=="1"]<-"C"
bears_np4$Give[bears_np4$Give=="2"]<-"B"
bears_np4$Give[bears_np4$Give=="3"]<-"A"
bears_np4$Receive[bears_np4$Receive=="1"]<-"C"
bears_np4$Receive[bears_np4$Receive=="2"]<-"B"
bears_np4$Receive[bears_np4$Receive=="3"]<-"A"

#unweighted network
igP_2=graph_from_data_frame(bears_np4, directed=T)
plot(igP_2)

#weighted
E(igP_2)$weight=0.25 #make all edge weights = 0.25
ig2P_2=simplify(igP_2, remove.multiple=T, edge.attr.comb=list(weight=sum))
plot(ig2P_2, edge.width=E(ig2P_2)$weight)
plot(ig2P_2, edge.width=E(ig2P_2)$weight/4, edge.curved=T, layout=layout_in_circle(ig2P_2))

plot(ig2P_2, edge.width=E(ig2P_2)$weight/4, edge.curved=T, layout=layout_in_circle(ig2P_2))
#SUCCESSS!!!!! Dont know how to make super pretty









#try stats now-----

#what stats do I want...differences in positive and negative interactions between bears

#Create summary df


#Separate into individual groupings (aka 1->2, 2->1, etc)------
B1 <- subset.data.frame(bears,bears$focal._individ=="1")
B2 <- subset.data.frame(bears,bears$focal._individ=="2")
B3 <- subset.data.frame(bears,bears$focal._individ=="3")

#seaparating df
#1->2
B12 <- subset.data.frame(B1,B1$other_individ=="2")
B12_sum <- summarise_all(B12, ~if(is.numeric(.)) sum(.) else "Total")[,5:15]
B12_sum$individ="1to2"
#1->3
B13 <- subset.data.frame(B1,B1$other_individ=="3")
B13_sum <- summarise_all(B13, ~if(is.numeric(.)) sum(.) else "Total")[,5:15]
B13_sum$individ="1to3"
#1->4
B14 <- subset.data.frame(B1,B1$other_individ=="4")
B14_sum <- summarise_all(B14, ~if(is.numeric(.)) sum(.) else "Total")[,5:15]
B14_sum$individ="1to4"

#2->1
B21 <- subset.data.frame(B2,B2$other_individ=="1")
B21_sum <- summarise_all(B21, ~if(is.numeric(.)) sum(.) else "Total")[,5:15]
B21_sum$individ="2to1"
#2->3
B23 <- subset.data.frame(B2,B2$other_individ=="3")
B23_sum <- summarise_all(B23, ~if(is.numeric(.)) sum(.) else "Total")[,5:15]
B23_sum$individ="2to3"
#2->4
B24 <- subset.data.frame(B2,B2$other_individ=="4")
B24_sum <- summarise_all(B24, ~if(is.numeric(.)) sum(.) else "Total")[,5:15]
B24_sum$individ="2to4"

#3->1
B31 <- subset.data.frame(B3,B3$other_individ=="1")
B31_sum <- summarise_all(B31, ~if(is.numeric(.)) sum(.) else "Total")[,5:15]
B31_sum$individ="3to1"
#3->2
B32 <- subset.data.frame(B3,B3$other_individ=="2")
B32_sum <- summarise_all(B32, ~if(is.numeric(.)) sum(.) else "Total")[,5:15]
B32_sum$individ="3to2"
#3->4
B34 <- subset.data.frame(B3,B3$other_individ=="4")
B34_sum <- summarise_all(B34, ~if(is.numeric(.)) sum(.) else "Total")[,5:15]
B34_sum$individ="3to4"

bsums <- rbind(B12_sum,B13_sum,B14_sum,B21_sum,B23_sum,B24_sum,B31_sum,B32_sum,B34_sum)


###bsums FAIL: take two, dont summarise df, just change so there is a individ column----
bears_sum <- bears  %>%
  mutate(Group=case_when(
    focal._individ=="1" & other_individ=="2" ~ "B-C",
    focal._individ=="1" & other_individ=="3" ~ "A-C",
    focal._individ=="1" & other_individ=="4" ~ "C-H", 
    focal._individ=="2" & other_individ=="1" ~ "B-C",
    focal._individ=="2" & other_individ=="3" ~ "A-B",
    focal._individ=="2" & other_individ=="4" ~ "B-H", 
    focal._individ=="3" & other_individ=="1" ~ "A-C",
    focal._individ=="3" & other_individ=="2" ~ "A-B",
    focal._individ=="3" & other_individ=="4" ~ "A-H" ))
bears_sum = select(bears_sum, -1:-4)

#Anova Tests-----
#apprach
approachanova <- aov(Approach~Group,data=bears_sum)
summary(approachanova)
TukeyHSD(approachanova)
#success! repeat for all columns

#appraoch by
approachbyanova <- aov(Approach_by~Group,data=bears_sum)
summary(approachbyanova)
TukeyHSD(approachbyanova)

#Tongue.licking
TLanova <- aov(Tongue.licking~Group,data=bears_sum)
summary(TLanova)
TukeyHSD(TLanova)

#Paw_Swat
PSanova <- aov(Paw_Swat~Group,data=bears_sum)
summary(PSanova)
TukeyHSD(PSanova)

#Paw_Swat_R
PSRanova <- aov(Paw_Swat_R~Group,data=bears_sum)
summary(PSRanova)
TukeyHSD(PSRanova)

#Playing_I
IPanova <- aov(Playing_I~Group,data=bears_sum)
summary(IPanova)
TukeyHSD(IPanova)

#IPfail
IPFanova <- aov(IPfail~Group,data=bears_sum)
summary(IPFanova)
TukeyHSD(IPFanova)

#Playing_Accept
PAanova <- aov(Playing_Accept~Group,data=bears_sum)
summary(PAanova)
TukeyHSD(PAanova)

#Interest
Ianova <- aov(Interest~Group,data=bears_sum)
summary(Ianova)
TukeyHSD(Ianova)

#Sniff
Sanova <- aov(Sniff~Group,data=bears_sum)
summary(Sanova)
TukeyHSD(Sanova)

#Pacing_A
Panova <- aov(Pacing_A~Group,data=bears_sum)
summary(Panova)
TukeyHSD(Panova)

#Chasing
Chanova <- aov(Chasing~Group,data=bears_sum)
summary(Chanova)
TukeyHSD(Chanova)

#Being_Chased
BCanova <- aov(Being_Chased~Group,data=bears_sum)
summary(BCanova)
TukeyHSD(BCanova)

#avoided
Aedanova <- aov(avoided~Group,data=bears_sum)
summary(Aedanova)
TukeyHSD(Aedanova)

#Avoid
Aanova <- aov(Avoid~Group,data=bears_sum)
summary(Aanova)
TukeyHSD(Aanova)

#Displaced_by
Dbanova <- aov(Displaced_by~Group,data=bears_sum)
summary(Dbanova)
TukeyHSD(Dbanova)

#Displace
Danova <- aov(Displace~Group,data=bears_sum)
summary(Danova)
TukeyHSD(Danova)

#Food_T
FTanova <- aov(Food_T~Group,data=bears_sum)
summary(FTanova)
TukeyHSD(FTanova)

#Food_G
FGanova <- aov(Food_G~Group,data=bears_sum)
summary(FGanova)
TukeyHSD(FGanova)

#Treeing
Tanova <- aov(Treeing~Group,data=bears_sum)
summary(Tanova)
TukeyHSD(Tanova)

#follow_Treeing
Tfanova <- aov(follow_Treeing~Group,data=bears_sum)
summary(Tfanova)
TukeyHSD(Tfanova)

#Agressivevocal_HHH
Vanova <- aov(AggressiveVocal_HHH~Group,data=bears_sum)
summary(Vanova)
TukeyHSD(Vanova)

#Fighting
Fanova <- aov(Fighting~Group,data=bears_sum)
summary(Fanova)
TukeyHSD(Fanova)

#Push
Pushanova <- aov(Push~Group,data=bears_sum)
summary(Pushanova)
TukeyHSD(Pushanova)

#Push_by
Pushbanova <- aov(Push_by~Group,data=bears_sum)
summary(Pushbanova)
TukeyHSD(Pushbanova)

#Digging_together
Danova <- aov(Digging_together~Group,data=bears_sum)
summary(Danova)
TukeyHSD(Danova)

#Anova using positive/negative interactions df----

bears_posneg <- bears

bears_posneg <- bears  %>%
  mutate(Group=case_when(
    focal._individ=="1" & other_individ=="2" ~ "B-C",
    focal._individ=="1" & other_individ=="3" ~ "A-C",
    focal._individ=="1" & other_individ=="4" ~ "C-H", 
    focal._individ=="2" & other_individ=="1" ~ "B-C",
    focal._individ=="2" & other_individ=="3" ~ "A-B",
    focal._individ=="2" & other_individ=="4" ~ "B-H", 
    focal._individ=="3" & other_individ=="1" ~ "A-C",
    focal._individ=="3" & other_individ=="2" ~ "A-B",
    focal._individ=="3" & other_individ=="4" ~ "A-H" ))
bears_posneg = select(bears_posneg, -1:-4)

bears_posneg$Positive=bears_posneg$Approach+bears_posneg$Approach_by+bears_posneg$Tongue.licking+bears_posneg$Paw_Swat+
  bears_posneg$Paw_Swat_R+bears_posneg$Playing_I+bears_posneg$Playing_Accept+bears_posneg$Interest+bears_posneg$Sniff+
  bears_posneg$Pacing_A+bears_posneg$Digging_together
#minus pacing
bears_posneg$PositiveNOP=bears_posneg$Approach+bears_posneg$Approach_by+bears_posneg$Tongue.licking+bears_posneg$Paw_Swat+
  bears_posneg$Paw_Swat_R+bears_posneg$Playing_I+bears_posneg$Playing_Accept+bears_posneg$Interest+bears_posneg$Sniff+
  +bears_posneg$Digging_together
#minus pacing and interest
bears_posneg$PositiveNOPI=bears_posneg$Approach+bears_posneg$Approach_by+bears_posneg$Tongue.licking+bears_posneg$Paw_Swat+
  bears_posneg$Paw_Swat_R+bears_posneg$Playing_I+bears_posneg$Playing_Accept+bears_posneg$Sniff+
  +bears_posneg$Digging_together

bears_posneg$Negative=bears_posneg$Chasing+bears_posneg$Being_Chased+bears_posneg$avoided+bears_posneg$Avoid+
  bears_posneg$Displaced_by+bears_posneg$Displace+bears_posneg$Food_T+bears_posneg$Food_G+bears_posneg$Treeing+
  bears_posneg$follow_Treeing+bears_posneg$AgressiveVocal_HHH+bears_posneg$Fighting+bears_posneg$Push+bears_posneg$Push_by

#anova

POSanova <- aov(Positive~Group,data=bears_posneg)
summary(POSanova)
TukeyHSD(POSanova)

POSnoPanova <- aov(PositiveNOP~Group,data=bears_posneg)
summary(POSnoPanova)
TukeyHSD(POSnoPanova)

POSnoPIanova <- aov(PositiveNOPI~Group,data=bears_posneg)
summary(POSnoPIanova)
TukeyHSD(POSnoPIanova)

NEGanova <- aov(Negative~Group,data=bears_posneg)
summary(NEGanova)
TukeyHSD(NEGanova)


#To see difference: bsums_consolidated2
bsums_consolidated <- bears_sum
bsums_consolidated <- aggregate(.~Group,data=bsums_consolidated,FUN=sum)

bsums_consolidated$Positive=bsums_consolidated$Approach+bsums_consolidated$Approach_by+bsums_consolidated$Tongue.licking+bsums_consolidated$Paw_Swat+
  bsums_consolidated$Paw_Swat_R+bsums_consolidated$Playing_I+bsums_consolidated$Playing_Accept+bsums_consolidated$Interest+bsums_consolidated$Sniff+
  bsums_consolidated$Pacing_A+bsums_consolidated$Digging_together

bsums_consolidated$PositiveNOP=bsums_consolidated$Approach+bsums_consolidated$Approach_by+bsums_consolidated$Tongue.licking+bsums_consolidated$Paw_Swat+
  bsums_consolidated$Paw_Swat_R+bsums_consolidated$Playing_I+bsums_consolidated$Playing_Accept+bsums_consolidated$Interest+bsums_consolidated$Sniff+bsums_consolidated$Digging_together

bsums_consolidated$Negative=bsums_consolidated$Chasing+bsums_consolidated$Being_Chased+bsums_consolidated$avoided+bsums_consolidated$Avoid+
  bsums_consolidated$Displaced_by+bsums_consolidated$Displace+bsums_consolidated$Food_T+bsums_consolidated$Food_G+bsums_consolidated$Treeing+
  bsums_consolidated$follow_Treeing+bsums_consolidated$AgressiveVocal_HHH+bsums_consolidated$Fighting+bsums_consolidated$Push+bsums_consolidated$Push_by

bsums_consolidated2 = select(bsums_consolidated, -2:-28)
