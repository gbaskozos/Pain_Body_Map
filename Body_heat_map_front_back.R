library(grImport)
library(XML)
library(gridExtra)
library(RColorBrewer)
library(scales)
library(gridGraphics)

#function to change the rgb color of the xml paths
changeColor<-function(bodypart,color){
        node<-xpathSApply(doc, paste("//path[@id='",bodypart,"']/context/rgb",sep=""))[[1]]
        rgbCol<-col2rgb(color)
        xmlAttrs(node)["r"]=rgbCol[1]/255
        xmlAttrs(node)["g"]=rgbCol[2]/255
        xmlAttrs(node)["b"]=rgbCol[3]/255
}

#colfunc<-colorRampPalette(c("red","royalblue"))

colfunc<-colorRampPalette(c("red","royalblue"))


#read the xml image
doc<-xmlParse("Human_body_front_and_back.ps.xml")

#these are the different parts you can change
bodyparts<-c("Face","Chest","Arm-left","Arm-right","Hand-left","Hand-right",
            "Shoulder-left","Shoulder-right","Neck","Back", "Abdomen","Hip-left","Hip-right", "Leg-left", "Leg-right", "Feet-left", "Feet-right", "Buttock-left", "Buttock-right")


### LGI1 vs CASPR2

#Read percentages
LGI1_CASPR2 <- read.csv("LGI1_CASPR2.csv", row.name=1)

colour_data <- data.frame(Part = bodyparts, Percentages = c(LGI1_CASPR2["Face",]$LGI1, LGI1_CASPR2["Chest",]$LGI1, LGI1_CASPR2["Arms",]$LGI1, LGI1_CASPR2["Arms",]$LGI1, LGI1_CASPR2["Hands",]$LGI1, LGI1_CASPR2["Hands",]$LGI1, LGI1_CASPR2["Shoulder",]$LGI1, LGI1_CASPR2["Shoulder",]$LGI1, LGI1_CASPR2["Neck",]$LGI1, LGI1_CASPR2["Back",]$LGI1, LGI1_CASPR2["Abdomen",]$LGI1, LGI1_CASPR2["Hips",]$LGI1, LGI1_CASPR2["Hips",]$LGI1, LGI1_CASPR2["Legs",]$LGI1, LGI1_CASPR2["Legs",]$LGI1, LGI1_CASPR2["Feet",]$LGI1, LGI1_CASPR2["Feet",]$LGI1, LGI1_CASPR2["Buttocks",]$LGI1, LGI1_CASPR2["Buttocks",]$LGI1, LGI1_CASPR2["Face",]$CASPR2, LGI1_CASPR2["Chest",]$CASPR2, LGI1_CASPR2["Arms",]$CASPR2, LGI1_CASPR2["Arms",]$CASPR2, LGI1_CASPR2["Hands",]$CASPR2, LGI1_CASPR2["Hands",]$CASPR2, LGI1_CASPR2["Shoulder",]$CASPR2, LGI1_CASPR2["Shoulder",]$CASPR2, LGI1_CASPR2["Neck",]$CASPR2, LGI1_CASPR2["Back",]$CASPR2, LGI1_CASPR2["Abdomen",]$CASPR2, LGI1_CASPR2["Hips",]$CASPR2, LGI1_CASPR2["Hips",]$CASPR2, LGI1_CASPR2["Legs",]$CASPR2, LGI1_CASPR2["Legs",]$CASPR2, LGI1_CASPR2["Feet",]$CASPR2, LGI1_CASPR2["Feet",]$CASPR2, LGI1_CASPR2["Buttocks",]$CASPR2, LGI1_CASPR2["Buttocks",]$CASPR2), Condition = c(rep("LGI1", length(bodyparts)), rep("CASPR2", length(bodyparts))))

colour_data$order <- rank(-c(colour_data$Percentages))

#color the bodyparts for LGI1
mapply(function(x,y){changeColor(x,y)},bodyparts,colfunc(nrow(colour_data)+1)[colour_data[colour_data$Condition=="LGI1",]$order])
#load the XML as a picture
body_LGI1<-readPicture(saveXML(doc))

#color the bodyparts for CASPR2
mapply(function(x,y){changeColor(x,y)},bodyparts,colfunc(nrow(colour_data)+1)[colour_data[colour_data$Condition=="CASPR2",]$order])
#load the XML as a picture
body_CASPR2<-readPicture(saveXML(doc))

#Create colour key
show_col(colfunc(nrow(colour_data)+1)[(nrow(colour_data)+1):1], labels = FALSE, borders = FALSE, cex_label = 1, ncol = nrow(colour_data)+1)
grid.echo()
key <- grid.grab()

#Layout of figure
lay <- rbind(c(1,1,2,2),
	     c(1,1,2,2),
	     c(3,3,3,3))


#plot it, you can change titles in top = "title" and layout by changing the order
pdf("LGI1_CASPR2.pdf")
grid.arrange(arrangeGrob(pictureGrob(body_LGI1), top = 'LGI1'), arrangeGrob(pictureGrob(body_CASPR2), top = 'CASPR2'), arrangeGrob(key), top = "Pain heatmap LGI2 vs CASPR2", layout_matrix=lay)
dev.off()

### No Neup vs Neup

#Read percentages
NoNeup_neup <- read.csv("NO_noNEUP.csv", row.name=1)

colour_data <- data.frame(Part = bodyparts, Percentages = c(NoNeup_neup["Face",]$Unlikely.NP, NoNeup_neup["Chest",]$Unlikely.NP, NoNeup_neup["Arms",]$Unlikely.NP, NoNeup_neup["Arms",]$Unlikely.NP, NoNeup_neup["Hands",]$Unlikely.NP, NoNeup_neup["Hands",]$Unlikely.NP, NoNeup_neup["Shoulder",]$Unlikely.NP, NoNeup_neup["Shoulder",]$Unlikely.NP, NoNeup_neup["Neck",]$Unlikely.NP, NoNeup_neup["Back",]$Unlikely.NP, NoNeup_neup["Abdomen",]$Unlikely.NP, NoNeup_neup["Hips",]$Unlikely.NP, NoNeup_neup["Hips",]$Unlikely.NP, NoNeup_neup["Legs",]$Unlikely.NP, NoNeup_neup["Legs",]$Unlikely.NP, NoNeup_neup["Feet",]$Unlikely.NP, NoNeup_neup["Feet",]$Unlikely.NP, NoNeup_neup["Buttocks",]$Unlikely.NP, NoNeup_neup["Buttocks",]$Unlikely.NP, NoNeup_neup["Face",]$NP, NoNeup_neup["Chest",]$NP, NoNeup_neup["Arms",]$NP, NoNeup_neup["Arms",]$NP, NoNeup_neup["Hands",]$NP, NoNeup_neup["Hands",]$NP, NoNeup_neup["Shoulder",]$NP, NoNeup_neup["Shoulder",]$NP, NoNeup_neup["Neck",]$NP, NoNeup_neup["Back",]$NP, NoNeup_neup["Abdomen",]$NP, NoNeup_neup["Hips",]$NP, NoNeup_neup["Hips",]$NP, NoNeup_neup["Legs",]$NP, NoNeup_neup["Legs",]$NP, NoNeup_neup["Feet",]$NP, NoNeup_neup["Feet",]$NP, NoNeup_neup["Buttocks",]$NP, NoNeup_neup["Buttocks",]$NP), Condition = c(rep("Unlikely.NP", length(bodyparts)), rep("NP", length(bodyparts))) )

colour_data$order <- rank(-c(colour_data$Percentages))

#color the bodyparts for Unlikely.NP
mapply(function(x,y){changeColor(x,y)},bodyparts,colfunc(nrow(colour_data)+1)[colour_data[colour_data$Condition=="Unlikely.NP",]$order])
#load the XML as a picture
body_Unlikely.NP<-readPicture(saveXML(doc))


#color the bodyparts for  NP
mapply(function(x,y){changeColor(x,y)},bodyparts,colfunc(nrow(colour_data)+1)[colour_data[colour_data$Condition=="NP",]$order])
#load the XML as a picture
body_NP<-readPicture(saveXML(doc))

#plot it, you can change titles in top = "title" and layout by changing the order
pdf("NoNeup_neup.pdf")
grid.arrange(arrangeGrob(pictureGrob(body_Unlikely.NP), top = 'Unlikely Neuropathic Pain'), arrangeGrob(pictureGrob(body_NP), top = 'Neuropathic Pain'), arrangeGrob(key), top = "Pain heatmap No NeuP vs NeuP", layout_matrix=lay)
dev.off()

