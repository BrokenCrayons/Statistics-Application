##############################################################################################
##############################################################################################
##############################################################################################

library(Cairo)
library(RCircos)
library(circlize)
library(openxlsx)

grandpa_career <- read.xlsx(file.choose())
head(grandpa_career)
str(grandpa_career)
grandpa_career$control <- rep("T",85)
str(grandpa_career)
grandpa_career$持家者 <- as.numeric(grandpa_career$持家者)+5
grandpa_career$工作者 <- as.numeric(grandpa_career$工作者)+5
grandpa_career$公民 <- as.numeric(grandpa_career$公民)+5
grandpa_career$休闲者 <- as.numeric(grandpa_career$休闲者)+5
grandpa_career$学生 <- as.numeric(grandpa_career$学生)+5
grandpa_career <- rbind(grandpa_career,c(0,0,0,0,0,0,0,"F"))
grandpa_career <- rbind(grandpa_career,c(85,0,0,0,0,0,0,"F"))
grandpa_career$年龄 <- as.numeric(grandpa_career$年龄)
grandpa_career$持家者 <- as.numeric(grandpa_career$持家者)
grandpa_career$工作者 <- as.numeric(grandpa_career$工作者)
grandpa_career$公民 <- as.numeric(grandpa_career$公民)
grandpa_career$休闲者 <- as.numeric(grandpa_career$休闲者)
grandpa_career$学生 <- as.numeric(grandpa_career$学生)
grandpa_career$儿童 <- as.numeric(grandpa_career$儿童)
str(grandpa_career)


circos.par(track.height = 0.1)
circos.initialize(factors = grandpa_career$control,  x = grandpa_career$年龄)
# Layer1
circos.trackPlotRegion(factors = grandpa_career$control, y = grandpa_career$持家者,bg.col =c(NA,"#ECF0F2"),bg.border = c(NA,"black"),panel.fun = function(x, y) { 
  circos.axis(labels.pos.adjust = FALSE)})
circos.trackLines(factors = grandpa_career$control, x = grandpa_career$年龄, y = grandpa_career$持家者,lwd = 2,straight = FALSE,area = TRUE,col = "#455C7B", type = "s")
# Layer2
circos.trackPlotRegion(factors = grandpa_career$control, y = grandpa_career$工作者, bg.col =c(NA,"#ECF0F2"),bg.border = c(NA,"black"))
circos.lines(x = grandpa_career$年龄[1:85], y = grandpa_career$工作者[1:85],lwd = 1.5,straight = FALSE,area = TRUE,col = "#685C79", type = "s")
# Layer3     
circos.trackPlotRegion(factors = grandpa_career$control, y = grandpa_career$公民, bg.col =c(NA,"#ECF0F2"),bg.border = c(NA,"black"))
circos.lines(x = grandpa_career$年龄[1:85], y = grandpa_career$公民[1:85],lwd = 1.5,straight = FALSE,area = TRUE,col = "#AC6C82", type = "s")
# Layer4     
circos.trackPlotRegion(factors = grandpa_career$control, y = grandpa_career$休闲者, bg.col =c(NA,"#ECF0F2"),bg.border = c(NA,"black"))
circos.lines(x = grandpa_career$年龄[1:85], y = grandpa_career$休闲者[1:85],lwd = 1.5,straight = FALSE,area = TRUE,col = "#DA727E", type = "s")
# Layer5     
circos.trackPlotRegion(factors = grandpa_career$control, y = grandpa_career$学生, bg.col =c(NA,"#ECF0F2"),bg.border = c(NA,"black"))
circos.lines(x = grandpa_career$年龄[1:85], y = grandpa_career$学生[1:85],lwd = 1.5,straight = FALSE,area = TRUE,col = "#F8B195", type = "s")
# Layer6     
circos.trackPlotRegion(factors = grandpa_career$control, y = grandpa_career$儿童, bg.col =c(NA,"#ECF0F2"),bg.border = c(NA,"black"))
circos.lines(x = grandpa_career$年龄[1:85], y = grandpa_career$儿童[1:85],lwd = 1.5,straight = FALSE,area = TRUE,col = "#FFBC67", type = "s")

# Layer7
circos.trackPlotRegion(factors = grandpa_career$control, y = grandpa_career$儿童, bg.col = NA,bg.border = NA)
circos.text(43, 30, "生涯彩虹图", sector.index = "T", track.index = 7,family = "STHeiti",cex = 1.5)
circos.text(43, -90, "爷爷的0-85岁", sector.index = "T", track.index = 7,family = "STHeiti",cex = 1.5)
circos.text(43, -180, "嘉嘉出品", sector.index = "T", track.index = 7,family = "STHeiti",cex = 0.8)

# Add Text
circos.text(4, 65, "持家者", sector.index = "T", track.index = 1,family = "STHeiti",cex = 1)
circos.text(4, 70, "工作者", sector.index = "T", track.index = 2,family = "STHeiti",cex = 1)
circos.text(4, 70, "公民", sector.index = "T", track.index = 3,family = "STHeiti",cex = 1)
circos.text(4, 45, "休闲者", sector.index = "T", track.index = 4,family = "STHeiti",cex = 1)
circos.text(3, 70, "学生", sector.index = "T", track.index = 5,family = "STHeiti",cex = 1)
circos.text(5, 52, "儿童", sector.index = "T", track.index = 6,family = "STHeiti",cex = 1)
circos.text(7, 280, "成长期", sector.index = "T", track.index = 1,family = "STHeiti",cex = 1)
circos.text(20, 280, "探索期", sector.index = "T", track.index = 1,family = "STHeiti",cex = 1)
circos.text(32, 280, "建立期", sector.index = "T", track.index = 1,family = "STHeiti",cex = 1)
circos.text(54, 280, "维持期", sector.index = "T", track.index = 1,family = "STHeiti",cex = 1)
circos.text(67, 280, "卸任期", sector.index = "T", track.index = 1,family = "STHeiti",cex = 1)
circos.text(85, 360, "(2018年)", sector.index = "T", track.index = 1,family = "STHeiti",cex = 1,facing = "clockwise")
circos.clear()



##############################################################################################
##############################################################################################
##############################################################################################

library(Cairo)
library(RCircos)
library(circlize)
library(openxlsx)
CairoPDF("/Users/wujiawen/Desktop/2018 Autumn/职业心理学/期末论文——生涯彩虹图/create rainbow plot with R/have_a_try.pdf",width = 9,height = 9)

grandpa_career <- read.xlsx("/Users/wujiawen/Desktop/2018 Autumn/职业心理学/期末论文——生涯彩虹图/create rainbow plot with R/granpa_life_career.xlsx")
head(grandpa_career)
str(grandpa_career)
grandpa_career$control <- rep("T",85)
str(grandpa_career)
grandpa_career$持家者 <- as.numeric(grandpa_career$持家者)+5
grandpa_career$工作者 <- as.numeric(grandpa_career$工作者)+5
grandpa_career$公民 <- as.numeric(grandpa_career$公民)+5
grandpa_career$休闲者 <- as.numeric(grandpa_career$休闲者)+5
grandpa_career$学生 <- as.numeric(grandpa_career$学生)+5
grandpa_career <- rbind(grandpa_career,c(0,0,0,0,0,0,0,"F"))
grandpa_career <- rbind(grandpa_career,c(85,0,0,0,0,0,0,"F"))
grandpa_career$年龄 <- as.numeric(grandpa_career$年龄)
grandpa_career$持家者 <- as.numeric(grandpa_career$持家者)
grandpa_career$工作者 <- as.numeric(grandpa_career$工作者)
grandpa_career$公民 <- as.numeric(grandpa_career$公民)
grandpa_career$休闲者 <- as.numeric(grandpa_career$休闲者)
grandpa_career$学生 <- as.numeric(grandpa_career$学生)
grandpa_career$儿童 <- as.numeric(grandpa_career$儿童)
str(grandpa_career)


circos.par(track.height = 0.1)
circos.initialize(factors = grandpa_career$control,  x = grandpa_career$年龄)
# Layer1
circos.trackPlotRegion(factors = grandpa_career$control, y = grandpa_career$持家者,bg.col =c(NA,"#ECF0F2"),bg.border = c(NA,"black"),panel.fun = function(x, y) { 
  circos.axis(labels.pos.adjust = FALSE)})
circos.trackLines(factors = grandpa_career$control, x = grandpa_career$年龄, y = grandpa_career$持家者,lwd = 2,straight = FALSE,area = TRUE,col = "#455C7B", type = "s")
# Layer2
circos.trackPlotRegion(factors = grandpa_career$control, y = grandpa_career$工作者, bg.col =c(NA,"#ECF0F2"),bg.border = c(NA,"black"))
circos.lines(x = grandpa_career$年龄[1:85], y = grandpa_career$工作者[1:85],lwd = 1.5,straight = FALSE,area = TRUE,col = "#685C79", type = "s")
# Layer3     
circos.trackPlotRegion(factors = grandpa_career$control, y = grandpa_career$公民, bg.col =c(NA,"#ECF0F2"),bg.border = c(NA,"black"))
circos.lines(x = grandpa_career$年龄[1:85], y = grandpa_career$公民[1:85],lwd = 1.5,straight = FALSE,area = TRUE,col = "#AC6C82", type = "s")
# Layer4     
circos.trackPlotRegion(factors = grandpa_career$control, y = grandpa_career$休闲者, bg.col =c(NA,"#ECF0F2"),bg.border = c(NA,"black"))
circos.lines(x = grandpa_career$年龄[1:85], y = grandpa_career$休闲者[1:85],lwd = 1.5,straight = FALSE,area = TRUE,col = "#DA727E", type = "s")
# Layer5     
circos.trackPlotRegion(factors = grandpa_career$control, y = grandpa_career$学生, bg.col =c(NA,"#ECF0F2"),bg.border = c(NA,"black"))
circos.lines(x = grandpa_career$年龄[1:85], y = grandpa_career$学生[1:85],lwd = 1.5,straight = FALSE,area = TRUE,col = "#F8B195", type = "s")
# Layer6     
circos.trackPlotRegion(factors = grandpa_career$control, y = grandpa_career$儿童, bg.col =c(NA,"#ECF0F2"),bg.border = c(NA,"black"))
circos.lines(x = grandpa_career$年龄[1:85], y = grandpa_career$儿童[1:85],lwd = 1.5,straight = FALSE,area = TRUE,col = "#FFBC67", type = "s")

# Layer7
circos.trackPlotRegion(factors = grandpa_career$control, y = grandpa_career$儿童, bg.col = NA,bg.border = NA)
circos.text(43, 30, "生涯彩虹图", sector.index = "T", track.index = 7,family = "STKaiti",cex = 1.5)
circos.text(43, -90, "爷爷的0-85岁", sector.index = "T", track.index = 7,family = "STKaiti",cex = 1.5)
circos.text(43, -180, "嘉嘉出品", sector.index = "T", track.index = 7,family = "STKaiti",cex = 0.8)

# Add Text
circos.text(4, 65, "持家者", sector.index = "T", track.index = 1,family = "STKaiti",cex = 1)
circos.text(4, 70, "工作者", sector.index = "T", track.index = 2,family = "STKaiti",cex = 1)
circos.text(4, 70, "公民", sector.index = "T", track.index = 3,family = "STKaiti",cex = 1)
circos.text(4, 45, "休闲者", sector.index = "T", track.index = 4,family = "STKaiti",cex = 1)
circos.text(3, 70, "学生", sector.index = "T", track.index = 5,family = "STHeiti",cex = 1)
circos.text(5, 52, "儿童", sector.index = "T", track.index = 6,family = "STHeiti",cex = 1)
circos.text(7, 280, "成长期", sector.index = "T", track.index = 1,family = "STHeiti",cex = 1)
circos.text(20, 280, "探索期", sector.index = "T", track.index = 1,family = "STHeiti",cex = 1)
circos.text(32, 280, "建立期", sector.index = "T", track.index = 1,family = "STHeiti",cex = 1)
circos.text(54, 280, "维持期", sector.index = "T", track.index = 1,family = "STHeiti",cex = 1)
circos.text(67, 280, "卸任期", sector.index = "T", track.index = 1,family = "STHeiti",cex = 1)
circos.text(85, 360, "(2018年)", sector.index = "T", track.index = 1,family = "STHeiti",cex = 1,facing = "clockwise")
circos.clear()


dev.off() 




