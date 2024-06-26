########################################
## Prepare plots for report
########################################

library(icesTAF)#3.0.0
library(icesAdvice)
suppressMessages(taf.library(FLEDA))
library(RColorBrewer)
library(ggplot2)
library(reshape)
library(tidyr)
library(dplyr)
library(ggpubr)
source("utilities_plot.R")
source("utilities_model.R")

mkdir("report")
stock.name <- "ple.27.7d"
assyear <- 2023
MSYBtrigger <- 37761 
Blim <- 27174
Fmsy <- 0.156

### ------------------------------------------------------------------------------------------------------
### ====   Explore data submitted in InterCatch ### ==== 
### ------------------------------------------------------------------------------------------------------

## Load data
load(paste0("data/StockOverview_", assyear,".RData")) 
load(paste0("data/input_", assyear, ".RData"))

x <- div(WtData, "CatchWt", grep=TRUE)

#==== P1 Overview of the total amount of landings and discards with sampling provided or not ====
Catch <- subset(x, CatchWt > 0)
Catch$Sampled[Catch$Sampled=="FALSE"] <- "Unsampled"
Catch$Sampled[Catch$Sampled=="TRUE"] <- "Sampled"

# percentage of sampled landings
lan_tot <- sum(subset(x, CatchCat == "L")$CatchWt)
dis_tot <- sum(subset(x, CatchCat == "D")$CatchWt)
samp_lan <- sum(subset(x, CatchCat == "L" & Sampled == "TRUE")$CatchWt)
samp_dis <- sum(subset(x, CatchCat == "D" & Sampled == "TRUE")$CatchWt)
perc_samp_lan <- 100 * samp_lan/lan_tot
perc_samp_dis <- 100 * samp_dis/dis_tot

dat_text <- data.frame(label = c( paste0("Sampled Discards ", round(perc_samp_dis, 1), "%"), 
                                  paste0("Sampled Landings ", round(perc_samp_lan, 1), "%")),
                       CatchCat = c("D", "L"), 
                       Sampled = c("Sampled", "Sampled"))

newlabs <- c("Landings", "Discards")
names(newlabs) <- c("L", "D")

Catch$Fleet = with(Catch, reorder(Fleet, CatchWt))

p1 <- ggplot(Catch, aes(x= Fleet)) +
  geom_bar(stat="identity", aes(y=CatchWt, fill = Country)) +
  scale_fill_brewer(name="", palette = "Dark2") +
  xlab("") +
  ylab("Catch weight (t)") +
  facet_grid(Sampled~CatchCat, labeller = labeller(CatchCat=newlabs), scales = "free_y") +
  coord_flip() +
  geom_label(data = dat_text, mapping = aes(x = -Inf, y = -Inf, label = label),
             hjust = 0, vjust = 0, x=c(1, 1), y=c(500, 500), size = 5) +
  ggtitle(paste("Sampling of landings and discards by country fleets", assyear-1)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 14), axis.text.y = element_text(size = 9),
        strip.text.x = element_text(size = 18), strip.text.y = element_text(size = 18),
        legend.text = element_text(size=16))
p1

ggsave(p1, file= paste0("report/Landings and discrads provision ", assyear-1, ".png"), width = 35, height = 20, units = "cm", dpi = 300)

##==== Total landings and catches per quarter ====
Catch <- subset(x, CatchWt > 0)

Catch_Q <- aggregate(Catch$CatchWt, list(Catch$Season, Catch$CatchCat), sum)
names(Catch_Q) <- c("Quarter", "Catch_category", "Catch")

p2 <- ggplot(Catch_Q, aes(as.factor(Quarter), Catch, fill = Catch_category)) +
  geom_bar(stat = 'identity') +
  xlab("Quarter") +
  ylab("Catch (t)") +
  scale_fill_brewer(name="", palette = "Dark2", labels = c("Discards", "Landings")) +  
  ggtitle(paste0("Landings and discards by quarter ", assyear-1)) +
  theme_bw() +
  theme(legend.position = "bottom", axis.title.x = element_text(size = 16),
                                    axis.title.y = element_text(size = 16),
                                    axis.text.x = element_text(size = 16),
                                    axis.text.y = element_text(size = 16))
p2

ggsave(p2, file= paste0("report/Landings and discards by quarter ", assyear, ".png"), width = 20, height = 15, units = "cm", dpi = 300)

### ------------------------------------------------------------------------------------------------------
###   Indices overview
### ------------------------------------------------------------------------------------------------------

taf.png(paste0(stock.name,"_UK_BTS_,", assyear, ".png"), width = 1500, height = 1500)
cor.tun(indices[1])
dev.off()

taf.png(paste0(stock.name,"_FR_GFS_", assyear, ".png"), width = 1500, height = 1500)
cor.tun(indices[2])
dev.off()

### ------------------------------------------------------------------------------------------------------
###   Catch Data overview
### ------------------------------------------------------------------------------------------------------
## Landings
st_lan <- as.data.frame(stock@landings)

p3 <- ggplot(st_lan, aes(year, data)) +
  geom_line() +
  ylab("Landings (t)") +
  ggtitle(paste0("Landings 1980 - ", assyear)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16))
p3

ggsave(p3, file= paste0("report/Landings ", assyear, ".png"), width = 20, height = 15, units = "cm", dpi = 300)

st_lan_n <- as.data.frame(stock@landings.n)

p4 <- ggplot(st_lan_n, aes(year, y=as.factor(age))) +
  geom_point(aes(size = data, fill=data, color=data), shape=21) +
  scale_fill_viridis_c(alpha = 0.5) +
  scale_color_viridis_c() +
  scale_size_continuous(name="", limits = c(0, 20000), range = c(1,17), breaks = c(5000,10000,15000)) + 
  xlab("Year") +
  ylab("Age") +
  ggtitle(paste0("Landings (numbers) by age : 1980 - ", assyear)) +
  theme_bw() +
  theme(legend.position = "none",  
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16))
p4

ggsave(p4, file= paste0("report/Landings n by age 1980 - ", assyear, ".png"), width = 20, height = 15, units = "cm", dpi = 300)

## Weight at age in the stock
st_wt <- as.data.frame(stock@stock.wt)

p5 <- ggplot(st_wt, aes(year, data)) +
  geom_line(aes(color=as.factor(age))) +
  geom_point(aes(color=as.factor(age)), size = 0.75) +
  xlab("Year") +
  ylab("Weight (kg)") +
  scale_color_brewer(name= "Age", palette = "Dark2") +
  ggtitle(paste0("Weight at age in the stock : 1980 - ", assyear)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16))
p5

ggsave(p5, file= paste0("report/Weight at age in the stock 1980 - ", assyear, ".png"), width = 20, height = 15, units = "cm", dpi = 300)

## Weight at age in the landings
lan_wt <- as.data.frame(stock@landings.wt)

p6 <- ggplot(lan_wt, aes(year, data)) +
  geom_line(aes(color=as.factor(age))) +
  geom_point(aes(color=as.factor(age)), size = 0.75) +
  xlab("Year") +
  ylab("Weight (kg)") +
  scale_color_brewer(name= "Age", palette = "Dark2") +
  ggtitle(paste0("Weight at age in the landings : 1980 - ", assyear)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16))
p6

ggsave(p6, file= paste0("report/Weight at age in the landings 1980 - ", assyear, ".png"), width = 20, height = 15, units = "cm", dpi = 300)

## Weight at age in the discards
dis_wt <- as.data.frame(stock@discards.wt)

p7 <- ggplot(dis_wt, aes(year, data)) +
  geom_line(aes(color=as.factor(age))) +
  geom_point(aes(color=as.factor(age)), size = 0.75) +
  xlab("Year") +
  ylab("Weight (kg)") +
  scale_color_brewer(name= "Age", palette = "Dark2") +
  ggtitle(paste0("Weight at age in the discards : 1980 - ", assyear)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16))
p7

ggsave(p7, file= paste0("report/Weight at age in the discards 1980 - ", assyear, ".png"), width = 20, height = 15, units = "cm", dpi = 300)

## Weight at age in the landings, discards and stock
st_wt$Cat <- "Stock"
lan_wt$Cat <- "Landings"
dis_wt$Cat <- "Discards"
weight_age <- rbind(st_wt, lan_wt, dis_wt)

p8 <- ggplot(weight_age, aes(year, data)) +
  geom_line(aes(color=as.factor(age))) +
  geom_point(aes(color=as.factor(age)), size = 0.75) +
  xlab("Year") +
  ylab("Weight (kg)") +
  scale_color_brewer(name= "Age", palette = "Dark2") +
  facet_grid(Cat~.) +
  ggtitle(paste0("Weight at age  1980 - ", assyear)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 14), legend.key.size = unit(1, "cm"),
        strip.text.y = element_text(size = 18), legend.title = element_text(size = 16))
p8

ggsave(p8, file= paste0("report/Weight at age 1980 - ", assyear, ".png"), width = 25, height = 20, units = "cm", dpi = 300)

### Discards
st_dis <- as.data.frame(stock@discards)

p9 <- ggplot(st_dis, aes(year, data)) +
  geom_line() +
  xlab("Year") +
  ylab("Discards (t)") +
  ggtitle(paste0("Discards 1980 - ", assyear)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16))
p9

ggsave(p9, file= paste0("report/Discards ", assyear, ".png"), width = 20, height = 15, units = "cm", dpi = 300)

## Landings and Discards on the same plot
st_lan$Category <- "Landings"
st_dis$Category <- "Discards"
st_cat <- rbind(st_lan, st_dis)

p10 <- ggplot(st_cat, aes(year, data)) +
  geom_line() +
  xlab("Year") +
  ylab("Tonnes") +
  facet_grid(.~Category) +
  ggtitle(paste0("Landings and discards 1980 - ", assyear)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
p10

ggsave(p10, file= paste0("report/Landings_Discards ", assyear, ".png"), width = 25, height = 15, units = "cm", dpi = 300)

p10.1 <- ggplot(st_cat, aes(year, data, color=Category)) +
  geom_line() +
  xlab("Year") +
  ylab("Tonnes") +
  scale_color_brewer(name="", palette="Dark2") +
  ggtitle(paste0("Landings and discards 1980 - ", assyear)) +
  theme_bw() +
  expand_limits(y = 0) +
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 16), legend.key.size = unit(2, "cm"))

p10.1

ggsave(p10.1, file= paste0("report/Landings_Discards_2 ", assyear, ".png"), width = 25, height = 15, units = "cm", dpi = 300)

## Observed discards at age
st_dis_n <- as.data.frame(stock@discards.n)
st_dis_n <- na.omit(st_dis_n)

p11 <- ggplot(st_dis_n, aes(year, y=as.factor(age))) +
  geom_point(aes(size = data, fill=data, color=data), shape=21) +
  scale_fill_viridis_c(alpha = 0.5) +
  scale_color_viridis_c() +
  scale_size_continuous(name="", limits = c(0, max(st_dis_n$data)), range = c(1,25), breaks = c(5000,10000,15000)) + 
  xlab("Year") +
  ylab("Age") +
  ggtitle(paste0("Observed discards (numbers) by age : 2006 - ", assyear)) +
  theme_bw() +
  theme(legend.position = "none", axis.title.x = element_text(size = 16),
                                  axis.title.y = element_text(size = 16),
                                  axis.text.x = element_text(size = 16),
                                  axis.text.y = element_text(size = 16))
p11

ggsave(p11, file= paste0("report/Observed discards n by age 2006 - ", assyear, ".png"), width = 25, height = 20, units = "cm", dpi = 300)

### ------------------------------------------------------------------------------------------------------
###   Assessment diagnostic ====
### ------------------------------------------------------------------------------------------------------

## load results and retrospectives
load("output/output.RData")
load("output/retro_mohnrho.RData")

summary <- read.taf("output/summary.csv")
# x <- div(summary, "SSB", grep=TRUE)
summary_lon <- pivot_longer(summary,cols = 2:dim(summary)[2], names_to = "variable")
maxyear <- range(stock)[["maxyear"]]

## Biomass
p12 <- ggplot(summary, aes(Year, SSB)) +
  geom_ribbon(aes(ymin=SSB_lo, ymax=SSB_hi), fill = "grey", alpha = 0.5) +
  geom_line(color = "tomato", linewidth=1) +
  ylab("SSB (t)") +
  geom_hline(yintercept = MSYBtrigger, linetype="dotted", linewidth=1) +
  geom_hline(yintercept = Blim, linetype="dashed", linewidth=1) +
  annotate("text", x = 1990, y = MSYBtrigger + 4000, label = "MSY~B[trigger]", parse=TRUE, size = 7) +
  annotate("text", x = 1990, y = Blim + 4000, label = "B[lim]", parse=TRUE, size = 7) +
  ggtitle(paste0("SSB : 1980 - ", assyear)) +
  theme_bw() +
  theme (axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
         axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16))
p12

ggsave(p12, file= paste0("report/SSB 1980 - ", assyear, ".png"), width = 20, height = 15, units = "cm", dpi = 300)

## Mean F
p13 <- ggplot(summary, aes(Year, Fbar)) +
  geom_ribbon(aes(ymin=Fbar_lo, ymax=Fbar_hi), fill = "grey", alpha = 0.5) +
  geom_line(color = "tomato", linewidth=1) +
  ylab("Mean F") +
  geom_hline(yintercept = Fmsy, linetype="dotted", linewidth=1) +
  annotate("text", x = 1987, y = Fmsy + 0.025, label = "F[MSY]", parse=TRUE, size = 7) +
  ggtitle(paste0("Mean F  (ages 3-6) : 1980 - ", assyear)) +
  theme_bw() +
  theme (axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
         axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16))
p13

ggsave(p13, file= paste0("report/Fbar 1980 - ", assyear, ".png"), width = 20, height = 15, units = "cm", dpi = 300)

## Recruitment
p14 <- ggplot(summary, aes(Year, Rec)) +
  geom_ribbon(aes(ymin=Rec_lo, ymax=Rec_hi), fill = "grey", alpha = 0.5) +
  geom_line(color = "tomato", linewidth=1) +
  ylab("Recruits (x1000)") +
  ggtitle(paste0("Recruitement : 1980 - ", assyear)) +
  theme_bw() +
  theme (axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
         axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16))
p14

ggsave(p14, file= paste0("report/Recruitment 1980 - ", assyear, ".png"), width = 20, height = 15, units = "cm", dpi = 300)

## landings observed vs modeled
pred_land <- as.data.frame(apply(stock@landings.wt * results@landings.n, 2, sum, na.rm=T))
orig_land <- as.data.frame(stock@landings)
lan_data <- data.frame(Year = stock@range["minyear"] : stock@range["maxyear"],
                       Model_estimate = pred_land$data,
                       Observations =  orig_land$data)
lan_data$perc_diff <- ((lan_data$Model_estimate - lan_data$Observations)/lan_data$Observations)*100
lan_data <- mutate(lan_data, color_sign = ifelse(perc_diff>0, "type1", "type2"))

p15 <- ggplot(lan_data, aes(Year, Model_estimate)) +
  geom_line(aes(color = "Model estimate")) +
  geom_point(aes(y=Observations, shape = "Observations"), fill = "black") +
  scale_color_manual(name = "", values = c("black")) +
  scale_shape_manual(name = "", values = 21) +
  xlab("Year") +
  ylab("Landings (t)") +
  ggtitle(paste0("Model estimate (with observed weights) vs observed landings : 1980 - ", assyear)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16),
        legend.position = "bottom", legend.text = element_text(size=16))
p15

ggsave(p15, file= paste0("report/Landings prediction vs observations ", assyear, ".png"), width = 20, height = 15, units = "cm", dpi = 300)

p15.1 <- ggplot(lan_data, aes(Year, perc_diff)) +
  geom_segment( aes(x=Year, xend=Year, y=0, yend=perc_diff, color=color_sign), linewidth=1.3, alpha=0.9) +
  scale_color_brewer(name="", palette="Dark2") +
  theme_light() +
  ylab("% of change")+
  ggtitle(paste0("Percentage of change between estimated and observed landings 1980 - ", assyear)) +
  theme(legend.position = "none", 
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16))
p15.1

ggsave(p15.1, file= paste0("report/Perc of change between estimated and observed landings ", assyear, ".png"), width = 20, height = 15, units = "cm", dpi = 300)

### landings observed vs modeled at age in tonnes
pred_land_age <- as.data.frame(stock@landings.wt * results@landings.n) 
pred_land_age$category <- "Model estimate" 
orig_land_age <- as.data.frame(stock@landings.wt * stock@landings.n) 
orig_land_age$category <- "Observations"

p16 <- ggplot(data=pred_land_age, aes(year, data)) +
  geom_line() +
  geom_point(data=orig_land_age, aes(year, data)) +
  facet_grid(age~., scales = "free_y") +
  xlab("Year") +
  ylab("Landings (t)") +
  ggtitle(paste0("Model estimate (with observed weights) vs observed landings by age : 1980 - ", assyear)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14),
        legend.position = "bottom", legend.text = element_text(size=18), 
        legend.title = element_blank(), legend.key.size = unit(2, "cm"), strip.text= element_text(size = 16))
p16

ggsave(p16, file= paste0("report/Landings prediction vs observations by age ", assyear, ".png"), width = 25, height = 25, units = "cm", dpi = 300)

### landings observed vs modeled at age in numbers
pred_land_age <- as.data.frame(results@landings.n) 
pred_land_age <- subset(pred_land_age, age %in% 1:6)
orig_land_age <- as.data.frame(stock@landings.n) 
orig_land_age <- subset(orig_land_age, age %in% 1:6)

p16.1 <- ggplot(pred_land_age, aes(year, data)) +
  geom_line() +
  geom_point(data=orig_land_age, aes(year, data)) +
  facet_grid(age~., scales = "free_y") +
  xlab("Year") +
  ylab("Landings (numbers)") +
  ggtitle(paste0("Model estimate in numbers (line) vs observed landings (points) by age : 1980 - ", assyear)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14),
        legend.position = "bottom", legend.text = element_text(size=18), 
        legend.title = element_blank(), legend.key.size = unit(2, "cm"), strip.text= element_text(size = 16))
p16.1

ggsave(p16.1, file= paste0("report/Landings in numbers prediction vs observations by age ", assyear, ".png"), width = 25, height = 25, units = "cm", dpi = 300)

## discards observed vs modeled
pred_dis <- as.data.frame(apply(stock@discards.wt * results@discards.n, 2, sum, na.rm=T))
orig_dis <- as.data.frame(stock@discards)
dis_data <- data.frame(Year = stock@range["minyear"] : stock@range["maxyear"],
                       Model_estimate = pred_dis$data,
                       Observations =  orig_dis$data)
dis_data$perc_diff <- ((dis_data$Model_estimate - dis_data$Observations)/dis_data$Observations)*100
dis_data <- mutate(dis_data, color_sign = ifelse(perc_diff>0, "type1", "type2"))
dis_data <- subset(dis_data, Year >= 2006)

p17 <- ggplot(dis_data, aes(Year, Model_estimate)) +
  geom_line(aes(color = "Model estimate")) +
  geom_point(aes(y=Observations, shape = "Observations"), fill = "black") +
  scale_color_manual(name = "", values = c("black")) +
  scale_shape_manual(name = "", values = 21) +
  xlab("Year") +
  ylab("Discards (t)") +
  expand_limits(y = 0) +
  ggtitle(paste0("Model estimate (with observed weights) vs observed discards : 1980 - ", assyear)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16),
        legend.position = "bottom", legend.text = element_text(size=16))
p17

ggsave(p17, file= paste0("report/Discrads prediction vs observations ", assyear, ".png"), width = 20, height = 15, units = "cm", dpi = 300)

p17.1 <- ggplot(dis_data, aes(Year, perc_diff)) +
  geom_segment( aes(x=Year, xend=Year, y=0, yend=perc_diff, color=color_sign), size=1.3, alpha=0.9) +
  scale_color_brewer(name="", palette="Dark2") +
  theme_light() +
  ylab("% of change")+
  ggtitle(paste0("Percentage of change between estimated and observed discards 2006 - ", assyear)) +
  theme(legend.position = "none", 
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16))
p17.1

ggsave(p17.1, file= paste0("report/Perc of change between estimated and observed discards ", assyear, ".png"), width = 20, height = 15, units = "cm", dpi = 300)

### Discards observed vs modelled at age
pred_dis_age <- as.data.frame(stock@discards.wt * results@discards.n) 
pred_dis_age$category <- "Model estimate" 
orig_dis_age <- as.data.frame(stock@discards.wt * stock@discards.n) 
orig_dis_age$category <- "Observations"
dis_data_age <- rbind(pred_dis_age, orig_dis_age)
dis_data_age <- subset(dis_data_age, age %in% c(1:6))

p18 <- ggplot(dis_data_age, aes(year, data, linetype = category)) +
  geom_line() +
  facet_grid(age~., scales = "free_y") +
  xlab("Year") +
  ylab("Discards (t)") +
  ggtitle(paste0("Model estimate (with observed weights) vs observed discards by age : 1980 - ", assyear)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14),
        legend.position = "bottom", legend.text = element_text(size=18), 
        legend.title = element_blank(), legend.key.size = unit(2, "cm"), strip.text = element_text(size = 16))
p18

ggsave(p18, file= paste0("report/Discards prediction vs observations by age ", assyear, ".png"), width = 25, height = 25, units = "cm", dpi = 300)

### discards observed vs modeled at age in numbers
pred_dis_age <- as.data.frame(results@discards.n) 
pred_dis_age <- subset(pred_dis_age, age %in% 1:6 & year >= 2006)
orig_dis_age <- as.data.frame(stock@discards.n) 
orig_dis_age <- subset(orig_dis_age, age %in% 1:6 & year >= 2006)

p18.1 <- ggplot(pred_dis_age, aes(year, data)) +
  geom_line() +
  geom_point(data=orig_dis_age, aes(year, data)) +
  facet_grid(age~., scales = "free_y") +
  xlab("Year") +
  ylab("Discards (numbers)") +
  ggtitle(paste0("Model estimate in numbers (line) vs observed landings (point) by age : 1980 - ", assyear)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14),
        legend.position = "bottom", legend.text = element_text(size=18), 
        legend.title = element_blank(), legend.key.size = unit(2, "cm"), strip.text= element_text(size = 16))
p18.1

ggsave(p18.1, file= paste0("report/Discards in numbers prediction vs observations by age ", assyear, ".png"), width = 25, height = 25, units = "cm", dpi = 300)

## Total catches modelled vs simulated
pred_dis <- as.data.frame(apply(stock@discards.wt * results@discards.n, 2, sum, na.rm=T))
pred_land <- as.data.frame(apply(stock@landings.wt * results@landings.n, 2, sum, na.rm=T))
orig_cat <- as.data.frame(apply(stock@landings.wt * stock.orig@landings.n,2,sum, na.rm=T) + apply(stock.orig@discards.wt * stock.orig@discards.n,2,sum, na.rm=T))
cat_data <- data.frame(Year = stock@range["minyear"] : stock@range["maxyear"],
                       Model_estimate = pred_dis$data + pred_land$data,
                       Observations =  orig_cat$data)
cat_data <- cat_data[which(cat_data$Year>=2006),]
cat_data$perc_diff <- ((cat_data$Model_estimate - cat_data$Observations)/cat_data$Observations)*100
cat_data <- mutate(cat_data, color_sign = ifelse(perc_diff>0, "type1", "type2"))

p19 <- ggplot(cat_data, aes(Year, Model_estimate)) +
  geom_line(aes(color = "Model estimate")) +
  geom_point(aes(y=Observations, shape = "Observations"), fill = "black") +
  scale_color_manual(name = "", values = c("black")) +
  scale_shape_manual(name = "", values = 21) +
  xlab("Year") +
  ylab("Catch (t)") +
  ggtitle(paste0("Model estimate (with observed weights) vs observed catches : 2006 - ", assyear)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16),
        legend.position = "bottom", legend.text = element_text(size=16))
p19

ggsave(p19, file= paste0("report/Catches prediction vs observations ", assyear, ".png"), width = 20, height = 15, units = "cm", dpi = 300)

p19.1 <- ggplot(cat_data, aes(Year, perc_diff)) +
  geom_segment( aes(x=Year, xend=Year, y=0, yend=perc_diff, color=color_sign), size=1.3, alpha=0.9) +
  scale_color_brewer(name="", palette="Dark2") +
  theme_light() +
  ylab("% of change")+
  ggtitle(paste0("Percentage of change between estimated and observed catch 2006 - ", assyear)) +
  theme(legend.position = "none", 
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16))
p19.1

ggsave(p19.1, file= paste0("report/Perc of change between estimated and observed catch ", assyear, ".png"), width = 20, height = 15, units = "cm", dpi = 300)

## Estimated discards at age
pred_dis_n <- as.data.frame(results@discards.n)
pred_dis_n <- na.omit(pred_dis_n)

p20 <- ggplot(pred_dis_n, aes(year, y=as.factor(age))) +
  geom_point(aes(size = data, fill=data, color=data), shape=21) +
  scale_fill_viridis_c(alpha = 0.5) +
  scale_color_viridis_c() +
  scale_size_continuous(name="", limits = c(0, max(pred_dis_n$data)), range = c(1,25), breaks = c(5000,10000,15000)) + 
  xlab("Year") +
  ylab("Age") +
  ggtitle(paste0("Estimated discards (numbers) by age : 1980 - ", assyear)) +
  theme_bw() +
  theme(legend.position = "none", axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16), axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16))
p20

ggsave(p20, file= paste0("report/Estimated discards n by age 1980 - ", assyear, ".png"), width = 25, height = 20, units = "cm", dpi = 300)

## Observed landings at age
orig_lan_n <- as.data.frame(stock@landings.n)

p21 <- ggplot(orig_lan_n, aes(year, y=as.factor(age))) +
  geom_point(aes(size = data, fill=data, color=data), shape=21) +
  scale_fill_viridis_c(alpha = 0.5) +
  scale_color_viridis_c() +
  scale_size_continuous(name="", limits = c(0, max(orig_lan_n$data)), range = c(1,25), breaks = c(5000,10000,15000)) + 
  xlab("Year") +
  ylab("Age") +
  ggtitle(paste0("Observed landings (numbers) by age : 1980 - ", assyear)) +
  theme_bw() +
  theme(legend.position = "none", axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16), axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16))
p21

ggsave(p21, file= paste0("report/Observed landings n by age 1980 - ", assyear, ".png"), width = 25, height = 20, units = "cm", dpi = 300)

## Estimated landings at age bubbles
pred_lan_n <- as.data.frame(results@landings.n)

p22 <- ggplot(pred_lan_n, aes(year, y=as.factor(age))) +
  geom_point(aes(size = data, fill=data, color=data), shape=21) +
  scale_fill_viridis_c(alpha = 0.5) +
  scale_color_viridis_c() +
  scale_size_continuous(name="", limits = c(0, max(pred_lan_n$data)), range = c(1,25), breaks = c(5000,10000,15000)) + 
  xlab("Year") +
  ylab("Age") +
  ggtitle(paste0("Estimated landings (numbers) by age : 1980 - ", assyear)) +
  theme_bw() +
  theme(legend.position = "none", axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16))
p22

ggsave(p22, file= paste0("report/Estimated landings n by age 1980 - ", assyear, ".png"), width = 25, height = 20, units = "cm", dpi = 300)

## Estimated stock at age bubbles
pred_st_n <- as.data.frame(results@stock.n)

p23 <- ggplot(pred_st_n, aes(year, y=as.factor(age))) +
  geom_point(aes(size = data, fill=data, color=data), shape=21) +
  scale_fill_viridis_c(alpha = 0.5) +
  scale_color_viridis_c() +
  scale_size_continuous(name="", limits = c(0, max(pred_st_n$data)), range = c(1,25), breaks = c(5000,10000,15000)) + 
  xlab("Year") +
  ylab("Age") +
  ggtitle(paste0("Estimated stock (numbers) by age : 1980 - ", assyear)) +
  theme_bw() +
  theme(legend.position = "none", axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16))
p23

ggsave(p23, file= paste0("report/Estimated stock n by age 1980 - ", assyear, ".png"), width = 25, height = 20, units = "cm", dpi = 300)

## Estimated catch at age bubbles
pred_cat_n <- as.data.frame(results@catch.n)

p24 <- ggplot(pred_cat_n, aes(year, y=as.factor(age))) +
  geom_point(aes(size = data, fill=data, color=data), shape=21) +
  scale_fill_viridis_c(alpha = 0.5) +
  scale_color_viridis_c() +
  scale_size_continuous(name="", limits = c(0, max(pred_st_n$data)), range = c(1,25), breaks = c(5000,10000,15000)) + 
  xlab("Year") +
  ylab("Age") +
  ggtitle(paste0("Estimated catch (numbers) by age : 1980 - ", assyear)) +
  theme_bw() +
  theme(legend.position = "none", axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16))
p24

ggsave(p24, file= paste0("report/Estimated Catch n by age 1980 - ", assyear, ".png"), width = 25, height = 20, units = "cm", dpi = 300)

## Observed discard ratio
obs_dis_ratio <- stock.orig@discards.n/(stock.orig@discards.n + stock.orig@landings.n)
obs_dis_ratio <-as.data.frame(obs_dis_ratio)
obs_dis_ratio <- subset(obs_dis_ratio, year >= 2006)

p25 <- ggplot(obs_dis_ratio, aes(age, data, color=as.factor(year))) +
  geom_line() +
  scale_color_viridis_d(name="Year") +
  xlab("Age") +
  ylab("Discard ratio") +
  ggtitle(paste0("Observed discard ratio: 2006 - ", assyear)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16))
p25

ggsave(p25, file=paste0("report/Observed discard ratio 2006 - ", assyear, ".png"), width = 25, height = 15, units = "cm", dpi = 300)

## discards ratio obs vs model
mean_obs_dis_ratio <- apply(stock.orig@discards.n[,ac(2006:(assyear))]/(stock.orig@discards.n[,ac(2006:(assyear))] + stock.orig@landings.n[,ac(2006:(assyear))]),1,mean)
obs_discard_ratio <- (stock.orig@discards.n/(stock.orig@discards.n + stock.orig@landings.n))[,dim(stock.orig@discards.n)[2]]
# obs_dis_ratio <- data.frame(age = stock.orig@range[1]:stock.orig@range[2], observed_discard_ratio = as.vector(obs_dis_ratio))
pred_dis_ratio <- (results@discards.n/(results@discards.n + results@landings.n))[,dim(results@discards.n)[2]]
dis_ratio_data_df <- data.frame(age = stock.orig@range[1]:stock.orig@range[2],
                             average_dis_ratio = as.vector(mean_obs_dis_ratio),
                             estimated_dis_ratio = as.vector(pred_dis_ratio),
                             obs_dis_ratio = as.vector(obs_discard_ratio))
dis_ratio_data <- pivot_longer(dis_ratio_data_df, cols=c(2:4))

p26 <- ggplot(dis_ratio_data, aes(age, value, color = name)) +
  geom_line(data=dis_ratio_data[dis_ratio_data$name!="obs_dis_ratio", ], aes(age, value)) +
  scale_color_brewer(name = "", palette = "Dark2",
                     labels=c(paste0("Average discard ratio 2006-", assyear),
                              paste0("Estimated discard ratio ", assyear))) +
  xlab("Age") +
  ylab("Discard ratio") +
  ggtitle(paste0("Estimated discard ratio vs average discard ratio (2006 - ", assyear, ")")) +
  theme_bw() +
  theme(legend.position = "bottom", axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16), axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16), legend.text = element_text(size = 16),
        legend.key.size = unit(2, "cm"))
p26

p26.1 <- ggplot(dis_ratio_data_df, aes(age, estimated_dis_ratio)) +
  geom_line(aes(color="estimated_dis_ratio")) +
  geom_point(data=dis_ratio_data_df, aes(age, obs_dis_ratio, shape="obs_dis_ratio"), alpha = 0.9) +
  scale_shape_manual(name = "", values = 19, 
                     labels = paste0("Observed discard ratio ", assyear)) +
  scale_color_manual(name = "", values = c("black", "black"),
                     labels=c(paste0("Estimated discard ratio ", assyear))) +
  xlab("Age") +
  ylab("Discard ratio") +
  ggtitle(paste0("Model estimate vs observed discard ratio by age ", assyear)) +
  theme_bw() +
  theme(legend.position = "bottom", axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16), axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16), legend.text = element_text(size = 16),
        legend.key.size = unit(2, "cm"))
p26.1

ggsave(p26, file=paste0("report/Estimated discard ratio vs average observed discard ratio - ", assyear, ".png"), width = 25, height = 15, units = "cm", dpi = 300)
ggsave(p26.1, file=paste0("report/Estimated discard ratio vs observed discard ratio - ", assyear, ".png"), width = 25, height = 15, units = "cm", dpi = 300)


## discards ratio obs vs model
# mean_obs_dis_ratio <- apply(stock.orig@discards.n[,ac(2006:(assyear))]/(stock.orig@discards.n[,ac(2006:(assyear))] + stock.orig@landings.n[,ac(2006:(assyear))]),1,mean)
pred_dis_ratio_y <- results@discards.n/(results@discards.n + results@landings.n)
pred_dis_ratio_y <- as.data.frame(pred_dis_ratio_y)
pred_dis_ratio_y <- subset(pred_dis_ratio_y, year >= 2006 & age %in% c(1:6))
obs_dis_ratio_y <- (stock.orig@discards.n/(stock.orig@discards.n + stock.orig@landings.n))
obs_dis_ratio_y <- as.data.frame(obs_dis_ratio_y)
obs_dis_ratio_y <- subset(obs_dis_ratio_y, year >= 2006 & age %in% c(1:6))


p26.2 <- ggplot(pred_dis_ratio_y, aes(year, data)) +
  geom_line(aes(color = "Estimated discard ratio")) +
  geom_point(data=obs_dis_ratio_y, aes(year, data, color = "Observed discard ratio")) +
  scale_color_manual(name="", values = c("black", "black")) +
  xlab("Year") +
  ylab("Discard ratio") +
  facet_grid(age~., scales = "free_y") +
  ggtitle(paste0("Estimated vs observed discard ratio by age: 2006 - ", assyear)) +
  theme_bw() +
  theme(legend.position = "right", axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16), axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), legend.text = element_text(size = 16),
        legend.key.size = unit(2, "cm"), strip.text = element_text(size=16))
p26.2

ggsave(p26.2, file=paste0("report/Estimated discard ratio by age - ", assyear, ".png"), width = 30, height = 20, units = "cm", dpi = 300)

## Observed discard ratio
p27 <- ggplot(obs_dis_ratio, aes(age, data, color=as.factor(year))) +
  geom_line() +
  scale_color_viridis_d(name="Year") +
  geom_point(data =dis_ratio_data_df, aes(x= age, y=estimated_dis_ratio), color = "gold2", size = 2) +
  geom_point(data =dis_ratio_data_df, aes(x= age, y=average_dis_ratio ), color = "tomato", size = 2) +
  geom_line(data =dis_ratio_data_df, aes(x= age, y=average_dis_ratio ), color = "tomato") +
  xlab("Age") +
  ylab("Discard ratio") +
  ggtitle(paste0("Observed discard ratio: 2006 - ", assyear)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16))
p27

ggsave(p27, file=paste0("report/Observed discard ratio vs average discard ratio - ", assyear, " 2.png"), width = 25, height = 15, units = "cm", dpi = 300)

## Observed discard ratio through time
p28 <- ggplot(obs_dis_ratio, aes(year, data, color=as.factor(age))) +
  geom_line() +
  scale_color_viridis_d(name="Age") +
  xlab("Year") +
  ylab("Discard ratio") +
  ggtitle(paste0("Observed discard ratio through time : 2006 - ", assyear)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16))
p28

ggsave(p28, file=paste0("report/Observed discard ratio through time - ", assyear, " 2.png"), width = 25, height = 15, units = "cm", dpi = 300)

#### SELECTIVITY PATTERNS
q_hat <-as.data.frame(results@q.hat)
levels(q_hat$qname) <- c("UK_BTS", "FR_GFS")
q_hat2 <- pivot_wider(q_hat, names_from = qname, values_from = data)
q_hat3 <- mutate(q_hat2, UK_BTS_scale=UK_BTS /max(UK_BTS),
                         FR_GFS_scale=FR_GFS/max(FR_GFS))
q_hat4 <- pivot_longer(q_hat3, cols = c(9:10), values_to = "value")

p29 <- ggplot(q_hat4, aes(age, value, color = name)) +
  geom_line() +
  xlab("Year") +
  ylab("") +
  scale_color_brewer(name="", palette = "Dark2", labels = c("FR GFS", "UK BTS")) +
  ggtitle("Survey selectivity") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16),
        legend.position = "bottom", legend.text = element_text(size=16))
p29

ggsave(p29, file= paste0("report/Survey selectivity ", assyear, ".png"), width = 25, height = 15, units = "cm", dpi = 300)

## fleet selectivity (harvest rate = Ratio between landings and total stock abundance)
taf.png("fleet selectivity through age")
par(mfrow=c(3,2), mar=c(4,4,0,1)) 
for( yr in c(1990,2000,2010,2014,2018, 2022)){
  plot(x=1:length(results@harvest[,ac(yr)]),y=results@harvest[,ac(yr)], type="l", xlab="Age", ylab= "F", xlim=c(1,7), ylim=c(0,1.1), lwd=2)
  lines(x=1:length(results@harvest[,ac(yr)]),y=results@harvest[,ac(yr)]*(results@discards.n/catch.n(results))[,ac(yr)] , xlim=c(1,7), lwd=2, lty=2)
  lines(x=1:length(results@harvest[,ac(yr)]),y=results@harvest[,ac(yr)]*(results@landings.n/catch.n(results))[,ac(yr)] , xlim=c(1,7), lwd=2, lty=3)
  text(2,1,yr)
  legend(4,1,c("catch","discards","landings"),bty="n", lty=c(1:3), cex=1.0, lwd=2)
}
dev.off()

# fleet sel through time
taf.png("fleet selectivity through time")
par(mfrow=c(2,2), mar=c(4,4,0,1)) 
for( age in 1:4){
  plot(x=c(1980:(assyear)),y=results@harvest[age,], type="l", xlab="years", ylab= paste("F at age", age), xlim=c(1980,(assyear)), ylim=c(0,1.1), lwd=2)
  lines(x=c(1980:(assyear)),y=results@harvest[age,]*(results@discards.n/catch.n(results))[age,] , xlim=c(1980,(assyear)), lwd=2, lty=2)
  lines(x=c(1980:(assyear)),y=results@harvest[age,]*(results@landings.n/catch.n(results))[age,] , xlim=c(1980,(assyear)), lwd=2, lty=3)
  text(2,1,age)
  legend(2005,1,c("catch","discards","landings"),bty="n", lty=c(1:3), cex=1.0, lwd=2)
}
dev.off()

## Sigmas: Variance around landings, discards, and surveys selectivity. Even if discard ratio forced in the last years, still model adjustement on landings and discards and not on catch like for plaice north Sea
colset      <- brewer.pal(9, "Set1")

taf.png("sigmas", width = 3000, height = 1500, res = 300)
par(mfrow=c(1,2))
plot(x=1:7,y=results@landings.var, type="l", xlab="Age", ylab= "Sigma", xlim=c(0,8), ylim=c(0,2), main="Estimated Sigmas 
     (L+D)", col="blue", lwd=2, las=1, xaxs="i", yaxs="i")
lines(x=1:7,y=results@discards.var,lty=2, col=colset[2], lwd=2)
legend(0,1.8,c("Landings","Discards"),col=c("blue",colset[2]), lwd=2,lty=c(1,2), bty="n")

plot(x=1:7,y=results@index.var[[1]], type="l", xlab="Age", ylab= "Sigma", xlim=c(0,8), ylim=c(0,2), main="Estimated Sigmas (surveys)", col=colset[1], lwd=2, las=1, xaxs="i", yaxs="i")
for(ii in 2:length(indices.assess)) lines(x=1:7,y=results@index.var[[ii]],lty=1,col=colset[ii], lwd=2)
legend(0,1.8,unlist(names(indices.assess)),col=colset, lwd=2,lty=1, bty="n")
dev.off()

######################################################################
#####----- residuals standardized by sigma(weight)--------------#######
######################################################################

## Landings standardized log residuals
Lsd              <-  c(results@landings.var)
resCOrigLan     <-  log1p(landings.n(stock.orig)) - log(landings.n(ass.stock)) # observed - estimated as in assessment function in utilities_model.R
stdres           <- apply(resCOrigLan,2,function(x,Lsd){x <- x/Lsd},Lsd)
stdres[is.infinite(stdres)] <- NA

stdres           <- as.data.frame(stdres)
stdres$colr                     <- colset[2]
stdres$colr[stdres$data<0]      <- colset[3]

p33 <- ggplot(stdres, aes(x=year, y=age, size=abs(data), color=colr )) +
  geom_point(alpha=0.6) +
  scale_size(name = "", range = c(0, 12)) +
  scale_color_discrete(name="", labels=c("+", "-")) +
  scale_y_continuous(breaks=unique(stdres$age), labels=unique(stdres$age))+
  ggtitle("Standardized log landings residual") +
  theme_bw()+
  xlab("Year") +
  ylab("Age") +
  theme(plot.title = element_text(color="black", size=14, face="bold"),
        legend.position = "right", legend.title = element_text(colour="black", size=15, face="bold"),
        legend.text = element_text(colour="black", size=12, face="bold"),
        axis.text.x = element_text(color = "black", size = 14, angle = 0),
        axis.text.y = element_text(color = "black", size = 14, angle = 0),
        axis.title.x = element_text(color = "black", size = 14, angle = 0),
        axis.title.y = element_text(color = "black", size = 14))
p33

ggsave(p33, file= paste0("report/landings std log residuals ", assyear, ".png"), width = 25, height = 15, units = "cm", dpi = 300)

## Landings log residuals
Lsd              <-  c(results@landings.var )
resCOrigLan     <-  log1p(landings.n(stock.orig)) - log(landings.n(ass.stock)) # observed - estimated as in assessment function in utilities_model.R
resCOrigLan[is.infinite(resCOrigLan)] <- NA

resCOrigLan           <- as.data.frame(resCOrigLan)
resCOrigLan$colr                     <- colset[2]
resCOrigLan$colr[resCOrigLan$data<0]      <- colset[3]

p34 <- ggplot(resCOrigLan, aes(x=year, y=age, size=abs(data), color=colr )) +
  geom_point(alpha=0.6) +
  scale_size(name="", range = c(0, 12)) +
  scale_color_discrete(name="", labels=c("+", "-")) +
  scale_y_continuous(breaks=unique(resCOrigLan$age), labels=unique(resCOrigLan$age))+
  ggtitle("Landings log residual") +
  theme_bw() +
  xlab("Year") +
  ylab("Age") +
  theme(plot.title = element_text(color="black", size=14, face="bold"),
        legend.position = "right", legend.title = element_text(colour="black", size=15, face="bold"),
        legend.text = element_text(colour="black", size=12, face="bold"),
        axis.text.x = element_text(color = "black", size = 14, angle = 0),
        axis.text.y = element_text(color = "black", size = 14, angle = 0),
        axis.title.x = element_text(color = "black", size = 14, angle = 0),
        axis.title.y = element_text(color = "black", size = 14))
p34

ggsave(p34, file= paste0("report/Landings log residuals ", assyear, ".png"), width = 25, height = 15, units = "cm", dpi = 300)

## Discards standardized log residuals
Dsd              <-  c(results@discards.var)
resCOrigDis     <-  log1p(discards.n(stock.orig)) - log(discards.n(ass.stock)) # observed - estimated as in assessment function in utilities_model.R
stdres           <- apply(resCOrigDis,2,function(x,Dsd){x <- x/Dsd},Dsd)
stdres[is.infinite(stdres)] <- NA

## check if it is indeed the sigma of residuals calculated from log scale
# plot(1:7,apply(stdres,1,sd,na.rm=T),xlab='age',ylab='SD(stdres)',type='p',ylim=c(0,1.7), pch=19, main="Catches", col="blue", las=1)
# abline(h=1,col='magenta',lty=2)

stdres           <- as.data.frame(stdres)
stdres$colr                     <- colset[2]
stdres$colr[stdres$data<0]      <- colset[3]
stdres <- subset(stdres, year >= 2006)

p35 <- ggplot(stdres, aes(x=year, y=age, size=abs(data), color=colr )) +
  geom_point(alpha=0.6) +
  scale_size(name="", range = c(0, 12)) +
  scale_color_discrete(name="", labels=c("+", "-")) +
  scale_y_continuous(breaks=unique(stdres$age), labels=unique(stdres$age))+
  ggtitle("Standardized log discards residual") +
  theme_bw()+
  xlab("Year") +
  ylab("Age") +
  theme(plot.title = element_text(color="black", size=14, face="bold"),
        legend.position = "right", legend.title = element_text(colour="black", size=15, face="bold"),
        legend.text = element_text(colour="black", size=12, face="bold"),
        axis.text.x = element_text(color = "black", size = 14, angle = 0),
        axis.text.y = element_text(color = "black", size = 14, angle = 0),
        axis.title.x = element_text(color = "black", size = 14, angle = 0),
        axis.title.y = element_text(color = "black", size = 14))
p35

ggsave(p35, file= paste0("report/Standardized log discards residual ", assyear, ".png"), width = 25, height = 15, units = "cm", dpi = 300)

## Discards log residuals
Dsd <-  c(results@discards.var )
resCOrigDis     <-  log1p(discards.n(stock.orig)) - log(discards.n(ass.stock)) # observed - estimated as in assessment function in utilities_model.R
resCOrigDis[is.infinite(resCOrigDis)] <- NA

resCOrigDis <- as.data.frame(resCOrigDis)
resCOrigDis$colr                     <- colset[2]
resCOrigDis$colr[resCOrigDis$data<0]      <- colset[3]
resCOrigDis <- subset(resCOrigDis, year >= 2006)

p36 <- ggplot(resCOrigDis, aes(x=year, y=age, size=abs(data), color=colr )) +
  geom_point(alpha=0.6) +
  scale_size(name="", range = c(0, 12)) +
  scale_color_discrete(name="", labels=c("+", "-")) +
  scale_y_continuous(breaks=unique(resCOrigDis$age), labels=unique(resCOrigDis$age))+
  ggtitle("Log discards residual") +
  theme_bw()+
  xlab("Year") +
  ylab("Age") +
  theme(plot.title = element_text(color="black", size=14, face="bold"),
        legend.position = "right", legend.title = element_text(colour="black", size=15, face="bold"),
        legend.text = element_text(colour="black", size=12, face="bold"),
        axis.text.x = element_text(color = "black", size = 14, angle = 0),
        axis.text.y = element_text(color = "black", size = 14, angle = 0),
        axis.title.x = element_text(color = "black", size = 14, angle = 0),
        axis.title.y = element_text(color = "black", size = 14))
p36

ggsave(p36, file= paste0("report/Discards log residual ", assyear, ".png"), width = 25, height = 15, units = "cm", dpi = 300)

## survey residuals
stdresall <- NA
for(ss in c(1:2)){
  Usd    <-  as.data.frame(results@index.var[[ss]])
  Usd    <-  Usd[, c("age","data")]
  names(Usd) <- c("age", "var")
  stdres  <- as.data.frame(trim(results@index.res[[ss]],
                                age=1:6, year=1989:maxyear)) #already calculated in assessment function in utilities_model.R
  names(stdres)  <-  gsub("data", "resid", names(stdres))
  stdres <- merge(stdres, Usd, by = "age")
  stdres$stdresid <-  stdres$resid / stdres$var
  stdres$colr                     <- colset[2]
  stdres$colr[stdres$resid<0]      <- colset[3]
  stdres$survey                   <- names(indices.assess)[ss]
  stdresall         <- rbind(stdresall, stdres)
}

stdresall <- stdresall[!is.na(stdresall$resid),]

a1 <-  ggplot(stdresall, aes(x=year, y=age, size=abs(resid), color=colr )) +
  geom_point(alpha=0.6) +
  scale_size(name="", range = c(0, 10)) +
  scale_color_discrete(name="", labels=c("+", "-")) +
  scale_y_continuous(breaks=unique(stdres$age), labels=unique(stdres$age))+
  ggtitle("Survey log residuals") +
  xlab("Year") +
  ylab("Age") +
  theme(plot.title = element_text(color="black", size=14, face="bold"),
        legend.position = "right", legend.title = element_text(colour="black", size=15, face="bold"),
        legend.text = element_text(colour="black", size=12, face="bold"),
        axis.text.x = element_text(color = "black", size = 14, angle = 0),
        axis.text.y = element_text(color = "black", size = 14, angle = 0),
        axis.title.x = element_text(color = "black", size = 14, angle = 0),
        axis.title.y = element_text(color = "black", size = 14, angle = 0)) + theme_bw()

p37 <-  facet(a1, facet.by = c("survey"), ncol = 2, panel.labs.font.y = list(size = 14, angle=0), panel.labs.font.x = list(size = 14)) +
        theme(plot.title = element_text(color="black", size=20, face="bold"),
        legend.position = "bottom", legend.title = element_text(colour="black", size=20, face="bold"),
        legend.text = element_text(colour="black", size=14, face="plain"),
        axis.text.x = element_text(color = "black", size = 14, angle = 0),
        axis.text.y = element_text(color = "black", size = 14, angle = 0))
p37

ggsave(p37, file= paste0("report/Survey log residuals ", assyear, ".png"), width = 25, height = 15, units = "cm", dpi = 300)

# Survey standardized log residuals
a2 <-  ggplot(stdresall, aes(x=year, y=age, size=abs(stdresid), color=colr )) +
  geom_point(alpha=0.6) +
  scale_size(name="", range = c(0, 10)) +
  scale_color_discrete(name="", labels=c("+", "-")) +
  scale_y_continuous(breaks=unique(stdres$age), labels=unique(stdres$age))+
  ggtitle("Survey standardized log residuals") +
  xlab("Year") +
  ylab("Age") +
  theme(plot.title = element_text(color="black", size=14, face="bold"),
        legend.position = "right", legend.title = element_text(colour="black", size=15, face="bold"),
        legend.text = element_text(colour="black", size=12, face="bold"),
        axis.text.x = element_text(color = "black", size = 14, angle = 0),
        axis.text.y = element_text(color = "black", size = 14, angle = 0),
        axis.title.x = element_text(color = "black", size = 14, angle = 0),
        axis.title.y = element_text(color = "black", size = 14, angle = 0)) + theme_bw()

p38 <-  facet(a2, facet.by = c("survey"), ncol = 2, panel.labs.font.y = list(size = 14, angle=0), panel.labs.font.x = list(size = 14)) +
        theme(plot.title = element_text(color="black", size=20, face="bold"),
        legend.position = "bottom", legend.title = element_text(colour="black", size=20, face="bold"),
        legend.text = element_text(colour="black", size=14, face="plain"),
        axis.text.x = element_text(color = "black", size = 14, angle = 0),
        axis.text.y = element_text(color = "black", size = 14, angle = 0))
p38

ggsave(p38, file= paste0("report/Survey standardized log residuals ", assyear, ".png"), width = 25, height = 15, units = "cm", dpi = 300)

###################################################
# Retro with disc 
###################################################
MohnRho_SSB <- mohn(retro.MohnRho$SSB, plot = T, lwd = 2)
MohnRho_Rec <- mohn(retro.MohnRho$Recruits, plot = T, lwd = 2)
MohnRho_MeanF <- mohn(retro.MohnRho$MeanF, plot = T, lwd = 2)

# F at age
harvest <- as.data.frame(results@harvest)

p39 <- ggplot(harvest, aes(year, data)) +
  geom_line(aes(color=as.factor(age))) +
  geom_point(aes(color=as.factor(age)), size = 0.75) +
  xlab("Year") +
  ylab("F") +
  scale_color_brewer(name= "Age", palette = "Dark2") +
  ggtitle(paste0("F at age : 1980 - ", assyear)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16))
p39

ggsave(p39, file= paste0("report/F at age 1980 - ", assyear, ".png"), width = 20, height = 15, units = "cm", dpi = 300)

## add IC to SSB retrospective 
IC_SSB_ret <-data.frame(Year = summary$Year, retro.MohnRho$SSB, summary$SSB_lo, summary$SSB_hi)
names(IC_SSB_ret) <- c("Year", "SSB", "n1", "n2", "n3", "n4", "n5", "SSB_low", "SSB_high")

p40 <- ggplot(data = IC_SSB_ret, aes(x=Year, y=SSB)) +
  geom_ribbon(aes(x=Year, ymin = SSB_low , ymax = SSB_high), fill = "lightblue", alpha = 0.5, color = "white") +
  geom_line(color = "tomato", size = 1)+
  geom_line(IC_SSB_ret, mapping =aes(x=Year, y=n1), color = "grey40", size = 1.2) +
  geom_line(IC_SSB_ret, mapping =aes(x=Year, y=n2), color = "grey50", size = 1.2) +
  geom_line(IC_SSB_ret, mapping =aes(x=Year, y=n3), color = "grey60", size = 1.2) +
  geom_line(IC_SSB_ret, mapping =aes(x=Year, y=n4), color = "grey70", size = 1.2) +
  geom_line(IC_SSB_ret, mapping =aes(x=Year, y=n5), color = "grey70", size = 1.2) +
  ggtitle("SSB retrospective with interval of confidence") +
  annotate("text", x= 1990, y=55000, label=paste0("MonhRho = ", round(MohnRho_SSB, 3)), size = 9) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16))
p40

ggsave(p40, file= paste0("report/SSB retro 1980 - ", assyear, ".png"), width = 25, height = 10, units = "cm", dpi = 300)

## add IC to Rec retrospective
IC_Rec_ret <-data.frame(Year = summary$Year, retro.MohnRho$Recruits, summary$Rec_lo, summary$Rec_hi)
names(IC_Rec_ret) <- c("Year", "Rec", "n1", "n2", "n3", "n4", "n5", "Rec_low", "Rec_high")

p41 <- ggplot(data = IC_Rec_ret, aes(x=Year, y=Rec)) +
  geom_ribbon(aes(x=Year, ymin = Rec_low , ymax = Rec_high), fill = "lightblue", alpha = 0.5, color = "white") +
  geom_line(color = "tomato", size = 1)+
  geom_line(IC_Rec_ret, mapping =aes(x=Year, y=n1), color = "grey40", size = 1.2) +
  geom_line(IC_Rec_ret, mapping =aes(x=Year, y=n2), color = "grey50", size = 1.2) +
  geom_line(IC_Rec_ret, mapping =aes(x=Year, y=n3), color = "grey60", size = 1.2) +
  geom_line(IC_Rec_ret, mapping =aes(x=Year, y=n4), color = "grey70", size = 1.2) +
  geom_line(IC_Rec_ret, mapping =aes(x=Year, y=n5), color = "grey70", size = 1.2) +
  ggtitle("Rec retrospective with interval of confidence") +
  annotate("text", x= 1990, y=330000, label=paste0("MonhRho = ", round(MohnRho_Rec, 3)), size = 9) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16))
p41

ggsave(p41, file= paste0("report/Rec retro 1980 - ", assyear, ".png"), width = 25, height = 10, units = "cm", dpi = 300)
# ggsave(p41, file= paste0("report/Rec retro 1980 - ", assyear, "v2.png"), width = 20, height = 12, units = "cm", dpi = 300)

## add IC to Fbar retrospective
IC_Fbar_ret <-data.frame(Year = summary$Year, retro.MohnRho$MeanF, summary$Fbar_lo, summary$Fbar_hi)
names(IC_Fbar_ret) <- c("Year", "Fbar", "n1", "n2", "n3", "n4", "n5", "Fbar_low", "Fbar_high")

p42 <- ggplot(data = IC_Fbar_ret, aes(x=Year, y=Fbar)) +
  geom_ribbon(aes(x=Year, ymin = Fbar_low , ymax = Fbar_high), fill = "lightblue", alpha = 0.5, color = "white") +
  geom_line(color = "tomato", size = 1)+
  geom_line(IC_Fbar_ret, mapping =aes(x=Year, y=n1), color = "grey40", size = 1.2) +
  geom_line(IC_Fbar_ret, mapping =aes(x=Year, y=n2), color = "grey50", size = 1.2) +
  geom_line(IC_Fbar_ret, mapping =aes(x=Year, y=n3), color = "grey60", size = 1.2) +
  geom_line(IC_Fbar_ret, mapping =aes(x=Year, y=n4), color = "grey70", size = 1.2) +
  geom_line(IC_Fbar_ret, mapping =aes(x=Year, y=n5), color = "grey70", size = 1.2) +
  ggtitle("Fbar retrospective with interval of confidence") +
  annotate("text", x= 2010, y=0.55, label=paste0("MonhRho = ", round(MohnRho_MeanF, 3)), size = 9) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16))
p42

ggsave(p42, file= paste0("report/Fbar retro 1980 - ", assyear, ".png"), width = 25, height = 10, units = "cm", dpi = 300)


# Report ADG plots  -------------------------------------------------------
load("output/output.RData")

library(ggplotFL)
library(patchwork)
library(tidyr)

a <- ass.stock@stock.n[,ac((assyear-7):(assyear))] 
b <- ass.stock@stock.n[,ac((assyear-8):(assyear-1))] 
cohort <- a/b 
cohort_df <- as.data.frame(cohort)
cohort_df_wide <- pivot_wider(cohort_df, names_from = year, values_from = data)
cohort_df_wide <- cohort_df_wide[,-c(2:5)]
 
write.csv(cohort_df_wide, "report/cohort_n_percentage_change.csv", row.names = FALSE)
 
# plotting percentage of change of n between year n and year n-1
p_report1 <- ggplot(cohort_df, aes(year, age, fill= data, label = round(data, 2))) + 
 geom_tile() +
 geom_label() +
 scale_fill_gradient2(low = "tomato",
                      mid = "white",
                      high = "steelblue",
                      midpoint = 1,
                      name= "% of change") +
  scale_x_continuous(breaks = (assyear-7):(assyear)) +
  scale_y_continuous(breaks = 1:7) +
  xlab("") +
  ylab("Age") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
         axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16),
         legend.position = "bottom")
p_report1
 
ggsave(p_report1, file= paste0("report/Heatmap plot age_n_year ", assyear, ".png"), width = 25, height = 14, units = "cm", dpi = 300)
 
 





# # +++++++++++++++++++++++++ Additional plot with recrutement assumption (2024)
# 
# #### Recruitment assumption ####
# rec_age1 <- as.data.frame(rec(ass.stock)) #( == ass.stock@stock.n["1",] )
# finalYr <- assyear
# GM_Age1 <- exp(mean(log(rec(ass.stock)[,ac(1980:(finalYr-1))]))) # GM the whole time serie except the last year
# GM_Age2 <- exp(mean(log(rec(ass.stock)[,ac((finalYr-5):(finalYr-1))]))) # GM final_year-5 to final_year-3 
# GM_Age3 <- exp(mean(log(rec(ass.stock)[,ac(1980:(finalYr-15))]))) # GM the whole time serie except the 2 last years
# GM_Age4 <- quantile(rec_age1$data[-dim(rec_age1)[1]], 0.25)
# 
# 
# year <- c(1980, assyear)
# p41.1 <- ggplot(data = IC_Rec_ret, aes(x=Year, y=Rec)) +
#   geom_ribbon(aes(x=Year, ymin = Rec_low , ymax = Rec_high), fill = "lightblue", alpha = 0.5, color = "white") +
#   geom_line(color = "tomato", size = 1)+
#   geom_line(IC_Rec_ret, mapping =aes(x=Year, y=n1), color = "grey40", size = 1.2) +
#   geom_line(IC_Rec_ret, mapping =aes(x=Year, y=n2), color = "grey50", size = 1.2) +
#   geom_line(IC_Rec_ret, mapping =aes(x=Year, y=n3), color = "grey60", size = 1.2) +
#   geom_line(IC_Rec_ret, mapping =aes(x=Year, y=n4), color = "grey70", size = 1.2) +
#   geom_line(IC_Rec_ret, mapping =aes(x=Year, y=n5), color = "grey70", size = 1.2) +
#   
#   
#   geom_segment(aes(x=min(year), xend=(max(year)-1), y=GM_Age1, yend=GM_Age1), color = "tomato", linewidth = 2) +
#   annotate("text", x=(finalYr + 5), y=GM_Age1, label= paste("GM1", round(GM_Age1, 0), sep = " "), color = "tomato", size=4) +
#   
#   geom_segment(aes(x=max(year-5), xend=(max(year)-1), y=GM_Age2, yend=GM_Age2), color = "deepskyblue", linewidth = 2) +
#   annotate("text", x=(finalYr + 5), y=GM_Age2, label= paste("GM2", round(GM_Age2, 0), sep = " "), color = "deepskyblue", size=4) +
#   
#   geom_segment(aes(x=min(year), xend=(max(year)-15), y=GM_Age3, yend=GM_Age3), color = "purple1", linewidth = 2) +
#   annotate("text", x=(finalYr-10), y=GM_Age3, label= paste("GM3", round(GM_Age3, 0), sep = " "), color = "purple1", size=4) +
#   
#   geom_segment(aes(x=min(year), xend=(max(year)-1), y=GM_Age4, yend=GM_Age4), color = "seagreen3", linewidth = 2) +
#   annotate("text", x=(finalYr + 5), y=GM_Age4, label= paste("25% quantile", round(GM_Age4, 0), sep = "\n"), color = "seagreen3", size=4) +
#   
#   
#   
#   ggtitle("Rec retrospective with interval of confidence") +
#   annotate("text", x= 1990, y=330000, label=paste0("MonhRho = ", round(MohnRho_Rec, 3)), size = 9) +
#   theme_bw() +
#   theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),
#         axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16))
# p41.1
# 
# ggsave(p41.1, file= paste0("report/Rec retro 1980 - ", assyear, "v2.png"), width = 20, height = 12, units = "cm", dpi = 300)
# 
# 
# # ============================
# rec_age1 <- as.data.frame(rec(ass.stock)) #( == ass.stock@stock.n["1",] )
# ssb <- as.data.frame(ass.stock@stock)
# 
# rec_ssb <- rec_age1$data / ssb$data
# rec_ssb <- data.frame(year = rec_age1$year, data= log(rec_ssb))
# rec_ssb2 <- data.frame(year = rec_age1$year, ssb = ssb$data, rec = rec_age1$data)
# 
# p_rec_ssb <- ggplot(rec_ssb, aes(x=year, y=data)) +
#   geom_line() +
#   geom_point() +
#   ylab("log(rec / ssb)") +
#   ggtitle(paste0("Assessment year ", assyear)) +
# theme_bw()
# p_rec_ssb
# 
# ggsave(p_rec_ssb, file= paste0("report/Rec_ssb retro 1980 - ", assyear, ".png"), width = 18, height = 13, units = "cm", dpi = 300)
# 
# p_rec_ssb2 <- ggplot(rec_ssb2, aes(x=ssb, y=rec, color = year)) +
#   # geom_line() +
#   geom_point() +
#   scale_colour_viridis_c() +
#   ggtitle(paste0("Assessment year ", assyear)) +
#   theme_bw() 
# p_rec_ssb2
# 
# ggsave(p_rec_ssb2, file= paste0("report/Rec_ssb2 retro 1980 - ", assyear, ".png"), width = 18, height = 13, units = "cm", dpi = 300)
# 
# p_SRR <- ggplot(rec_ssb2, aes(x=ssb, y=rec, label=year)) +
#   geom_point(aes(color=year)) +
#   geom_path(aes(color=year)) +
#   geom_text(check_overlap = TRUE,   nudge_y = 7000) +
#   scale_colour_viridis_c() +
#   xlim(0, 60000) +
#   ylim(0, 250000) +
#   ggtitle(paste0("Assessment year ", assyear)) +
#   theme_bw() 
# p_SRR
# 
# ggsave(p_SRR, file= paste0("report/Rec_ssb3 retro 1980 - ", assyear, ".png"), width = 18, height = 13, units = "cm", dpi = 300)
# 
# 
# # ===
# rec_ssb <- rec_age1$data / ssb$data
# rec_ssb <- data.frame(year = rec_age1$year, data= rec_ssb)
# rec_ssb2 <- data.frame(year = rec_age1$year, ssb = ssb$data, rec = rec_age1$data)
# 
# p_rec_ssb <- ggplot(rec_ssb, aes(x=year, y=data)) +
#   geom_line() +
#   geom_point() +
#   ylab("rec / ssb") +
#   ggtitle(paste0("Assessment year ", assyear)) +
#   theme_bw()
# p_rec_ssb
# 
# ggsave(p_rec_ssb, file= paste0("report/Rec_ssb4 retro 1980 - ", assyear, ".png"), width = 18, height = 13, units = "cm", dpi = 300)
assyear <- 2022
library(ggplot2)
  
# Weight at age 2024
stf_f_curr.catch.wt <- as.data.frame(catch.wt(ass.stock)[,ac((assyear-7):(assyear+1))]) 
stf_f_curr.catch.wt$cat <- "catch weight"
stf_f_curr.stock.wt <- as.data.frame(stock.wt(ass.stock)[,ac((assyear-7):(assyear+1))])
stf_f_curr.stock.wt$cat <- "stock weight"

stf_f_curr.weight <- rbind(stf_f_curr.catch.wt, stf_f_curr.stock.wt)


stf_f_curr.wt <- subset(stf_f_curr.weight, year %in% c(2021:2023))
# stf_f_curr.wt.mean <- subset(stf_f_curr.weight, year==2024)
stf_f_curr.wt.mean2 <- subset(stf_f_curr.weight, year==2022)


p_wt_age <- ggplot(stf_f_curr.wt, aes(age, data, color = as.factor(year))) +
  geom_point() +
  geom_line() +
  # geom_line(data=stf_f_curr.wt.mean, aes(age, data), color = "tomato", linetype = "dashed") +
  # geom_point(data=stf_f_curr.wt.mean, aes(age, data), color = "tomato") +
  ylab("Weight") +
  xlab("age") +
  facet_wrap(.~cat) +
  scale_color_viridis_d(name="Year") +
  # facet_wrap(.~year, ncol = 3) +
  # ggtitle("F at age") +
  theme_bw() 
p_wt_age

ggsave(p_wt_age, file= paste0("results/ACOM_request/Comparison catch and weight at age", assyear, ".png"), width = 20, height = 10, units = "cm", dpi = 300)

