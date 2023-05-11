#Based on grapghs with all windows, which have min or max charge, the following zones can be set: 
#Min charge:
####1 zone: x 0-15 y 19-50
####2 zone: y < 20

#Max charge:
###Only one zone 
###########Gramnegative
#Alkalilacustris 
dist_alk <- new_min_max_gramneg[new_min_max_gramneg$genus=="Alkalilacustris",]
#MIN
#zone 1
g1 <- ggplot(dist_alk[dist_alk$n_reg <15 & dist_alk$charge_min > 19,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightgreen", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 1") + 
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

#zone2
g2 <- ggplot(dist_alk[dist_alk$charge_min < 20,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightpink", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 2") + 
  theme(plot.title = element_text(hjust = 0.5, size =10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

g3 <- ggarrange(g1,g2, ncol=2, nrow=1, align="v")  
annotate_figure(g3, top = text_grob("Alkalilacustris", 
                                    color = "black", size = 13))

#MAX
ggplot(dist_alk, aes(x=max_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightblue", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение максимального заряда в зоне 1, Alkalilacustris") + 
  theme(plot.title = element_text(hjust = 0.5, size =12))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

#Ameyamaea 
dist_ame <- new_min_max_gramneg[new_min_max_gramneg$genus=="Ameyamaea",]
#MIN
#zone 1
g4 <- ggplot(dist_ame[dist_ame$n_reg <15 & dist_ame$charge_min > 19,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightgreen", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 1") + 
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

#zone2
g5 <- ggplot(dist_ame[dist_ame$charge_min < 20,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightpink", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 2") + 
  theme(plot.title = element_text(hjust = 0.5, size =10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

g6 <- ggarrange(g4,g5, ncol=2, nrow=1, align="v")  
annotate_figure(g6, top = text_grob("Ameyamaea", 
                                    color = "black", size = 13))

#MAX
ggplot(dist_ame, aes(x=max_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightblue", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение максимального заряда в зоне 1, Ameyamaea") + 
  theme(plot.title = element_text(hjust = 0.5, size =12))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))


#Aquidulcibacter
dist_aqu <- new_min_max_gramneg[new_min_max_gramneg$genus=="Aquidulcibacter",]
#MIN
#zone 1
g7 <- ggplot(dist_aqu[dist_aqu$n_reg <15 & dist_aqu$charge_min > 19,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightgreen", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 1") + 
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

#zone2
g8 <- ggplot(dist_aqu[dist_aqu$charge_min < 20,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightpink", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 2") + 
  theme(plot.title = element_text(hjust = 0.5, size =10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

g9 <- ggarrange(g7,g8, ncol=2, nrow=1, align="v")  
annotate_figure(g9, top = text_grob("Aquidulcibacter", 
                                    color = "black", size = 13))

#MAX
ggplot(dist_aqu, aes(x=max_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightblue", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение максимального заряда в зоне 1, Aquidulcibacter") + 
  theme(plot.title = element_text(hjust = 0.5, size =12))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))


#Cucumibacter
dist_cuc <- new_min_max_gramneg[new_min_max_gramneg$genus=="Cucumibacter",]
#MIN
#zone 1
g10 <- ggplot(dist_cuc[dist_cuc$n_reg <15 & dist_cuc$charge_min > 19,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightgreen", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 1") + 
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

#zone2
g11 <- ggplot(dist_cuc[dist_cuc$charge_min < 20,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightpink", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 2") + 
  theme(plot.title = element_text(hjust = 0.5, size =10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

g12 <- ggarrange(g10,g11, ncol=2, nrow=1, align="v")  
annotate_figure(g12, top = text_grob("Cucumibacter", 
                                    color = "black", size = 13))

#MAX
ggplot(dist_cuc, aes(x=max_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightblue", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение максимального заряда в зоне 1, Cucumibacter") + 
  theme(plot.title = element_text(hjust = 0.5, size =12))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

#Halovulum
dist_hal <- new_min_max_gramneg[new_min_max_gramneg$genus=="Halovulum",]
#MIN
#zone 1
g13 <- ggplot(dist_hal[dist_hal$n_reg <15 & dist_hal$charge_min > 19,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightgreen", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 1") + 
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

#zone2
g14 <- ggplot(dist_hal[dist_hal$charge_min < 20,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightpink", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 2") + 
  theme(plot.title = element_text(hjust = 0.5, size =10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

g15 <- ggarrange(g13,g14, ncol=2, nrow=1, align="v")  
annotate_figure(g15, top = text_grob("Halovulum", 
                                    color = "black", size = 13))

#MAX
ggplot(dist_hal, aes(x=max_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightblue", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение максимального заряда в зоне 1, Halovulum") + 
  theme(plot.title = element_text(hjust = 0.5, size =12))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

#Hellea
dist_hel <- new_min_max_gramneg[new_min_max_gramneg$genus=="Hellea",]
#MIN
#zone 1
g16 <- ggplot(dist_hel[dist_hel$n_reg <15 & dist_hel$charge_min > 19,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightgreen", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 1") + 
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

#zone2
g17 <- ggplot(dist_hel[dist_hel$charge_min < 20,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightpink", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 2") + 
  theme(plot.title = element_text(hjust = 0.5, size =10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

g18 <- ggarrange(g16,g17, ncol=2, nrow=1, align="v")  
annotate_figure(g18, top = text_grob("Hellea", 
                                    color = "black", size = 13))

#MAX
ggplot(dist_hel, aes(x=max_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightblue", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение максимального заряда в зоне 1, Hellea") + 
  theme(plot.title = element_text(hjust = 0.5, size =12))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))


#Limibacillus
dist_lim <- new_min_max_gramneg[new_min_max_gramneg$genus=="Limibacillus",]
#MIN
#zone 1
g19 <- ggplot(dist_lim[dist_lim$n_reg <15 & dist_lim$charge_min > 19,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightgreen", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 1") + 
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

#zone2
g20 <- ggplot(dist_lim[dist_lim$charge_min < 20,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightpink", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 2") + 
  theme(plot.title = element_text(hjust = 0.5, size =10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

g21 <- ggarrange(g19,g20, ncol=2, nrow=1, align="v")  
annotate_figure(g21, top = text_grob("Limibacillus", 
                                     color = "black", size = 13))

#MAX
ggplot(dist_lim, aes(x=max_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightblue", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение максимального заряда в зоне 1, Limibacillus") + 
  theme(plot.title = element_text(hjust = 0.5, size =12))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))


#Luteithermobacter
dist_lut <- new_min_max_gramneg[new_min_max_gramneg$genus=="Luteithermobacter",]
#MIN
#zone 1
g22 <- ggplot(dist_lut[dist_lut$n_reg <15 & dist_lut$charge_min > 19,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightgreen", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 1") + 
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

#zone2
g23 <- ggplot(dist_lut[dist_lut$charge_min < 20,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightpink", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 2") + 
  theme(plot.title = element_text(hjust = 0.5, size =10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

g24 <- ggarrange(g22,g23, ncol=2, nrow=1, align="v")  
annotate_figure(g24, top = text_grob("Luteithermobacter", 
                                     color = "black", size = 13))

#MAX
ggplot(dist_lut, aes(x=max_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightblue", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение максимального заряда в зоне 1, Luteithermobacter") + 
  theme(plot.title = element_text(hjust = 0.5, size =12))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

#Neptunicoccus
dist_nep <- new_min_max_gramneg[new_min_max_gramneg$genus=="Neptunicoccus",]
#MIN
#zone 1
g25 <- ggplot(dist_nep[dist_nep$n_reg <15 & dist_nep$charge_min > 19,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightgreen", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 1") + 
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

#zone2
g26 <- ggplot(dist_nep[dist_nep$charge_min < 20,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightpink", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 2") + 
  theme(plot.title = element_text(hjust = 0.5, size =10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

g27 <- ggarrange(g25,g26, ncol=2, nrow=1, align="v")  
annotate_figure(g27, top = text_grob("Neptunicoccus", 
                                     color = "black", size = 13))

#MAX
ggplot(dist_nep, aes(x=max_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightblue", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение максимального заряда в зоне 1, Neptunicoccus") + 
  theme(plot.title = element_text(hjust = 0.5, size =12))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

#Nguyenibacter
dist_ngu <- new_min_max_gramneg[new_min_max_gramneg$genus=="Nguyenibacter",]
#MIN
#zone 1
g28 <- ggplot(dist_ngu[dist_ngu$n_reg <15 & dist_ngu$charge_min > 19,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightgreen", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 1") + 
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.6))

#zone2
g29 <- ggplot(dist_ngu[dist_ngu$charge_min < 20,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightpink", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 2") + 
  theme(plot.title = element_text(hjust = 0.5, size =10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.6))
                     
g30 <- ggarrange(g28,g29, ncol=2, nrow=1, align="v")  
annotate_figure(g30, top = text_grob("Nguyenibacter", 
                                     color = "black", size = 13))

#MAX
ggplot(dist_ngu, aes(x=max_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightblue", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение максимального заряда в зоне 1, Nguyenibacter") + 
  theme(plot.title = element_text(hjust = 0.5, size =12))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

#Parapontixanthobacter
dist_par <- new_min_max_gramneg[new_min_max_gramneg$genus=="Parapontixanthobacter",]
#MIN
#zone 1
g31 <- ggplot(dist_par[dist_par$n_reg <15 & dist_par$charge_min > 19,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightgreen", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 1") + 
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  scale_x_continuous(breaks=seq(-12,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

#zone2
g32 <- ggplot(dist_par[dist_par$charge_min < 20,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightpink", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 2") + 
  theme(plot.title = element_text(hjust = 0.5, size =10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

g33 <- ggarrange(g31,g32, ncol=2, nrow=1, align="v")  
annotate_figure(g33, top = text_grob("Parapontixanthobacter", 
                                     color = "black", size = 13))

#MAX
ggplot(dist_par, aes(x=max_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightblue", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение максимального заряда в зоне 1, Parapontixanthobacter") + 
  theme(plot.title = element_text(hjust = 0.5, size =12))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

#Phaeovibrio
dist_pha <- new_min_max_gramneg[new_min_max_gramneg$genus=="Phaeovibrio",]
#MIN
#zone 1
g34 <- ggplot(dist_pha[dist_pha$n_reg <15 & dist_pha$charge_min > 19,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightgreen", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 1") + 
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

#zone2
g35 <- ggplot(dist_pha[dist_pha$charge_min < 20,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightpink", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 2") + 
  theme(plot.title = element_text(hjust = 0.5, size =10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

g36 <- ggarrange(g34,g35, ncol=2, nrow=1, align="v")  
annotate_figure(g36, top = text_grob("Phaeovibrio", 
                                     color = "black", size = 13))

#MAX
ggplot(dist_pha, aes(x=max_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightblue", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение максимального заряда в зоне 1, Phaeovibrio") + 
  theme(plot.title = element_text(hjust = 0.5, size =12))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

#Rhizorhapis
dist_rhi <- new_min_max_gramneg[new_min_max_gramneg$genus=="Rhizorhapis",]
#MIN
#zone 1
g37 <- ggplot(dist_rhi[dist_rhi$n_reg <15 & dist_rhi$charge_min > 19,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightgreen", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 1") + 
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

#zone2
g38 <- ggplot(dist_rhi[dist_rhi$charge_min < 20,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightpink", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 2") + 
  theme(plot.title = element_text(hjust = 0.5, size =10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

g39 <- ggarrange(g37,g38, ncol=2, nrow=1, align="v")  
annotate_figure(g39, top = text_grob("Rhizorhapis", 
                                     color = "black", size = 13))

#MAX
ggplot(dist_rhi, aes(x=max_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightblue", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение максимального заряда в зоне 1, Rhizorhapis") + 
  theme(plot.title = element_text(hjust = 0.5, size =12))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

#Rubricella
dist_rub <- new_min_max_gramneg[new_min_max_gramneg$genus=="Rubricella",]
#MIN
#zone 1
g40 <- ggplot(dist_rub[dist_rub$n_reg <15 & dist_rub$charge_min > 19,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightgreen", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 1") + 
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.8))

#zone2
g41 <- ggplot(dist_rub[dist_rub$charge_min < 20,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightpink", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 2") + 
  theme(plot.title = element_text(hjust = 0.5, size =10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.8))

g42 <- ggarrange(g40,g41, ncol=2, nrow=1, align="v")  
annotate_figure(g42, top = text_grob("Rubricella", 
                                     color = "black", size = 13))

#MAX
ggplot(dist_rub, aes(x=max_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightblue", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение максимального заряда в зоне 1, Rubricella") + 
  theme(plot.title = element_text(hjust = 0.5, size =12))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

#Thermithiobacillus
dist_the <- new_min_max_gramneg[new_min_max_gramneg$genus=="Thermithiobacillus",]
#MIN
#zone 1
g43 <- ggplot(dist_the[dist_the$n_reg <15 & dist_the$charge_min > 19,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightgreen", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 1") + 
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.6))

#zone2
g44 <- ggplot(dist_the[dist_the$charge_min < 20,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightpink", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 2") + 
  theme(plot.title = element_text(hjust = 0.5, size =10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.6))

g45 <- ggarrange(g43,g44, ncol=2, nrow=1, align="v")  
annotate_figure(g45, top = text_grob("Thermithiobacillus", 
                                     color = "black", size = 13))

#MAX
ggplot(dist_the, aes(x=max_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightblue", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение максимального заряда в зоне 1, Thermithiobacillus") + 
  theme(plot.title = element_text(hjust = 0.5, size =12))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

#Woodsholea
dist_woo <- new_min_max_gramneg[new_min_max_gramneg$genus=="Woodsholea",]
#MIN
#zone 1
g46 <- ggplot(dist_woo[dist_woo$n_reg <15 & dist_woo$charge_min > 19,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightgreen", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 1") + 
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

#zone2
g47 <- ggplot(dist_woo[dist_woo$charge_min < 20,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightpink", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 2") + 
  theme(plot.title = element_text(hjust = 0.5, size =10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

g48 <- ggarrange(g46,g47, ncol=2, nrow=1, align="v")  
annotate_figure(g48, top = text_grob("Woodsholea", 
                                     color = "black", size = 13))

#MAX
ggplot(dist_woo, aes(x=max_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightblue", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение максимального заряда в зоне 1, Woodsholea") + 
  theme(plot.title = element_text(hjust = 0.5, size =12))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

#######E.coli
#MIN
#zone 1
g49 <- ggplot(new_min_max_ecoli[new_min_max_ecoli$n_reg <15 & new_min_max_ecoli$charge_min > 19,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightgreen", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 1") + 
  theme(plot.title = element_text(hjust = 0.5, size=10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

#zone2
g50 <- ggplot(new_min_max_ecoli[new_min_max_ecoli$charge_min < 20,], aes(x=min_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightpink", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение минимального заряда в зоне 2") + 
  theme(plot.title = element_text(hjust = 0.5, size =10))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))

g51 <- ggarrange(g49,g50, ncol=2, nrow=1, align="v")  
annotate_figure(g51, top = text_grob("Escherichia coli", 
                                     color = "black", size = 13))

#MAX
ggplot(new_min_max_ecoli, aes(x=max_charge_value))+
  geom_histogram(aes(y=..density..),alpha=.7, fill="lightblue", col= "black", binwidth= 1)+
  labs(x="Заряд", y="Плотность распределения", title= "Распределение максимального заряда в зоне 1, Escherichia coli") + 
  theme(plot.title = element_text(hjust = 0.5, size =12))+
  scale_x_continuous(breaks=seq(-10,10,1))+
  scale_y_continuous(limits=c(0, 0.5))




