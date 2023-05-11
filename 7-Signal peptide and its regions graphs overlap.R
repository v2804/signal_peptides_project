#Накладываем графики, чтобы убедиться в том, что только N-регоин вносит вклад в исследуемую зависимость 

########Gramnegative

colors <- c("Сигнальный пептид"="blue", "N-регион"="red", "H-регион"="lightgreen", "C-регион"="lightblue")
#Dataset1 
ggplot(new_min_max_gramneg1, aes(x = cleav_site, y = charge_min, col="Сигнальный пептид"))+ 
  geom_point( alpha=.3)+
  geom_point(aes(x= h_reg, y= charge_min, col='H-регион'), alpha =.3)+
  geom_point(aes(x= c_reg, y= charge_min, col='C-регион'),alpha =.3)+
  geom_point(aes(x= n_reg, y= charge_min, col='N-регион'), alpha =.3)+
  xlab("Длина региона, АК остатки")+
  ylab("Длина окна АК последовательности с минимальным зарядом, АК остатки")+
  facet_wrap(~genus)+
  scale_color_manual(values=colors, name="Длина региона")

ggplot(new_min_max_gramneg1, aes(x = cleav_site, y = charge_max, col="Сигнальный пептид"))+ 
  geom_point( alpha=.3)+
  geom_point(aes(x= h_reg, y= charge_max, col='H-регион'), alpha =.3)+
  geom_point(aes(x= c_reg, y= charge_max, col='C-регион'),alpha =.3)+
  geom_point(aes(x= n_reg, y= charge_max, col='N-регион'), alpha =.3)+
  xlab("Длина региона, АК остатки")+
  ylab("Длина окна АК последовательности с максимальным зарядом, АК остатки")+
  facet_wrap(~genus)+
  scale_color_manual(values=colors, name="Длина региона")

#Dataset2
ggplot(new_min_max_gramneg2, aes(x = cleav_site, y = charge_min, col="Сигнальный пептид"))+ 
  geom_point( alpha=.3)+
  geom_point(aes(x= h_reg, y= charge_min, col='H-регион'), alpha =.3)+
  geom_point(aes(x= c_reg, y= charge_min, col='C-регион'),alpha =.3)+
  geom_point(aes(x= n_reg, y= charge_min, col='N-регион'), alpha =.3)+
  xlab("Длина региона, АК остатки")+
  ylab("Длина окна АК последовательности с минимальным зарядом, АК остатки")+
  facet_wrap(~genus)+
  scale_color_manual(values=colors, name="Длина региона")

ggplot(new_min_max_gramneg2, aes(x = cleav_site, y = charge_max, col="Сигнальный пептид"))+ 
  geom_point( alpha=.3)+
  geom_point(aes(x= h_reg, y= charge_max, col='H-регион'), alpha =.3)+
  geom_point(aes(x= c_reg, y= charge_max, col='C-регион'),alpha =.3)+
  geom_point(aes(x= n_reg, y= charge_max, col='N-регион'), alpha =.3)+
  xlab("Длина региона, АК остатки")+
  ylab("Длина окна АК последовательности с максимальным зарядом, АК остатки")+
  facet_wrap(~genus)+
  scale_color_manual(values=colors, name="Длина региона")

#Dataset3
ggplot(new_min_max_gramneg3, aes(x = cleav_site, y = charge_min, col="Сигнальный пептид"))+ 
  geom_point( alpha=.3)+
  geom_point(aes(x= h_reg, y= charge_min, col='H-регион'), alpha =.3)+
  geom_point(aes(x= c_reg, y= charge_min, col='C-регион'),alpha =.3)+
  geom_point(aes(x= n_reg, y= charge_min, col='N-регион'), alpha =.3)+
  xlab("Длина региона, АК остатки")+
  ylab("Длина окна АК последовательности с минимальным зарядом, АК остатки")+
  facet_wrap(~genus)+
  scale_color_manual(values=colors, name="Длина региона")

ggplot(new_min_max_gramneg3, aes(x = cleav_site, y = charge_max, col="Сигнальный пептид"))+ 
  geom_point( alpha=.3)+
  geom_point(aes(x= h_reg, y= charge_max, col='H-регион'), alpha =.3)+
  geom_point(aes(x= c_reg, y= charge_max, col='C-регион'),alpha =.3)+
  geom_point(aes(x= n_reg, y= charge_max, col='N-регион'), alpha =.3)+
  xlab("Длина региона, АК остатки")+
  ylab("Длина окна АК последовательности с максимальным зарядом, АК остатки")+
  facet_wrap(~genus)+
  scale_color_manual(values=colors, name="Длина региона")

#Dataset4
ggplot(new_min_max_gramneg4, aes(x = cleav_site, y = charge_min, col="Сигнальный пептид"))+ 
  geom_point( alpha=.3)+
  geom_point(aes(x= h_reg, y= charge_min, col='H-регион'), alpha =.3)+
  geom_point(aes(x= c_reg, y= charge_min, col='C-регион'),alpha =.3)+
  geom_point(aes(x= n_reg, y= charge_min, col='N-регион'), alpha =.3)+
  xlab("Длина региона, АК остатки")+
  ylab("Длина окна АК последовательности с минимальным зарядом, АК остатки")+
  facet_wrap(~genus)+
  scale_color_manual(values=colors, name="Длина региона")

ggplot(new_min_max_gramneg4, aes(x = cleav_site, y = charge_max, col="Сигнальный пептид"))+ 
  geom_point( alpha=.3)+
  geom_point(aes(x= h_reg, y= charge_max, col='H-регион'), alpha =.3)+
  geom_point(aes(x= c_reg, y= charge_max, col='C-регион'),alpha =.3)+
  geom_point(aes(x= n_reg, y= charge_max, col='N-регион'), alpha =.3)+
  xlab("Длина региона, АК остатки")+
  ylab("Длина окна АК последовательности с максимальным зарядом, АК остатки")+
  facet_wrap(~genus)+
  scale_color_manual(values=colors, name="Длина региона")


#######E.coli
ggplot(new_min_max_ecoli, aes(x = cleav_site, y = charge_min, col="Сигнальный пептид"))+ 
  geom_point( alpha=.3)+
  geom_point(aes(x= h_reg, y= charge_min, col='H-регион'), alpha =.3)+
  geom_point(aes(x= c_reg, y= charge_min, col='C-регион'),alpha =.3)+
  geom_point(aes(x= n_reg, y= charge_min, col='N-регион'), alpha =.3)+
  xlab("Длина региона, АК остатки")+
  ylab("Длина окна АК последовательности с минимальным зарядом, АК остатки")+
  scale_color_manual(values=colors, name="Длина региона")

ggplot(new_min_max_ecoli, aes(x = cleav_site, y = charge_max, col="Сигнальный пептид"))+ 
  geom_point( alpha=.3)+
  geom_point(aes(x= h_reg, y= charge_max, col='H-регион'), alpha =.3)+
  geom_point(aes(x= c_reg, y= charge_max, col='C-регион'),alpha =.3)+
  geom_point(aes(x= n_reg, y= charge_max, col='N-регион'), alpha =.3)+
  xlab("Длина региона, АК остатки")+
  ylab("Длина окна АК последовательности с максимальным зарядом, АК остатки")+
  scale_color_manual(values=colors, name="Длина региона")










