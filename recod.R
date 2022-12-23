#RECODAGE P1

# solitude
coco$coco1_q21 <- fct_collapse(factor(coco$coco1_q21),"Très seul"="1","Plutôt seul"="2",
                               "Plutôt entouré"="3","Très entouré"="4")
freq(coco$coco1_q21)

# coco1_q37 --> situation pro pendant le confinementsolitude

coco$coco1_q37[coco$coco1_q37 %in% c("1","2","3")]<-"Congés maladie parental"
coco$coco1_q37[coco$coco1_q37 %in% c("4","5")]<-"Chômage"               

coco$coco1_q23[coco$coco1_q23 ==2.6] <- 3

coco$coco1_q28[coco$coco1_q28 %in% c("1","2","3")]<-"Congés maladie parental"
table(coco$coco1_q37)
freq(coco$coco1_q23) #recodage n'a pas fonctionné

# coco1_q28 --> nb d'ordi /!\ Score d'éqyupement numérique
# coco1_q29 --> nb de smartphone/tablette /!\ Score d'éqyupement numérique
# coco1_q31 --> bonne connexion internet /!\ Score d'éqyupement numérique


#nb d'ordi par personne
class(coco$coco1_q28) #class character
coco$coco1_q28 <- as.factor(coco$coco1_q28) #devient un factor
freq(coco$coco1_q28) #modalités 4,5,6, 9999 + NA

coco$coco1_q28_rap <- (coco$coco1_q28/coco$coco1_q23)
#  Warning : ‘/’ n'est pas pertinent pour des variables facteurs

freq(coco$coco1_q28_rap) #847 NA

#en dessous de 1 ordi pour trois --> 0
#moins de 1 --> 1
#1 (inclu) --> 2

#nb de smartphone par personne
coco$coco1_q29_rap <- (coco$coco1_q29/coco$coco1_q23)
freq(coco$coco1_q31) #mod 1, 2 et 6666

#internet --> on peu pas parce que c'est une question filtre
# coco$coco1_q31[coco$coco1_q31 %in% c(2)]<-0
# 
# coco$coco1_score_num[is.na(coco$coco1_q28) | is.na(coco$coco1_q29)]<-NA
# freq(coco$coco1_score_num)

#reçu de l'aide
class(coco$coco1_q54) #NULL

coco$coco1_q54 [coco$coco1_q54_1==1 | coco$coco1_q54_2==1 | coco$coco1_q54_3==1 |
                  coco$coco1_q54_4==1 | coco$coco1_q54_5==1 | coco$coco1_q54_6==1 |
                  coco$coco1_q54_7==1 ] <- "OUI"
freq(coco$coco1_q54)

coco$coco1_q54 [coco$coco1_q54_8==1] <- "NON"
coco$coco1_q54[is.na(coco$coco1_q54) ]<-"NON"

#porté de l'aide
coco$coco1_q54_bis [coco$coco1_q54_bis_1==1 | coco$coco1_q54_bis_2==1 | coco$coco1_q54_bis_3==1 |
                      coco$coco1_q54_bis_4==1 | coco$coco1_q54_bis_5==1 | coco$coco1_q54_bis_6==1 |
                      coco$coco1_q54_bis_7==1 ] <- "OUI"

coco$coco1_q54_bis [coco$coco1_q54_bis_8==1] <- "NON"
coco$coco1_q54_bis[is.na(coco$coco1_q54_bis)]<-"NON"
freq(coco$coco1_q54_bis)
class(coco$coco1_q54_bis) #NULL


# coco1_q52_1 --> fréquence des rencontres avec des amis dans la vie réelle /!\ combiner famille et amis 
# coco1_q52_2 --> fréquence des rencontres avec des amis virtuellement
# coco1_q52_3 --> famille IRL
# coco1_q52_4 --> famille IVL
# coco1_q53_1 --> fréquence contact famille à l'étranger
# coco1_q53_2 --> amis à l'étranger
# coco1_q53_3 --> collègues à l'étranger

class(coco$coco1_q52_rel) #NULL
coco$coco1_q52_rel <- as.factor()
coco$coco1_q52_rel <- ifelse(coco$coco1_q52_1 %in% c("1","2") | coco$coco1_q52_3 %in% c("1","2"), 
                             "Beaucoup de contact IRL", coco$coco1_q52_rel)
coco$coco1_q52_rel <- ifelse(coco$coco1_q52_2 %in% c("1","2") | coco$coco1_q52_4 %in% c("1","2"), 
                             "Beaucoup de contact IVL", coco$coco1_q52_rel)
coco$coco1_q52_rel <- ifelse(coco$coco1_q53_1 %in% c("1","2") | coco$coco1_q53_2 %in% c("1","2")| 
                               coco$coco1_q53_3 %in% c("1","2"), "Beaucoup de contact à l'étranger", 
                             coco$coco1_q52_rel)

coco$coco1_q52_rel <- ifelse(coco$coco1_q52_1 %in% c("3","4") | coco$coco1_q52_3 %in% c("3","4"), 
                             "un peu de contact IRL", 
                             coco$coco1_q52_rel)
coco$coco1_q52_rel <- ifelse(coco$coco1_q52_2 %in% c("3","4") | coco$coco1_q52_4 %in% c("3","4"), 
                             "un peu de contact IVL", 
                             coco$coco1_q52_rel)
coco$coco1_q52_rel <- ifelse(coco$coco1_q53_1 %in% c("3","4") | coco$coco1_q53_2 %in% c("3","4")| 
                               coco$coco1_q53_3 %in% c("3","4"), "un peu de contact à l'étranger", 
                             coco$coco1_q52_rel)


coco$coco1_q52_rel <- ifelse(coco$coco1_q52_1 == 5 | coco$coco1_q52_3 == 5, 
                             "pas de contact", 
                             coco$coco1_q52_rel)
coco$coco1_q52_rel <- ifelse(coco$coco1_q52_2 == 5 | coco$coco1_q52_4 == 5, 
                             "pas de contact", 
                             coco$coco1_q52_rel)
coco$coco1_q52_rel <- ifelse(coco$coco1_q53_1 == 5 | coco$coco1_q53_2 == 5| 
                               coco$coco1_q53_3 == 5, "pas de contact", 
                             coco$coco1_q52_rel)

coco$coco1_q52_rel[is.na(coco$coco1_q52_rel)]<-"pas de contact"
freq(coco$coco1_q52_rel)