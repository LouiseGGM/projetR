#exploration hypothèse une : 
#La technologie a permis le maintien d’un certain lien social, 
#mais son visage reste différentiel selon l’âge. → bon maintien jusqu’à 40 ans.
library(tidyselect)
library(dplyr)
library(tidyverse)
library(questionr)

#Nombre de personnes vivant dans le logement 
freq(coco$coco1_q23)
coco$coco1_q23[coco$coco1_q23 ==2.6] <- 3 #On transforme le 2,6 en 3, surtout que ne concerne q'un enquêté.e
#Peu de ménages comptent plus de 4 individus
#La plus grande part des foyers sont des ménages de 2 personnes : couple ou parent solo+enf

# eayy_A2A_rec --> Age quinquennal
freq(coco$eayy_A2A_rec)
coco$Age_rec <- fct_collapse(factor(coco$eayy_A2A_rec),"-24"="4","25-29"="5",
                             "30-34"="6","35-39"="7", "40-44"="8", "45-49"="9", "50-54"="10",
                             "55-59"="11", "60-64"="12", "65-79"="13", "+70"="14")


# coco1_q28 --> nb d'ordi /!\ Score d'équipement numérique
freq(coco$coco1_q28)
#La majorité des gens ont entre 2 et 3 ordis (env 68%)

#tris croisés nb ordi et nb de gens par foyer
addmargins(table(coco$coco1_q23, coco$coco1_q28))
#En ligne le nb de pers / foyer et en col le nb d'ordi
#Parmi les 175 pers vivant seule, 119 ont 2 ordinateurs
#les pers vivant à 2 ont entre 2 et 3 ordi (respectivement 144,139)
#le nombre de personnes vivant dans le foyer augmentant, le nb d'ordi stagne autour de 3, 
    # logique si on pense aux familles 

rtable(table(coco$eayy_A2A_rec,coco$coco1_q28))


# coco1_q29 --> nb de smartphone/tablette /!\ Score d'équipement numérique
freq(coco$coco1_q29)
median(coco$coco1_q29) #mediane : 4
mean(coco$coco1_q29) #Moyenne de 51 ???
#53 % des gens ont entre 3 et 4 smartphones et/ ou tablettes

#tris croisés nb smartphones/tab et nb de gens par foyer
addmargins(table(coco$coco1_q23, coco$coco1_q29))
#En ligne le nb de pers / foyer et en col le nb d'ordi
#La majorité des consommateurs de téléphone / tablette sont les foyers de 2 pers
#Ils en ont entre 3 et 4 soit un smartphone chacun + 1 tablette pour 2 ou une chacun
#Le score d'équipement numérique est plus sensible à l'augmentation du nombre de personne dans le foyer.
#Les valeurs sont plus répartis que dans le cas des ordinateurs.

addmargins(table(coco$eayy_A2A_rec,coco$coco1_q29))
#Les plus de 70 ans sont bien équipés en smartphones et tablettes (2,3,4), ce sont eux qui répondent le plus
#Les 35-60 sont les plus équipés en nombre par foyers


# coco1_q31 --> bonne connexion internet /!\ Score d'équipement numérique
freq(coco$coco1_q31)
#Beaucoup de NR : 282 6666 soit 33.3% de NR

# coco1_q47 --> fréquences des sorties au cours des 2 dernières semaines
freq(coco$coco1_q47)
freq(coco$coco2_q47)
freq(coco$coco3_q47)

addmargins(table(coco$coco1_q23,coco$coco1_q29))
addmargins(table(coco$eayy_A2A_rec,coco$coco1_q29))


#repartition proche entre avant et après
#fréquence de visite des ami.e.s
# coco1_q52_1 --> fréquence des rencontres avec des amis dans la vie réelle /!\ combiner famille et amis 
freq(coco$coco1_q47)

# coco1_q52_2 --> fréquence des rencontres avec des amis virtuellement
freq(coco$coco1_q47)

# coco1_q59_7 --> temps consacré à parler au telephone
freq(coco$coco1_q47)

# coco1_q59_8 --> temps consacré à utiliser les réseau sociaux
freq(coco$coco1_q47)

# coco1_q61_7 --> temps consacré par rappport à d'habitude à parler au telephone
freq(coco$coco1_q47)

# coco1_q61_8 --> temps consacré par rappport à d'habitude à utiliser les réseau sociaux
freq(coco$coco1_q47)

