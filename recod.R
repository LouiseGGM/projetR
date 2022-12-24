library(tidyverse)
library(questionr)

#table femmes uniquement ----
cocofemme <- coco %>% filter(eayy_A1 == "2")
freq(cocofemme$coco1_q21)


#tris croisés pondérés ----
#Solitude ----
#solitude/enfants?  (1 oui 2 non)

wtd.table(coco$eayy_C8, coco$coco1_q21, 
          weights = coco$coco1_POIDS, useNA = "ifany") %>% 
  cprop(digits=1)

wtd.table(cocofemme$eayy_C8, cocofemme$coco1_q21, 
          weights = cocofemme$coco1_POIDS, useNA = "ifany") %>% 
  cprop(digits=1)


#solitude/nb enfant

wtd.table(coco$eayy_C8A_rec, coco$coco1_q21, 
          weights = coco$coco1_POIDS, useNA = "ifany") %>% 
  cprop(digits=1)

wtd.table(cocofemme$eayy_C8A_rec, cocofemme$coco1_q21, 
          weights = cocofemme$coco1_POIDS, useNA = "ifany") %>% 
  cprop(digits=1)

#solitude/nb enfant cohabitants

wtd.table(coco$eayy_C8B_rec, coco$coco1_q21, 
          weights = coco$coco1_POIDS, useNA = "ifany") %>% 
  cprop(digits=1)

wtd.table(cocofemme$eayy_C8B_rec, cocofemme$coco1_q21, 
          weights = cocofemme$coco1_POIDS, useNA = "ifany") %>% 
  cprop(digits=1)

#Fréquence des sorties ----
# coco1_q47 --> fréquences des sorties 1 Jamais/2 Une fois par semaine ou -/
#3 Une fois tous les 2-3 jours/4 Tous les jours ou presque


#Sortie/enfants?  (1 oui 2 non)

wtd.table(coco$eayy_C8, coco$coco1_q47, 
          weights = coco$coco1_POIDS, useNA = "ifany") %>% 
  cprop(digits=1)

wtd.table(cocofemme$eayy_C8, cocofemme$coco1_q47, 
          weights = cocofemme$coco1_POIDS, useNA = "ifany") %>% 
  cprop(digits=1)

#sortie/nb enfant

wtd.table(coco$eayy_C8A_rec, coco$coco1_q47, 
          weights = coco$coco1_POIDS, useNA = "ifany") %>% 
  cprop(digits=1)

wtd.table(cocofemme$eayy_C8A_rec, cocofemme$coco1_q47, 
          weights = cocofemme$coco1_POIDS, useNA = "ifany") %>% 
  cprop(digits=1)

#sortie/nb enfant cohabitants

wtd.table(coco$eayy_C8B_rec, coco$coco1_q47, 
          weights = coco$coco1_POIDS, useNA = "ifany") %>% 
  cprop(digits=1)

wtd.table(cocofemme$eayy_C8B_rec, cocofemme$coco1_q47, 
          weights = cocofemme$coco1_POIDS, useNA = "ifany") %>% 
  cprop(digits=1)

# Stress lié au confinement ----
# coco1_q64 --> stress lié au confinement (échelle)

wtd.table(coco$eayy_A1, coco$coco1_q64, 
          weights = coco$coco1_POIDS, useNA = "ifany") %>% 
  cprop(digits=1)

#Stress/enfants?  (1 oui 2 non)

wtd.table(coco$eayy_C8, coco$coco1_q64, 
          weights = coco$coco1_POIDS, useNA = "ifany") %>% 
  cprop(digits=1)

wtd.table(cocofemme$eayy_C8, cocofemme$coco1_q64, 
          weights = cocofemme$coco1_POIDS, useNA = "ifany") %>% 
  cprop(digits=1)

#stress/nb enfant
#faire un bockspot avec les don pondérées

wtd.table(coco$eayy_C8A_rec, coco$coco1_q64, 
          weights = coco$coco1_POIDS, useNA = "ifany") %>% 
  cprop(digits=1)

wtd.table(cocofemme$eayy_C8A_rec, cocofemme$coco1_q64, 
          weights = cocofemme$coco1_POIDS, useNA = "ifany") %>% 
  cprop(digits=1)

#stress/nb enfant cohabitants

wtd.table(coco$eayy_C8B_rec, coco$coco1_q64, 
          weights = coco$coco1_POIDS, useNA = "ifany") %>% 
  cprop(digits=1)

wtd.table(cocofemme$eayy_C8B_rec, cocofemme$coco1_q64, 
          weights = cocofemme$coco1_POIDS, useNA = "ifany") %>% 
  cprop(digits=1)

#Attentes sur les relations sociales ----
# coco1_q83_6 --> attente que le confinement affecte ses relations sociales
#1 pas du tout/un peu/assez/bcp

#Relations sociales/enfants?  (1 oui 2 non)

wtd.table(coco$eayy_C8, coco$coco1_q83_6, 
          weights = coco$coco1_POIDS, useNA = "ifany") %>% 
  cprop(digits=1)

wtd.table(cocofemme$eayy_C8, cocofemme$coco1_q83_6, 
          weights = cocofemme$coco1_POIDS, useNA = "ifany") %>% 
  cprop(digits=1)

#Relations sociales/nb enfant

wtd.table(coco$eayy_C8A_rec, coco$coco1_q83_6, 
          weights = coco$coco1_POIDS, useNA = "ifany") %>% 
  cprop(digits=1)

wtd.table(cocofemme$eayy_C8A_rec, cocofemme$coco1_q83_6, 
          weights = cocofemme$coco1_POIDS, useNA = "ifany") %>% 
  cprop(digits=1)

#Relations sociales/nb enfant cohabitants 

wtd.table(coco$eayy_C8B_rec, coco$coco1_q83_6, 
          weights = coco$coco1_POIDS, useNA = "ifany") %>% 
  cprop(digits=1)

wtd.table(cocofemme$eayy_C8B_rec, cocofemme$coco1_q83_6, 
          weights = cocofemme$coco1_POIDS, useNA = "ifany") %>% 
  cprop(digits=1)