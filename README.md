# projetR
réunion du 21 décembre :
les identitfiants anonymes semblent propre à un individu donc ok pour l'appariement
apparier les ind si au moins une fois (voir au moins 2 fois)

left join sur R : merge(base1, base2, all.x=TRUE) on peut rajouter un by='clef' avant le all.x
left join avec dplyr : dplyr::left_join(base1, base2)

si pas de réponse à ay regarder sur la colonne suivante

Recoder en utilisant Rbase coco1$coco1_q37[coco1$coco1_q37 %in% c("1","2","3")]<-"Congés maladie parental"
Recoder les NA coco1$score[is.na(coco1$q28) | is.na(coco1$q29) | is.na(coco1$q31)]<-NA

{}[]

Pour garder les col qui ont le nom coco2 (sup aussi l'id)
coco2a <- coco2[,grepl("coco2", names(coco2))]
Pour sup les col qui ont le noms eavy (pour garder l'id)
coco2a <- coco2[,!grepl("eayy", names(coco2))]
