library(Rcpp)
library(dtw)
library(timeSeries)
library(TSclust)
library(parallel)
library(caTools)
sourceCpp("./fonctions_de_distances.cpp")


chercherAGauche <- function(i, ai, di, Tab_W){
   k   <- i
   ak  <- ai

   while(k >= 1){

     if (Tab_W[(k - 1)] == -1){
       break
     }

     eval.parent(substitute(Tab_W[(k - 1)] <- -1))
     aux <- Un_NNPDTW(di, c((k - 1)))
     ak_1 <- aux[1, 2]

     if(ak_1 < ak){
       break
     }

     k   <- k - 1
     ak  <- ak_1
   }

   return(list(w_max = k, acc_max = ak))
 }
 
chercherADroite <- function(i, ai, di, Tab_W){

  k   <- i
  max <- i
  a_max <- ai

  n <- length(Tab_W)

  while( k <= n){

    if(k == n || Tab_W[(k + 1)] == -1){
      break
    }


    eval.parent(substitute(Tab_W[(k + 1)] <- -1))
    aux <- Un_NNPDTW(di, c((k + 1)))
    ak_plus1 <- aux[1, 2]

    if(ak_plus1 > a_max){
      max   <- k + 1
      a_max <- ak_plus1
    }

    k <- k + 1
  }

  return(list(w_max = max, acc_max = a_max))
}

maximunLocal    <- function(i, ai, di, Tab_W){

  aux <- Tab_W
  wg <- chercherAGauche(i, ai, di, aux)
  wd <- chercherADroite(i, ai, di, aux)
  
  long <- length(Tab_W)
  for(i in 1:long){
    eval.parent(substitute(Tab_W[i] <- aux[i]))
  }

  if(wg$acc_max > wd$acc_max){

    return(wg)

  }else{

    return(wd)

  }
}

divisieurs      <- function(n){

  i       <- 2
  cmpteur <- 1
  tab_d   <- c()

  while(i <= n){

    if( n %% i == 0){
      tab_d[cmpteur] <- i
      cmpteur <- cmpteur + 1
    }
    i <- i + 1
  }

  return(tab_d)
}

maximun         <- function(d){

  nb_lign <- dim(d)[1]
  nb_col  <- dim(d)[2]
  i       <- 2
  id_max  <- 1

  while(i <= nb_lign){
    if( d[id_max, 2] < d[i, 2]){
      id_max <- i
    }

    i <- i + 1

  }

  return(list(w_max = d[id_max, 1], acc_max = d[id_max, 2]))

}


ZnormalisationDeTS <- function(t){
  return (t - mean(t)) / sd(t) 
}

#EntrCB)e : 
# - t : une sCB)rie temporelle
#Sortie : 
# - sCB)rie temporelle sans les valeurs maquantes
SupprimerNA <- function(t){
  a <- data.frame(t(t))
  b <- na.omit(a)
  c <- t(b)
  c
}


# PrCB)dire la classe d'une sCB)rie temporelle CB  partir de celle de ses k plus proche voisins
#EntrCB)e : 
# - ind.TS : l'indice de la sCB)rie temporelle dont il faut prCB)dire la classe
# - training_d : le training_set
# - clId_training_d : classe du training set
# - matrice.distance : matrice de distance entre les TS du test_set (en ligne) et celles du training_set (en colonne)
#Sortie : 
# - La classe prCB)dite
PredirClassPAA<-function(ind.TS,training_d,k, clId_training_d, matrice.distance)
{
  distances <- matrix(nrow = dim(training_d)[1], ncol = 1)
  distances <- matrice.distance[ind.TS, ]
  
  s <- sort(as.vector(distances), index.return=TRUE)
  tb <- as.vector(clId_training_d[s$ix[1:(k)]])
  
  cl <- ClasseMajoritaire(tb, clId_training_d)
  
  return(cl)
}

#Calcul la somme du carrCB) des Erreurs
#EntrCB)e : 
# - Un vecteur de reels
#Sortie : 
# Somme du carrCB) des erreurs
SSE <- function(v){
  v_numeric <- as.numeric(v)
  taille    <- length(v_numeric)
  moy       <- mean(v_numeric)
  som       <- 0
  
  for(i in 1:taille){
    som <- som + (moy - v_numeric[i])^2
  }
  return(som)
}


# Calculer la somme des carrCB)es des erreurs d'un segment
#Entree : 
# - v : un vecteur de points
# - nbPoints : le nombre de points d'un segment
# - ind_debut : indice de dCB)but du segment
#Sortie : 
#somme du carrCB) des erreurs d'un segment
SSE_segment <- function(v, nbPoints, ind_debut){
  if((ind_debut + nbPoints - 1) > length(v)){
    print("Erreur dans la fonction SSE_segment")
    print("La longueur du segment est superieure CB  celle de la serie temporelle")
    return(-1)
  }
  else{
    
  }
  return(SSE(v[ind_debut:(ind_debut + nbPoints - 1)]))
}


#Calcul la somme des carrCB)s des erreurs tous les segments d'une sCB)rie temporelle
#EntrCB)es : 
# - v : une sCB)rie temporelle
# - nbPoints : la longueur d'un segment
#Sortie : 
#Somme des erreurs carrCB)e d'un segment
somme_SSE <- function( v, nbPoints){
  n         <- length(v)
  ind_debut <- 1
  aux_se    <- 0
  
  while((ind_debut + nbPoints - 1) <= n){
    aux_se <- aux_se + SSE_segment(v, nbPoints, ind_debut)
    ind_debut <- ind_debut + nbPoints
  }
  return(aux_se)
}

#Calcul de la matrice de distance entre les TS de longueur w
#Entrees: 
#training_d : jeu de donnCB)es d'apprentissage
#test_d : jeu de donnCB)es de test
#w le nombre de segments de la sC)rie temporelle
#code_distance : entier pour le choix de la distance
#          si code_distance = 1 alors PaaDist
#          si code_distance = 2 alors dtw
#Sortie:
#une matrice de distance avec dim(test_d)[1] lignes et dim(training_d)[1] colonnes
MatriceDeDistance_w <- function(training_d, test_d, w, code_distance){
  
  #initialisation des variables
  distances          <- matrix(nrow= dim(test_d)[1],ncol=dim(training_d)[1])
  w_op_training      <- matrix(nrow= dim(training_d)[1],ncol=1)
  w_op_test          <- matrix(nrow= dim(test_d)[1],ncol=1)
  lg_min             <- 2
  lg_max             <- 20
  
  
  
  #Calcul de la distance entre les s?ries temporelles
  for (i in 1:dim(test_d)[1]){
    a1 <- SupprimerNA(test_d[i,])
    b1 <- as.numeric(unlist(a1))
    t1 <- ZnormalisationDeTS(b1)# a voir
    t1.paa    <- if(w > length(t1)) t1 else as.vector(PAA(t1[1:(length(t1)-length(t1)%% w)], w))
    
    for (j in 1:dim(training_d)[1]){
      a2     <- SupprimerNA(training_d[j,])
      b2     <- as.numeric(unlist(a2))
      t2     <- ZnormalisationDeTS(b2)# a voir
      t2.paa <- if(w > length(t2)) t2 else as.vector(PAA(t2[1:(length(t2) - length(t2)%% w)], w))
      
      if(code_distance == 1){
        #distances[i, j] <- PaaDist(t1.paa, t2.paa, n, w_op_test[i]) # on fait l'hypoth?se que toutes les TS ont la m?me longueur d'ou n
        print("En cours de construction")
      }else{
        if(code_distance == 2){
          distances[i, j] <- dynamictw(t1.paa, t2.paa)
        }else{
          if(code_distance == 3){
            r <- dtw (t1.paa, t2.paa, window.type = "itakura", distance.only = TRUE)
            distances[i, j] <- r$distance
          }else{
            r <- dtw (t1.paa, t2.paa, window.type = "sakoechiba", distance.only = TRUE)
            distances[i, j] <- r$distance
          }
          
        }
        
      }
      
    }
  }
  as.matrix(distances)
}

#Tester les effets du choix d'une longueure optimale sur la classification
#EntrCB)es : 
# training_set : le jeux de donnCB)es d'apprentissage
# test_set : le jeux de donnCB)es de test
# code_distance : si code_distance  1 alors PaaDist sinon alors DTW
#Sorties : 
#rien en sortie
test_w <- function(training_set, test_set, w, k, code_distance, nom_dataset=""){
  
  #initialisation des variables
  pClassId         <- c()
  classId_training <- training_set[,1]
  training_data    <- training_set[,-(1)]
  classId_test     <- test_set[,1]
  test_data        <- test_set[,-(1)]
  
  heure1 <- Sys.time()
  
  matrice.distance <- MatriceDeDistance_w(training_data, test_data, w, code_distance)
  
  for (i in 1:dim(test_data)[1]){
    pClassId[i] <- PredirClassPAA(i,training_data,k, classId_training, matrice.distance)
  }
  
  print(table(classId_test, pClassId))
  # accuracy
  acc <- (sum(classId_test==pClassId)) / nrow(test_data)
  print(paste("Accuracy = ", acc))
  
  heure2<-Sys.time()
  
  t <- difftime(heure2, heure1, units = "mins")
  print(paste("Duree = ", t))
  
  return(list(accuracy = acc, temps = t))
  
}

Un_NNPDTW       <- function(d, tab_W_selectionnes){
  
  no_cores <- detectCores()
  cl <<- makeCluster(no_cores, type = "PSOCK")
  
  clusterEvalQ(cl=cl, library(Rcpp))
  clusterEvalQ(cl=cl, library(dtw))
  clusterEvalQ(cl=cl, library(timeSeries))
  clusterEvalQ(cl=cl, library(TSclust))
  clusterEvalQ(cl=cl, library(parallel))
  clusterEvalQ(cl=cl, sourceCpp("./fonctions_de_distances.cpp"))
  
  nb_lign         <- dim(d)[1]
  moitie_nb_lign  <- floor(nb_lign / 2)
  
  k_cross <- 3
  
  #Create 10 equally size folds
  folds <- cut(seq(1,nrow(d)),breaks=k_cross,labels=FALSE)
  
  mat_r_final <- matrix(0, length(tab_W_selectionnes), 3)
  
  #Perform k_cross fold cross validation
  for(i in 1:k_cross){
    #Segment your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- d[testIndexes, ]
    trainData <- d[-testIndexes, ]
    #Use the test and train data partitions however you desire...
    r <- kNNPDTW(trainData, testData, 1, 2, "", tab_W_selectionnes)
    mat_r  <- matrix(unlist(t(r)), ncol = 3, byrow = TRUE)
    mat_r_final <- mat_r_final + mat_r
  }
  
 

  stopCluster(cl)
  
  return(mat_r_final / k_cross)
}

kNNPDTW         <- function(training_set, test_set, nb_voisin, code_distance, nom_dataset, tab_W_selectionnes){
  

  
  ZnormalisationDeTS <- function(t){
    return (t - mean(t)) / sd(t) 
  }
  
  #EntrCB)e : 
  # - t : une sCB)rie temporelle
  #Sortie : 
  # - sCB)rie temporelle sans les valeurs maquantes
  SupprimerNA <- function(t){
    a <- data.frame(t(t))
    b <- na.omit(a)
    c <- t(b)
    c
  }
  
  
  # PrCB)dire la classe d'une sCB)rie temporelle CB  partir de celle de ses k plus proche voisins
  #EntrCB)e : 
  # - ind.TS : l'indice de la sCB)rie temporelle dont il faut prCB)dire la classe
  # - training_d : le training_set
  # - clId_training_d : classe du training set
  # - matrice.distance : matrice de distance entre les TS du test_set (en ligne) et celles du training_set (en colonne)
  #Sortie : 
  # - La classe prCB)dite
  PredirClassPAA<-function(ind.TS,training_d,k, clId_training_d, matrice.distance)
  {
    distances <- matrix(nrow = dim(training_d)[1], ncol = 1)
    distances <- matrice.distance[ind.TS, ]
    
    s <- sort(as.vector(distances), index.return=TRUE)
    tb <- as.vector(clId_training_d[s$ix[1:(k)]])
    
    cl <- ClasseMajoritaire(tb, clId_training_d)
    
    return(cl)
  }
  
  #Calcul la somme du carrCB) des Erreurs
  #EntrCB)e : 
  # - Un vecteur de reels
  #Sortie : 
  # Somme du carrCB) des erreurs
  SSE <- function(v){
    v_numeric <- as.numeric(v)
    taille    <- length(v_numeric)
    moy       <- mean(v_numeric)
    som       <- 0
    
    for(i in 1:taille){
      som <- som + (moy - v_numeric[i])^2
    }
    return(som)
  }
  
  
  # Calculer la somme des carrCB)es des erreurs d'un segment
  #Entree : 
  # - v : un vecteur de points
  # - nbPoints : le nombre de points d'un segment
  # - ind_debut : indice de dCB)but du segment
  #Sortie : 
  #somme du carrCB) des erreurs d'un segment
  SSE_segment <- function(v, nbPoints, ind_debut){
    if((ind_debut + nbPoints - 1) > length(v)){
      print("Erreur dans la fonction SSE_segment")
      print("La longueur du segment est superieure CB  celle de la serie temporelle")
      return(-1)
    }
    else{
      
    }
    return(SSE(v[ind_debut:(ind_debut + nbPoints - 1)]))
  }
  
  
  #Calcul la somme des carrCB)s des erreurs tous les segments d'une sCB)rie temporelle
  #EntrCB)es : 
  # - v : une sCB)rie temporelle
  # - nbPoints : la longueur d'un segment
  #Sortie : 
  #Somme des erreurs carrCB)e d'un segment
  somme_SSE <- function( v, nbPoints){
    n         <- length(v)
    ind_debut <- 1
    aux_se    <- 0
    
    while((ind_debut + nbPoints - 1) <= n){
      aux_se <- aux_se + SSE_segment(v, nbPoints, ind_debut)
      ind_debut <- ind_debut + nbPoints
    }
    return(aux_se)
  }
  
  #Calcul de la matrice de distance entre les TS de longueur w
  #Entrees: 
  #training_d : jeu de donnCB)es d'apprentissage
  #test_d : jeu de donnCB)es de test
  #w le nombre de segments de la sC)rie temporelle
  #code_distance : entier pour le choix de la distance
  #          si code_distance = 1 alors PaaDist
  #          si code_distance = 2 alors dtw
  #Sortie:
  #une matrice de distance avec dim(test_d)[1] lignes et dim(training_d)[1] colonnes
  MatriceDeDistance_w <- function(training_d, test_d, w, code_distance){
    
    #initialisation des variables
    distances          <- matrix(nrow= dim(test_d)[1],ncol=dim(training_d)[1])
    w_op_training      <- matrix(nrow= dim(training_d)[1],ncol=1)
    w_op_test          <- matrix(nrow= dim(test_d)[1],ncol=1)
    lg_min             <- 2
    lg_max             <- 20
    
    
    
    #Calcul de la distance entre les s?ries temporelles
    for (i in 1:dim(test_d)[1]){
      a1 <- SupprimerNA(test_d[i,])
      b1 <- as.numeric(unlist(a1))
      t1 <- ZnormalisationDeTS(b1)# a voir
      t1.paa    <- if(w > length(t1)) t1 else as.vector(PAA(t1[1:(length(t1)-length(t1)%% w)], w))
      
      for (j in 1:dim(training_d)[1]){
        a2     <- SupprimerNA(training_d[j,])
        b2     <- as.numeric(unlist(a2))
        t2     <- ZnormalisationDeTS(b2)# a voir
        t2.paa <- if(w > length(t2)) t2 else as.vector(PAA(t2[1:(length(t2) - length(t2)%% w)], w))
        
        if(code_distance == 1){
          #distances[i, j] <- PaaDist(t1.paa, t2.paa, n, w_op_test[i]) # on fait l'hypoth?se que toutes les TS ont la m?me longueur d'ou n
          print("En cours de construction")
        }else{
          if(code_distance == 2){
            distances[i, j] <- dynamictw(t1.paa, t2.paa)
          }else{
            if(code_distance == 3){
              r <- dtw (t1.paa, t2.paa, window.type = "itakura", distance.only = TRUE)
              distances[i, j] <- r$distance
            }else{
              r <- dtw (t1.paa, t2.paa, window.type = "sakoechiba", distance.only = TRUE)
              distances[i, j] <- r$distance
            }
            
          }
          
        }
        
      }
    }
    as.matrix(distances)
  }
  
  #Tester les effets du choix d'une longueure optimale sur la classification
  #EntrCB)es : 
  # training_set : le jeux de donnCB)es d'apprentissage
  # test_set : le jeux de donnCB)es de test
  # code_distance : si code_distance  1 alors PaaDist sinon alors DTW
  #Sorties : 
  #rien en sortie
  test_w <- function(training_set, test_set, w, k, code_distance, nom_dataset=""){
    
    #initialisation des variables
    pClassId         <- c()
    classId_training <- training_set[,1]
    training_data    <- training_set[,-(1)]
    classId_test     <- test_set[,1]
    test_data        <- test_set[,-(1)]
    
    heure1 <- Sys.time()
    
    matrice.distance <- MatriceDeDistance_w(training_data, test_data, w, code_distance)
    
    for (i in 1:dim(test_data)[1]){
      pClassId[i] <- PredirClassPAA(i,training_data,k, classId_training, matrice.distance)
    }
    
    print(table(classId_test, pClassId))
    # accuracy
    acc <- (sum(classId_test==pClassId)) / nrow(test_data)
    print(paste("Accuracy = ", acc))
    
    heure2<-Sys.time()
    
    t <- difftime(heure2, heure1, units = "mins")
    print(paste("Duree = ", t))
    
    return(list(accuracy = acc, temps = t))
    
  }
  
  #Automatiser le test des algorithmes suivant les valeurs de w
  #Entrees : 
  #  training_set : sC)ries temporelles d'apprentissage
  #  test_set : sC)ries temporelles de test
  #  nb_voisin : le nombre de voisin de l'algorithme k-moyennes
  #  code_distance
  #             si code_distance = 1 alors PAAdist
  #             si code_distance = 2 alors DTW
  #nom_dataset : nom a utiliser pour les figures
  #Sorties : 
  #Rien en sortie
  
  
  
  #initialisation des variables
  r <- list()
  r2 <- list()
  k <- nb_voisin
  
  
  tab_accuracy <- c()
  tab_nb_seg   <- c()
  tab_temps    <- c()

  
  
  heure1<-Sys.time()
  j <- 1
  
  
  maFonction <- function(i, training_set, test_set,  k, code_distance, nom_dataset){
    r <- test_w(training_set, test_set, i, k, code_distance, nom_dataset)
    c(i, r[[1]], r[[2]])
  }
  
  matrice_resultat <- t(parSapply(cl, X=tab_W_selectionnes, FUN=maFonction, training_set, test_set,  k, code_distance, nom_dataset, simplify = TRUE))
  
  heure2<-Sys.time()
  t <- difftime(heure2, heure1, units = "mins")
  

  
  return(matrice_resultat)
  
}

H               <- function(training_set, test_set, code_distance, nom_dataset, n, tab_W, bol_diviseur, nb_iter){
  
  tab_diviseurs <- c()
  if(bol_diviseur == 1){
    #-
    print("Liste des diviseurs")
    tab_diviseurs <- divisieurs(n)
  }else{
    l <- floor(n/sqrt(2 * n))
    tab_diviseurs <- seq(from = n, to = 1, by = -l)
    #-
    print("Liste des candidats")
  }
  
  tab_div <- c()
  nb_candidat <- length(tab_diviseurs)
  
  j <- 1
  for(i in 1:nb_candidat){
    if(tab_W[tab_diviseurs[i]] != -1){
      tab_div[j] <- tab_diviseurs[i]
      j <- j + 1
    }
  }
  
  print(tab_div)

  mat_r         <- Un_NNPDTW(training_set, tab_div)
  
  #-
  writeLines("\n")
  print("Accuracy pour chaque candidat sur le training set")
  print(mat_r)

  for(i in tab_div){  tab_W[i] <- -1 }


  max           <- maximun(mat_r)
  
  writeLines("\n")
  print("Le accuracy maximale")
  print(max)
  
  nbTests <- 0
  for(i in 1:n){
    if(tab_W[i] == -1){
      nbTests <- nbTests + 1
    }
  }
  
  print(paste("Iteration Num ", nb_iter," Nombre de tests avant la recherche du maximun est : ", nbTests))
#-
  print("Candidat pour être maximun local")
  print(paste("W_candidat = ", max$w_max, " maximun_candidat = ", max$acc_max))
  max_local     <- maximunLocal(max$w_max, max$acc_max, training_set, tab_W)

  nbTests <- 0
  for(i in 1:n){
    if(tab_W[i] == -1){
      nbTests <- nbTests + 1
    }
  }

  print(paste("Iteration Num ", nb_iter, "Le nombre de tests pour ", nom_dataset, " est : ", nbTests))
  #-
  writeLines("\n")
  print("Le maximun local")
  print(max_local)
  
  return(list( max_local = max_local, tab_W = tab_W))
}


f              <- function(training_set, test_set, code_distance, nom_dataset, bol_diviseur = 0){

  n      <- dim(training_set)[2]
  n      <- n - 1

  nb_rep <- floor(log(n))

  result <- list()

  #-
  print("------------------------------")
  print("------------------------------")
  print("------------------------------")
  print(nom_dataset)
  writeLines("\n")
  
  tab_W         <- 1:n
  for(i in 0:(nb_rep - 1)){
    aux  <- H(training_set, test_set, code_distance, nom_dataset, n-i, tab_W, bol_diviseur, (i + 1))
    result[[(i + 1)]]  <- aux$max_local
    rm(tab_W)
    tab_W <- (aux$tab_W)[-(n-i)]
  }

  print("réslutats de l'heuristique  du max")

  mat_result  <- matrix(unlist(result), ncol = 2, byrow = TRUE)
  

  m <- maximun(mat_result)

  #r <- test_w(training_set, test_set, m$w_max, 1, 2, nom_dataset="")

  #print(paste("Accuracy maximale pour le jeux de données ", nom_dataset, "est : ", r$acc, "le nombre de segments est : ", m$w_max))
  #print(paste( nom_dataset," Nombre de test dans la pire des cas: ", n))
  #return(m)

}


sink(file="FPDTW_SDM_10-10-16.txt")


DiatomSizeReduction_TEST <- read.csv("./UCR_TS_Archive_2015/DiatomSizeReduction_TEST", header=FALSE)
DiatomSizeReduction_TRAIN <- read.csv("./UCR_TS_Archive_2015/DiatomSizeReduction_TRAIN", header=FALSE)
heure1<-Sys.time()
f(DiatomSizeReduction_TRAIN, DiatomSizeReduction_TEST, 2, 'DiatomSizeReduction')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")



BeetleFly_TEST <- read.csv("./UCR_TS_Archive_2015/BeetleFly_TEST", header=FALSE)
BeetleFly_TRAIN <- read.csv("./UCR_TS_Archive_2015/BeetleFly_TRAIN", header=FALSE)
heure1<-Sys.time()
f(BeetleFly_TRAIN, BeetleFly_TEST, 2, 'BeetleFly')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")



BirdChicken_TEST <- read.csv("./UCR_TS_Archive_2015/BirdChicken_TEST", header=FALSE)
BirdChicken_TRAIN <- read.csv("./UCR_TS_Archive_2015/BirdChicken_TRAIN", header=FALSE)
heure1<-Sys.time()
f(BirdChicken_TRAIN, BirdChicken_TEST, 2, 'BirdChicken')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


ShapeletSim_TEST <- read.csv("./UCR_TS_Archive_2015/ShapeletSim_TEST", header=FALSE)
ShapeletSim_TRAIN <- read.csv("./UCR_TS_Archive_2015/ShapeletSim_TRAIN", header=FALSE)
heure1<-Sys.time()
f(ShapeletSim_TRAIN, ShapeletSim_TEST, 2, 'ShapeletSim')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


ToeSegmentation1_TEST <- read.csv("./UCR_TS_Archive_2015/ToeSegmentation1_TEST", header=FALSE)
ToeSegmentation1_TRAIN <- read.csv("./UCR_TS_Archive_2015/ToeSegmentation1_TRAIN", header=FALSE)
heure1<-Sys.time()
f(ToeSegmentation1_TRAIN, ToeSegmentation1_TEST,  2, 'ToeSegmentation1_TEST')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


Wine_TEST <- read.csv("./UCR_TS_Archive_2015/Wine_TEST", header=FALSE)
Wine_TRAIN <- read.csv("./UCR_TS_Archive_2015/Wine_TRAIN", header=FALSE)
heure1<-Sys.time()
f(Wine_TRAIN, Wine_TEST,  2, 'Wine')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


Meat_TEST <- read.csv("./UCR_TS_Archive_2015/Meat_TEST", header=FALSE)
Meat_TRAIN <- read.csv("./UCR_TS_Archive_2015/Meat_TRAIN", header=FALSE)
heure1<-Sys.time()
f(Meat_TRAIN, Meat_TEST,  2, 'Meat')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


Car_TEST <- read.csv("./UCR_TS_Archive_2015/Car_TEST", header=FALSE)
Car_TRAIN <- read.csv("./UCR_TS_Archive_2015/Car_TRAIN", header=FALSE)
heure1<-Sys.time()
f(Car_TRAIN, Car_TEST,  2, 'Car')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


Herring_TEST <- read.csv("./UCR_TS_Archive_2015/Herring_TEST", header=FALSE)
Herring_TRAIN <- read.csv("./UCR_TS_Archive_2015/Herring_TRAIN", header=FALSE)
heure1<-Sys.time()
f(Herring_TRAIN, Herring_TEST,  2, 'Herring_TEST')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")

Worms_TEST <- read.csv("./UCR_TS_Archive_2015/Worms_TEST", header=FALSE)
Worms_TRAIN <- read.csv("./UCR_TS_Archive_2015/Worms_TRAIN", header=FALSE)
heure1<-Sys.time()
f(Worms_TRAIN, Worms_TEST,  2, 'Worms')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


WormsTwoClass_TEST <- read.csv("./UCR_TS_Archive_2015/WormsTwoClass_TEST", header=FALSE)
WormsTwoClass_TRAIN <- read.csv("./UCR_TS_Archive_2015/WormsTwoClass_TRAIN", header=FALSE)
heure1<-Sys.time()
f(WormsTwoClass_TRAIN, WormsTwoClass_TEST,  2, 'WormsTwoClass')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


Plane_TEST <- read.csv("./UCR_TS_Archive_2015/Plane_TEST", header=FALSE)
Plane_TRAIN <- read.csv("./UCR_TS_Archive_2015/Plane_TRAIN", header=FALSE)
heure1<-Sys.time()
f(Plane_TRAIN, Plane_TEST,  2, 'Plane_TEST')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


Ham_TEST <- read.csv("./UCR_TS_Archive_2015/Ham_TEST", header=FALSE)
Ham_TRAIN <- read.csv("./UCR_TS_Archive_2015/Ham_TRAIN", header=FALSE)
heure1<-Sys.time()
f(Ham_TRAIN, Ham_TEST,  2, 'Ham_TEST')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


Earthquakes_TEST <- read.csv("./UCR_TS_Archive_2015/Earthquakes_TEST", header=FALSE)
Earthquakes_TRAIN <- read.csv("./UCR_TS_Archive_2015/Earthquakes_TRAIN", header=FALSE)
heure1<-Sys.time()
f(Earthquakes_TRAIN, Earthquakes_TEST,  2, 'Earthquakes')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


DistalPhalanxOutlineAgeGroup_TEST <- read.csv("./UCR_TS_Archive_2015/DistalPhalanxOutlineAgeGroup_TEST", header=FALSE)
DistalPhalanxOutlineAgeGroup_TRAIN <- read.csv("./UCR_TS_Archive_2015/DistalPhalanxOutlineAgeGroup_TRAIN", header=FALSE)
heure1<-Sys.time()
f(DistalPhalanxOutlineAgeGroup_TRAIN, DistalPhalanxOutlineAgeGroup_TEST,  2, 'DistalPhalanxOutlineAgeGroup')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


DistalPhalanxTW_TEST <- read.csv("./UCR_TS_Archive_2015/DistalPhalanxTW_TEST", header=FALSE)
DistalPhalanxTW_TRAIN <- read.csv("./UCR_TS_Archive_2015/DistalPhalanxTW_TRAIN", header=FALSE)
heure1<-Sys.time()
f(DistalPhalanxTW_TRAIN, DistalPhalanxTW_TEST,  2, 'DistalPhalanxTW')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


MiddlePhalanxTW_TEST <- read.csv("./UCR_TS_Archive_2015/MiddlePhalanxTW_TEST", header=FALSE)
MiddlePhalanxTW_TRAIN <- read.csv("./UCR_TS_Archive_2015/MiddlePhalanxTW_TRAIN", header=FALSE)
heure1<-Sys.time()
f(MiddlePhalanxTW_TRAIN, MiddlePhalanxTW_TEST ,  2, 'MiddlePhalanxTW')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


MiddlePhalanxOutlineAgeGroup_TEST <- read.csv("./UCR_TS_Archive_2015/MiddlePhalanxOutlineAgeGroup_TEST", header=FALSE)
MiddlePhalanxOutlineAgeGroup_TRAIN <- read.csv("./UCR_TS_Archive_2015/MiddlePhalanxOutlineAgeGroup_TRAIN", header=FALSE)
heure1<-Sys.time()
f(MiddlePhalanxOutlineAgeGroup_TRAIN, MiddlePhalanxOutlineAgeGroup_TEST,  2, 'MiddlePhalanxOutlineAgeGroup')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


ProximalPhalanxTW_TEST <- read.csv("./UCR_TS_Archive_2015/ProximalPhalanxTW_TEST", header=FALSE)
ProximalPhalanxTW_TRAIN <- read.csv("./UCR_TS_Archive_2015/ProximalPhalanxTW_TRAIN", header=FALSE)
heure1<-Sys.time()
f(ProximalPhalanxTW_TRAIN, ProximalPhalanxTW_TEST,  2, 'ProximalPhalanxTW')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


Computers_TEST <- read.csv("./UCR_TS_Archive_2015/Computers_TEST", header=FALSE)
Computers_TRAIN <- read.csv("./UCR_TS_Archive_2015/Computers_TRAIN", header=FALSE)
heure1<-Sys.time()
f(Computers_TRAIN, Computers_TEST,  2, 'Computers')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


DistalPhalanxOutlineCorrect_TEST <- read.csv("./UCR_TS_Archive_2015/DistalPhalanxOutlineCorrect_TEST", header=FALSE)
DistalPhalanxOutlineCorrect_TRAIN <- read.csv("./UCR_TS_Archive_2015/DistalPhalanxOutlineCorrect_TRAIN", header=FALSE)
heure1<-Sys.time()
f(DistalPhalanxOutlineCorrect_TRAIN, DistalPhalanxOutlineCorrect_TEST,  2, 'DistalPhalanxOutlineCorrect')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


Strawberry_TEST <- read.csv("./UCR_TS_Archive_2015/Strawberry_TEST", header=FALSE)
Strawberry_TRAIN <- read.csv("./UCR_TS_Archive_2015/Strawberry_TRAIN", header=FALSE)
heure1<-Sys.time()
f(Strawberry_TRAIN, Strawberry_TEST,  2, 'Strawberry')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


InsectWingbeatSound_TEST <- read.csv("./UCR_TS_Archive_2015/InsectWingbeatSound_TEST", header=FALSE)
InsectWingbeatSound_TRAIN <- read.csv("./UCR_TS_Archive_2015/InsectWingbeatSound_TRAIN", header=FALSE)
heure1<-Sys.time()
f(InsectWingbeatSound_TRAIN, InsectWingbeatSound_TEST,  2, 'InsectWingbeatSound')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


LargeKitchenAppliances_TEST <- read.csv("./UCR_TS_Archive_2015/LargeKitchenAppliances_TEST", header=FALSE)
LargeKitchenAppliances_TRAIN <- read.csv("./UCR_TS_Archive_2015/LargeKitchenAppliances_TRAIN", header=FALSE)
heure1<-Sys.time()
f(LargeKitchenAppliances_TRAIN, LargeKitchenAppliances_TEST,  2, 'LargeKitchenAppliances')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")

CinC_ECG_torso_TEST <- read.csv("./UCR_TS_Archive_2015/CinC_ECG_torso_TEST", header=FALSE)
CinC_ECG_torso_TRAIN <- read.csv("./UCR_TS_Archive_2015/CinC_ECG_torso_TRAIN", header=FALSE)
heure1<-Sys.time()
f(CinC_ECG_torso_TRAIN, CinC_ECG_torso_TEST,  2, 'CinC_ECG_torso')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


MALLAT_TEST <- read.csv("./UCR_TS_Archive_2015/MALLAT_TEST", header=FALSE)
MALLAT_TRAIN <- read.csv("./UCR_TS_Archive_2015/MALLAT_TRAIN", header=FALSE)
heure1<-Sys.time()
f(MALLAT_TRAIN, MALLAT_TEST,  2, 'MALLAT')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")

Phoneme_TEST <- read.csv("./UCR_TS_Archive_2015/Phoneme_TEST", header=FALSE)
Phoneme_TRAIN <- read.csv("./UCR_TS_Archive_2015/Phoneme_TRAIN", header=FALSE)
heure1<-Sys.time()
f(Phoneme_TRAIN, Phoneme_TEST,  2, 'Phoneme')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")

sink()








