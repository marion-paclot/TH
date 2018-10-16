library(shiny)
library(shinyBS) # Additional Bootstrap Controls
library(DT)
library(ggplot2)
library(plotly)
library(Cairo)
library(plyr)
library(stringr)
library(shinydashboard)
library(Cairo)
library(shinyjs)

options(shiny.usecairo=T, shiny.reactlog = T, stringsAsFactors = FALSE)
source('components/modal_pourquoi.R', encoding='UTF-8')
source('components/modal_rfr.R', encoding='UTF-8')
source('components/text_exoneration.R', encoding='UTF-8')
source('components/text_abattements.R', encoding='UTF-8')



# Fichier de recensement des éléments d'imposition à la fiscalité directe locale
# Seule les colonnes relatives à la TH ont été conservées
rei = read.csv2("data/REI_TH.csv", fileEncoding = "UTF-8")
rei = subset(rei, ! LIBCOM %in% c("SAINT-BARTHELEMY", "SAINT-MARTIN"))
url_rei = "https://www.data.gouv.fr/fr/datasets/impots-locaux-fichier-de-recensement-des-elements-dimposition-a-la-fiscalite-directe-locale-rei-3/"
dep = unique(rei$LIBDEP)


## Allègement du rei
# rei = read.csv2("data/REI_TH_complet.csv", fileEncoding = "UTF-8")
# rei = rei[, c("LIBCOM", "LIBDEP", "IDCOM", "COMTLV", "J11",
#               "J51A", "J31A", "J41A", "J61A", "J61HA",
#               "J53A", "J33A", "J43A", "J63A", "J63HA",
#               "J21", "J22", "J23",
#               "H12", "H22", "H32", "H52", "H52A", "H52cGEMAPI", "H52gGEMAPI")]
# write.csv2(rei, "data/REI_TH.csv", row.names = F)

# Seuil d'éligibilité du CGI
grille_1417_1_CGI = read.csv2("data/seuils_1417-1-CGI.csv", fileEncoding = "UTF-8")
grille_1417_1bis_CGI = read.csv2("data/seuils_1417-1bis-CGI.csv", fileEncoding = "UTF-8")
grille_1417_2_CGI = read.csv2("data/seuils_1417-2-CGI.csv", fileEncoding = "UTF-8")
grille_1414_A1_CGI = read.csv2("data/seuils_1414-A-1-CGI.csv", fileEncoding = "UTF-8")
grille_1417_2bisa_CGI = read.csv2("data/seuils_1417-2bisa-CGI.csv", fileEncoding = "UTF-8")
grille_1417_2bisb_CGI = read.csv2("data/seuils_1417-2bisb-CGI.csv", fileEncoding = "UTF-8")

# Liens vers les articles du CGI
url_1417 = "https://www.legifrance.gouv.fr/affichCodeArticle.do?idArticle=LEGIARTI000027517723&cidTexte=LEGITEXT000006069577"
url_1414 = "https://www.legifrance.gouv.fr/affichCodeArticle.do?cidTexte=LEGITEXT000006069577&idArticle=LEGIARTI000037100266&dateTexte=&categorieLien=id"


###################################################
myTooltip = function (id, title) {
  # On remplace la fonction bsTooltip par celle-ci qui prend soin de retirer les 
  # guillemets simples et de les remplacer par leur code HTML.
  bsTooltip(id = id, title = gsub("'", "&#39;", title), trigger = "hover")
}

# Tooltip d'explication
tooltipVeuf = "Veuf ou veuve, sans condition d'âge"
tooltipSenior = "Plus de 60 ans"
tooltipHandicape = "Au moins une personne du foyer doit être dans la situation suivante : 
bénéficiaire de l'ASI ou de l'AAH, 
atteint d'une infirmité ou invalidité l'empêchant de subvenir par son travail aux nécessités de l'existence,
titulaire d'une carte mobilité inclusion avec la mention invalidité.
Pour bénéficier de ce statut, la demande doit avoir été adressée avant le 1er janvier de l'année concernée"
tooltipHandicape = gsub("\n", " ", tooltipHandicape)

tooltipIndigent = "Personnes physiques reconnues indigentes par la commission 
communale des impôts directs après avis conforme du représentant du service des impôts"
tooltipIndigent = gsub("\n", " ", tooltipIndigent)

tooltipRfr = "Vous le trouverez en haut à droite du verso de votre avis d'imposition"
tooltipPac = "Enfants, infirmes quel que soit leur âge, ascendants de plus de 70 ans s'ils résident avec le contribuable.
Les enfants en garde partagée comptent pour 0,5"
tooltipPac = gsub("\n", " ", tooltipPac)

tooltipIsf = "Impôt de solidarité sur la fortune payé au titre de l'année 2016"
tooltipAah = "Bénéficiaire de l'allocation adulte handicapé. 
<br>Cas équivalent : vous êtes infirme ou invalide et ne pouvez subvenir par votre travail aux nécessités de l'existence."
tooltipAah = gsub("\n", " ", tooltipAah)
tooltipAsi = "Allocation supplémentaire d'invalidité"
tooltipAspa = "Allocation de solidarité aux personnes âgées"

tooltipVlb = "Vous la trouverez en haut à droite du verso de votre avis d'imposition"
tooltipMajRs = "Si la commune en a voté une, valeur comprise entre 5% et 60%."

tooltipGemapi = "Gestion des milieux aquatiques et prévention des inondations"
tooltipTse = "Taxe spéciale d'équipement"

# tooltipVacant = "Logement vacant depuis plus de 2 années consécutive au 1er janvier
# et habitable"
################################################### 
# Valeurs d'abattements pour une collectivité territoriale.
# Colonnes Syndicat identiques à celles de la commune car EPCI sans fiscalité propre
colAbattements = list(commune = c("J51A", "J31A", "J41A", "J61A", "J61HA"), 
                      syndicat = c("J51A", "J31A", "J41A", "J61A", "J61HA"), 
                      interco = c("J53A", "J33A", "J43A", "J63A", "J63HA"), 
                      TSE = c("J51A", "J31A", "J41A", "J61A", "J61HA"),
                      GEMAPI = c("J51A", "J31A", "J41A", "J61A", "J61HA"))

colValeurLoc = list(commune = "J21", 
                    syndicat = "J22", 
                    interco = "J23", 
                    TSE = "J21",
                    GEMAPI = "J21")

colTauxCotisations = list(commune = "H12", 
                          syndicat = "H22", 
                          interco = "H32", 
                          TSE = c("H52", "H52A"),
                          GEMAPI = c("H52cGEMAPI", "H52gGEMAPI"))

# Dans l'ordre : principal, dépendance principale, secondaire, vacant
tauxGestion = list(commune = c(0.01, 0.01, 0.03, 0.08),
                   syndicat = c(0.08, 0.08, 0.08, 0.08),
                   interco = c(0.01, 0.01, 0.03, 0.08),
                   TSE = c(0.09, 0.09, 0.09, 0),
                   GEMAPI = c(0.03, 0.03, 0.03, 0.03))

tauxResidenceSecondaire = list(commune = 0.0015,
                               syndicat = 0,
                               interco = 0.0015,
                               TSE = 0,
                               GEMAPI = 0)

# Fixé arbitrairement en attendant les vraies valeurs
tauxMajorationResidenceSecondaire = 0.0


######## MESSAGES POP UP

  
######## FONCTIONS

# Affichage des %
percent <- function(x, digits = 2, format = "f", ...) {
  x = paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
  x = gsub('\\.', ',', x)
}

# Arrondi spécifique à la taxe 0.5 --> 1
round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

# Enlever les retour à la ligne non souhaités et conserver les autres
formatter_phrase = function(phrase){
  phrase = gsub("\n|\\s+", " ", phrase)
  phrase = gsub("<br>", "\n", phrase)
  return(phrase)
}

# Afficher € après un montant
euro = function(montant, allegement){
  montant = format(montant, digits=9, decimal.mark=",",big.mark=" ")
  montant = paste(montant, "€")
  # if (allegement){
  #   montant = gsub("^0 €$", "-", montant)
  # }
  return(montant)
}

euro_back = function(montant){
  montant = gsub('€|\\s+', '', montant)
  montant = gsub(',', '.', montant)
  montant = gsub('-', 0, montant)
  montant = as.numeric(montant)
  return(montant)
}

# Seuils au titre de l'article 1417 et 1414 du CGI
calculer_seuil = function (grille, zone, annee, nombreParts) {
  referentiel = subset(grille, Zone == zone & Annee == annee)
  
  partJusqua3 = min(3, nombreParts)
  valeurPartsJusqua3 = subset(referentiel, Parts == partJusqua3)$Seuil
  demiPartsAuDela3 = 2 * max(nombreParts - 3, 0)
  valeurDemiPartMarginale = subset(referentiel, Parts == 0.5)$Seuil
  
  seuil = valeurPartsJusqua3 + demiPartsAuDela3 * valeurDemiPartMarginale
  seuil = round2(seuil, 0)
  return(seuil)
}

afficher_seuil = function(tableSeuils, zone, annee, nombreParts){
  seuil_exo = calculer_seuil(tableSeuils, zone, annee, nombreParts)
  phraseSeuil = sprintf("Dans votre département, le seuil d'exonération pour un foyer de %s parts est de %s€.", nombreParts, seuil_exo)
}

makeButton = function (myId, header, data, rvTabBox) {
  myValueBox = valueBox(
    "Title",
    data,
    color = ifelse(rvTabBox == myId, "navy", "light-blue"),
    width = 12,
    href = "#"
  )
  myValueBox$children[[1]]$attribs$class = "action-button"
  myValueBox$children[[1]]$attribs$id = paste0("button_", myId)
  myValueBox$children[[1]]$children[[1]]$children[[1]]$children = list(
    tags$h3(data),
    tags$p(header)  
  )
  return(myValueBox)
}

# Logement modeste dans les DOM 
# Certaines communes ont porté le taux de 40% à 50%. Pas d'indication dans le REI.
# Il est donc possible qu'un logement soit exonéré car sa VL est comprise entre 40 et 50% de la VL communale
# Par ailleurs dans certaines communes, la VL est manquante (car pas assez d'habitations)
exo_logement_modeste_DOM = function(zone, vlBrute, reiCom){
  vlCommune = reiCom$J11
  return(zone != "Métropole" & vlBrute <= 0.4*vlCommune)
}

log = function (message) {
  cat(file = stderr(), message, "\n")
}

################################################################################
# Fonctions pour le calcul de la TH étape par étape

calculer_multiplicateur = function(nbParts, nbPAC, rfr, vlBrute, situation, 
                                   alloc, reiCommune, zoneGeo){
  seuil = calculer_seuil(grille_1417_2_CGI, zoneGeo, 2017, nbParts)
  vlMoyenne = reiCommune[,as.character(colValeurLoc)]
  general = 1
  PAC12 = min(2, nbPAC)
  PAC3 = max(0, nbPAC -2)
  special = as.numeric(rfr <= seuil &  vlBrute <= (1.3 + nbPAC*0.1)*vlMoyenne) # Condition modeste + logement raisonnable
  handicape = as.numeric("Handicapé" %in% situation | "ASI" %in% alloc |"AAH" %in% alloc)
  
  multiplicateur = rbind.data.frame(general, PAC12, PAC3, special, handicape)
  
  colnames(multiplicateur) = c("commune", "syndicat", "interco", "TSE", "GEMAPI")
  rownames(multiplicateur) = c("general", "pac12", "pac3", "special", "handicape")
  
  return(multiplicateur)
}

extraire_abattements = function(reiCommune, nomCol, typeRes){
  nomCol = colAbattements
  nomCol = as.character(unlist(nomCol))
  
  # Création des colonnes manquantes
  reiCommune[, setdiff(nomCol, colnames(reiCommune))] = 0
  abattements = data.frame(matrix(as.numeric(reiCommune[, nomCol]), nrow = 5))
  abattements = round2(abattements, 0)
  
  # Résidence secondaire --> Abattements non applicables
  if (typeRes != "principale"){
    abattements = data.frame(ifelse(abattements>0, 0, 0))
  }
  colnames(abattements) = c("commune", "syndicat", "interco", "TSE", "GEMAPI")
  rownames(abattements) = c("general", "pac12", "pac3", "special", "handicape")
  
  # Cas des EPCI à fiscalité propre. Hypothèse que le
  if (sum(abattements$interco) == 0){
    abattements$interco = abattements$commune
  }
  return(abattements)
}

calculer_prelevement = function(assiette, taux, type){
  if (type == "casPart13"){
    assiette[1] = sum(assiette[c(1,3)])
    assiette[3] = 0
  }
  if (type == "casPart123"){
    assiette[1] = sum(assiette[c(1,2,3)])
    assiette[c(2:3)] = 0
  }
  prelevement = round2(assiette*taux,0)
  return(prelevement)
}

calculer_taux_cotisation = function(colTaux, reiCom, typeRes, txMajRsCom){
  valeur = c()
  for (i in 1:length(colTaux)){
    tx = sum(reiCom[, as.character(unlist(colTaux[i]))])
    valeur = c(valeur, tx/100)
  }
  
  # Ajout de la majoration résidence secondaire si la commune en a voté une (entrée dans le menu)
  if (typeRes == "secondaire"){
    valeur[1] = valeur[1] + txMajRsCom/100
  }
  
  return(valeur)
}

calculer_taux_gestion = function(typeRes, taux){
  residencePS = c("principale", "dépendance princ", "secondaire", "vacant")
  valeur = sapply(taux,function(x) x[match(typeRes, residencePS)])
  valeur = as.numeric(valeur)
  return(valeur)
}

calculer_taux_base_elevee = function(typeRes, vlNette){
  tauxCommune = 0
  if (typeRes %in% c("principale", "dépendance princ") & vlNette[1] > 4573){
    tauxCommune = 0.002
  }
  if (typeRes %in% c("secondaire", "vacant") & vlNette[1] > 4573 & vlNette[1] <= 7622){
    tauxCommune = 0.012
  }
  if (typeRes %in% c("secondaire", "vacant") & vlNette[1] > 7622){
    tauxCommune = 0.017
  }
  return(c(tauxCommune, 0, 0, 0, 0))
}

calculer_taux_residence_secondaire = function(typeRes){
  taux = rep(0,5)
  if (typeRes == "secondaire"){
    taux = c(0.015, 0, 0.015, 0, 0)
  }
  return(taux)
}


################################
calculComplet = function(nbPAC, rfr, seuil, vlBrute, situation, alloc, reiCommune,
                            nomColAbattements, typeRes, zoneGeo, isf, nbParts, txMajRsCom){
  
  # Abattements 
  multiplicateurs = calculer_multiplicateur(nbParts, nbPAC, rfr, vlBrute, 
                                            situation, alloc, reiCommune, zoneGeo)
  abattements = extraire_abattements(reiCommune, nomColAbattements, typeRes)
  totauxAbattements = colSums(abattements*multiplicateurs)
  basesNettes = pmax(0, vlBrute - totauxAbattements)
  
  # Cotisations par collectivité, incluant la majoration RS commune
  tauxCotisation = calculer_taux_cotisation(colTauxCotisations, reiCommune, typeRes, txMajRsCom)
  cotisations = calculer_prelevement(basesNettes, tauxCotisation, "casNormal")
  
  # Si les cotisations sont à 0, on met les abattements à 0
  abattements[,which(tauxCotisation == 0)] = rep(0,5)
  
  # Frais de gestion
  tauxFraisGestion = calculer_taux_gestion(typeRes, tauxGestion)

  # Attention les frais sont calculés différemment pour les logements vacants
  fraisGestion = calculer_prelevement(cotisations, tauxFraisGestion, "casPart13")
  if (typeRes == "vacant"){
    fraisGestion = calculer_prelevement(cotisations, tauxFraisGestion, "casPart123")
  }
  fraisGestion_affichage = euro(fraisGestion, F)
  fraisGestion_affichage[1] = ifelse (typeRes == "vacant", 
                                      paste(fraisGestion_affichage[1] ,"(Com. + syndic. + interco.)"),
                                      paste(fraisGestion_affichage[1], "(Com. + syndic. + interco.)"))
  
  
  # Prélèvement pour base elevée - applicable uniquement aux communes
  tauxBaseElevee = calculer_taux_base_elevee(typeRes, basesNettes[1])
  prelevementBaseElevee = calculer_prelevement(basesNettes[1], tauxBaseElevee, "casNormal")
  
  # Prélèvement résidense secondaire
  tauxResSecondaire = calculer_taux_residence_secondaire(typeRes)
  prelevementResSecondaire = calculer_prelevement(basesNettes, tauxResSecondaire, "casPart13")
  prelevementResSecondaire_affichage = euro(prelevementResSecondaire,F)
  prelevementResSecondaire_affichage[1] = ifelse (typeRes == "secondaire", 
                                      paste(prelevementResSecondaire_affichage[1], "(Com. + interco.)"),
                                      "0 €")
  # Tout en un tableau
  detail = data.frame(
    "Valeur locative brute" = euro(vlBrute, F), 
    "Abattements" = paste('-', euro(totauxAbattements, T)), 
    "Base nette" = paste('=', euro(basesNettes, F)), 
    "Taux d'imposition" = paste('x', percent(tauxCotisation)), 
    "Cotisations" = paste('=', euro(cotisations, T)), 
    "Taux de gestion" = paste('x', percent(tauxFraisGestion)), 
    "Frais de gestion" = paste('=', fraisGestion_affichage),
    "Taux de cotisation pour base élevée" = paste('x', percent(tauxBaseElevee)), 
    "Prélèvement pour base élevée" = paste('=', euro(prelevementBaseElevee, T)),
    "Taux de cotisation pour résidence secondaire" = paste('x', percent(tauxResSecondaire)), 
    "Prélèvement sur résidence secondaire" = paste('=', prelevementResSecondaire_affichage)
  )
  
  # Si les cotisations sont à 0%, on n'affiche pas les 3 premières lignes 
  for (i in which(tauxCotisation == 0)){
    detail[i, c(1:3)] = ""
  }
  detail = t(detail)
  rownames(detail) = gsub("\\.", " ", rownames(detail))
  colnames(detail) = c("Commune", 'Syndicat', "Intercommunalité", 'TSE', "GEMAPI")
  
  ##############################################################################
  # Calcul du plafonnement
  # Eligible au plafonnement
  
  seuilEligibilite = calculer_seuil(grille_1417_2_CGI, zoneGeo, 2017, nbParts)
  # cat("\nRFR : ", rfr)
  # cat("\nSeuil éligibilité : ", seuilEligibilite)
  eligibilite = ! any(isf | typeRes != "principale" | rfr > seuilEligibilite)
  
  # Montant de l'abattement de rfr
  abattementPlafond = ifelse(eligibilite, calculer_seuil(grille_1414_A1_CGI, zoneGeo, 2017, nbParts), NA)
  
  # RFR abattu
  rfrAbattu = ifelse(eligibilite, max(rfr - abattementPlafond,0), rfr)

  # Montant de la TH max dans le cas du plafonnement
  plafond = ifelse(eligibilite, round2(0.0344*rfrAbattu, 0), NA)
  
  # Montant du dégrèvement calculé != appliqué
  totalCotisations = sum(cotisations, na.rm = TRUE)
  totalFraisGestion = sum(fraisGestion, na.rm = TRUE)
  totalPrelevementBaseElevee = sum(prelevementBaseElevee,na.rm = TRUE)
  totalPrelevementResSecondaire = sum(prelevementResSecondaire,na.rm = TRUE)
  
  montantThPourPlafond = totalCotisations + totalFraisGestion
  degrevementCalcule = ifelse(eligibilite, montantThPourPlafond - plafond, 0)
  degrevementApplique = ifelse (degrevementCalcule < 8, 0, degrevementCalcule)
  
  # Dans le cas où une cotisation pour base elevée était payée, 
  # le plafonnement ouvre droit à son dégrèvement.
  degrevementBaseElevee = eligibilite & degrevementCalcule>0 & totalPrelevementBaseElevee>0
  
  # Montant total avant plafonnement
  montantThAvPlafonnement = totalCotisations + totalFraisGestion +
    totalPrelevementBaseElevee + totalPrelevementResSecondaire
  
  # Montant total après application du plafonnement
  montantThFinal = ifelse(degrevementCalcule>0, 
                          totalCotisations + totalFraisGestion - degrevementApplique,
                          montantThAvPlafonnement)
  # Montant total du
  montantDu = ifelse(montantThFinal>12, montantThFinal, 0)
  return(list(detail = detail,
              vlBrute = euro(vlBrute, F), 
              multiplicateurs = multiplicateurs,
              abattements = abattements,
              totauxAbattements = euro(totauxAbattements, F),
              basesNettes = euro(basesNettes, F),
              tauxCotisation = tauxCotisation,
              cotisations = euro(cotisations, F), 
              totalCotisations = euro(totalCotisations, F),
              tauxFraisGestion = tauxFraisGestion,
              fraisGestion = euro(fraisGestion, F),
              fraisGestion_affichage = fraisGestion_affichage,
              totalFraisGestion = euro(totalFraisGestion, F),
              tauxBaseElevee = tauxBaseElevee,
              prelevementBaseElevee = euro(prelevementBaseElevee,F),
              totalPrelevementBaseElevee = euro(totalPrelevementBaseElevee, F),
              tauxResSecondaire = tauxResSecondaire,
              prelevementResSecondaire = euro(prelevementResSecondaire, F),
              prelevementResSecondaire_affichage = prelevementResSecondaire_affichage,
              totalPrelevementResSecondaire = euro(totalPrelevementResSecondaire, F),
              
              # Partie sur le plafond
              seuilEligibilite = euro(seuilEligibilite, F),
              eligibilite = eligibilite,
              abattementPlafond = abattementPlafond, 
              rfrAbattu = euro(rfrAbattu, F),
              plafond = euro(plafond, F), 
              montantThPourPlafond = euro(montantThPourPlafond, F),
              degrevementCalcule = degrevementCalcule,
              degrevementApplique = degrevementApplique,
              degrevementBaseElevee = degrevementBaseElevee,
              montantThAvPlafonnement = euro(montantThAvPlafonnement, F),
              montantThFinal = euro(montantThFinal, F),
              montantDu = euro(montantDu, F)
              )
         )
}



calculer_reforme2018 = function(rfr, nbParts, montantTotalTH){
  montantTotalTH = euro_back(montantTotalTH) # Retour à un chiffre
  
  seuilBas = calculer_seuil(grille_1417_2bisa_CGI, "France", "2018", nbParts)
  seuilHaut = calculer_seuil(grille_1417_2bisb_CGI, "France", "2018", nbParts)
  
  tauxReforme <- function(x){
    y = ifelse(x <= seuilBas, 0.3, 
               ifelse(x<= seuilHaut, 0.3*(seuilHaut-x)/(seuilHaut - seuilBas), 0))
    y = round(y,2)
    return(y)
  }
  
  taux = tauxReforme(rfr)
  degrevement = round2(taux*montantTotalTH,0)
  montantTaxe = montantTotalTH - degrevement
  
  ## Graphique
  points = c(round(min(seuilBas, rfr)*0.8), seuilBas:seuilHaut, round(max(seuilHaut,rfr)*1.1))
  g <- ggplot(data = data.frame(x = points, y = tauxReforme(points)), mapping = aes(x, y)) +
    geom_line() + xlab('Revenu') + ylab('') +
    scale_x_continuous(labels = function(x) euro(x,F)) +
    scale_y_continuous(labels=function(x) paste0(round(x*100,0), "%"))
  g = ggplotly(g,tooltip = NULL)
  
  
  return(list(taux = percent(taux),
              degrevement = euro(degrevement, F),
              montantTaxe = euro(montantTaxe, F),
              graph = g))
}

cascade_abattements = function(vlBrute, abattements, multiplicateur){
  nom_abattements = c("GAB", "PAC1-2", "PAC3+", "Special", "Handicape")
  vlBrute = euro_back(vlBrute)
  
  valeur = abattements*multiplicateur
  calcul = data.frame(
    etape = nom_abattements,
    abattements = abattements,
    multiplicateur = multiplicateur,
    valeur = valeur,
    start = c(vlBrute, vlBrute -cumsum(valeur[-length(valeur)])),
    end = c(vlBrute -cumsum(valeur)),
    type = "abattement")
  
  calcul_net = calcul
  calcul_net$type = "net"
  calcul_net$end = pmax(calcul_net$end, 0)
  calcul_net$start = 0
  
  calcul_vlb = data.frame(etape = "Valeur locative brute", start = 0, end = vlBrute, type = "total")
  calcul_vln = data.frame(etape = "Base nette", start = 0, end = max(vlBrute-sum(valeur),0), type = "total")
  
  calcul = rbind.fill(calcul_vlb, calcul, calcul_net, calcul_vln)
  calcul$etape = factor(calcul$etape,levels = unique(calcul$etape))
  calcul$id = as.numeric(calcul$etape)
  calcul$type = factor(calcul$type, levels = c("abattement","net"))
  
  calcul$tooltip = ifelse(calcul$type == "abattement",
                          paste(calcul$multiplicateur, "x",calcul$abattements, "=", euro(calcul$valeur,F)),
                          paste("Valeur locative transitoire :", euro(calcul$end, F)))
  calcul$tooltip = ifelse(calcul$etape == "Valeur locative brute", paste("Valeur locative brute :", euro(calcul$end, F)), calcul$tooltip)
  calcul$tooltip = ifelse(calcul$etape == "Base nette", paste("Base nette :", euro(calcul$end, F)), calcul$tooltip)
  
  # Graphique
  g = ggplot(calcul, aes(etape, fill = type)) +
    geom_rect(aes(x = etape, text = tooltip,
                  xmin = id - 0.45, xmax = id + 0.45, ymin = end, ymax = start)) +
    geom_segment(aes(x = id - 0.45, xend = id + 0.45, y = 0, yend = 0, colour=type)) +
    xlab("") + ylab("Montant") +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  
  # Cas où la vl est nulle
  if (max(calcul$end) == 0){
    g = g + scale_y_continuous(breaks = c(0,100))
  }
  
  # Si abattements, affichage des valeurs
  if (any(valeur>0)){
    g = g + geom_text(data=subset(calcul, type == "abattement" & valeur >0),
                      aes(x=id,y=(start+end)/2, label=euro(-(start-end), T)))
  }
  
  g = ggplotly(g, tooltip = c("text"))
    
  g = hide_legend(g)
  return(g)
}


## Fonction pour le tooltip d'un groupInput
# https://stackoverflow.com/questions/36132204/reactive-radiobuttons-with-tooltipbs-in-shiny
groupTooltip <- function(id, choice, title, placement = "bottom", trigger = "hover", options = NULL){
  
  options = shinyBS:::buildTooltipOrPopoverOptionsList(gsub("'", "&#39;", title), 
                                                       placement, trigger, options)
  options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  bsTag <- shiny::tags$script(shiny::HTML(paste0("
                                                 $(document).ready(function() {
                                                 setTimeout(function() {
                                                 $('input', $('#", id, "')).each(function(){
                                                 if(this.getAttribute('value') == '", choice, "') {
                                                 opts = $.extend(", options, ", {html: true});
                                                 $(this.parentElement).tooltip('destroy');
                                                 $(this.parentElement).tooltip(opts);
                                                 }
                                                 })
                                                 }, 500)
                                                 });
                                                 ")))
  htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
}


dessiner_plafonnement = function(rfr, abattement){
  rfrAbattu = max(0,rfr - abattement)
  plafond1 = rfrAbattu*0.0344
  d = data.frame(x1=c(1.2, 1.2, 3.2, 5.2, 5.2), 
                 x2=c(2.8, 2.8, 4.8, 6.8, 6.8), 
                 y1=c(0,rfrAbattu,0, 0, plafond1), 
                 y2=c(rfrAbattu,rfr, plafond1, plafond1, max(0.08*rfrAbattu, 100)), 
                 t=c('a','a', 'a', 'a', 'a'), r=c('RFR abattu', "Abattement", "", "", ""))
  
  g = ggplot() + 
    scale_x_continuous(name="", limits = c(0.5,7)) +
    scale_y_continuous(name="", limits = c(0,rfr*1.1)) +
    geom_rect(data=d, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=t),
              fill = c(1,2,3, 3,4), color="black", alpha=0.5) +
    geom_text(data=d, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=r), size=4) +
    annotate("segment", x=0.8, xend=0.8, y=0, yend=rfr, alpha=1, lwd = 1) +
    annotate("segment", x=0.8, xend=0.9, y=rfr, yend=rfr, alpha=1, lwd = 1)+
    annotate("segment", x=0.8, xend=0.9, y=0, yend=0, alpha=1, lwd = 1) +
    annotate("text", x=0.5, y=rfr/2, label = "RFR") +
    annotate("text", x = c(2, 4, 6), y = rep(rfr*1.1,3), label = c(1,2,3)) +
    annotate("text", x=4, y=2*max(0.08*rfrAbattu, 100), 
             label = "RFR abattu x 3,44%") +
    annotate("text", x=6, y=2*max(0.08*rfrAbattu, 100), 
             label = "RFR abattu x 3,44% \n+ éventuel\nréhaussement") +
    annotate("segment", x=3, xend=4, y=rfrAbattu/2, yend=2*plafond1,
             alpha=1, lwd = 1, arrow=arrow(length=unit(0.03,"npc"))) +
    annotate("segment", x=4.2, xend=6, y=2*plafond1,  yend= 2*2*plafond1,
             alpha=1, lwd = 1, arrow=arrow(length=unit(0.03,"npc"))) +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.ticks.length = unit(0, "mm")) 
  # g = ggplotly(g)
  return(g)
}

# observeEvent(input[["tauxMajRsCommune"]],
#              {
#                updateSelectInput(session = session,
#                                  inputId = "tauxMajRsCommune2",
#                                  selected = input[["tauxMajRsCommune"]])
#              })
#
# observeEvent(input[["tauxMajRsCommune2"]],
#              {
#                updateSelectInput(session = session,
#                                  inputId = "tauxMajRsCommune",
#                                  selected = input[["tauxMajRsCommune2"]])
#              })