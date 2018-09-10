showTextExoneration = function (nbParts, seuil, exonerations, residence, tlv) {
  
  phrase = NA
  phraseExoPartielle = "<br>Dans le cas où vous bénéficiez d'une exonération partielle 
  au titre de l'article 1414-I bis 2° du CGI, la valeur locative brute de votre 
  bien tient compte de l'abattement auquel vous avez droit.
  <br>Dans le cas où votre habitation se situe en zone de revitalisation rurale (ZRR) et que
  la commune a décidé d'une exonération de taxe d'habitation pour les parties louées
  à titre de meublé, la valeur locative brute de votre bien tient compte de l'abattement
  auquel vous avez droit."
  
  phraseSeuil = sprintf("<br>L'exonération est soumise à condition de revenu : 
                        Le RFR d'un foyer de %s part%s ne doit pas dépasser %s.", 
                        nbParts, ifelse(nbParts > 1, 's', ''), euro(seuil, F))
  
  phraseExoDom = "Vous habitez dans un département d'Outre-Mer et la valeur locative brute
  de votre logement est inférieure à 40% de la valeur locative moyenne de la commune.
  <br>Vous êtes exonéré de TH au titre de l'article 332 de l'annexe II du CGI."
  
  phraseExoIndigent = "La commission communale des impôts directs vous a reconnu indigent.
  <br>Vous êtes exonéré de TH au titre de l'article 1408-II-3° du CGI."
  
  phraseExoAspaAsi = "Vous percevez l'ASPA ou l'ASI. Aucune condition de revenu n'est requise.
  <br>Vous êtes exonéré de TH au titre de l'article 1417-I du CGI."
  
  phraseExoAah = sprintf("Vous percevez l'AAH. %s <br>Vous êtes exonéré de TH au 
                         titre de l'article 1417-I du CGI.", phraseSeuil)
  
  phraseExoAahRejet = sprintf("Vous percevez l'AAH. %s Vous n'êtes pas exonéré de TH. %s", 
                              phraseSeuil, phraseExoPartielle)
  
  phraseExoSeniorRejetIsf = sprintf("Vous êtes veuf/veuve ou avez plus de 60 ans 
                                    mais vous avez payé l'ISF l'an passé. <br>Vous n'êtes pas exonéré de TH. %s", phraseExoPartielle)
  
  phraseExoSeniorRejet = sprintf("Vous êtes veuf/veuve ou avez plus de 60 ans et 
                                 vous n'avez pas payé l'ISF l'an passé. %s 
                                 <br>Vous n'êtes pas exonéré de TH. %s", phraseSeuil, phraseExoPartielle)
  
  phraseExoSenior = sprintf("Vous êtes veuf/veuve ou avez plus de 60 ans et vous n'avez pas payé l'ISF l'an passé.
                            %s <br>Vous êtes exonéré de TH au titre de l'article 1417-I du CGI", phraseSeuil)
  
  phrase = ifelse(exonerations$exoDOM, phraseExoDom, NA)
  phrase = ifelse(exonerations$exoAspaAsi, phraseExoAspaAsi, phrase)
  phrase = ifelse(exonerations$exoAAH, phraseExoAah, phrase)
  phrase = ifelse(exonerations$exoIndigent, phraseExoIndigent, phrase)
  
  phrase = ifelse(exonerations$exoAahRejet  & is.na(phrase), phraseExoAahRejet, phrase)
  phrase = ifelse(exonerations$exoVeufSeniorRejetIsf  & is.na(phrase), phraseExoSeniorRejetIsf, phrase)
  phrase = ifelse(exonerations$exoVeufSeniorRejetSeuil  & is.na(phrase), phraseExoSeniorRejet, phrase)
  phrase = ifelse(exonerations$exoVeufSenior & is.na(phrase), phraseExoSenior, phrase)
  
  if (is.na(phrase)){
    phrase = "Votre situation personnelle ne vous permet pas d'être exonéré 
    de TH au titre de l'article 1417-I du CGI."
  }
  
  # Cas des logements vacants
  if (residence == 'vacant'){
    phrase = ifelse(tlv, "Pour les logements vacants, votre commune a opté pour une taxe spécifique
                    qui remplace la taxe d'habitation.",
                    "Votre logement est vacant, mais la taxe d'habitation est due.")
  }
  
  return(formatter_phrase(phrase))  
}
