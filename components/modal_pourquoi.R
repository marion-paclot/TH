showModalPourquoi = function () {
  showModal(modalDialog(
    HTML(
      "<h5>Cette application a pour objectif de permettre aux contribuables de
      comprendre comment a été calculée leur taxe d'habitation 2017.</h5>
      <p><br>La page Détail vous donnera un rapide aperçu de la
      décomposition de votre taxe, tandis que l'onglet Abattements vous permettra
      de comprendre comment a été calculé la base d'imposition compte tenu de
      votre situation personnelle.</p>
      <p>L'application n'a été développée qu'à partir de documents et données
      librement accessibles, si bien que le calcul de la taxe n'est pas
      possible pour tous les cas et que des erreurs peuvent subsister. Par
      ailleurs, des cas très spécifiques (foyers, ambassadeurs) n'ont pas été
      traités, l'objectif étant de simuler la taxe du plus grand nombre sans
      refaire intégralement le travail de la DgFiP.</p>
      <p>Quelques exemples : </p>
      <ul>
      <li>Lorsqu'une commune réunit trop peu d'habitations, la valeur locative
      moyenne de la commune n'est pas renseignée afin de préserver le secret
      statistique. Cette valeur est pourtant nécessaire au calcul de
      l'abattement spécial à la base.</li>
      <li>Les habitations principales des DOM sont exonérées de taxe
      d'habitation si leur valeur locative est inférieure à 40% de la
      valeur locative moyenne de la commune. Ce taux peut être porté à
      50% par décision de la commune, mais cette information est absente
      du REI. Aussi, certaines habitations sont-elles effectivement
      exonérées sans qu'on puisse le savoir.</li>
      <li>Le dégrèvement lié au plafonnement peut être réduit lorsque les
      collectivités ont modifié leur taux d'imposition ou leurs
      abattements. Cependant, calculer exactement le plafond nécessite
      de disposer de valeurs pour les années 2000 (taux global 2000
      corrigé) et 2003 (abattements de référence en 2003), qui ne sont pas
      disponibles en open data.</li>
      </ul>"
    ),
    easyClose = TRUE,
    footer = NULL
  ))
}
