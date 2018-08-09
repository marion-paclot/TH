# Taxe d'habitation

Cette application R-Shiny a pour objectif de détailler le calcul de la taxe d'habitation 2017 d'un foyer, à partir de ses informations personnelles (valeur locative brute, commune d'habitation, revenu fiscal de référence, nombre de parts fiscales, minima sociaux éventuellement perçus ...) et des éléments d'impositions relatifs à sa commune (abattements, majoration ...).

L'application a été codée en ne tenant compte que de la documentation disponible à cette adresse :
https://www.impots.gouv.fr/portail/www2/fichiers/documentation/brochure/idl_2017/files/assets/common/downloads/publication.pdf, page 87-114
Elle n'a qu'une valeur pédagogique et n'est pas un calcul officiel, le seul montant faisant foi étant celui qui figure sur l'avis d'impôt.

L'éléments de fiscalité relatifs au lieu d'habitation se trouvent dans le fichier REI-2017.csv, disponible à l'adresse https://www.data.gouv.fr/fr/datasets/r/249a166e-6a4e-4d2a-b3f4-00ef0ac40d2d. Pour achever le calcul et l'explication, manquent plusieurs informations : 
* Le taux de majoration "résidence secondaire" voté par la commune (de 5% à 60%, taux voté par les communes ayant choisi d'appliquer une majoration)
* Le "taux consolidé 2000 global", correspondant à la somme des taux d'imposition applicables en 2000
* Les taux d'abattements applicables en 2003 et 2017. 
Pour 2017, on dispose des quotités (montants) ce qui suffit à faire le calcul des abattements pour l'année 2017. En revanche, dans le cas d'un plafonnement, un double mécanisme de compensation entre Etat et collectivités territoriales se met en marche et nécessite l'utilisation des valeurs 2000 et 2003 (2014 pour Mayotte)

Des erreurs, notamment sur des cas particuliers peuvent subsister, l'ensemble des règles relatives à la taxe d'habitation étant très complexes. 
Enfin, le cas modélisé par l'application reste le cas "classique" d'un ménage. Les cas spécifiques d'hébergement collectifs ne sont pas étudiés ici.
