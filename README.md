# Intro-R
Introduction to ensemble methods using R



# Objectif
Ce tutoriel la trame pour implementer un Random Forest et boosting sous R. La construction d’un arbre de décision est d’abord décrite, nous mesurons les performances en prédiction, puis nous voyons ce que peuvent apporter les méthodes ensemblistes. Différents aspects de ces méthodes seront mis en lumière : l’importance des variables, l’influence du paramétrage, l’impact des caractéristiques des arbres sous-jacents, etc.
Le langage R est utilisé avec les packages rpart, adabag et randomforest. Evaluer l’influence du paramétrage sur les performances sera notamment très intéressant. 
1	Données
Nous utilisons les données « Image Segmentation Data Set » du dépôt UCI Machine Learning. Elles décrivent 7 types d’images d’extérieur à partir des paramètres qui ont été extraites. Les observations ont été subdivisées en échantillons d’apprentissage (30 exemples par type d’image, soit 210 individus) et de test (300 par type).
Plutôt que de manipuler deux fichiers, nous d’abord lire les fichiers d’apprentissage et test 
et les réunir dans un seul data frame
Une colonne supplémentaire « sample » est ajouté, indiquant leur d’appartenance (apprentissage [train] ou test [test]). REGION.TYPE est la variable cible
