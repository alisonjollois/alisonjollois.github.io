# alisonjollois.github.io

Page d'Alison Jollois

Cours LP Data-Mining - **R et Shiny**

- [Journée 1 - Manipulation de données](r-shiny/jour1-manip.html)
- [Journée 2 - *Dashboard* avec shiny](r-shiny/jour2-shiny.html)
    - Questions complémentaires :
        - Ajouter la possibilité de faire un TOP inversé (pour voir les moins bons)
        - Ajouter la possibilité d'avoir la droite de régression linéaire sur le graphique (cf geom_smooth)
        - Ajouter un graphique d'évolution en base 100 pour la ville, à mettre en dessous du premier graphique, avec l'évolution globale en plus sur le graphique
        - La progression est en fait le pourcentage de la valeur de 2015 par rapport à celle de 2000. Modifier le code pour avoir le delta entre cette valeur et 100%
            - par exemple : 169% deviendra +69%, et 85% deviendra -15%
            - modifier les couleurs pour avertir du problème (vert si > 0 et orange sinon)
    - Correction : [app.R](r-shiny/dashboard/app.R)
- [Journée 3 - *Widgets HTML*](r-shiny/jour3-htmlwidgets.html)