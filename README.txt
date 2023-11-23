Le but de ce projet est d’implémenter le noyau minimal d’un logiciel ressemblant à git en ocaml,
c'est-à-dire permettant de gérer les versions d’un ensemble de fichiers. Ce git ne fonctionnera
qu’en local pour un seul utilisateur.


Partie Objects :
Pour la partie Objects on a fait toutes les fonctions et on a fait tous les tests mais pour la fonction
store_work_directory () normalement elle effectue le travail qui est demandé cependant dans la création
des hashs pour les fichiers obtient des hashs diffèrent par rapport aux hashs de test 
Et on a trouvé que le problème c’est juste dans les directory car on n’a pas le même ordre dont 
les fichiers sont structures a cause de la date de creation.


Partie Logs :
Pour la partie logs on fait toutes les fonctions et tous les tests passent bien 


Partie Commandes :
Pour la partie Commandes on a fait jusqu’à la fonction ogit_log ,
et pour la fonction ogit_merge on a pas eu le temps pour la faire.
