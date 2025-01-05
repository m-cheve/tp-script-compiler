# Rapport sur l'utilisation du programme, les limitations et problèmes identifiés dans le projet

## Utilisation
Afin de pouvoir tester un programme, il faut d'abord executer la commande
```bash
make
```
Cela va générer l'executable `tp_parser` dans le dossier `bin`. Il suffit alors d'executer le programme en donnant le fichier contenant le script en entrée :
```bash
./bin/tp_parser script.ts
```

## Limitations et problèmes

### Conflit dans le fichier `parser.mly`
Le fichier `parser.mly` contient un conflit décaler/réduire sur la règle suivante :
```ocaml
type__ -> type__ LBracket RBracket
```

Ce conflit n'a pas été résolu, ce qui entraîne une limitation importante :
- Si un type complexe contient un type tableau (par exemple, `t[]`), cela provoquera une erreur lors de l'analyse syntaxique.

Cette limitation peut restreindre l'utilisation correcte de certains types complexes dans le programme.

---

### Problème avec la fonction `check_scope`
La fonction `check_scope`, qui est censée gérer la portée des déclarations de variables, ne fonctionne pas correctement dans le cas des déclarations avec `var`. Cela peut entraîner des erreurs ou des comportements inattendus dans le programme lors de la vérification des portées.

---

### Problèmes avec la fonction `check_type`
La fonction `check_type` présente plusieurs limitations majeures :
- Elle ne fonctionne quasiment pas et détecte fréquemment des erreurs inexistantes.
- Ces problèmes sont dus à une mauvaise utilisation et compréhension des types dans le programme.

Pour pallier ces problèmes temporaires, la fonction a été désactivée afin de permettre le test des autres parties du programme sans interférence. Pour réactiver cette fonction, il suffit de la décommenter dans le fichier `main.ml` à la ligne 342 :
```ocaml
(*check_type ast;*)
```

---

### Affichage des erreurs
Des efforts ont été faits pour afficher des erreurs précises, mais il reste un problème notable :
- L'endroit exact où l'erreur est détectée n'est pas bien indiqué, ce qui complique le débogage.
