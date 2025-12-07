# 📘 Projet WHILEb — Analyseur & Interpréteur en OCaml

Ce projet implémente :
- **un analyseur syntaxique** basé sur des combinateurs d'analyse (anacomb)
- **un interpréteur** basé sur la sémantique naturelle (SN)
- **des extensions** du langage WHILEb⁻⁻ au langage WHILEb (expressions booléennes complètes)

Le projet est principalement constitué de trois fichiers :
- `whileb--.ml` — Partie 1.1 et simplifiée du langage
- `whileb.ml` — Analyseur complet (2.1.x) + exécution WHILEb (2.2.x)
- `whileb_with_spaces.ml` — Version améliorée acceptant les espaces (1.1.4)

---

## 🔧 1. Combinateurs d'analyse (anacomb)

Le projet utilise la bibliothèque légère `anacomb`, qui permet de construire des analyseurs syntaxiques en combinant de petits parseurs primitifs.

### **Combinateurs de base (sans résultat)**

| Combinateur | Description |
|------------|-------------|
| `terminal c` | Analyse un caractère exactement égal à `c` |
| `terminal_cond p` | Analyse un caractère vérifiant le prédicat `p` |
| `epsilon` | Réussit toujours, ne consomme rien |
| `a1 --> a2` | Séquence : exécute `a1` puis `a2` |
| `a1 -| a2` | Alternative : essaie `a1`, sinon essaie `a2` |
| `star a` | Répète `a` (0 ou plusieurs fois) |

### **Combinateurs avec résultat**

| Combinateur | Description |
|------------|-------------|
| `epsilon_res x` | Retourne `x` sans consommer |
| `terminal_res f` | Lit un caractère et applique `f` pour produire un résultat |
| `a1 -+> a2` | Analyseur sans résultat suivi d'un analyseur avec résultat |
| `a1 ++> f` | Applique une fonction dépendant du résultat précédent |
| `a1 +| a2` | Alternative entre analyseurs avec résultat |


Ces outils permettent de construire un analyseur récursif descendant sans utiliser de parser generator.

🧩 2. Structure du projet
📄 whileb--.ml

Cette première partie correspond aux exercices :

1.1.1 — Analyseur minimal WHILEb⁻⁻

1.1.2 — Ajout d’AST pour Affectation, Boucles, Séquences

2.2.1 — Définition de l’état + exécution selon la sémantique naturelle

Langage accepté :

uniquement les variables a,b,c,d

pas d’opérateurs booléens complets

expressions limitées (constantes et variables)

🧠 3. Extension vers le langage WHILEb
📘 Grammaire officielle

Le langage WHILEb étend les expressions avec :

C ::= '0' | '1'
V ::= 'a' | 'b' | 'c' | 'd'
A ::= C | V

E ::= E '+' T | T
T ::= T '.' F | F
F ::= '!' F | A | '(' E ')'


Priorités :

!e

e1 . e2 (conjonction, gauche)

e1 + e2 (disjonction, gauche)


📄 4. whileb.ml — Partie principale

Ce fichier couvre les exercices :

2.1.3 — Analyseur étendu pour toute la grammaire WHILEb

gestion des priorités (+, ., !)

variables et constantes

parenthèses

2.2.2 — Exécution du langage complet WHILEb

interprétation booléenne (0/1)

gestion des nouveaux opérateurs (Not, And, Or)

exécution SN sur l’AST complet

🧹 5. whileb_with_spaces.ml — Version tolérante aux blancs

Correspond à 1.1.4 (facultatif).

Cette version ajoute :

gestion des espaces, retours ligne, indentation

combinateur space et star space

adaptation des analyseurs existants pour ignorer les blancs

▶️ 6. Exécution & Exemple

Le programme final intègre :

Analyse syntaxique (avec anacomb)

Évaluation d’expression

Exécution d’un programme WHILEb selon la sémantique naturelle

Exemple :

run "w(a){i(b){c:=0;a:=0}{c:=1;a:=0}}" [1;0;0;0]


Affiche l’état final des variables a,b,c,d.
