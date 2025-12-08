# Projet WHILEb / WHILEb-- --- Analyseur & Sémantique

Ce projet est divisé en **trois grandes parties**, chacune située dans
un dossier dédié :

    1_Préliminaires
    2_PartiePrincipale
    3_ExtensionsOptionnelles

------------------------------------------------------------------------

## 📌 1. Préliminaires théoriques --- `1_Préliminaires/`

Ce dossier contient deux fichiers :

### **1. `1_preliminaires.ml`**

Contient les réponses aux exercices :

-   **Exercice 1.1.2**
-   **Exercice 1.1.3**
-   **Exercice 1.1.4**

Ce fichier présente notamment :

-   la grammaire du langage **WHILEb--**
-   la grammaire du langage **WHILEb**
-   une version **sans récursion**

------------------------------------------------------------------------

### **2. `1-2-1-LT_PROJECT.txt`**

Contient les **règles de la sémantique naturelle (SN)**\
pour les programmes de la forme :

    if expr then P else Q

------------------------------------------------------------------------

## 📌 2. Partie principale --- `2_PartiePrincipale/`

Objectif :\
**Implémentation de l'analyseur syntaxique et exécution de programmes
WHILEb-- et WHILEb.**

Ce dossier contient trois fichiers principaux :

### **1. `WHILEb--.ml`**

Répond aux exercices liés au langage **WHILEb--** :

-   **Exercice 2.1.1**
-   **Exercice 2.1.2**
-   **Exercice 2.2.1**

Ce fichier contient :

-   l'analyse syntaxique de WHILEb--\
-   l'exécution selon la sémantique naturelle (SN)

------------------------------------------------------------------------

### **2. `WHILEb.ml`**

Répond aux exercices liés au langage **WHILEb** :

-   **Exercice 2.1.3**
-   **Exercice 2.2.2**

------------------------------------------------------------------------

### **3. `WHILEb_With_Spaces.ml`**

Contient la solution à :

-   **Exercice 2.1.4** :\
    *amélioration de l'analyseur pour ignorer les espaces blancs*

------------------------------------------------------------------------

## 📌 3. Extensions optionnelles --- `3_ExtensionsOptionnelles/`

Ce dossier contient plusieurs fichiers nommés selon le format :

    numero-numero.ml

Ils correspondent aux exercices :

-   **Exercice 3.2** --- Analyse lexicale & syntaxique avancée\
-   **Exercice 3.3** --- Listes paresseuses\
-   **Exercice 3.4**\
-   **Exercice 3.5**

------------------------------------------------------------------------

### **`TD6et7.v`**

Ce fichier regroupe des preuves en Coq pour :

-   **Partie 2.3** --- Preuves sur la sémantique naturelle (SN)\
-   **Partie 3.1** --- Preuves sur la sémantique opérationnelle
    structurelle (SOS)

------------------------------------------------------------------------

## ✔️ Structure du projet

    .
    ├── 1_Préliminaires
    │   ├── 1_preliminaires.ml
    │   └── 1-2-1-LT_PROJECT.txt
    │
    ├── 2_PartiePrincipale
    │   ├── WHILEb--.ml
    │   ├── WHILEb.ml
    │   └── (fichier exercice 2.1.4)
    │
    ├── 3_ExtensionsOptionnelles
    │   ├── 3.2.ml
    │   ├── 3.3.ml
    │   ├── 3.4.ml
    │   ├── 3.5.ml
    │   └── TD6et7.v
    │
    └── README.md
