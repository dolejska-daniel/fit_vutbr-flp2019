# rka-2-dka

> 1.0

## Překlad

```console
~> .\build.ps1
```

## Spuštění

```console
~> rka-2-dka.exe {přepínače} [vstupní soubor]
```

| Přepínač | Popis                        |
|----------|------------------------------|
| `-i`     | Vytiskne načtený vstupní KA. |
| `-t`     | Vytiskne determinizovaný KA. |

V případě, kdy jsou použity oba přepínače, je nejdříve vytisknut vstupní KA a následně výsledný DKA.
**Alespoň jeden z přepínačů musí být použit**, program je jinak ukončen s chybou.

## Struktura projektu

Řešení je rozděleno mezi několik souborů:

| Soubor    | Popis obsahu
|-----------|-----------------------
| Main.hs   | Řízení chování programu dle vstupů.
| Cli.hs    | Zpracování přepínačů z příkazové řádky.
| Parser.hs | Zpracování vstupu - vytvoření FSM.
| FSM.hs    | Struktury a funkce popisující FSM.
| DFSM.hs   | Struktury a funkce popisující DFSM, funkce pro determinizaci.
| Utils.hs  | Generické funkce pro práci se seznamy aj.
