# rka-2-dka

> 1.0

## Překlad

Použitý překladový příkaz pro GHC:

```console
~> ghc --make -W -j -O2 -o ../rka-2-dka.exe -odir ../build -hidir ../build Main.hs
```

Možnost snadného překladu pomocí PowerShell scriptu:

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

## Vstup programu

Pokud je specifikována cesta ke vstupnímu souboru jako poslední parametr při spuštění programu, je jako vstup použit obsah tohoto souboru.
Jinak je použita definice ze standartního vstupu - program čte definici dokud se nesetká s prázdným řádkem.
Po prvním prázdném řádku program začne zpracovávat poskytnutý vstup na základě zvolených přepínačů.

Program zpracuje pouze korektně definovaný KA na svém vstupu, pro neplatné definice vyvolá výjimku.
Popis výjimky nemusí vždy odpovídat skutečné chybě ve vstupní definici a to z důvodu lazy evaluation programovacího jazyka.

## Struktura projektu

Řešení je rozděleno mezi několik souborů ve složce `src`:

| Soubor      | Popis obsahu
|-------------|-----------------------
| `Main.hs`   | Řízení chování programu dle vstupů.
| `Cli.hs`    | Zpracování přepínačů z příkazové řádky.
| `Parser.hs` | Zpracování vstupu - vytvoření FSM.
| `FSM.hs`    | Struktury a funkce popisující FSM.
| `DFSM.hs`   | Struktury a funkce popisující DFSM, funkce pro determinizaci.
| `Utils.hs`  | Generické funkce pro práci se seznamy aj.

Řešení dále ve složce `tests` obsahuje vlastní testovací sadu:

| Složka     | Popis obsahu
|------------|-----------------------
| `invalid/` | Kontrola chování programu pro neplatné vstupy.
| `valid/`   | Kontrola chování algoritmu pro platné vstupy.

Všechny testy je možné spustit pomocí Python scriptu `tests.py`.
Tento script přijímá 2 argumenty - zkompilovaný spustitelný soubor programu a cestu k testovací sadě.

```console
~> python3.6 tests.py rka-2-dka.exe tests
```
