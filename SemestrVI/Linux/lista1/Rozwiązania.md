# Lista 1 -- Kurs Administrowania Systemem Linux
###### tags: `LINUX`

## Zadanie 1

![](https://i.imgur.com/sN0uRLs.png)

```
alias ll='ls -lAFbhv --color=always | less -XER'
```

Znaczenie flag `ls`:
* `-l` -- wydruk w rozszerzonym formacie
* `-A` -- wypisz wszystko, łącznie z wpisami zaczynającymi się od `.`, ale oprócz `.` i `..`.
* `-F` -- dopisuje do nazwy wpisu jeden z symboli `*/=>@|`. Znaczenie: https://www.gnu.org/software/coreutils/manual/html_node/General-output-formatting.html
* `-b` -- zmienia sposób wypisywania niewidocznych znaków.
* `-h` -- wypisuje rozmiary w czytelny dla człowieka sposób.
* `-v` -- sortuje nazwy w bardziej naturalny sposób
* `--color` -- pokoloruj wydruk

Znaczenie flag `less`:
* `-E` -- `less` jest wyłączany automatycznie w momencie zescrollowania do `EOF`.
* `-R` -- wyświetla kody kontrolne związane z kolorowaniem w surowej postaci Potrzebne żeby kolory się dobrze wyświetlały.
* `-X` -- wyłącza obsługę poleceń `termcap` przy inicjalizacji i deinicjalizacji. W tym przypadku przydatne do tego żeby wydruk pozostał na ekranie po zamknięciu.

```
alias gentmp='echo $(date +tmp-%Y%m%d%I%M%S)'
```

```
alias genpwd='cat /dev/urandom | tr -dc "3-9A-HJ-NP-Z" | fold -w 32 | head -n 1'
```

## Zadanie 2

![](https://i.imgur.com/f9zMUMU.png)

### Wyrażenia regularne

Są to wzorce opisujące jakiś zbiór napisów. Konstruowane są wyrażeń podstawowych czyli pojedynczych symboli dopasowujących się do samych siebie, za pomocą operatorów.

#### Zbiory symboli

Za pomocą kwadratowych nawiasów możemy definiować zbiory symboli wymieniając je wewnątrz nich. Taki zbiór dopasowuje się do jednego z symboli w nim się znajdującego, chyba że pierwszy symbol to `^` wtedy dopasowuje się do każdego symbolu, którego w nim nie ma.

Można wewnątrz nich używać operatora zakresu `-`. Np. litery od `a` do `z` można zapisać w skrócie `[a-z]`.

Istnieją też predefiniowane klasy symboli np. `[:alpha:]` oznaczające litery alfabetu.

#### Operatory powtórzenia

Na koniec wyrażenia regularnego możemy dopisać jeden, lub więcej, operatorów powtórzenia, które mają następujący wpływ na wyrażenie:

* `?` -- dopasowanie jest opcjonalne, czyli dopasowuje się 0 lub 1 raz.
* `*` -- dopasowuje się 0 lub więcej razy.
* `+` -- dopasowuje się co najmniej 1 raz.
* `{n}` -- dopasowuje się dokładnie `n` razy.
* `{n,}` -- dopasowuje się `n` lub więcej razy.
* `{,n}` -- dopasowuje się co najwyżej `n` razy.
* `{n,m}` -- dopasowuje się co najmniej `n`, ale co najwyżej `m` razy.

#### Konkatenacja

Wyrażenia regularne są konkatenowane jeśli są napisane obok siebie, nie ma specjalnego operatora.

#### Alternacja

Infiksowy operator `|` tworzy wzorzec który dopasowuje się jeśli którakolwiek ze stron operatora się dopasuje.

#### Podwyrażenia i wsteczna referencja

Operator `\n` dopasowuje się do napisu dopasowanego do n-tego podwyrażenia (otoczonego nawiasami) w wyrażeniu. Np. wzorzec `(.)\1` dopasuje się do dwóch takich samych symboli następujących po sobie.

#### Symbole specjalne

* `\<` i `\>` -- dopasowują się do pustych napisów odpowiednio na początku i na końcu napisu.
* `\b` -- dopasowuje się do pustego napisu na krawędzi słowa
* `\B` -- dopasowuje sie do pustego napisu, który nie jest na krawędzi słowa
* `\w` -- to samo co `[_[:alnum:]]`
* `\W` -- dopełnienie `\w`.

### Grep

Jest to program służący wypisywania lini ze standardowego wejścia, które zawierają dopasowania zadanego wzorca.

Jego główne opcje dotyczą trybu jego pracy:
* `-G` -- wzorzec jest interpretowany jako podstawowe wyrażenie regularne. Podstawowe od rozszerzonego różni się tym, że operatory (`?`, `*`, `|` itd.) tracą swoje znaczenie i żeby działały trzeba je uprzedzić symbolem `\` (domyślne działanie).
* `-E` -- wzorzec jest interpretowany jako rozszerzone wyrażenie regularne.
* `-F` -- wzorzec nie jest traktowany jako wyrażenie regularne, wyszukiwane jest dosłowne dopasowanie.

## Zadanie 3

![](https://i.imgur.com/wy9dCI8.png)

Program `find` służy do przeszukiwania drzewa katalogów z zadanych punktów startowych i ewaluowania zadanego wyrażenia dla każdego znalezionego pliku.

Jego główne opcje to `-P`, `-L`, `-H`, które definiują zachowanie w stosunku do dowiązań symbolicznych oraz `-D` służące do specyfikowania funkcji debugujących i `-Olevel` definiujące poziom optymalizacji.

Domyślnym punktem startowym, jeśli nie podano innego jest `.`.

### Wyrażenia

Wyrażenia składają się z sekwencji "opcji", które można podzielić na grupy:

* `Testy` -- zwracają prawdę lub fałsz na podstawie właściwości rozważanego pliku
* `Akcje` -- posiadają skutki uboczne, zwracają prawdę lub fałsz w zależności od powodzenia w ich wykonaniu
* `Opcje globalne` -- wpływają na działanie przeszukiwania. Zawsze zwracają prawdę.
* `Opcje pozycyjne` -- wpływają tylko na akcje i testy które występują po nich w poleceniu. Zawsze zwracają prawdę.
* `Operatory` -- służą do sklejania innych elementów wyrażenia, pomiędzy którymi w domyśle występuje `-a` (and). Są to np. operatory logiczne.

Domyślnym wyrażeniem, jeśli nie podano żadnego innego, jest akcja `-print`, czyli wypisanie nazwy pliku.

## Zadanie 5

* Listę zainstalowanych pakietów, które nie posiadają własnego podkatalogu w `/usr/share/doc/`.
  ```bash=
  #!/bin/bash
  
  installed=$(\
    dpkg-query -W -f='${Package} ${Status}\n' \
    | grep installed \
    | cut -d' ' -f1 \
    | sort \
    | uniq)
  doc_present=$(ls -1 /usr/share/doc)
  comm -23 <(echo "${installed}") <(echo "${doc_present}")
  ```
* Listę podkatalogów katalogu `/usr/share/doc/`, których nazwy nie są nazwami żadnego zainstalowanego pakietu. Przy każdym z takich podkatalogów wypisz nazwę pakietu, który jest jego właścicielem.
  ```bash=
  #!/bin/bash

  installed=$(\
    dpkg-query -W -f='${Package} ${Status}\n' \
    | grep installed \
    | cut -d' ' -f1 \
    | sort \
    | uniq)
  doc_present=$(ls -1 /usr/share/doc | sort)
  comm -13 <(echo "${installed}") <(echo "${doc_present}") | \
  while read line 
  do
    echo "${line} $(dpkg-query -S "/usr/share/doc/${line}" | cut -d':' -f1)"
  done
  ```
* Listę pakietów posiadających własny podkatalog w katalogu `/usr/share/doc/`, który jednak nie zawiera pliku changelog.Debian.gz.
  ```bash=
  #!/bin/bash

  doc_dir="/usr/share/doc"

  installed=$(\
    dpkg-query -W -f='${Package} ${Status}\n' \
    | grep installed \
    | cut -d' ' -f1 \
    | sort \
    | uniq)

  doc_present=$(ls -1 $doc_dir | sort)

  comm -12 <(echo "${installed}") <(echo "${doc_present}") |
  while read line 
  do
    changelog="${doc_dir}/${line}/changelog.Debian.gz"
    if !(test -f $changelog); then 
      echo "${line}"
    fi
  done

  ```
* Listę pakietów posiadających własny plik changelog.Debian.gz, który zawiera tylko jeden wpis (zwykle *Initial release*).
  ```bash=
  #!/bin/bash

  doc_dir="/usr/share/doc"

  installed=$(\
    dpkg-query -W -f='${Package} ${Status}\n' \
    | grep installed \
    | cut -d' ' -f1 \
    | sort \
    | uniq)

  doc_present=$(ls -1 $doc_dir | sort)

  comm -12 <(echo "${installed}") <(echo "${doc_present}") |
  while read line 
  do
    changelog="${doc_dir}/${line}/changelog.Debian.gz"
    if test -f $changelog; then 
      lnum=$(gzip -d -c $changelog | grep "^ -- .* <.*>" | wc -l)
      if test $lnum = "1"; then
        echo "${line}"
      fi
    fi
  done

  ```
* Liczbę wystąpień słowa bash (zapisanego małymi lub wielkimi literami) w pliku `/usr/share/doc/bash/INTRO.gz`.
  ```bash=
  gzip -d -c /usr/share/doc/bash/INTRO.gz | grep -i -o -w bash | wc -l
  ```
  
## Zadanie 6

Wypisz:

* Listę wszystkich bibliotek współdzielonych zainstalowanych w Twoim systemie.
  ```bash
  find / -regextype grep -regex ".*\.so\(\.[0-9]\+\)\?$"
  ```
  Domyślnie dowiązania symboliczne nie są rozwiązywane.
* Listę dowiązań symbolicznych do bibliotek współdzielonych zainstalowanych w Twoim systemie.
  ```bash
  find / -type l -exec sh -c "if readlink {} | grep -q '.*\.so\(\.[0-9]\+\)\?$'; then echo '{}'; fi" \;
  ```
* Liczbę, sumaryczny rozmiar w bajtach i średni rozmiar wszystkich bibliotek współdzielonych zainstalowanych w Twoim systemie.
  ```bash
  #!/bin/bash

  lib_regex=".*\.so\(\.[0-9]\+\)\?$"

  number=0
  size=0
  libs=$(find / -regextype grep -regex $lib_regex -printf "%s\n" 2>/dev/null)
  while read line; do
    ((number+=1))
    ((size+=line))
  done <<< "$libs"

  echo "Number: ${number}"
  echo "Total size: ${size}"
  echo "Average: $(expr $size / $number)"
  ```
* Listę (uporządkowaną i bez powtórzeń) wszystkich katalogów, w których występują biblioteki współdzielone.
  ```bash
  find / -regextype grep -regex ".*\.so\(\.[0-9]\+\)\?$" -printf "%h\n" 2>/dev/null | sort | uniq
  ```
* Listę (uporządkowaną i bez powtórzeń) wszystkich katalogów, w których występują dowiązania symboliczne do bibliotek współdzielonych zainstalowanych w Twoim systemie.
  ```bash
  find / -type l -exec sh -c "if readlink {} | grep -q '.*\.so\(\.[0-9]\+\)\?  $'; then echo '{}'; fi" \; -printf "%h\n" 2>/dev/null | sort | uniq
  ```
  
## Zadanie 7

![](https://i.imgur.com/7uIjY94.png)

* Lista wszystkich nazw języków, dla których istnieje plik MO co najmniej jednego programu.
  ```bash
  find /usr/share/locale/ -regextype egrep -regex '.*.mo' | cut -d/ -f5 | sort | uniq
  ```
* Listę wszystkich nazw języków, dla których istnieją komunikaty programu dpkg.
  ```bash
  find /usr/share/locale/ -regextype egrep -regex '.*dpkg.mo' | cut -d/ -f5 | sort | uniq 
  ```
* Listę wszystkich programów posiadających komunikaty w języku pl.
  ```bash
  ls -1 /usr/share/locale/pl/LC_MESSAGES | cut -d. -f1 | sort | uniq 
  ```
* Dla każdego z ośmiu rozdziałów podręcznika listę wszystkich nazw języków, dla których istnieje co najmniej jedna strona dokumentacji w danym języku i w danym rozdziale. Pamiętaj że zamiast katalogu `en/man1` mamy katalog `man1/` itd.
  ```bash
  for i in {1..8}
  do
      echo "man$i:"
      find /usr/share/man/ -regextype egrep -regex ".*$i.gz$" \
      | sed -r 's:man/man:man/en/man:' \
      | cut -d/ -f5 \
      | sort \
      | uniq
      echo
  done
  ```
  
* Dla każdego z ośmiu rozdziałów podręcznika listę wszystkich stron podręcznika w języku pl.
  ```bash
  for man in man{1..8}
  do
      echo "${man}:"
      find /usr/share/man/pl/$man -type f -printf "%f\n" 2>/dev/null \
      | cut -d. -f1
      echo
  done
  ```


## Zadanie 9

![](https://i.imgur.com/Y2EB9sV.png)

`sed` to program służący do automatycznego edytowania tekstu za pomocą prostego języka skryptowego. W zamyśle zaprojektowany jest pod bycie używanym w potokach. Wywołuje się go w następujący sposób:
```
sed [OPTION]... {script-only-if-no-other-script} [input-file]...
```
Jeśli nie podamy pliku, to przetwarzane jest standardowe wejście. Transformacja tekstu wykonywana jest liniami.

Najistotniejsze opcje:
* `-e` -- dodaje kolejne polecenie do skryptu edytującego. Przydatne jeśli chcemy wykonać wiele poleceń pojedynczym wywołaniem.
* `-f` -- wykonuje skrypt z podanego pliku. Polecenia w takim pliku zapisuje się w kolejnych liniach, możliwe są komentarze po `#`.
* `-n` -- wyłącza automatyczne wypisanie wyniku. Przydatne jeśli do tego używamy poleceń `p`.
* `-i` -- (in-place) wykonuje edycję bezpośrednio w plikach wejściowych.

Warto też wiedzieć że `sed` posiada dwa bufory, większość poleceń operuje w tzw. `pattern space`, ale mamy do dyspozycji także przestrzeń do przechowywania danych w postaci `hold space`.

### Język skryptów

Większość poleceń w ogólności ma postać 
```
[addr]X[options]
```

Gdzie `[addr]` to opcjonalny argument opisujący dla których linii ma być wykonane polecenie. Może to być np. numer linii, ich zakres (w postaci `n,m`), czy wyrażenie regularne (w postaci `/regex/`). Jeśli żadny `[addr]` nie jest podany, to polecenie jest wykonywane dla każdej linii.

`X` to jednoliterowe polecenie, a `[options]` to jego ewentualne opcje.

#### Polecenia

Najbardziej przydatnym i często stosowanym jest polecenie `s/<regex>/<replacement>/<flags>`, które służy do zastąpienia napisu dopasowującego się do wyrażenia regularnego `regex` przez `replacement`. Do dopasowanego napisu możemy się odwołać wewnątrz `replacement` za pomocą `&`. Przydatne flaga to `g`, które powoduje zastąpienie każdego wystąpienia wzorca, a nie tylko pierwszego.

Oprócz tego mamy rzeszę innych poleceń np. `p` które wypisuje od razu zawartość bufora, `d` które czyści obecnie przetwarzany bufor, `x` które zamienia `hold space` z `pattern space` (i wiele poleceń które przenoszą między nimi dane) i wiele wiele innych.

##### Kontrola przepływu

Ciekawymi z perspektywy kolejnego zadania mogą być również polecenia służące do zmienienia przepływu sterowania. Są to np.

* `:label` -- które definiuje etykietę w skrypcie
* `b label` -- bezwarunkowy skok do etykiety `label`.
* `t label` -- skok pod warunkiem, że ostanie wywołanie polecenia `s` dokonało podstawienia. 

Więcej info: `info sed`
Przydatny tutorial: https://www.grymoire.com/Unix/Sed.html

## Zadanie 10

![](https://i.imgur.com/w2zjdBD.png)

Plik `chameleons`:
```
#!/bin/sed -rnf

:loop
  p
  # if chameleons bacame monochromatic, then quit
  /^(.)\1*$/ b exit
  # suround first pair of different colored chameleons with `-` symbols
  s:RG|GR|RB|BR|BG|GB:-&-:
  # change color of those chameleons
  s:-RG-|-GR-:BB:
  s:-RB-|-BR-:GG:
  s:-GB-|-BG-:RR:
  b loop
:exit
```

Przykład użycia: `echo "RRRRGBGB" | sed -r -n -f chameleons | less`