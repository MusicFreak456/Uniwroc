# Systemy Operacyjne -- Lista 6
###### tags: `SO`

## Zadanie 1

![](https://i.imgur.com/hmjooMi.png)

Stan początkowy tożsamości procesu

* `ruid=1000`
* `euid=0`
* `suid=0`

czyli rozpoczynamy działanie jako proces uprzywilejowany. Przyda się poniższa grafika: 

![](https://i.imgur.com/9Czbcl1.png)

a) `setuid(2000)` -- Działamy jako proces uprzywilejowany, zatem każde pole zostanie zmienione na wartość argumentu.

* `ruid 1000 -> 2000`
* `euid 0    -> 2000`
* `suid 0    -> 2000`

Nieuprzywilejowany proces w tym przypadku zmieniłby tylko `euid`, a argumentem musiałby być jego `ruid` lub `suid`.

b) `setreuid(-1, 2000)` -- Pierwszy argument to nowe `ruid`, drugi to nowe `euid`, dodatkowo jeśli pierwszy argument jest różny od -1 (czyli jest ustawiany `ruid`) i\lub wartość `euid` jest zmieniana na inną niż `ruid`, to `suid` jest ustawiany na nowy `euid`. Czyli nowa tożsamość to

* `ruid 1000`
* `euid 0    -> 2000`
* `suid 0    -> 2000`

Oczywiście proces nieuprzywilejowany może ustawić `ruid` tylko na wartość swojego `euid`, a `euid` na `ruid` albo `suid`.

c) `seteuid(200)` - ustawia `euid` na podaną wartość

* `ruid 1000`
* `euid 0    -> 2000`
* `suid 0`

Gdybyśmy byli procesem nieuprzywilejowanym to jedynymi akceptowanymi wartościami parametru byłyby nasze `ruid` i `suid`. 

d) `setresuid(-1, 2000, 3000)` - ustawia wszystkie pola na raz. Argument `-1` oznacza pozostawienie pola bez zmian. 

* `ruid 1000`
* `euid 0 -> 2000`
* `suid 0 -> 3000`

Gdybyśmy nie działali w trybie uprzywilejowanym, to ograniczenia byłyby podobne jak w przykładach wyżej.

### Czy proces ruid=0 euid=1000 suid=1000 jest uprzywilejowany?

Nie jest. Sprawdzanie kredencjałów bierze pod uwage wyłącznie `euid` i tylko procesy z nim ustawionym na `0` można nazywać uprzywilejowanymi.

Z *The Linux Programming Interface: A Linux and UNIX System Programming Handbook* [9.2]:
![](https://i.imgur.com/T6X94LZ.png)

## Zadanie 2

![](https://i.imgur.com/9st5Sq7.png)


### Znaczenie bitów uprawnień dla katalogów

* `r` -- umożliwia przeglądanie wpisów w katalogu (czyli w praktyce nazw plików w nim), ale nie umożliwia dostępu do faktycznych plików, w tym do ich metadanych.
* `x` -- umożliwia przeczytanie metadanych pliku w katalogu, pod warunkiem że znamy jego nazwę.
* `w` -- umożliwia na edytowanie, usuwanie i dodawanie wpisów w katalogu (ma znaczenie tylko jeśli jednocześnie ustawiony jest bit `x`)
* `sticky` -- pozwala na edytowanie, usuwanie lub przenoszenie zawartości katalogu tylko jej właścicielom 
* `set-gid` -- pliki tworzone w katalogu z ustawionym tym bitem domyślnie odziedziczą po nim grupę

(*tutaj będą eksperymenty w trakcie prezentacji*)

### Procedura my_access

TODO: Przetestować

```c=
bool my_access(struct stat *sb, int mode) {
  uid_t euid = geteuid();
  mode_t file_mode = sb->st_mode;
  mode_t file_permision_bits = file_mode & 0777;

  if(!euid) return true;

  gid_t file_uid = sb->st_uid;
  if(file_uid == euid) 
    return (file_permision_bits >> 6) & mode == mode;

  int number_of_groups = getgroups(0, NULL);
  gid_t groups[number_of_groups];
  getgroups(number_of_groups, groups);

  gid_t file_gid = sb->st_gid;
  for (int i = 0; i < number_of_groups; i++)
  {
    if(file_gid == groups[i]) 
      return (file_permision_bits >> 3) & mode == mode;
  }

  return file_permision_bits & mode == mode;
}
```

## Zadanie 3

![](https://i.imgur.com/7k9AvaJ.png)

Program `su` pozwala na uruchomienie danego polecenia jako inny użytkownik. Plik tego programu ma ustawiony bit `set-uid` co oznacza że podczas `execve`, `euid` zostanie zmieniony na identyfikator właściciela pliku `su` (w tym przypdaku roota), a `suid` zostanie skopiowany z nowego `euid`.

![](https://i.imgur.com/Iz5XGGu.png)

Co oznacza, że jeśli przed execve `euid=1000` (i założymy że `euid==ruid`) to
nowa tożsamość po execve to:

* `ruid = 1000`
* `euid = 0`
* `suid = 0`

### Uproszczona wersja programu su

Przechodząc do fragmentów programu które mają największe znaczenie.

Zmienna `usr` przechowuje nazwę użytkownika, z którego uprawnieniami ma zostac wykonane polecenie, pobraną z lini polecenia, lub domyślnie `root`.

```c=
  char *usr = "root", *pass;
  [...]
  if (argc < 1)
      ;
  else if (argc == 1)
      usr = argv[0];
  else
      usage();
```

Następnie pobierana jest struktura `passwd`, przez polecenie `getpwnam`, która zawiera informacje z odpowiedniego dla użytkownika `usr` wpisu w `/etc/passwd`.

```c=
  pw = getpwnam(usr);
  if (!pw) {
      if (errno)
          eprintf("getpwnam: %s:", usr);
      else
          eprintf("who are you?\n");
  }
```
Struktura ta wygląda tak:
```c=
struct passwd {
   char   *pw_name;       /* username */
   char   *pw_passwd;     /* user password */
   uid_t   pw_uid;        /* user ID */
   gid_t   pw_gid;        /* group ID */
   char   *pw_gecos;      /* user information */
   char   *pw_dir;        /* home directory */
   char   *pw_shell;      /* shell program */
};
```
Następnie sprawdzamy czy `su` nie jest już uruchomione przez superużytkownika, w przeciwnym wypadku prosimy o wpisanie hasła i sprawdzamy czy się zgadza z hasłem użytkownika, którego uprawnienia chcemy przejąć.

Hasło pobierane jest procedurą `getpass`, która przy okazji wyłącza echo terminala.

```c=
  uid = getuid();
  if (uid) {
      pass = getpass("Password: ");
      if (!pass)
          eprintf("getpass:");
      if (pw_check(pw, pass) <= 0)
          exit(1);
  }
```

#### Interlude: pw_check (libutil/passwd.c)

Przyjrzyjmy się dokładnie co się dzieje w procedurze `pw_check` (no bo w końcu mamy się skupiać na procedurach sprawdzających hasło):

Jeśli wpis z hasłem w `passwd` jest pusty, albo zawiera napis ``"!"`` lub `"*"`, to zalogowanie się metodą podania hasła jest niemożliwe.

```c=
pw_check(const struct passwd *pw, const char *pass)
{
	char *cryptpass, *p;
	struct spwd *spw;

	p = pw->pw_passwd;
	if (p[0] == '!' || p[0] == '*') {
		weprintf("denied\n");
		return -1;
	}

	if (pw->pw_passwd[0] == '\0') {
		if (pass[0] == '\0')
			return 1;
		weprintf("incorrect password\n");
		return 0;
	}
```

Jeśli zawiera `"x"` to znaczy, że hasło trzeba sprowadzić z bazy `shadow` (czyli specjalnego pliku, którego można czytać tylko z upoważnieniami `roota`). Osiągane jest to przez specjalną procedurę `getspnam`, która zwraca strukturę `spwd` opisującą wpis w takim pliku.

```c=
	if (pw->pw_passwd[0] == 'x' && pw->pw_passwd[1] == '\0') {
		errno = 0;
		spw = getspnam(pw->pw_name);
		if (!spw) {
			if (errno)
				weprintf("getspnam: %s:", pw->pw_name);
			else
				weprintf("who are you?\n");
			return -1;
		}
		p = spw->sp_pwdp;
		if (p[0] == '!' || p[0] == '*') {
			weprintf("denied\n");
			return -1;
		}
	}
```

Postać struktury `spwd`:
```c=
struct spwd {
   char *sp_namp;     /* Login name */
   char *sp_pwdp;     /* Encrypted password */
   long  sp_lstchg;   /* Date of last change
                         (measured in days since
                         1970-01-01 00:00:00 +0000 (UTC)) */
   long  sp_min;      /* Min # of days between changes */
   long  sp_max;      /* Max # of days between changes */
   long  sp_warn;     /* # of days before password expires
                         to warn user to change it */
   long  sp_inact;    /* # of days after password expires
                         until account is disabled */
   long  sp_expire;   /* Date when account expires
                         (measured in days since
                         1970-01-01 00:00:00 +0000 (UTC)) */
   unsigned long sp_flag;  /* Reserved */
};
```

Kiedy już dostaliśmy się do wpisu reprezentującego hasło, możemy użyć go jako ustawień do funkcji `crypt` (gdyż zawiera on w sobie wszystko co potrzebne) i zahaszować podany przez użytkownika napis, a następnie porównać te hash'e.

```c=
	cryptpass = crypt(pass, p);
	if (!cryptpass) {
		weprintf("crypt:");
		return -1;
	}
	if (strcmp(cryptpass, p) != 0) {
		weprintf("incorrect password\n");
		return 0;
	}
	return 1;
}
```

#### Powrót do su.c

Po udany uwierzytelnieniu przechodzimy do zmiany tożsamości procesu. Najpierw ustawiane są wszystkie grupy dodatkowe za pomocą `initgroups`, który przeczyta je z właściwego dla użytkownika `usr` wpisu w pliku `/etc/group`.

```c=
	if (initgroups(usr, pw->pw_gid) < 0)
		eprintf("initgroups:");
```

Następnie zmieniane są identyfikatory grupy

```c=
	if (setgid(pw->pw_gid) < 0)
		eprintf("setgid:");
```

A na końcu identyfikatory użytkownika

```c=
	if (setuid(pw->pw_uid) < 0)
		eprintf("setuid:");
```

**Uwaga!** : Przewiduję wieloryba w postaci pytania, czy ta kolejność ma znaczenie, otóż ma, gdybyśmy najpierw zmienili identyfikator użytkownika to już nic więcej nie moglibyśmy zrobić (bo zrzeklibyśmy się uprawnień `roota`).

Następnie program już z wymienioną tożsamością przystępuje do uruchomienia shella docelowego użytkownika (na tym już nie mieliśmy się skupiać).

## Zadanie 4

![](https://i.imgur.com/uZoKYnt.png)

### The least priviledge

Idea projektowania programów `set-user-ID`, lub `set-group-id` operujących z najmniejszym możliwym zestawem upoważnień polega na minimalizowaniu liczby operacji jaką program wykonuje z większym zestawem uprawnień niż jest do ich wykonania wymagany. Oznacza to czasowe zrzekanie się uprawnień kiedy ich nie potrzebujemy, lub całkowite porzucenie ich jeśli do zakończenia pracy programu nie zamierzamy ich użyć.

Metodyka ta motywowana jest chęcią minimalizowania szkód jakie mogą zostać wyrządzone jeśli kontrola nad programem zostanie przejęta np. na skutek ataku.

### Wybrane wytyczne dotycznące tworzenia takich programów

* Należy ich unikać i szukać bezpieczniejszych alternatyw, jeśli są absolutnie konieczne, to w drugiej kolejności należy unikać uruchamnia ich z upoważnieniami roota. Przykład z podręcznika:
![](https://i.imgur.com/WLRzIOT.png)
* Jak już zostało napisane, należy minimalizować liczbę operacji wykonywaną z uprawnieniami szerszymi niż w innym przypadku miałby użytkownik. Np. dobrą praktyką jest zrzec się uprawnień na początku takiego programu i nabywać je tylko kiedy są niezbędne.
* Jeśli nie zamierzamy użyć rozszerzonych uprawnień przez resztę programu, należy się ich zrzec (pozbywając się ich zarówno z `effective` jak i `saved` id).
* Zawsze należy się upewnić, czy zmiana tożsamości miała taki efekt, jakiego oczekiwaliśmy, gdyż semantyka wywołań ją zmieniających może się różnić w obrębie systemów operacyjnych, lub w zależności od `effective user id`.
* Porzucając uprawnienia należy upewnić się że zmieniamy `euid` jako ostatni, a nabywając -- jako pierwszy (gdyż semantyka wywołań różni się w zależności od niego).

### Dlaczego standardowy zestaw funkcji jest niewystarczający?

Problemem jest fakt, że operowanie jako proces uprzywilejowany umożliwia ominięcie dowolnego testu uprawnień, co w praktyce oznacza, że jeśli chcemy wykonać bardzo konkretną operacją dozwoloną tylko superużytkownikowi, musimy, przynajmniej tymczasowo, nadać mu jego tożsamość, która oprócz pożądanej operacji, pozwala mu zrobić absolutnie wszystko. Co może łatwo doprowadzić do luk w bezpieczeństwie.

### Jak rozwiązują to zdolności?

Zdolności (ang. *capabilties*) starają się rozwiązać ten problem rozbijając uprawnienia procesu uprzywilejowanego na mniejsze, bardziej konkretne zdolności. Czyli jeśli proces będzie chciał wykonać operację, która podlega sprawdzaniu tożsamości, to omijać je będzie nie na podstawie posiadania `euid=0`, a na podstawie posiadania zdolności odpowiedniej dla danej operacji. Powodem, przez który zazwyczaj nie zauważamy działania tego mechanizmu, jest fakt, że proces ustawiając `euid` na `0` otrzymuje pełen zestaw zdolności.

Dzięki zdolnościom jeśli chcemy mieć program który jest w stanie czytać dowolny plik i przeglądać dowolny katalog, wystarczy że nadamy plikowi progamu zdolność `CAP_READ_SEARCH`.

### Kiedy proces użytkownika może wysłać sygnał do innego procesu?

* Kiedy użytkownik jest uprzywilejowany (ma zdolność `CAP_KILL`)
* Kiedy sygnałem jest `SIGCONT`, a procesy leżą w tej samej sesji (ta przypadek jest potrzebny żeby powłoka mogła kontunuować programy uruchomione w niej np przez `sudo`)
* Jeśli zajdzie jedno z tych dopasowań:
![](https://i.imgur.com/P7JoR5G.png)


## Zadanie 6

![](https://i.imgur.com/9mjQUYb.png)

### Konfiguracja bufora

```c=
if (strcmp(choice, "fwrite-line") == 0) {
  mode = _IOLBF;
  size = length + 1;
} else if (strcmp(choice, "fwrite-full") == 0) {
  mode = _IOFBF;
  size = getpagesize();
} else {
  mode = _IONBF;
  size = 0;
}

/* TODO: Attach new buffer to stdout stream. */
buf = malloc(size);
setvbuf(stdout, buf, mode, size);
```

### Zapis z użyciem writev

```c=
if (strcmp(choice, "writev") == 0) {
  int n = sysconf(_SC_IOV_MAX);
  struct iovec iov[n];

  /* TODO: Write file by filling in iov array and issuing writev. */
  for (int j = 0; j < times; j++) {
    for (int k = 0; k < length; k++) {
      int index = ( j * length + k ) % n;
      iov[ index ].iov_base = line + k;
      iov[ index ].iov_len = length + 1 - k;
      if(index == n - 1) Writev(STDOUT_FILENO, iov, n);
    }
  }

  if ((times * length) % n > 0) {
    Writev(STDOUT_FILENO, iov, (times * length) % n);
  }
}
```

### Liczba wywołań

#### Skrypt testujący

```bash=
#!/bin/bash

OPTS="-l 1000 -t 1000 -s"

runtest() {
  echo "Method: $1"
  strace -e write,writev -c ./writeperf $OPTS $1 > test
  md5sum test
  rm test
  echo ""
}

runtest write
runtest fwrite
runtest fwrite-line
runtest fwrite-full
runtest writev
```

#### Wynik
```
Method: write
% time     seconds  usecs/call     calls    errors syscall
------ ----------- ----------- --------- --------- ----------------
100,00    4,626051           4   1000000           write
------ ----------- ----------- --------- --------- ----------------
100.00    4,626051               1000000           total

Method: fwrite
% time     seconds  usecs/call     calls    errors syscall
------ ----------- ----------- --------- --------- ----------------
100,00    5,092490           5   1000000           write
------ ----------- ----------- --------- --------- ----------------
100.00    5,092490               1000000           total

Method: fwrite-line
% time     seconds  usecs/call     calls    errors syscall
------ ----------- ----------- --------- --------- ----------------
100,00    4,950159           4   1000000           write
------ ----------- ----------- --------- --------- ----------------
100.00    4,950159               1000000           total

Method: fwrite-full
% time     seconds  usecs/call     calls    errors syscall
------ ----------- ----------- --------- --------- ----------------
100,00    0,848596           6    122437           write
------ ----------- ----------- --------- --------- ----------------
100.00    0,848596                122437           total

Method: writev
% time     seconds  usecs/call     calls    errors syscall
------ ----------- ----------- --------- --------- ----------------
100,00    0,239180         244       977           writev
------ ----------- ----------- --------- --------- ----------------
100.00    0,239180                   977           total

```

### Wydajność metod zapisu

#### Skrypt testujący

```bash=
#!/bin/bash

OPTS="-l 1000 -t 1000"

runtest() {
  echo "Method: $1"
  time ./writeperf $OPTS $1 > test
  md5sum test
  rm test
  echo ""
}

runtest write
runtest fwrite
runtest fwrite-line
runtest fwrite-full
runtest writev

```

#### Wynik pomiarów

```
Method: write

real    0m1,821s
user    0m0,276s
sys     0m1,545s
594c417685170dd3eb60286c0f634dc9  test

Method: fwrite

real    0m1,872s
user    0m0,312s
sys     0m1,560s
594c417685170dd3eb60286c0f634dc9  test

Method: fwrite-line

real    0m1,958s
user    0m0,300s
sys     0m1,658s
594c417685170dd3eb60286c0f634dc9  test

Method: fwrite-full

real    0m0,362s
user    0m0,056s
sys     0m0,306s
594c417685170dd3eb60286c0f634dc9  test

Method: writev

real    0m0,222s
user    0m0,000s
sys     0m0,221s
594c417685170dd3eb60286c0f634dc9  test

```

## Zadanie 7

![](https://i.imgur.com/c7sTi3o.png)

### Pobranie nazw mając identyfikator

```c= 
static const char *uidname(uid_t uid) {
  /* TODO: Something is missing here! */
  return getpwuid(uid)->pw_name;
}

static const char *gidname(gid_t gid) {
  /* TODO: Something is missing here! */
  return getgrgid(gid)->gr_name;
}
```

### Pobranie identyfikatorów i listy grup

```c=
static int getid(uid_t *uid_p, gid_t *gid_p, gid_t **gids_p) {
  gid_t *gids = NULL;
  int groups = 0;

  /* TODO: Something is missing here! */
  *uid_p = getuid();
  *gid_p = getgid();

  /* 'If  size  is  zero, list is not modified, but the total number of 
   *  supplementary group IDs for the process is returned.' 
   * ~ man 2 setgroups
   */
  groups = getgroups(0, gids);
  gids = malloc(groups * sizeof(gid_t));
  groups = getgroups(groups, gids);

  *gids_p = gids;
  return groups;
}
```