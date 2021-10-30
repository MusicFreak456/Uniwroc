# Systemy Operacyjne -- Lista 0
###### tags: `SO`




## Zadanie 1

![](https://i.imgur.com/f0BFEwh.png)

### Przerwanie systemowe

Wywoływane asynchronicznie (niezależne od aktualnie wykonywanej instrukcji) przez czynniki zewnętrzne względem procesora (kontrolery pamięci, klawiaturę, karty sieciowe). Sterowanie powraca z handlera do kolejnej instrukcji.

### Wyjątek procesora

(przy założeniu że chodzi o fault) Jest to wyjątek wywoływany synchronicznie (przez obecnie wykonywaną instrukcję kodu użytkownika) w przypadku kiedy nie jest możliwe jej zrealizowanie (np. page fault), ale możliwe jest naprawienie tej sytuacji. Handler może (ale nie musi) naprawić środowisko wykonania takiej instrukcji, wtedy sterowanie wraca do niej i jest wykonywana ponownie. Jeżeli nie było to możliwe często kończy się to wywołaniem wyjątku klasy abort kończącego działanie procesu.
Przykłady: błąd dzielenia przez zero, page fault, segmentation fault

### Pułapka

intencjonalny, synchronicznie wywoływany wyjątek, który najczęściej ma na celu stworzenie interfejsu dla kodu użytkownika, który pozwalałby na wykonywanie czynności możliwych tylko z poziomu jądra systemu.
Przykłady: funkcje open, read, ale także breakpointy

### W jakim scenariuszu wyjątek procesora nie oznacza błędu czasu wykonania programu?

Kiedy instrukcja wywołująca wyjątek może zostać wykonana po naprawieniu środowiska przez handler (np. przez ściągnięcie danych z dysku w przypadku page faulta).

### Kiedy pułapka jest generowana w wyniku poprawnej pracy programu?

Zawsze, pułapka jest z definicji intencjonalna.

## Zadanie 2

![](https://i.imgur.com/1WqeaSl.png)

Kiedy wyjątek musi zostać obsłużony (czyli po stronie jądra musi zostać wykonany kod reagujący na zgłoszenie wyjątku -- handler), skok do odpowiedniego handlera wykonywany jest poprzez tablicę skoków (wektor przerywań aka. interrupt vecto table) indeksowaną przez identyfikatory wyjątków (częsciowo narzucane przez sprzęt, częściowo przez SO).

![](https://i.imgur.com/UHnwp6t.png)

### Co robi procesor przed pobraniem pierwszej instrukcji procedury obsługi przerwania?

* Wrzuca na stos adres powrotu (adres obecnej instrukcji lub następnej w zależności od klasy wyjątku)
* W zależności od architektury może wrzucić na stos również inne informacje opisujące stan (np. rejestr flag)
* Jeśli kontrola jest przekazywana do jądra, wszystkie te informacje trafiają na stos jądra systemu

### Co robi procesor po natrafieniu na instrukcję powrotu z przerwania?

Odtwarza stan procesu i wznawia jego wykonywanie wykorzystując wcześniej zapisane na stosie informacje.

### Czemu procedura przerwania powinna być wykonywana w trybie jądra?

Procedury te zazwyczaj potrzebują dostępu do pełnych zasobów sprzętowych, a duża część czynności, takich jak pisanie i czytanie z dysku, czy obsługa sprzętu I/O jest dla bezpieczeństwa zarezerwowana wyłącznie dla trybu jądra.

### Czemu procedura przerwania powinna używać stosu odrębnego od stosu użytkownika?

Wskaźnik na stos użytkownika znajdujący się w %rsp mógł być zmodyfikowany przez użytkownika i wskazywać na bezsensowne miejsce. W skrajnym przypadku może również skończyć się miejsce na stosie użytkownika, żeby np zapisać zawartości wszystkich rejestrów.

## Zadanie 3

![](https://i.imgur.com/Mgeztgu.png)

Składowe pliku wykonywalnego w przybliżeniu oddaje poniższy rysunek:

![](https://i.imgur.com/M8zHgyD.png)

### ELF header

Sekcja widoczna na górze zawiera ogólne informacje dotyczące programu i jego architektury docelowej. Jego zawartość można podejrzeć za pomocą polecenia `readelf -h`. Przykładowy wydruk:

![](https://i.imgur.com/16io30o.png)

Jak widać można tu znaleźć np. informacje o kolejności bajtów, wersji, system ABI, początkach i rozmiarach innych części pliku, a co istotne adres *entry pointu*, czyli pierwszej instrukcji która ma zostać wykonana po załadowaniu programu do pamięci.

### Sekcje

Sekcje to zgrupowane surowe dane (np. instrukcje programu) o podobnej charakterystyce, lub metadane dotyczące innych sekcji przydatne w procesie konsolidacji i relokacji (np .relo.text), ale które nie będą brały udziału przy działaniu programu.

Nagłówki sekcji można podejrzeć poleceniem `readelf -S`. Przykładowy wydruk:

![](https://i.imgur.com/3GjY8eb.png)

### Segmenty

Segmenty grupują sekcje i uzupełniają informacje, które potrzebne będą przy uruchomieniu programu, takie jak np. adresy pod które mają zostać załadowane segmenty oraz jakie uprawnienia mają otrzymać (do odczytu, zapisu, wykonania).

Nagłówki programu zawierające informacje o segmentach oraz mapowaniu sekcji do segmentów można wyświetlić poleceniem `readelf -l`. Przykładowy wydruk:

![](https://i.imgur.com/aD40wkV.png)



## Zadanie 5

![](https://i.imgur.com/OGXNuY4.png)

**volatile** - jest to modyfikator typu, który informuje kompilator o tym, że stan zmiennej może być zmieniany przez zewnętrzne programy. Czyli na przykład nie może zostać usunięta, lub zastąpiona w programie, który pozornie nigdzie nie modyfikuje jej stanu.

Przykład scenariuszy, w których ich brak może wpłynąć na poprawność programu:

* zmapowane na pamięć urządzenia wejścia-wyjścia (memory-mapped I/O)
* zmienne współdzielone pomiędzy kilkoma wątkami

## Zadanie 6

![](https://i.imgur.com/ElBG4a8.png)


### Tłumaczenie adresu

Adres dzielony jest na offset (VPO) i numer strony wirtualnej (VPN). VPO jest równoważny PPO, czyli offsetowi strony fizycznej i nie wymaga tłumaczenia.

Translacji podlega tylko VPN.

### Przeszukanie TLB

W pierwszej kolejności przeszukiwana jest pamięć podręczna TLB, czyli niewielka, szybka pamięć gromadząca instancje PTE (czyli struktur danych przechowujących PPN) dla niedawno używanych adresów.

W tym celu dzielimy VPN na TLBI (TLB index) i TLBT (TLB tag) i wyszukujemy czy pod określonym indeksem i tagiem w TLB znajduje się odpowiednie PTE. Jeśli tak, to wyczytujemy z niego PPN.

W przeciwnym wypadku kontynuujemy poszukiwania w tablicy stron.

### Przeszukiwanie tablicy stron

Tablica stron zorganizowana jest wielopoziomowo dla zaoszczędzenia miejsca. Żeby wydobyć z niej PTE dzielimy VPN na tyle części ile mamy poziomów. Następnie pierwsza część VPN (VPN1) posłuży jako offset w tablicy z pierwszego poziomu, na którą wskazuje ukryty przed użytkownikiem rejestr CR3. Z niej wydobędziemy adres kolejnej tablicy, w której offsetem będzie VPN2 itd. aż na ostatnim poziomie otrzymamy szukane PTE.

Jeśli w którymś z węzłów nie będzie ustawiony bit valid, lub nie będą się zgadzać uprawnienia to zgłoszony zostanie (potencjalnie naprawialny) wyjątek. Nieustawiony bit valid niekoniecznie musi oznaczać błędne odwołanie się do pamięci, gdyż np. ten obszar pamięci może musieć dopiero zostać sprowadzony z dysku, lub chmury.

### Dodatkowo: PTE

#### Najwyższy poziom
![](https://i.imgur.com/GAo8q4Z.png)
![](https://i.imgur.com/NP7uot8.png)

#### Niższe poziomy

![](https://i.imgur.com/BFzngmh.png)


## Zadanie 7

![](https://i.imgur.com/hs64HMO.png)

Wydruk po wykonaniu polecenia `ltrace -S ./1_ls ~`

```
musicfreak456@Czarek-PC$ ltrace -S ./1_ls ~                                                           ~/Pobrane/lista_0
SYS_brk(0)                                                                = 0x5620545a0000
SYS_arch_prctl(0x3001, 0x7ffd3c6f4d80, 0x7f93c6e4e130, -1)                = -22
SYS_access("/etc/ld.so.preload", 04)                                      = -2
SYS_openat(0xffffff9c, 0x7f93c6e58d5b, 0x80000, 0)                        = 3
SYS_newfstatat(3, 0x7f93c6e58572, 0x7ffd3c6f3fc0, 4096)                   = 0
SYS_mmap(0, 0x1abf8, 1, 2)                                                = 0x7f93c6e17000
SYS_close(3)                                                              = 0
SYS_openat(0xffffff9c, 0x7f93c6e63df0, 0x80000, 0)                        = 3
SYS_read(3, "\177ELF\002\001\001\003", 832)                               = 832
SYS_pread(3, 0x7ffd3c6f3d20, 784, 64)                                     = 784
SYS_pread(3, 0x7ffd3c6f3cc0, 80, 848)                                     = 80
SYS_pread(3, 0x7ffd3c6f3c70, 68, 928)                                     = 68
SYS_newfstatat(3, 0x7f93c6e58572, 0x7ffd3c6f3fc0, 4096)                   = 0
SYS_mmap(0, 8192, 3, 34)                                                  = 0x7f93c6e15000
SYS_pread(3, 0x7ffd3c6f3c10, 784, 64)                                     = 784
SYS_mmap(0, 0x1cb1d8, 1, 2050)                                            = 0x7f93c6c49000
SYS_mmap(0x7f93c6c6f000, 0x14b000, 5, 2066)                               = 0x7f93c6c6f000
SYS_mmap(0x7f93c6dba000, 0x4c000, 1, 2066)                                = 0x7f93c6dba000
SYS_mmap(0x7f93c6e06000, 0x6000, 3, 2066)                                 = 0x7f93c6e06000
SYS_mmap(0x7f93c6e0c000, 0x81d8, 3, 50)                                   = 0x7f93c6e0c000
SYS_close(3)                                                              = 0
SYS_mmap(0, 8192, 3, 34)                                                  = 0x7f93c6c47000
SYS_arch_prctl(4098, 0x7f93c6e16580, 0x7f93c6c49000, 34)                  = 0
SYS_mprotect(0x7f93c6e06000, 12288, 1)                                    = 0
SYS_mprotect(0x562052f24000, 4096, 1)                                     = 0
SYS_mprotect(0x7f93c6e60000, 8192, 1)                                     = 0
SYS_munmap(0x7f93c6e17000, 109560)                                        = 0
opendir("/home/musicfreak456" <unfinished ...>
SYS_openat(0xffffff9c, 0x7ffd3c6f6a16, 0x90800, 0)                        = 3
SYS_newfstatat(3, 0x7f93c6dd495a, 0x7ffd3c6f4d30, 4096)                   = 0
SYS_brk(0)                                                                = 0x5620545a0000
SYS_brk(0x5620545c1000)                                                   = 0x5620545c1000
<... opendir resumed> )                                                   = 0x5620545a02a0
readdir(0x5620545a02a0 <unfinished ...>
SYS_getdents64(3, 0x5620545a02d0, 0x8000, 0x7f93c6c51598)                 = 1184
<... readdir resumed> )                                                   = 0x5620545a02d0
puts("Muzyka" <unfinished ...>
SYS_newfstatat(1, 0x7f93c6dd495a, 0x7ffd3c6f4c80, 4096)                   = 0
SYS_write(1, "Muzyka\n", 7Muzyka
)                                               = 7
<... puts resumed> )                                                      = 7
readdir(0x5620545a02a0)                                                   = 0x5620545a02f0
puts(".mozilla" <unfinished ...>
SYS_write(1, ".mozilla\n", 9.mozilla
)                                             = 9
<... puts resumed> )                                                      = 9
readdir(0x5620545a02a0)                                                   = 0x5620545a0310
puts(".config" <unfinished ...>
SYS_write(1, ".config\n", 8.config
)                                              = 8
<... puts resumed> )                                                      = 8
readdir(0x5620545a02a0)                                                   = 0x5620545a0330
puts(".bashrc" <unfinished ...>
SYS_write(1, ".bashrc\n", 8.bashrc
)                                              = 8
<... puts resumed> )                                                      = 8
readdir(0x5620545a02a0)                                                   = 0x5620545a0350
puts(".python_history" <unfinished ...>
SYS_write(1, ".python_history\n", 16.python_history
)                                     = 16
<... puts resumed> )                                                      = 16
readdir(0x5620545a02a0)                                                   = 0x5620545a0378
puts(".zcompdump" <unfinished ...>
SYS_write(1, ".zcompdump\n", 11.zcompdump
)                                          = 11
<... puts resumed> )                                                      = 11
readdir(0x5620545a02a0)                                                   = 0x5620545a0398
puts(".lesshst" <unfinished ...>
SYS_write(1, ".lesshst\n", 9.lesshst
)                                             = 9
<... puts resumed> )                                                      = 9
readdir(0x5620545a02a0)                                                   = 0x5620545a03b8
puts(".Xauthority" <unfinished ...>
SYS_write(1, ".Xauthority\n", 12.Xauthority
)                                         = 12
<... puts resumed> )                                                      = 12
readdir(0x5620545a02a0)                                                   = 0x5620545a03d8
puts(".bash_profile" <unfinished ...>
SYS_write(1, ".bash_profile\n", 14.bash_profile
)                                       = 14
<... puts resumed> )                                                      = 14
readdir(0x5620545a02a0)                                                   = 0x5620545a0400
puts(".idapro" <unfinished ...>
SYS_write(1, ".idapro\n", 8.idapro
)                                              = 8
<... puts resumed> )                                                      = 8
readdir(0x5620545a02a0)                                                   = 0x5620545a0420
puts(".gdbinit" <unfinished ...>
SYS_write(1, ".gdbinit\n", 9.gdbinit
)                                             = 9
<... puts resumed> )                                                      = 9
readdir(0x5620545a02a0)                                                   = 0x5620545a0440
puts(".bash_logout" <unfinished ...>
SYS_write(1, ".bash_logout\n", 13.bash_logout
)                                        = 13
<... puts resumed> )                                                      = 13
readdir(0x5620545a02a0)                                                   = 0x5620545a0460
puts("Pobrane" <unfinished ...>
SYS_write(1, "Pobrane\n", 8Pobrane
)                                              = 8
<... puts resumed> )                                                      = 8
readdir(0x5620545a02a0)                                                   = 0x5620545a0480
puts(".pki" <unfinished ...>
SYS_write(1, ".pki\n", 5.pki
)                                                 = 5
<... puts resumed> )                                                      = 5
readdir(0x5620545a02a0)                                                   = 0x5620545a0498
puts(".nvidia-settings-rc" <unfinished ...>
SYS_write(1, ".nvidia-settings-rc\n", 20.nvidia-settings-rc
)                                 = 20
<... puts resumed> )                                                      = 20
readdir(0x5620545a02a0)                                                   = 0x5620545a04c0
puts(".bash_history" <unfinished ...>
SYS_write(1, ".bash_history\n", 14.bash_history
)                                       = 14
<... puts resumed> )                                                      = 14
readdir(0x5620545a02a0)                                                   = 0x5620545a04e8
puts("gef" <unfinished ...>
SYS_write(1, "gef\n", 4gef
)                                                  = 4
<... puts resumed> )                                                      = 4
readdir(0x5620545a02a0)                                                   = 0x5620545a0500
puts("Szablony" <unfinished ...>
SYS_write(1, "Szablony\n", 9Szablony
)                                             = 9
<... puts resumed> )                                                      = 9
readdir(0x5620545a02a0)                                                   = 0x5620545a0520
puts("Obrazy" <unfinished ...>
SYS_write(1, "Obrazy\n", 7Obrazy
)                                               = 7
<... puts resumed> )                                                      = 7
readdir(0x5620545a02a0)                                                   = 0x5620545a0540
puts(".." <unfinished ...>
SYS_write(1, "..\n", 3..
)                                                   = 3
<... puts resumed> )                                                      = 3
readdir(0x5620545a02a0)                                                   = 0x5620545a0558
puts("Pulpit" <unfinished ...>
SYS_write(1, "Pulpit\n", 7Pulpit
)                                               = 7
<... puts resumed> )                                                      = 7
readdir(0x5620545a02a0)                                                   = 0x5620545a0578
puts(".cargo" <unfinished ...>
SYS_write(1, ".cargo\n", 7.cargo
)                                               = 7
<... puts resumed> )                                                      = 7
readdir(0x5620545a02a0)                                                   = 0x5620545a0598
puts(".histfile" <unfinished ...>
SYS_write(1, ".histfile\n", 10.histfile
)                                           = 10
<... puts resumed> )                                                      = 10
readdir(0x5620545a02a0)                                                   = 0x5620545a05b8
puts("Wideo" <unfinished ...>
SYS_write(1, "Wideo\n", 6Wideo
)                                                = 6
<... puts resumed> )                                                      = 6
readdir(0x5620545a02a0)                                                   = 0x5620545a05d8
puts(".cache" <unfinished ...>
SYS_write(1, ".cache\n", 7.cache
)                                               = 7
<... puts resumed> )                                                      = 7
readdir(0x5620545a02a0)                                                   = 0x5620545a05f8
puts(".vscode-oss" <unfinished ...>
SYS_write(1, ".vscode-oss\n", 12.vscode-oss
)                                         = 12
<... puts resumed> )                                                      = 12
readdir(0x5620545a02a0)                                                   = 0x5620545a0618
puts(".kde4" <unfinished ...>
SYS_write(1, ".kde4\n", 6.kde4
)                                                = 6
<... puts resumed> )                                                      = 6
readdir(0x5620545a02a0)                                                   = 0x5620545a0638
puts(".gtkrc-2.0" <unfinished ...>
SYS_write(1, ".gtkrc-2.0\n", 11.gtkrc-2.0
)                                          = 11
<... puts resumed> )                                                      = 11
readdir(0x5620545a02a0)                                                   = 0x5620545a0658
puts(".zshrc" <unfinished ...>
SYS_write(1, ".zshrc\n", 7.zshrc
)                                               = 7
<... puts resumed> )                                                      = 7
readdir(0x5620545a02a0)                                                   = 0x5620545a0678
puts(".gnupg" <unfinished ...>
SYS_write(1, ".gnupg\n", 7.gnupg
)                                               = 7
<... puts resumed> )                                                      = 7
readdir(0x5620545a02a0)                                                   = 0x5620545a0698
puts(".gdb_history" <unfinished ...>
SYS_write(1, ".gdb_history\n", 13.gdb_history
)                                        = 13
<... puts resumed> )                                                      = 13
readdir(0x5620545a02a0)                                                   = 0x5620545a06b8
puts(".gdbinit_gef" <unfinished ...>
SYS_write(1, ".gdbinit_gef\n", 13.gdbinit_gef
)                                        = 13
<... puts resumed> )                                                      = 13
readdir(0x5620545a02a0)                                                   = 0x5620545a06d8
puts("Dokumenty" <unfinished ...>
SYS_write(1, "Dokumenty\n", 10Dokumenty
)                                           = 10
<... puts resumed> )                                                      = 10
readdir(0x5620545a02a0)                                                   = 0x5620545a06f8
puts(".local" <unfinished ...>
SYS_write(1, ".local\n", 7.local
)                                               = 7
<... puts resumed> )                                                      = 7
readdir(0x5620545a02a0)                                                   = 0x5620545a0718
puts(".wget-hsts" <unfinished ...>
SYS_write(1, ".wget-hsts\n", 11.wget-hsts
)                                          = 11
<... puts resumed> )                                                      = 11
readdir(0x5620545a02a0)                                                   = 0x5620545a0738
puts("Publiczny" <unfinished ...>
SYS_write(1, "Publiczny\n", 10Publiczny
)                                           = 10
<... puts resumed> )                                                      = 10
readdir(0x5620545a02a0)                                                   = 0x5620545a0758
puts("." <unfinished ...>
SYS_write(1, ".\n", 2.
)                                                    = 2
<... puts resumed> )                                                      = 2
readdir(0x5620545a02a0 <unfinished ...>
SYS_getdents64(3, 0x5620545a02d0, 0x8000, 119)                            = 0
<... readdir resumed> )                                                   = 0
closedir(0x5620545a02a0 <unfinished ...>
SYS_close(3)                                                              = 0
<... closedir resumed> )                                                  = 0
exit(0 <unfinished ...>
SYS_exit_group(0 <no return ...>
+++ exited (status 0) +++
```

### Które z wywołań systemowych są użwyane przez procedury...

* opendir
	* openat - otwiera połączenie z plikiem (relatywnie do zadanego deskryptora katalogu)
	* newfstatat - zwraca informacje o pliku
	* brk - rozszerza segment data, czyli w praktyce zwiększa miejsce dostępne na heapie (alokuje pamięć), jeśli to możliwe.
* readdir
	* getdents64 - załadowuje listę wpisów z katalogu do bufora
* printf
	* write - wypisuje bajty pod zadany deskryptor pliku
* closedir
	* close - zamyka połączenie z plikiem którego dotyczy deskryptor i zwraca go do dostępnej puli

### Która funkcja używa brk?

Malloc

```
─── Stack ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
[0] from 0x00007ffff7ee53eb in __brk+11 at ../sysdeps/unix/sysv/linux/x86_64/brk.c:31
[1] from 0x00007ffff7ee54a7 in __GI___sbrk+103 at sbrk.c:41
[2] from 0x00007ffff7e6dd0d in __GI___default_morecore+13 at morecore.c:47
[3] from 0x00007ffff7e686f5 in sysmalloc+885 at malloc.c:2470
[4] from 0x00007ffff7e69913 in _int_malloc+3363 at malloc.c:4141
[5] from 0x00007ffff7e69afb in tcache_init+59 at malloc.c:2982
[6] from 0x00007ffff7e6ad8e in tcache_init+22 at malloc.c:3044
[7] from 0x00007ffff7e6ad8e in __GI___libc_malloc+207 at malloc.c:3044
[8] from 0x00007ffff7e6ad8e in malloc_hook_ini+254 at hooks.c:32
[9] from 0x00007ffff7eaf099 in __alloc_dir+45 at ../sysdeps/posix/opendir.c:118
[+]
```


## Zadanie 8
![](https://i.imgur.com/mOUxdBV.png)



```
czarek@DESKTOP-IO7LO1N:/mnt/c/Users/czare/SemestrV/SO/lista0/lista_0$ strace ./2_cat
execve("./2_cat", ["./2_cat"], 0x7ffd6ffc89a0 /* 28 vars */) = 0
brk(NULL)                               = 0x55822dda2000
arch_prctl(0x3001 /* ARCH_??? */, 0x7fff46f6ac20) = -1 EINVAL (Invalid argument)
access("/etc/ld.so.preload", R_OK)      = -1 ENOENT (No such file or directory)
openat(AT_FDCWD, "/etc/ld.so.cache", O_RDONLY|O_CLOEXEC) = 3
fstat(3, {st_mode=S_IFREG|0644, st_size=35708, ...}) = 0
mmap(NULL, 35708, PROT_READ, MAP_PRIVATE, 3, 0) = 0x7f074ab90000
close(3)                                = 0
openat(AT_FDCWD, "/lib/x86_64-linux-gnu/libc.so.6", O_RDONLY|O_CLOEXEC) = 3
read(3, "\177ELF\2\1\1\3\0\0\0\0\0\0\0\0\3\0>\0\1\0\0\0\360q\2\0\0\0\0\0"..., 832) = 832
pread64(3, "\6\0\0\0\4\0\0\0@\0\0\0\0\0\0\0@\0\0\0\0\0\0\0@\0\0\0\0\0\0\0"..., 784, 64) = 784
pread64(3, "\4\0\0\0\20\0\0\0\5\0\0\0GNU\0\2\0\0\300\4\0\0\0\3\0\0\0\0\0\0\0", 32, 848) = 32
pread64(3, "\4\0\0\0\24\0\0\0\3\0\0\0GNU\0\t\233\222%\274\260\320\31\331\326\10\204\276X>\263"..., 68, 880) = 68
fstat(3, {st_mode=S_IFREG|0755, st_size=2029224, ...}) = 0
mmap(NULL, 8192, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0) = 0x7f074ab8e000
pread64(3, "\6\0\0\0\4\0\0\0@\0\0\0\0\0\0\0@\0\0\0\0\0\0\0@\0\0\0\0\0\0\0"..., 784, 64) = 784
pread64(3, "\4\0\0\0\20\0\0\0\5\0\0\0GNU\0\2\0\0\300\4\0\0\0\3\0\0\0\0\0\0\0", 32, 848) = 32
pread64(3, "\4\0\0\0\24\0\0\0\3\0\0\0GNU\0\t\233\222%\274\260\320\31\331\326\10\204\276X>\263"..., 68, 880) = 68
mmap(NULL, 2036952, PROT_READ, MAP_PRIVATE|MAP_DENYWRITE, 3, 0) = 0x7f074a99c000
mprotect(0x7f074a9c1000, 1847296, PROT_NONE) = 0
mmap(0x7f074a9c1000, 1540096, PROT_READ|PROT_EXEC, MAP_PRIVATE|MAP_FIXED|MAP_DENYWRITE, 3, 0x25000) = 0x7f074a9c1000
mmap(0x7f074ab39000, 303104, PROT_READ, MAP_PRIVATE|MAP_FIXED|MAP_DENYWRITE, 3, 0x19d000) = 0x7f074ab39000
mmap(0x7f074ab84000, 24576, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_FIXED|MAP_DENYWRITE, 3, 0x1e7000) = 0x7f074ab84000
mmap(0x7f074ab8a000, 13528, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_FIXED|MAP_ANONYMOUS, -1, 0) = 0x7f074ab8a000
close(3)                                = 0
arch_prctl(ARCH_SET_FS, 0x7f074ab8f540) = 0
mprotect(0x7f074ab84000, 12288, PROT_READ) = 0
mprotect(0x55822c367000, 4096, PROT_READ) = 0
mprotect(0x7f074abc6000, 4096, PROT_READ) = 0
munmap(0x7f074ab90000, 35708)           = 0
read(0, Test
"Test\n", 4096)                 = 5
write(1, "Test\n", 5Test
)                   = 5
read(0, Echo
"Echo\n", 4096)                 = 5
write(1, "Echo\n", 5Echo
)                   = 5
read(0, "", 4096)                       = 0
exit_group(0)                           = ?
+++ exited with 0 +++
```

### Zmodyfikowany kod:

```c=
#include "apue.h"
#include "fcntl.h"

#define BUFFSIZE 4096

int main(int argc, char *argv[]) {
  int n;
  char buf[BUFFSIZE];

  if (argc != 2)
    err_quit("usage: cat file_name");

  int file_descr;

  if((file_descr = open(argv[1], O_RDONLY)) < 0)
    err_sys("can't open %s", argv[1]);
  while ((n = read(file_descr, buf, BUFFSIZE)) > 0)
    if (write(STDOUT_FILENO, buf, n) != n)
      err_sys("write error");

  if (n < 0)
    err_sys("read error");

  exit(0);
}
```

### Co się stanie jeśli przekażesz ścieżkę do katalogu zamiast do pliku regularnego?

```
czarek@DESKTOP-IO7LO1N:/mnt/c/Users/czare/SemestrV/SO/lista0/lista_0$ ./2_cat ./
read error: Is a directory
```





