# Systemy Operacyjne -- Lista 9
###### tags: `SO`

## Zadanie 1

### Zadania protokołów z różnych warstw

#### Warstwa transportowa

Odpowiadają za przesył wiadomości pomiędzy warstwami aplikacji. W sieci internet funkcjonują dwa istotne protokoły żyjące w tej warstwie, jest to TCP gwarantujący dostarczanie pakietów w całości, w dobrej kolejności i bez duplikatów oraz UDP, w którym tych gwarancji nie ma. Pakiety przesyłane pomiędzy tymi warstwami nazywa się segmentami.

#### Warstwa sieciowa

Otrzymuje od warstwy transportowej segment wraz z adresem pod jaki należy go przesłać. Zadaniem tej warstwy jest przeniesienie segmentu w pakiecie, nazywanym w tej warstwie datagramem, z jednego hosta do drugiego. Musi przy tym zdecydować o ścieżce jaką obierze datagram podróżując do celu, dlatego w tej warstwie żyją wszelkie algorytmy routingowe oraz protokół IP. Warstwa sieciowa ustali jaki jest następny node sieci do którego chcemy wysłać datagram, a następnie przekaże go do warstwy łącza żeby to wykonała.

#### Warstwa łącza

Jej usługa polega na odbieraniu datagramów od warstwy sieciowej, a następnie przesyłaniu *ramek* je zawierających pomiędzy obecnym node'em a kolejnym wybranym przez warstwę sieciową. Jeśli jesteśmy po stronie otrzymującej ramkę to przekazujemy ją do warstwy sieciowej, żeby zdecydowała co dalej. Protokoły żyjące w tej warstwie to na przykład Ethernet czy WiFi. Datagramy mogą podróżować wieloma różnymi protokołami warstwy łącza w drodze do celu. Wysyłanie poszczególnych bitów jest oddelegowane do warstwy fizycznej, o której nie będziemy rozmawiać, bo nie.

### Kapsułkowanie

Polega na owijaniu zawartości pakietu przekazywanego z wyższej warstwy w nagłówek z metadanymi potrzebnymi do poprawnego funkcjonowania warstwy w której się znajdujemy i do poprawnego przekazania pakietu dalej. 

Zatem każdy pakiet będzie się składał z nagłówka warstwy i zawartości, która również może być pakietem wyższej warstwy, ale my nie musimy potrafić interpretować jego zawartości jeśli tej warstwy nie implementujemy.

![](https://i.imgur.com/JfLFxBQ.png)

### Wiresharkowanie

(* tutaj będzie prezentacja *)

Ogólnie to ten nagłówek z TCP/UDP to segment, ten z IP to datagram ten np. z Ethernet to ramka. Adres źródłowy i docelowy powienien być w warstwie sieciowej.

### Dlaczego protokoły warstwy łącza i sieciowej nie są używane w IPC?

Nie jestem pewien czy dobrze rozumiem, ale raczej po prostu nie są potrzebne (adres to zawsze loopback), więc nie ma co route'ować, a tym bardziej nie będziemy niczego przesyłać pomiędzy node'ami w sieci.

## Zadanie 2

### Protokół UDP

Prosty protokół warstwy transportowej. Cechuje go duża zawodność. Pakiety mogą zostać zgubione, uszkodzone, a nawet odebrane kilkukrotnie. Nie ma żadnego mechanizmu zapewniejącego że pakiety dotarły do odbiorcy, chyba że sami go dopiszemy w warstwie aplikacji.

Protokół UDP jest bezpołączeniowy, co zmniejsza koszt obsługi połączenia klient-obiorca, ma również niski narzut pamięciowy (w zasadzie tylko długość i sumę kontrolną).

Wykorzystywany jest w sytuacjach w których chcemu móc przesyłać pakiety jak najszybciej i nie potrzebujemy niezawodności przesyłów (streaming video, muzyki itp). Ewentualną korekcję błędów możemy oddelegować do wyższej warstwy.

### Protokół TCP

Protokół warstwy transportowej oparty na połączeniach, czyli powiązaniach odbiorcy z nadawcą, którzy przesyłają dane w kontrolowany sposób. Jest to protokół prezentujący dużą niezawodność. Pozwala na porządkowanie pakietów przychodzących w złej kolejności, ponowne przesłanie zgubionych pakietów, zabezpieczenie przed przepełnieniem bufora odbierającego, czy zapewnienie że pakiet dotrze, jeśli to tylko możliwe.

#### Jak TCP radzi sobie z faktem, że segmenty mogą przyjść w złej kolejności?

Wszystkie bajty przesyłane w ramach połączenia numerowane są numerem sekwencji. Ustanowienie połączenia zaczyna się od rozmówcy `A` wysyłającego pakiet z ustawionym bitem `SYN` i numerem sekwencji, od którego będzie numerował kolejne bajty (dla uproszczenia 0).

```
A ---------SYN 0---------> B (nr_seq = 0)
```

Rozmówca `B` odpowiada pakietem z ustawionym bitem `ACK` i numerem potwierdzenia `1`, informującym, że kolejnym oczekiwanym numerem sekwencji jest `1` (mimo że pierwszy pakiet nie przesłał danych, jest to tzw. *ghost byte*), a także ustawionym bitem `SYN` i własnym pierwszym numerem sekwencji (również przyjmijmy 0 dla uproszczenia)

```
A <-----ACK 1---SYN 0---- B (nr_seq = 0)
```

Następnie `A` potwierdzi odebranie tego numeru sekwencji pakietem o numerze sekwencji `1`  nie zawierającym żadnych danych, tylko ustawiony bit `ACK` z numerem potwierdzenia `1`.

```
A --------ACK 1---------> B (nr_seq = 1)
```

Teraz może rozpocząć się wymiana danych i np. jeśli `B` wyśle pakiet o numerze sekwencji `1` i 18 bajtami pamięci, a następnie `4` bajtami pamięci, to `A` odpowie potwierdzeniem z oczekiwanym kolejnym numerem sekwencji `19`, a następnie `23`. 

```
A <--------18B----------- B (nr_seq = 1)
```

```
A --------ACK 19--------> B (nr_seq = 1)
```

```
A <--------4B------------ B (nr_seq = 19)
```

```
A --------ACK 23--------> B (nr_seq = 1)
```

W ten sposób wszystkie pakiety są ponumerowane i można je ułożyć w dobrej kolejności jeśli dotrą do odbiorcy w złej.

### Jak TCP sobie radzi z faktem zagubienia segmentu

Każde przesłanie segmentu jest potwierdzane przez drugą stroną pakietem z bitem `ACK`, zatem jeśli taki pakiet potwierdzający nie dotrze w określonym czasie (dopasowywanym odpowiednio do prędkości sieci), to protokół TCP gwarantuje automatyczne ponowne przesłanie utraconego pakietu (jeśli to ten potwierdzający się zgubił to ponowne wysłanie nie jest dużym problememem, bo duplikat zostanie zignorowany ze względu na powtórzony numer sekwencji).

### Skąd TCP wie że połączenie zostało przerwane

Jesli chodzi o celowe zakończenie połączenia to realizuje się to przez wysłanie pakietu z bitem `FIN`. Jeśli połączenie zostanie nagle zerwane protokół nie otrzyma pakietu z potwierdzeniem odebrania, więc spróbuje wysłać pakiet ponownie. Po około 4-10 minutach prób się podda.

### Komunikacja półdupleksowa, a dupleksowa

Różnica polega na tym, że w komunikacji dupleksowej strony są w stanie przesyłać dane jednocześnie w obu kierunkach, a w półdupleksowej tylko na przemian.

### Jaki problem rozwiązuje sterowanie przepływem?

Kontrola przepływu polega na informowania nadawcy o rozmiarze wolnego miejsca w buforze wejściowym odbiornika (window), tak żeby nadawca nie mógł spowodować jego przepełnienia. Wartosc ta jest zmieniana dynamicznie w zależności od tego ile bajtów przybyło w buforze, a ile ubyło, bo aplikacja zaczęła z niego czytać.

## Zadanie 3

![](https://i.imgur.com/G5adzcZ.png)

Pierwszy uruchamia się serwer:

* Tworzy gniazdo wywołaniem `socket`
![](https://i.imgur.com/4Anc5YS.png)
* Przypisuje adres lokalny do gniazda wywołaniem `bind`.
![](https://i.imgur.com/MT8phkY.png)
* Rozpoczyna nasłuchiwanie nadchodzących połączeń wywołaniem `listen`
![](https://i.imgur.com/Y9dy5A9.png)
* A następnie wywołuje `accept`, które będzie blokujące dopóki nie pojawi się jakaś prośba o połączenie od klienta.
![](https://i.imgur.com/1r7XdCk.png)

Teraz może wystartować klient:
* Tworzy on gniazdo wywołaniem `socket`
* Następnie na otrzymanym deskryptorze wywołuje `connect` z adresem serwera, jednocześnie wiążąc gniazdo z adresem zdalnym.
![](https://i.imgur.com/kWwesw4.png)

W tym momencie połączenie zostaje nawiązane i można przejść do wymiany danych. Proces czyta i pisze do deskryptora zwróconego przez `accept`, a klient do deskryptora utworzonego przez wywołanie `socket`.

Proces wczytywania i przetwarzania danych będzie trwał aż klient nie zamknie deskryptora gniazda, co w efekcie zamknie połączenie i sprawi że serwer przeczyta EOF. W tym momencie serwer również może zamknąć deskryptor obecnego klienta i zaakceptować połączenie od kolejnego klienta stojącego w kolejce.

### Która ze stron używa portów efemerycznych?

Serwer będzie korzystał z jednego ze znanych portów (w zależności od protokołu warstwy aplikacji, np. dla http jest to port `80`), natomiast klient otrzyma port efemeryczny, w niewyspecyfikowany sposób wybrany z puli dostępnych portów przez jądro.

### Co specyfikuje drugi argument listen?

Drugi argument nazwany `backlog` oznacza maksymalną liczbę zakolejkowanych połączeń czekających na akceptację. Jeśli przy pełnej kolejce pojawi się nowa prośba o nawiązanie połączenia, to w zależności od użytego protokołu transportu, albo prośba użytkownika zostanie zignorowana (żeby spróbował jeszcze raz), albo zakończy się błędem po stronie użytkownika.

### Z jakimi portami związane są gniazda przekazywane do i z accept

Do accept przekazujemy deskryptor do gniazda nasłuchującego, którego port będzie jednym ze znanych portów, wybranych przez serwer, natomiast zwracany deskryptor gniazda klienta będzie miał port efemeryczny, wybrany przez jego jądro systemu przy wywołaniu `connect`.

### Skąd serwer wie że klient zakończył połączenie?

Protokół TCP określa sposób zamykania połączeń. Odbywa się to przez wysłanie pakietów z odpowiednimi flagami. Najpierw klient wyśle pakiet z flagą `FIN`

```
C --------FIN---------> S
```

Następnie serwer potwierdzi otrzymanie pakietu
```
C <-------ACK---------- S
```

Teraz kierunek `C->S` jest już zamknięty, w następnej kolejności serwer zrobi to samo co wcześniej klient:

```
C <-------FIN---------- S
```

```
C --------ACK---------> S
```

To zakończy połączenie TCP, a w efekcie z deskryptora klienta serwer przeczyta EOF.

## Zadanie 4

## Zadanie 5

### Short county na gniazdach

Na podstawie *The Linux Programming Interface: A Linux and UNIX System Programming Handbook (w sktrócie LPI) [61.1 Partial Reads and Writes on Stream Sockets]*

#### Read

Wywołanie `read` może zwrócić mniejszą liczbę bajtów niż ta o którą prosiliśmy jeśli w buforze gniazda znajduje się mniejsza, ale niezerowa liczba bajtów gotowych do odczytania. Jeśli wynosi ona zero, to wywołanie `read` jest blokujące.

#### Write

Wywołanie `write` będzie zwracać short count jeśli w buforze wyjściowym nie ma miejsca żeby na raz zapisać wszystkich bajtów, ale znajdzie się miejsce na zapisanie co najmniej jednego oraz jeden z następujących warunków jest prawdziwy:

* `write` został przerwany przez obsługę sygnału
* Gniazdo działało w trybie `O_NONBLOCK`.
* Wystąpił jakiś asynchroniczny błąd, związany np. z problemem z połączeniem TCP.

W przypadkach kiedy występuje występuje short count na read albo write (wywołany przerwaniem przez obsługę sygnału) często opłaca się zawołać to wywołanie jeszcze raz, aż nie uda się przeczytać tyle bajtów ile chcieliśmy.

### Z jakich przyczyn trzeba być przygotowanym na to że operacje na gniazdach moga zwrócić EINTR?

Wywołania operujące na gniazdach takie jak `read`, `accept`, `connect` itd. często sa blokujące i to na długi czas, a jak czytamy w LPI:

![](https://i.imgur.com/vO4DPKg.png)

W skrócie: jeśli zablokowane wywołanie zostanie przerwane obsługa sygnału, to w większości przypadków po wyjściu z handlera takie wywołanie natychmiast zakończy się z błędem `EINTR`, a w takim wypadku raczej sensowne jest jej zrestartowanie, a nie np. wykonywanie dalej programu.

### Skąd wiemy, że odebrany datagram jest w całości?

W przeciwieństwie do TCP, w którym operujemy na strumieniu danych, w UDP operujemy na datagramach, które są odbierane w całości przez aplikację, tak jak zostały przekazane niżej przez warstwę aplikacji wysyłającej. Dodatkowo do warstwy aplikacji przekazywane są także dane z nagłówka, które zawierają długość datagramu (która jest tu definiowalna w przeciwieństwie do TCP) oraz sumę kontrolną, które pozwalają na skontrolowanie czy pakiet dotarł w całości.

![](https://i.imgur.com/Hj6xmN1.png)

Z *UNIX Network Programming, Volume 1, Third Edition, The Sockets Networking:*

![](https://i.imgur.com/df7F0Ws.png)


### Co się stanie jeśli klient zapisze coś do gniazda w celu przesłania do serwera który zamknął połączenie?

Otrzyma sygnał `SIGPIPE`, oraz wywołanie zwróci błąd `EPIPE`.

![](https://i.imgur.com/PpgRQs6.png)

### Co by się stało gdyby zapomnieć o SO_REAUSEADDR w open_listenfd

Flaga ta jest użyta w `setsockopt`, wywołaniu służącym do zmiany konfiguracji gniazda. `SO_REAUSEADDR` pozwala na siłowe bindowanie się do gniazda, którego używała poprzednia instancja programu, a które pozostają jeszcze w stanie oczekiwania na pakiety przychodzące z dużym opóźnieniem (gniazda potrafią w takim stanie wisieć paręnaście sekund).

Gdyby zapomnieć o tej opcji, ponowne uruchomienie serwera natychmiast po jego wyłączeniu nie powiodłoby się, trzeba by odczekać za każdym razem ten czas w którym gniazda jeszcze oczekuję (co nie jest przyjemne kiedy się np. coś debuguje).

## Zadanie 6

### Dwa kluczowe wywołania systemowe

W kodzie programu najistotniejsze są dwa wywołania systemowe:

* `getaddrinfo(3)` -- odpowiada za translację nazwy hosta i usługi na addresy i porty. Zwraca 0 przy powodzeniu. Przyjmuje następujące argumenty:
  * `const char* node` -- nazwa hosta
  * `const char* service` -- nazwa usługi
  * `const struct addrinfo *hints` -- wskaźnik na strukturę `addrinfo`, której pola będą wykorzystywane do zawężenia wyników zwracanych przez tą procedurę.
  * `struct addrinfo **res` -- argument przez który zwrócony zostanie wskaźnik na zaalokowaną listę wiązaną wyników procedury, które zostaną zapisane w strukturach typu `addrinfo`.
* `getnameinfo(3)` -- funkcja odwrotna do `getaddrinfo`, odpowiada za translację adresów i portów na nazwy hosta i usługi. Przyjmuje następujące argumenty:
  * `const struct sockaddr *addr` -- wskaźnik na strukturę `sockaddr` zawierającą adres IP i numer portu.
  * `socklen_t addrlen` -- rozmiar pierwszego argumentu
  * `char *host` -- bufor na zwrócenie nazwy hosta
  * `socklen_t hostlen` -- rozmiar powyższego bufora
  * `char *serv` -- bufor na zwrócenie nazwy usługi
  * `socklen_t servlen` -- rozmiar powyższego bufora
  * `int flags` -- różnego rodzaju flagi sterujące zachowaniem, np flagi `NI_NUMERICHOST` i `NI_NUMERICSERV` sprawiają że do buforów drukowane są reprezentacje numeryczne, a nie nazwy, odpowiednich własności.

Przydatna jest również znajomość struktury `addrinfo`:

```c=
struct addrinfo {
   int              ai_flags;
   int              ai_family;
   int              ai_socktype;
   int              ai_protocol;
   socklen_t        ai_addrlen;
   struct sockaddr *ai_addr;
   char            *ai_canonname;
   struct addrinfo *ai_next;
};
```

### Zmodyfikowny program hostinfo

```c=
#include "csapp.h"

int main(int argc, char **argv) {
  struct addrinfo *p, *listp, hints;
  char *node;
  char *service = NULL;
  char ip_address_buf[MAXLINE];
  char port_buf[MAXLINE];
  int rc, flags;

  if (argc < 2 || argc > 3)
    app_error("usage: %s <domain name> [service name]\n", argv[0]);
  node = argv[1];
  if (argc == 3) service = argv[2];

  /* Get a list of addrinfo records */
  memset(&hints, 0, sizeof(struct addrinfo));
  hints.ai_family = AF_UNSPEC; /* IPv4 or IPv6 */
  hints.ai_socktype = SOCK_STREAM;
  /* Connections only */
  if ((rc = getaddrinfo(node, service, &hints, &listp)) != 0)
    gai_error(rc, "getaddrinfo");

  /* Walk the list and display each IP address */
  flags  = NI_NUMERICHOST; /* Display address string instead of domain name */
  flags |= NI_NUMERICSERV; /* Display port string instead of service name   */
  for (p = listp; p; p = p->ai_next) {
    Getnameinfo(
      p->ai_addr, p->ai_addrlen, 
      ip_address_buf, MAXLINE, port_buf, MAXLINE, 
      flags
    );
    
    printf(p->ai_family == AF_INET ? "%s" : "[%s]", ip_address_buf);
    if(service) printf(":%s", port_buf);
    printf("\n");
  }

  /* Clean up */
  freeaddrinfo(listp);

  return EXIT_SUCCESS;
}
```

### TFTPS

Jeśli spróbujemy wpisać tą usługę jako argument to otrzymamy błąd:

```
cezary@cezary-MSI$ ./hostinfo www.google.com tftp
getaddrinfo: Servname not supported for ai_socktype
```

Dzieje się tak ponieważ `TFTPS` to prosty protokół do przesyłania plików bazujący na `UDP`, zatem wykluczamy go podając typ gniazda `SOCK_STREAM` (czyli gniazda służące do poruzumiewania się z użyciem `TCP`), żeby program rozpoznawał `TFTPS` trzeba ustawić typ gniazda na dowolne, zgodnie z dokumentacją oznacza to pozostawienie tego pola wyzerowanego, więc wystarczy usunąć linijkę
```c=
  hints.ai_socktype = SOCK_STREAM;
```

## Zadanie 7

### Modyfikacja serwera

#### Obsługa sygnału

```c=
jmp_buf sig_ctx;
static void sigint_handler(int sig) {
  /* TODO: Change control flow so that it does not return to main loop. */
  siglongjmp(sig_ctx, sig);
}
```

#### Wydrukowanie wartości
```c=
/* TODO: Print bytes received after SIGINT has been received. */
int val = sigsetjmp(sig_ctx, 1);
if(val) {
  safe_printf("Server received %lu bytes\n", nread);
  exit(val);
}
```


### Uruchomienie serwera

Stan gniazd sieciowych możemy monitorować za pomocą programów `watch` i `netstat`. Ten pierwszy służy do uruchamiania poleceń co jakiś czas.

Zatem do powłoki wpisujemy polecenie `watch -n 1 netstat -ptn`.

![](https://i.imgur.com/5dc6Fp4.png)


Następnie uruchamiamy serwer poleceniem `./echoserver 7777 &` i klienta poleceniem `./echoclient 127.0.0.1 7777` adres `127.0.0.1` to standardowy adres loopback, który zawraca wysyłane pakiety z powrotem do naszego komputera bez przesyłania ich do sieci.

Powinniśmy teraz ujrzeć ustanowione połączenia na wydruku polecenia `netstat`:

![](https://i.imgur.com/yYrNBG5.png)

![](https://i.imgur.com/WOuXC18.png)

Jak widać w tym przypadku klient otrzymał port `60222`. Po wspianiu linii na standardowe wejście klienta, serwer będzie je nam odsyłał.

Teraz startujemy drugiego klienta, na wydruku pojawia się nowe połączenie:

![](https://i.imgur.com/WYuvux8.png)

![](https://i.imgur.com/5224YeK.png)

Jednak po wpisaniu linii na wejściu klienta nie zostają one odesłane. Dzieje się tak ponieważ serwer nie jest wielobieżny i jest w stanie obsługiwać tylko jednego klienta na raz, żeby kolejny mógł się połączyć najpierw musi zostać zamknięte połączenie z pierwszym.

### Wireshark

Uruchamiamy program z uprawnianimi roota `sudo wireshark` następnie wybieramy interfejs `loopback`. 

#### Pakiety przesłane przy nawiązaniu połączenia

![](https://i.imgur.com/eBQDKtA.png)

#### Pakiety przesłane przy przesyłaniu danych

![](https://i.imgur.com/agwBgL9.png)

![](https://i.imgur.com/TRwM0XR.png)

## Zadanie 8

### Wypisywanie liczby przeczytanych bajtów

Prawie tak samo jak w poprzednim zadaniu, tylko czekamy aż wszystkie dzieci skończą działanie:

```c=
/* TODO: Need to define context to be used with sigsetjmp & siglongjmp. */
static jmp_buf sig_ctx;
[...]
static void sigint_handler(int sig) {
  safe_printf("Server received quit request!\n");
  /* TODO: Change control flow so that it does not return to main loop. */
  siglongjmp(sig_ctx, sig);
}
```

```c=
/* TODO: Wait for all clients to quit and print a message with nread. */
int val = sigsetjmp(sig_ctx,1);
if (val) {
  while (nclients > 0) pause();
  safe_printf("Server received %lu bytes\n", nread);
  exit(val);
}
```

### Tworzenie dziecka dla klienta

```c=
/* TODO: Start client in subprocess, close unused file descriptors. */
client_t *new_client = addclient();
pid_t pid = Fork();
if(pid == 0) {
  Signal(SIGINT, SIG_DFL);
  Close(listenfd);
  echo(new_client, connfd);
  Close(connfd);
  exit(0);
}
new_client->pid = pid;
Close(connfd);
printf("[%d] Connected to %s:%s\n", pid, client_hostname, client_port);
```

### Grzebanie dzieci

```c=
static void sigchld_handler(int sig) {
  pid_t pid;
  /* TODO: Delete clients as they die. */
  while ((pid = waitpid(-1, NULL, WNOHANG)) > 0) {
    delclient(pid);
    safe_printf("[%d] Disconnected!\n", pid);
  } 
}
```

### Przykładowe działanie serwera

![](https://i.imgur.com/XvWbMwr.png)
