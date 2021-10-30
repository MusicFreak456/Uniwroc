# Zadanie 7

ASCII - popularny siedmiobitowy system kodowania znaków, w którym liczbom od 0 do 127 przypisuje się znaki i kody sterujące.

Kod sterujący - w przeciwieństwie do znaków drukowalnych, nie zostają one wyświetlone/wydrukowane, tylko sterują urządzeniem odbierającym dane, przykładem może być kod przeniesienia do nowej lini.

0 - NULL - oryginalnie reprezentował brak informacji, urządzenia miały na niego nie reagować.
Współcześnie używany na przykład w null-terminated-string'ach, w których oznacza ich koniec.

4 - EOT - oznacza koniec wprowadzania danych. Stosowany np. w programach które oczekują na wprowadzenie danych przez użytkownika. Koniec wprowadzania danych jest sygnalizowany programowi właśnie za pomocą EOT (ctrl + D).

7 - BELL - powoduje wydanie dźwięku, np by ostrzec operatora urządzenia o otrzymaniu wiadomości.

10 - EOL - sygnalizuje koniec bieżacego wiersza tekstu.

12 - FF - sygnalizuje koniec bieżącej strony tekstu. Np. w drukarkach powoduje przeniesienie się na kolejną kartkę.