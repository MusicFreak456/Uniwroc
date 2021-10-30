# Zadanie 8

Głównym ograniczeniem ASCII była możliwość przedstawienia jedynie 128 znaków, co nie pozwalało na pokrycie znaków diaktrytycznych, powszechnie występujących w wielu systemach pisma. Z myślą o nich powstał UTF-8, w którym da się reprezentować wszystkie znaki ze standardu Unicode.

## Sposób kodowania

x'y to kolejne bity oznaczenia znaku w Unicode

| Przedział | Kod |
|---|--|
| 0x00 do 0x7F     | 0xxxxxxx |
| 0x80 do 0x7FF           | 110xxxxx 10xxxxxx |
| 0x800 do 0xFFFF         | 1110xxxx 10xxxxxx 10xxxxxx |
| 0x10000 do 0x1FFFFF     | 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx |
| 0x200000 do 0x3FFFFFF   | 111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx |
| 0x4000000 do 0x7FFFFFFF | 1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx |

## Zapis podanego ciągu

| Znak | Unicode | UTF-8 | HEX |
| -- | -- | -- | -- |
| P | U+0050 | 0101 0000 | 0x50
| r | U+0072 | 0111 0010 | 0x72
| o | U+006F | 0110 1111 | 0x6F
| s | U+0073 | 0111 0011 | 0x73
| z | U+007A | 0111 1010 | 0x7A
| ę | U+0119 | 1100 0100 1001 1001 | 0xC499
|   | U+0020 | 0010 0000 | 0x20
| z | U+007A | 0111 1010 | 0x7A
| a | U+0061 | 0110 0001 | 0x61
| p | U+0070 | 0111 0000 | 0x70
| ł | U+0142 | 1100 0101 1000 0010 | 0xC582
| a | U+0061 | 0110 0001 | 0x61
| c | U+0063 | 0110 0011 | 0x63
| i | U+0069 | 0110 1001 | 0x69
| ć | U+0107 | 1100 0100 1000 0111 | 0xC487
|   | U+0020 | 0010 0000 | 0x20
| 5 | U+0035 | 0011 0101 | 0x35
| € | U+20AC | 1110 0010 1000 0010 1010 1100 | 0xE282AC
| ! | U+0021 | 0010 0001 | 0x21