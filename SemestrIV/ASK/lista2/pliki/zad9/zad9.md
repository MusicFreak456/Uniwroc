## Zadanie 9

Można zastosować metodę dziel i zwyciężaj. Mając dwa bity możemy określić parzystość jedynek wśród nich za pomocą xora. Znając parzystość jedynek w dwóch przedziałach, parzystość ich obu będzie również określona przez xor tych wartości. Wynik xora będziemy zapisywać zawsze w LSB każdej dwójki, czwórki, ósemki,... itd. bitów.


```c=
int32_t odd_ones(uint32_t x){
	x = x ^ (x>>1);
	x = x ^ (x>>2);
	x = x ^ (x>>4);
	x = x ^ (x>>8);
	return (x ^ (x>>16)) & 1;
}
```