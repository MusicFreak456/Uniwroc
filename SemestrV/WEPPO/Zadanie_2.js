console.log('!!!Opis w komentarzach kodu!!!')

let number = 42
/**********************************************************************************************/
console.log('* użycie operatorów . oraz [] do odwoływania się do składowych obiektu.')

let example1 = {
    bar: 'foobar'
}

// odwołanie za pomocą .
console.log(example1.bar)
// odwołanie za pomocą []
console.log(example1['bar'])
// [] daje nam więcej możliwości, np. możemy umieścić wyrażenie które obliczy się do nazwy pola
// oraz mamy dużą swobodę w używaniu różnych znaków
example1['field ' + 'from ' + '/(*expresion*)'] = number
console.log(Object.keys(example1))


/**********************************************************************************************/
console.log('* użycie argumentów innego typu niż string dla operatora [] dostępu do składowej obiektu.')

let example2 = {
    bar: 'foobar'
}

let example3 = {}

// liczba zamiast stringa
example2[number] = 'foo' // na liczbie zostanie wywołana konwersja toString pole zostanie nazwane 
                          // zgodnie z wynikiem
console.log(Object.keys(example2))

// obiekt zamiast stringa
example2[example3] = 'bar' // na obiekcie zostanie wywołana konwersja toString, a pole zostanie 
                             // nazwane zgodnie z wynikiem
console.log(Object.keys(example2))

// Programista może przeciążyć tę funkcję i w ten sposób kontrolować do jak nazwanego pola się odwoła
example3.toString = () => '42'
console.log(example2[example3])

console.log('*  użycie argumentów innego typu niż number dla operatora [] dostępu do tablicy')

let array = [0,1,2,3,4,5,6,7,8,9]
let example4 = { toString: () => '5'}

// string zamiast number
console.log(array[0], array['0']) // odwoływanie się do elementów tablicy działa tak samo jak odwołanie
                                  // do pól obiektu, więc użycie napisu zamiast liczby tak naprawdę
                                  // niczym się nie różni
// object zamiast number
console.log(array[example4]) // tutaj również zachowanie jest takie samo jak w przypadku obiektów

// dopisanie klucza, który nie jest liczbą
array['key'] = 'test' // do obiektu tablicy dodany zostaje klucz z wartością, ale długość tablicy 
                      // się nie zmienia (pola obiektu są traktowane inczej niż elementy tablicy)
console.log(array) 
console.log(array.length)

// zmienienie wartości pola length
let array2 = [0,1,2,3,4,5,6,7,8,9]

array2.length = 8
console.log(array2.length)
console.log(array2) // zostało 8 pierwszych elementów, dwa ostatnie zostały usunięte
array2.length = 12
console.log(array2.length)
console.log(array2) // tablica została rozszerzona do 12 elementów, z czego nowe elementy dołączone
                    // na końcu są puste