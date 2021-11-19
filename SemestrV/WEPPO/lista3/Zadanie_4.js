function createFs(n) { // tworzy tablicę n funkcji
  var fs = []; // i-ta funkcja z tablicy ma zwrócić i
  for ( var i=0; i<n; i++ ) {
    fs[i] = 
      function() {
        return i;
      };
  };
  return fs;
}

var myfs = createFs(10);

console.log( myfs[0]() ); // zerowa funkcja miała zwrócić 0
console.log( myfs[2]() ); // druga miała zwrócić 2
console.log( myfs[7]() );
// 10 10 10

// Dzieje się tak ponieważ zmienne `var` mają zasięg w obrębie całej funkcji, w
// której się pojawiają, co powoduje że każda z tworzonych i dołączanych do
// tablicy funkcji posiada w domknięciu to samo `i` i po zwiększeniu jego 
// w pętli, będzie miało ono taką samą wartość, równą `n`. 

// Można poradzić sobie z tym zamieniając `var` na `let`, którego zasięg jest
// ograniczony do bloku.

function createFsLet(n) { // tworzy tablicę n funkcji
  var fs = []; // i-ta funkcja z tablicy ma zwrócić i
  for ( let i=0; i<n; i++ ) {
    fs[i] = 
      function() {
        return i;
      };
  };
  return fs;
}

myfs = createFsLet(10);

console.log( myfs[0]() ); // 0
console.log( myfs[2]() ); // 2
console.log( myfs[7]() ); // 7

// Alternatywnie można wykorzystac fakt, że `var` ma zasięg tylko w obrębie 
// funkcji i w klasycznym stylu stworzyć natychmiastowo wywołaną funkcję(IIFE), 
// która zwróci funkcję, którą dodamy do tablicy

function createFsIIFE(n) { // tworzy tablicę n funkcji
  var fs = []; // i-ta funkcja z tablicy ma zwrócić i
  for ( var i=0; i<n; i++ ) {
    fs[i] = 
      function() {
        var _i = i
        return function() {
          return _i;
        };
      }();
  };
  return fs;
}

myfs = createFsIIFE(10);

console.log( myfs[0]() ); // 0
console.log( myfs[2]() ); // 2
console.log( myfs[7]() ); // 7