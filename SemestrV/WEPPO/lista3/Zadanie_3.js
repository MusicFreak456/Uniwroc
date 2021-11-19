function forEach( a, f ) {
  for (let elem of a) {
    f(elem)
  }
} 

function map( a, f ) {
  let mapped = []
  forEach(a, x => mapped.push( f(x) ))
  return mapped
}

function filter( a, f ) {
  let filtered = []
  forEach(a, x => f(x) ? filtered.push(x) : {})
  return filtered
}

var a = [1,2,3,4];

forEach( a, console.log ) // zwykła
forEach( a, x => { console.log( x ); } ) // lambda wyrażenie
// [1,2,3,4]

let filtered = filter( a, function(x) { return x % 2 == 0}) // zwykła
// [2,4]
console.log(filtered)

filtered = filter( a, x => x < 3 ) // lambda wyrażenie
// [1,2]
console.log( filtered )

let mapped = map( a, function(x) { return x * x }) // zwykła
// [1,4,9,16]
console.log(mapped)

mapped = map( a, x => x * 2 ) // lambda wyrażenie
// [2,4,6,8]
console.log(mapped)