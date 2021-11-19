// Wystarczy dodać funkcję przyjmującą parametr i zwrócić funkcję bezargumentową
// z nim w domknięciu

let createGenerator = n => () => {
  var _state = 0;
  return {
    next: function () {
      return {
        value: _state,
        done: _state++ >= n
      }
    }
  }
}

var foo = {
  [Symbol.iterator]: createGenerator(10)
};

for (var f of foo)
  console.log(f);

console.log("\n");

var foo1 = {
  [Symbol.iterator]: createGenerator(12)
};
  
for (var f of foo1)
  console.log(f);

console.log("\n");

var foo2 = {
  [Symbol.iterator]: createGenerator(14)
};
  
for (var f of foo2)
  console.log(f);