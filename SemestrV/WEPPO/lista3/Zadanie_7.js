function fib() {
  var curr = 0;
  var next = 1;
  return {
    next : () => {
      let temp = curr;
      curr = next;
      next = curr + temp;
      return {
        value: temp,
        done: false
      }
    }
  }
}

let n = 0

var _it = fib();
for (var _result; _result = _it.next(), !_result.done;) {
  console.log(_result.value);
  n++;
  if(n == 5) break; // bez tego będzie kontynuować w nieskończoność
}

// Czy da się przez for-of?
// let n = 0

// for(let f of fib()){
//   console.log(f)
//   n++;
//   if(n == 5) break; // bez tego będzie kontynuować w nieskończoność
// }

// Nie

function *fib_yield(){
  var curr = 0;
  var next = 1;

  while(true) {
    let temp = curr;
    curr = next;
    next = curr + temp;
    yield temp;
  }
}

n = 0
var _it = fib_yield();
for (var _result; _result = _it.next(), !_result.done;) {
  console.log(_result.value);
  n++;
  if(n == 5) break; // bez tego będzie kontynuować w nieskończoność
}

// Czy da się przez for-of?
n = 0;
for(let f of fib_yield()){
  console.log(f)
  n++;
  if(n == 5) break; // bez tego będzie kontynuować w nieskończoność
}
// Tak!
