function fib_rec(n){
  if(n == 0) return 0;
  if(n == 1) return 1;
  return fib_rec(n-2) + fib_rec(n-1)
}

function fib_iter(n){
  prev = 0
  curr = 1
  if(n == 0) return prev
  if(n == 1) return curr
  for(let i = 1; i < n; i++){
    let next = prev + curr;
    prev = curr
    curr = next
  }
  return curr;
} 

function memoize(fun) {
  let cache = {};

  return n => {
    if( cache[n] === undefined ) {
      return cache[n]
    } else {
      let result = fun(n)
      cache[n] = result
      return result
    }
  }
}

let fib_memo = memoize(fib_rec)

function time(fun,arg,label){
  console.time("    " + label)
  fun(arg)
  return console.timeEnd("    " + label);
}

function microbenchmark(){
  console.log(" n    time")
  console.log("-----------------")
  values_of_n = Array.from(Array(31), (elem,index) => index + 10)
  for(let n of values_of_n){
    console.log(` ${n}   `)
    time(fib_rec,  n, "rec" )
    time(fib_iter, n, "iter")
    time(fib_memo, n, "memo")
  }
}

microbenchmark()

// Wyniki wskazują na to, że wyliczanie n-tej liczby fibbonacciego techniką
// memoizacji jest najszybsze.