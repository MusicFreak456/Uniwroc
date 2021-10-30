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

function time(fun,arg,label){
    console.time("    " + label)
    fun(arg)
    return console.timeEnd("    " + label);
}

function benchmark(){
    console.log(" n    time")
    console.log("-----------------")
    values_of_n = Array.from(Array(31), (elem,index) => index + 10)
    for(let n of values_of_n){
        console.log(` ${n}   `)
        time(fib_rec,n,"rec")
        time(fib_iter,n,"iter")
    }
}

benchmark()

// Czas wykonywania w środowisku node.js i w przeglądarce
// Chromium był bardzo zbliżony, natomiast w Firefoxie
// czasy dla wersji rekurencyjnej były nawet 55 razy 
// gorsze! (dla n=40)