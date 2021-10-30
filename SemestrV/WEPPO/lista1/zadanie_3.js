function isPrime(n) {
    let sqrt = Math.sqrt(n)
    if (n <= 3) return n > 1
    for (let i = 2; i <= sqrt; i++) {
        if(n % i == 0) return false
    }
    return true
}

function construct_set() {
    let result = []
    for (let i = 1; i <= 100000; i++) {
        if(isPrime(i))result.push(i)
    }
    return result
}

console.log(construct_set())