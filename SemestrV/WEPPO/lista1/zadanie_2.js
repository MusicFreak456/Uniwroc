function getDigits(n){
    let digits = []
    let number = n

    do{
        digits.push(number % 10)
        number = Math.floor(number/10)
    }while(number > 0)

    return digits
}

function divisibleByDigits(n, digits){
    for(let digit of digits){
        if(digit == 0) return false
        if(n % digit != 0) return false
    }
    return true;
}

function test(n) {
    let digits = getDigits(n)
    let sumOfDigits = digits.reduce((previous,current) => previous + current)
    return divisibleByDigits(n, digits) && (n%sumOfDigits == 0) 
}

function construct_set() {
    let result = []
    for (let i = 1; i <= 100000; i++) {
        if (test(i)) result.push(i)
    }
    return result
}

console.log(construct_set())