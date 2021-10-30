console.log( (![]+[])[+[]] + (![]+[])[+!+[]] + ([![]]+[][[]])[+!+[]+[+[]]] + (![]+[])[!+[]+!+[]] );

let temp = +!+[] + [0]
console.log(temp)

let firstGroup = (![]+[])[+[]]
// ![] -> false
// ![]+[] -> 'false'
// +[] -> 0
// (![]+[])[+[]] -> 'false'[0] -> 'f'
console.log(firstGroup) // 'f'

let secondGroup = (![]+[])[+!+[]]
// ![]+[] -> 'false' (jak w poprzednim)
// +[] -> 0
// +!+[] -> +!0 -> +true -> 1
// (![]+[])[+!+[]] -> 'false'[1] -> 'a'
console.log(secondGroup) // 'a'

let thirdGroup = ([![]]+[][[]])[+!+[]+[+[]]]
// [+[]] -> [0]
// +!+[] -> +!0 -> +true -> 1
// +!+[]+[+[]] -> 1 + [0] -> '10'
// [][[]] -> undefined
// [![]] -> [false]
// [![]]+[][[]] -> [false] + undefined -> 'falseundefined'
// ([![]]+[][[]])[+!+[]+[+[]]] -> 'falseundefined'['10'] -> 'i'
console.log(thirdGroup)

let fourthGroup = (![]+[])[!+[]+!+[]]
// ![]+[] -> 'false' (jak w poprzednich)
// +!+[] -> +!0 -> +true -> 1
// !+[] -> !0 -> true
// !+[]+!+[] -> true + 1 -> 2
// (![]+[])[!+[]+!+[]] -> 'false'[2] -> 'l'
console.log(fourthGroup)