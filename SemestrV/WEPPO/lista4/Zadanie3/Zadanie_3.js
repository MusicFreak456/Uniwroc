// Czy możliwa jest sytuacja że dwa moduły tworzą cykl?
// Tak!

// https://en.wikipedia.org/wiki/Hofstadter_sequence#Hofstadter_Female_and_Male_sequences
var male = require('./male')
var female = require('./female')

console.log(female(4)) // 3
console.log(male(4))   // 2