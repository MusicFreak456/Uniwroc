var process = require('process')

console.log("Podaj imię:")

process.stdin.on('data', (buffer) => {
  let name = buffer.toString()
  console.log(`Witaj ${name}`)
  process.stdin.pause()
})