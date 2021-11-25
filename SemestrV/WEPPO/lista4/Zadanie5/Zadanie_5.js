var fs = require('fs')

fs.promises.readFile('./plik.txt')
  .then(buffer => {
    console.log(buffer.toString())
  })