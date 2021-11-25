var fs = require('fs')
var util = require('util')

var filePath = './plik.txt'

// pokazać klasyczny interfejs programowania asynchronicznego
fs.readFile(filePath, (error, buffer) => {
  if(error)
    console.log(error)
  else
    console.log(buffer.toString())
})

// Następnie pokazać jak taki klasyczny interfejs można zmienić na Promise 
// na trzy sposoby:

// • za pomocą ”ręcznie” napisanej funkcji przyjmującej te same argumenty co 
// fs::readFile ale zwracającej Promise
function manualPromisifyReadFile(path) {
  let promise = new Promise( (resolve, reject) => {
    fs.readFile(path, (error, buffer) => {
      if(error)
        reject(error)
      else
        resolve(buffer)
    })
  })
  return promise
}

manualPromisifyReadFile(filePath)
  .then(buffer => {
    console.log(buffer.toString())
  })
  .catch(error => console.log(error))

//• za pomocą util.promisify z biblioteki standardowej
let promisifyFileRead = util.promisify(fs.readFile)

promisifyFileRead(filePath)
  .then(buffer => {
    console.log(buffer.toString())
  })
  .catch(error => console.log(error))

//• za pomocą fs.promises z biblioteki standardowej
fs.promises.readFile(filePath)
  .then(buffer => {
    console.log(buffer.toString())
  })
  .catch(error => console.log(error))

// Na zademonstrowanym przykładzie pokazać dwa sposoby obsługi funkcji 
// zwracającej Promise

// • ”po staremu” - wywołanie z kontynuacją (Promise::then)
// Pokazane wyżej

// • ”po nowemu” - wywołanie przez async/await
async function read(path) {
  try {
    let buffer = await fs.promises.readFile(path)
    console.log(buffer.toString());
  }
  catch ( error ) {
    console.log(error);
  }
}

read(filePath);