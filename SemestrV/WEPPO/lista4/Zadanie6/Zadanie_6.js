var readline = require('readline')
var fs = require('fs')

async function process_logs() {
  let input = fs.createReadStream('logs.txt') // wygenerowane przez gen.py
  let readlineI = readline.createInterface({input})

  var resultDict = {}
  for await (let line of readlineI) {
    let ip = line.split(" ")[1]
    if(resultDict[ip])
      resultDict[ip]++
    else
      resultDict[ip] = 1
  }

  return resultDict
}

function sortDict(dict) {
  let array = []
  for(key in dict) {
    array.push([key, dict[key]])
  }
  array.sort((a,b) => b[1] - a[1])
  return array
}

process_logs().then(dict => {
  let sorted = sortDict(dict)

  for(let entry of sorted.slice(0,3)) {
    console.log( entry[0].toString() + " " + entry[1].toString() )
  }
})