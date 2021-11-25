module.exports = male
var female = require('./female')

function male(n) {
  if(n <= 0) return 0
  return n - female(male(n-1))
}