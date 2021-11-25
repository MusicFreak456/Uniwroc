module.exports = female
var male = require('./male')

function female(n) {
  if(n <= 0) return 1;
  return n - male(female(n-1))
}