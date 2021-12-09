var http = require('http')
var express = require('express')
var args = require('./args')
var app = express()

const decl = args.emptyDeclaration
const labels = args.labels

app.set('view engine', 'ejs')
app.set('views', './views')

app.use(express.static('./static'))

app.use(express.urlencoded({extended:true}))

app.get('/', (req, res) => {
  res.render('index', {decl, labels})
})

app.post('/', (req, res) => {
  let decl = req.body
  let [valResult, errMsg] = args.validate(decl)
  if(valResult)
    res.redirect('/print' + args.urlParamsOfArgs(decl))
  else
    res.render('index', {decl, labels, errMsg})
})

app.get('/print', (req, res) => {
  res.render('print', {decl: req.query, labels})
})

http.createServer(app).listen(8080)