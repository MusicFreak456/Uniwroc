var http = require('http')
var express = require('express')

var app = express();

app.use( (req, res) => {
  res.header('Content-disposition', 'attachment; filename="test.txt"');
  res.end('test'); // napis `test` zostanie zapisany do pliku test.txt
})

http.createServer(app).listen(8080)