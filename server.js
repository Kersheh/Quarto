var path = require('path');
var express = require('express');

var app = express();
var port = process.env.PORT || 8080;

app.use('/', express.static(path.join(__dirname, 'public')));
app.use('/node_modules', express.static(path.join(__dirname, 'node_modules')));

app.listen(port, () => {
  console.log('Listening on', port);
});
