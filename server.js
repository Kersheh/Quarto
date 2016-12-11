var path = require('path');
var express = require('express');
var irc = require('irc');

var app = express();
var port = process.env.PORT || 8080;

// server public files
app.use('/', express.static(path.join(__dirname, 'public')));
app.use('/node_modules', express.static(path.join(__dirname, 'node_modules')));

// IRC connection
var config = {
	channels: ['#quarto-test'],
	server: 'irc.freenode.net',
  port: 6667,
	botName: 'quarto-bot',
  connect: false,
  reconnect: 0
};

// IRC bot client
var client = new irc.Client(config.server, config.botName, {
  port: config.port,
	channels: config.channels,
  autoConnect: config.connect
});

// establich IRC connection
client.connect(config.reconnect, () => {
  console.log('Bot connected to channel ' + config.channels[0] + ' @ ' + config.server);

  // attach channel listeners
  client.addListener('message' + config.channels[0], (from, to, text, message) => {
  	client.say(config.channels[0], 'These aren\'t the droids you\'re looking for.');
  });
});

// execute game server
app.listen(port, () => {
  console.log('Quarto server listening on', port);
});
