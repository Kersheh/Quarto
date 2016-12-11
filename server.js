var path = require('path');
var irc = require('irc');
var express = require('express');
var app = express();
var http = require('http').Server(app);
var io = require('socket.io')(http);

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

// quarto socket.io interaction
io.on('connection', (socket) => {
  // client connect
  console.log('Local client connected.');

  // IRC message listener
  var ircListener = (from, to, message) => {
    // client.say(config.channels[0], 'Opponent message received.');
    console.log('Opponent:', JSON.parse(message.args[1]));
    socket.emit('opponent turn', message.args[1]);
  };

  // establich IRC connection when local user connects to app
  client.connect(config.reconnect, () => {
    console.log('Bot connected to channel ' + config.channels[0] + ' @ ' + config.server);

    // attach channel listeners
    client.addListener('message' + config.channels[0], ircListener);
  });

  // local client turn
  socket.on('local turn', (turn) => {
    console.log('Local:', JSON.parse(turn));
    // send turn over IRC
    client.say(config.channels[0], turn);
  });

  // client disconnect
  socket.on('disconnect', () => {
    console.log('Local client disconnected.');
    client.removeListener('message' + config.channels[0], ircListener);
    client.disconnect();
  });

  // hardcoded player first turn selection
  socket.emit('first', 'p1');
});

// execute game server
http.listen(port, () => {
  console.log('Quarto server listening on', port);
});
