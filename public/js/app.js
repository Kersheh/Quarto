$(() => {
  var modules = () => {
    return (typeof io !== 'undefined' &&        // socket.io
            typeof R !== 'undefined' &&         // ramda js -- functional library
            typeof Maybe !== 'undefined' &&     // monet js -- Maybe monad
            typeof Either !== 'undefined');     // monet js -- Either monad
  };
  if(!modules()) {
    console.log('Modules missing.');
  }
  else {
    var socket = io.connect();
    initApp(socket);
  }
});
