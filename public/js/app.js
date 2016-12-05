$(() => {
  var modules = () => {
    return (typeof R !== 'undefined' &&        // ramda js -- functional library
            typeof Maybe !== 'undefined' &&    // monet js -- Maybe monad
            typeof Either !== 'undefined');   // monet js -- Either monad
            // typeof UI !== 'undefined');        // web app UI with jQuery
  };

  if(!modules()) {
    console.log('Modules missing.');
  }
  else {
    quarto();
  }
});
