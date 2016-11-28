var UI = (() => {
  // play piece at location
  var playPiece = (x, y, classes) => {
    // update DOM
    var selector = $('tr[y="' + y + '"]').children('td[x="' + x + '"]');
    selector.append('<div class="' + classes + '"></div>');
    selector.removeClass('empty');
  };

  var selected_piece = undefined;
  var eventListeners = (selectors) => {
    // piece selectors
    selectors.forEach((selector) => {
      selector.on('click', () => {
        if(typeof selected_piece !== 'undefined'){
          selected_piece.removeClass('select');
        }
        selector.addClass('select');
        selected_piece = selector;
      });
    });
    // play location selectors
  };

  return {
    eventListeners: eventListeners,
    playPiece: playPiece
  };
})();
