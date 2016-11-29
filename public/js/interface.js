var UI = (() => {
  // play piece at location
  var playPiece = (x, y, classes) => {
    var selector = $('tr[y="' + y + '"]').children('td[x="' + x + '"]');
    selector.append('<div class="' + classes + '"></div>');
    selector.removeClass('empty');
  };

  // hide opponent piece selection
  var hideNext = () => {
    $('#info-next-piece').hide();
  };

  // show opponent piece selection
  var showNext = () => {
    $('#info-next-piece').show();
  };

  // update piece to play
  var updateNext = (classes) => {
    $('#next-piece').removeClass();
    $('#next-piece').addClass('piece');
    classes.forEach((x) => {
      $('#next-piece').addClass(x);
    });
    showNext();
  };

  // piece and tile selection
  var selected_piece = undefined;
  var clearSelectedPiece = () => {
    selected_piece = undefined;
  };
  var selected_tile = undefined;
  var clearSelectedTile = () => {
    selected_tile = undefined;
  };
  var eventListeners = (pieces, tiles) => {
    // piece selectors
    pieces.forEach((selector) => {
      selector.on('click', () => {
        if(typeof selected_piece !== 'undefined'){
          selected_piece.removeClass('select');
        }
        selector.addClass('select');
        selected_piece = selector;
      });
    });
    // tile selectors
    tiles.forEach((selector) => {
      selector.on('click', () => {
        if(typeof selected_tile !== 'undefined'){
          selected_tile.removeClass('select');
        }
        selector.addClass('select');
        selected_tile = selector;
      });
    });
    // select button
    // $('.select-button').on('click', () => {
    //   getTile();
    // });
  };

  // get selected tile coordinates
  var getTile = () => {
    if(typeof selected_tile !== 'undefined') {
      return {
        x: selected_tile.attr('x'),
        y: selected_tile.parent().attr('y')
      };
    }
    else {
      return undefined;
    }
  };

  var selectTile = (callback, board, pieces, turn) => {
    $('.select-button').on('click', () => {
      console.log(UI.getTile());
      callback(board, pieces, 'p1');
      // callback(board, pieces, turn);
      // runGame(board, pieces, 'p1');
    });
  };

  return {
    eventListeners: eventListeners,
    clearSelectedPiece: clearSelectedPiece,
    clearSelectedTile: clearSelectedTile,
    getTile: getTile,
    selectTile: selectTile,
    playPiece: playPiece,
    hideNext: hideNext,
    updateNext: updateNext
  };
})();
