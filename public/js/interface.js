var UI = (() => {
  // append list of classes to a jQuery selector
  var appendClasses = (classes, selector) => {
    classes.forEach((x) => {
      selector.addClass(x);
    });
  };

  // apply list of classes to list of jQuery selectors
  var applyClassesToSelectors = (classes, selectors) => {
    R.zipWith(appendClasses, classes, selectors);
  };

  // remove piece from selectable pieces
  var removePiece = (classes) => {
    var c = R.flatten(R.map((x) => {
      return R.prepend('.', x);
    }, classes)).join('');
    $('.piece' + c).remove();
  };

  // play piece at location
  var playPiece = (x, y, classes) => {
    var selector = $('tr[y="' + y + '"]').children('td[x="' + x + '"]');
    selector.append('<div class=""></div>');
    selector.removeClass('empty');
    appendClasses(classes, selector.children());
  };

  // hide opponent piece selection
  var hideNext = () => {
    $('.p1-select').show();
    $('.p2-select').hide();
  };

  // show opponent piece selection
  var showNext = () => {
    $('.p1-select').hide();
    $('.p2-select').show();
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
  var attachListeners = (pieces_selectors, tiles_selectors) => {
    // piece selectors
    pieces_selectors.forEach((selector) => {
      selector.on('click', () => {
        if(typeof selected_piece !== 'undefined'){
          selected_piece.removeClass('select');
        }
        selector.addClass('select');
        selected_piece = selector;
      });
    });
    // tile selectors
    tiles_selectors.forEach((selector) => {
      selector.on('click', () => {
        if(typeof selected_tile !== 'undefined'){
          selected_tile.removeClass('select');
        }
        selector.addClass('select');
        selected_tile = selector;
      });
    });
  };

  // get selected tile coordinates
  var getTile = () => {
    if(typeof selected_tile !== 'undefined') {
      return {
        x: parseInt(selected_tile.attr('x')),
        y: parseInt(selected_tile.parent().attr('y'))
      };
    }
    else {
      return undefined;
    }
  };

  var selectTile = (callback, updateBoard, updatePieces, board, piece, pieces, turn) => {
    if(turn === 'p2') {
      $('.select-button').on('click', () => {
        var new_board = updateBoard(board, piece, getTile().x, getTile().y);
        var new_pieces = updatePieces(piece, pieces);
        playPiece(getTile().x, getTile().y, piece);
        hideNext();
        removePiece(piece);
        callback(new_board, new_pieces, 'p1');
      });
    }
  };

  // setup UI
  var setup = (pieces, pieces_selectors, tiles_selectors) => {
    applyClassesToSelectors(pieces, pieces_selectors);
    attachListeners(pieces_selectors, tiles_selectors);
  };

  return {
    getTile: getTile,
    selectTile: selectTile,
    playPiece: playPiece,
    hideNext: hideNext,
    updateNext: updateNext,
    setup: setup
  };
})();
