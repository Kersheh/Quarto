$(function() {
  // piece characteristics
  piece_height = ['tall', 'short'];
  piece_top = ['holey', 'unholey'];
  piece_colour = ['dark', 'light'];
  piece_shape = ['box', 'cylinder'];

  // cartesian product of list of lists
  var cartesianProduct = (list) => {
    return R.reduce((a, b) => {
      return R.unnest(R.map((x) => {
        return R.map((y) => {
          return R.concat(x, [y]);
        }, b);
      }, a));
    }, [[]], list);
  };

  // pre-generated list of all pieces
  pieces = cartesianProduct([piece_height, piece_top, piece_colour, piece_shape]);

  // jQuery pieces selectors
  pieces_selectors = [$('#p0'), $('#p1'), $('#p2'), $('#p3'),
                      $('#p4'), $('#p5'), $('#p6'), $('#p7'),
                      $('#p8'), $('#p9'), $('#p10'), $('#p11'),
                      $('#p12'), $('#p13'), $('#p14'), $('#p15')];

  // append classes to a jQuery selector
  var appendClasses = (classes, selector) => {
    R.forEach((x) => {
      selector.addClass(x);
    }, classes);
  }
  // apply piece classes to jQuery selectors
  R.zipWith(appendClasses, pieces, pieces_selectors);
});
