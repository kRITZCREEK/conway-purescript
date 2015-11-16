var assert = require('assert');
var GOL = require('GameOfLife');

var stringifyWorld = function (w){
  return GOL.stringifyWorld(GOL.prettyWorld(w));
};

describe('GameOfLife', function() {
  describe('evolve', function () {
    it('an empty World stays empty', function () {
      assert.equal(GOL.emptyWorld, GOL.evolve(GOL.emptyWorld))
    });

    it('blinker', function () {
      assert.equal(
        stringifyWorld(GOL.threeInARow),
        stringifyWorld(GOL.evolve(GOL.evolve(GOL.threeInARow)))
      )
    })
  });
});
