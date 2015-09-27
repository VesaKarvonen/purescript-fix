"use strict";

// module Control.Monad.MonadFix

exports.lazyLiftFix = function () {
  function Defer (thunk) {
    if (this instanceof Defer) {
      this.thunk = thunk;
      return this;
    } else {
      return new Defer(thunk);
    }
  }

  Defer.prototype.force = function () {
    var value = this.thunk();
    delete this.thunk;
    this.force = function () {
      return value;
    };
    return value;
  };

  return Defer;
}();
