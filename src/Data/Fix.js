"use strict";

// module Data.Fix

exports.lazyProxy = function () {
  function LazyProxy () {
    if (this instanceof LazyProxy) {
      this.thunk = undefined;
      return this;
    } else {
      return new LazyProxy();
    }
  }

  LazyProxy.prototype.force = function () {
    var value = this.thunk();
    delete this.thunk;
    this.force = function () {
      return value;
    };
    return value;
  };

  function Tie (l) {
    var self = this;
    return function () {
      self.value.thunk = l.thunk;
    };
  }

  return {'value': LazyProxy(), 'tie': Tie};
};
