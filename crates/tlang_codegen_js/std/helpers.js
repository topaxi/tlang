/**
 * JavaScript Iterator Helper Functions
 * Provides runtime support for converting JavaScript iterables to tlang iterator protocol
 */

/**
 * Creates a tlang iterator from any JavaScript iterable
 * @param {Iterable<any>} jsIterable - The JavaScript iterable to convert (arrays, strings, Sets, Maps, etc.)
 * @returns {Object} An iterator object with a next() method that returns Option::Some or Option::None
 */
function createTlangIterator(jsIterable) {
  const jsIterator = jsIterable[Symbol.iterator]();
  return {
    _jsIterator: jsIterator,
    next: function () {
      const result = this._jsIterator.next();
      if (result.done) {
        return { tag: 1, _0: undefined }; // Option::None
      } else {
        return { tag: 0, _0: result.value }; // Option::Some(value)
      }
    },
  };
}

/**
 * Iterator module for tlang
 * Provides a unified interface for creating iterators from different types
 */
const iterator = {
  /**
   * Creates an iterator for the given value
   * Handles anything JavaScript for...of can handle (arrays, strings, Sets, Maps, generators, etc.)
   * @param {any} value - The value to create an iterator for
   * @returns {Object} An iterator object compatible with tlang iterator protocol
   */
  iter: function (value) {
    // Check if the value implements the JavaScript iterator protocol (Symbol.iterator)
    // This handles arrays, strings, Sets, Maps, generators, typed arrays, NodeLists, etc.
    if (value != null && typeof value[Symbol.iterator] === 'function') {
      return createTlangIterator(value);
    }

    // Check if the object is already an iterator (has a next method)
    if (value != null && typeof value.next === 'function') {
      // Wrap existing iterator to ensure tlang compatibility
      return {
        _iterator: value,
        next: function () {
          const result = this._iterator.next();
          if (result.done) {
            return { tag: 1, _0: undefined }; // Option::None
          } else {
            return { tag: 0, _0: result.value }; // Option::Some(value)
          }
        },
      };
    }

    // Check if it has a tlang iter method and call it
    if (value != null && typeof value.iter === 'function') {
      return value.iter();
    }

    // If none of the above, throw an error as the value is not iterable
    throw new Error(`Value is not iterable: ${typeof value} ${value}`);
  },
};

// Expose iterator for testing/external use
if (typeof globalThis !== 'undefined') {
  globalThis.iterator = iterator;
} else if (typeof window !== 'undefined') {
  window.iterator = iterator;
}
