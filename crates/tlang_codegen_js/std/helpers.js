/**
 * JavaScript Iterator Helper Functions
 * Provides runtime support for converting JavaScript iterables to tlang iterator protocol
 */

class TlangIteratorWrapper {
  #jsIterator;

  /**
   * Creates a tlang iterator from a JavaScript iterator
   * @param {Iterator<any>} jsIterator - The JavaScript iterator to convert
   * @returns {Object} An iterator object with a next() method that returns Option::Some or Option::None
   */
  static from(jsIterator) {
    return new TlangIteratorWrapper(jsIterator);
  }

  constructor(jsIterator) {
    this.#jsIterator = jsIterator;
  }

  next() {
    const { done, value } = this.#jsIterator.next();

    return done ? Option.None : Option.Some(value);
  }
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
      return TlangIteratorWrapper.from(value[Symbol.iterator]());
    }

    // Check if the object is already an iterator (has a next method)
    if (value != null && typeof value.next === 'function') {
      // Wrap existing iterator to ensure tlang compatibility
      return TlangIteratorWrapper.from(value);
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
