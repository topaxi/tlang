/**
 * JavaScript Iterator Helper Functions
 * Provides runtime support for converting JavaScript arrays to tlang iterator protocol
 */

/**
 * Creates a tlang iterator from a JavaScript array
 * @param {Array<any>} jsArray - The JavaScript array to convert
 * @returns {Object} An iterator object with a next() method that returns Option::Some or Option::None
 */
function createTlangIterator(jsArray) {
    return {
        _array: jsArray,
        _index: 0,
        next: function() {
            if (this._index >= this._array.length) {
                return { tag: 1, _0: undefined }; // Option::None
            } else {
                const value = this._array[this._index++];
                return { tag: 0, _0: value }; // Option::Some(value)
            }
        }
    };
}

/**
 * Iterator module for tlang
 * Provides a unified interface for creating iterators from different types
 */
const iterator = {
    /**
     * Creates an iterator for the given value
     * @param {any} value - The value to create an iterator for
     * @returns {Object} An iterator object compatible with tlang iterator protocol
     */
    iter: function(value) {
        // If it's a JavaScript array, convert it to tlang iterator
        if (Array.isArray(value)) {
            return createTlangIterator(value);
        }
        // If it already has an iter method, call it
        if (value && typeof value.iter === 'function') {
            return value.iter();
        }
        // Otherwise, assume it's a tlang list structure and create iterator
        return createTlangIterator(value);
    }
};