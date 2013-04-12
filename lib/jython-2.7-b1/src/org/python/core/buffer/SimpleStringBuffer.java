package org.python.core.buffer;

import org.python.core.PyBuffer;
import org.python.core.util.StringUtil;

/**
 * Buffer API that appears to be a one-dimensional array of one-byte items providing read-only API,
 * but which is actually backed by a Java String. Some of the buffer API absolutely needs access to
 * the data as a byte array (those parts that involve a {@link PyBuffer.Pointer} result), and therefore
 * this class must create a byte array from the String for them. However, it defers creation of a
 * byte array until that part of the API is actually used. Where possible, this class overrides
 * those methods in SimpleBuffer that would otherwise access the byte array attribute to use the
 * String instead.
 */
public class SimpleStringBuffer extends SimpleBuffer {

    /**
     * The string backing this PyBuffer. A substitute for {@link #buf} until we can no longer avoid
     * creating it.
     */
    private String bufString;

    /**
     * Provide an instance of SimpleStringBuffer meeting the consumer's expectations as expressed in
     * the flags argument.
     *
     * @param bufString storing the implementation of the object
     * @param flags consumer requirements
     */
    public SimpleStringBuffer(int flags, String bufString) {
        // Save the backing string
        this.bufString = bufString;
        shape[0] = bufString.length();
        // Check request is compatible with type
        checkRequestFlags(flags);
    }

    /**
     * {@inheritDoc}
     * <p>
     * This method uses {@link String#length()} rather than create an actual byte buffer.
     */
    @Override
    public int getLen() {
        // Avoid creating buf by using String.length
        return bufString.length();
    }

    /**
     * {@inheritDoc}
     * <p>
     * This method uses {@link String#charAt(int)} rather than create an actual byte buffer.
     */
    @Override
    public byte byteAt(int index) throws IndexOutOfBoundsException {
        // Avoid creating buf by using String.charAt
        return (byte)bufString.charAt(index);
    }

    /**
     * {@inheritDoc}
     * <p>
     * This method uses {@link String#charAt(int)} rather than create an actual byte buffer.
     */
    @Override
    public int intAt(int index) throws IndexOutOfBoundsException {
        // Avoid creating buf by using String.charAt
        return bufString.charAt(index);
    }

    /**
     * {@inheritDoc}
     * <p>
     * This method uses {@link String#charAt(int)} rather than create an actual byte buffer.
     */
    @Override
    public void copyTo(int srcIndex, byte[] dest, int destPos, int length)
            throws IndexOutOfBoundsException {
        // Avoid creating buf by using String.charAt
        int endIndex = srcIndex + length, p = destPos;
        for (int i = srcIndex; i < endIndex; i++) {
            dest[p++] = (byte)bufString.charAt(i);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * The <code>SimpleStringBuffer</code> implementation avoids creation of a byte buffer.
     */
    @Override
    public PyBuffer getBufferSlice(int flags, int start, int length) {
        // The new string content is just a sub-string. (Non-copy operation in Java.)
        return new SimpleStringView(getRoot(), flags, bufString.substring(start, start + length));
    }

    /**
     * {@inheritDoc}
     * <p>
     * The <code>SimpleStringBuffer</code> implementation creates an actual byte buffer.
     */
    @Override
    public PyBuffer getBufferSlice(int flags, int start, int length, int stride) {
        if (stride == 1) {
            // Unstrided slice of simple buffer is itself simple
            return getBufferSlice(flags, start, length);
        } else {
            // Force creation of the actual byte buffer be a SimpleBuffer
            getBuf();
            return super.getBufferSlice(flags, start, length, stride);
        }
    }

    /**
     * This method creates an actual byte array from the underlying String if none yet exists.
     */
    private void ensureHaveBytes() {
        if (storage == null) {
            // We can't avoid creating the byte array any longer (index0 already correct)
            storage = StringUtil.toBytes(bufString);
        }
    }

    /**
     * {@inheritDoc}
     * <p>
     * This method creates an actual byte array from the underlying String if none yet exists.
     */
    @Override
    public Pointer getBuf() {
        ensureHaveBytes();
        return super.getBuf();
    }

    /**
     * {@inheritDoc}
     * <p>
     * This method creates an actual byte array from the underlying String if none yet exists.
     */
    @Override
    public Pointer getPointer(int index) {
        ensureHaveBytes();
        return super.getPointer(index);
    }

    /**
     * {@inheritDoc}
     * <p>
     * This method creates an actual byte array from the underlying String if none yet exists.
     */
    @Override
    public Pointer getPointer(int... indices) {
        ensureHaveBytes();
        return super.getPointer(indices);
    }

    /**
     * The <code>toString()</code> method of a <code>SimpleStringBuffer</code> simply produces the
     * underlying <code>String</code>.
     */
    @Override
    public String toString() {
        return bufString;
    }

    /**
     * A <code>SimpleStringBuffer.SimpleStringView</code> represents a contiguous subsequence of
     * another <code>SimpleStringBuffer</code>.
     */
    static class SimpleStringView extends SimpleStringBuffer {

        /** The buffer on which this is a slice view */
        PyBuffer root;

        /**
         * Construct a slice of a SimpleStringBuffer.
         *
         * @param root buffer which will be acquired and must be released ultimately
         * @param flags the request flags of the consumer that requested the slice
         * @param buf becomes the buffer of bytes for this object
         */
        public SimpleStringView(PyBuffer root, int flags, String bufString) {
            // Create a new SimpleStringBuffer on the string passed in
            super(flags, bufString);
            // Get a lease on the root PyBuffer
            this.root = root.getBuffer(FULL_RO);
        }

        @Override
        protected PyBuffer getRoot() {
            return root;
        }

        @Override
        public void releaseAction() {
            // We have to release the root too if ours was final.
            root.release();
        }

    }
}
