/* Copyright (c) Jython Developers */
package org.python.core;

import java.io.IOException;
import java.io.InputStream;
import java.util.Map;

import org.python.core.util.importer;
import org.python.expose.ExposedMethod;
import org.python.expose.ExposedNew;
import org.python.expose.ExposedType;
import org.python.util.Generic;

@ExposedType(name="ClasspathPyImporter")
public class ClasspathPyImporter extends importer<String> {

    public static final String PYCLASSPATH_PREFIX = "__pyclasspath__/";
    public static final PyType TYPE = PyType.fromClass(ClasspathPyImporter.class);

    public ClasspathPyImporter(PyType subType) {
        super(subType);
    }

    public ClasspathPyImporter() {
        super();
    }

    @ExposedNew
    @ExposedMethod
    final void ClasspathPyImporter___init__(PyObject[] args, String[] kwds) {
        ArgParser ap = new ArgParser("__init__", args, kwds, new String[] {"path"});
        String path = ap.getString(0);
        if (path == null || !path.startsWith(PYCLASSPATH_PREFIX)) {
            throw Py.ImportError("path isn't for classpath importer");
        }
        if (!path.endsWith("/")) {
            path += "/";
        }
        this.path = path;
    }

    /**
     * Find the module for the fully qualified name.
     *
     * @param fullname the fully qualified name of the module
     * @param path if not installed on the meta-path None or a module path
     * @return a loader instance if this importer can load the module, None
     *         otherwise
     */
    @ExposedMethod(defaults = "null")
    final PyObject ClasspathPyImporter_find_module(String fullname, String path) {
        return importer_find_module(fullname, path);
    }

    /**
     * Load a module for the fully qualified name.
     *
     * @param fullname the fully qualified name of the module
     * @return a loaded PyModule
     */
    @ExposedMethod
    final PyObject ClasspathPyImporter_load_module(String fullname) {
        return importer_load_module(fullname);
    }

    @Override
    protected long getSourceMtime(String path) {
        // Can't determine this easily
        return -1;
    }

    @Override
    protected Bundle makeBundle(String fullFilename, String entry) {
        InputStream is = entries.remove(entry);
        return new Bundle(is) {
            @Override
            public void close() {
                try {
                    inputStream.close();
                } catch (IOException e) {
                    throw Py.JavaError(e);
                }
            }
        };
    }

    @Override
    protected String makeEntry(String filename) {
        if (entries.containsKey(filename)) {
            return filename;
        }
        InputStream is;
        if (Py.getSystemState().getClassLoader() != null) {
        	is = tryClassLoader(filename, Py.getSystemState().getClassLoader(), "sys");
        } else {
        	is = tryClassLoader(filename, imp.getParentClassLoader(), "parent");
        }
        if (is != null) {
            entries.put(filename, is);
            return filename;
        }
        return null;
    }

    private InputStream tryClassLoader(String fullFilename, ClassLoader loader, String name) {
        if (loader != null) {
            Py.writeDebug("import", "trying " + fullFilename + " in " + name + " class loader");
            return loader.getResourceAsStream(fullFilename);
        }
        return null;
    }

    @Override
    protected String makeFilename(String fullname) {
        return path.replace(PYCLASSPATH_PREFIX, "") + fullname.replace('.', '/');
    }

    @Override
    protected String makeFilePath(String fullname) {
        return path + fullname.replace('.', '/');
    }

    @Override
    protected String makePackagePath(String fullname) {
        return path;
    }

    @Override
    protected String getSeparator() {
        return "/";
    }

    private Map<String, InputStream> entries = Generic.map();

    private String path;
}
