// Copyright (c) Corporation for National Research Initiatives
package org.python.core;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.concurrent.locks.ReentrantLock;

import org.python.compiler.Module;
import org.python.core.util.FileUtil;
import org.python.core.util.PlatformUtil;

/**
 * Utility functions for "import" support.
 *
 * Note that this class tries to match the names of the corresponding functions
 * from CPython's Python/import.c. In these cases we use CPython's function
 * naming style (underscores and all lowercase) instead of Java's typical
 * camelCase style so that it's easier to compare with import.c.
 */
public class imp {
    private static final String IMPORT_LOG = "import";

    private static final String UNKNOWN_SOURCEFILE = "<unknown>";

    private static final int APIVersion = 33;

    public static final int NO_MTIME = -1;

    // This should change to 0 for Python 2.7 and 3.0 see PEP 328
    public static final int DEFAULT_LEVEL = -1;

    /** A non-empty fromlist for __import__'ing sub-modules. */
    private static final PyObject nonEmptyFromlist = new PyTuple(Py.newString("__doc__"));

    /** Synchronizes import operations */
    public static final ReentrantLock importLock = new ReentrantLock();

    private static Object syspathJavaLoaderLock = new Object();

    private static ClassLoader syspathJavaLoader = null;

    public static ClassLoader getSyspathJavaLoader() {
        synchronized (syspathJavaLoaderLock) {
            if (syspathJavaLoader == null) {
        		syspathJavaLoader = new SyspathJavaLoader(getParentClassLoader());
	        }            	
        }
        return syspathJavaLoader;
    }
    
    /**
     * Selects the parent class loader for Jython, to be used for
     * dynamically loaded classes and resources.  Chooses between the
     * current and context classloader based on the following
     * criteria:
     *
     * <ul>
     * <li>If both are the same classloader, return that classloader.
     * <li>If either is null, then the non-null one is selected.
     * <li>If both are not null, and a parent/child relationship can
     * be determined, then the child is selected.
     * <li>If both are not null and not on a parent/child
     * relationship, then the current class loader is returned (since
     * it is likely for the context class loader to <b>not</b> see the
     * Jython classes)
     * </ul>
     * 
     * @return the parent class loader for Jython or null if both the
     * current and context classloaders are null.
     */
    public static ClassLoader getParentClassLoader() {
    	ClassLoader current = imp.class.getClassLoader();
    	ClassLoader context = Thread.currentThread().getContextClassLoader();
    	if (context == current) {
    		return current;
    	}
    	if (context == null) {
    		return current;
    	}
    	if (current == null) {
    		return context;
    	}
    	if (isParentClassLoader(context, current)) {
    		return current;
    	}
    	if (isParentClassLoader(current, context)) {
    		return context;
    	}
    	return current;
    }    

    private static boolean isParentClassLoader(
    		ClassLoader suspectedParent, ClassLoader child) {
    	try { 
	    	ClassLoader parent = child.getParent();
	    	if (suspectedParent == parent) {
	    		return true;
	    	}
	    	if (parent == null || parent == child) {
	    		// We reached the boot class loader
	    		return false;
	    	}
	    	return isParentClassLoader(suspectedParent, parent);
	    	
    	} catch (SecurityException e) {
    		return false;
    	}
	}

	private imp() {
    }

    /**
     * If the given name is found in sys.modules, the entry from there is
     * returned. Otherwise a new PyModule is created for the name and added to
     * sys.modules
     */
    public static PyModule addModule(String name) {
        name = name.intern();
        PyObject modules = Py.getSystemState().modules;
        PyModule module = (PyModule) modules.__finditem__(name);
        if (module != null) {
            return module;
        }
        module = new PyModule(name, null);
        modules.__setitem__(name, module);
        return module;
    }

    /**
     * Remove name form sys.modules if it's there.
     *
     * @param name the module name
     */
    private static void removeModule(String name) {
        name = name.intern();
        PyObject modules = Py.getSystemState().modules;
        if (modules.__finditem__(name) != null) {
            try {
                modules.__delitem__(name);
            } catch (PyException pye) {
                // another thread may have deleted it
                if (!pye.match(Py.KeyError)) {
                    throw pye;
                }
            }
        }
    }

    private static byte[] readBytes(InputStream fp) {
        try {
            return FileUtil.readBytes(fp);
        } catch(IOException ioe) {
            throw Py.IOError(ioe);
        } finally {
            try {
                fp.close();
            } catch(IOException e) {
                throw Py.IOError(e);
            }
        }
    }

    private static InputStream makeStream(File file) {
        try {
            return new FileInputStream(file);
        } catch (IOException ioe) {
            throw Py.IOError(ioe);
        }
    }
    static PyObject createFromPyClass(String name, InputStream fp, boolean testing,
                                      String sourceName, String compiledName) {
        return createFromPyClass(name, fp, testing, sourceName, compiledName, NO_MTIME);

    }

    static PyObject createFromPyClass(String name, InputStream fp, boolean testing,
                                      String sourceName, String compiledName, long mtime) {
        byte[] data = null;
        try {
            data = readCode(name, fp, testing, mtime);
        } catch (IOException ioe) {
            if (!testing) {
                throw Py.ImportError(ioe.getMessage() + "[name=" + name + ", source=" + sourceName
                        + ", compiled=" + compiledName + "]");
            }
        }
        if (testing && data == null) {
            return null;
        }
        PyCode code;
        try {
            code = BytecodeLoader.makeCode(name + "$py", data, sourceName);
        } catch (Throwable t) {
            if (testing) {
                return null;
            } else {
                throw Py.JavaError(t);
            }
        }

        Py.writeComment(IMPORT_LOG, String.format("import %s # precompiled from %s", name,
                                                  compiledName));

        return createFromCode(name, code, compiledName);
    }

    public static byte[] readCode(String name, InputStream fp, boolean testing) throws IOException {
        return readCode(name, fp, testing, NO_MTIME);
    }

    public static byte[] readCode(String name, InputStream fp, boolean testing, long mtime) throws IOException {
        byte[] data = readBytes(fp);
        int api;
        AnnotationReader ar = new AnnotationReader(data);
        api = ar.getVersion();
        if (api != APIVersion) {
            if (testing) {
                return null;
            } else {
                throw Py.ImportError("invalid api version(" + api + " != "
                        + APIVersion + ") in: " + name);
            }
        }
        if (testing && mtime != NO_MTIME) {
            long time = ar.getMTime();
            if (mtime != time) {
                return null;
            }
        }
        return data;
    }

    public static byte[] compileSource(String name, File file) {
        return compileSource(name, file, null);
    }

    public static byte[] compileSource(String name, File file, String sourceFilename) {
        return compileSource(name, file, sourceFilename, null);
    }

    public static byte[] compileSource(String name, File file, String sourceFilename,
            String compiledFilename) {
        if (sourceFilename == null) {
            sourceFilename = file.toString();
        }
        long mtime = file.lastModified();
        return compileSource(name, makeStream(file), sourceFilename, mtime);
    }

    public static String makeCompiledFilename(String filename) {
        return filename.substring(0, filename.length() - 3) + "$py.class";
    }

    /**
     * Stores the bytes in compiledSource in compiledFilename.
     *
     * If compiledFilename is null, it's set to the results of
     * makeCompiledFilename(sourcefileName).
     *
     * If sourceFilename is null or set to UNKNOWN_SOURCEFILE, then
     * null is returned.
     *
     * @return the compiledFilename eventually used; or null if a
     *         compiledFilename couldn't be determined or if an error
     *         was thrown while writing to the cache file.
     */
    public static String cacheCompiledSource(String sourceFilename,
                                              String compiledFilename,
                                              byte[] compiledSource) {
        if(compiledFilename == null){
            if(sourceFilename == null || sourceFilename.equals(UNKNOWN_SOURCEFILE)){
               return null;
            }
            compiledFilename = makeCompiledFilename(sourceFilename);
        }
        FileOutputStream fop = null;
        try {
            SecurityManager man = System.getSecurityManager();
            if (man != null) {
                man.checkWrite(compiledFilename);
            }
            fop = new FileOutputStream(compiledFilename);
            fop.write(compiledSource);
            fop.close();
            return compiledFilename;
        } catch(IOException exc) {
            // If we can't write the cache file, just log and continue
            Py.writeDebug(IMPORT_LOG, "Unable to write to source cache file '"
                    + compiledFilename + "' due to " + exc);
            return null;
        } catch(SecurityException exc) {
            // If we can't write the cache file, just log and continue
            Py.writeDebug(IMPORT_LOG, "Unable to write to source cache file '"
                    + compiledFilename + "' due to " + exc);
            return null;
        } finally {
            if(fop != null) {
                try {
                    fop.close();
                } catch(IOException e) {
                    Py.writeDebug(IMPORT_LOG,
                                  "Unable to close source cache file '"
                                          + compiledFilename + "' due to " + e);
                }
            }
        }
    }

    public static byte[] compileSource(String name, InputStream fp, String filename) {
        return compileSource(name, fp, filename, NO_MTIME);
    }

    public static byte[] compileSource(String name, InputStream fp, String filename, long mtime) {
        ByteArrayOutputStream ofp = new ByteArrayOutputStream();
        try {
            if(filename == null) {
                filename = UNKNOWN_SOURCEFILE;
            }
            org.python.antlr.base.mod node;
            try {
                node = ParserFacade.parse(fp, CompileMode.exec, filename, new CompilerFlags());
            } finally {
                fp.close();
            }
            Module.compile(node, ofp, name + "$py", filename, true, false, null, mtime);
            return ofp.toByteArray();
        } catch(Throwable t) {
            throw ParserFacade.fixParseError(null, t, filename);
        }
    }

    public static PyObject createFromSource(String name, InputStream fp, String filename) {
        return createFromSource(name, fp, filename, null, NO_MTIME);
    }

    public static PyObject createFromSource(String name, InputStream fp,
            String filename, String outFilename) {
        return createFromSource(name, fp, filename, outFilename, NO_MTIME);
    }

    public static PyObject createFromSource(String name, InputStream fp,
            String filename, String outFilename, long mtime) {
        byte[] bytes = compileSource(name, fp, filename, mtime);
        if (!Py.getSystemState().dont_write_bytecode) {
            outFilename = cacheCompiledSource(filename, outFilename, bytes);
        }

        Py.writeComment(IMPORT_LOG, "'" + name + "' as " + filename);

        PyCode code = BytecodeLoader.makeCode(name + "$py", bytes, filename);
        return createFromCode(name, code, filename);
    }

    /**
     * Returns a module with the given name whose contents are the results of
     * running c. __file__ is set to whatever is in c.
     */
    public static PyObject createFromCode(String name, PyCode c){
        return createFromCode(name, c, null);
    }

    /**
     * Returns a module with the given name whose contents are the results of
     * running c. Sets __file__ on the module to be moduleLocation unless
     * moduleLocation is null. If c comes from a local .py file or compiled
     * $py.class class moduleLocation should be the result of running new
     * File(moduleLocation).getAbsoultePath(). If c comes from a remote file or
     * is a jar moduleLocation should be the full uri for c.
     */
    public static PyObject createFromCode(String name, PyCode c, String moduleLocation) {
        PyModule module = addModule(name);

        PyTableCode code = null;
        if (c instanceof PyTableCode) {
            code = (PyTableCode) c;
        }

        if (moduleLocation != null) {
            module.__setattr__("__file__", new PyString(moduleLocation));
        } else if (module.__findattr__("__file__") == null) {
            // Should probably never happen (but maybe with an odd custom builtins, or
            // Java Integration)
            Py.writeDebug(IMPORT_LOG, String.format("Warning: %s __file__ is unknown", name));
        }

        try {
            PyFrame f = new PyFrame(code, module.__dict__, module.__dict__, null);
            code.call(Py.getThreadState(), f);
        } catch (RuntimeException t) {
            removeModule(name);
            throw t;
        }
        return module;
    }

    static PyObject createFromClass(String name, Class<?> c) {
        // Two choices. c implements PyRunnable or c is Java package
        if (PyRunnable.class.isAssignableFrom(c)) {
            try {
                return createFromCode(name, ((PyRunnable)c.newInstance()).getMain());
            } catch (InstantiationException e) {
                throw Py.JavaError(e);
            } catch (IllegalAccessException e) {
                throw Py.JavaError(e);
            }
        }
        return PyType.fromClass(c, false); // xxx?
    }

    static PyObject getPathImporter(PyObject cache, PyList hooks, PyObject p) {

        // attempt to get an importer for the path
        // use null as default value since Py.None is
        // a valid value in the cache for the default
        // importer
        PyObject importer = cache.__finditem__(p);
        if (importer != null) {
            return importer;
        }

        // nothing in the cache, so check all hooks
        PyObject iter = hooks.__iter__();
        for (PyObject hook; (hook = iter.__iternext__()) != null;) {
            try {
                importer = hook.__call__(p);
                break;
            } catch (PyException e) {
                if (!e.match(Py.ImportError)) {
                    throw e;
                }
            }
        }

        importer = (importer == null ? Py.None : importer);
        cache.__setitem__(p, importer);

        return importer;
    }

    static PyObject find_module(String name, String moduleName, PyList path) {
        PyObject loader = Py.None;
        PySystemState sys = Py.getSystemState();
        PyObject metaPath = sys.meta_path;

        for (PyObject importer : metaPath.asIterable()) {
            PyObject findModule = importer.__getattr__("find_module");
            loader = findModule.__call__(new PyObject[] {
                    new PyString(moduleName), path == null ? Py.None : path });
            if (loader != Py.None) {
                return loadFromLoader(loader, moduleName);
            }
        }

        PyObject ret = loadBuiltin(moduleName);
        if (ret != null) {
            return ret;
        }

        path = path == null ? sys.path : path;
        for (int i = 0; i < path.__len__(); i++) {
            PyObject p = path.__getitem__(i);
            PyObject importer = getPathImporter(sys.path_importer_cache,
                    sys.path_hooks, p);
            if (importer != Py.None) {
                PyObject findModule = importer.__getattr__("find_module");
                loader = findModule.__call__(new PyObject[] { new PyString(
                        moduleName) });
                if (loader != Py.None) {
                    return loadFromLoader(loader, moduleName);
                }
            }
            if (!(p instanceof PyUnicode)) {
                p = p.__str__();
            }
            ret = loadFromSource(sys, name, moduleName, p.toString());
            if (ret != null) {
                return ret;
            }
        }

        return ret;
    }

    private static PyObject loadBuiltin(String name) {
        if (name == "sys") {
            Py.writeComment(IMPORT_LOG, "'" + name + "' as sys in builtin modules");
            return Py.java2py(Py.getSystemState());
        }
        if (name == "__builtin__") {
            Py.writeComment(IMPORT_LOG, "'" + name + "' as __builtin__ in builtin modules");
            return new PyModule("__builtin__", PySystemState.builtins);
        }
        String mod = PySystemState.getBuiltin(name);
        if (mod != null) {
            Class c = Py.findClassEx(mod, "builtin modules");
            if (c != null) {
                Py.writeComment(IMPORT_LOG, "'" + name + "' as " + mod
                        + " in builtin modules");
                try {
                    if (PyObject.class.isAssignableFrom(c)) { // xxx ok?
                        return PyType.fromClass(c);
                    }
                    return createFromClass(name, c);
                } catch (NoClassDefFoundError e) {
                    throw Py.ImportError("Cannot import " + name
                            + ", missing class " + c.getName());
                }
            }
        }
        return null;
    }

    static PyObject loadFromLoader(PyObject importer, String name) {
        PyObject load_module = importer.__getattr__("load_module");
        return load_module.__call__(new PyObject[] { new PyString(name) });
    }

    public static PyObject loadFromCompiled(String name, InputStream stream, String sourceName,
                                            String compiledName) {
        return createFromPyClass(name, stream, false, sourceName, compiledName);
    }

    static PyObject loadFromSource(PySystemState sys, String name, String modName, String entry) {
        String dirName = sys.getPath(entry);
        String sourceName = "__init__.py";
        String compiledName = "__init__$py.class";
        // display names are for identification purposes (e.g. __file__): when entry is
        // null it forces java.io.File to be a relative path (e.g. foo/bar.py instead of
        // /tmp/foo/bar.py)
        String displayDirName = entry.equals("") ? null : entry.toString();
        String displaySourceName = new File(new File(displayDirName, name), sourceName).getPath();
        String displayCompiledName = new File(new File(displayDirName, name),
                                              compiledName).getPath();

        // First check for packages
        File dir = new File(dirName, name);
        File sourceFile = new File(dir, sourceName);
        File compiledFile = new File(dir, compiledName);

        boolean pkg = false;
        try {
            pkg = dir.isDirectory() && caseok(dir, name)
                    && (sourceFile.isFile() || compiledFile.isFile());
        } catch (SecurityException e) {
            // ok
        }

        if (!pkg) {
            Py.writeDebug(IMPORT_LOG, "trying source " + dir.getPath());
            sourceName = name + ".py";
            compiledName = name + "$py.class";
            displaySourceName = new File(displayDirName, sourceName).getPath();
            displayCompiledName = new File(displayDirName, compiledName).getPath();
            sourceFile = new File(dirName, sourceName);
            compiledFile = new File(dirName, compiledName);
        } else {
            PyModule m = addModule(modName);
            PyObject filename = new PyString(new File(displayDirName, name).getPath());
            m.__dict__.__setitem__("__path__", new PyList(new PyObject[] {filename}));
        }

        try {
            if (sourceFile.isFile() && caseok(sourceFile, sourceName)) {
                long pyTime = sourceFile.lastModified();
                if (compiledFile.isFile() && caseok(compiledFile, compiledName)) {
                    Py.writeDebug(IMPORT_LOG, "trying precompiled " + compiledFile.getPath());
                    long classTime = compiledFile.lastModified();
                    if (classTime >= pyTime) {
                        PyObject ret = createFromPyClass(modName, makeStream(compiledFile), true,
                                                         displaySourceName, displayCompiledName, pyTime);
                        if (ret != null) {
                            return ret;
                        }
                    }
                    return createFromSource(modName, makeStream(sourceFile), displaySourceName,
                                            compiledFile.getPath(), pyTime);
                }
                return createFromSource(modName, makeStream(sourceFile), displaySourceName,
                                        compiledFile.getPath(), pyTime);
            }

            // If no source, try loading precompiled
            Py.writeDebug(IMPORT_LOG, "trying precompiled with no source " + compiledFile.getPath());
            if (compiledFile.isFile() && caseok(compiledFile, compiledName)) {
                return createFromPyClass(modName, makeStream(compiledFile), true, displaySourceName,
                                         displayCompiledName);
            }
        } catch (SecurityException e) {
            // ok
        }
        return null;
    }

    public static boolean caseok(File file, String filename) {
        if (Options.caseok || !PlatformUtil.isCaseInsensitive()) {
            return true;
        }
        try {
            File canFile = new File(file.getCanonicalPath());
            boolean match = filename.regionMatches(0, canFile.getName(), 0, filename.length());
            if (!match) {
                //possibly a symlink.  Get parent and look for exact match in listdir()
                //This is what CPython does in the case of Mac OS X and Cygwin.
                //XXX: This will be a performance hit, maybe jdk7 nio2 can give us a better
                //     method?
                File parent = file.getParentFile();
                String[] children = parent.list();
                for (String c: children) {
                    if (c.equals(filename)) {
                        return true;
                    }
                }
            }
            return match;
        } catch (IOException exc) {
            return false;
        }
    }

    /**
     * Load the module by name. Upon loading the module it will be added to
     * sys.modules.
     *
     * @param name the name of the module to load
     * @return the loaded module
     */
    public static PyObject load(String name) {
        return import_first(name, new StringBuilder());
    }

	/**
	 * Find the parent package name for a module.
	 * 
	 * If __name__ does not exist in the module or if level is <code>0</code>,
	 * then the parent is <code>null</code>. If __name__ does exist and is not a
	 * package name, the containing package is located. If no such package
	 * exists and level is <code>-1</code>, the parent is <code>null</code>. If
	 * level is <code>-1</code>, the parent is the current name. Otherwise,
	 * <code>level-1</code> doted parts are stripped from the current name. For
	 * example, the __name__ <code>"a.b.c"</code> and level <code>2</code> would
	 * return <code>"a.b"</code>, if <code>c</code> is a package and would
	 * return <code>"a"</code>, if <code>c</code> is not a package.
	 * 
	 * @param dict
	 *            the __dict__ of a loaded module
	 * @param level
	 *            used for relative and absolute imports. -1 means try both, 0
	 *            means absolute only, positive ints represent the level to look
	 *            upward for a relative path (1 means current package, 2 means
	 *            one level up). See PEP 328 at
	 *            http://www.python.org/dev/peps/pep-0328/
	 * 
	 * @return the parent name for a module
	 */
    private static String get_parent(PyObject dict, int level) {
        String modname;
        if (dict == null || level == 0) {
        	// try an absolute import
            return null;
        }

        PyObject tmp = dict.__finditem__("__package__");
        if (tmp != null && tmp != Py.None) {
            if (!Py.isInstance(tmp, PyString.TYPE)) {
                throw Py.ValueError("__package__ set to non-string");
            }
            modname = ((PyString)tmp).getString();
        } else {
            //__package__ not set, so figure it out and set it.

            tmp = dict.__finditem__("__name__");
            if (tmp == null) {
                return null;
            }
            modname = tmp.toString();

            // locate the current package
            tmp = dict.__finditem__("__path__");
            if (tmp instanceof PyList) {
                //__path__ is set, so modname is already the package name.
                dict.__setitem__("__package__", new PyString(modname));
            } else {
                // __name__ is not a package name, try one level upwards.
                int dot = modname.lastIndexOf('.');
                if (dot == -1) {
                    if (level <= -1) {
                        // there is no package, perform an absolute search
                        dict.__setitem__("__package__", Py.None);
                        return null;
                    }
                    throw Py.ValueError("Attempted relative import in non-package");
                }
                // modname should be the package name.
                modname = modname.substring(0, dot);
                dict.__setitem__("__package__", new PyString(modname));
            }
        }

        // walk upwards if required (level >= 2)
        while (level-- > 1) {
            int dot = modname.lastIndexOf('.');
            if (dot == -1) {
                throw Py.ValueError("Attempted relative import beyond toplevel package");
            }
            modname = modname.substring(0, dot);
        }
        return modname.intern();
    }

    /**
     *
     * @param mod a previously loaded module
     * @param parentNameBuffer
     * @param name the name of the module to load
     * @return null or None
     */
    private static PyObject import_next(PyObject mod,
            StringBuilder parentNameBuffer, String name, String outerFullName, PyObject fromlist) {
        if (parentNameBuffer.length() > 0 && name != null && name.length() > 0) {
            parentNameBuffer.append('.');
        }
        parentNameBuffer.append(name);

        String fullName = parentNameBuffer.toString().intern();

        PyObject modules = Py.getSystemState().modules;
        PyObject ret = modules.__finditem__(fullName);
        if (ret != null) {
            return ret;
        }
        if (mod == null) {
            ret = find_module(fullName.intern(), name, null);
        } else {
            ret = mod.impAttr(name.intern());
        }
        if (ret == null || ret == Py.None) {
            if (JavaImportHelper.tryAddPackage(outerFullName, fromlist)) {
                ret = modules.__finditem__(fullName);
            }
            return ret;
        }
        if (modules.__finditem__(fullName) == null) {
            modules.__setitem__(fullName, ret);
        } else {
            ret = modules.__finditem__(fullName);
        }
        return ret;
    }

    // never returns null or None
    private static PyObject import_first(String name,
            StringBuilder parentNameBuffer) {
        PyObject ret = import_next(null, parentNameBuffer, name, null, null);
        if (ret == null || ret == Py.None) {
            throw Py.ImportError("No module named " + name);
        }
        return ret;
    }


    private static PyObject import_first(String name, StringBuilder parentNameBuffer, String fullName, PyObject fromlist) {
        PyObject ret = import_next(null, parentNameBuffer, name, fullName, fromlist);
        if (ret == null || ret == Py.None) {
            if (JavaImportHelper.tryAddPackage(fullName, fromlist)) {
                ret = import_next(null, parentNameBuffer, name, fullName, fromlist);
            }
        }
        if (ret == null || ret == Py.None) {
            throw Py.ImportError("No module named " + name);
        }
        return ret;
    }


    // Hierarchy-recursively search for dotted name in mod;
    // never returns null or None
    // ??pending: check if result is really a module/jpkg/jclass?
    private static PyObject import_logic(PyObject mod,
            StringBuilder parentNameBuffer, String dottedName, String fullName, PyObject fromlist) {
        int dot = 0;
        int last_dot = 0;

        do {
            String name;
            dot = dottedName.indexOf('.', last_dot);
            if (dot == -1) {
                name = dottedName.substring(last_dot);
            } else {
                name = dottedName.substring(last_dot, dot);
            }
            PyJavaPackage jpkg = null;
            if (mod instanceof PyJavaPackage) {
                jpkg = (PyJavaPackage)mod;
            }

            mod = import_next(mod, parentNameBuffer, name, fullName, fromlist);
            if (jpkg != null && (mod == null || mod == Py.None)) {
                // try again -- under certain circumstances a PyJavaPackage may
                // have been added as a side effect of the last import_next
                // attempt.  see Lib/test_classpathimport.py#test_bug1126
                mod = import_next(jpkg, parentNameBuffer, name, fullName, fromlist);
            }
            if (mod == null || mod == Py.None) {
                throw Py.ImportError("No module named " + name);
            }
            last_dot = dot + 1;
        } while (dot != -1);

        return mod;
    }

    /**
     * @param name
     * @param top
     * @param modDict
     * @return a module
     */
    private static PyObject import_module_level(String name, boolean top,
            PyObject modDict, PyObject fromlist, int level) {
        if (name.length() == 0 && level <= 0) {
            throw Py.ValueError("Empty module name");
        }
        PyObject modules = Py.getSystemState().modules;
        PyObject pkgMod = null;
        String pkgName = null;
        if (modDict != null && modDict.isMappingType()) {
            pkgName = get_parent(modDict, level);
            pkgMod = modules.__finditem__(pkgName);
            if (pkgMod != null && !(pkgMod instanceof PyModule)) {
                pkgMod = null;
            }
        }
        int dot = name.indexOf('.');
        String firstName;
        if (dot == -1) {
            firstName = name;
        } else {
            firstName = name.substring(0, dot);
        }
        StringBuilder parentNameBuffer = new StringBuilder(pkgMod != null ? pkgName : "");
        PyObject topMod = import_next(pkgMod, parentNameBuffer, firstName, name, fromlist);
        if (topMod == Py.None || topMod == null) {
            // Add None to sys.modules for submodule or subpackage names that aren't found, but
            // leave top-level entries out.  This allows them to be tried again if another
            // import attempt is made after they've been added to sys.path.
            if (topMod == null && pkgMod != null) {
                modules.__setitem__(parentNameBuffer.toString().intern(), Py.None);
            }
            parentNameBuffer = new StringBuilder("");
            // could throw ImportError
            topMod = import_first(firstName, parentNameBuffer, name, fromlist);
        }
        PyObject mod = topMod;
        if (dot != -1) {
            // could throw ImportError
            mod = import_logic(topMod, parentNameBuffer, name
                    .substring(dot + 1), name, fromlist);
        }
        if (top) {
            return topMod;
        }

        if (fromlist != null && fromlist != Py.None) {
            ensureFromList(mod, fromlist, name);
        }
        return mod;
    }

    private static void ensureFromList(PyObject mod, PyObject fromlist, String name) {
        ensureFromList(mod, fromlist, name, false);
    }

    private static void ensureFromList(PyObject mod, PyObject fromlist, String name,
                                       boolean recursive) {
            if (mod.__findattr__("__path__") == null) {
                return;
            }

            //This can happen with imports like "from . import foo"
            if (name.length() == 0) {
                name = mod.__findattr__("__name__").toString();
            }

            StringBuilder modNameBuffer = new StringBuilder(name);
            for (PyObject item : fromlist.asIterable()) {
                if (!Py.isInstance(item, PyBaseString.TYPE)) {
                    throw Py.TypeError("Item in ``from list'' not a string");
                }
                if (item.toString().equals("*")) {
                    if (recursive) {
                        // Avoid endless recursion
                        continue;
                    }
                    PyObject all;
                    if ((all = mod.__findattr__("__all__")) != null) {
                        ensureFromList(mod, all, name, true);
                    }
                }

                if (mod.__findattr__((PyString) item) == null) {
                    String fullName = modNameBuffer.toString() + "." + item.toString();
                    import_next(mod, modNameBuffer, item.toString(), fullName, null);
                }
            }
    }

    /**
     * Import a module by name.
     *
     * @param name the name of the package to import
     * @param top if true, return the top module in the name, otherwise the last
     * @return an imported module (Java or Python)
     */
    public static PyObject importName(String name, boolean top) {
        return import_module_level(name, top, null, null, DEFAULT_LEVEL);
    }

    /**
     * Import a module by name. This is the default call for
     * __builtin__.__import__.
     *
     * @param name the name of the package to import
     * @param top if true, return the top module in the name, otherwise the last
     * @param modDict the __dict__ of an already imported module
     * @return an imported module (Java or Python)
     */
    public static PyObject importName(String name, boolean top,
            PyObject modDict, PyObject fromlist, int level) {
        importLock.lock();
        try {
            return import_module_level(name, top, modDict, fromlist, level);
        } finally {
            importLock.unlock();
        }
    }

    /**
     * Called from jython generated code when a statement like "import spam" is
     * executed.
     */
    @Deprecated
    public static PyObject importOne(String mod, PyFrame frame) {
    	return importOne(mod, frame, imp.DEFAULT_LEVEL);
    }
    /**
     * Called from jython generated code when a statement like "import spam" is
     * executed.
     */
    public static PyObject importOne(String mod, PyFrame frame, int level) {
        PyObject module = __builtin__.__import__(mod, frame.f_globals, frame
                .getLocals(), Py.None, level);
        return module;
    }

    /**
     * Called from jython generated code when a statement like "import spam as
     * foo" is executed.
     */
    @Deprecated
    public static PyObject importOneAs(String mod, PyFrame frame) {
    	return importOneAs(mod, frame, imp.DEFAULT_LEVEL);
    }
    /**
     * Called from jython generated code when a statement like "import spam as
     * foo" is executed.
     */
    public static PyObject importOneAs(String mod, PyFrame frame, int level) {
        PyObject module = __builtin__.__import__(mod, frame.f_globals, frame
                .getLocals(), Py.None, level);
        int dot = mod.indexOf('.');
        while (dot != -1) {
            int dot2 = mod.indexOf('.', dot + 1);
            String name;
            if (dot2 == -1) {
                name = mod.substring(dot + 1);
            } else {
                name = mod.substring(dot + 1, dot2);
            }
            module = module.__getattr__(name);
            dot = dot2;
        }
        return module;
    }

    /**
     * replaced by importFrom with level param.  Kept for backwards compatibility.
     * @deprecated use importFrom with level param.
     */
    @Deprecated
    public static PyObject[] importFrom(String mod, String[] names,
            PyFrame frame) {
        return importFromAs(mod, names, null, frame, DEFAULT_LEVEL);
    }

    /**
     * Called from jython generated code when a statement like "from spam.eggs
     * import foo, bar" is executed.
     */
    public static PyObject[] importFrom(String mod, String[] names,
            PyFrame frame, int level) {
        return importFromAs(mod, names, null, frame, level);
    }

    /**
     * replaced by importFromAs with level param.  Kept for backwards compatibility.
     * @deprecated use importFromAs with level param.
     */
    @Deprecated
    public static PyObject[] importFromAs(String mod, String[] names,
            PyFrame frame) {
        return importFromAs(mod, names, null, frame, DEFAULT_LEVEL);
    }

    /**
     * Called from jython generated code when a statement like "from spam.eggs
     * import foo as spam" is executed.
     */
    public static PyObject[] importFromAs(String mod, String[] names,
            String[] asnames, PyFrame frame, int level) {
        PyObject[] pyNames = new PyObject[names.length];
        for (int i = 0; i < names.length; i++) {
            pyNames[i] = Py.newString(names[i]);
        }

        PyObject module = __builtin__.__import__(mod, frame.f_globals, frame.getLocals(),
                                                 new PyTuple(pyNames), level);
        PyObject[] submods = new PyObject[names.length];
        for (int i = 0; i < names.length; i++) {
            PyObject submod = module.__findattr__(names[i]);
            //XXX: Temporary fix for http://bugs.jython.org/issue1900
            if (submod == null) {
                submod = module.impAttr(names[i]);
            }
            //end temporary fix.

            if (submod == null) {
                throw Py.ImportError("cannot import name " + names[i]);
            }
            submods[i] = submod;
        }
        return submods;
    }

    private final static PyTuple all = new PyTuple(Py.newString('*'));

    /**
     * Called from jython generated code when a statement like "from spam.eggs
     * import *" is executed.
     */
    public static void importAll(String mod, PyFrame frame, int level) {
        PyObject module = __builtin__.__import__(mod, frame.f_globals, frame
                .getLocals(), all, level);
        importAll(module, frame);
    }
    @Deprecated
    public static void importAll(String mod, PyFrame frame) {
        importAll(mod, frame, DEFAULT_LEVEL);
    }

    
    public static void importAll(PyObject module, PyFrame frame) {
        PyObject names;
        boolean filter = true;
        if (module instanceof PyJavaPackage) {
            names = ((PyJavaPackage) module).fillDir();
        } else {
            PyObject __all__ = module.__findattr__("__all__");
            if (__all__ != null) {
                names = __all__;
                filter = false;
            } else {
                names = module.__dir__();
            }
        }

        loadNames(names, module, frame.getLocals(), filter);
    }




    /**
     * From a module, load the attributes found in <code>names</code> into
     * locals.
     *
     * @param filter if true, if the name starts with an underscore '_' do not
     *            add it to locals
     * @param locals the namespace into which names will be loaded
     * @param names the names to load from the module
     * @param module the fully imported module
     */
    private static void loadNames(PyObject names, PyObject module,
            PyObject locals, boolean filter) {
        for (PyObject name : names.asIterable()) {
            String sname = ((PyString) name).internedString();
            if (filter && sname.startsWith("_")) {
                continue;
            } else {
                try {
                    PyObject value = module.__findattr__(sname);
                    if (value == null) {
                        PyObject nameObj = module.__findattr__("__name__");
                        if (nameObj != null) {
                            String submodName = nameObj.__str__().toString() + '.' + sname;
                            value = __builtin__.__import__(submodName, null, null,
                                                           nonEmptyFromlist);
                        }
                    }
                    locals.__setitem__(sname, value);
                } catch (Exception exc) {
                    continue;
                }
            }
        }
    }

    static PyObject reload(PyModule m) {
        String name = m.__getattr__("__name__").toString().intern();

        PyObject modules = Py.getSystemState().modules;
        PyModule nm = (PyModule) modules.__finditem__(name);

        if (nm == null || !nm.__getattr__("__name__").toString().equals(name)) {
            throw Py.ImportError("reload(): module " + name
                    + " not in sys.modules");
        }

        PyList path = Py.getSystemState().path;
        String modName = name;
        int dot = name.lastIndexOf('.');
        if (dot != -1) {
            String iname = name.substring(0, dot).intern();
            PyObject pkg = modules.__finditem__(iname);
            if (pkg == null) {
                throw Py.ImportError("reload(): parent not in sys.modules");
            }
            path = (PyList) pkg.__getattr__("__path__");
            name = name.substring(dot + 1, name.length()).intern();
        }

        nm.__setattr__("__name__", new PyString(modName));
        PyObject ret = find_module(name, modName, path);
        modules.__setitem__(modName, ret);
        return ret;
    }

    public static int getAPIVersion() {
        return APIVersion;
    }
}
