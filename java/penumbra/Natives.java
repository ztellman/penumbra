/*
 * Copyright (c) 2009-2010 jMonkeyEngine
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * * Neither the name of 'jMonkeyEngine' nor the names of its contributors
 *   may be used to endorse or promote products derived from this software
 *   without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package penumbra;

import penumbra.PenumbraSystem.Platform;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Helper class for extracting the natives (dll, so) from the jars.
 * This class should only be used internally.
 */
public class Natives {

    private static final Logger logger = Logger.getLogger(Natives.class.getName());
    private static final byte[] buf = new byte[1024];
    private static File workingDir = new File("").getAbsoluteFile();

    public static void setExtractionDir(String name){
        workingDir = new File(name).getAbsoluteFile();
    }

    protected static void extractNativeLib(String sysName, String name) throws IOException{
        String fullname = System.mapLibraryName(name);

        String path = "native/"+sysName+"/" + fullname;
        InputStream in = Thread.currentThread().getContextClassLoader().getResourceAsStream(path);
        //InputStream in = Natives.class.getResourceAsStream();
        if (in == null) {
            logger.log(Level.WARNING, "Cannot locate native library: {0}/{1}", 
                    new String[]{ sysName, fullname} );
            return;
        }
        File targetFile = new File(workingDir, fullname);
        try {
            OutputStream out = new FileOutputStream(targetFile);
            int len;
            while ((len = in.read(buf)) > 0) {
                out.write(buf, 0, len);
            }
            in.close();
            out.close();
        } catch (FileNotFoundException ex){
            if (ex.getMessage().contains("used by another process"))
                return;

            throw ex;
        }

        logger.log(Level.FINE, "Copied {0} to {1}", new Object[]{fullname, targetFile});
    }

    private static String getExtractionDir(){
        URL temp = Natives.class.getResource("");
        if (temp != null) {
            StringBuilder sb = new StringBuilder(temp.toString());
            if (sb.indexOf("jar:") == 0) {
                sb.delete(0, 4);
                sb.delete(sb.indexOf("!"), sb.length());
                sb.delete(sb.lastIndexOf("/") + 1, sb.length());
            }
            try {
                return new URL(sb.toString()).toString();
            } catch (MalformedURLException ex) {
                return null;
            }
        }
        return null;
    }

    public static void extractNativeLibs(Platform platform, String settings) throws IOException{
        String renderer = "LWJGL";
        String audioRenderer = null;
        boolean needLWJGL = false;
        boolean needGG = false;
        boolean needJOGL = false;
        boolean needJOAL = false;
        boolean needOAL = false;
        boolean needJInput = false;
        if (renderer != null){
            if (renderer.startsWith("LWJGL")){
                needLWJGL = true;
            }else if (renderer.startsWith("JOGL")){
                needGG = true;
                needJOGL = true;
            }
        }
        if (audioRenderer != null){
            if (audioRenderer.equals("LWJGL")){
                needLWJGL = true;
                needOAL = true;
            }else if (audioRenderer.equals("JOAL")){
                needJOAL = true;
                needGG = true;
                needOAL = true;
            }
        }

        if (needLWJGL){
            logger.log(Level.INFO, "Extraction Directory #1: {0}", getExtractionDir());
            logger.log(Level.INFO, "Extraction Directory #2: {0}", workingDir.toString());
            logger.log(Level.INFO, "Extraction Directory #3: {0}", System.getProperty("user.dir"));
            // LWJGL supports this feature where
            // it can load libraries from this path.
            // This is a fallback method in case the OS doesn't load
            // native libraries from the working directory (e.g Linux).
            System.setProperty("org.lwjgl.librarypath", workingDir.toString());
        }

        switch (platform){
            case Windows64:
                if (needLWJGL){
                    extractNativeLib("windows", "lwjgl64");
                }
                if (needJOGL){
                    extractNativeLib("win64", "jogl_awt");
                    extractNativeLib("win64", "jogl");
                }

                if (needJOAL)
                    extractNativeLib("win64", "joal_native");

                if (needGG)
                    extractNativeLib("win64", "gluegen-rt");

                if (needOAL)
                    extractNativeLib("windows", "OpenAL64");
                
                if (needJInput){
                    extractNativeLib("windows", "jinput-dx8_64");
                    extractNativeLib("windows", "jinput-raw_64");
                }
                break;
            case Windows32:
                if (needLWJGL){
                    extractNativeLib("windows", "lwjgl");
                }
                if (needJOGL){
                    extractNativeLib("win32", "jogl_awt");
                    extractNativeLib("win32", "jogl");
                }

                if (needJOAL)
                    extractNativeLib("win32", "joal_native");

                if (needGG)
                    extractNativeLib("win32", "gluegen-rt");

                if (needOAL)
                    extractNativeLib("windows", "OpenAL32");

                if (needJInput){
                    extractNativeLib("windows", "jinput-dx8");
                    extractNativeLib("windows", "jinput-raw");
                }
                break;
            case Linux64:
                if (needLWJGL){
                    extractNativeLib("linux", "lwjgl64");
                }
                if (needJOGL){
                    extractNativeLib("linux64", "jogl_awt");
                    extractNativeLib("linux64", "jogl");
                }

                if (needJOAL)
                    extractNativeLib("linux64", "joal_native");

                if (needGG)
                    extractNativeLib("linux64", "gluegen-rt");

                if (needJInput)
                    extractNativeLib("linux", "jinput-linux64");

                if (needOAL)    
                    extractNativeLib("linux", "openal64");

                break;
            case Linux32:
                if (needLWJGL){
                    extractNativeLib("linux", "lwjgl");
                }
                if (needJOGL){
                    extractNativeLib("linux32", "jogl_awt");
                    extractNativeLib("linux32", "jogl");
                }

                if (needJOAL)
                    extractNativeLib("linux32", "joal_native");

                if (needGG)
                    extractNativeLib("linux32", "gluegen-rt");

                if (needJInput)
                    extractNativeLib("linux", "jinput-linux");

                if (needOAL)
                    extractNativeLib("linux", "openal");

                break;
            case MacOSX_PPC32:
                if (needLWJGL){
                    extractNativeLib("macosx", "lwjgl");
                }
                if (needJOGL){
                    extractNativeLib("macosx_ppc", "jogl_awt");
                    extractNativeLib("macosx_ppc", "jogl");
                }

                if (needJOAL)
                    throw new UnsupportedOperationException("JOAL not available on Mac OS PPC");

                if (needGG)
                    extractNativeLib("macosx_ppc", "gluegen-rt");

//                if (needOAL)
//                    extractNativeLib("macosx", "openal");

                if (needJInput)
                    extractNativeLib("macosx", "jinput-osx");

                break;
            case MacOSX32:
                if (needLWJGL){
                    extractNativeLib("macosx", "lwjgl");
                }
                if (needJOGL){
                    extractNativeLib("macosx_universal", "jogl_awt");
                    extractNativeLib("macosx_universal", "jogl");
                }

                if (needJOAL)
                    extractNativeLib("macosx_universal", "joal_native");

                if (needGG)
                    extractNativeLib("macosx_universal", "gluegen-rt");

//                if (needOAL)
//                    extractNativeLib("macosx", "openal");

                if (needJInput)
                    extractNativeLib("macosx", "jinput-osx");

                break;
            case MacOSX_PPC64:
                if (needLWJGL){
                    extractNativeLib("macosx", "lwjgl");
                }
                if (needJOGL){
                    extractNativeLib("macosx_ppc", "jogl_awt");
                    extractNativeLib("macosx_ppc", "jogl");
                }

                if (needJOAL)
                    throw new UnsupportedOperationException("JOAL not available on Mac OS 64 bit");

                if (needGG)
                    extractNativeLib("macosx_ppc", "gluegen-rt");

//                if (needOAL)
//                    extractNativeLib("macosx", "openal");

                if (needJInput)
                    extractNativeLib("macosx", "jinput-osx");

                break;
            case MacOSX64:
                if (needLWJGL){
                    extractNativeLib("macosx", "lwjgl");
                }
                if (needJOGL){
                    extractNativeLib("macosx_universal", "jogl_awt");
                    extractNativeLib("macosx_universal", "jogl");
                }

                if (needJOAL)
                    throw new UnsupportedOperationException("JOAL not available on Mac OS 64 bit");

                if (needGG)
                    extractNativeLib("macosx_universal", "gluegen-rt");

//                if (needOAL)
//                    extractNativeLib("macosx", "openal");

                if (needJInput)
                    extractNativeLib("macosx", "jinput-osx");

                break;
            
                
        }
    }

}
