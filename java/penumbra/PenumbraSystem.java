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

import java.io.IOException;
import java.net.URL;


public class PenumbraSystem {

    public static enum Platform {

        /**
         * Microsoft Windows 32 bit
         */
        Windows32,

        /**
         * Microsoft Windows 64 bit
         */
        Windows64,

        /**
         * Linux 32 bit
         */
        Linux32,


        /**
         * Linux 64 bit
         */
        Linux64,

        /**
         * Apple Mac OS X 32 bit
         */
        MacOSX32,

        /**
         * Apple Mac OS X 64 bit
         */
        MacOSX64,

        /**
         * Apple Mac OS X 32 bit PowerPC
         */
        MacOSX_PPC32,

        /**
         * Apple Mac OS X 64 bit PowerPC
         */
        MacOSX_PPC64,

        /**
         * Google Android Smartphone OS
         */
        Android
    }

    private static boolean is64Bit(String arch){
        if (arch.equals("x86"))
            return false;
        else if (arch.equals("amd64"))
            return true;
        else if (arch.equals("x86_64"))
            return true;
        else if (arch.equals("ppc") || arch.equals("PowerPC"))
            return false;
        else if (arch.equals("ppc64"))
            return true;
        else if (arch.equals("i386") || arch.equals("i686"))
            return false;
        else
            throw new UnsupportedOperationException("Unsupported architecture: "+arch);
    }

    public static Platform getPlatform(){
        String os = System.getProperty("os.name").toLowerCase();
        String arch = System.getProperty("os.arch").toLowerCase();
        boolean is64 = is64Bit(arch);
        if (os.contains("windows")){
            return is64 ? Platform.Windows64 : Platform.Windows32;
        }else if (os.contains("linux") || os.contains("freebsd") || os.contains("sunos")){
            return is64 ? Platform.Linux64 : Platform.Linux32;
        }else if (os.contains("mac os x")){
            if (arch.startsWith("ppc")){
                return is64 ? Platform.MacOSX_PPC64 : Platform.MacOSX_PPC32;
            }else{
                return is64 ? Platform.MacOSX64 : Platform.MacOSX32;
            }
        }else{
            throw new UnsupportedOperationException("The specified platform: "+os+" is not supported.");
        }
    }
    
}
