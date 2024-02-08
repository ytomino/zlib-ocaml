zlib interface library for Objective-Caml
=========================================

What's this?
------------

Objective-Caml binding to zlib.

Prerequisites
-------------

OCaml >= 4.08
 https://ocaml.org/
zlib >= 1.0
 http://www.zlib.net/

How to make
-----------

Install
+++++++

::

 make install PREFIX=/usr/local

Specify your preferred directory to ``PREFIX``.
The libraries would be installed into ``$PREFIX/lib/ocaml`` (default is
``ocamlc -where``).

If zlib is not installed in the default search path, specify the directory
containing zlib to ``WITH_ZLIB``.
``$WITH_ZLIB/include`` and ``$WITH_ZLIB/lib`` would be used.

Uninstall
+++++++++

::

 make uninstall PREFIX=/usr/local

Build examples
++++++++++++++

::

 make -C examples

License
-------

It is dual-licensed under the New BSD License and the zlib License, see below.
I recommend the zlib License that is same as zlib.

**license of zlib-ocaml (1)** ::

 Copyright 2010-2024 YT. All rights reserved.
 
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 
 THIS SOFTWARE IS PROVIDED BY THE AUTHOR(S) ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

**license of zlib-ocaml (2)** ::

 Copyright 2010-2024 YT.
 
 This software is provided 'as-is', without any express or implied
 warranty.  In no event will the authors be held liable for any damages
 arising from the use of this software.
 
 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it
 freely, subject to the following restrictions:
 
 1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software
    in a product, an acknowledgment in the product documentation would be
    appreciated but is not required.
 2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.
 3. This notice may not be removed or altered from any source distribution.

**license of zlib** ::

 /* zlib.h -- interface of the 'zlib' general purpose compression library
   version 1.2.5, April 19th, 2010
 
   Copyright (C) 1995-2010 Jean-loup Gailly and Mark Adler
 
   This software is provided 'as-is', without any express or implied
   warranty.  In no event will the authors be held liable for any damages
   arising from the use of this software.
 
   Permission is granted to anyone to use this software for any purpose,
   including commercial applications, and to alter it and redistribute it
   freely, subject to the following restrictions:
 
   1. The origin of this software must not be misrepresented; you must not
      claim that you wrote the original software. If you use this software
      in a product, an acknowledgment in the product documentation would be
      appreciated but is not required.
   2. Altered source versions must be plainly marked as such, and must not be
      misrepresented as being the original software.
   3. This notice may not be removed or altered from any source distribution.
 
   Jean-loup Gailly
   Mark Adler
 
 */
