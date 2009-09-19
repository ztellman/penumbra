    Copyright (c) Zachary Tellman. All rights reserved.
    The use and distribution terms for this software are covered by the
    Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
    which can be found in the file epl-v10.html at the root of this distribution.
    By using this software in any fashion, you are agreeing to be bound by
    the terms of this license.
    You must not remove this notice, or any other, from this software.

Penumbra is a wrapper for OpenGL in Clojure, by way of JOGL.

This Java/C code

	glEnable(GL_LIGHT0);
	glPushMatrix();
	glTranslated(0, 0, -10);
	glBegin(GL_QUADS);
	glVertex3d(0, 0, 0);
	glVertex3d(0, 1, 0);
	glVertex3d(1, 1, 0);
	glVertex3d(1, 0, 0);
	glEnd();
	glPopMatrix();
	
becomes

	(enable :light0)
	(push-matrix
	  (translate 0 0 -10)
	  (draw-quads
	    (vertex 0 0 0)
		(vertex 0 1 0)
		(vertex 1 1 0)
		(vertex 1 0 0)))


It also allows for intra-primitive transformations, which allows us to define a circle like this:

	(draw-line-strip
  	  (dotimes [_ 360]
        (rotate 1 0 0 1)
    	(vertex 0 1 0)))

rather than this:

	(draw-line-strip
  	  (dotimes [angle 360]
        (vertex (Math/cos (* Math/PI (/ angle 180.)))
        (Math/sin (* Math/PI (/ angle 180.)))
        0)))

Shaders are supported, via an s-expression representation of GLSL.  It mimics Clojure where possible, and has basic type inference, but is constrained by the target language (imperative, explicit types, etc.)  

	=> (use 'penumbra.glsl.core)
	
	=> (translate-glsl '(-> a (+ 2) (/ 3) sin))
	sin(((a + 2) / 3))
	
	=> (translate-glsl '(let [a (float2 1.0 2.0), b (float4 1.0 2.0 3.0 4.0)]))
	vec2 a = vec2(1.0, 2.0);
	vec4 b = vec4(1.0, 2.0, 3.0, 4.0);

	=> (translate-shader '(set! :frag-color (fract :frag-coord)))
	void main()
	{
	  gl_FragColor = fract(gl_FragCoord);
	}
	
Using this representation, Penumbra supports running general-purpose computations on the graphics card (GPGPU).  For sufficiently large data sets, this can be significantly faster than the same operation on the CPU. Currently map and reduce operations are supported, filter and convolve are to follow.

	=> (use 'penumbra.compute)

  	=> (with-blank-slate
	     (defmap scaled-add
	       (+ 
		     #^float4 %1 
		     (* #^float k #^float4 %2))) ;a + k*b
	     (unwrap* (scaled-add {:k 2.0} [(range 20) (range 20)])))
	(3.0 6.0 9.0 12.0 ...)
	
	=> (with-blank-slate
	     (defreduce sum #^float4 (+ %1 %2))
	     (apply + (sum (range 20)))
	210

