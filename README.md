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
	glTranslate(0, 0, -10);
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

Shaders are supported, via an s-expression representation of GLSL.  It mimics Clojure where possible, and generally follows the principle of least surprise, but is constrained by the limitations of the target language (it's imperative rather than functional, variables must have explicit types, etc.).

	=> (use 'penumbra.opengl.translate)
	
	=> (translate '(+ a 1 (- b 2)))
	((a + 1) + (b - 2))
	
	=> (translate '(-> a (+ 2) (/ 3) sin))
	sin(((a + 2) / 3))
	
	=> (translate '(let [(vec2i b) [1 2] (vec3 a) [1.0 2.0 3.0] ]))
	float b = vec2i(1, 2);
	vec3 a = vec3(1.0, 2.0, 3.0);
	
	=> (translate '(if (< a b) (+= a 1) (+= b 1)))
	if ((a < b))
	{
	  a += 1;
	}
	else
	{
	  b += 1;
	}
	
	=> (translate '(set! a (if (< a b) (+ a 1) (+ b 1))))
	a = ((a < b) ? (a + 1) : (b + 1))
	
	=> (translate-shader '(set! :frag-color (fract :frag-coord)))
	void main()
	{
	  gl_FragColor = fract(gl_FragCoord);
	}
	
Using this intermediate GLSL representation, I'm working on adding idiomatic GPGPU to the library.

