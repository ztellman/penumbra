Penumbra is a basic wrapper for OpenGL in Clojure.

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

	(gl-enable :light0)
	(push-matrix
		(translate 0 0 -10)
		(draw-quads
			(vertex 0 0 0)
			(vertex 0 1 0)
			(vertex 1 1 0)
			(vertex 1 0 0)))


It also allows for intra-primitive transformations, which allows us to define a circle like this:

	(draw-line-strip
  		(dotimes [angle 360]
    		(rotate 1 0 0 1)
    		(vertex 0 1 0)))

rather than this:

	(draw-line-strip
  		(dotimes [angle 360]
    		(vertex (Math/cos (* Math/PI (/ angle 180.)))
            		(Math/sin (* Math/PI (/ angle 180.)))
            		0)))


Some features I'd like to add, in ascending order of complexity:
* procedural mesh generation
* pixel shading
* GPGPU