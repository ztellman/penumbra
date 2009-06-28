A basic, idiomatic wrapper for OpenGL in Clojure.

Vellum allows for intra-primitive transformations, which allows us to define a circle like so:

	(draw-line-strip
  		(dotimes [angle 360]
    		(rotate 1 0 0 1)
    		(vertex 0 1 0)))

rather than:

	(draw-line-strip
  		(dotimes [angle 360]
    		(vertex (Math/cos (* Math/PI (/ angle 180)))
            		(Math/sin (* Math/PI (/ angle 180)))
            		0)))

This is a trivial example, but as shapes become more complex, this becomes increasingly useful.
