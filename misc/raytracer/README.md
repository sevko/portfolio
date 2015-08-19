# raytracer
A simple Python raytracer that supports spheres with configurable "material" properties (base color and a bunch of
light coefficients). To generate a raytraced image of the pre-defined scene, run: `python raytracer.py` and open
`image.ppm` with a PPM-compatible viewer (`eog` works fine on Linux):

![raytraced spheres](https://cloud.githubusercontent.com/assets/4467604/9369807/a7d158f8-4699-11e5-9634-7b9cc20c5607.png)

## acknowledgements
I found the following resources extremely helpful:

  * [a high-level overview of raytracing](http://www.cs.unc.edu/~rademach/xroads-RT/RTarticle.html)
  * [a walkthrough of a C raytracer implementation and the relevant linear algebra](http://www.purplealienplanet.com/node/20)
  * [a literate JavaScript raytracer](http://www.macwright.org/literate-raytracer/)
