<style>
	.reveal section img {
		border-style: none;
	}

	.reveal {
		background-color: white;
		color: #777;
	}

	h1, h2, h3, h4, h5, h6 {
		color: #d4645c !important;
	}
</style>

# Mapzen

(one year later)

---

## some background

  * I graduated from high-school a year ago
  * was into programming/pure CS for a couple of years
  * wanted to take a gap year to work a tech startup
  * spoke to someone who spoke to someone who spoke to Randy
  * and ended up here

---

## what's a GIS?

  * knew 0 about geo
  * EPSG 4326/900913 hell
  * mystical ogr2ogr incantations
  * &#35;opendata
  * the companies, people, and software

---

## pelias

![](http://files.sevko.io/presentations/mapzen_internship/node_meme.png)

---

## pelias cont.

  * touched most of the codebase:
    * improved/sped-up our admin-region name lookups
    * maintained our OpenAddresses importer
    * shaped our acceptance-tests
    * modernized large chunks of our import pipelines
    * enhanced some elasticsearch/API logic
    * helped manage deployments and follow-up QA
    * maintained Quattroshapes
    * broke builds, shipped stuff, etc.
  * 528 commits, +11352/-7636

---

## broke allthethings

![](http://files.sevko.io/presentations/mapzen_internship/broken_builds.png)

---

## vector tiles

![](http://files.sevko.io/presentations/mapzen_internship/complex_diagram.png)

---

## vector tiles cont.

* worked on vector tile rendering
* enhanced/abstracted out parts of tilequeue
* added higher-precision layers/more data
* found Antarctica
* fixed tile seams

---

## vector tiles: seams

![An example of tile seams.](http://files.sevko.io/presentations/mapzen_internship/seam.png)

---

## misc

  * learned about the geo industry
  * team management
  * enhanced my workflow and ops chops
  * and of course:
    * maximizing disruption
    * speed-up loops

```python
# for a rainy day
for _ in range(10000):
	pass
```

---

![Burrito!](http://files.sevko.io/presentations/mapzen_internship/burrito.png)
![Ramen!](http://files.sevko.io/presentations/mapzen_internship/ramen.png)

---

## farewell

  * had an awesome time
  * you guys are amazing
  * come visit at 42.36426, -71.08851
