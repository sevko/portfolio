/**
 * Render a sprouting Kock snowflake.
 */

/**
 * The self-splitting line that constitutes a Koch curve.
 */
class KochLine {
	PVector start, end;

	KochLine(PVector start_, PVector end_){
		start = start_;
		end = end_;
	}

	KochLine[] split(){
		PVector b, c, d;

		PVector third = PVector.sub(end, start);
		third.div(3);

		b = PVector.add(start, third);
		d = PVector.add(b, third);
		third.rotate(PI / 3);
		c = PVector.add(b, third);

		KochLine sublines[] = new KochLine[]{
			new KochLine(start, b),
			new KochLine(b, c),
			new KochLine(c, d),
			new KochLine(d, end)
		};
		return sublines;
	}

	void render(){
		line(start.x, start.y, end.x, end.y);
	}
}

ArrayList<KochLine> kochCurve;

void setup(){
	size(1000, 800);
	frameRate(3);

	PVector a = new PVector(width * 0.4, height / 2),
		b = new PVector(width * 0.6, height / 2);
	PVector base = PVector.sub(b, a);
	base.rotate(-PI / 3);
	PVector c = PVector.add(a.get(), base);

	kochCurve = new ArrayList<KochLine>();
	kochCurve.add(new KochLine(a, b));
	kochCurve.add(new KochLine(b, c));
	kochCurve.add(new KochLine(c, a));
}

void draw(){
	background(255);
	ArrayList<KochLine> nextCurve = new ArrayList<KochLine>();
	for(KochLine line : kochCurve){
		line.render();
		for(KochLine subline : line.split()){
			nextCurve.add(subline);
		}
	}
	kochCurve = nextCurve;
}
