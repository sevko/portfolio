/**
 * Render a Sirpinski Triangle.
 */

static final boolean[] states = {
	false, true, false, true, true, false, true, false
};
boolean[] prevRow;
int row;

void setup(){
	size(1000, 801);
	prevRow = new boolean[width];
	prevRow[prevRow.length / 2] = true;
	row = 0;
}

void draw(){
	if(++row < height){
		boolean[] currRow = new boolean[width];
		for(int ind = 1; ind < currRow.length - 1; ind++){
			int stateInd = ((prevRow[ind - 1] ? 1 : 0) << 2) +
				((prevRow[ind] ? 1 : 0) << 1) +
				((prevRow[ind + 1] ? 1 : 0));

			boolean state = states[stateInd];
			currRow[ind] = state;
			if(state){
				point(ind, row);
			}
		}

		prevRow = currRow;
	}
	else {
		noLoop();
	}
}
