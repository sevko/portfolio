/**
 * A simulation involving a triangular actor which can be moved by the arrow
 * keys and always points towards the mouse.
 */

/**
 * Used to identify which keys are pressed at any given time, to support
 * simultaneous key-presses that we wouldn't otherwise be able to handle with
 * `keyPressed()`.
 */
boolean[] keysPressed;

// Govern actor position and movement.
PVector location;
final float VELOCITY = 3;

void keyPressed(){
	keysPressed[keyCode] = true;
}

void keyReleased(){
	keysPressed[keyCode] = false;
}

void setup(){
	size(1000, 700);
	location = new PVector(width / 2, height / 2);
	keysPressed = new boolean[255];
}

void draw(){
	background(255);
	handleKeys();
	translate(location.x, location.y);
	rotate(atan2(mouseY - location.y, mouseX - location.x));
	triangle(30, 0, 0, 10, 0, -10);
}

/**
 * Handle the potentially multiple keys that are currently pressed.
 */
void handleKeys(){
	if(keysPressed[37]){
		location.x -= VELOCITY;
	}

	if(keysPressed[38]){
		location.y -= VELOCITY;
	}

	if(keysPressed[39]){
		location.x += VELOCITY;
	}

	if(keysPressed[40]){
		location.y += VELOCITY;
	}
}
