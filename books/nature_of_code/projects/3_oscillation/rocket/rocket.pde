/**
 * A simulation of a rocket that can be steered with the left and right arrow
 * keys, and accelerated forwards/backwards with the up and down arrows. The
 * rocket exhibits constant deceleration during motion.
 */

class Rocket {
	PVector location;
	float speed, angle;

	Rocket(){
		location = new PVector(width / 2, height / 2);
		speed = 0;
		angle = 0;
	}

	void move(){
		location.x += speed * cos(angle);
		location.y += speed * sin(angle);
	}

	void render(){
		pushMatrix();
		translate(rocket.location.x, rocket.location.y);
		rotate(rocket.angle);
		triangle(30, 0, 0, 10, 0, -10);
		popMatrix();
	}
}

Rocket rocket;

void setup(){
	size(1000, 700);
	rocket = new Rocket();
}

void keyPressed(){
	switch(keyCode){
		case LEFT:
			rocket.angle -= 0.1;
			break;

		case RIGHT:
			rocket.angle += 0.1;
			break;

		case UP:
			rocket.speed++;
			break;

		case DOWN:
			rocket.speed--;
			break;
	}
}

void draw(){
	background(255);
	rocket.move();
	rocket.render();
	if(rocket.speed > 0){
		rocket.speed -= 0.01;
	}
}
