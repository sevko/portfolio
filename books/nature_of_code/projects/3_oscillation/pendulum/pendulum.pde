/**
 * An animation of a swinging pendulum.
 */

/**
 * A pendulum with a configurable length, as well as mass and starting angle of
 * its weight.
 */
class Pendulum {
	float length, mass;
	float angle, angVelocity, angAcceleration;

	Pendulum(float length_, float mass_, float angle_){
		length = length_;
		mass = mass_;
		angle = angle_;
		angVelocity = 0;
		angAcceleration = 0;
	}

	void move(){
		angAcceleration = mass * sin(angle) / length;
		angVelocity += angAcceleration;
		angle -= angVelocity;
	}

	void render(){
		float x = sin(angle) * length;
		float y = cos(angle) * length;

		fill(0);
		ellipse(0, 0, 2, 2); // fulcrum
		line(0, 0, x, y); // rod
		noFill();
		ellipse(x, y, 10, 10); // weight
	}
}

Pendulum pendulum;

void setup(){
	size(1000, 800);
	pendulum = new Pendulum(300, 1, radians(10));
}

void draw(){
	background(255);
	translate(width / 2, height / 3);
	pendulum.move();
	pendulum.render();
}
