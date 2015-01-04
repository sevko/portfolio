/**
 * A simulation of multiple moving actors under the influence of various
 * forces, including an imaginary force exerted by a pressed mouse.
 */

/**
 * A moving actor that exhibits acceleration. Rebounds vertically with no loss
 * of speed, but at some loss horizontally.
 */
class Mover {
	PVector location, velocity, acceleration;
	float mass;
	int size;

	Mover(float x, float y){
		location = new PVector(x, y);
		velocity = new PVector(0, 0);
		acceleration = new PVector(0, 0);
		mass = random(10, 40);
		size = (int)mass;
	}

	void move(){
		velocity.add(acceleration);
		location.add(velocity);

		if(location.x < 0){
			location.x = 0;
			velocity.x *= -0.7;
		}
		else if(location.x > width){
			location.x = width;
			velocity.x *= -0.7;
		}

		if(location.y < 0){
			location.y = 0;
			velocity.y *= -1;
		}
		else if(location.y > height){
			location.y = height;
			velocity.y *= -1;
		}
	}

	void display(){
		stroke(0);
		fill(175);
		ellipse(location.x, location.y, size, size);
	}

	void applyForce(PVector force){
		acceleration.add(PVector.div(force, mass));
	}
}

PVector[] forces;
Mover[] movers;

final int numMovers = 30;

void setup(){
	size(1000, 600);
	forces = new PVector[] {
		new PVector(0, 9.81)
	};
	movers = new Mover[numMovers];
	for(int ind = 0; ind < movers.length; ind++){
		movers[ind] = new Mover(random(width), random(height * 0.8));
	}
}

/**
 * Moves each `Mover` in `movers`, first subjecting it to each force in
 * `forces` and the force exerted by a pressed mouse.
 */
void draw(){
	background(255);
	for(int movInd = 0; movInd < movers.length; movInd++){
		Mover mover = movers[movInd];
		for(int forceInd = 0; forceInd < forces.length; forceInd++){
			mover.applyForce(forces[forceInd]);
		}

		if(mousePressed){
			PVector mouse = new PVector(mouseX, mouseY);
			PVector windForce = PVector.sub(mover.location, mouse);
			windForce.normalize();
			windForce.mult(10);
			mover.applyForce(windForce);
		}

		mover.move();
		mover.display();
		mover.acceleration.mult(0);
	}
}
