/**
 * A particle system simulation, consisting of a particle emitter that tracks
 * the user's mouse and spews particles from that location.
 */

import java.util.Iterator;

/**
 * A particle with a location, velocity, acceleration, angle, and angular
 * acceleration, that expires over time.
 */
class Particle {
	PVector location, velocity, acceleration;
	float angle, angAcceleration;
	int life;

	Particle(PVector location_, PVector velocityBase){
		location = location_;
		velocity = new PVector(random(-3, 3), random(-3, 3));
		velocity.add(velocityBase);
		acceleration = new PVector(0, 0.1);

		life = 255;
		angle = velocity.heading();
		angAcceleration = radians(10);
	}

	void move(){
		angle += angAcceleration;
		velocity.add(acceleration);
		location.add(velocity);
	}

	boolean isExpired(){
		return life <= 0;
	}

	void render(){
		noStroke();
		fill(0, life);

		pushMatrix();
		translate(location.x, location.y);
		rotate(angle);
		rectMode(CENTER);
		rect(0, 0, 2, 10);
		popMatrix();
	}

	void update(){
		move();
		render();
		life--;
	}
}

/**
 * A particle emitter that spews out particles from the mouse location, and
 * speeds them up according to the mouse's recent movement.
 */
class ParticleEmitter {
	PVector oldOrigin, origin;
	ArrayList<Particle> particles;

	ParticleEmitter(){
		origin = new PVector(width / 2, height / 2);
		oldOrigin = origin.get();
		particles = new ArrayList<Particle>();
	}

	void addParticle(){
		PVector velocityBase = PVector.sub(origin, oldOrigin);
		velocityBase.div(10);
		particles.add(new Particle(origin.get(), velocityBase));
	}

	void update(){
		oldOrigin = origin;
		origin = new PVector(mouseX, mouseY);

		addParticle();

		fill(0xffff0000);
		ellipse(origin.x, origin.y, 10, 10);

		Iterator<Particle> partIter = particles.iterator();
		while(partIter.hasNext()){
			Particle currPart = partIter.next();
			currPart.update();
			if(currPart.isExpired()){
				partIter.remove();
			}
		}
	}
}

ParticleEmitter particleEmitter;

void setup(){
	size(1000, 700);
	particleEmitter = new ParticleEmitter();
}

void draw(){
	background(255);
	particleEmitter.update();
}
