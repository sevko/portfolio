/**
 * A sketch that renders a cluster of nodes that tend towards equidistance
 * from one another.
 */

import toxi.geom.*;
import toxi.physics2d.*;

class Node extends VerletParticle2D {
	Node(Vec2D pos){
		super(pos);
	}

	void display(){
		fill(0, 150);
		stroke(0);
		ellipse(x, y, 16, 16);
	}
}

class Cluster {
	ArrayList<Node> nodes;
	float diameter;

	Cluster(int numNodes, float diameter_, Vec2D center){
		nodes = new ArrayList<Node>();
		diameter = diameter_;
		for(int ind = 0; ind < numNodes; ind++){
			Vec2D pos = center.add(Vec2D.randomVector());
			nodes.add(new Node(pos));
		}

		for(int ind1 = 0; ind1 < nodes.size(); ind1++){
			for(int ind2 = ind1 + 1; ind2 < nodes.size(); ind2++){
				physics.addSpring(new VerletSpring2D(
					nodes.get(ind1), nodes.get(ind2), diameter, 0.001
				));
			}
		}
	}

	void display(){
		for(Node node : nodes){
			node.display();
		}
	}
}

VerletPhysics2D physics;
ArrayList<Cluster> clusters;

void setup(){
	size(800, 600);
	physics = new VerletPhysics2D();
	clusters = new ArrayList<Cluster>();
}

void mouseClicked(){
	clusters.add(new Cluster(
		(int)random(15, 20), random(20, 100), new Vec2D(mouseX, mouseY))
	);
}

void draw(){
	background(255);
	physics.update();
	for(Cluster cluster : clusters){
		cluster.display();
	}
}
