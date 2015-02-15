"""
A simple genetic algorithm that evolves a population towards a desired state.
The population consists of random strings, and the target is a line from
Hamlet.
"""

import random

class Population(object):
	def __init__(self, members, target):
		self.members = members
		self.target = target

	def iterate(self):
		"""
		Evolve this population towards `self.target`.
		"""

		fitnesses = [mem.fitness(self.target) for mem in self.members]

		new_members = []
		for _ in self.members:
			parentA = self.get_member(fitnesses)
			parentB = self.get_member(fitnesses)
			new_members.append(parentA.reproduce(parentB, self.target))

		self.members = new_members

	def get_member(self, fitnesses):
		"""
		Get a random member from `self.members` using the `fitnesses` values
		(as retrieved with `Member.fitness()`) as weights that influence the
		choice.
		"""

		selector = random.uniform(0, sum(fitnesses))
		for member, fitness in zip(self.members, fitnesses):
			if selector < fitness:
				return member
			selector -= fitness

		raise Exception("No member found. This shouldn't have happened.")

	def get_fittest(self):
		mem_fitnesses = [(mem.fitness(self.target), mem) for mem in self.members]
		mem_fitnesses.sort(key=lambda item: item[0])
		return mem_fitnesses[-1][1]

class Member(object):
	def __init__(self, genes):
		self.genes = genes

	def fitness(self, target):
		"""
		Quantify the fitness of this `Member` with respect to `target`, the
		desired state.
		"""

		fitness_val = 0
		for chr1, chr2 in zip(self.genes, target):
			if chr1 == chr2:
				fitness_val += 1

		return fitness_val / len(target)

	def reproduce(self, other, target):
		"""
		Reproduce this `Member` with another, creating a child `Member` from
		their pooled genes that's maximally similar to `target`.
		"""

		genes = []
		for gene1, gene2, target_gene in zip(self.genes, other.genes, target):
			next_gene = None
			if gene1 == target_gene:
				next_gene = gene1
			elif gene2 == target_gene:
				next_gene = gene2
			else:
				next_gene = random.choice((gene1, gene2))

			genes.append(next_gene)

		return Member(genes)

def simulation():
	"""
	Create a `Population` of random `Member`s and `print()` its state over
	various iterations, as its fittest members approach the desired state.
	"""

	letters = [chr(num) for num in range(ord('a'), ord('z') + 1)] + [' ']
	target = (
		"to be or not to be that is the question whether tis nobler in"
		" the mind to suffer"
	)
	members = [
		Member([random.choice(letters) for _ in target])
		for _ in range(1000)
	]
	population = Population(members, target)

	for _ in range(1000):
		population.iterate()
		curr_state = "".join(population.get_fittest().genes)
		print(curr_state)
		if curr_state == target:
			break

if __name__ == "__main__":
	simulation()
