import {symbolic, Rule, scoreFactory, StringifyOptions} from './symbolic';

class EClass extends symbolic {
	parent?: EClass;
	parents = new Set<symbolic>();
	bestNode?: symbolic;
	bestScore?: number;
	constructor(id: string, public nodes: Set<symbolic>) { super(id); }
	repr(): symbolic|undefined { const it = this.nodes.values(); const first = it.next(); return first.value; }
//	toString(_opts?: StringifyOptions): string { return `EClass ${this.repr() ?? this.id}`; }
	_toString(_opts: StringifyOptions): string { return `EClass ${this.id}`; }
}

export default class EGraph {
	private enodeMap		= new Map<string, EClass>();
	private canonicalMap	= new Map<string, EClass>();
	public validate?: (node: symbolic, rep: symbolic) => boolean;
	private scorer = scoreFactory();

	constructor(public verbose = false, public maxExpansions = 128) {
	}

	private updateBestNode(eclass: EClass) {
		let best: symbolic | undefined;
		let bestScore = Infinity;
		
		for (const node of eclass.nodes) {
			const score = this.scorer(node);
			if (score < bestScore) {
				bestScore = score;
				best = node;
			}
		}
		
		eclass.bestNode = best;
		eclass.bestScore = bestScore;
	}

	private find(e: EClass): EClass {
		if (!e.parent)
			return e;
		const r = this.find(e.parent);
		e.parent = r;
		return r;
	}

	private eclassOfNode(nid: string): EClass | undefined {
		const e = this.enodeMap.get(nid);
		if (!e || !e.parent)
			return e;
		const e2 = this.find(e);
		this.enodeMap.set(nid, e2);
		return e2;
	}

	private computeCanonicalSig(node: symbolic): string {
		const mapped = node.visit({pre: (n: symbolic) => this.eclassOfNode(n.id) ?? n});
		return mapped.id;
	}
	
	// merge with congruence-closure worklist processing will be done by mergeClasses
	private mergeClasses(a: EClass, b: EClass): boolean {
		if (a === b)
			return false;

		if (this.verbose)
			console.log('egraph-diagnostic: merging', String(a), 'with', String(b));

		if (this.validate && !this.validate(a.repr()!, b.repr()!))
			throw new Error("Validation failed during merge");

		// merge smaller class into larger class (by node count)
		if (a.nodes.size < b.nodes.size)
			[a, b] = [b, a];

		// attach source to target and move nodes
		b.parent = a;

		for (const n of b.nodes)
			a.nodes.add(n);
		b.nodes.clear();

		for (const p of b.parents)
			a.parents.add(p);
		b.parents.clear();

		for (const parent of Array.from(a.parents)) {
			// recompute canonical signature
			const newSig			= this.computeCanonicalSig(parent);

			// if canonicalMap already maps newSig to some EClass, union their classes
			const parentEClass		= this.eclassOfNode(parent.id)!;
			const existingEClass	= this.canonicalMap.get(newSig);
			if (existingEClass)
				this.mergeClasses(this.find(existingEClass), parentEClass);
			else
				this.canonicalMap.set(newSig, parentEClass);

			// Materialize small Cartesian products of child alternatives so parent combinations become explicit enodes.
			// This enables parent-level rules (e.g. mul-distribute) to fire on combinations of child alternatives.

			const orderedChildEcs: EClass[] = [];
			parent.visit({
				post: (child: symbolic, parent2?: symbolic) => {
					if (parent === parent2) {
						const ce = this.eclassOfNode(child.id);
						if (ce)
							orderedChildEcs.push(this.find(ce));
					}
					return child;
				}
			});
			
			// Update best nodes for each child eclass
			for (const ec of orderedChildEcs)
				this.updateBestNode(ec);
			
			// Strategy: Only expand combinations involving the best node from each eclass
			// This prevents combinatorial explosion while ensuring good candidates are tried
			const childCandidates: symbolic[][] = orderedChildEcs.map(ec => {
				const nodes = Array.from(ec.nodes);
				if (nodes.length === 1)
					return nodes;
				// For eclasses with multiple nodes, include the best one plus originals
				const best = ec.bestNode!;
				const originals = nodes.slice(0, Math.min(2, nodes.length)); // Keep first 2 as originals
				return originals.includes(best) ? originals : [best, ...originals];
			});
			
			const total = childCandidates.reduce((acc, c) => acc * c.length, 1);

			if (total > 1 && total < this.maxExpansions) {
				for (let j = 0; j < total; j++) {
					let i = j;
					const selection = childCandidates.map((candidates: symbolic[]) => {
						const mod = i % candidates.length;
						i = Math.floor(i / candidates.length);
						return candidates[mod];
					});

					i = 0;
					const concrete = parent.visit({
						pre: (n: symbolic, parent2?: symbolic) => parent === parent2 ? selection[i++] : n
					});

					if (this.validate && !this.validate(parent, concrete))
						throw new Error("Validation failed during merging expansion");

					const eclass = this.addSymbolic(concrete);
					this.mergeClasses(this.eclassOfNode(parent.id)!, eclass);
				}

			}
		}

		if (this.validate) {
			const repr = a.repr()!;
			for (const n of a.nodes) {
				if (!this.validate(n, repr))
					throw new Error("Validation failed during merge");
			}
		}

		return true;
	}

	addSymbolic(node: symbolic): EClass {
		const existing = this.eclassOfNode(node.id);
		if (existing)
			return existing;

		// Post-order traversal: children are processed before parents so their eclasses exist when we handle a parent.
		// For each visited node we compute a canonical signature (by substituting immediate children with their current eclass reps), create or reuse an EClass
		node.visit({
			pre: (n: symbolic) => {
				if (this.eclassOfNode(n.id))
					return undefined;
				return n;
			},
			post: (n: symbolic, parent?: symbolic) => {
				// If this node already has an eclass (possibly created earlier), skip
				if (this.eclassOfNode(n.id))
					return undefined;

				if (this.verbose)
					console.log('egraph-diagnostic: adding node', String(n));

				const key = this.computeCanonicalSig(n);

				// if there's already a canonical representative for this key, reuse it
				let eclass: EClass;
				const existing = this.canonicalMap.get(key);
				if (existing) {
					eclass = this.find(existing);
				} else {
					eclass = new EClass(`ec:${this.canonicalMap.size}`, new Set);
					this.canonicalMap.set(key, eclass);
				}
				eclass.nodes.add(n);
				this.enodeMap.set(n.id, eclass);

				// attach this eclass as a child of its parent (if any)
				if (parent)
					eclass.parents.add(parent);
				return;
			}
		});

		const out = this.eclassOfNode(node.id);
		if (!out)
			throw new Error(`addSymbolic: failed to register node ${node.id}`);
		return out;
	}

	applyRules(rules: Rule[], debugNode?: string): boolean {
		const keys = Array.from(this.enodeMap.keys());
		let changed = false;
		for (const nid of keys) {
			const node = symbolic.getById(nid)! as symbolic;

			for (const r of rules) {
				const bs = r.match(node, {exact: true});

				if (!bs || (r.guard && !r.guard(bs))) {
					if (debugNode === 'all' || debugNode === nid)
						console.warn('egraph-diagnostic: rule', r.name, 'on', String(node), bs ? "matched, but guard failed" : "no match");
					continue;
				}

				const rep = r.replace(bs);
				if (this.verbose || debugNode === 'all' || debugNode === 'replace' || debugNode === nid)
					console.log('egraph-diagnostic: rule', r.name, 'on', String(node), 'produced', rep ? String(rep) : '<none>');

				if (this.validate && !this.validate(node, rep))
					throw new Error(`EGraph validation failed for rule ${r.name} on node ${String(node)} producing ${String(rep)}`);

				const eclass	= this.eclassOfNode(nid)!;
				const existing	= this.addSymbolic(rep);
				const changed1	= this.mergeClasses(eclass, existing);

				if (!this.verbose && changed1 && (debugNode === 'changed' || debugNode === 'all' || debugNode === nid))
					console.log('egraph-diagnostic: merged', String(eclass), 'with', String(existing));

				changed ||= changed1;
			}
		}
		return changed;
	}

	// Extract best representative from the equivalence class of nid using iterative DP and scorer
	extractBest(nid: string, scorer: (n: symbolic) => number, maxIters = 50): symbolic {
		const root = this.eclassOfNode(nid);
		if (!root)
			return symbolic.getById(nid)! as symbolic;

		// Update the scorer in case a custom one was provided
		this.scorer = scorer;

		// Build deduplicated array of canonical representative roots
		const seenRoots = new Set<EClass>();
		for (const ec of this.canonicalMap.values())
			seenRoots.add(this.find(ec));
		const roots: EClass[] = Array.from(seenRoots);

		// Update best nodes for all eclasses
		for (const ec of roots)
			this.updateBestNode(ec);

		if (this.verbose) {
			console.warn('egraph-diagnostic: reps=', roots.map(r => r.id));
			for (const ec of roots)
				console.warn({rep: ec.id, bestCost: ec.bestScore, bestNode: String(ec.bestNode), nodes: Array.from(ec.nodes).map(String)});
		}

		// Iterative stabilization: rebuild candidates by substituting children with best representatives
		for (let iter = 0, changed = true; changed && iter <= maxIters; iter++) {
			changed = false;
			for (const ec of roots) {
				for (const node of ec.nodes) {
					const candidate = node.visit({pre: (n: symbolic) => {
						if (n === node)
							return n;
						const ce = this.eclassOfNode(n.id);
						return ce?.bestNode ?? n;
					}});

					const cost = scorer(candidate);
					if (cost < ec.bestScore!) {
						if (this.verbose)
							console.warn('egraph-diagnostic: updating best for', ec.id, 'from', ec.bestScore, 'to', cost, 'node:', String(candidate));
						ec.bestNode = candidate;
						ec.bestScore = cost;
						changed = true;
					}
				}
			}
		}

		if (this.verbose) {
			console.warn('egraph-diagnostic: final best choices:');
			for (const ec of roots)
				console.warn('  rep=', ec.id, 'cost=', ec.bestScore, 'node=', String(ec.bestNode));
		}
		
		return root.bestNode!;
	}
}

export interface EGraphOptions {
	scorer?:		(n: symbolic) => number,
	validate?:		(node: symbolic, rep: symbolic) => boolean,
	maxRounds?:		number,
	maxExpansions?: number,
	verbose?:		boolean,
	debugNode?: 	string,
} 

export function applyRulesEgraph(node: symbolic, rules: Rule[], opts?: EGraphOptions): symbolic {
	const egraph = new EGraph(opts?.verbose ?? false, opts?.maxExpansions ?? 128);

	egraph.validate = opts?.validate;

	egraph.addSymbolic(node);

	const rounds = opts?.maxRounds ?? 6;
	for (let i = 0; i < rounds; i++) {
		if (opts?.verbose)
			console.warn('egraph-diagnostic: round', i + 1);
		if (!egraph.applyRules(rules, opts?.debugNode))
			break;
	}

	return egraph.extractBest(node.id, opts?.scorer ?? scoreFactory());
}
