import {symbolic, Rule, scoreFactory, StringifyOptions} from './symbolic';

class EClass extends symbolic {
	parent?: EClass;
	parents = new Set<string>();
	constructor(id: string, public nodes: Set<symbolic>) { super(id); }
	repr(): symbolic { const it = this.nodes.values(); const first = it.next(); return first.done ? symbolic.from(NaN) : first.value; }
	toString(_opts?: StringifyOptions): string {
		return `EClass ${this.repr()}`;
	}
}

export default class EGraph {
	private enodeMap		= new Map<string, EClass>();
	private canonicalMap	= new Map<string, EClass>();
	private childMap 		= new Map<string, EClass[]>();

	constructor(public verbose = false, public debugNode = '') {
	}

	// return a symbolic-like view for a stored enode id.
	// If the id refers to an EClass we return the class representative; otherwise fall back to the normal interned symbolic.
	private getNodeView(nid: string): symbolic | undefined {
		const e = this.enodeMap.get(nid);
		if (e)
			return this.find(e).repr();
		return symbolic.getById(nid);
	}

	private computeCanonicalSig(node: symbolic): string {
		const mapped = node.visit({pre: (n: symbolic) => this.eclassOfNode(n.id) ?? n});
		return mapped.id;
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

	// merge with congruence-closure worklist processing will be done by mergeClasses
	private mergeClasses(a: EClass, b: EClass): boolean {
		if (a === b)
			return false;

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

		for (const pid of Array.from(a.parents)) {
			const node			= this.getNodeView(pid)!;
			const parentEClass	= this.eclassOfNode(pid)!;

			// recompute canonical signature by using stored childMap and current reps
			const rawChildren	= this.childMap.get(pid) ?? [];
			const newSig		= this.computeCanonicalSig(node);

			// update stored childMap to point to the current representative EClass objects
			this.childMap.set(pid, rawChildren.map(ec => this.find(ec)));

			// if canonicalMap already maps newSig to some EClass, union their classes
			const existingEClass = this.canonicalMap.get(newSig);
			if (existingEClass)
				this.mergeClasses(this.find(existingEClass), parentEClass);
			else
				this.canonicalMap.set(newSig, parentEClass);

			// Materialize small Cartesian products of child alternatives so parent combinations become explicit enodes.
			// This enables parent-level rules (e.g. mul-distribute) to fire on combinations of child alternatives.
			// Limit the total number to avoid explosion.
			const MAX_COMBINATIONS = 128;
			// build candidate lists for each child (list of concrete symbolic nodes)
			const childCandidates: symbolic[][] = rawChildren.map(ec => Array.from(this.find(ec).nodes));

			const total = childCandidates.reduce((acc, c) => acc * Math.max(1, c.length), 1);
			if (total <= MAX_COMBINATIONS && total > 1) {
				// enumerate combos
				for (let j = 0; j < total; j++) {
					let i = j;
					const selection = childCandidates.map((candidates: symbolic[]) => {
						const mod = i % candidates.length;
						i = Math.floor(i / candidates.length);
						return candidates[mod] ?? symbolic.from(NaN);
					});

					i = 0;
					const concrete = node.visit({
						pre: (n: symbolic, parent?: symbolic) => parent === node ? selection[i++] : n
					});

					const other = this.addSymbolic(concrete);
					const ec1 = this.eclassOfNode(pid);
					if (ec1)
						this.mergeClasses(ec1, other);
				}

			}
		}

		return true;
	}

	addSymbolic(node: symbolic): EClass {
		const existing = this.eclassOfNode(node.id);
		if (existing)
			return existing;

		// Post-order traversal: children are processed before parents so their eclasses exist when we handle a parent.
		// For each visited node we compute a canonical signature (by substituting immediate children with their current eclass reps), create or reuse an EClass, and record the parent->child relationship in childMap.
		node.visit({
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
					eclass = new EClass(`ec:${n.id}`, new Set);
					// store the root EClass for this canonical signature
					this.canonicalMap.set(key, eclass);
				}
				eclass.nodes.add(n);
				this.enodeMap.set(n.id, eclass);

				// attach this eclass as a child of its parent (if any)
				if (parent) {
					let arr = this.childMap.get(parent.id);
					if (!arr) {
						arr = [];
						this.childMap.set(parent.id, arr);
					}
					arr.push(eclass);
					eclass.parents.add(parent.id);
				}
				return;
			}
		});

		// After traversal, return the eclass for the original node id
		const out = this.eclassOfNode(node.id);
		if (!out)
			throw new Error(`addSymbolic: failed to register node ${node.id}`);
		return out;
	}

	fixup(): void {
		// Rebuild canonical signature map and use-lists from current enodeMap
		this.canonicalMap.clear();
		// clear parent sets on all EClasses
		for (const e of this.enodeMap.values())
			e.parents.clear();

		for (const nid of Array.from(this.enodeMap.keys())) {
			const node = this.getNodeView(nid)!;

			// Compute canonical signature using current child eclass representatives
			const sig = this.computeCanonicalSig(node);

			// if there's a collision, union classes
			const nodeEClass = this.eclassOfNode(nid);
			if (nodeEClass) {
				const existingEClass = this.canonicalMap.get(sig);
				if (existingEClass)
					this.mergeClasses(this.find(existingEClass), nodeEClass);
				else
					this.canonicalMap.set(sig, nodeEClass);
			}

			// build use-lists: discover immediate children via a visitor so we don't need to branch on node kinds.
			// For every visited child whose parent is the root node, add this `nid` as a parent of the child's eclass.
			node.visit({
				post: (child: symbolic, parent?: symbolic) => {
					if (parent === node) {
						const ce = this.eclassOfNode(child.id);
						if (ce)
							ce.parents.add(nid);
					}
					return child;
				}
			});
		}
	}

	private runOneRound(rules: Rule[]): boolean {
		const keys = Array.from(this.enodeMap.keys());
		let changed = false;
		for (const nid of keys) {
			const node = symbolic.getById(nid)!;//this.getNodeView(nid)!;

			for (const r of rules) {
				const bs = r.match(node);

				if (!bs || (r.guard && !r.guard(bs))) {
					if (this.debugNode === 'all' || node.id === this.debugNode)
						console.warn('egraph-diagnostic: rule', r.name, 'on', String(node), bs ? "matched, but guard failed" : "no match");
					continue;
				}

				const rep = r.replace(bs);

				const eclass	= this.eclassOfNode(nid);
				const existing	= this.eclassOfNode(rep.id) ?? this.addSymbolic(rep);
				const changed1	= this.mergeClasses(eclass!, existing);

				if ((changed1 && this.debugNode === 'replace') || this.verbose || this.debugNode === 'all' || node.id === this.debugNode)
					console.log('egraph-diagnostic: rule', r.name, 'on', String(node), 'produced', rep ? String(rep) : '<none>');

				changed ||= changed1;
			}
		}
		return changed;
	}

	applyRules(rules: Rule[], rounds: number): void {
		for (let i = 0; i < rounds; i++) {
			if (!this.runOneRound(rules))
				break;
			this.fixup();
		}
	}

	// Extract best representative from the equivalence class of nid using iterative DP and scorer
	extractBest(nid: string, scorer: (n: symbolic) => number, maxIters = 50): symbolic {
		// Determine root representative early for targeted diagnostics
		const root		= this.eclassOfNode(nid);
		if (!root)
			return this.getNodeView(nid) || symbolic.from(NaN);

		// Build a deduplicated array of canonical representative roots using canonicalMap.
		const seenRoots = new Set<EClass>();
		for (const ec of this.canonicalMap.values())
			seenRoots.add(this.find(ec));
		const roots: EClass[] = Array.from(seenRoots);

		// best: map from representative eclass id -> {cost,node}
		const best = new Map<EClass, {cost: number, node: symbolic}>();

		// initialize: use the class representative (repr) as the initial sample
		for (const ec of roots) {
			const sample = ec.repr();
			best.set(ec, sample ? {cost: scorer(sample), node: sample} : {cost: Infinity, node: symbolic.from(NaN)});
		}

		// diagnostic: print initial best per rep
		if (this.verbose) {
			console.warn('egraph-diagnostic: reps=', roots.map(r => r.id));
			for (const ec of roots) {
				const b = best.get(ec);
				console.warn({rep: ec.id, initCost: b?.cost, nodes: Array.from(ec.nodes).map(String)});
			}
		}

		// Iterative stabilization: repeatedly rebuild candidates by substituting each node's children with the current best representatives until no improvements are found or maxIters is reached
		for (let iter = 0, changed = true; changed && iter <= maxIters; iter++) {
			changed = false;
			for (const ec of roots) {
				for (const node of ec.nodes) {

					const candidate	= node.visit({pre: (n: symbolic) => {
						if (n === node)
							return n;
						const ce = this.eclassOfNode(n.id);
						if (!ce)
							return n;
						const be = best.get(ce);
						if (this.verbose && be && n !== be.node)
							console.warn('egraph-diagnostic: pre-replace child', String(n), 'with', String(be.node));
						return be ? be.node : n;
					}});

					const cost		= scorer(candidate);
					const cur		= best.get(ec)!;

					if (this.verbose)
						console.warn('egraph-diagnostic: eval root rep', ec.id, 'node=', String(node), 'candidate=', String(candidate), 'cost=', cost, 'curCost=', cur.cost);

					if (cost < cur.cost) {
						if (this.verbose)
							console.warn('egraph-diagnostic: updating best for root rep', ec.id, 'node=', String(node), 'candidate=', String(candidate), 'cost=', cost, 'old=', cur.cost);
						best.set(ec, {cost, node: candidate});
						changed = true;
					}
				}
			}
		}


		if (this.verbose) {
			console.warn('egraph-diagnostic: final best choices:');
			for (const [k, v] of best.entries())
				console.warn('  rep=', k, ' cost=', v.cost, ' chosenId=', v.node.id, ' repr=', String(v.node));
		}
		const out = best.get(root);
		return out ? out.node : root.repr();
	}
}

export interface EGraphOptions {
	verbose?: boolean,
	debugNode?: string,
	scorer?: (n: symbolic) => number,
	rounds?: number,
} 

export function applyRulesEgraph(node: symbolic, rules: Rule[], opts?: EGraphOptions): symbolic {
	const egraph = new EGraph(opts?.verbose ?? false, opts?.debugNode ?? '');

	egraph.addSymbolic(node);
	egraph.applyRules(rules, opts?.rounds ?? 6);

	// disable debug-all to avoid noisy extract diagnostics during extraction
	egraph.debugNode = '';

	// normalize and extract the best representative using the scorer
	return egraph.extractBest(node.id, opts?.scorer ?? scoreFactory());
}
