import {symbolic, Rule, Bindings} from './symbolic';

// Minimal e-graph: tiny, well-formed, and safe to compile. We'll extend functionality once this baseline is verified.

class EClass extends symbolic {
	parent?: EClass;
	references = new Set<symbolic>();

	constructor(id: string, public nodes: Set<symbolic>) {
		super(id);
		//symbolic.interner.set(id, this);
	}

}

export default class EGraph {
	private enodeMap = new Map<string, EClass>();

	private find(e: EClass): EClass {
		if (!e.parent)
			return e;
		const r = this.find(e.parent);
		e.parent = r;
		return r;
	}

	private union(a: EClass, b: EClass): EClass {
		// merge smaller class into larger class (by node count)
		if (a.nodes.size < b.nodes.size)
			[a, b] = [b, a];

		// attach source to target and move nodes
		b.parent = a;
		for (const n of b.nodes)
			a.nodes.add(n);

		for (const n of b.references)
			a.references.add(n);

		b.nodes.clear();
		b.references.clear();
		return a;
	}

	private eclassOfNode(nid: string): EClass | undefined {
		const e = this.enodeMap.get(nid);
		if (!e || !e.parent)
			return e;
		const e2 = this.find(e);
		this.enodeMap.set(nid, e2);
		return e2;
	}

	addSymbolic(node: symbolic): EClass {
		const nid		= node.id;
		const existing	= this.eclassOfNode(nid);
		if (existing)
			return existing;

		const eclass	= new EClass(`ec:${nid}`, new Set([node]));
		this.enodeMap.set(nid, eclass);
		return eclass;
	}

	runOneRound(rules: Rule[]): boolean {
		const compare = (a: symbolic, b: symbolic, bindings: Bindings) => {
			if (b instanceof EClass) {
				for (const b2 of b.nodes) {
					if (b2 instanceof EClass) {
						if (compare(a, b2, bindings))
							return bindings;
					} else if (a.match(b2, compare, bindings)) {
						return bindings;
					}
				}
				return null;
			}
			return a === b ? bindings : null;
		};

		let changed = false;
		const keys = Array.from(this.enodeMap.keys());
		for (const nid of keys) {
			const node = symbolic.getById(nid);
			if (!node)
				continue;

			for (const r of rules) {
				const bs = r.match(node, compare);
				if (!bs)
					continue;
				if (r.guard && !r.guard(bs))
					continue;

				const rep = r.replace(bs);
				if (rep) {
					const eclass = this.eclassOfNode(nid);
					if (!eclass)
						continue;
					const existing = this.eclassOfNode(rep.id);
					if (existing) {
						// only mark as changed if the union actually merges two different e-classes
						if (eclass !== existing) {
							this.union(eclass, existing);
							changed = true;
						}
					} else {
						eclass.nodes.add(node);
						this.enodeMap.set(rep.id, eclass);
						changed = true;
					}

					//const other = this.addSymbolic(rep);
					//if (this.find(eclass) !== this.find(other)) {
					//	this.union(eclass, other);
					//	changed = true;
					//}
				}
			}
		}
		return changed;
	}
	fixup(): void {
	}

	applyRules(rules: Rule[], rounds = 6): void {
		for (let i = 0; i < rounds; i++) {
			if (!this.runOneRound(rules))
				break;
			this.fixup();
		}
	}

	equivalenceSet(nid: string): symbolic[] {
		const e = this.eclassOfNode(nid);
		return e ? Array.from(e.nodes) : [];
	}

}

export function applyRulesEgraph(node: symbolic, rules: Rule[], scorer: (n: symbolic) => number): symbolic {
	const egraph = new EGraph();

	egraph.addSymbolic(node);
	const enode = node.visit(node => egraph.addSymbolic(node));
	const es0 = egraph.equivalenceSet(node.id);

	egraph.applyRules(rules);

	const es = egraph.equivalenceSet(node.id);
	let bestNode = node;
	if (es.length > 1) {
		let bestCost = Infinity;
		for (const i of es) {
			const cost = scorer(i);
			if (cost <= bestCost) {
				bestCost = cost;
				bestNode = i;
			}
		}
	}
	return bestNode;
}

/*
	fixup(): void {
		const enodeMap = new Map<string, EClass>();
		let eclasses = 0;

		function addSymbolic(node: symbolic): EClass {
			const nid		= node.id;
			const existing	= enodeMap.get(nid);
			if (existing)
				return existing;

			const eclass	= new EClass(`ec:${nid}`, new Set([node]));
			enodeMap.set(nid, eclass);
			++eclasses;
			return eclass;
		}

		for (const [nid, _eclass] of this.enodeMap) {
			const node = symbolic.getById(nid);
			if (!node)
				continue;
			
			addSymbolic(node);
			node.visit(node => {
				if (!(node instanceof EClass))
					return addSymbolic(node);
				return node;
			});
		}
		this.enodeMap = enodeMap;
	}
		*/
