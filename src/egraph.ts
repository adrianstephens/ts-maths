/* eslint-disable no-restricted-syntax */

import {symbolic} from './symbolic';

// Minimal e-graph: tiny, well-formed, and safe to compile. We'll extend
// functionality once this baseline is verified.

export type Rule = {
	name: string; apply(egraph: EGraph, nodeId: string): symbolic | null;
};

export default class EGraph {
	private nextEClass		= 1;
	private nodeToEClass	= new Map<string, number>();
	private eclassNodes		= new Map<number, Set<string>>();
	private nodeOp			= new Map<string, string>();
	private nodeChildren	= new Map<string, string[]>();
	private parent			= new Map<number, number>();
	private rules: Rule[]	= [];

	private find(id: number): number {
		const p = this.parent.get(id) ?? id;
		if (p === id)
			return id;
		const r = this.find(p);
		this.parent.set(id, r);
		return r;
	}

	private union(a: number, b: number): number {
		a = this.find(a);
		b = this.find(b);
		if (a === b)
			return a;

		// merge smaller class into larger class
		const sa = this.eclassNodes.get(a) ?? new Set<string>();
		const sb = this.eclassNodes.get(b) ?? new Set<string>();
		const target = sa.size >= sb.size ? a : b;
		const source = target === a ? b : a;
		const tset = this.eclassNodes.get(target) ?? new Set<string>();
		const sset = this.eclassNodes.get(source) ?? new Set<string>();
		for (const n of sset) {
			tset.add(n);
			this.nodeToEClass.set(n, target);
		}
		this.eclassNodes.set(target, tset);
		this.eclassNodes.delete(source);
		this.parent.set(source, target);
		return target;
	}

	addSymbolic(node: symbolic): number {
		const nid		= node.id;
		const existing	= this.nodeToEClass.get(nid);
		if (existing !== undefined)
			return this.find(existing);

		const eid = this.nextEClass++;
		this.nodeToEClass.set(nid, eid);
		this.eclassNodes.set(eid, new Set([nid]));
		this.parent.set(eid, eid);

		const ctor = (node as any).constructor?.name ?? '';
		this.nodeOp.set(nid, ctor.replace(/^symbolic/, '').toLowerCase());
		const kids = this.getChildrenIds(node);
		this.nodeChildren.set(nid, kids);
		return this.find(eid);
	}

	// Infer child node ids from a `symbolic` instance using duck-typing.
	// This accommodates the common shapes used in `symbolic.ts` without
	// importing lots of internal classes: unary (.arg), binary (.arg1/.arg2),
	// additive (.terms -> term.item), multiplicative (.factors -> factor.item),
	// and other array-like containers.
	private getChildrenIds(node: symbolic): string[] {
		const n = node as any;
		const kids: string[] = [];
		if (n === undefined || n === null)
			return kids;

		// unary
		if (n.arg !== undefined && n.arg && n.arg.id !== undefined)
			kids.push(String(n.arg.id));

		// binary
		if (n.arg1 !== undefined && n.arg2 !== undefined && n.arg1 && n.arg2 && n.arg1.id !== undefined && n.arg2.id !== undefined) {
			kids.push(String(n.arg1.id));
			kids.push(String(n.arg2.id));
		}

		// additive: terms is an array of { item, coef }
		if (Array.isArray(n.terms)) {
			for (const t of n.terms) {
				if (t && t.item && t.item.id !== undefined)
					kids.push(String(t.item.id));
			}
		}

		// multiplicative: factors is an array of { item, pow }
		if (Array.isArray(n.factors)) {
			for (const f of n.factors) {
				if (f && f.item && f.item.id !== undefined)
					kids.push(String(f.item.id));
			}
		}

		// some binary-like nodes may expose argA/argB or left/right; handle common names
		if (n.left !== undefined && n.right !== undefined && n.left && n.right && n.left.id !== undefined && n.right.id !== undefined) {
			kids.push(String(n.left.id));
			kids.push(String(n.right.id));
		}

		// dedupe while preserving order
		const seen = new Set<string>();
		const out: string[] = [];
		for (const k of kids) {
			if (!seen.has(k)) {
				seen.add(k);
				out.push(k);
			}
		}
		return out;
	}

	registerRule(r: Rule): void {
		this.rules.push(r);
	}

	runOneRound(): boolean {
		const nodes = Array.from(this.nodeToEClass.keys());
		let changed = false;
		for (const nid of nodes) {
			for (const r of this.rules) {
				try {
					const rep = r.apply(this, nid);
					if (rep) {
						const rid = this.addSymbolic(rep);
						const lhs = this.nodeToEClass.get(nid);
						if (lhs !== undefined) {
							this.union(lhs, rid);
							changed = true;
						}
					}
				} catch {
					// ignore in minimal version
				}
			}
		}
		return changed;
	}

	saturate(rounds = 6): void {
		for (let i = 0; i < rounds; i++) {
			const c = this.runOneRound();
			if (!c)
				break;
		}
	}

	extract(eid: number): symbolic {
		const r		= this.find(eid);
		const set	= this.eclassNodes.get(r);
		if (!set || set.size === 0)
			return symbolic.from(0);

		const nid = set.values().next().value as string;
		const op = this.nodeOp.get(nid) ?? '';
		if (op === 'constant' || op === 'const')
			return symbolic.from(0);
		if (op === 'variable' || op === 'var')
			return symbolic.variable(nid);
		return symbolic.from(0);
	}

	eclassOfNode(nodeId: string): number|undefined {
		return this.nodeToEClass.get(nodeId);
	}
	nodeIds(): string[] {
		return Array.from(this.nodeToEClass.keys());
	}
}

export const sinSumRule: Rule = {
	name: 'sin-sum',
	apply(egraph, nid) {
		const op = egraph['nodeOp'].get(nid);
		if (op !== 'sin')
			return null;
		const kids = egraph['nodeChildren'].get(nid) || [];
		if (kids.length !== 1)
			return null;
		const arg = kids[0];
		const aop = egraph['nodeOp'].get(arg);
		if (aop !== 'add')
			return null;
		const parts = egraph['nodeChildren'].get(arg) || [];
		if (parts.length !== 2)
			return null;
		const L = egraph.extract(egraph.eclassOfNode(parts[0]) ?? 0);
		const R = egraph.extract(egraph.eclassOfNode(parts[1]) ?? 0);
		return symbolic.sin(L).mul(symbolic.cos(R)).add(symbolic.cos(L).mul(symbolic.sin(R)));
	}
};
