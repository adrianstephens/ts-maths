import type {ops} from './core';

// A lightweight symbolic expression system and a generic ops<T> implementation.
// Intended to be used as ops<Expr>() but exported as generic for compatibility.

export type Expr =
	| { kind: "const"; value: number }
	| { kind: "var"; name: string }
	| { kind: "neg"; child: Expr }
	| { kind: "add"; left: Expr; right: Expr }
	| { kind: "sub"; left: Expr; right: Expr }
	| { kind: "mul"; left: Expr; right: Expr }
	| { kind: "div"; left: Expr; right: Expr };


function isConst(e: Expr): e is { kind: "const"; value: number } {
	return e.kind === "const";
}
function isZero(e: Expr) {
	return isConst(e) && e.value === 0;
}
function isOne(e: Expr) {
	return isConst(e) && e.value === 1;
}

function makeConst(n: number): Expr {
	return { kind: "const", value: n };
}
function makeVar(name: string): Expr {
	return { kind: "var", name };
}

function deepEqual(a: Expr, b: Expr): boolean {
	if (a.kind !== b.kind)
		return false;
	switch (a.kind) {
		case "const":
			return (b as any).value === a.value;
		case "var":
			return (b as any).name === a.name;
		case "neg":
			return deepEqual(a.child, (b as any).child);
		case "add":
		case "sub":
		case "mul":
		case "div":
			return deepEqual(a.left, (b as any).left) && deepEqual(a.right, (b as any).right);
	}
}

function stringify(e: Expr): string {
	switch (e.kind) {
		case "const":
			return String(e.value);
		case "var":
			return e.name;
		case "neg": {
			const c = e.child;
			const s = c.kind === "const" || c.kind === "var" ? stringify(c) : `(${stringify(c)})`;
			return `-${s}`;
		}
		case "add":
			return `(${stringify(e.left)} + ${stringify(e.right)})`;
		case "sub":
			return `(${stringify(e.left)} - ${stringify(e.right)})`;
		case "mul":
			return `(${stringify(e.left)} * ${stringify(e.right)})`;
		case "div":
			return `(${stringify(e.left)} / ${stringify(e.right)})`;
	}
}

function evalExpr(e: Expr, env: Record<string, number> = {}): number {
	switch (e.kind) {
		case "const":
			return e.value;
		case "var":
			return env[e.name] ?? NaN;
		case "neg":
			return -evalExpr(e.child, env);
		case "add":
			return evalExpr(e.left, env) + evalExpr(e.right, env);
		case "sub":
			return evalExpr(e.left, env) - evalExpr(e.right, env);
		case "mul":
			return evalExpr(e.left, env) * evalExpr(e.right, env);
		case "div":
			return evalExpr(e.left, env) / evalExpr(e.right, env);
	}
}

function simplifyOnce(e: Expr): Expr {
	// constant folding and basic identities
	switch (e.kind) {
		case "const":
		case "var":
			return e;
		case "neg": {
			const c = simplifyOnce(e.child);
			return	c.kind === "const" ? makeConst(-c.value)
				:	c.kind === "neg" ? c.child
				:	{ kind: "neg", child: c };
		}
		case "add": {
			const L = simplifyOnce(e.left);
			const R = simplifyOnce(e.right);
			return isConst(L) && isConst(R) ? makeConst(L.value + R.value) : isZero(L) ? R : isZero(R) ? L : { kind: "add", left: L, right: R };
		}
		case "sub": {
			const L = simplifyOnce(e.left);
			const R = simplifyOnce(e.right);
			return isConst(L) && isConst(R) ? makeConst(L.value - R.value)
				:	isZero(R) ? L
				:	deepEqual(L, R) ? makeConst(0)
				:	{ kind: "sub", left: L, right: R };
		}
		case "mul": {
			const L = simplifyOnce(e.left);
			const R = simplifyOnce(e.right);
			return 	isConst(L) && isConst(R) ? makeConst(L.value * R.value)
			 	:	isZero(L) || isZero(R) ? makeConst(0)
			  	:	isOne(L) ? R
			   	:	isOne(R) ? L
			    :	{ kind: "mul", left: L, right: R };
		}
		case "div": {
			const L = simplifyOnce(e.left);
			const R = simplifyOnce(e.right);
			return	isConst(L) && isConst(R) ? makeConst(L.value / R.value)
				:	isZero(L) ? makeConst(0)
				:	isOne(R) ? L
				:	deepEqual(L, R) ? makeConst(1)
				:	{ kind: "div", left: L, right: R };
		}
	}
}

function simplifyFixedPoint(e: Expr): Expr {
	let prev: string | null = null;
	let cur = e;
	while (true) {
		const s = stringify(cur);
		if (s === prev)
			return cur;
		prev = s;
		cur = simplifyOnce(cur);
	}
}

function derivativeExpr(e: Expr, v: string): Expr {
	switch (e.kind) {
		case "const":
			return makeConst(0);
		case "var":
			return makeConst(e.name === v ? 1 : 0);
		case "neg":
			return { kind: "neg", child: derivativeExpr(e.child, v) };
		case "add":
			return { kind: "add", left: derivativeExpr(e.left, v), right: derivativeExpr(e.right, v) };
		case "sub":
			return { kind: "sub", left: derivativeExpr(e.left, v), right: derivativeExpr(e.right, v) };
		case "mul":
			// product rule: (u*v)' = u'*v + u*v'
			return {
				kind: "add",
				left: { kind: "mul", left: derivativeExpr(e.left, v), right: e.right },
				right: { kind: "mul", left: e.left, right: derivativeExpr(e.right, v) },
			};
		case "div":
			// quotient: (u/v)' = (u'*v - u*v') / v^2
			return {
				kind: "div",
				left: {
					kind: "sub",
					left: { kind: "mul", left: derivativeExpr(e.left, v), right: e.right },
					right: { kind: "mul", left: e.left, right: derivativeExpr(e.right, v) },
				},
				right: { kind: "mul", left: e.right, right: e.right },
			};
	}
}

// ---------------------------------------------------------------------------
// Instance-style adapter compatible with `ops<C>` from core.ts
// ---------------------------------------------------------------------------

// ExprNode implements the instance-based ops<C> interface so symbolic
// expressions can be used where code expects objects with methods like
// `neg(), add(), mul()` etc.
export class ExprNode implements ops<ExprNode> {
	constructor(public expr: Expr) {}

	// instance-style ops
	dup(): ExprNode { return new ExprNode(this.expr); }
	neg(): ExprNode { return new ExprNode({ kind: 'neg', child: this.expr }); }
	scale(b: number): ExprNode { return new ExprNode({ kind: 'mul', left: makeConst(b), right: this.expr }); }
	mul(b: ExprNode): ExprNode { return new ExprNode({ kind: 'mul', left: this.expr, right: b.expr }); }
	div(b: ExprNode): ExprNode { return new ExprNode({ kind: 'div', left: this.expr, right: b.expr }); }
	add(b: ExprNode): ExprNode { return new ExprNode({ kind: 'add', left: this.expr, right: b.expr }); }
	sub(b: ExprNode): ExprNode { return new ExprNode({ kind: 'sub', left: this.expr, right: b.expr }); }
	// For symbolic expressions mag returns a numeric evaluation when constant, otherwise NaN
	mag(): number { const v = evalExpr(this.expr); return Number.isFinite(v) ? Math.abs(v) : NaN; }

	// convenience
	toString() { return stringify(this.expr); }
}

export function exprFromNumber(n: number) { return new ExprNode(makeConst(n)); }
export function exprVariable(name: string) { return new ExprNode(makeVar(name)); }

// adapter: turn a functional Ops<T> implementation (the one above) into
// a factory producing ExprNode instances — useful when callers expect
// the `ops<C>` instance-style API on their element type.
export function makeExprOpsAdapter() {
	// Use the functional ops<Expr> implementation under the hood and
	// wrap/unwrap ExprNode instances so callers can use the instance-style
	// `Ops<ExprNode>` API (zero/one/fromNumber/variable/add/.. etc).
	const base = {
		zero: makeConst(0),
		one: makeConst(1),
		fromNumber(n: number) {
			return makeConst(n);
		},
		variable(name: string) {
			return makeVar(name);
		},
		add(a: Expr, b: Expr): Expr {
			return { kind: "add", left: a, right: b };
		},
		sub(a: Expr, b: Expr): Expr {
			return { kind: "sub", left: a, right: b };
		},
		mul(a: Expr, b: Expr): Expr {
			return { kind: "mul", left: a, right: b };
		},
		div(a: Expr, b: Expr): Expr {
			return { kind: "div", left: a, right: b };
		},
		neg(a: Expr): Expr {
			return { kind: "neg", child: a };
		},
		eq(a: Expr, b: Expr): boolean {
			const sa = simplifyFixedPoint(a);
			const sb = simplifyFixedPoint(b);
			return deepEqual(sa, sb);
		},
		simplify(a: Expr): Expr {
			return simplifyFixedPoint(a);
		},
		derivative(a: Expr, v: string): Expr {
			return simplifyFixedPoint(derivativeExpr(a, v));
		},
		toString(a: Expr): string {
			return stringify(a);
		},
		evaluate(a: Expr, env = {}) {
			return evalExpr(a, env);
		},
	};

	return {
		zero: new ExprNode(makeConst(0)),
		one: new ExprNode(makeConst(1)),
		fromNumber(n: number) { return new ExprNode(makeConst(n)); },
		variable(name: string) { return new ExprNode(makeVar(name)); },
		add(a: ExprNode, b: ExprNode) { return new ExprNode(base.add(a.expr, b.expr)); },
		sub(a: ExprNode, b: ExprNode) { return new ExprNode(base.sub(a.expr, b.expr)); },
		mul(a: ExprNode, b: ExprNode) { return new ExprNode(base.mul(a.expr, b.expr)); },
		div(a: ExprNode, b: ExprNode) { return new ExprNode(base.div(a.expr, b.expr)); },
		neg(a: ExprNode) { return new ExprNode(base.neg(a.expr)); },
		eq(a: ExprNode, b: ExprNode) { return base.eq(a.expr, b.expr); },
		simplify(a: ExprNode) { return new ExprNode(base.simplify(a.expr)); },
		derivative(a: ExprNode, v: string) { return new ExprNode(base.derivative(a.expr, v)); },
		toString(a: ExprNode) { return base.toString(a.expr); },
		evaluate(a: ExprNode, env?: Record<string, number>) { return base.evaluate(a.expr, env); },
	};
}