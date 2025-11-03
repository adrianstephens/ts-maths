import type {ops} from './core';

interface addend {
	neg: boolean;
	item: Expr;
}

interface factor {
	pow: number;
	item: Expr;
}

type Expr =
	| { kind: "const"; value: number }
	| { kind: "var"; name: string }
	| { kind: "add"; addends: addend[] }
	| { kind: "mul"; factors: factor[] }


function isConst(e: Expr): e is { kind: "const"; value: number } {
	return e.kind === "const";
}
function isZero(e: Expr) {
	return isConst(e) && e.value === 0;
}
function isOne(e: Expr) {
	return isConst(e) && e.value === 1;
}

function constant(value: number): Expr { return { kind: 'const', value }; }
function variable(name: string): Expr { return { kind: "var", name }; }

function addends(a: Expr) {
	return a.kind === 'add' ? a.addends : [{ neg: false, item: a }];
}
function factors(a: Expr) {
	return a.kind === 'mul' ? a.factors : [{ pow: 1, item: a }];
}

function neg(a: Expr): Expr {
	return isConst(a) ? constant(-a.value)
	: { kind: 'add', addends: addends(a).map(i => ({ neg: !i.neg, item: i.item })) };
}

function recip(a: Expr): Expr {
	return isConst(a) ? constant(1 / a.value)
		: { kind: 'mul', factors: factors(a).map(i => ({ pow: -i.pow, item: i.item })) };
}

function pow(a: Expr, b: number): Expr {
	return b === 1 ? a
		: b === 0 ? constant(1)
		: isConst(a) ? constant(a.value ** b)
		: { kind: 'mul', factors: factors(a).map(i => ({ pow: i.pow * b, item: i.item })) };
}

function addConst(a: number, b: addend[]): Expr {
	const i = b.findIndex(f => isConst(f.item));
	if (i >= 0) {
		const fb = b[i].item as { kind: "const"; value: number };
		return { kind: 'add', addends: [ ...b.slice(0, i), ...b.slice(i + 1), { neg: false, item: constant(a * fb.value) }] };
	}
	return { kind: 'add', addends: [ ...b, { neg: false, item: constant(a) }] };
}

function add(...v: Expr[]): Expr {
	let a = v[0];
	for (let j = 1; j < v.length; ++j) {
		const b = v[j];
		a = isZero(a) ? b
			: isZero(b) ? a
			: isConst(a) && isConst(b) ? constant(a.value * b.value)
			: isConst(a) ? addConst(a.value, addends(b))
			: isConst(b) ? addConst(b.value, addends(a))
			: { kind: 'add', addends: [...addends(a), ...addends(b)] };
	}
	return a;
}

function sub(a: Expr, b: Expr): Expr {
	return add(a, neg(b));
}

function mulConst(a: number, b: factor[]): Expr {
	const i = b.findIndex(f => isConst(f.item));
	if (i >= 0) {
		const fb = b[i].item as { kind: "const"; value: number };
		return { kind: 'mul', factors: [{ pow: 1, item: constant(a * fb.value) }, ...b.slice(0, i), ...b.slice(i + 1)] };
	}
	return { kind: 'mul', factors: [{ pow: 1, item: constant(a) }, ...b] };
}

function mul(...v: Expr[]): Expr {
	let a = v[0];
	for (let j = 1; j < v.length; ++j) {
		const b = v[j];
		a =isOne(a) ? b
		: isOne(b) ? a
		: isZero(a) || isZero(b) ? constant(0)
		: isConst(a) && isConst(b) ? constant(a.value * b.value)
		: isConst(a) ? mulConst(a.value, factors(b))
		: isConst(b) ? mulConst(b.value, factors(a))
		: { kind: 'mul', factors: [...factors(a), ...factors(b)] };
	}
	return a;
}
function div(a: Expr, b: Expr): Expr {
	return mul(a, recip(b));
}

function deepEqual(a: Expr, b: Expr): boolean {
	if (a.kind !== b.kind)
		return false;
	switch (a.kind) {
		case "const":
			return (b as any).value === a.value;
		case "var":
			return (b as any).name === a.name;
		case "add": {
			const addendsb = (b as any).addends as addend[];
			return a.addends.length === addendsb.length && a.addends.every((v, i) => v.neg === addendsb[i].neg && deepEqual(v.item, addendsb[i].item));
		}
		case "mul": {
			const factorsb = (b as any).factors as factor[];
			return a.factors.length === factorsb.length && a.factors.every((v, i) => v.pow === factorsb[i].pow && deepEqual(v.item, factorsb[i].item));
		}
	}
}

function stringify(e: Expr): string {
	switch (e.kind) {
		case "const":
			return String(e.value);
		case "var":
			return e.name;
		case "add":
			return `(${e.addends.map((i, j) => (j === 0 ? (i.neg ? '-' : '') : i.neg ? ' - ' : ' + ') + stringify(i.item)).join('')})`;
		case "mul":
			return `(${e.factors.map((i, j) => (j === 0 ? (i.pow < 0 ? '1 / ': '') : i.pow < 0 ? ' / ' : ' * ') + stringify(i.item) + (Math.abs(i.pow) !== 1 ? ` ^ ${Math.abs(i.pow)}` : '')).join('')})`;
	}
}

function evaluate(e: Expr, env: Record<string, number> = {}): number {
	switch (e.kind) {
		case "const":
			return e.value;
		case "var":
			return env[e.name] ?? NaN;
		case "add":
			return e.addends.reduce((acc, curr) => {
				const v = evaluate(curr.item, env);
				return acc + (curr.neg ? -v : v);
			}, 0);
		case "mul":
			return e.factors.reduce((acc, curr) => {
				const v = evaluate(curr.item, env);
				switch (curr.pow) {
					case 1:		return acc * v;
					case -1:	return acc / v;
					default:	return acc * v ** curr.pow;
				}
			}, 1);
	}
}

function derivativeExpr(e: Expr, v: string): Expr {
	switch (e.kind) {
		case "const":
			return constant(0);
		case "var":
			return constant(e.name === v ? 1 : 0);
		case "add":
			return add(...e.addends.map(i =>
				i.neg ? neg(derivativeExpr(i.item, v)) : derivativeExpr(i.item, v)
			));
		case "mul":
			return add(...e.factors.map(f => mul(
				constant(f.pow),
				pow(f.item, f.pow - 1),
				derivativeExpr(f.item, v),
				...e.factors.filter(g => g !== f).map(i => pow(i.item, i.pow))
			)));
	}
}

export class symbolic implements ops<symbolic> {
	constructor(public expr: Expr) {}

	static from(i: number)			{ return new symbolic(constant(i)); }
	static variable(name: string)	{ return new symbolic(variable(name)); }

	dup(): symbolic 			{ return new symbolic(this.expr); }
	neg(): symbolic 			{ return new symbolic(neg(this.expr)); }
	scale(b: number): symbolic	{ return new symbolic(mul(constant(b), this.expr)); }
	add(b: symbolic): symbolic	{ return new symbolic(add(this.expr, b.expr)); }
	sub(b: symbolic): symbolic	{ return new symbolic(sub(this.expr, b.expr)); }
	mul(b: symbolic): symbolic	{ return new symbolic(mul(this.expr, b.expr)); }
	div(b: symbolic): symbolic	{ return new symbolic(div(this.expr, b.expr)); }
	// For symbolic2 expressions mag returns a numeric evaluation when constant, otherwise NaN
	mag(): number				{ const v = evaluate(this.expr); return Number.isFinite(v) ? Math.abs(v) : NaN; }

	eq(b: symbolic)				{ return deepEqual(this.expr, b.expr); }
	derivative(v: string)		{ return new symbolic(derivativeExpr(this.expr, v)); }
	evaluate(env?: Record<string, number>) { return evaluate(this.expr, env); }

	toString() { return stringify(this.expr); }
}
