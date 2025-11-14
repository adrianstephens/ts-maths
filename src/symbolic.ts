/* eslint-disable no-restricted-syntax */
import { ops, scalar2, Operators, compare, isAlmostInteger, OperatorsBase } from './core';
import { toSuperscript } from './string';

// invariants:
// - all symbolic instances are interned and unique by id
// - symbolic instances are immutable
// - symbolic instances with same id are structurally equal
//	- Additive:
// 		- terms are sorted and combined - no duplicate terms
//		- terms are not constants (constant term is stored separately)
//		- each term has a constant multiplier = 1 (if not zero)
//		- multiplicative terms store their constant in the term *not* in the item
//
//	- Multiplicative:
// 		- factors are sorted and combined - no duplicate factors
//		- factors are not constants (constant factor is stored separately)

class Interner<T extends object> {
	private table = new Map<string, WeakRef<T>>();
	private finalizer: FinalizationRegistry<string>;

	constructor() {
		this.finalizer = new FinalizationRegistry((key: string) => {
			const ref = this.table.get(key);
			if (ref && !ref.deref())
				this.table.delete(key);
		});
	}

	intern(key: string, factory: (key: string) => T): T {
		return this.get(key) || this.set(key, factory(key));
	}

	get(key: string): T | undefined {
		return this.table.get(key)?.deref?.();
	}

	set(key: string, value: T): T {
		this.table.set(key, new WeakRef(value));
		this.finalizer.register(value, key);
		return value;
	}

	pruneDeadEntries(): void {
		for (const [k, ref] of this.table) {
			if (ref.deref?.() === undefined)
				this.table.delete(k);
		}
	}
}

export type Bindings = Record<string, symbolic>;

export interface Visitor {
	pre?:	(node: symbolic, parent?: symbolic) => symbolic | undefined;
	post?:	(node: symbolic, parent?: symbolic) => symbolic | undefined;
}

export type Rule = {
	name:		string;
	match:		(node: symbolic) => Bindings | null;
	replace:	(bs: Bindings) => symbolic;
	guard?: 	(bs: Bindings) => boolean;
};

export type MatchOptions = {
	exact?: boolean;
	depth?: number;
};

export type ExpandOptions = {
	depth?: number;
	maxPow?: number;
	limit?: 'small' | 'polynomial';
};

export type StringifyOptions = {
	//parentheses?: 'all' | 'minimal' | 'none';
	fromMul?: boolean;
	superPower?: boolean;
	printConst: (n: number) => string;
};

function visit(node: symbolic, recurse: () => symbolic, visitor: Visitor, parent?: symbolic): symbolic {
	function postvisit(node: symbolic): symbolic {
		return visitor.post?.(node, parent) ?? node;
	}
	if (visitor.pre) {
		const n = visitor.pre(node, parent);
		if (!n)
			return node;
		if (n !== node)
			return postvisit(n);
	}
	return postvisit(recurse());
}

function printConst(n: number, opts?: StringifyOptions) {
	return opts?.printConst?.(n) ?? n.toString();
}

function printPower(n: number, opts?: StringifyOptions) {
	const s = printConst(n, opts);
	if (opts?.superPower)
		return toSuperscript(s);
	return `^${s}`;
}

type param = number|symbolic;

export class symbolic implements scalar2<symbolic> {
	static interner = new Interner<symbolic>();

	static from(i: number)			{ return Number.isFinite(i) ? symbolicConstant.create(i) : i === Infinity ? infinity : i === -Infinity ? infinity.neg() : nan; }
	static variable(name: string)	{ return symbolicVariable.create(name); }
	static get zero()				{ return zero; }
	static get one()				{ return one; }
	static get i()					{ return i; }
	static get e()					{ return e; }
	static get pi()					{ return pi; }
	static get infinity()			{ return infinity; }
	static get nan()				{ return nan; }

	static sign(i: param)			{ return symbolicSign.create(asSymbolic(i)); }
	static frac(i: param)			{ return symbolicFrac.create(asSymbolic(i)); }
	static floor(i: param)			{ return symbolicFloor.create(asSymbolic(i)); }
	static ceil(i: param)			{ return symbolicCeil.create(asSymbolic(i)); }
	static trunc(i: param)			{ return symbolicTrunc.create(asSymbolic(i)); }
	static round(i: param)			{ return symbolicRound.create(asSymbolic(i)); }
	static min(a: param, b: param)	{ return symbolicMin.create(asSymbolic(a), asSymbolic(b)); }
	static max(a: param, b: param)	{ return symbolicMax.create(asSymbolic(a), asSymbolic(b)); }

	static sin(i: param)			{ return symbolicSin.create(asSymbolic(i)); }
	static cos(i: param)			{ return symbolicCos.create(asSymbolic(i)); }
	static tan(i: param)			{ return symbolicTan.create(asSymbolic(i)); }
	static asin(i: param)			{ return symbolicAsin.create(asSymbolic(i)); }
	static acos(i: param)			{ return symbolicAcos.create(asSymbolic(i)); }
	static atan(i: param)			{ return symbolicAtan.create(asSymbolic(i)); }
	static atan2(a: param, b: param) { return symbolicAtan2.create(asSymbolic(a), asSymbolic(b)); }

	static exp(i: param)			{ return symbolicExp.create(asSymbolic(i)); }
	static log(i: param)			{ return symbolicLog.create(asSymbolic(i)); }
	static sqrt(i: param)			{ return asSymbolic(i).pow(1 / 2); }

	static sinh(i: param)			{ return symbolicSinh.create(asSymbolic(i)); }
	static cosh(i: param)			{ return symbolicCosh.create(asSymbolic(i)); }
	static tanh(i: param)			{ return symbolicTanh.create(asSymbolic(i)); }
	static asinh(i: param)			{ return symbolicAsinh.create(asSymbolic(i)); }
	static acosh(i: param)			{ return symbolicAcosh.create(asSymbolic(i)); }
	static atanh(i: param)			{ return symbolicAtanh.create(asSymbolic(i)); }

	static set(key: symbolic, value: symbolic) {
		this.interner.set(key.id, value);
	}
	static getById(key: string) {
		return this.interner.get(key);
	}

	constructor(public id: string) {}
	is<T extends keyof typeof types>(type: T): this is InstanceType<(typeof types)[T]> {
		return this instanceof types[type];
	}

	from(n: number | bigint) { return symbolic.from(Number(n)); }
	dup():		symbolic	{ return this; }

	sign(): number {
		const v = this.evaluate();
		return Math.sign(v);
	}
	divmod(_b: symbolic): number | bigint {
		throw new Error("Method not implemented.");
	}
	lt(): boolean {
		throw new Error("Method not implemented.");
	}

	sqrt():		symbolic	{ return this.pow(1 / 2); }
	abs():		symbolic	{ return symbolicAbs.create(this); }
	neg():		symbolic	{ return symbolicAdd.create([term(this, -1)]); }
	recip():	symbolic	{ return symbolicMul.create([factor(this, -1)]); }

	npow(b: number): symbolic {
		return b === 1 ? this : b === 0 ? one : symbolicMul.create([factor(this, b)]);
	}

	pow(b: param): symbolic {
		return typeof b === 'number' ? this.npow(b)
			: isConst(b) ? this.npow(b.value)
			: symbolicPow.create(this, b);
	}

	scale(b: number): symbolic	{
		return (b === 0 ? zero : b === 1 ? this : symbolicMul.create([factor(this)], b));
	}
	add(b: param): symbolic	{
		return typeof b === 'number' ? (b === 0 ? this : addTerms(b, term(this, 1)))
			: isConst(b) ? (b.value === 0 ? this : addTerms(b.value, term(this, 1)))
			: b instanceof symbolicAdd ? addTerms(b.num, term(this, 1), ...b.terms)
			: addTerms(0, term(this, 1), term(b, 1));
	}
	sub(b: param): symbolic	{ 
		return typeof b === 'number' ? (b === 0 ? this : addTerms(-b, term(this, 1)))
			: isConst(b) ? (b.value === 0 ? this : addTerms(-b.value, term(this, 1)))
			: b instanceof symbolicAdd ? addTerms(-b.num, term(this, 1), ...b.terms.map(i => term(i.item, -i.coef)))
			: addTerms(0, term(this, 1), term(b, -1));
	}
	mul(b: param): symbolic	{
		return typeof b === 'number' ? this.scale(b)
			: isConst(b) ? this.scale(b.value)
			: b instanceof symbolicMul ? mulFactors(b.num, ...b.factors, factor(this))
			: mulFactors(1, factor(this), factor(b));
	}

	div(b: param): symbolic	{
		return typeof b === 'number' ? (b === 0 ? infinity : this.scale(1 / b))
			: isConst(b) ? (b.value === 0 ? infinity : this.scale(1 / b.value))
			: b instanceof symbolicMul ? mulFactors(b.num, ...b.factors.map(i => factor(i.item, -i.pow)), factor(this))
			: mulFactors(1, factor(this), factor(b.recip()));
	}
	mod(b: param): symbolic		{ return symbolicMod.create(this, asSymbolic(b)); }
	mag(): number				{ const v = this.evaluate(); return Number.isFinite(v) ? Math.abs(v) : NaN; }
	eq(b: symbolic) : boolean	{ return this === b;}

	substitute(_map: Bindings):			symbolic	{ return this; }
	expand(_opts?: ExpandOptions):		symbolic	{ return this; }
	collect(_v: string):				symbolic[]	{ return [this]; }
	factor():							symbolic	{ return this; }
	visit(visitor: Visitor, parent?: symbolic):		symbolic	{ return visit(this, () => this, visitor, parent); }

	match(node: symbolic, bindings: Bindings, _opts?: MatchOptions): Bindings | null {
		return this === node ? bindings : null;
	}

	evaluate(_env?: Record<string, number>) :	number			{ return NaN; }
	evaluateT<T>(_ops: Operators<T>, _env?: Record<string, T>) 	:	T|undefined	{ return undefined; }
	derivative(_v: string):						symbolic		{ return zero;}
	toString(_opts?: StringifyOptions):			string			{ return this.id; }
	valueOf():									number | bigint { return this.evaluate(); }
	[Symbol.for("debug.description")]():		string			{ return this.toString(); }
}

//-----------------------------------------------------------------------------
// constant
//-----------------------------------------------------------------------------

class symbolicConstant extends symbolic {
	static create(i: number)	{ return this.interner.intern(`c:${i}`, id => new symbolicConstant(id, i)); }

	constructor(id: string, public value: number) {
		super(id);
	}
	neg():		symbolic		{ return symbolicConstant.create(-this.value); }
	recip():	symbolic		{ return symbolicConstant.create(1 / this.value); }
	npow(b: number): symbolic	{ return symbolicConstant.create(this.value ** b); }

	scale(b: number): symbolic	{ return symbolicConstant.create(this.value * b); }
	add(b: param): symbolic	{
		return typeof b === 'number' ? symbolicConstant.create(this.value + b)
			: this.value === 0 ? b
			: isConst(b) ? symbolicConstant.create(this.value + b.value)
			: b.add(this);
	}
	mul(b: param): symbolic	{
		return typeof b === 'number' ? symbolicConstant.create(this.value * b)
			: this.value === 0 ? this
			: this.value === 1 ? b
			: isConst(b) ? symbolicConstant.create(this.value * b.value)
			: b.mul(this);
	}

	evaluate(_env?: Record<string, number>):	number { return this.value; }
	evaluateT<T>(ops: Operators<T>, _env?: Record<string, T>) 	:	T|undefined	{ return ops.from(this.value); }
	toString(opts: StringifyOptions):			string { return printConst(this.value, opts); }
}

const zero	= symbolicConstant.create(0);
const one	= symbolicConstant.create(1);

function isConst(e: symbolic): e is symbolicConstant {
	return e instanceof symbolicConstant;
}

function asSymbolic(i: param): symbolic {
	return typeof i === 'number' ? symbolic.from(i) : i;
}

//-----------------------------------------------------------------------------
// variable
//-----------------------------------------------------------------------------

class symbolicVariable extends symbolic {
	static create(name: string)	{ return this.interner.intern(`v:${name}`, id => new symbolicVariable(id, name)); }

	constructor(id: string, public name: string) {
		super(id);
	}
	substitute(map: Bindings) : symbolic {
		const val = map[this.name];
		return val === undefined ? this : val;
	}
	derivative(v: string) : symbolic {
		return v === this.name ? one : zero;
	}
	evaluate(env?: Record<string, number>) : number {
		return env ? (env[this.name] ?? NaN) : NaN;
	}
	evaluateT<T>(ops: Operators<T>, env?: Record<string, T>) {
		return env ? env[this.name] : undefined;
	}
	toString() : string { return this.name; }
}

class symbolicMatcher extends symbolicVariable {
	static create(name: string)	{ return this.interner.intern(`*:${name}`, id => new symbolicMatcher(id, name)); }

	match(node: symbolic, bindings: Bindings, _opts?: MatchOptions): Bindings | null {
		const got = bindings[this.name];
		if (got)
			return got === node ? bindings : null;

		bindings[this.name] = node;
		return bindings;
	}
}

//-----------------------------------------------------------------------------
// add
//-----------------------------------------------------------------------------

interface term {
	item: symbolic;
	coef: number;
}
function term(item: symbolic, coef = 1): term {
	return { item, coef };
}

export function addTerms(num: number, ...a: readonly Readonly<term>[]): symbolic {
	const terms: term[] = [];
	for (const i of a) {
		if (isConst(i.item)) {
			num += i.coef * i.item.value;

		} else if (i.item instanceof symbolicAdd) {
			num += i.item.num * i.coef;
			for (const j of i.item.terms)
				terms.push(term(j.item, j.coef * i.coef));

		} else if (i.item instanceof symbolicMul) {
			// pull numeric multiplier from mul into term coefficient
			const coef = i.coef * i.item.num;
			if (coef !== 0)
				terms.push(term(i.item.num === 1 ? i.item : i.item.factors.length === 1 && i.item.factors[0].pow === 1 ? i.item.factors[0].item : symbolicMul.create(i.item.factors, 1), coef));

		} else {
			terms.push(term(i.item, i.coef));	// don't use original
		}
	}

	// canonical order
	terms.sort((a, b) => compare(a.item.id, b.item.id));

	// combine like terms by summing coefficients
	const combined: term[] = [];
	for (const t of terms) {
		const last = combined.at(-1);
		if (last && last.item.id === t.item.id)
			last.coef += t.coef;
		else
			combined.push(t);
	}

	// remove zero coefficients
	const nonzero = combined.filter(t => t.coef !== 0);

	if (nonzero.length === 0)
		return symbolic.from(num);

	if (nonzero.length === 1 && num === 0)
		return nonzero[0].coef === 1 ? nonzero[0].item : nonzero[0].item.scale(nonzero[0].coef);

	return symbolicAdd.create(nonzero, num);
}

// collect factor counts and min powers across terms
function getFactors(terms: readonly Readonly<term>[]) {
	const factors: Record<string, factor & { terms: Set<term> }> = {};

	function addFactor(item: symbolic, pow: number, t: term) {
		const info = factors[item.id] ??= { item: item, pow: pow, terms: new Set<term>() };
		info.terms.add(t);
		info.pow = Math.min(info.pow, pow);
	}
	for (const t of terms) {
		if (t.item instanceof symbolicMul) {
			for (const f of t.item.factors)
				addFactor(f.item, f.pow, t);
		} else {
			addFactor(t.item, 1, t);
		}
	}
	return Object.values(factors);
}

// scoring: prefer candidates that cover more terms and that simplify their inner sum under trig rules
function commonFactors(terms: readonly Readonly<term>[]): readonly term[] {
	const factors = getFactors(terms);
	const singles = factors.filter(i => i.terms.size > 1);
	const pairs: {a: factor, b: factor, terms: Set<term>}[] = [];

	for (const a of singles) {
		for (const b of singles) {
			if (a === b)
				break;
			const terms = a.terms.intersection(b.terms);
			if (terms.size > 1)
				pairs.push({a, b, terms});
		}
	}

	const scorer = scoreFactory();
	const scoreCandidate = (terms: Set<term>, inner: symbolic) => terms.size * 10 - scorer(inner);
	
	function reducedSingle(t: term, f: factor) {
		if (t.item instanceof symbolicMul) {
			const newFactors = t.item.factors.map(ff => ff.item === f.item ? factor(ff.item, ff.pow - f.pow) : ff).filter(ff => ff.pow !== 0);
			return term(mulFactors(t.item.num, ...newFactors), t.coef);
		}
		return t.item === f.item ? term(one, t.coef) : t;
	}

	function reducedPair(t: term, a: factor, b: factor) {
		if (t.item instanceof symbolicMul) {
			const newFactors = t.item.factors.map(f =>
				f.item === a.item ? factor(f.item, f.pow - a.pow)
			:   f.item === b.item ? factor(f.item, f.pow - b.pow)
			:   f
			).filter(ff => ff.pow !== 0);
			return term(mulFactors(t.item.num, ...newFactors), t.coef);
		}
		return t;
	}

	const factored: term[] = [];
	let remaining = new Set<term>(terms);

	while (remaining.size >= 2) {
		// find best candidate
		type Candidate = { mul: symbolic; todo: Set<term>; inner: symbolic; score: number };

		let best: Candidate | undefined;

		// singles
		for (const f of singles) {
			const todo = f.terms.intersection(remaining);
			if (todo.size > 1)  {
				const inner = addTerms(0, ...Array.from(todo).map(t => reducedSingle(t, f)));
				const score = scoreCandidate(todo, inner);
				if (!best || score > best.score)
					best = { mul: factorAsSymbolic(f), todo, inner, score };
			}
		}

		// pairs
		for (const p of pairs) {
			const todo = p.terms.intersection(remaining);
			if (todo.size > 1)  {
				const inner = addTerms(0, ...Array.from(todo).map(t => reducedPair(t, p.a, p.b)));
				const score = scoreCandidate(todo, inner);
				if (!best || score > best.score)
					best = { mul: factorAsSymbolic(p.a).mul(factorAsSymbolic(p.b)), todo, inner, score };
			}
		}

		if (!best)
			break;

		factored.push(term(best.inner.mul(best.mul)));
		remaining = remaining.difference(best.todo);
	}

	return [...factored, ...remaining];
}

class symbolicAdd extends symbolic {
	static create(terms: readonly term[], num = 0) : symbolic	{
		return this.interner.intern(`a(${num ? `${num},` : ''}${terms.map(i => `${i.coef === 1 ? '' : i.coef === -1 ? '-' : i.coef}${i.item.id}`).join(',')})`, id => new symbolicAdd(id, terms, num));
	}

	constructor(id: string, public terms: readonly Readonly<term>[], public num = 0) {
		super(id);
	}
	neg(): symbolic {
		if (this.num === 0 && this.terms.length === 1 && this.terms[0].coef === -1)
			return this.terms[0].item;
		return symbolicAdd.create(this.terms.map(i => term(i.item, -i.coef)), -this.num);
	}

	add(b: param): symbolic	{
		return  typeof b === 'number' ? symbolicAdd.create(this.terms, this.num + b)
			: 	isConst(b) ? symbolicAdd.create(this.terms, this.num + b.value)
			: 	b instanceof symbolicAdd ? addTerms(this.num + b.num, ...this.terms, ...b.terms)
			:	addTerms(this.num, ...this.terms, term(b, 1));
	}

	match(node: symbolic, bindings: Bindings, opts?: MatchOptions): Bindings | null {
		if (node instanceof symbolicAdd) {
			if (opts?.exact && this.num != node.num)
				return null;

			const opts2 	= { ...opts, exact: true};
			const nterms	= node.terms.slice();
			for (const pterm of this.terms) {
				let found = -1;
				for (const nterm of nterms) {
					if (pterm.coef === nterm.coef && pterm.item.match(nterm.item, bindings, opts2)) {
						found = nterms.indexOf(nterm);
						break;
					}
				}
				if (found === -1)
					return null;

				nterms.splice(found, 1);
			}
			if (opts?.exact && nterms.length)
				return null;

			if (this.num != node.num || nterms.length)
				bindings['_addrest'] = addTerms(this.num - node.num, ...nterms);

			return bindings;
		}
		return null;
	}

	visit(visitor: Visitor, parent?: symbolic): symbolic {
		return visit(this, () => addTerms(this.num, ...this.terms.map((i) => term(i.item.visit(visitor, this), i.coef))), visitor, parent);
	}

	substitute(map: Bindings) : symbolic {
		return addTerms(this.num, ...this.terms.map(i => term(i.item.substitute(map), i.coef)));
	}

	collect(v: string): symbolic[] {
		const groups: symbolic[] = [symbolic.from(this.num)];

		for (const t of this.terms) {
			const i = t.item;
			if (i instanceof symbolicMul) {
				let pow = 0;
				const remaining: factor[] = [];
				for (const ff of i.factors as factor[]) {
					if (ff.item instanceof symbolicVariable && ff.item.name === v)
						pow += ff.pow;
					else
						remaining.push(ff);
				}
				groups[pow] = (groups[pow] ??= zero).add(mulFactors(i.num * t.coef, ...remaining));

			} else if (i instanceof symbolicVariable && i.name === v) {
				groups[1] = (groups[1] ??= zero).add(symbolic.from(t.coef));
			} else {
				groups[0] = groups[0].add(t.item.scale(t.coef));
			}
		}
		return groups;
	}
	expand(opts?: ExpandOptions): symbolic {
		return addTerms(this.num, ...this.terms.map(i => term(i.item.expand(opts), i.coef)));
	}
	factor(): symbolic {
		const terms = commonFactors(this.terms);
		return addTerms(this.num, ...terms);
	}

	derivative(v: string) : symbolic {
		return addTerms(0, ...this.terms.map(i => term(i.item.derivative(v), i.coef)));
	}
	evaluate(env?: Record<string, number>) : number {
		return this.terms.reduce((acc, curr) => acc + (curr.item.evaluate(env) * curr.coef), this.num);
	}
	evaluateT<T>(ops: Operators<T>, env?: Record<string, T>) {
		return this.terms.reduce((acc, curr) => ops.add(acc, ops.mul(curr.item.evaluateT(ops, env)!, ops.from(curr.coef))), ops.from(this.num));
	}

	toString(opts: StringifyOptions) : string {
		function termOrder(t: term): number {
			return t.item instanceof symbolicMul ? t.item.factors.reduce((acc, curr) => acc + curr.pow, 0) : 0;
		}

		const terms = this.terms.slice().sort((a, b) => termOrder(b) - termOrder(a));
		return (opts?.fromMul ? '(' : '') + terms.map((t, j) => {
			const abs	= Math.abs(t.coef);
			return	(j === 0 ? (t.coef < 0 ? '-' : '') : (t.coef < 0 ? ' - ' : ' + '))
				+	(abs === 1 ? '' : `${printConst(abs, opts)} * `)
				+	t.item.toString();
		}).join('')
		+ (this.num !== 0 ? (this.num < 0 ? ' - ' : ' + ') + printConst(Math.abs(this.num), opts) : '')
		+ (opts?.fromMul ? ')' : '');
	}
}

//-----------------------------------------------------------------------------
// mul
//-----------------------------------------------------------------------------

interface factor {
	item:	symbolic;
	pow:	number;
}
function factor(item: symbolic, pow = 1): factor {
	return { item, pow };
}
function factorAsSymbolic(f: factor): symbolic {
	return f.item.pow(f.pow);
}

export function mulFactors(num: number, ...f: Readonly<factor>[]): symbolic {
	const factors: factor[] = [];
	for (const i of f) {
		if (i.pow === 0)
			continue;

		if (isConst(i.item)) {
			num *= i.item.value ** i.pow;

		} else if (i.item instanceof symbolicMul) {
			num *= i.item.num ** i.pow;
			for (const j of i.item.factors)
				factors.push(factor(j.item, j.pow * i.pow));

		} else if (i.item instanceof symbolicAdd) {
			const add = i.item;
			const neg = add.terms[0].coef < 0 && Number.isInteger(i.pow);
			if (neg && i.pow % 2 === 1) {
				num = -num;
				f.push(factor(add.neg(), i.pow));
				continue;
			}

			if (add.terms.length === 1 && add.num === 0) {
				num *= add.terms[0].coef ** i.pow;
				factors.push(factor(add.terms[0].item, i.pow));
			} else {
				factors.push(factor(add, i.pow));
			}

		} else {
			factors.push(factor(i.item, i.pow));// don't use original!
		}
	}

	if (num === 0)
		return zero;

	// canonical order
	factors.sort((a, b) => compare(a.item.id, b.item.id));

	// combine like factors by summing powers
	const combined: factor[] = [];
	for (const f of factors) {
		const last = combined.at(-1);
		if (last && last.item.id === f.item.id)
			last.pow += f.pow;
		else
			combined.push(f);
	}

	// remove zero coefficients and handle powers of i
	const nonzero = combined.filter(t => {
		if (t.item === i) {
			const pow = ((t.pow % 4) + 4) % 4;
			if (pow & 2)
				num = -num;
			t.pow = pow % 2;
		}
		return t.pow !== 0;
	});

	if (nonzero.length === 0)
		return symbolic.from(num);

	if (nonzero.length === 1 && nonzero[0].pow === 1 && num === 1)
		return nonzero[0].item;

	return symbolicMul.create(nonzero, num);
}

class symbolicMul extends symbolic {
	static create(factors: readonly factor[], num = 1)	{
		return this.interner.intern(`m(${num !== 1 ? `${num},` : ''}${factors.map(i => `${i.item.id}${i.pow !== 1 ? i.pow : ''}`).join(',')})`, id => new symbolicMul(id, factors, num));
	}

	constructor(id: string, public factors: readonly Readonly<factor>[], public num = 1) {
		super(id);
	}
	
	neg(): symbolic {
		return symbolicMul.create(this.factors, -this.num);
	}
	recip(): symbolic {
		return symbolicMul.create(this.factors.map(i => factor(i.item, -i.pow)), 1 / this.num);
	}
	npow(b: number): symbolic {
		return b === 0 ? one : b === 1 ? this : symbolicMul.create(this.factors.map(i => factor(i.item, i.pow * b)), this.num ** b);
	}
	scale(b: number): symbolic	{
		return b === 0 ? zero : b === 1 ? this : symbolicMul.create(this.factors, this.num * b);
	}

	mul(b: param): symbolic	{
		return typeof b === 'number' ? this.scale(b)
			:	isConst(b) ? this.scale(b.value)
			:	b instanceof symbolicMul ? mulFactors(this.num * b.num, ...this.factors, ...b.factors)
			:	mulFactors(this.num, ...this.factors, factor(b));
	}
	match(node: symbolic, bindings: Bindings, opts?: MatchOptions): Bindings | null {
		if (node instanceof symbolicMul) {
			if (opts?.exact && this.num != node.num)
				return null;

			const opts2 	= { ...opts, exact: true};
			const nfactors	= node.factors.slice();
			for (const pfactor of this.factors) {
				let found = -1;
				for (const nfactor of nfactors) {
					if (pfactor.pow === nfactor.pow && pfactor.item.match(nfactor.item, bindings, opts2)) {
						found = nfactors.indexOf(nfactor);
						break;
					}
				}
				if (found === -1)
					return null;

				nfactors.splice(found, 1);
			}

			if (opts?.exact && nfactors.length)
				return null;

			if (this.num != node.num || nfactors.length)
				bindings['_mulrest'] = mulFactors(this.num / node.num, ...nfactors);

			return bindings;
		}
		return null;
	}
	substitute(map: Bindings) : symbolic {
		return mulFactors(this.num, ...this.factors.map(f => factor(f.item.substitute(map), f.pow)));
	}
	visit(visitor: Visitor, parent?: symbolic): symbolic {
		return visit(this, () => mulFactors(this.num, ...this.factors.map((f) => factor(f.item.visit(visitor, this), f.pow))), visitor, parent);
	}
	expand(opts?: ExpandOptions): symbolic {
		let factors = this.factors;
		if (opts?.depth ?? 2 > 1) {
			const opts2	= opts?.depth ? { ...opts, depth: opts.depth - 1 } : opts;
			factors		= factors.map(f => factor(f.item.expand(opts2), f.pow));
		}

		const additive: symbolicAdd[] = [];
		const others: factor[] = [];

		for (const f of factors) {
			if (f.item instanceof symbolicAdd && f.pow > 0 && (!opts?.maxPow || f.pow < opts.maxPow) && Number.isInteger(f.pow)) {
				for (let i = 0; i < f.pow; i++)
					additive.push(f.item);
			} else {
				others.push(f);
			}
		}

		let parts: symbolic[] = [mulFactors(this.num, ...others)];
		for (const a of additive) {
			const parts2: symbolic[] = [];
			for (const p of parts) {
				for (const t of a.terms)
					parts2.push(p.mul(t.item).scale(this.num * t.coef));
				if (a.num !== 0)
					parts2.push(p.scale(this.num * a.num));

			}
			parts = parts2;
		}

		return addTerms(0, ...parts.map(p => term(p, 1)));
	}

	derivative(v: string) : symbolic {
		return addTerms(0, ...this.factors.map(f => term(mulFactors(
			f.pow * this.num,
			factor(f.item, f.pow - 1),
			factor(f.item.derivative(v)),
			...this.factors.filter(g => g !== f)
		))));
	}
	
	evaluate(env?: Record<string, number>) : number {
		return this.factors.reduce((acc, curr) => {
			const v = curr.item.evaluate(env);
			switch (curr.pow) {
				case 1:		return acc * v;
				case -1:	return acc / v;
				default:	return acc * v ** curr.pow;
			}
		}, this.num);
	}
	evaluateT<T>(ops: Operators<T>, env?: Record<string, T>) {
		return this.factors.reduce((acc, curr) => {
			const v = curr.item.evaluateT(ops, env);
			switch (curr.pow) {
				case 1:		return ops.mul(acc, v!);
				case -1:	return ops.div(acc, v!);
				default:	return ops.mul(acc, ops.pow(v!, ops.from(curr.pow)));
			}
		}, ops.from(this.num));
	}

	toString(opts: StringifyOptions) : string {
		const factors	= this.factors.slice().sort((a, b) => b.pow - a.pow);
		const anum		= Math.abs(this.num);

		//0 - no const, 1 - at start, 2 - at end
		const doConst = factors[0].pow < 0 ? 1 : anum < 1 && isAlmostInteger(1 / anum) ? 2 : anum !== 1 ? 1 : 0;

		return (this.num < 0 ? '-' : '') + (doConst === 1 ? printConst(anum, opts) : '') + factors.map((i, j) =>
			(j === 0 && doConst !== 1 ? '' : i.pow < 0 ? ' / ' : ' * ')
			+ i.item.toString({...opts, fromMul: true})
			+ (Math.abs(i.pow) !== 1 ? printPower(Math.abs(i.pow), opts) : '')
		).join('')
		+ (doConst === 2 ? ` / ${Math.round(1 / anum)}` : '');
	}
}

//-----------------------------------------------------------------------------
// special constants
//-----------------------------------------------------------------------------

function specialConstant(name: string, toString = name, value?: number) {
	const C = class extends symbolic {
		constructor()		{ super(name); }
		evaluate(_env?: Record<string, number>): number { return value ?? NaN; }
		evaluateT<T>(ops: Operators<T>, _env?: Record<string, T>) { return value !== undefined ? ops.from(value) : undefined; }
		toString(): string	{ return toString; }
	};
	return C;
}

const i			= new (specialConstant('i', '𝑖'));
const e			= new (specialConstant('e', '𝑒', Math.E));
const pi		= new (specialConstant('pi', 'π', Math.PI));
const nan		= new (specialConstant('NaN'));

class symbolicInfinity extends specialConstant('infinity', '∞', Infinity) {
	add(_b: param): symbolic {
		return this;
	}
	mul(b: param): symbolic {
		if (typeof b === 'number' ? b === 0 : isConst(b) && b.value === 0)
			return nan;
		return this;
	}
}

const infinity	= new symbolicInfinity;

symbolic.set(i.mul(i), one.neg());
symbolic.set(zero.recip(), infinity);

//-----------------------------------------------------------------------------
// functions
//-----------------------------------------------------------------------------

class unaryFunctionBase extends symbolic {
	constructor(id: string, public arg: symbolic) { super(id); }
	create(i: symbolic)						{ return (this.constructor as any).create(i); }
	expand(opts?: ExpandOptions): symbolic	{ return this.create(this.arg.expand(opts)); }
};

function unaryFunction(name: string,
	evaluate: (arg: number) => number,
	derivative: (arg: symbolic) => symbolic,
	toString = (a: symbolic, opts: StringifyOptions) => `${name}(${a.toString(opts)})`
) {
	const C = class extends unaryFunctionBase {
		static create(i: symbolic)	{
			return this.interner.intern(`${name}:${i.id}`, id => new C(id, i));
		}
		match(node: symbolic, bindings: Bindings, opts?: MatchOptions): Bindings | null {
			return node instanceof C && this.arg.match(node.arg, bindings, opts) ? bindings : null;
		}
		visit(visitor: Visitor, parent?: symbolic): symbolic {
			return visit(this, () => this.create(this.arg.visit(visitor, this)), visitor, parent);
		}
		substitute(map: Bindings) : symbolic {
			return this.create(this.arg.substitute(map));
		}
		derivative(v: string): symbolic { return derivative(this.arg).mul(this.arg.derivative(v)); }
		evaluate(env?: Record<string, number>): number { return evaluate(this.arg.evaluate(env)); }
		evaluateT<T>(ops: Operators<T>, env?: Record<string, T>) { return ops.func(name, [this.arg.evaluateT(ops, env)!]); }

		toString(opts: StringifyOptions): string { return toString(this.arg, {...opts, fromMul: false}); }
	};

	return C;
}

class binaryFunctionBase extends symbolic {
	constructor(id: string, public arg1: symbolic, public arg2: symbolic) { super(id); }
	create(i: symbolic, j: symbolic)		{ return (this.constructor as any).create(i, j); }
	expand(opts?: ExpandOptions): symbolic	{ return this.create(this.arg1.expand(opts), this.arg2.expand(opts)); }
};

function binaryFunction(name: string,
	evaluate: (a: number, b: number) => number,
	derivative: (a: symbolic, b: symbolic) => [symbolic, symbolic],
	toString = (a: symbolic, b: symbolic, opts: StringifyOptions) => `${name}(${a.toString(opts)}, ${b.toString(opts)})`
) {
	const C = class extends binaryFunctionBase {
		static create(i: symbolic, j: symbolic)	{
			return this.interner.intern(`${name}:${i.id}:${j.id}`, id => new C(id, i, j));
		}
		match(node: symbolic, bindings: Bindings, opts?: MatchOptions): Bindings | null {
			return node instanceof C && this.arg1.match(node.arg1, bindings, opts) && this.arg2.match(node.arg2, bindings, opts) ? bindings : null;
		}
		visit(visitor: Visitor, parent?: symbolic): symbolic {
			return visit(this, () => this.create(this.arg1.visit(visitor, this), this.arg2.visit(visitor, this)), visitor, parent);
		}
		substitute(map: Bindings) : symbolic {
			return this.create(this.arg1.substitute(map), this.arg2.substitute(map));
		}
		derivative(v: string): symbolic {
			const [d1, d2] = derivative(this.arg1, this.arg2);
			return d1.mul(this.arg1.derivative(v)).add(d2.mul(this.arg2.derivative(v)));
		}
		evaluate(env?: Record<string, number>): number { return evaluate(this.arg1.evaluate(env), this.arg2.evaluate(env)); }
		evaluateT<T>(ops: Operators<T>, env?: Record<string, T>) { return ops.func(name, [this.arg1.evaluateT(ops, env)!, this.arg2.evaluateT(ops, env)!]); }

		toString(opts: StringifyOptions): string { return toString(this.arg1, this.arg2, {...opts, fromMul: false}); }
	};
	return C;
}

// basic

const symbolicAbs	= unaryFunction('abs', Math.abs, a => symbolicSign.create(a), a => `|${a}|`);
const symbolicFloor	= unaryFunction('floor', Math.floor, _a => zero, a => `⌊${a}⌋`);
const symbolicCeil	= unaryFunction('ceil', Math.ceil, _a => zero, a => `⌈${a}⌉`);
const symbolicTrunc	= unaryFunction('trunc', Math.trunc, _a => zero);
const symbolicRound	= unaryFunction('round', Math.round, _a => zero);

const symbolicMod	= binaryFunction('mod', (a, b) => a % b, (a, b) => [one, symbolicFloor.create(a.div(b)).neg()], (a, b, opts) => `mod(${a.toString(opts)}, ${b.toString(opts)})`);
const symbolicSign	= unaryFunction('sign', Math.sign, _a => zero);
const symbolicFrac	= unaryFunction('frac', a => a - Math.floor(a), _a => one);

// min/max implemented via abs to keep expressions smooth and composable:
// min(a,b) = 0.5*(a + b - |a - b|)
// max(a,b) = 0.5*(a + b + |a - b|)
const symbolicMin = binaryFunction('min', (a, b) => Math.min(a, b), (a, b) => [
	one.sub(symbolicSign.create(a.sub(b))).scale(0.5),
	one.add(symbolicSign.create(a.sub(b))).scale(0.5)
]);

const symbolicMax = binaryFunction('max', (a, b) => Math.max(a, b), (a, b) => [
	one.add(symbolicSign.create(a.sub(b))).scale(0.5),
	one.sub(symbolicSign.create(a.sub(b))).scale(0.5)
]);


/*
function defineProtoGetter<Ctor extends new (...args: any[]) => any, K extends string, R>(ctor: Ctor, key: K, getter: (this: InstanceType<Ctor>) => R) {
	Object.defineProperty(ctor.prototype, key, {
		configurable: false,
		enumerable: false,
		value: getter,
	});
	return ctor as unknown as Ctor & {[key in K]: typeof getter};
}

function defineStaticGetter<Ctor extends new (...args: any[]) => any, K extends string, V>(ctor: Ctor, key: K, value: V) {
	Object.defineProperty(ctor, key, {
		configurable: false,
		enumerable: false,
		value: value,
	});
	return ctor as unknown as Ctor & {[key in K]: V};
}
*/
//export const symbolic2 = defineProtoGetter(symbolic, 'frac', function(this: symbolic) { return symbolicFrac.create(this); });
//export let symbolic2 = defineStaticGetter(symbolic, 'frac', function(a: symbolic) { return symbolicFrac.create(a); });
//symbolic2 = defineStaticGetter(symbolic, 'sign', function(a: symbolic) { return symbolicSign.create(a); });

// trigonometric

const symbolicSin	= unaryFunction('sin', Math.sin, arg => symbolicCos.create(arg));
const symbolicCos	= unaryFunction('cos', Math.cos, arg => symbolicSin.create(arg).neg());
const symbolicTan	= unaryFunction('tan', Math.tan, arg => symbolicCos.create(arg).pow(-2));

const symbolicAsin	= unaryFunction('asin', Math.asin, arg => one.sub(arg.pow(2)).pow(-1 / 2));
const symbolicAcos	= unaryFunction('acos', Math.acos, arg => one.sub(arg.pow(2)).pow(-1 / 2).neg());
const symbolicAtan	= unaryFunction('atan', Math.atan, arg => one.div(arg.pow(2).add(one)));

const symbolicAtan2 = binaryFunction('atan2',
	Math.atan2,
	(a, b) => {
		const denom = b.pow(2).add(a.pow(2));
		return [ b.div(denom), a.mul(symbolic.from(-1)).div(denom) ];
	}
);

symbolic.set(symbolic.sin(zero), zero);
symbolic.set(symbolic.sin(pi.scale(0.5)), one);
symbolic.set(symbolic.sin(pi), zero);

symbolic.set(symbolic.cos(zero), one);
symbolic.set(symbolic.cos(pi.scale(0.5)), zero);
symbolic.set(symbolic.cos(pi), one.neg());

symbolic.set(symbolic.tan(zero), zero);
symbolic.set(symbolic.tan(pi.scale(0.25)), one);

symbolic.set(symbolic.asin(zero), zero);
symbolic.set(symbolic.asin(one), pi.scale(0.5));

symbolic.set(symbolic.acos(zero), pi.scale(0.5));
symbolic.set(symbolic.acos(one), zero);

symbolic.set(symbolic.atan(zero), zero);
symbolic.set(symbolic.atan(one), pi.scale(0.25));

// exponential and logarithm

const symbolicExp = unaryFunction('exp', Math.exp, arg => symbolic.exp(arg));
const symbolicLog = unaryFunction('log', Math.log, arg => arg.recip());

const symbolicPow = binaryFunction('pow',
	Math.pow,
	(a, b) => [b.mul(a.pow(b.sub(one))), symbolic.log(a).mul(a.pow(b))] as [symbolic, symbolic],
	(a, b) => `(${a.toString()}) ^ (${b.toString()})`
);
symbolic.set(symbolic.exp(one), e);
symbolic.set(symbolic.log(e), one);

// hyperbolic

const symbolicSinh = unaryFunction('sinh', Math.sinh, arg => symbolicCosh.create(arg));
const symbolicCosh = unaryFunction('cosh', Math.cosh, arg => symbolicSinh.create(arg));
const symbolicTanh = unaryFunction('tanh', Math.tanh, arg => symbolicCosh.create(arg).pow(-2));

const symbolicAsinh = unaryFunction('asinh', Math.asinh, arg => arg.pow(2).add(one).pow(-1 / 2));
const symbolicAcosh = unaryFunction('acosh', Math.acosh, arg => arg.pow(2).sub(one).pow(-1 / 2));
const symbolicAtanh = unaryFunction('atanh', Math.atanh, arg => one.sub(arg.pow(2)).recip());

symbolic.set(symbolic.sinh(zero), zero);
symbolic.set(symbolic.cosh(zero), one);
symbolic.set(symbolic.tanh(zero), zero);

symbolic.set(symbolic.asinh(zero), zero);
symbolic.set(symbolic.acosh(one), zero);
symbolic.set(symbolic.atanh(zero), zero);


//-----------------------------------------------------------------------------
// transformation rules
//-----------------------------------------------------------------------------

function replaceRest(bs: Bindings, node: symbolic): symbolic {
	const addrest = bs['_addrest'];
	if (addrest)
		node = node.add(addrest);
	const mulrest = bs['_mulrest'];
	if (mulrest)
		node = node.mul(mulrest);
	return node;
}

function PatternRule(name: string, pattern: symbolic, replace: (bs: Bindings) => symbolic, guard?: (bs: Bindings) => boolean): Rule {
	return {
		name,
		match: node => pattern.match(node, {}),
		replace: bs => replaceRest(bs, replace(bs)),
		guard,
	};
}

function bind(name: string) {
	return symbolicMatcher.create(name);
}

export const generalRules: Rule[] = [
	{
		name: 'mul-distribute',
		match(node: symbolic) {
			const result = node.expand({ depth: 1, maxPow: 4 });
			if (result !== node)
				return { result };
			return null;
		},
		replace(bs: Bindings): symbolic {
			return bs.result;
		}
	},
	{
		name: 'collect-like-terms',
		match(node: symbolic) {
			const result = node.factor();
			if (result !== node)
				return { result };
			return null;
		},
		replace(bs: Bindings) {
			return bs.result;
		}
	},
	/*
	PatternRule('factor-common',
		// pattern: A*B + A*C  ->  A * (B + C)
		bind('A').mul(bind('B')).add(bind('A').mul(bind('C'))),
		bs => bs.A.mul(bs.B.add(bs.C))
	),*/
];

export const trigRules: Rule[] = [
	PatternRule('sin-sum',
		symbolic.sin(bind('A').add(bind('B'))),
		bs => symbolic.sin(bs.A).mul(symbolic.cos(bs.B)).add(symbolic.cos(bs.A).mul(symbolic.sin(bs.B)))
	),
	PatternRule('sin-diff',
		symbolic.sin(bind('A').sub(bind('B'))),
		bs => symbolic.sin(bs.A).mul(symbolic.cos(bs.B)).sub(symbolic.cos(bs.A).mul(symbolic.sin(bs.B)))
	),
	PatternRule('sin-half-angle',
		symbolic.sin(bind('A').scale(0.5)),
		bs => symbolic.sqrt(one.sub(symbolic.cos(bs.A)).scale(0.5))
	),

	PatternRule('cos-sum',
		symbolic.cos(bind('A').add(bind('B'))),
		bs => symbolic.cos(bs.A).mul(symbolic.cos(bs.B)).sub(symbolic.sin(bs.A).mul(symbolic.sin(bs.B)))
	),
	PatternRule('cos-diff',
		symbolic.cos(bind('A').sub(bind('B'))),
		bs => symbolic.cos(bs.A).mul(symbolic.cos(bs.B)).add(symbolic.sin(bs.A).mul(symbolic.sin(bs.B)))
	),
	PatternRule('cos-half-angle',
		symbolic.cos(bind('A').scale(0.5)),
		bs => symbolic.sqrt(one.add(symbolic.cos(bs.A)).scale(0.5))
	),

	PatternRule('tan-sum',
		symbolic.tan(bind('A').add(bind('B'))),
		bs => symbolic.tan(bs.A).add(symbolic.tan(bs.B)).div(one.sub(symbolic.tan(bs.A).mul(symbolic.tan(bs.B))))
	),
	PatternRule('tan-diff',
		symbolic.tan(bind('A').sub(bind('B'))),
		bs => symbolic.tan(bs.A).sub(symbolic.tan(bs.B)).div(one.add(symbolic.tan(bs.A).mul(symbolic.tan(bs.B))))
	),
	PatternRule('tan-half-angle',
		symbolic.tan(bind('A').scale(0.5)),
		bs => symbolic.sin(bs.A).scale(0.5).div(symbolic.cos(bs.A).scale(0.5))
	),

	PatternRule('sin-cos-pythag',
		symbolic.sin(bind('A')).pow(2).add(symbolic.cos(bind('A')).pow(2)),
		_ => one
	),
	PatternRule('tan-secant-pythag',
		symbolic.tan(bind('A')).pow(2).add(one),
		bs => symbolic.cos(bs.A).pow(-2)
	),
	PatternRule('cot-cosecant-pythag',
		one.add(symbolic.tan(bind('A')).pow(-2)),
		bs => symbolic.sin(bs.A).pow(-2)
	),
	/*
	PatternRule('sin-cos-product',
		symbolic.sin(bind('A')).mul(symbolic.cos(bind('B'))),
		bs => symbolic.sin(bs.A.add(bs.B)).mul(symbolic.cos(bs.A.sub(bs.B))).scale(0.5)
	),
	*/

	// double-angle compression: sin(x) * cos(x) -> 1/2 * sin(2x)
	PatternRule('double-angle-sin-compress',
		symbolic.sin(bind('X')).mul(symbolic.cos(bind('X'))),
		bs => symbolic.sin(bs.X.scale(2)).scale(0.5)
	),
];

export const invTrigRules: Rule[] = [
	PatternRule('sin-sum',
		symbolic.sin(bind('A')).mul(symbolic.cos(bind('B'))).add(symbolic.cos(bind('A')).mul(symbolic.sin(bind('B')))),
		bs => symbolic.sin(bs.A.add(bs.B)),
	),
	/*
	PatternRule( 'sin-cos-product',
		symbolic.sin(bind('A').add(bind('B'))).mul(symbolic.cos(bind('A').sub(bind('B')))),
		bs => symbolic.sin(bs.A).mul(symbolic.cos(bs.B))
	),
	*/

	// double-angle expansion: sin(2x) -> 2 * sin(x) * cos(x)
	PatternRule('double-angle-sin-expand',
		symbolic.sin(bind('X').scale(2)),
		bs => symbolic.from(2).mul(symbolic.sin(bs.X).mul(symbolic.cos(bs.X)))
	),
];

export function scoreFactory(depthPenalty = 0.2, mulW = 0.5, addW = 1, constW = 0.1) {
    return (n: symbolic) => {
        let total = 0;
		let depth = 1;

        n.visit({
            pre: (x: symbolic) => {
                ++depth;
                return x;
            },
            post: (x: symbolic) => {
                --depth;
                const w = x.is('mul') ? (x.factors.length - 1) * mulW
                		: x.is('add') ? (x.terms.length - 1) * addW
                		: x.is('const') ? constW
                		: 0.5;
                total += w * (1 + depthPenalty * depth);
                return x;
            }
        });

        return total;
    };
}

export function applyRules(node: symbolic, rules: Rule[]): symbolic {
	const cache		= new Map<string, symbolic>();

	const visitor = {
		post: (node: symbolic) => {
			const cached = cache.get(node.id);
			if (cached)
				return cached;

			for (const r of rules) {
				const bs = r.match(node);
				if (bs && (!r.guard || r.guard(bs))) {
					const replaced		= r.replace(bs);
					const simplified	= replaced.visit(visitor);
					cache.set(node.id, simplified);
					return simplified;
				}
			}
			cache.set(node.id, node);
			return node;
		}
	};

	return node.visit(visitor);
}

class SymbolicOperators extends OperatorsBase<symbolic> {
	from(n: number): symbolic		{ return symbolic.from(n); }
	func(name: string, args: symbolic[]) {
		const fn = (symbolic as unknown as Record<string, (...args: symbolic[]) => symbolic>)[name];
		if (typeof fn === 'function')
			return fn ? fn(...args) : undefined;
	}
	variable(name: string) {
		const v = (symbolic as unknown as Record<string, symbolic>)[name];
		if (v instanceof symbolic)
			return v;
		return symbolic.variable(name);
	}
	pow(a: symbolic, b: symbolic): symbolic {
		return a.pow(b);
	}
};
export const symbolicOperators = new SymbolicOperators();

const types = {
	const:	symbolicConstant,
	var:	symbolicVariable,
	add:	symbolicAdd,
	mul: 	symbolicMul,
	unary:	unaryFunctionBase,
	binary:	binaryFunctionBase,
	sin: 	symbolicSin,
	cos: 	symbolicCos,
	tan: 	symbolicTan,
	asin: 	symbolicAsin,
	acos: 	symbolicAcos,
	atan: 	symbolicAtan,
	atan2: 	symbolicAtan2,
	exp: 	symbolicExp,
	log: 	symbolicLog,
	sinh: 	symbolicSinh,
	cosh: 	symbolicCosh,
	tanh: 	symbolicTanh,
	asinh: 	symbolicAsinh,
	acosh: 	symbolicAcosh,
	atanh: 	symbolicAtanh,
	pow: 	symbolicPow
} as const;
