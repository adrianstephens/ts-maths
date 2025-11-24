/* eslint-disable no-restricted-syntax */
import { Operators, compare, isAlmostInteger, OperatorsBase, gcd2, lcm } from './core';
import { toSuperscript, radicalChars } from './string';

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

	intern<U extends T>(key: string, factory: (key: string) => U): U {
		return (this.get(key) || this.set(key, factory(key))) as U;
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

function visit(node: symbolicBase, recurse: () => symbolicBase, visitor: Visitor, parent?: symbolicBase): symbolicBase {
	function postvisit(node: symbolicBase): symbolicBase {
		return visitor.post?.(node as symbolic, parent as symbolic) ?? node;
	}
	if (visitor.pre) {
		const n = visitor.pre(node as symbolic, parent as symbolic);
		if (!n)
			return node;
		if (n !== node)
			return postvisit(n);
	}
	return postvisit(recurse());
}

export type Rule = {
	name:		string;
	match:		(node: symbolic, opts?: MatchOptions) => Bindings | null;
	replace:	(bs: Bindings) => symbolic;
	guard?: 	(bs: Bindings) => boolean;
};

export type MatchOptions = {
	exact?:		boolean;
	depth?:		number;
};

export type ExpandOptions = {
	depth?:		number;
	maxPow?:	number;
	uses?:		Set<string>;	// only expand if these variables are present
};

const StringifyOptionsDefault = {
	direct:			false,
	ccode:			false,
	fromMul: 		false,
	superPower: 	false,
	radicalPower:	false,
	mulChar:		' * ',
	divChar:		' / ',
	addChar:		' + ',
	subChar:		' - ',
	printConst:		(n: number) => n.toString(),
};

export type StringifyOptions = typeof StringifyOptionsDefault;

function printScaled(s: string, num: number, opts: StringifyOptions) {
	const sign = num < 0 ? '-' : '';
	const abs = Math.abs(num);
	if (abs === 1)
		return sign + s;

	if (abs < 1 && isAlmostInteger(1 / abs))
		return `${sign}${s}${opts.divChar}${opts.printConst(Math.round(1 / abs))}`;
	else
		return `${sign}${opts.printConst(abs)}${s}`;
//		return `${printConst(abs, opts)}${opts.mulChar}${s}`;
}

type param = number|symbolic;

export class symbolicBase {
	static interner = new Interner<symbolicBase>();
	static defStringify = StringifyOptionsDefault;

	static set(key: symbolic, value: symbolic) {
		this.interner.set(key.id, value);
	}
	static getById(key: string) {
		return this.interner.get(key);
	}
	static setDefaultStringifyOptions(opts: Partial<StringifyOptions>) {
		this.defStringify = { ...this.defStringify, ...opts };
	}

	constructor(public id: string) {}
	is<T extends keyof typeof types>(type: T): this is InstanceType<(typeof types)[T]> {
		return this instanceof types[type];
	}

	dup():		symbolicBase		{ return this; }
	eq(b: symbolicBase) : boolean	{ return this === b;}

	substitute(_map: Bindings):			symbolicBase	{ return this; }
	expand(_opts?: ExpandOptions):		symbolicBase	{ return this; }
	factor():							symbolicBase	{ return this; }
	visit(visitor: Visitor, parent?: symbolicBase):		symbolicBase	{ return visit(this, () => this, visitor, parent); }

	match(node: symbolicBase, bindings: Bindings, opts?: MatchOptions): Bindings | null {
		// Allow patterns to match through single numeric factors (e.g., pattern A+B matches 2(A+B))
		if (!opts?.exact && node instanceof symbolic && node.is('mul')) {
			const mul = node as symbolicMul;
			if (mul.factors.length === 1 && mul.factors[0].pow === 1) {
				const inner = mul.factors[0].item;
				const result = this.match(inner, bindings, opts);
				if (result && mul.num !== 1)
					bindings['_numfactor'] = symbolic.from(mul.num);
				return result;
			}
		}
		return this === node ? bindings : null;
	}

	derivative(_v: string):						symbolic		{ return zero;}
	_toString(_opts: StringifyOptions):			string			{ return this.id; }
	toString(opts?: Partial<StringifyOptions>):	string			{ return this._toString({ ...symbolic.defStringify, ...opts }); }
	[Symbol.for("debug.description")]():		string			{ return this.toString({}); }
}

//-----------------------------------------------------------------------------
// symbolic
//-----------------------------------------------------------------------------

export class symbolic extends symbolicBase {
	static from(i: number): symbolic { return Number.isFinite(i) ? symbolicConstant.create(i) : i === Infinity ? infinity : i === -Infinity ? infinity.neg() : nan; }
	static variable(name: string)	{ return symbolicVariable.create(name); }
	static get zero()		: symbolic		{ return zero; }
	static get one()		: symbolic		{ return one; }
	static get i()			: symbolic		{ return i; }
	static get e()			: symbolic		{ return e; }
	static get pi()			: symbolic		{ return pi; }
	static get infinity()	: symbolic		{ return infinity; }
	static get nan()		: symbolic		{ return nan; }

	static sign(i: param)	: symbolic		{ return symbolicSign.create(asSymbolic(i)); }
	static frac(i: param)	: symbolic		{ return symbolicFrac.create(asSymbolic(i)); }
	static floor(i: param)	: symbolic		{ return symbolicFloor.create(asSymbolic(i)); }
	static ceil(i: param)	: symbolic		{ return symbolicCeil.create(asSymbolic(i)); }
	static trunc(i: param)	: symbolic		{ return symbolicTrunc.create(asSymbolic(i)); }
	static round(i: param)	: symbolic		{ return symbolicRound.create(asSymbolic(i)); }
	static min(a: param, b: param)	: symbolic		{ return symbolicMin.create(asSymbolic(a), asSymbolic(b)); }
	static max(a: param, b: param)	: symbolic		{ return symbolicMax.create(asSymbolic(a), asSymbolic(b)); }

	static sin(i: param)	: symbolic		{ return symbolicSin.create(asSymbolic(i)); }
	static cos(i: param)	: symbolic		{ return symbolicCos.create(asSymbolic(i)); }
	static tan(i: param)	: symbolic		{ return symbolicTan.create(asSymbolic(i)); }
	static asin(i: param)	: symbolic		{ return symbolicAsin.create(asSymbolic(i)); }
	static acos(i: param)	: symbolic		{ return symbolicAcos.create(asSymbolic(i)); }
	static atan(i: param)	: symbolic		{ return symbolicAtan.create(asSymbolic(i)); }
	static atan2(a: param, b: param): symbolic		{ return symbolicAtan2.create(asSymbolic(a), asSymbolic(b)); }

	static exp(i: param)	: symbolic		{ return symbolicExp.create(asSymbolic(i)); }
	static log(i: param)	: symbolic		{ return symbolicLog.create(asSymbolic(i)); }
	static sqrt(i: param)	: symbolic		{ return asSymbolic(i).pow(1 / 2); }

	static sinh(i: param)	: symbolic		{ return symbolicSinh.create(asSymbolic(i)); }
	static cosh(i: param)	: symbolic		{ return symbolicCosh.create(asSymbolic(i)); }
	static tanh(i: param)	: symbolic		{ return symbolicTanh.create(asSymbolic(i)); }
	static asinh(i: param)	: symbolic		{ return symbolicAsinh.create(asSymbolic(i)); }
	static acosh(i: param)	: symbolic		{ return symbolicAcosh.create(asSymbolic(i)); }
	static atanh(i: param)	: symbolic		{ return symbolicAtanh.create(asSymbolic(i)); }

	from(n: number | bigint) { return symbolic.from(Number(n)); }
	dup():		symbolic	{ return this; }

	// Covariant return type overrides
	substitute(map: Bindings): symbolic { return super.substitute(map) as symbolic; }
	expand(opts?: ExpandOptions): symbolic { return super.expand(opts) as symbolic; }
	collect(v: string|symbolic): symbolic[] {
		const r: symbolic[] = [];
		r[v === this ? 1 : 0] = this;
		return r;
	}
	factor(): symbolic { return super.factor() as symbolic; }
	visit(visitor: Visitor, parent?: symbolicBase): symbolic { return super.visit(visitor, parent) as symbolic; }

	sign(): number {
		const v = this.evaluate();
		return Math.sign(v);
	}
	lt(b: symbolic): boolean {
		const v1 = this.evaluate();
		const v2 = b.evaluate();
		return v1 < v2;
	}

	less(b: param): symbolicBoolean {
		return symbolicCompare.create(OpMasks.LT, this, asSymbolic(b));
	}
	greater(b: param): symbolicBoolean {
		return symbolicCompare.create(OpMasks.GT, this, asSymbolic(b));
	}

	sqrt():		symbolic	{ return this.pow(1 / 2); }
	abs():		symbolic	{ return symbolicAbs.create(this); }
	neg():		symbolic	{ return symbolicAdd.create([term(this, -1)]); }
	recip():	symbolic	{ return symbolicMul.create([factor(this, -1)]); }

	npow(b: number): symbolic {
		return b === 1 ? this : b === 0 ? one : symbolicMul.create([factor(this, b)]);
	}
	ipow(b: number): symbolic {
		return b === 1 ? this : b === 0 ? one : symbolicMul.create([factor(this, b)]);
	}
	rpow(n: number, d: number): symbolic {
		return this.npow(n / d);
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

	evaluate(_env?: Record<string, number>) :	number			{ return NaN; }
	evaluateT<T>(_ops: Operators<T>, _env?: Record<string, T>) 	:	T|undefined	{ return undefined; }
	derivative(_v: string):						symbolic		{ return zero;}
	valueOf():									number | bigint { return this.evaluate(); }
}

//-----------------------------------------------------------------------------
// constant
//-----------------------------------------------------------------------------

class symbolicConstant extends symbolic {
	static create(i: number): symbolic	{ return this.interner.intern(`c:${i}`, id => new symbolicConstant(id, i)); }

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
	_toString(opts: StringifyOptions):		string { return opts.printConst(this.value); }
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
	static create(name: string)	{ return this.interner.intern(`v:${name}`, id => new symbolicVariable(id, name)) as symbolic; }

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
	_toString() : string { return this.name; }
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

export interface term {
	item:	symbolic;
	coef:	number;
}
export function term(item: symbolic, coef = 1): term {
	return { item, coef };
}
export function termAsSymbolic(t: term): symbolic {
	return t.coef === 1 ? t.item : t.item.scale(t.coef);
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
			if (coef !== 0) {
				if (i.item.factors.length === 1 && i.item.factors[0].pow === 1) {
					const inner = i.item.factors[0].item;
					if (inner instanceof symbolicAdd) {
						num += inner.num * coef;
						for (const j of inner.terms)
							terms.push(term(j.item, j.coef * coef));
					} else {
						terms.push(term(i.item.factors[0].item, coef));
					}
				} else {
					terms.push(term(i.item.num === 1 ? i.item : symbolicMul.create(i.item.factors, 1), coef));
				}
			}

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
		return termAsSymbolic(nonzero[0]);

	// factor out GCD from term coefficients and constant
	const coeffs = nonzero.map(t => Math.abs(t.coef));
	if (num)
		coeffs.push(Math.abs(num));
	let g = 1;

	if (!coeffs.every(Number.isInteger)) {
		// for rational coefficients, find common denominator and factor GCD of numerators
		const denoms = coeffs.map(f => {
			// find denominator for common fractions (1/2, 1/3, 1/4, etc.)
			for (let d = 2; d <= 12; d++)
				if (isAlmostInteger(f * d))
					return d;
			return 1;
		});
		const lcmDenom = lcm(...denoms);
		
		// scale all coefficients to integers
		const scaled = coeffs.map(c => c * lcmDenom);
		if (scaled.every(isAlmostInteger))
			g = gcd2(...scaled.map(Math.round)) / lcmDenom;;
	} else  {
		g = gcd2(...coeffs);
	}
		
	// ensure leading term has positive coefficient
	if (nonzero[0].coef < 0)
		g = -g;
	
	if (g !== 1)
		return symbolicMul.create([factor(symbolicAdd.create(nonzero.map(t => term(t.item, t.coef / g)), num / g))], g);

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

function commonFactors(terms: readonly Readonly<term>[]): readonly term[] {
	const factors = getFactors(terms);
	const singles = factors.filter(i => i.terms.size > 1);
	const pairs: {a: factor, b: factor, terms: Set<term>}[] = [];

	singles.sort((a, b) => b.terms.size - a.terms.size);
	singles.length = Math.min(singles.length, 16);

	for (const a of singles) {
		for (const b of singles) {
			if (a === b)
				break;
			const terms = a.terms.intersection(b.terms);
			if (terms.size > 1)
				pairs.push({a, b, terms});
		}
	}

	pairs.sort((a, b) => b.terms.size - a.terms.size);
	pairs.length = Math.min(pairs.length, 16);


	// prefer candidates that cover more terms and that simplify their inner sum under trig rules
	const scorer = scoreFactory();
	const scoreCandidate = (terms: Set<term>, inner: symbolic) => terms.size * 10 - scorer(inner);
	
	function remove1Factor(t: term, a: factor) {
		if (t.item instanceof symbolicMul) {
			const newFactors = t.item.factors.map(f =>
				f.item === a.item ? factor(f.item, f.pow - a.pow)
				: f
			).filter(ff => ff.pow !== 0);
			return term(mulFactors(t.item.num, ...newFactors), t.coef);
		}
		return t.item === a.item ? term(one, t.coef) : t;
	}

	function remove2Factors(t: term, a: factor, b: factor) {
		if (t.item instanceof symbolicMul) {
			const newFactors = t.item.factors.map(f =>
				f.item === a.item ? factor(f.item, f.pow - a.pow)
			:   f.item === b.item ? factor(f.item, f.pow - b.pow)
			:   f
			).filter(f => f.pow !== 0);
			return term(mulFactors(t.item.num, ...newFactors), t.coef);
		}
		return t;
	}

	const factored: term[] = [];
	let remaining = new Set<term>(terms);

	while (remaining.size > 1) {
		// find best candidate
		type Candidate = { mul: symbolic; terms: Set<term>; inner: symbolic; score: number };
		let best: Candidate | undefined;

		// singles
		for (const f of singles) {
			const terms = f.terms.intersection(remaining);
			if (terms.size > 1)  {
				const inner = addTerms(0, ...Array.from(terms).map(t => remove1Factor(t, f)));
				const score = scoreCandidate(terms, inner);
				if (!best || score > best.score)
					best = { mul: factorAsSymbolic(f), terms, inner, score };
			}
		}

		// pairs
		for (const p of pairs) {
			const terms = p.terms.intersection(remaining);
			if (terms.size > 1)  {
				const inner = addTerms(0, ...Array.from(terms).map(t => remove2Factors(t, p.a, p.b)));
				const score = scoreCandidate(terms, inner);
				if (!best || score > best.score)
					best = { mul: factorAsSymbolic(p.a).mul(factorAsSymbolic(p.b)), terms, inner, score };
			}
		}

		if (!best)
			break;

		factored.push(term(best.inner.mul(best.mul)));
		remaining = remaining.difference(best.terms);
	}

	return [...factored, ...remaining];
}

export class symbolicAdd extends symbolic {
	static create(terms: readonly term[], num = 0) : symbolic	{
		return this.interner.intern(`a(${num ? `${num},` : ''}${terms.map(i => `${i.coef === 1 ? '' : i.coef === -1 ? '-' : i.coef}${i.item.id}`).join(',')})`, id => new symbolicAdd(id, terms, num));
	}

	constructor(id: string, public terms: readonly Readonly<term>[], public num = 0) {
		super(id);
	}
	neg(): symbolic {
		if (this.num === 0 && this.terms.length === 1 && this.terms[0].coef === -1)
			return this.terms[0].item;
		return addTerms(-this.num, ...this.terms.map(i => term(i.item, -i.coef)));
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
		// Try base class behavior (match through numeric factors)
		return super.match(node, bindings, opts);
	}

	visit(visitor: Visitor, parent?: symbolic): symbolic {
		return visit(this, () => addTerms(this.num, ...this.terms.map((i) => term(i.item.visit(visitor, this), i.coef))), visitor, parent) as symbolic;
	}

	substitute(map: Bindings) : symbolic {
		return addTerms(this.num, ...this.terms.map(i => term(i.item.substitute(map), i.coef)));
	}

	collect(v: string|symbolic): symbolic[] {
		const groups: symbolic[] = [symbolic.from(this.num)];
		if (typeof v === 'string')
			v = symbolic.variable(v);

		for (const t of this.terms) {
			const i = t.item;
			if (i instanceof symbolicMul) {
				let pow = 0;
				const remaining: factor[] = [];
				for (const ff of i.factors as factor[]) {
					if (ff.item === v)
						pow += ff.pow;
					else
						remaining.push(ff);
				}
				groups[pow] = (groups[pow] ??= zero).add(mulFactors(i.num * t.coef, ...remaining));

			} else if (i === v) {
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
		return this.terms.reduce((acc, curr) => {
			if (acc !== acc)	// NaN check
				return acc;
			return acc + (curr.item.evaluate(env) * curr.coef);
		}, this.num);
	}
	evaluateT<T>(ops: Operators<T>, env?: Record<string, T>) {
		return this.terms.reduce((acc, curr) => {
			if (!acc)
				return acc;
			const v = curr.item.evaluateT(ops, env);
			return v ? ops.add(acc, ops.mul(v, ops.from(curr.coef))) : v;
		}, ops.from(this.num) as T | undefined);
	}

	_toString(opts: StringifyOptions) : string {
		if (opts.direct) {
			return '(' + this.terms.map(t => printScaled(t.item._toString(opts), Math.abs(t.coef), opts)).join(opts.addChar)
			+ (this.num !== 0 ? opts.addChar + opts.printConst(this.num) : '')
			+ ')';
		}

		function termOrder(t: term): number {
			return t.item instanceof symbolicMul ? t.item.factors.reduce((acc, curr) => acc + curr.pow, 0) : 0;
		}

		const terms = this.terms.slice().sort((a, b) => termOrder(b) - termOrder(a));
		return (opts?.fromMul ? '(' : '') + terms.map((t, j) => {
			return	(j === 0 ? (t.coef < 0 ? '-' : '') : (t.coef < 0 ? opts.subChar : opts.addChar))
				+	printScaled(t.item._toString(opts), Math.abs(t.coef), opts);
		}).join('')
		+ (this.num !== 0 ? (this.num < 0 ? opts.subChar : opts.addChar) + opts.printConst(Math.abs(this.num)) : '')
		+ (opts?.fromMul ? ')' : '');
	}
}

//-----------------------------------------------------------------------------
// mul
//-----------------------------------------------------------------------------

export interface factor {
	item:	symbolic;
	pow:	number; 
}
export function factor(item: symbolic, pow = 1): factor {
	return { item, pow };
}
export function factorAsSymbolic(f: factor): symbolic {
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

	if (nonzero.length === 1 && num === 1)
		return factorAsSymbolic(nonzero[0]);

	return symbolicMul.create(nonzero, num);
}

export class symbolicMul extends symbolic {
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
		if (b === 0)
			return one;
		if (b === 1)
			return this;

		const num = this.num < 0 && !Number.isInteger(b) ? Math.abs(this.num) : this.num;
		return symbolicMul.create(this.factors.map(i => factor(i.item, i.pow * b)), num ** b);
	}
	scale(b: number): symbolic	{
		return	b === 0	? zero
			:	b === 1	? this
			:	this.num * b === 1 && this.factors.length === 1 && this.factors[0].pow === 1 ? this.factors[0].item
			:	symbolicMul.create(this.factors, this.num * b);
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
		// Try base class behavior (match through numeric factors)
		return super.match(node, bindings, opts);
	}
	substitute(map: Bindings) : symbolic {
		return mulFactors(this.num, ...this.factors.map(f => factor(f.item.substitute(map), f.pow)));
	}
	visit(visitor: Visitor, parent?: symbolic): symbolic {
		return visit(this, () => mulFactors(this.num, ...this.factors.map((f) => factor(f.item.visit(visitor, this), f.pow))), visitor, parent) as symbolic;
	}
	expand(opts?: ExpandOptions): symbolic {
		let factors = this.factors;
//		if ((opts?.depth ?? 2) > 1) {
		if (opts?.depth ?? 2 > 1) {
			const opts2	= opts?.depth ? { ...opts, depth: opts.depth - 1 } : opts;
			factors		= factors.map(f => factor(f.item.expand(opts2), f.pow));
		}

		const uses = opts?.uses;
		const checkUse = !uses ? ()=>true
		: (terms: readonly Readonly<term>[]) => {
			for (const t of terms) {
				//if (t.item.is('var') && uses.has(t.item.name))
				//	return true;
				if (t.item.is('mul') && t.item.factors.some(f => f.item.is('var') && uses.has(f.item.name)))
					return true;
			}
			return false;
		};

		const additive: symbolicAdd[] = [];
		const others: factor[] = [];

		for (const f of factors) {
			if (f.item instanceof symbolicAdd && f.pow > 0 && (!opts?.maxPow || f.pow < opts.maxPow) && Number.isInteger(f.pow) && checkUse(f.item.terms)) {
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
					parts2.push(p.mul(t.item).scale(t.coef));
				if (a.num !== 0)
					parts2.push(p.scale(a.num));
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
			if (acc !== acc || acc === 0)	// NaN check
				return acc;
			const v = curr.item.evaluate(env);
			return	curr.pow === 1 ? acc * v
				:	curr.pow === -1 ? acc / v
				:	acc * v ** curr.pow;
		}, this.num);
	}
	evaluateT<T>(ops: Operators<T>, env?: Record<string, T>) {
		return this.factors.reduce((acc, curr) => {
			if (!acc)
				return acc;
			const v = curr.item.evaluateT(ops, env);
			if (!v)
				return v;
			return curr.pow === 1 ? ops.mul(acc, v)
				:	curr.pow === -1 ? ops.div(acc, v)
				:	ops.mul(acc, ops.pow(v, ops.from(curr.pow)));
		}, ops.from(this.num) as T | undefined);
	}

	_toString(opts: StringifyOptions) : string {
		const factors	= opts.direct ? this.factors : this.factors.slice().sort((a, b) => b.pow - a.pow);

		function printTerm(item: symbolic, n: number) {
			const s = item._toString({...opts, fromMul: true});
			if (n === 1)
				return s;

			if (opts.ccode) {
				if (n === 0.5)
					return `sqrt(${s})`;
				if (Number.isInteger(n) && n < 3 && item.is('var'))
					return Array.from({length: n}, () => s).join(opts.mulChar);
				return `pow(${s}, ${opts.printConst(n)})`;
			}

			if (opts.radicalPower && n < 1 && isAlmostInteger(1 / n)) {
				const c = radicalChars[Math.round(1 / n)];
				if (c)
					return `${c}${s}`;
			}

			if (opts.superPower)
				return `${s}${toSuperscript(opts.printConst(n))}`;
			return `${s}^${opts.printConst(n)}`;
		}

		if (factors[0].pow < 0) {
			return opts.printConst(this.num) + factors.map(i =>
				(i.pow < 0 ? opts.divChar : opts.mulChar) + printTerm(i.item, Math.abs(i.pow))
			).join('');
		} else {
			return printScaled(factors.map((i, j) =>
				(j === 0 ? '' : i.pow < 0 ? opts.divChar : opts.mulChar) + printTerm(i.item, Math.abs(i.pow))
			).join(''), this.num, opts);
		}

	}
}

//-----------------------------------------------------------------------------
// symbolicBoolean - maintain in disjunctive normal form (DNF)
//-----------------------------------------------------------------------------

abstract class symbolicBoolean extends symbolicBase {
	abstract not(): symbolicBoolean;
	abstract and(b: symbolicBoolean): symbolicBoolean;
	abstract or(b: symbolicBoolean): symbolicBoolean;

	ifelse(t: param, f: param): symbolic {
		return combinePartitions(asSymbolic(f), partition(this, asSymbolic(t)));
	}

	evaluate(_env?: Record<string, number>)						: boolean|undefined	{ return; }
	evaluateT<T>(_ops: Operators<T>, _env?: Record<string, T>) 	: boolean|undefined	{ return; }
	visit(visitor: Visitor, parent?: symbolicBase): symbolicBoolean { return super.visit(visitor, parent) as symbolicBoolean; }
}

class symbolicFalse extends symbolicBoolean {
	static create()	{ return this.interner.intern(`false`, id => new symbolicFalse(id)); }
	constructor(id: string) { super(id); }
	not()					{ return symTrue; }
	and(_b: symbolicBoolean){ return this; }
	or(b: symbolicBoolean)	{ return b; }
}

class symbolicTrue extends symbolicBoolean {
	static create()	{ return this.interner.intern(`true`, id => new symbolicTrue(id)); }
	constructor(id: string) { super(id); }
	not()					{ return symFalse; }
	and(b: symbolicBoolean)	{ return b; }
	or(_b: symbolicBoolean)	{ return this; }
}

const symTrue = symbolicTrue.create();
const symFalse = symbolicFalse.create();

const OpMasks = {
	EQ:	1,
	LT:	2,
	GT:	4,
	FALSE: 0,
	TRUE: 7,
	NE: 6,
};

const Ops = {
	'=':	OpMasks.EQ,
	'!=':	OpMasks.EQ ^ OpMasks.TRUE,
	'<':	OpMasks.LT,
	'>=':	OpMasks.LT ^ OpMasks.TRUE,
	'>':	OpMasks.GT,
	'<=':	OpMasks.GT ^ OpMasks.TRUE,
	'false': OpMasks.FALSE,
	'true':	OpMasks.TRUE,
} as const;

type Ops = typeof Ops[keyof typeof Ops];
const OpNames = Object.fromEntries(Object.entries(Ops).map(([k, v]) => [v, k]));

function reverseOp(op: Ops): Ops {
	return	(op & OpMasks.LT ? OpMasks.GT : 0)
		| 	(op & OpMasks.GT ? OpMasks.LT : 0)
		|	(op & OpMasks.EQ);
}

interface compareConsts {
	opL:	Ops;
	opH:	Ops;
	compL:	symbolicCompare;
	compH:	symbolicCompare;
}

function compareConsts(a: symbolicCompare, b: symbolicCompare): compareConsts | undefined {
	function inner(opA: Ops, opB: Ops, vA: number, vB: number) : compareConsts {
		return vA < vB
			? {opL: opA, opH: opB, compL: a, compH: b}
			: {opL: opB, opH: opA, compL: b, compH: a};
	}

	if (a.a === b.a) {
		if (isConst(a.b) && isConst(b.b))
			return inner(a.op, b.op, a.b.value, b.b.value);

	} else if (a.b === b.b) {
		if (isConst(a.a) && isConst(b.a))
			return inner(reverseOp(a.op), reverseOp(b.op), a.a.value, b.a.value);
	}
}

function andConsts(c: compareConsts): symbolicBoolean | undefined {
	// Equalities: if one is EQ, check if value satisfies the other
	if (c.opL === OpMasks.EQ)
		return c.opH & OpMasks.LT ? c.compL : symFalse;
	if (c.opH === OpMasks.EQ)
		return c.opL & OpMasks.GT ? c.compH : symFalse;

	// Inequalities: != with any bound returns the bound
	if (c.opL === OpMasks.NE)
		return c.compH;
	if (c.opH === OpMasks.NE)
		return c.compL;

	const opO = c.opL | c.opH;

	// Same direction: take the stronger bound
	if (!(opO & OpMasks.GT))
		return c.compL; // both ≤: take min
	if (!(opO & OpMasks.LT))
		return c.compH; // both ≥: take max

	// Opposite directions: check if satisfiable
	if (!(c.opL & c.opH & ~OpMasks.EQ))
		return symFalse; // strict bounds don't meet
}

function orConsts(c: compareConsts): symbolicBoolean | undefined {
	// Equalities: check if one subsumes the other
	if (c.opL === OpMasks.EQ)
		return c.opH & OpMasks.LT ? c.compH : c.compL;
	if (c.opH === OpMasks.EQ)
		return c.opL & OpMasks.GT ? c.compL : c.compH;

	// Inequalities: often cover everything or subsume the other
	if (c.opL === OpMasks.NE)
		return c.opH & OpMasks.LT ? symTrue : c.compL;
	if (c.opH === OpMasks.NE)
		return c.opL & OpMasks.GT ? symTrue : c.compH;

	const opO = c.opL | c.opH;

	// Same direction: take the weaker bound
	if (!(opO & OpMasks.GT))
		return c.compH; // both ≤: take max
	if (!(opO & OpMasks.LT))
		return c.compL; // both ≥: take min

	// Opposite directions: usually covers everything
	if ((c.opL & OpMasks.GT) && (c.opH & OpMasks.LT))
		return symTrue;
}

export class symbolicCompare extends symbolicBoolean {
	static create(op: Ops, a: symbolic, b: symbolic)	{
		if (a.id > b.id)
			[a, b, op] = [b, a, reverseOp(op)];
		return this.interner.intern(`${OpNames[op]}:${a.id},${b.id}`, id => new symbolicCompare(id, a, b, op));
	}

	constructor(id: string, public a: symbolic, public b: symbolic, public op: Ops) {
		super(id);
	}
	not(): symbolicBoolean {
		return symbolicCompare.create(this.op ^ OpMasks.TRUE, this.a, this.b);
	}

	and(b: symbolicBoolean): symbolicBoolean {
		if (!(b instanceof symbolicCompare))
			return b.and(this);

		if (this.a === b.a && this.b === b.b) {
			const opA = this.op & b.op;
			return opA ? symbolicCompare.create(opA, this.a, this.b) : symFalse;
		}

		const consts = compareConsts(this, b);
		return (consts && andConsts(consts)) || symbolicAnd.create([this, b]);
	}
	or(b: symbolicBoolean): symbolicBoolean {
		if (!(b instanceof symbolicCompare))
			return b.or(this);

		if (this.a === b.a && this.b === b.b) {
			const opO = this.op | b.op;
			return opO === OpMasks.TRUE ? symTrue : symbolicCompare.create(opO, this.a, this.b);
		}

		const consts = compareConsts(this, b);
		return (consts && orConsts(consts)) || symbolicOr.create([this, b]);
	}

	evaluate(env?: Record<string, number>) {
		const a = this.a.evaluate(env);
		const b = this.b.evaluate(env);
		switch (this.op) {
			case Ops['=']:	return a === b;
			case Ops['!=']:	return a !== b;
			case Ops['<']:	return a < b;
			case Ops['<=']:	return a <= b;
			case Ops['>']:	return a > b;
			case Ops['>=']:	return a >= b;
		}
	}

	evaluateT<T>(ops: Operators<T>, env?: Record<string, T>) {
		const a = this.a.evaluateT(ops, env);
		if (!a)
			return;
		const b = this.b.evaluateT(ops, env);
		if (!b)
			return;
		switch (this.op) {
			case Ops['=']:	return ops.eq(a, b);
			case Ops['!=']:	return !ops.eq(a, b);
			case Ops['<']:	return ops.lt(a, b);
			case Ops['<=']:	return !ops.lt(b, a);
			case Ops['>']:	return ops.lt(b, a);
			case Ops['>=']:	return !ops.lt(a, b);
		}
	}

	_toString(opts: StringifyOptions): string {
		return `${this.a._toString(opts)} ${OpNames[this.op]} ${this.b._toString(opts)}`;
	}
}
/*
class symbolicNot extends symbolicBoolean {
	static create(a: symbolicBoolean)	{ return this.interner.intern(`~:${a.id}`, id => new symbolicNot(id, a)); }
	constructor(id: string, public a: symbolicBoolean) {
		super(id);
	}
	not(): symbolicBoolean {
		return this.a;
	}
	and(b: symbolicBoolean): symbolicBoolean {
		return	b instanceof symbolicCompare ? symbolicAnd.create([this, b])
			:	b.and(this);
	}
	or(b: symbolicBoolean): symbolicBoolean {
		return	b instanceof symbolicCompare ? symbolicOr.create([this, b])
			:	b.or(this);
	}
}
	*/
class symbolicAnd extends symbolicBoolean {
	static create(terms: symbolicBoolean[])	{
		terms.sort((a, b) => compare(a.id, b.id));
		return this.interner.intern(`&:${terms.map(t => t.id).join(',')}`, id => new symbolicAnd(id, terms));
	}
	constructor(id: string, public terms: symbolicBoolean[]) {
		super(id);
	}
	not(): symbolicBoolean {
		return symbolicOr.create(this.terms.map(t => t.not()));
	}
	and(b: symbolicBoolean): symbolicBoolean {
		return	b instanceof symbolicAnd	? symbolicAnd.create([...this.terms, ...b.terms])
			:	b instanceof symbolicOr		? symbolicOr.create(b.terms.map(t => this.and(t)))
			:	symbolicAnd.create([this, b]);
	}
	or(b: symbolicBoolean): symbolicBoolean {
		return	b instanceof symbolicAnd	? symbolicOr.create([b, this])
			:	b instanceof symbolicOr		? symbolicOr.create(b.terms.map(t => this.or(t)))
			:	symbolicOr.create([this, b]);
	}
	_toString(opts: StringifyOptions): string {
		return `(${this.terms.map(t => t._toString(opts)).join(' && ')})`;
	}
}
class symbolicOr extends symbolicBoolean {
	static create(terms: symbolicBoolean[])	{
		terms.sort((a, b) => compare(a.id, b.id));
		return this.interner.intern(`|:${terms.map(t => t.id).join(',')}`, id => new symbolicOr(id, terms));
	}
	constructor(id: string, public terms: symbolicBoolean[]) {
		super(id);
	}
	not(): symbolicBoolean {
		return symbolicAnd.create(this.terms.map(t => t.not()));
	}
	and(b: symbolicBoolean): symbolicBoolean {
		return	b instanceof symbolicAnd	? symbolicOr.create(this.terms.map(t => t.and(b)))
			:	b instanceof symbolicOr		? symbolicOr.create(this.terms.flatMap(t1 => b.terms.map(t2 => t1.and(t2))))
			:	symbolicOr.create(this.terms.map(t => t.and(b)));
	}
	or(b: symbolicBoolean): symbolicBoolean {
		return	b instanceof symbolicAnd	? symbolicOr.create([...this.terms, b])
			:	b instanceof symbolicOr		? symbolicOr.create([...this.terms, ...b.terms])
			:	symbolicOr.create([...this.terms, b]);
	}
	_toString(opts: StringifyOptions): string {
		return `(${this.terms.map(t => t._toString(opts)).join(' || ')})`;
	}
}

//-----------------------------------------------------------------------------
// partition
//-----------------------------------------------------------------------------


interface partition {
	bool:	symbolicBoolean;
	value:	symbolic;
}
function partition(bool: symbolicBoolean, value: symbolic): partition {
	return { bool, value };
}

export function combinePartitions(otherwise: symbolic, ...p: readonly Readonly<partition>[]): symbolic {
	const partitions: partition[] = [];
	const trues: symbolic[] = [];

	if (otherwise instanceof symbolicPartition) {
		partitions.push(...otherwise.partitions);
		otherwise = otherwise.otherwise;
	}

	for (const i of p) {
		if (i.value instanceof symbolicPartition) {
			i.value.partitions.forEach(sub => {
				partitions.push(partition(i.bool.and(sub.bool), sub.value));
			});
			partitions.push(partition(i.bool, i.value.otherwise));
		} else {
			partitions.push(partition(i.bool, i.value));	// don't use original (?)
		}
	}

	// canonical order
	partitions.sort((a, b) => compare(a.value.id, b.value.id));

		// combine like terms by summing coefficients
	const combined: partition[] = [];
	for (const t of partitions) {
		const last = combined.at(-1);
		if (last && last.value === t.value)
			last.bool = last.bool.or(t.bool);
		else
			combined.push(t);
	}

	// remove zero coefficients
	const nonzero = combined.filter(t => {
		if (t.bool === symTrue) {
			trues.push(t.value);
			return false;
		}
		return t.bool !== symFalse;
	});

	if (trues.length > 0) {
		if (!trues.every(v => v === trues[0]))
			throw new Error('Conflicting partition true values');
		return trues[0];
	}

	if (nonzero.length === 0)
		return otherwise;

	return symbolicPartition.create(nonzero, otherwise);
}

class symbolicPartition extends symbolic {

	static create(partitions: readonly Readonly<partition>[], otherwise: symbolic)	{
		return this.interner.intern(`p(${partitions.map(i => `${i.bool.id}?${i.value.id}`).join(':')}:${otherwise.id})`, id => new symbolicPartition(id, partitions, otherwise));
	}

	constructor(id: string, public partitions: readonly Readonly<partition>[], public otherwise: symbolic) {
		super(id);
	}

	visit(visitor: Visitor, parent?: symbolic):		symbolic	{
		return visit(this,
			() => symbolicPartition.create(this.partitions.map(p => partition(
				p.bool.visit(visitor, this), p.value.visit(visitor, this)
			)), this.otherwise.visit(visitor, this)),
			visitor, parent
		) as symbolic;
	}
	evaluate(env?: Record<string, number>) :	number			{
		for (const p of this.partitions) {
			if (p.bool.evaluate(env))
				return p.value.evaluate(env);
		}
		return this.otherwise.evaluate(env);
	}
	evaluateT<T>(ops: Operators<T>, env?: Record<string, T>) : T|undefined	{
		for (const p of this.partitions) {
			if (p.bool.evaluateT(ops, env))
				return p.value.evaluateT(ops, env);
		}
		return this.otherwise.evaluateT(ops, env);
	}
	derivative(v: string):						symbolic		{
		return symbolicPartition.create(this.partitions.map(p => partition(p.bool, p.value.derivative(v))), this.otherwise.derivative(v));
	}
	_toString(opts: StringifyOptions):			string			{
		return '(' + this.partitions.map(p => 
			`${p.bool.toString(opts)} ? ${p.value.toString(opts)}`
		).join(' : ') + ' : ' + this.otherwise.toString(opts) + ')';
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
		_toString(): string	{ return toString; }
	};
	return C;
}

const i			= new (specialConstant('i', '𝑖'));
const e			= new (specialConstant('e', '𝑒', Math.E));
//const i			= new (specialConstant('𝑖', '𝑖'));
//const e			= new (specialConstant('𝑒', '𝑒', Math.E));
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

const infinity: symbolic	= new symbolicInfinity;

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
	toString = (a: symbolic, opts: StringifyOptions) => `${name}(${a._toString(opts)})`
) {
	const C = class extends unaryFunctionBase {
		static create(i: symbolic) : symbolic	{
			return this.interner.intern(`${name}:${i.id}`, id => new C(id, i));
		}
		match(node: symbolic, bindings: Bindings, opts?: MatchOptions): Bindings | null {
			return node instanceof C && this.arg.match(node.arg, bindings, opts) ? bindings : null;
		}
		visit(visitor: Visitor, parent?: symbolic): symbolic {
			return visit(this, () => this.create(this.arg.visit(visitor, this)), visitor, parent) as symbolic;
		}
		substitute(map: Bindings) : symbolic {
			return this.create(this.arg.substitute(map));
		}
		derivative(v: string): symbolic { return derivative(this.arg).mul(this.arg.derivative(v)); }
		evaluate(env?: Record<string, number>): number { return evaluate(this.arg.evaluate(env)); }
		evaluateT<T>(ops: Operators<T>, env?: Record<string, T>) { return ops.func(name, [this.arg.evaluateT(ops, env)!]); }

		_toString(opts: StringifyOptions): string { return toString(this.arg, {...opts, fromMul: false}); }
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
	toString = (a: symbolic, b: symbolic, opts: StringifyOptions) => `${name}(${a._toString(opts)}, ${b._toString(opts)})`
) {
	const C = class extends binaryFunctionBase {
		static create(i: symbolic, j: symbolic): symbolic	{
			return this.interner.intern(`${name}:${i.id}:${j.id}`, id => new C(id, i, j));
		}
		match(node: symbolic, bindings: Bindings, opts?: MatchOptions): Bindings | null {
			return node instanceof C && this.arg1.match(node.arg1, bindings, opts) && this.arg2.match(node.arg2, bindings, opts) ? bindings : null;
		}
		visit(visitor: Visitor, parent?: symbolic): symbolic {
			return visit(this, () => this.create(this.arg1.visit(visitor, this), this.arg2.visit(visitor, this)), visitor, parent) as symbolic;
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

		_toString(opts: StringifyOptions): string { return toString(this.arg1, this.arg2, {...opts, fromMul: false}); }
	};
	return C;
}

// basic

const symbolicAbs	= unaryFunction('abs', Math.abs, a => symbolicSign.create(a), a => `|${a}|`);
const symbolicFloor	= unaryFunction('floor', Math.floor, _a => zero, a => `⌊${a}⌋`);
const symbolicCeil	= unaryFunction('ceil', Math.ceil, _a => zero, a => `⌈${a}⌉`);
const symbolicTrunc	= unaryFunction('trunc', Math.trunc, _a => zero);
const symbolicRound	= unaryFunction('round', Math.round, _a => zero);

const symbolicMod	= binaryFunction('mod', (a, b) => a % b, (a, b) => [one, symbolicFloor.create(a.div(b)).neg()], (a, b, opts) => `mod(${a._toString(opts)}, ${b._toString(opts)})`);
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
	(a, b, opts) => opts.ccode ? `pow(${a._toString(opts)}, ${b._toString(opts)})` : `(${a._toString(opts)}) ^ (${b._toString(opts)})`
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
		match: (node, opts) => pattern.match(node, {}, opts),
		replace: bs => replaceRest(bs, replace(bs)),
		guard,
	};
}

function bind(name: string) {
	return symbolicMatcher.create(name);
}

const A = bind('A');
const B = bind('B');

export const generalRules: Rule[] = [
	{
		name: 'mul-distribute',
		match(node: symbolic) {
			if (node instanceof symbolicMul) {
				const result = node.expand({ depth: 1, maxPow: 4 });
				if (result !== node)
					return {result};
			}
			return null;
		},
		replace(bs: Bindings): symbolic {
			return bs.result;
		}
	},
	{
		name: 'collect-like-terms',
		match(node: symbolic) {
			// Only factor expressions where terms share multiplicative factors
			// Skip simple additions like A+B that have no common structure
			if (!(node instanceof symbolicAdd) || node.terms.length < 2)
				return null;
			
			// Check if any terms share factors (otherwise factoring makes no sense)
			const hasMul = node.terms.some(t => t.item instanceof symbolicMul);
			if (!hasMul)
				return null;
				
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
		A.mul(B).add(A.mul(C)),
		bs => bs.A.mul(bs.B.add(bs.C))
	),*/
	
	// Perfect square trinomial: A^2 + 2AB + B^2 -> (A+B)^2
	PatternRule('perfect-square-sum',
		A.pow(2).add(A.mul(B).scale(2)).add(B.pow(2)),
		bs => bs.A.add(bs.B).pow(2)
	),
	// Perfect square trinomial: A^2 - 2AB + B^2 -> (A-B)^2
	PatternRule('perfect-square-diff',
		A.pow(2).sub(A.mul(B).scale(2)).add(B.pow(2)),
		bs => bs.A.sub(bs.B).pow(2)
	),
];

export const trigRules: Rule[] = [
	PatternRule('sin-sum',
		symbolic.sin(A.add(B)),
		bs => symbolic.sin(bs.A).mul(symbolic.cos(bs.B)).add(symbolic.cos(bs.A).mul(symbolic.sin(bs.B)))
	),
	PatternRule('sin-diff',
		symbolic.sin(A.sub(B)),
		bs => symbolic.sin(bs.A).mul(symbolic.cos(bs.B)).sub(symbolic.cos(bs.A).mul(symbolic.sin(bs.B)))
	),
	PatternRule('sin-half-angle',
		symbolic.sin(A.scale(0.5)),
		bs => symbolic.sqrt(one.sub(symbolic.cos(bs.A)).scale(0.5))
	),

	PatternRule('cos-sum',
		symbolic.cos(A.add(B)),
		bs => symbolic.cos(bs.A).mul(symbolic.cos(bs.B)).sub(symbolic.sin(bs.A).mul(symbolic.sin(bs.B)))
	),
	PatternRule('cos-diff',
		symbolic.cos(A.sub(B)),
		bs => symbolic.cos(bs.A).mul(symbolic.cos(bs.B)).add(symbolic.sin(bs.A).mul(symbolic.sin(bs.B)))
	),
	PatternRule('cos-half-angle',
		symbolic.cos(A.scale(0.5)),
		bs => symbolic.sqrt(one.add(symbolic.cos(bs.A)).scale(0.5))
	),

	PatternRule('tan-sum',
		symbolic.tan(A.add(B)),
		bs => symbolic.tan(bs.A).add(symbolic.tan(bs.B)).div(one.sub(symbolic.tan(bs.A).mul(symbolic.tan(bs.B))))
	),
	PatternRule('tan-diff',
		symbolic.tan(A.sub(B)),
		bs => symbolic.tan(bs.A).sub(symbolic.tan(bs.B)).div(one.add(symbolic.tan(bs.A).mul(symbolic.tan(bs.B))))
	),
	PatternRule('tan-half-angle',
		symbolic.tan(A.scale(0.5)),
		bs => symbolic.sin(bs.A).scale(0.5).div(symbolic.cos(bs.A).scale(0.5))
	),

	PatternRule('sin-cos-pythag',
		symbolic.sin(A).pow(2).add(symbolic.cos(A).pow(2)),
		_ => one
	),
	PatternRule('tan-secant-pythag',
		symbolic.tan(A).pow(2).add(one),
		bs => symbolic.cos(bs.A).pow(-2)
	),
	PatternRule('cot-cosecant-pythag',
		one.add(symbolic.tan(A).pow(-2)),
		bs => symbolic.sin(bs.A).pow(-2)
	),
	/*
	PatternRule('sin-cos-product',
		symbolic.sin(A).mul(symbolic.cos(B)),
		bs => symbolic.sin(bs.A.add(bs.B)).mul(symbolic.cos(bs.A.sub(bs.B))).scale(0.5)
	),
	*/

	// double-angle compression: sin(x) * cos(x) -> 1/2 * sin(2x)
	PatternRule('double-angle-sin-compress',
		symbolic.sin(A).mul(symbolic.cos(A)),
		bs => symbolic.sin(bs.A.scale(2)).scale(0.5)
	),

	// atan2 basic properties
	PatternRule('atan2-to-atan',
		symbolic.atan2(B, A),
		bs => symbolic.atan(bs.B.div(bs.A)),
		bs => !isConst(bs.A) || bs.A.value > 0  // only when x > 0
	),

	// atan2 symmetry
	PatternRule('atan2-negate-both',
		symbolic.atan2(B.neg(), A.neg()),
		bs => symbolic.atan2(bs.B, bs.A)
	),

	// atan2 scale invariance
	PatternRule('atan2-scale-invariance',
		symbolic.atan2(B.mul(bind('K')), A.mul(bind('K'))),
		bs => symbolic.atan2(bs.B, bs.A),
		bs => !isConst(bs.K) || bs.K.value > 0  // only for positive K
	),

	// atan2 of zero
	PatternRule('atan2-zero-y',
		symbolic.atan2(zero, A),
		_ => zero
	),

	// atan2 special angles
	PatternRule('atan2-x-x',
		symbolic.atan2(A, A),
		_ => pi.scale(1/4)
	),
	PatternRule('atan2-x-neg-x',
		symbolic.atan2(A, A.neg()),
		_ => pi.scale(3/4)
	),

	// Trigonometric function composition
	//PatternRule('cos-atan2-div-const',
	//	symbolic.cos(symbolic.atan2(B, A).div(bind('N'))),
	//	bs => {
	//		if (!isConst(bs.N))
	//			return undefined;
	//		const n = bs.N.value;
	//		if (n === 3) {
	//			// This is the specific case in cubic formula: cos(atan2(y,x)/3)
	//			// For the cubic formula, when the discriminant is positive, this can be related to
	//			// the cube root structure, but it's complex to simplify generally
	//			return undefined;  // or return a specialized form if you develop one
	//		}
	//		return undefined;
	//	},
	//	bs => isConst(bs.N) && bs.N.value === 3
	//}

];
// For your specific cubic case, the key insight is:
// atan2(sqrt(Δ³/27 + q²/4), -q/2) relates to acos(-q/2 / sqrt(p³/27))
// when Δ > 0 (three real roots)

export const invTrigRules: Rule[] = [
	PatternRule('sin-sum',
		symbolic.sin(A).mul(symbolic.cos(B)).add(symbolic.cos(A).mul(symbolic.sin(B))),
		bs => symbolic.sin(bs.A.add(bs.B)),
	),
	/*
	PatternRule( 'sin-cos-product',
		symbolic.sin(A.add(B)).mul(symbolic.cos(A.sub(B))),
		bs => symbolic.sin(bs.A).mul(symbolic.cos(bs.B))
	),
	*/

	// double-angle expansion: sin(2x) -> 2 * sin(x) * cos(x)
	PatternRule('double-angle-sin-expand',
		symbolic.sin(A.scale(2)),
		bs => symbolic.from(2).mul(symbolic.sin(bs.A).mul(symbolic.cos(bs.A)))
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
/*
import {polynomialT} from "./polynomial";


export function resultantParametric<T extends scalar0<T>>(ops: Operators<T>,p: polynomialT<symbolic>, q: polynomialT<symbolic>, varName: string, fixedEnv: Record<string, T> = {}, maxDegree = -1) {
	const coeffAt = (c: symbolic, t: T) => c.evaluateT(ops, { ...fixedEnv, [varName]: t });
	const v = symbolic.variable(varName);

	const pd = p.degree();
	const qd = q.degree();
//	if (pd < 0 || qd < 0)
//		return new polynomialT<U>([]);

	// choose number of samples: default to pd*qd + 1
	//const degGuess = maxDegree > -1 ? maxDegree : Math.max(0, pd * qd);
	//const samples = degGuess + 1;
	const pts: [T, T][] = [];
	for (const x of samples) {
		const a = p.c.map(c => coeffAt(c, x));
		const b = q.c.map(c => coeffAt(c, x));
		const M = sylvesterMatrix(a, b, zero);
		const { swaps } = LUDecomposeBareissPivotT(M, true);
		const r = swaps & 1 ? M[M.length - 1][M.length - 1].neg() : M[M.length - 1][M.length - 1];
		pts.push([x, r]);
	}

	return interpolateT(pts, zero.from(1)); // coefficients numeric low->high
//	return resultantEvalInterp(p, q, v, coeffAt, maxDegree);
}
*/

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
	//sin: 	symbolicSin,
	//cos: 	symbolicCos,
	//tan: 	symbolicTan,
	//asin: 	symbolicAsin,
	//acos: 	symbolicAcos,
	//atan: 	symbolicAtan,
	//atan2: 	symbolicAtan2,
	//exp: 	symbolicExp,
	//log: 	symbolicLog,
	//sinh: 	symbolicSinh,
	//cosh: 	symbolicCosh,
	//tanh: 	symbolicTanh,
	//asinh: 	symbolicAsinh,
	//acosh: 	symbolicAcosh,
	//atanh: 	symbolicAtanh,
	//pow: 	symbolicPow
} as const;
