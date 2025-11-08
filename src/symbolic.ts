/* eslint-disable no-restricted-syntax */
import {ops, compare, toSuperscript, isAlmostInteger} from './core';

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
export type Visitor = (node: symbolic) => symbolic;
export type Prematch = (a: symbolic, b: symbolic, bindings: Bindings) => Bindings | null;

export type Rule = {
	name:		string;
	match:		(node: symbolic, compare: Prematch) => Bindings | null;
	replace:	(bs: Bindings) => symbolic;
	guard?: 	(bs: Bindings) => boolean;
};

type ExpandOptions = {
	limit?: 'small' | 'polynomial';
};

type StringifyOptions = {
	//fractions?: false | { char?: boolean; superSub?: boolean };
	//radicals?: boolean;
	//upright?: boolean;
	//parentheses?: 'all' | 'minimal' | 'none';
	fromMul?: boolean;
	superPower?: boolean;
	printConst: (n: number) => string;
};


function printConst(n: number, opts?: StringifyOptions) {
	return opts?.printConst?.(n) ?? n.toString();
}

function printPower(n: number, opts?: StringifyOptions) {
	const s = printConst(n, opts);
	if (opts?.superPower)
		return toSuperscript(s);
	return `^${s}`;
//	if (opts?.superPower === false)
//		return `^${s}`;
//	return toSuperscript(s);
}

type param = number|symbolic;

export class symbolic implements ops<symbolic> {
	static interner = new Interner<symbolic>();

	static from(i: number)			{ return symbolicConstant.create(i); }
	static variable(name: string)	{ return symbolicVariable.create(name); }
	static get zero()				{ return zero; }
	static get one()				{ return one; }
	static get i()					{ return i; }
	static get e()					{ return e; }
	static get pi()					{ return pi; }
	static get infinity()			{ return infinity; }

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

	neg():		symbolic		{ return symbolicAdd.create([term(this, -1)]); }
	recip():	symbolic		{ return symbolicMul.create([factor(this, -1)]); }
	pow(b: param): symbolic {
		if (typeof b === 'number')
			return b === 1 ? this : b === 0 ? one : symbolicMul.create([factor(this, b)]);
		return symbolicPow.create(this, b);
	}

	scale(b: number): symbolic	{
		return (b === 0 ? zero : b === 1 ? this : symbolicMul.create([factor(this)], b));
		//return (b === 0 ? zero : b === 1 ? this : symbolicAdd.create([term(this, b)]));
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
	mag(): number				{ const v = this.evaluate(); return Number.isFinite(v) ? Math.abs(v) : NaN; }
	eq(b: symbolic) : boolean	{ return this === b;}

	substitute(_map: Bindings):			symbolic	{ return this; }
	expand(_options?: ExpandOptions):	symbolic	{ return this; }
	collect(_v: string):				symbolic[]	{ return [this]; }
	factor():							symbolic	{ return this; }
	visit(apply: Visitor, pre?: Visitor):symbolic	{ return apply(this); }

	match(node: symbolic, prematch: Prematch, bindings: Bindings): Bindings | null {
		return prematch(this, node, bindings);
	}

	evaluate(_env?: Record<string, number>) :	number		{ return NaN; }
	derivative(_v: string):						symbolic	{ return zero;}
	toString(_opts?: StringifyOptions):			string		{ return this.id; }
}

function visit(node: symbolic, apply: Visitor, pre?: Visitor):symbolic	{
	if (pre)
		node = pre(node);
	return node.visit(apply, pre);
}


//-----------------------------------------------------------------------------
// constant
//-----------------------------------------------------------------------------

class symbolicConstant extends symbolic {
	static create(i: number)	{ return this.interner.intern(`c:${i}`, id => new symbolicConstant(id, i)); }

	constructor(id: string, public value: number) {
		super(id);
	}
	neg(): symbolic				{ return symbolicConstant.create(-this.value); }
	recip(): symbolic			{ return symbolicConstant.create(1 / this.value); }
	pow(b: param): symbolic	{
		return typeof b === 'number' ? symbolicConstant.create(this.value ** b)
			: symbolicPow.create(this, b);
	}

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
	toString() : string { return this.name; }
}

class symbolicMatcher extends symbolicVariable {
	static create(name: string)	{ return this.interner.intern(`*:${name}`, id => new symbolicMatcher(id, name)); }

	match(node: symbolic, prematch: Prematch, bindings: Bindings): Bindings | null {
		const got = bindings[this.name];
		if (got)
			return prematch(got, node, bindings);

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

function addTerms(num: number, ...a: term[]): symbolic {
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
				terms.push(term(i.item.num === 1 ? i.item : symbolicMul.create(i.item.factors, 1), coef));

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

function commonFactors(terms: term[]) {
	interface factorInfo {
		item: symbolic;
		count: number;
		minpow: number;
		maxpow: number;
	}
	const factors: Record<string, factorInfo> = {};

	for (const m of terms) {
		if (m.item instanceof symbolicMul) {
			for (const i of m.item.factors) {
				const info = factors[i.item.id] ??= { item: i.item, count: 0, minpow: Infinity, maxpow: -Infinity };
				++info.count;
				info.minpow = Math.min(info.minpow, i.pow);
				info.maxpow = Math.max(info.maxpow, i.pow);
			}
		}
	}

	const factors2 = Object.values(factors).filter(i => i.count > 1).sort((a, b) => a.count - b.count);

	if (factors2.length > 0) {
		const factored: term[] = [];
		let from = terms;
		for (const f of factors2) {
			const terms1: term[] = [];
			const terms2: term[] = [];
			
			for (const i of from) {
				if (i.item instanceof symbolicMul && i.item.factors.find(j => j.item === f.item)) {
					const factors = i.item.factors.map(j => j.item === f.item ? factor(j.item, j.pow - f.minpow) : j).filter(i => i.pow !== 0);
					terms2.push(term(mulFactors(i.item.num, ...factors), i.coef));
				} else {
					terms1.push(i);
				}
			}
			factored.push(term(addTerms(0, ...terms2).mul(f.item.pow(f.minpow))));
			from = terms1;
		}
		terms.length = 0;
		terms.push(...factored, ...from);
	}
}

class symbolicAdd extends symbolic {
	static create(terms: term[], num = 0) : symbolic	{
		return this.interner.intern(`a(${num ? `${num},` : ''}${terms.map(i => `${i.coef === 1 ? '' : i.coef === -1 ? '-' : i.coef}${i.item.id}`).join(',')})`, id => new symbolicAdd(id, terms, num));
	}

	constructor(id: string, public terms: term[], public num = 0) {
		super(id);
	}
	neg(): symbolic {
		if (this.num === 0 && this.terms.length === 1 && this.terms[0].coef === -1)
			return this.terms[0].item;
		return symbolicAdd.create(this.terms.map(i => term(i.item, -i.coef)), -this.num);
	}
	/*
	scale(b: number): symbolic {
		return b === 0 ? zero : b === 1 ? this
			//: this.terms.length === 1 ? symbolicAdd.create([term(this.terms[0].item, this.terms[0].coef * b)], this.num * b)
			: mulFactors(b, factor(this));
			//: symbolicAdd.create(this.terms.map(i => term(i.item, i.coef * b)), this.num * b);
	}*/

	add(b: param): symbolic	{
		return  typeof b === 'number' ? symbolicAdd.create(this.terms, this.num + b)
			: 	isConst(b) ? symbolicAdd.create(this.terms, this.num + b.value)
			: 	b instanceof symbolicAdd ? addTerms(this.num + b.num, ...this.terms, ...b.terms)
			:	addTerms(this.num, ...this.terms, term(b, 1));
	}

	match(node: symbolic, prematch: Prematch, bindings: Bindings): Bindings | null {
		if (prematch(this, node, bindings))
			return bindings;

		if (node instanceof symbolicAdd) {
			if (this.num !== node.num)
				return null;

			const nterms = node.terms.slice();
			for (const pterm of this.terms) {
				let found = -1;
				for (const nterm of nterms) {
					if (pterm.coef === nterm.coef && pterm.item.match(nterm.item, prematch, bindings)) {
						found = nterms.indexOf(nterm);
						break;
					}
				}
				if (found === -1)
					return null;

				nterms.splice(found, 1);
			}
			return nterms.length === 0 ? bindings : null;
		}
		return null;
	}

	visit(apply: Visitor, pre: Visitor): symbolic {
		if (pre) {
			const p = pre(this);
			if (p !== this)
				return apply(addTerms(this.num, ...this.terms.map(i => term(i.item.visit(apply, pre), i.coef))));
		}
		return apply(addTerms(this.num, ...this.terms.map(i => term(i.item.visit(apply, pre), i.coef))));
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
	expand(options?: ExpandOptions): symbolic {
		return addTerms(this.num, ...this.terms.map(i => term(i.item.expand(options), i.coef)));
	}
	factor(): symbolic {
		const terms = this.terms.slice();
		commonFactors(terms);

		if (terms.length === 1 && this.num === 0)
			return terms[0].coef === 1 ? terms[0].item : terms[0].item.scale(terms[0].coef);

		return symbolicAdd.create(terms, this.num);
	}

	derivative(v: string) : symbolic {
		return addTerms(0, ...this.terms.map(i => term(i.item.derivative(v), i.coef)));
	}
	evaluate(env?: Record<string, number>) : number {
		return this.terms.reduce((acc, curr) => acc + (curr.item.evaluate(env) * curr.coef), this.num);
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
	item: symbolic;
	pow: number;
}
function factor(item: symbolic, pow = 1): factor {
	return { item, pow };
}

function mulFactors(num: number, ...f: factor[]): symbolic {
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
	factors.sort((a, b) => compare(a.item.id, b.item.id) || (a.pow !== b.pow ? b.pow - a.pow : 0));

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
	static create(factors: factor[], num = 1)	{
		return this.interner.intern(`m(${num !== 1 ? `${num},` : ''}${factors.map(i => `${i.item.id}${i.pow !== 1 ? i.pow : ''}`).join(',')})`, id => new symbolicMul(id, factors, num));
	}

	constructor(id: string, public factors: factor[], public num = 1) {
		super(id);
	}

	neg(): symbolic {
		return symbolicMul.create(this.factors, -this.num);
	}
	recip(): symbolic {
		return symbolicMul.create(this.factors.map(i => factor(i.item, -i.pow)), 1 / this.num);
	}
	pow(b: param): symbolic {
		if (typeof b === 'number')
			return b === 0 ? one : b === 1 ? this : symbolicMul.create(this.factors.map(i => factor(i.item, i.pow * b)), this.num ** b);
		return symbolicPow.create(this, asSymbolic(b));
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
	match(node: symbolic, prematch: Prematch, bindings: Bindings): Bindings | null {
		if (node instanceof symbolicMul) {
			if (this.num !== node.num)
				return null;

			const nfactors = node.factors.slice();
			for (const pfactor of this.factors) {
				let found = -1;
				for (const nfactor of nfactors) {
					if (pfactor.item.match(nfactor.item, prematch, bindings)) {
						found = nfactors.indexOf(nfactor);
						break;
					}
				}
				if (found === -1)
					return null;

				nfactors.splice(found, 1);
			}
			return nfactors.length === 0 ? bindings : null;
		}
		return null;
	}
	substitute(map: Bindings) : symbolic {
		return mulFactors(this.num, ...this.factors.map(f => factor(f.item.substitute(map), f.pow)));
	}
	visit(apply: Visitor): symbolic {
		return apply(mulFactors(this.num, ...this.factors.map(f => factor(f.item.visit(apply), f.pow))));
	}
	expand(options?: ExpandOptions): symbolic {
		const factors = this.factors.map(f => factor(f.item.expand(options), f.pow));
		const additive: symbolicAdd[] = [];
		const others: factor[] = [];

		for (const f of factors) {
			if (f.item instanceof symbolicAdd && f.pow > 0 && Number.isInteger(f.pow)) {
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

function specialConstant(name: string, toString = name) {
	const C = class extends symbolic {
		constructor()		{ super(name); }
		toString(): string	{ return toString; }
	};
	return C;
}

class symbolicInfinity extends specialConstant('∞') {
	add(_b: param): symbolic {
		return this;
	}
	mul(b: param): symbolic {
		if (typeof b === 'number' ? b === 0 : isConst(b) && b.value === 0)
			return nan;
		return this;
	}
}

const i			= new (specialConstant('i', '𝑖'));
const e			= new (specialConstant('𝑒'));
const pi		= new (specialConstant('π'));
const infinity	= new symbolicInfinity;
const nan		= new (specialConstant('NaN'));

symbolic.set(i.mul(i), one.neg());
symbolic.set(zero.recip(), infinity);

//-----------------------------------------------------------------------------
// functions
//-----------------------------------------------------------------------------

function unaryFunction(name: string,
	evaluate: (arg: number) => number,
	derivative: (arg: symbolic) => symbolic,
	toString = (a: symbolic, opts: StringifyOptions) => `${name}(${a.toString(opts)})`
) {
	const C = class extends symbolic {
		static create(i: symbolic)	{ return this.interner.intern(`${name}:${i.id}`, id => new C(id, i)); }
		create(i: symbolic)			{ return (this.constructor as any).create(i); }

		constructor(id: string, public arg: symbolic) {
			super(id);
		}
		match(node: symbolic, prematch: Prematch, bindings: Bindings): Bindings | null {
			return node instanceof C && this.arg.match(node.arg, prematch, bindings) ? bindings : null;
		}
		visit(apply: Visitor): symbolic {
			return apply(this.create(this.arg.visit(apply)));
		}
		substitute(map: Bindings) : symbolic {
			return this.create(this.arg.substitute(map));
		}
		expand(options?: ExpandOptions): symbolic {
			return this.create(this.arg.expand(options));
		}
		derivative(v: string): symbolic { return derivative(this.arg).mul(this.arg.derivative(v)); }
		evaluate(env?: Record<string, number>): number { return evaluate(this.arg.evaluate(env)); }
		toString(opts: StringifyOptions): string { return toString(this.arg, {...opts, fromMul: false}); }
	};
	return C;
}

function binaryFunction(name: string,
	evaluate: (a: number, b: number) => number,
	derivative: (a: symbolic, b: symbolic) => [symbolic, symbolic],
	toString = (a: symbolic, b: symbolic, opts: StringifyOptions) => `${name}(${a.toString(opts)}, ${b.toString(opts)})`
) {
	const C = class extends symbolic {
		static create(i: symbolic, j: symbolic)	{
			return this.interner.intern(`${name}:${i.id}:${j.id}`, id => new C(id, i, j));
		}
		create(i: symbolic, j: symbolic)		{ return (this.constructor as any).create(i, j); }

		constructor(id: string, public arg1: symbolic, public arg2: symbolic) {
			super(id);
		}
		match(node: symbolic, prematch: Prematch, bindings: Bindings): Bindings | null {
			return node instanceof C && this.arg1.match(node.arg1, prematch, bindings) && this.arg2.match(node.arg2, prematch, bindings) ? bindings : null;
		}
		visit(apply: Visitor): symbolic {
			return apply(this.create(this.arg1.visit(apply), this.arg2.visit(apply)));
		}
		substitute(map: Bindings) : symbolic {
			return this.create(this.arg1.substitute(map), this.arg2.substitute(map));
		}
		expand(options?: ExpandOptions): symbolic {
			return this.create(this.arg1.expand(options), this.arg2.expand(options));
		}
		derivative(v: string): symbolic {
			const [d1, d2] = derivative(this.arg1, this.arg2);
			return d1.mul(this.arg1.derivative(v)).add(d2.mul(this.arg2.derivative(v)));
		}
		evaluate(env?: Record<string, number>): number { return evaluate(this.arg1.evaluate(env), this.arg2.evaluate(env)); }
		toString(opts: StringifyOptions): string { return toString(this.arg1, this.arg2, {...opts, fromMul: false}); }
	};
	return C;
}

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

//-----------------------------------------------------------------------------
// transformation rules
//-----------------------------------------------------------------------------


function Rule(name: string, pattern: symbolic, replace: (bs: Bindings) => symbolic, guard?: (bs: Bindings) => boolean): Rule {
	return {
		name,
		match: (node, compare) => pattern.match(node, compare, {}),
		replace,
		guard,
	};
}

function bind(name: string) {
	return symbolicMatcher.create(name);
}

export const trigRules: Rule[] = [
	Rule('sin-sum',
		symbolic.sin(bind('A').add(bind('B'))),
		bs => symbolic.sin(bs.A).mul(symbolic.cos(bs.B)).add(symbolic.cos(bs.A).mul(symbolic.sin(bs.B)))
	),
	Rule('sin-diff',
		symbolic.sin(bind('A').sub(bind('B'))),
		bs => symbolic.sin(bs.A).mul(symbolic.cos(bs.B)).sub(symbolic.cos(bs.A).mul(symbolic.sin(bs.B)))
	),
	Rule('sin-half-angle',
		symbolic.sin(bind('A').scale(0.5)),
		bs => symbolic.sqrt(one.sub(symbolic.cos(bs.A)).scale(0.5))
	),

	Rule('cos-sum',
		symbolic.cos(bind('A').add(bind('B'))),
		bs => symbolic.cos(bs.A).mul(symbolic.cos(bs.B)).sub(symbolic.sin(bs.A).mul(symbolic.sin(bs.B)))
	),
	Rule('cos-diff',
		symbolic.cos(bind('A').sub(bind('B'))),
		bs => symbolic.cos(bs.A).mul(symbolic.cos(bs.B)).add(symbolic.sin(bs.A).mul(symbolic.sin(bs.B)))
	),
	Rule('cos-half-angle',
		symbolic.cos(bind('A').scale(0.5)),
		bs => symbolic.sqrt(one.add(symbolic.cos(bs.A)).scale(0.5))
	),

	Rule('tan-sum',
		symbolic.tan(bind('A').add(bind('B'))),
		bs => symbolic.tan(bs.A).add(symbolic.tan(bs.B)).div(one.sub(symbolic.tan(bs.A).mul(symbolic.tan(bs.B))))
	),
	Rule('tan-diff',
		symbolic.tan(bind('A').sub(bind('B'))),
		bs => symbolic.tan(bs.A).sub(symbolic.tan(bs.B)).div(one.add(symbolic.tan(bs.A).mul(symbolic.tan(bs.B))))
	),
	Rule('tan-half-angle',
		symbolic.tan(bind('A').scale(0.5)),
		bs => symbolic.sin(bs.A).scale(0.5).div(symbolic.cos(bs.A).scale(0.5))
	),

];

export const invTrigRules: Rule[] = [
	Rule('sin-sum',
		symbolic.sin(bind('A')).mul(symbolic.cos(bind('B'))).add(symbolic.cos(bind('A')).mul(symbolic.sin(bind('B')))),
		bs => symbolic.sin(bs.A.add(bs.B)),
	),
];

export function applyRules(node: symbolic, rules: Rule[]): symbolic {
	const cache		= new Map<string, symbolic>();

	const prematch: Prematch = (a, b, bindings) => a === b ? bindings : null;

	const apply = (node: symbolic) => {
		const cached = cache.get(node.id);
		if (cached)
			return cached;

		for (const r of rules) {
			const bs = r.match(node, prematch);
			if (bs && (!r.guard || r.guard(bs))) {
				const replaced		= r.replace(bs);
				const simplified	= replaced.visit(apply);
				cache.set(node.id, simplified);
				return simplified;
			}
		}
		cache.set(node.id, node);
		return node;
	};

	return node.visit(apply);
}

export function parse(s: string): symbolic {
}