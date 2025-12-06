/* eslint-disable no-restricted-syntax */
import { Operators, compare, isAlmostInteger, OperatorsBase, gcd, commonDenominator, denominator, isNumber, lcm } from './core';
import rational, { Numeric } from './rational';
import { toSuperscript, radicalChars } from './string';

// invariants:
// - all symbolic instances are interned and unique by id
// - symbolic instances are immutable
// - symbolic instances with same id are structurally equal
//	- Additive:
// 		- terms are sorted and combined - no duplicate terms
//		- terms are not constants (constant term is stored separately)
//		- each term has a numeric multiplier
//		- multiplicative terms store their constant in the term *not* in the item
//
//	- Multiplicative:
// 		- factors are sorted and combined - no duplicate factors
//		- factors are not constants (constant factor is stored separately)
//		- each factor has a numeric power
//		- constant factor is positive

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

	prune(): void {
		for (const [k, ref] of this.table) {
			if (ref.deref?.() === undefined)
				this.table.delete(k);
		}
	}
}

export type Bindings = Record<string, symbolic>;

export interface Visitor {
	noRemake?:boolean;
	pre?:	(node: symbolic, parent?: symbolic) => symbolic | undefined;
	post?:	(node: symbolic, parent?: symbolic) => symbolic | undefined;
}

function visit(node: symbolicBase, recurse: () => symbolicBase, visitor: Visitor, parent?: symbolicBase, children?: () =>symbolicBase[]): symbolicBase {
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
	if (visitor.noRemake)
		children?.().forEach(child => child.visit(visitor, node as symbolic));
	else
		node = recurse();
	return postvisit(node);
}

export type MatchOptions = {
	exact?:		boolean;
	depth?:		number;
};

export type ExpandOptions = {
	depth?:		number;
	maxPow?:	number;
	uses?:		Set<string>;	// only expand if these variables are present
	maxParts?:		number;    // cap number of expansion parts to avoid combinatorial explosion
};

const StringifyOptionsDefault = {
	parentheses:	'minimal' as 'minimal' | 'always' | 'never',
	direct:			false,
	ccode:			false,
	superPower: 	false,
	radicalPower:	false,
	sortTerms:		true,
	sortFactors:	true,
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
		return `${sign}${maybeParentheses(s, opts)}${opts.divChar}${opts.printConst(Math.round(1 / abs))}`;
	else
		return `${sign}${opts.printConst(abs)}${maybeParentheses(s, opts)}`;
//		return `${printConst(abs, opts)}${opts.mulChar}${maybeParentheses(s, opts)}`;
}

function maybeParentheses(s: string, opts: StringifyOptions) {
	if (opts.parentheses === 'always' || (opts.parentheses === 'minimal' && s.includes(' ')))
		return `(${s})`;
	return s;
}

function Num(n: number|Numeric) {
	return isNumber(n) ? new Numeric(n) : n;
}
function asNumeric(b: param) : Numeric | null {
	return !(b instanceof symbolic) ? Num(b) : isConst(b) ? b.value : null;
}

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
	visit(visitor: Visitor, parent?: symbolicBase):		symbolicBase	{ return visit(this, () => this, visitor, parent); }

	match(node: symbolicBase, bindings: Bindings, opts?: MatchOptions): Bindings | null {
		// Allow patterns to match through single numeric factors (e.g., pattern A+B matches 2(A+B))
		if (!opts?.exact && node instanceof symbolic && node.is('mul')) {
			const mul = node as symbolicMul;
			if (mul.factors.length === 1 && mul.factors[0].pow.is1()) {
				const inner = mul.factors[0].item;
				const result = this.match(inner, bindings, opts);
				if (result && !mul.num.is1())
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
type param = number | Numeric | symbolic;
type param2 = param | (() => param);

export class symbolic extends symbolicBase {
	static from(i: number|Numeric): symbolic {
		return  i instanceof Numeric
			? symbolicConstant.create(i)
			: Number.isFinite(i) ? symbolicConstant.create(new Numeric(i))
			: i === Infinity ? infinity
			: i === -Infinity ? infinity.neg()
			: nan;
	}
	static variable(name: string): symbolic	{ return symbolicVariable.create(name); }
	static bind(name: string): symbolic 	{ return symbolicMatcher.create(name); }

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
	static sqrt(i: param)	: symbolic		{ return asSymbolic(i).npow(1 / 2); }

	static sinh(i: param)	: symbolic		{ return symbolicSinh.create(asSymbolic(i)); }
	static cosh(i: param)	: symbolic		{ return symbolicCosh.create(asSymbolic(i)); }
	static tanh(i: param)	: symbolic		{ return symbolicTanh.create(asSymbolic(i)); }
	static asinh(i: param)	: symbolic		{ return symbolicAsinh.create(asSymbolic(i)); }
	static acosh(i: param)	: symbolic		{ return symbolicAcosh.create(asSymbolic(i)); }
	static atanh(i: param)	: symbolic		{ return symbolicAtanh.create(asSymbolic(i)); }

	static switch(otherwise: param2, ...args: [symbolicBoolean, param2][]): symbolic {
		return combinePartitions(asSymbolic2(otherwise), ...args.map(([bool, value]) => partition(bool, asSymbolic2(value))));
	}

	from(n: number) { return symbolic.from(n); }
	dup():		symbolic	{ return this; }

	// Covariant return type overrides
	substitute(map: Bindings): symbolic { return super.substitute(map) as symbolic; }
	expand(_opts?: ExpandOptions): symbolic { return this; }
	collect(v: string|symbolic): symbolic[] {
		const r: symbolic[] = [];
		r[v === this ? 1 : 0] = this;
		return r;
	}
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

	compare(op: OpNames, b: param): symbolicBoolean {
		return symbolicCompare.create(Ops[op], this, asSymbolic(b));
	}
	less(b: param): symbolicBoolean {
		return symbolicCompare.create(OpMasks.LT, this, asSymbolic(b));
	}
	greater(b: param): symbolicBoolean {
		return symbolicCompare.create(OpMasks.GT, this, asSymbolic(b));
	}

	sqrt():		symbolic	{ return this.npow(1 / 2); }
	abs():		symbolic	{ return symbolicAbs.create(this); }
	neg():		symbolic	{ return symbolicAdd.create([term(this, Num(-1))]); }
	recip():	symbolic	{ return symbolicMul.create([factor(this, Num(-1))]); }

	npow(b: number|Numeric): symbolic {
		b = Num(b);
		return b.is1() ? this : b.is0() ? one : mulFactors(Num(1), factor(this, b));//symbolicMul.create([factor(this, b)]);
	}
	ipow(b: number): symbolic				{ return this.npow(b); }
	rpow(n: number, d: number): symbolic	{ return this.npow(new Numeric(rational(n, d))); }

	pow(b: param): symbolic {
		return !(b instanceof symbolic) ? this.npow(b)
			: isConst(b) ? this.npow(b.value)
			: symbolicPow.create(this, b);
	}

	scale(b: number|Numeric): symbolic	{
		b = Num(b);
		return b.is0() ? zero : b.is1() ? this : mulFactors(b, factor(this));
	}

	add(b: param): symbolic	{
		const n = asNumeric(b);
		return	n ? (n.is0() ? this : addTerms(n, term(this)))
			:	b instanceof symbolicAdd ? addTerms(b.num, term(this), ...b.terms)
			:	addTerms(Num(0), term(this), term(b as symbolic));
	}
	sub(b: param): symbolic	{
		const n = asNumeric(b);
		return	n ? (n.is0() ? this : addTerms(n.neg(), term(this)))
			:	b instanceof symbolicAdd ? addTerms(b.num.neg(), term(this), ...b.terms.map(i => term(i.item, i.coef.neg())))
			:	addTerms(Num(0), term(this), term(b as symbolic, Num(-1)));
	}
	mul(b: param): symbolic	{
		const n = asNumeric(b);
		return n ? this.scale(n)
			: b instanceof symbolicMul ? mulFactors(b.num, ...b.factors, factor(this))
			: mulFactors(Num(1), factor(this), factor(b as symbolic));
	}

	div(b: param): symbolic	{
		const n = asNumeric(b);
		return n ? (n.is0() ? infinity : this.scale(n.recip()))
			: b instanceof symbolicMul ? mulFactors(b.num, ...b.factors.map(i => factor(i.item, i.pow.neg())), factor(this))
			: mulFactors(Num(1), factor(this), factor((b as symbolic).recip()));
	}
	mod(b: param): symbolic		{ return symbolicMod.create(this, asSymbolic(b)); }
	mag(): number				{ const v = this.evaluate(); return Number.isFinite(v) ? Math.abs(v) : NaN; }
	eq(b: symbolic) : boolean	{ return this === b;}

	evaluate(_env?: Record<string, number>):	number			{ return NaN; }
	evaluateT<T>(_ops: Operators<T>, _env?: Record<string, T>) 	:	T|undefined	{ return undefined; }
	derivative(_v: string):						symbolic		{ return zero;}
	valueOf():									number 			{ return this.evaluate(); }
}

//-----------------------------------------------------------------------------
// constant
//-----------------------------------------------------------------------------
function constPow(value: Numeric, pow: Numeric): [Numeric, boolean] {
	let i = false;
	const npow = Number(pow);
	if (value.sign() < 0) {
		const d = pow.denominator();
		if (d && (d & 1) === 0)
			i = true;
		if (npow & 1)
			return [value.abs().npow(npow).neg(), i];
	}

	return [value.abs().npow(npow), i];
}

class symbolicConstant extends symbolic {
	//static create(i: number)	{ return this.interner.intern(`c:${i}`, id => new symbolicConstant(id, i)); }

	constructor(id: string, public value: Numeric) {
		super(id);
	}

	static create(i: Numeric) { return this.interner.intern(`c:${i}`, id => new symbolicConstant(id, i)); }
	neg():		symbolic		{ return symbolicConstant.create(this.value.neg()); }
	recip():	symbolic		{ return symbolicConstant.create(this.value.recip()); }
	npow(b: Numeric): symbolic	{
		const [value, needi] = constPow(this.value, b);
		return needi ? i.scale(value) : symbolicConstant.create(value);
	}

	scale(b: Numeric): symbolic	{ return symbolicConstant.create(this.value.mul(b)); }
	add(b: param): symbolic	{
		const n = asNumeric(b);
		return n ? symbolicConstant.create(this.value.add(n)) : (b as symbolic).add(this);
	}
	mul(b: param): symbolic	{
		const n = asNumeric(b);
		return n ? symbolicConstant.create(this.value.mul(n))
			: this.value.is0() ? this
			: this.value.is1() ? b as symbolic
			: (b as symbolic).mul(this);
	}

	evaluate(_env?: Record<string, number>):	number { return Number(this.value); }
	evaluateT<T>(ops: Operators<T>, _env?: Record<string, T>) 	: 	T|undefined	{ return ops.from(Number(this.value)); }
	_toString(opts: StringifyOptions):		string { return opts.printConst(Number(this.value)); }
}

const zero	= symbolic.from(0);
const one	= symbolic.from(1);

function isConst(e: symbolic): e is symbolicConstant {
	return e instanceof symbolicConstant;
}

function asSymbolic(i: param): symbolic {
	return i instanceof symbolic ? i : symbolic.from(i);
}
function asSymbolic2(i: param2): symbolic {
	return asSymbolic(typeof i === 'function' ? i() : i);
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
	_toString() : string { return this.name; }
}

export class symbolicMatcher extends symbolicVariable {
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
	coef:	Numeric;
}
export function term(item: symbolic, coef: Numeric = Num(1)): term {
	return { item, coef };
}
export function termAsSymbolic(t: term): symbolic {
	return t.coef.is1() ? t.item : t.item.scale(t.coef);
}


export function addTerms(num: Numeric, ...a: readonly Readonly<term>[]): symbolic {
	function add(item: symbolic, coef: Numeric) {
		if (isConst(item)) {
			num = num.add(coef.mul(item.value));

		} else if (item instanceof symbolicAdd) {
			num = num.add(item.num.mul(coef));
			for (const j of item.terms)
				add(j.item, j.coef.mul(coef));

		} else if (item instanceof symbolicMul) {
			coef = item.num.mul(coef);
			if (!coef.is0()) {
				if (item.factors.length === 1 && item.factors[0].pow.is1()) {
					const inner = item.factors[0].item;
					if (inner instanceof symbolicAdd) {
						num = num.add(inner.num.mul(coef));
						for (const j of inner.terms)
							add(j.item, j.coef.mul(coef));
					} else {
						add(inner, coef);
					}
				} else {
					// pull numeric multiplier from mul into term coefficient
					terms.push(term(item.num.is1() ? item : symbolicMul.create(item.factors), coef));
				}
			}

		} else {
			terms.push(term(item, coef));
		}
	}

	const terms: term[] = [];
	for (const i of a)
		add(i.item, i.coef);

	// canonical order
	terms.sort((a, b) => compare(a.item.id, b.item.id));

	// combine like terms by summing coefficients
	const combined: term[] = [];
	for (const t of terms) {
		const last = combined.at(-1);
		if (last && last.item.id === t.item.id)
			last.coef = last.coef.add(t.coef);
		else
			combined.push(t);
	}

	// remove zero coefficients
	const nonzero = combined.filter(t => !t.coef.is0());

	if (nonzero.length === 0)
		return symbolic.from(Number(num));

	if (nonzero.length === 1 && num.is0())
		return termAsSymbolic(nonzero[0]);


	// factor out GCD from term coefficients and constant
	const	coeffs = nonzero.map(t => t.coef.abs());
	if (!num.is0())
		coeffs.push(num.abs());

	const 	den = lcm(...coeffs.map(i => i.denominator()));
//	const	den	= commonDenominator(coeffs, 1000, 1e-8);
	let	g	= den ? gcd(...coeffs.map(n => Math.round(Number(n) * den))) / den : 1;
	if (nonzero[0].coef.sign() < 0)
		g = -g;

	if (g !== 1) {
		const gg = Num(g);
		return mulFactors(gg, factor(symbolicAdd.create(nonzero.map(t => term(t.item, t.coef.div(gg))), num.div(gg))));
	}

	return symbolicAdd.create(nonzero, num);
}

export class symbolicAdd extends symbolic {
	static validate(terms: readonly Readonly<term>[])  {
		let prevId = '';
		for (const t of terms) {
			if (t.coef.is0())
				throw new Error('unexpected zero coefficient in term');
			if (t.item.id <= prevId)
				throw new Error('terms not in canonical order or duplicate term');
			if (t.item instanceof symbolicAdd) {
				if (t.coef.is1() || terms.length === 1)
					throw new Error('unexpected additive term in term item');
			} else if (t.item instanceof symbolicMul) {
				if (!t.item.num.is1())
					throw new Error('unexpected numeric factor in term item');
				if (t.item.factors.length === 1 && t.item.factors[0].pow.is1())
					throw new Error('unexpected factor in term item');
			}
			prevId = t.item.id;
		}
		// TODO: fix egraph rules that create fractional coefficients
		// if (terms.length > 1 && gcd2(...terms.map(t => Math.abs(t.coef))) !== 1)
		// 	throw new Error('term coefficients not in lowest terms');
	}
	static create(terms: readonly term[], num: Numeric = Num(0)) : symbolic	{
		this.validate(terms);
		return this.interner.intern(`a(${num.is0() ? '' : `${num},`}${terms.map(i => `${i.coef.is1() ? '' : Number(i.coef) === -1 ? '-' : i.coef}${i.item.id}`).join(',')})`, id => new symbolicAdd(id, terms, num));
	}

    constructor(id: string, public terms: readonly Readonly<term>[], public num: Numeric) {
		super(id);
	}
	neg(): symbolic {
		if (this.num.is0() && this.terms.length === 1 && Number(this.terms[0].coef) === -1)
			return this.terms[0].item;
		return addTerms(this.num.scale(-1), ...this.terms.map(i => term(i.item, i.coef.scale(-1))));
	}

	add(b: param): symbolic	{
		const n = asNumeric(b);
		return  n ? symbolicAdd.create(this.terms, this.num.add(n))
				: 	b instanceof symbolicAdd ? addTerms(this.num.add(b.num), ...this.terms, ...b.terms)
				: 	addTerms(this.num, ...this.terms, term(b as symbolic));
	}
	sub(b: param): symbolic	{ 
		const n = asNumeric(b);
		return	n ? symbolicAdd.create(this.terms, this.num.sub(n))
				: b instanceof symbolicAdd ? addTerms(this.num.sub(b.num), ...this.terms, ...b.terms.map(i => term(i.item, i.coef.neg())))
				: addTerms(this.num, ...this.terms, term(b as symbolic, Num(-1)));
	}

	match(node: symbolic, bindings: Bindings, opts?: MatchOptions): Bindings | null {
		if (node instanceof symbolicAdd) {
			if (opts?.exact && this.num != node.num)
				return null;

			//const	bindings0 = {...bindings};
			const	opts2 	= { ...opts, exact: true};
			// eslint-disable-next-line prefer-const
			let		nterms	= node.terms.slice();
			let		found	= -1;

			for (const pterm of this.terms) {
				found = -1;
				for (const nterm of nterms) {
					if (Number(pterm.coef) === Number(nterm.coef) && pterm.item.match(nterm.item, bindings, opts2)) {
						found = nterms.indexOf(nterm);
						break;
					}
				}
				if (found === -1)
					break;
				nterms.splice(found, 1);
			}

			if (found === -1) {
				return null;
/*				let factor	= 0;
				for (let start = 0; start < this.terms.length; ++start) {
					const pterms = this.terms.slice(start).concat(this.terms.slice(0, start));
					factor	= 0;
					nterms	= node.terms.slice();
					bindings = {...bindings0};
					for (const pterm of pterms) {
						found = -1;
						for (const nterm of nterms) {
							if (factor === 0 || pterm.coef * factor === nterm.coef) {
								if (pterm.item.match(nterm.item, bindings, opts2)) {
									if (factor === 0)
										factor = nterm.coef / pterm.coef;
									found = nterms.indexOf(nterm);
									break;
								}
							}
						}
						if (found === -1)
							break;
						nterms.splice(found, 1);
					}
					if (found !== -1)
						break;
				}
				if (found === -1)
					return null;
				bindings['_mulrest'] = symbolic.from(factor);
*/
			}
			if (opts?.exact && nterms.length)
				return null;

			if (Number(this.num) != Number(node.num) || nterms.length)
				bindings['_addrest'] = addTerms(this.num.sub(node.num), ...nterms);

			return bindings;
		}
		// Try base class behavior (match through numeric factors)
		return super.match(node, bindings, opts);
	}

	visit(visitor: Visitor, parent?: symbolic): symbolic {
		return visit(this,
			() => addTerms(this.num, ...this.terms.map((i) => term(i.item.visit(visitor, this), i.coef))), visitor, parent,
			() => this.terms.map(i => i.item)
		) as symbolic;
	}

	substitute(map: Bindings) : symbolic {
		return addTerms(this.num, ...this.terms.map(i => term(i.item.substitute(map), i.coef)));
	}

	collect(v: string|symbolic): symbolic[] {
		const groups: symbolic[] = [symbolic.from(Number(this.num))];
		if (typeof v === 'string')
			v = symbolic.variable(v);

		for (const t of this.terms) {
			const i = t.item;
			if (i instanceof symbolicMul) {
				let pow = 0;
				const remaining: factor[] = [];
				for (const ff of i.factors as factor[]) {
					if (ff.item === v)
						pow += Number(ff.pow);
					else
						remaining.push(ff);
				}
				groups[pow] = (groups[pow] ??= zero).add(mulFactors(i.num.mul(t.coef), ...remaining));

			} else if (i === v) {
				groups[1] = (groups[1] ??= zero).add(symbolic.from(Number(t.coef)));

			} else {
				groups[0] = groups[0].add(t.item.scale(t.coef));
			}
		}
		return groups;
	}

	expand(opts?: ExpandOptions): symbolic {
		return addTerms(this.num, ...this.terms.map(i => term(i.item.expand(opts), i.coef)));
	}

	derivative(v: string) : symbolic {
		return addTerms(Num(0), ...this.terms.map(i => term(i.item.derivative(v), i.coef)));
	}
	evaluate(env?: Record<string, number>) : number {
		return this.terms.reduce((acc, curr) => {
			if (acc !== acc) 	// NaN check
				return acc;
			return acc + Number(curr.coef.scale(curr.item.evaluate(env)));
		}, Number(this.num));
	}
	evaluateT<T>(ops: Operators<T>, env?: Record<string, T>) {
		return this.terms.reduce((acc, curr) => {
			if (!acc)
				return acc;
			const v = curr.item.evaluateT(ops, env);
			return v ? ops.add(acc, ops.mul(v, ops.from(Number(curr.coef)))) : v;
		}, ops.from(Number(this.num)) as T | undefined);
	}

	_toString(opts: StringifyOptions) : string {
		function termOrder(t: term): number {
			return t.item instanceof symbolicMul ? t.item.factors.reduce((acc, curr) => acc + Number(curr.pow), 0) : 0;
		}

		const terms = opts.sortTerms ? this.terms.slice().sort((a, b) => termOrder(b) - termOrder(a)) : this.terms;

		if (opts.direct) {
			return '(' + terms.map(t => printScaled(t.item._toString(opts), Number(t.coef), opts)).join(opts.addChar)
			+ (this.num.is0() ? '' : opts.addChar + opts.printConst(Number(this.num)))
			+ ')';
		}

		return terms.map((t, j) => {
			return	(j === 0 ? (Number(t.coef) < 0 ? '-' : '') : (Number(t.coef) < 0 ? opts.subChar : opts.addChar))
				+	printScaled(t.item._toString(opts), Math.abs(Number(t.coef)), opts);
		}).join('')
		+ (!this.num.is0() ? (Number(this.num) < 0 ? opts.subChar : opts.addChar) + opts.printConst(Math.abs(Number(this.num))) : '');
	}
}

//-----------------------------------------------------------------------------
// mul
//-----------------------------------------------------------------------------

export interface factor {
	item:	symbolic;
	pow:	Numeric; 
}
export function factor(item: symbolic, pow: Numeric = Num(1)): factor {
	return { item, pow };
}
export function factorAsSymbolic(f: factor): symbolic {
	return f.item.npow(f.pow);
}

export function mulFactors(num: Numeric, ...f: Readonly<factor>[]): symbolic {
	const factors: factor[] = [];

	function add(item: symbolic, pow: Numeric) {
		if (isConst(item)) {
			const [value, needi] = constPow(item.value, pow);
			num = num.mul(value);
			if (needi)
				factors.push(factor(i));

		} else if (item instanceof symbolicMul) {
			num = num.mul(item.num);
			for (const j of item.factors)
				add(j.item, j.pow.mul(pow));

		} else if (item instanceof symbolicAdd) {
			const first = item.terms[0];
			if (item.terms.length === 1 && item.num.is0()) {
				const [value, needi] = constPow(first.coef, pow);
				num = num.mul(value);
				if (needi)
					factors.push(factor(i));
				add(first.item, pow);
			} else if (first.coef.sign() < 0 && Number.isInteger(Number(pow))) {
				if (Number(pow) & 1)
					num = num.neg();
				add(item.neg(), pow);
			} else {
				factors.push(factor(item, pow));
			}

		} else {
			factors.push(factor(item, pow));
		}
	}

	for (const i of f)
		add(i.item, i.pow);

	if (num.is0())
		return zero;

	// canonical order
	factors.sort((a, b) => compare(a.item.id, b.item.id));

	// combine like factors by summing powers
	const combined: factor[] = [];
	for (const f of factors) {
		const last = combined.at(-1);
		if (last && last.item.id === f.item.id)
			last.pow = last.pow.add(f.pow);
		else
			combined.push(f);
	}

	// remove zero coefficients and handle powers of i
	const nonzero = combined.filter(t => {
		if (t.item === i) {
			const pow = Number(t.pow) & 3;
			if (pow & 2)
				num = num.scale(-1);
			t.pow = Num(pow % 2);
		}
		return t.pow.sign() !== 0;
	});

	if (nonzero.length === 0)
		return symbolic.from(Number(num));

	if (nonzero.length === 1 && nonzero[0].pow.is1()) {
		const first = nonzero[0].item;
		if (num.is1())
			return first;
		/*
		if (first instanceof symbolicAdd) {
			if (first.terms.length === 1)
				return symbolicAdd.create([term(first.terms[0].item, first.terms[0].coef * num)], first.num * num);
			// Multi-term symbolicAdd with num !== 1:
			// For negative num, negate the symbolicAdd and use abs(num) to keep symbolicMul.num > 0
			// e.g., -0.5(A+B+C) → 0.5(-A-B-C)
			//if (num < 0)
			//	return symbolicMul.create([factor(first.neg(), 1)], -num);
			//else
//				return symbolicMul.create(nonzero, num);
//		} else {
//			return symbolicAdd.create([term(first, num)]);
		}
*/
	}

	return symbolicMul.create(nonzero, num);

	// Canonical form: symbolicMul.num must always be positive, so if num is negative, wrap in addition with negative coefficient
	//return num < 0
	//	? symbolicAdd.create([term(symbolicMul.create(nonzero), num)])
	//	: symbolicMul.create(nonzero, num);
}

export class symbolicMul extends symbolic {
	static validate(factors: readonly factor[], num: Numeric)  {
		function numericIsFinite(n: any): boolean {
			if (typeof n === 'number')
				return Number.isFinite(n);
			if (n && typeof n.valueOf === 'function') {
				const v = Number(n.valueOf());
				return Number.isFinite(v);
			}
			return false;
		}
		if (!numericIsFinite(num))
			throw new Error('unexpected NaN numeric factor');
		//if (num <= 0)
		//	throw new Error('unexpected non-positive numeric factor in mul');
		let prevId = '';
		for (const t of factors) {
			if (t.pow.is0())
				throw new Error('unexpected zero power in factor');
			if (t.item.id <= prevId)
				throw new Error('factors not in canonical order or duplicate factor');
			if (t.item instanceof symbolicMul)
				throw new Error('unexpected multiplicative factor in factor item');
			//if (t.item instanceof symbolicAdd) {
			//	if (t.item.terms[0].coef < 0)
			//		throw new Error('unexpected negative first term');
			//}
			prevId = t.item.id;
		}
	}
	static create(factors: readonly factor[], num: Numeric = Num(1)) 	{
		this.validate(factors, num);
		return this.interner.intern(`m(${num.is1() ? '' : `${Number(num)},`}${factors.map(i => `${i.item.id}${!i.pow.is1() ? i.pow : ''}`).join(',')})`, id => new symbolicMul(id, factors, num));
	}

	constructor(id: string, public factors: readonly Readonly<factor>[], public num: Numeric) {
		super(id);
	}
	
	neg(): symbolic {
		return symbolicMul.create(this.factors, this.num.scale(-1));
		//return symbolicAdd.create([term(symbolicMul.create(this.factors, 1), -this.num)], 0);
	}
	recip(): symbolic {
		return symbolicMul.create(this.factors.map(i => factor(i.item, i.pow.scale(-1))), new Numeric(1).div(this.num));
	}
	npow(b: number|Numeric): symbolic {
		b = Num(b);
		if (b.is0())
			return one;
		if (b.is1())
			return this;

		const [value, needi] = constPow(this.num, b);
		return mulFactors(value, ...(needi ? [factor(i)] : []), ...this.factors.map(i => factor(i.item, i.pow.mul(b))));
		/*
		if (this.num < 0 && !Number.isInteger(b))
			return mulFactors(1, factor(this.from(this.num), b), ...this.factors.map(i => factor(i.item, i.pow * b)));
		// this.num is always positive due to invariant, so no need for abs
		return mulFactors(this.num ** b, ...this.factors.map(i => factor(i.item, i.pow * b)));
		*/
	}
	scale(b: number|Numeric): symbolic	{
		b = Num(b);
		return	b.is0()	? zero
				: b.is1()	? this
				: Number(this.num.mul(b)) === 1 && this.factors.length === 1 && Number(this.factors[0].pow) === 1 ? this.factors[0].item
				: mulFactors(this.num.mul(b), ...this.factors);
	}

	mul(b: param): symbolic	{
		const n = asNumeric(b);
		return n ? this.scale(n)
			: b instanceof symbolicMul ? mulFactors(this.num.mul(b.num), ...this.factors, ...b.factors)
			: mulFactors(this.num, ...this.factors, factor(b as symbolic));
	}
	match(node: symbolic, bindings: Bindings, opts?: MatchOptions): Bindings | null {
		if (node instanceof symbolicMul) {
			if (opts?.exact && Number(this.num) != Number(node.num))
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

			if (Number(this.num) != Number(node.num) || nfactors.length)
				bindings['_mulrest'] = mulFactors(this.num.div(node.num), ...nfactors);

			return bindings;
		} else if (Number(this.num) === -1 && this.factors.length === 1 && this.factors[0].pow.is1() && this.factors[0].item.is('add')) {
			// Special case: match negation
			//console.log('try negation');
			if (this.factors[0].item.match(node, bindings, opts)) {
				bindings['_mulrest'] = one.neg();
				return bindings;
			}
		}
		// Try base class behavior (match through numeric factors)
		return super.match(node, bindings, opts);
	}
	substitute(map: Bindings) : symbolic {
		return mulFactors(this.num, ...this.factors.map(f => factor(f.item.substitute(map), f.pow)));
	}
	visit(visitor: Visitor, parent?: symbolic): symbolic {
		return visit(this,
			() => mulFactors(this.num, ...this.factors.map((f) => factor(f.item.visit(visitor, this), f.pow))), visitor, parent,
			() => this.factors.map(f => f.item),
		) as symbolic;
	}
	expand(opts?: ExpandOptions): symbolic {
		let factors = this.factors;

		// expansion: recursively expand child nodes per opts
		if ((opts?.depth ?? 2) > 1) {
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
			const npow = Number(f.pow);
			if (f.item instanceof symbolicAdd && npow > 0 && (!opts?.maxPow || npow < opts.maxPow) && Number.isInteger(npow) && checkUse(f.item.terms)) {
				for (let i = 0; i < npow; i++)
					additive.push(f.item);
			} else {
				others.push(f);
			}
		}

		if (opts?.maxParts) {
			let totalParts = Math.max(others.length, 1);
			for (const a of additive)
				totalParts += totalParts * (a.terms.length + (a.num.is0() ? 0 : 1));

			if (totalParts > opts?.maxParts)
				return this; // Skip expansion to avoid combinatorial explosion
		}

		let parts: symbolic[] = [mulFactors(this.num, ...others)];
		for (const a of additive) {
			const parts2: symbolic[] = [];
			for (const p of parts) {
				for (const t of a.terms)
					parts2.push(p.mul(t.item).scale(t.coef));
				if (!a.num.is0())
					parts2.push(p.scale(a.num));
			}
			parts = parts2;
		}

		return addTerms(Num(0), ...parts.map(p => term(p)));
	}

	derivative(v: string) : symbolic {
		return addTerms(Num(0), ...this.factors.map(f => term(mulFactors(
			this.num.mul(f.pow),
			factor(f.item, f.pow.sub(Num(1))),
			factor(f.item.derivative(v)),
			...this.factors.filter(g => g !== f)
		))));
	}
	
		evaluate(env?: Record<string, number>) : number {
		return this.factors.reduce((acc, curr) => {
			if (acc !== acc || acc === 0) 	// NaN check
				return acc;
			const v = curr.item.evaluate(env);
			return	Number(curr.pow) === 1 ? Number(new Numeric(acc).mul(new Numeric(v)))
				: 	Number(curr.pow) === -1 ? Number(new Numeric(acc).div(new Numeric(v)))
				: 	Number(new Numeric(acc).mul(new Numeric(v ** Number(curr.pow))));
		}, Number(this.num));
	}
	evaluateT<T>(ops: Operators<T>, env?: Record<string, T>) {
		return this.factors.reduce((acc, curr) => {
			if (!acc)
				return acc;
			const v = curr.item.evaluateT(ops, env);
			if (!v)
				return v;
			return Number(curr.pow) === 1 ? ops.mul(acc, v)
				:	Number(curr.pow) === -1 ? ops.div(acc, v)
				:	ops.mul(acc, ops.pow(v, ops.from(Number(curr.pow))));
		}, ops.from(Number(this.num)) as T | undefined);
	}

	_toString(opts: StringifyOptions) : string {
		const factors	= opts.sortFactors ? this.factors.slice().sort((a, b) => Number(b.pow) - Number(a.pow)) : this.factors;

		function printTerm(item: symbolic, n: Numeric) {
			const s = maybeParentheses(item._toString(opts), opts);

			if (n.is1())
				return s;

			const nn = Number(n);
			if (opts.ccode) {
				if (nn === 0.5)
					return `sqrt(${s})`;
				if (Number.isInteger(nn) && nn < 3 && item.is('var'))
					return Array.from({length: nn}, () => s).join(opts.mulChar);
				return `pow(${s}, ${opts.printConst(nn)})`;
			}

			if (opts.radicalPower && nn < 1 && isAlmostInteger(1 / nn)) {
				const c = radicalChars[Math.round(1 / nn)];
				if (c)
					return `${c}${s}`;
			}

			if (opts.superPower)
				return `${s}${toSuperscript(opts.printConst(nn))}`;
			return `${s}^${opts.printConst(nn)}`;
		}

		if (factors[0].pow.sign() < 0) {
			return opts.printConst(Number(this.num)) + factors.map(i =>
				(i.pow.sign() < 0 ? opts.divChar : opts.mulChar) + printTerm(i.item, i.pow.abs())
			).join('');
		} else {
			return printScaled(factors.map((i, j) =>
				(j === 0 ? '' : i.pow.sign() < 0 ? opts.divChar : opts.mulChar) + printTerm(i.item, i.pow.abs())
			).join(''), Number(this.num), opts);
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

	then(t: param2, f: param2): symbolic {
		return combinePartitions(asSymbolic2(f), partition(this, asSymbolic2(t)));
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

type OpNames = keyof typeof Ops;
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

export class symbolicCompare extends symbolicBoolean {
	static create(op: Ops, a: symbolic, b: symbolic)	{
		if (a.id > b.id)
			[a, b, op] = [b, a, reverseOp(op)];
		return this.interner.intern(`${OpNames[op]}:${a.id},${b.id}`, id => new symbolicCompare(id, a, b, op));
	}

	static consts(a: symbolicCompare, b: symbolicCompare): compareConsts | undefined {
		function inner(opA: Ops, opB: Ops, vA: number, vB: number) : compareConsts {
			return vA < vB
				? {opL: opA, opH: opB, compL: a, compH: b}
				: {opL: opB, opH: opA, compL: b, compH: a};
		}

		if (a.a === b.a) {
				if (isConst(a.b) && isConst(b.b))
				return inner(a.op, b.op, Number(a.b.value), Number(b.b.value));

		} else if (a.b === b.b) {
				if (isConst(a.a) && isConst(b.a))
				return inner(reverseOp(a.op), reverseOp(b.op), Number(a.a.value), Number(b.a.value));
		}
	}

	static andConsts(c: compareConsts): symbolicBoolean | undefined {
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

	static orConsts(c: compareConsts): symbolicBoolean | undefined {
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

		const consts = symbolicCompare.consts(this, b);
		return (consts && symbolicCompare.andConsts(consts)) || symbolicAnd.create([this, b]);
	}
	or(b: symbolicBoolean): symbolicBoolean {
		if (!(b instanceof symbolicCompare))
			return b.or(this);

		if (this.a === b.a && this.b === b.b) {
			const opO = this.op | b.op;
			return opO === OpMasks.TRUE ? symTrue : symbolicCompare.create(opO, this.a, this.b);
		}

		const consts = symbolicCompare.consts(this, b);
		return (consts && symbolicCompare.orConsts(consts)) || symbolicOr.create([this, b]);
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

	add(b: param): symbolic {
		return	symbolicPartition.create(this.partitions.map(p => partition(p.bool, p.value.add(b))), this.otherwise.add(b));
	}
	sub(b: param): symbolic {
		return	symbolicPartition.create(this.partitions.map(p => partition(p.bool, p.value.sub(b))), this.otherwise.sub(b));
	}
	scale(b: Numeric): symbolic	{
		return	symbolicPartition.create(this.partitions.map(p => partition(p.bool, p.value.scale(b))), this.otherwise.scale(b));
	}
	mul(b: param): symbolic {
		return	symbolicPartition.create(this.partitions.map(p => partition(p.bool, p.value.mul(b))), this.otherwise.mul(b));
	}
	div(b: param): symbolic {
		return	symbolicPartition.create(this.partitions.map(p => partition(p.bool, p.value.div(b))), this.otherwise.div(b));
	}

	visit(visitor: Visitor, parent?: symbolic):		symbolic	{
		return visit(this,
			() => symbolicPartition.create(this.partitions.map(p => partition(p.bool.visit(visitor, this), p.value.visit(visitor, this))), this.otherwise.visit(visitor, this)),
			visitor, parent,
			() => this.partitions.map(p => [p.bool, p.value]).flat().concat(this.otherwise)
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
		constructor()		{ super(name); symbolic.set(this, this); }
		evaluate(_env?: Record<string, number>): number { return value ?? NaN; }
		evaluateT<T>(ops: Operators<T>, _env?: Record<string, T>) { return ops.variable(name); }
		_toString(): string	{ return toString; }
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
		const n = asNumeric(b);
		if (n && n.is0())
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
			return node instanceof C && this.arg.match(node.arg, bindings, {...opts, exact: true}) ? bindings : null;
		}
		visit(visitor: Visitor, parent?: symbolic): symbolic {
			return visit(this,
				() => this.create(this.arg.visit(visitor, this)),
				visitor, parent,
				() => [this.arg]
			) as symbolic;
		}
		substitute(map: Bindings) : symbolic {
			return this.create(this.arg.substitute(map));
		}
		derivative(v: string): symbolic { return derivative(this.arg).mul(this.arg.derivative(v)); }
		evaluate(env?: Record<string, number>): number { return evaluate(this.arg.evaluate(env)); }
		evaluateT<T>(ops: Operators<T>, env?: Record<string, T>) { return ops.func(name, [this.arg.evaluateT(ops, env)!]); }

		_toString(opts: StringifyOptions): string { return toString(this.arg, opts); }
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
			const opts2 = {...opts, exact: true};
			return node instanceof C && this.arg1.match(node.arg1, bindings, opts2) && this.arg2.match(node.arg2, bindings, opts2) ? bindings : null;
		}
		visit(visitor: Visitor, parent?: symbolic): symbolic {
			return visit(this,
				() => this.create(this.arg1.visit(visitor, this), this.arg2.visit(visitor, this)), visitor, parent,
				() => [this.arg1, this.arg2]
			) as symbolic;
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

		_toString(opts: StringifyOptions): string { return toString(this.arg1, this.arg2, opts); }
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

// trigonometric

const symbolicSin	= unaryFunction('sin', Math.sin, arg => symbolicCos.create(arg));
const symbolicCos	= unaryFunction('cos', Math.cos, arg => symbolicSin.create(arg).neg());
const symbolicTan	= unaryFunction('tan', Math.tan, arg => symbolicCos.create(arg).npow(-2));

const symbolicAsin	= unaryFunction('asin', Math.asin, arg => one.sub(arg.npow(2)).npow(-1 / 2));
const symbolicAcos	= unaryFunction('acos', Math.acos, arg => one.sub(arg.npow(2)).npow(-1 / 2).neg());
const symbolicAtan	= unaryFunction('atan', Math.atan, arg => one.div(arg.npow(2).add(one)));

const symbolicAtan2 = binaryFunction('atan2',
	Math.atan2,
	(a, b) => {
		const denom = b.npow(2).add(a.npow(2));
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
const symbolicTanh = unaryFunction('tanh', Math.tanh, arg => symbolicCosh.create(arg).npow(-2));

const symbolicAsinh = unaryFunction('asinh', Math.asinh, arg => arg.npow(2).add(one).npow(-1 / 2));
const symbolicAcosh = unaryFunction('acosh', Math.acosh, arg => arg.npow(2).sub(one).npow(-1 / 2));
const symbolicAtanh = unaryFunction('atanh', Math.atanh, arg => one.sub(arg.npow(2)).recip());

symbolic.set(symbolic.sinh(zero), zero);
symbolic.set(symbolic.cosh(zero), one);
symbolic.set(symbolic.tanh(zero), zero);

symbolic.set(symbolic.asinh(zero), zero);
symbolic.set(symbolic.acosh(one), zero);
symbolic.set(symbolic.atanh(zero), zero);

export const symbolicOperators: Operators<symbolic> = {
	...OperatorsBase(symbolic),
	variable(name: string) {
		const v = (symbolic as unknown as Record<string, symbolic>)[name];
		if (v instanceof symbolic)
			return v;
		return symbolic.variable(name);
	},
	eq(a: symbolic, b: symbolic): symbolicBoolean {
		return a.compare('=', b);
	},
	lt(a: symbolic, b: symbolic): symbolicBoolean {
		return a.compare('<', b);
	}
};

const types = {
	const:	symbolicConstant,
	var:	symbolicVariable,
	add:	symbolicAdd,
	mul: 	symbolicMul,
	unary:	unaryFunctionBase,
	binary:	binaryFunctionBase,
	partition: symbolicPartition,
	compare: symbolicCompare,
	and:	symbolicAnd,
	or:		symbolicOr,
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
