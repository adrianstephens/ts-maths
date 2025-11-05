/* eslint-disable no-restricted-syntax */
import {ops, compare, superScript} from './core';
import { rationalApprox } from './rational';

class Interner<T extends object> {
	private table = new Map<string, WeakRef<T>>();
	private finalizer: FinalizationRegistry<string>;

	constructor() {
		// when a Node is collected, this registry callback receives the key (the heldValue)
		this.finalizer = new FinalizationRegistry((key: string) => {
			// best-effort cleanup: remove the map entry if it still points to a dead ref
			const ref = this.table.get(key);
			if (!ref || typeof ref.deref !== 'function' || ref.deref() === undefined)
				this.table.delete(key);
		});
	}

	internByKey(key: string, nodeFactory: (key: string) => T): T {
		const existingRef = this.table.get(key);
		const alive = existingRef?.deref?.();
		if (alive)
			return alive;
		
		// create canonical node
		const node = nodeFactory(key);
		this.table.set(key, new WeakRef(node));
		this.finalizer.register(node, key);
		return node;
	}

	// optional helper to inspect/cleanup eagerly
	pruneDeadEntries(): void {
		for (const [k, ref] of this.table) {
			if (ref.deref?.() === undefined)
				this.table.delete(k);
		}
	}
}

type Substitution = Record<string, symbolic | number>;
type ExpandOptions = {
	limit?: 'small' | 'polynomial';
};

function superScript2(n: number): string {
	if (n < 0)
		return '⁻' + superScript2(-n);
	if (Number.isInteger(n))
		return superScript(n);

	const [num, den] = rationalApprox(n, 1e10);
	return superScript(num) + 'ᐟ' + superScript(den);
}

export abstract class symbolic implements ops<symbolic> {
	static interner = new Interner<symbolic>();

	static from(i: number)			{ return symbolicConstant.create(i); }
	static variable(name: string)	{ return symbolicVariable.create(name); }

	static sin(i: symbolic)			{ return symbolicSin.create(i); }
	static cos(i: symbolic)			{ return symbolicCos.create(i); }
	static tan(i: symbolic)			{ return symbolicTan.create(i); }
	static asin(i: symbolic)		{ return symbolicAsin.create(i); }
	static acos(i: symbolic)		{ return symbolicAcos.create(i); }
	static atan(i: symbolic)		{ return symbolicAtan.create(i); }
	static atan2(a: symbolic, b: symbolic) { return symbolicAtan2.create(a, b); }

	static exp(i: symbolic)			{ return symbolicExp.create(i); }
	static log(i: symbolic)			{ return symbolicLog.create(i); }
	static sqrt(i: symbolic)		{ return i.pow(1 / 2); }
	static pow(i: symbolic, j: symbolic)	{ return symbolicPow.create(i, j); }

	static sinh(i: symbolic)		{ return symbolicSinh.create(i); }
	static cosh(i: symbolic)		{ return symbolicCosh.create(i); }
	static tanh(i: symbolic)		{ return symbolicTanh.create(i); }
	static asinh(i: symbolic)		{ return symbolicAsinh.create(i); }
	static acosh(i: symbolic)		{ return symbolicAcosh.create(i); }
	static atanh(i: symbolic)		{ return symbolicAtanh.create(i); }

	constructor(public id: string) {}

	neg():		symbolic		{ return symbolicAdd.create([term(this, -1)]); }
	recip():	symbolic		{ return symbolicMul.create([factor(this, -1)]); }
	pow(b: number): symbolic {
		return b === 1 ? this : b === 0 ? one
			: symbolicMul.create([{ pow: b, item: this }]);
	}

	scale(b: number): symbolic	{
		return (b === 0 ? zero : b === 1 ? this : symbolicMul.create([factor(this)], b));
	}
	add(b: symbolic): symbolic	{
		return isConst(b) ? (b.value === 0 ? this : addTerms(b.value, term(this, 1)))
			: b instanceof symbolicAdd ? addTerms(b.num, term(this, 1), ...b.terms)
			: addTerms(0, term(this, 1), term(b, 1));
	}
	sub(b: symbolic): symbolic	{ 
		return isConst(b) ? (b.value === 0 ? this : addTerms(-b.value, term(this, 1)))
			: b instanceof symbolicAdd ? addTerms(-b.num, term(this, 1), ...b.terms.map(i => term(i.item, -i.coef)))
			: addTerms(0, term(this, 1), term(b, -1));
	}
	mul(b: symbolic): symbolic	{
		return isConst(b) ? (b.value === 0 ? b : b.value === 1 ? this : symbolicMul.create([factor(this)], b.value))
			: b instanceof symbolicMul ? mulFactors(b.num, ...b.factors, factor(this))
			: mulFactors(1, factor(this), factor(b));
	}

	div(b: symbolic): symbolic	{
		return isConst(b) ? (b.value === 1 ? this : symbolicMul.create([factor(this)], 1 / b.value))
			: b instanceof symbolicMul ? mulFactors(b.num, ...b.factors.map(i => factor(i.item, -i.pow)), factor(this))
			: mulFactors(1, factor(this), factor(b, -1));
	}
	mag(): number			{ const v = this.evaluate(); return Number.isFinite(v) ? Math.abs(v) : NaN; }
	eq(b: symbolic) : boolean	{ return this === b;}

	substitute(_map: Substitution): symbolic { return this; }
	expand(_options: ExpandOptions): symbolic { return this; };// distribute mul over add
	collect(_v: string): symbolic { return this; }// group terms by powers of a variable
	factor(): symbolic { return this; }// numeric and polynomial factorization (start with integer/ rational factorization for univariate polynomials). This is harder; start small (pull out GCD of coefficients, common factor extraction).

	abstract derivative(v: string) : symbolic;
	abstract evaluate(env?: Record<string, number>) : number;

	abstract toString() : string;

}

//-----------------------------------------------------------------------------
// constant
//-----------------------------------------------------------------------------

class symbolicConstant extends symbolic {
	static create(i: number)	{ return this.interner.internByKey(`c:${i}`, id => new symbolicConstant(id, i)); }

	constructor(id: string, public value: number) {
		super(id);
	}
	neg(): symbolic				{ return symbolicConstant.create(-this.value); }
	recip(): symbolic			{ return symbolicConstant.create(1 / this.value); }
	pow(b: number): symbolic	{ return symbolicConstant.create(this.value ** b); }

	scale(b: number): symbolic	{ return symbolicConstant.create(this.value * b); }
	add(b: symbolic): symbolic	{ return this.value === 0 ? b : isConst(b) ? symbolicConstant.create(this.value + b.value) : b.add(this); }
	mul(b: symbolic): symbolic	{ return this.value === 0 ? this : this.value === 1 ? b :isConst(b) ? symbolicConstant.create(this.value * b.value) : b.mul(this); }

	//eq(b: symbolic) : boolean	{ return b instanceof symbolicConstant && b.value === this.value; }
	derivative(_v: string) : symbolic { return zero; }
	evaluate(_env?: Record<string, number>) : number { return this.value; }
	toString() : string { return this.value.toString(); }
}

const zero	= symbolicConstant.create(0);
const one	= symbolicConstant.create(1);

function isConst(e: symbolic): e is symbolicConstant {
	return e instanceof symbolicConstant;
}

//-----------------------------------------------------------------------------
// variable
//-----------------------------------------------------------------------------

class symbolicVariable extends symbolic {
	static create(name: string)	{ return this.interner.internByKey(`v:${name}`, id => new symbolicVariable(id, name)); }

	constructor(id: string, public name: string) {
		super(id);
	}
	//eq(b: symbolic) : boolean { return b instanceof symbolicVariable && b.name === this.name; }

	substitute(map: Substitution) : symbolic {
		const val = map[this.name];
		return val === undefined ? this
			: typeof val === 'number' ? symbolic.from(val)
			: val;
	}
	derivative(v: string) : symbolic {
		return v === this.name ? one : zero;
	}
	evaluate(env?: Record<string, number>) : number {
		return env ? (env[this.name] ?? NaN) : NaN;
	}
	toString() : string { return this.name; }
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

function termOrder(t: term): number {
	return t.item instanceof symbolicMul ? t.item.order() : 0;
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
			if (coef !== 0) {
				const itm = i.item.num === 1 ? i.item : symbolicMul.create(i.item.factors, 1);
				terms.push(term(itm, coef));
			}
		} else {
			terms.push(i);
		}
	}

	// canonical order
	terms.sort((a, b) => compare(a.item.id, b.item.id));

	// combine like terms by summing coefficients
	const combined: term[] = [];
	for (const t of terms) {
		const last = combined.at(-1);
		if (last && last.item.id === t.item.id) {
			last.coef += t.coef;
		} else {
			combined.push(term(t.item, t.coef));
		}
	}
	// remove zero coefficients
	const nonzero = combined.filter(t => t.coef !== 0);

	// find common factors
//	if (nonzero.length > 1)
//		commonFactors(nonzero);

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
	const factors = new Map<symbolic, factorInfo>();
	for (const m of terms) {
		if (m.item instanceof symbolicMul) {
			for (const i of m.item.factors) {
				const info = factors.get(i.item) || { item: i.item, count: 0, minpow: Infinity, maxpow: -Infinity };
				info.count += 1;
				info.minpow = Math.min(info.minpow, i.pow);
				info.maxpow = Math.max(info.maxpow, i.pow);
				factors.set(i.item, info);
			}
		}
	}

	const factors2 = Array.from(factors.values()).filter(i => i.count > 1).sort((a, b) => a.count - b.count);

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
		return this.interner.internByKey(`a:${terms.map(i => i.coef + ':' + i.item.id).join(',')}:${num}`, id => new symbolicAdd(id, terms, num));
	}

	constructor(id: string, public terms: term[], public num = 0) {
		super(id);
	}
	neg(): symbolic {
		if (this.num === 0 && this.terms.length === 1 && this.terms[0].coef === -1)
			return this.terms[0].item;
		return symbolicAdd.create(this.terms.map(i => term(i.item, -i.coef)), -this.num);
	}
/*	eq(b: symbolic) : boolean {
		return b instanceof symbolicAdd
			&& this.num === b.num
			&& this.terms.length === b.terms.length
			&& this.terms.every((v, i) => v.coef === b.terms[i].coef && v.item.eq(b.terms[i].item));
	}*/
	add(b: symbolic): symbolic	{
		if (isConst(b))
			return symbolicAdd.create(this.terms, this.num + b.value);
		if (b instanceof symbolicAdd)
			return addTerms(this.num + b.num, ...this.terms, ...b.terms);
		return addTerms(this.num, ...this.terms, term(b, 1));
	}
	substitute(map: Substitution) : symbolic {
		return addTerms(this.num, ...this.terms.map(i => term(i.item.substitute(map), i.coef)));
	}
	expand(options: ExpandOptions): symbolic {
		return addTerms(this.num, ...this.terms.map(i => term(i.item.expand(options), i.coef)));
	}
	derivative(v: string) : symbolic {
		return addTerms(0, ...this.terms.map(i => term(i.item.derivative(v), i.coef)));
	}
	evaluate(env?: Record<string, number>) : number {
		return this.terms.reduce((acc, curr) => acc + (curr.item.evaluate(env) * curr.coef), this.num);
	}
	toString() : string {
		const terms = this.terms.slice().sort((a, b) => termOrder(b) - termOrder(a));
		return '(' + terms.map((t, j) => {
			const abs	= Math.abs(t.coef);
			return	(j === 0 ? (t.coef < 0 ? '-' : '') : (t.coef < 0 ? ' - ' : ' + '))
				+	(abs === 1 ? '' : `${abs} * `)
				+	t.item.toString();
		}).join('')
		+ (this.num !== 0 ? (this.num < 0 ? ' - ' : ' + ') + Math.abs(this.num).toString() : '')
		+ ')';
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

		} else if (i.item instanceof symbolicAdd && i.item.terms[0].coef < 0 && i.pow % 1 === 0) {
			if (i.pow % 2 === 1)
				num = -num;
			factors.push(factor(i.item.neg(), i.pow));

		} else {
			factors.push(i);
		}
	}

	if (num === 0)
		return zero;

	factors.sort((a, b) => compare(a.item.id, b.item.id) || (a.pow !== b.pow ? b.pow - a.pow : 0));

	// coalesce same factors
	for (let i = 0; i < factors.length - 1; i++) {
		if (factors[i].item.id === factors[i + 1].item.id) {
			const pow = factors[i].pow + factors[i + 1].pow;
			if (pow === 0) {
				factors.splice(i, 2);
				i -= 2;
			} else {
				factors[i] = factor(factors[i].item, pow);
				factors.splice(i + 1, 1);
				i -= 1;
			}
		}
	}
	if (factors.length === 0)
		return symbolic.from(num);

	if (factors.length === 1 && factors[0].pow === 1 && num === 1)
		return factors[0].item;

	return symbolicMul.create(factors, num);
}


class symbolicMul extends symbolic {
	static create(factors: factor[], num = 1)	{
		return this.interner.internByKey(`m:${factors.map(i => i.pow.toString()+':'+i.item.id).join(',')}:${num}`, id => new symbolicMul(id, factors, num));
	}

	constructor(id: string, public factors: factor[], public num = 1) {
		super(id);
	}

	order(): number {
		return this.factors.reduce((acc, curr) => acc + curr.pow, 0);
	}
/*	eq(b: symbolic) : boolean {
		return b instanceof symbolicMul
			&& this.num === b.num
			&& this.factors.length === b.factors.length
			&& this.factors.every((v, i) => v.pow === b.factors[i].pow && v.item.eq(b.factors[i].item));
	}*/

	neg(): symbolic {
		return symbolicMul.create(this.factors, -this.num);
	}
	recip(): symbolic {
		return symbolicMul.create(this.factors.map(i => factor(i.item, -i.pow)), 1 / this.num);
	}
	pow(b: number): symbolic {
		return b === 0 ? one : b === 1 ? this : symbolicMul.create(this.factors.map(i => factor(i.item, i.pow * b)), this.num ** b);
	}
	scale(b: number): symbolic	{
		return b === 0 ? zero : b === 1 ? this : symbolicMul.create(this.factors, this.num * b);
	}

	mul(b: symbolic): symbolic	{
		if (isConst(b))
			return b.value === 0 ? zero : symbolicMul.create(this.factors, this.num * b.value);
		if (b instanceof symbolicMul)
			return mulFactors(this.num * b.num, ...this.factors, ...b.factors);
		return mulFactors(this.num, ...this.factors, factor(b));
	}

	substitute(map: Substitution) : symbolic {
		return mulFactors(this.num, ...this.factors.map(f => factor(f.item.substitute(map), f.pow)));
	}

	expand(options: ExpandOptions): symbolic {
		// expand each factor first
		const factors = this.factors.map(f => factor(f.item.expand(options), f.pow));

		const additive: symbolicAdd[] = [];
		const others: factor[] = [];

		for (const f of factors) {
			if (f.item instanceof symbolicAdd && f.pow > 0 && f.pow % 1 === 0) {
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
					parts2.push(mulFactors(this.num * t.coef, factor(p), factor(t.item, 1)));
				if (a.num !== 0)
					parts2.push(mulFactors(this.num * a.num, factor(p)));

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
	toString() : string {
		return this.factors.slice().sort((a, b) => b.pow - a.pow).map((i, j) =>
			(j === 0 ? (i.pow < 0 ? `${this.num} / `: this.num !== 1 ? `${this.num} * ` : '') : i.pow < 0 ? ' / ' : ' * ')
			+ i.item.toString()
			+ (Math.abs(i.pow) !== 1 ? superScript2(Math.abs(i.pow)) : '')
		).join('');
	}
}


//-----------------------------------------------------------------------------
// functions
//-----------------------------------------------------------------------------

function unaryToString(name: string) {
	return (a: symbolic) => {
		return `${name}(${a.toString()})`;
	};
}

function unaryFunction(name: string, evaluate: (arg: number) => number, derivative: (arg: symbolic) => symbolic, toString = unaryToString(name)) {
	const C = class extends symbolic {
		static create(i: symbolic)	{ return this.interner.internByKey(`${name}:${i.id}`, id => new C(id, i)); }
		create(i: symbolic)			{ return (this.constructor as any).create(i); }

		constructor(id: string, public arg: symbolic) {
			super(id);
		}
		substitute(map: Substitution) : symbolic {
			return this.create(this.arg.substitute(map));
		}

		expand(options: ExpandOptions): symbolic {
			return this.create(this.arg.expand(options));
		}
		//eq(b: symbolic): boolean { return b instanceof C && b.arg.eq(this.arg); }
		derivative(v: string): symbolic { return derivative(this.arg).mul(this.arg.derivative(v)); }
		evaluate(env?: Record<string, number>): number { return evaluate(this.arg.evaluate(env)); }
		toString(): string { return toString(this.arg); }
	};
	return C;
}

function binaryToString(name: string) {
	return (a: symbolic, b: symbolic) => {
		return `${name}(${a.toString()}, ${b.toString()})`;
	};
}

function binaryFunction(name: string, evaluate: (a: number, b: number) => number, derivative: (a: symbolic, b: symbolic) => [symbolic, symbolic], toString = binaryToString(name)) {
	const C = class extends symbolic {
		static create(i: symbolic, j: symbolic)	{ return this.interner.internByKey(`${name}:${i.id}:${j.id}`, id => new C(id, i, j)); }
		create(i: symbolic, j: symbolic)		{ return (this.constructor as any).create(i, j); }

		constructor(id: string, public arg1: symbolic, public arg2: symbolic) {
			super(id);
		}
		substitute(map: Substitution) : symbolic {
			return this.create(this.arg1.substitute(map), this.arg2.substitute(map));
		}

		expand(options: ExpandOptions): symbolic {
			return this.create(this.arg1.expand(options), this.arg2.expand(options));
		}

		//eq(b: symbolic): boolean { return b instanceof C && b.arg1.eq(this.arg1) && b.arg2.eq(this.arg2); }
		derivative(v: string): symbolic {
			const [d1, d2] = derivative(this.arg1, this.arg2);
			return d1.mul(this.arg1.derivative(v)).add(d2.mul(this.arg2.derivative(v)));
		}
		evaluate(env?: Record<string, number>): number { return evaluate(this.arg1.evaluate(env), this.arg2.evaluate(env)); }
		toString(): string { return toString(this.arg1, this.arg2); }
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

// exponential and logarithm
const symbolicExp = unaryFunction('exp', Math.exp, arg => symbolic.exp(arg));
const symbolicLog = unaryFunction('log', Math.log, arg => arg.recip());

const symbolicPow = binaryFunction('pow',
	Math.pow,
	(a, b) => [b.mul(symbolic.pow(a, b.sub(one))), symbolic.log(a).mul(symbolic.pow(a, b))] as [symbolic, symbolic],
	(a, b) => `(${a.toString()}) ^ (${b.toString()})`
);

// hyperbolic

const symbolicSinh = unaryFunction('sinh', Math.sinh, arg => symbolicCosh.create(arg));
const symbolicCosh = unaryFunction('cosh', Math.cosh, arg => symbolicSinh.create(arg));
const symbolicTanh = unaryFunction('tanh', Math.tanh, arg => symbolicCosh.create(arg).pow(-2));

const symbolicAsinh = unaryFunction('asinh', Math.asinh, arg => arg.pow(2).add(one).pow(-1 / 2));
const symbolicAcosh = unaryFunction('acosh', Math.acosh, arg => arg.pow(2).sub(one).pow(-1 / 2));
const symbolicAtanh = unaryFunction('atanh', Math.atanh, arg => one.sub(arg.pow(2)).recip());

