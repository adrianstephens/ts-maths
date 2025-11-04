/* eslint-disable no-restricted-syntax */
import {ops, compare} from './core';

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

	constructor(public id: string) {}

	neg():		symbolic		{ return symbolicAdd.create([addend(this, true)]); }
	recip():	symbolic		{ return symbolicMul.create([factor(this, -1)]); }
	pow(b: number): symbolic {
		return b === 1 ? this : b === 0 ? one
			: symbolicMul.create([{ pow: b, item: this }]);
	}

	scale(b: number): symbolic	{
		return (b === 0 ? zero : b === 1 ? this : symbolicMul.create([factor(this)], b));
		//return this.mul(symbolicConstant.create(b));
	}
	add(b: symbolic): symbolic	{
		return isConst(b) ? (b.value === 0 ? this : symbolicAdd.create([addend(this)], b.value))
			: b instanceof symbolicAdd ? adda(b.num, addend(this), ...b.addends)
			: adda(0, addend(this), addend(b));
	}
	sub(b: symbolic): symbolic	{ 
		//return this.add(b.neg());
		return isConst(b) ? (b.value === 0 ? this : symbolicAdd.create([addend(this)], -b.value))
			: b instanceof symbolicAdd ? adda(b.num, addend(this), ...b.addends.map(i => addend(i.item, i.neg)))
			: adda(0, addend(this), addend(b, true));
	}
	mul(b: symbolic): symbolic	{
		return isConst(b) ? (b.value === 0 ? b : b.value === 1 ? this : symbolicMul.create([factor(this)], b.value))
			: b instanceof symbolicMul ? mulf(b.num, ...b.factors, factor(this))
			: mulf(1, factor(this), factor(b));
	}

	div(b: symbolic): symbolic	{
	//	return this.mul(b.recip());
		return isConst(b) ? (b.value === 1 ? this : symbolicMul.create([factor(this)], 1 / b.value))
			: b instanceof symbolicMul ? mulf(b.num, ...b.factors.map(i => factor(i.item, -i.pow)), factor(this))
			: mulf(1, factor(this), factor(b, -1));
	}
	mag(): number				{ const v = this.evaluate(); return Number.isFinite(v) ? Math.abs(v) : NaN; }
	eq(b: symbolic) : boolean	{ return this === b;}

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

	eq(b: symbolic) : boolean	{ return b instanceof symbolicConstant && b.value === this.value; }
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
	eq(b: symbolic) : boolean { return b instanceof symbolicVariable && b.name === this.name; }

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

interface addend {
	item: symbolic;
	neg: boolean;
}
function addend(item: symbolic, neg = false): addend {
	return { item, neg };
}

function adda(num: number, ...a: addend[]): symbolic {
	const addends: addend[] = [];
	for (const i of a) {
		if (isConst(i.item)) {
			num += i.neg ? -i.item.value : i.item.value;

		} else if (i.item instanceof symbolicAdd) {
			num += i.item.num;
			for (const j of i.item.addends)
				addends.push(addend(j.item, j.neg !== i.neg));

		} else if (i.item instanceof symbolicMul && i.item.num < 0) {
			addends.push(addend(i.item.neg(), !i.neg));
		} else {
			addends.push(i);
		}
	}
	if (addends.length === 0)
		return symbolic.from(num);

	//canonical order
	addends.sort((a, b) => compare(a.item.id, b.item.id) || (a.neg !== b.neg ? (a.neg ? 1 : -1) : 0));

	//combine like terms
	let j = 0;
	for (let i = 1; i < addends.length; i++) {
		if (j >= 0 && addends[i].item.id === addends[j].item.id) {
			if (addends[i].neg !== addends[j].neg) {
				--j;
			} else {
				addends[j] = addend(addends[j].item.scale(2), addends[i].neg);
			}
		} else {
			addends[++j] = addends[i];
		}
	}
	addends.length = ++j;

	if (j === 0)
		return symbolic.from(num);

	//find common factors
	if (j > 1)
		commonFactors(addends);

	if (addends.length === 1 && num === 0)
		return addends[0].neg ? addends[0].item.neg() : addends[0].item;

	return symbolicAdd.create(addends, num);
}

function commonFactors(addends: addend[]) {
	interface factorInfo {
		item: symbolic;
		count: number;
		minpow: number;
		maxpow: number;
	}
	const factors = new Map<symbolic, factorInfo>();
	for (const m of addends) {
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

	if (factors2.length > 0) {//} && factors2[0][1] === addends.length && num === 0) {
		const factored: addend[] = [];
		let from = addends;
		for (const f of factors2) {
			const addends1: addend[] = [];
			const addends2: addend[] = [];
			
			for (const i of from) {
				if (i.item instanceof symbolicMul && i.item.factors.find(j => j.item === f.item)) {
					const factors = i.item.factors.map(j => j.item === f.item ? factor(j.item, j.pow - f.minpow) : j).filter(i => i.pow !== 0);
					addends2.push(addend(mulf(i.item.num, ...factors), i.neg));
				} else {
					addends1.push(i);
				}
			}
			factored.push(addend(adda(0, ...addends2).mul(f.item.pow(f.minpow))));
			from = addends1;
		}
		addends.length = 0;
		addends.push(...factored, ...from);
	}
}

class symbolicAdd extends symbolic {
	static create(addends: addend[], num = 0) : symbolic	{
		return this.interner.internByKey(`a:${addends.map(i => (i.neg ? '-' : '') + i.item.id).join(',')}:${num}`, id => new symbolicAdd(id, addends, num));
	}

	constructor(id: string, public addends: addend[], public num = 0) {
		super(id);
	}
	neg(): symbolic {
		if (this.num === 0 && this.addends.length === 1 && this.addends[0].neg)
			return this.addends[0].item;
		return symbolicAdd.create(this.addends.map(i => ({ neg: !i.neg, item: i.item })), -this.num);
	}

	eq(b: symbolic) : boolean {
		 return b instanceof symbolicAdd
		 	&& this.num === b.num
		 	&& this.addends.length === b.addends.length
			&& this.addends.every((v, i) => v.neg === b.addends[i].neg && v.item.eq(b.addends[i].item));
	}
	add(b: symbolic): symbolic	{
		if (isConst(b))
			return symbolicAdd.create(this.addends, this.num + b.value);
		if (b instanceof symbolicAdd)
			return adda(this.num * b.num, ...this.addends, ...b.addends);
		return adda(this.num, ...this.addends, addend(b));
	}
	derivative(v: string) : symbolic {
		return adda(0, ...this.addends.map(i => addend(i.item.derivative(v), i.neg)));
	}
	evaluate(env?: Record<string, number>) : number {
		return this.addends.reduce((acc, curr) => acc + (curr.item.evaluate(env) * (curr.neg ? -1 : 1)), this.num);
	}
	toString() : string {
		return '(' + this.addends.map((i, j) => 
			(j === 0 ? (this.num !== 0 ? (i.neg ? `${this.num} - ` : `${this.num} + `) : (i.neg ? '-' : '')) : i.neg ? ' - ' : ' + ')
			+ i.item.toString()).join('')
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

function mulf(num: number, ...f: factor[]): symbolic {
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

		} else if (i.item instanceof symbolicAdd && i.item.addends[0].neg && i.pow % 1 === 0) {
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

	eq(b: symbolic) : boolean {
		return b instanceof symbolicMul
			&& this.num === b.num
			&& this.factors.length === b.factors.length
			&& this.factors.every((v, i) => v.pow === b.factors[i].pow && v.item.eq(b.factors[i].item));
	}

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
			return mulf(this.num * b.num, ...this.factors, ...b.factors);
		return mulf(this.num, ...this.factors, factor(b));
	}

	derivative(v: string) : symbolic {
		return adda(0, ...this.factors.map(f => addend(mulf(
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
			+ (Math.abs(i.pow) !== 1 ? ` ^ ${Math.abs(i.pow)}` : '')
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
		static create(i: symbolic)			{ return this.interner.internByKey(`${name}:${i.id}`, id => new C(id, i)); }

		constructor(id: string, public arg: symbolic) {
			super(id);
		}
		eq(b: symbolic): boolean { return b instanceof C && b.arg.eq(this.arg); }
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
		static create(i: symbolic, j: symbolic) { return this.interner.internByKey(`${name}:${i.id}:${j.id}`, id => new C(id, i, j)); }

		constructor(id: string, public arg1: symbolic, public arg2: symbolic) {
			super(id);
		}

		eq(b: symbolic): boolean { return b instanceof C && b.arg1.eq(this.arg1) && b.arg2.eq(this.arg2); }
		derivative(v: string): symbolic {
			const [d1, d2] = derivative(this.arg1, this.arg2);
			return d1.mul(this.arg1.derivative(v)).add(d2.mul(this.arg2.derivative(v)));
		}
		evaluate(env?: Record<string, number>): number { return evaluate(this.arg1.evaluate(env), this.arg2.evaluate(env)); }
		toString(): string { return toString(this.arg1, this.arg2); }
	};
	return C;
}

const symbolicSin	= unaryFunction('sin', Math.sin, arg => symbolic.cos(arg));
const symbolicCos	= unaryFunction('cos', Math.cos, arg => symbolic.sin(arg).neg());
const symbolicTan	= unaryFunction('tan', Math.tan, arg => symbolic.cos(arg).pow(-2));

const symbolicAsin	= unaryFunction('asin', Math.asin, arg => symbolic.sqrt(one.sub(arg.pow(2))).recip());
const symbolicAcos	= unaryFunction('acos', Math.acos, arg => symbolic.sqrt(one.sub(arg.pow(2))).recip().neg());
const symbolicAtan	= unaryFunction('atan', Math.atan, arg => one.div(arg.pow(2).add(one)));

const symbolicAtan2 = binaryFunction('atan2',
	Math.atan2,
	(a, b) => {
		const denom = b.pow(2).add(a.pow(2));
		return [ b.div(denom), a.mul(symbolic.from(-1)).div(denom) ];
	}
);

const symbolicExp = unaryFunction('exp', Math.exp, arg => symbolic.exp(arg));
const symbolicLog = unaryFunction('log', Math.log, arg => symbolic.exp(arg).recip());

const symbolicPow = binaryFunction('pow',
	Math.pow,
	(a, b) => [b.mul(symbolic.pow(a, b.sub(one))), symbolic.log(a).mul(symbolic.pow(a, b))] as [symbolic, symbolic],
	(a, b) => `(${a.toString()}) ^ (${b.toString()})`
);

// hyperbolic

const symbolicSinh = unaryFunction('sinh', Math.sinh, arg => symbolic.cosh(arg));
const symbolicCosh = unaryFunction('cosh', Math.cosh, arg => symbolic.sinh(arg));
const symbolicTanh = unaryFunction('tanh', Math.tanh, arg => one.sub(symbolic.tanh(arg).pow(2)));

