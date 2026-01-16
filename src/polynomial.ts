/* eslint-disable custom/no-single-use-local */
/* eslint-disable no-restricted-syntax */
import {
	arithmeticOps,		scalar, scalarExt,
	isInstance,	isInstanceMaker, hasop, canop, hasStatic,	arrayOf,
	isScalar, isScalarExt,	asScalarT, sign, compare,
	builtinNumber,
	arrayEntry,
} from './core';

import real from './real';
import integer from './integer';
import big from './big';
import gen from './gen';
import complex, { complexT, complexFor, complexOps } from './complex';
import rational, { rationalB, canMakeRationalOps } from './rational';

import { toSuperscript } from './string';
import { factorisationI, factorisationB } from './prime';

import { squareFreeFactorization } from './factors';
export {PolyMod, PolyModFactory, Factor, Extension,
	squareFreeFactorization,
	factorOverQ, factorOverK, factorOverK_Q, factorOverK_I,
	factorSquareFreeOverK_Q, factorSquareFreeOverK_I,
	partialFractionsT, partialPower, resultant, sylvesterMatrix
} from './factors';

const sqrt3	= Math.sqrt(3);
const defaultEpsilon = 1e-9;

//-----------------------------------------------------------------------------
//	Interfaces
//-----------------------------------------------------------------------------
export type coeffOps<T,S = any>	= arithmeticOps<T, S> & (hasop<'mag'> | hasop<'sign'>);
export type PolyNTypes	= number | coeffOps<any>;
export type PolyTypes	= PolyNTypes | bigint;
export type PolyIType<T extends PolyTypes> = T extends number|rational ? number : T extends bigint|rationalB|canMakeRationalOps<any> ? bigint : never;

type normalized<T>		= T extends bigint ? rationalB : T;
type realRoot<T>		= T extends number ? number : T extends bigint ? rationalB : T extends scalar<any> ? T : never;
type rationalRoot<T>	= T extends number ? rational : T extends bigint ? rationalB : T extends rational ? rational : T extends rationalB ? rationalB : T extends canMakeRationalOps<any> ? rationalB : never;

interface evaluatable<T, U extends evaluatable<T, U> = any> extends hasop<'add'|'mul'|'scale', U, T> {
	from(t: T): U;
}

// Interface for normalized polynomials with implicit leading coefficient of 1
export interface PolynomialN<C> {
	c:		C[];
	is<U extends C>(g: (x: C) => x is U):			this is PolynomialN<U>;
	is<U extends PolyTypes>(g: (x: any) => x is U): this is PolynomialN<U>;

	degree():						number;
	dup(): 							PolynomialN<C>;
	evaluate(this: PolynomialN<number>, t: number): number;
	evaluate(this: PolynomialN<number>, t: complex): complex;
	evaluate(this: PolynomialN<bigint>, t: bigint): bigint;
	evaluate(this: PolynomialN<bigint>, t: rationalB): rationalB;
	evaluate<T extends coeffOps<T>>(this: PolynomialN<T>, t: T): T;
	evaluate<U extends evaluatable<C, U>>(t: U): U;

	sign():							number;
	abs():							PolynomialN<C>;
	deriv():						Polynomial<C>;
	mul(b: PolynomialN<C>):			PolynomialN<C>;
	div(b: PolynomialN<C>):			PolynomialN<C>;
	rationalRoots():				rationalRoot<C>[];
	realRoots(epsilon?: number):	realRoot<C>[];
	allRoots(epsilon?: number):		complexFor<C>[];

	unmonic():						Polynomial<C>;
	toString(x?: string, debug?: boolean): string;
}

// Interface for regular polynomials
export interface Polynomial<C> extends PolynomialN<C> {
	is<U extends C>(g: (x: C) => x is U):			this is Polynomial<U>;
	is<U extends PolyTypes>(g: (x: any) => x is U): this is Polynomial<U>;
	leadCoeff():					C;
	
	from(x: C|number|bigint):		Polynomial<C>;
	dup():							Polynomial<C>;
	neg():							Polynomial<C>;
	abs():							Polynomial<C>;
	add(b: C | Polynomial<C>):		Polynomial<C>;
	sub(b: C | Polynomial<C>):		Polynomial<C>;
	scale(b: C|number):				Polynomial<C>;
	rscale(b: C):					Polynomial<C>;
	mul(b: Polynomial<C>):			Polynomial<C>;
	divmod(b: Polynomial<C>):		Polynomial<C>;
	div(b: Polynomial<C>):			Polynomial<C>;
	ipow(b: number):				Polynomial<C>;
	compare(b: Polynomial<C>):		number;
	eq(b: Polynomial<C>):			boolean;
	lt(b: Polynomial<C>):			boolean;

	selfAdd(b: C | Polynomial<C>):	void;
	selfSub(b: C | Polynomial<C>):	void;
	selfScale(b: C):				void;
	selfRscale(b: C):				void;
	pseudoDivmod(b: Polynomial<C>):	Polynomial<C>;

	syntheticDiv(b: C|rationalRoot<C>):		C;
	content():						C | undefined;
	shift(n: number):				Polynomial<C>;
	
	normalise(epsilon?: number):	PolynomialN<normalized<C>>;
	map<U extends PolyTypes>(func: (c: C, i: number) => U): Polynomial<U>;
	toString(x?: string, debug?: boolean): string;
}

export function sparseClean<T>(a: T[]) {
	for (const i in a)
		if (a[i] === undefined)
			delete a[i];
	return a;
}

// Overloads to ensure numeric literal arrays like `[-1, 1]` resolve to `Polynomial<number>`
export function PolynomialN(c: readonly number[]): PolynomialN<number>;
export function PolynomialN<T extends coeffOps<T>>(c: T[]): PolynomialN<T>;
export function PolynomialN(c: readonly any[]) {
	for (const i in c) {
		switch (typeof c[i]) {
			case 'number':
				return new polynomialN((c as number[])) as PolynomialN<number>;
			default:
				return new polynomialNT(c as any) as unknown as PolynomialN<coeffOps<any>>;
		}
	}
}

// Overloads to ensure numeric literal arrays like `[-1, 1]` resolve to `Polynomial<number>`
export function Polynomial(c: readonly number[]): Polynomial<number>;
export function Polynomial(c: readonly bigint[]): Polynomial<bigint>;
export function Polynomial<T extends coeffOps<T, any>>(c: T[]): Polynomial<T>;
export function Polynomial<T extends PolyTypes>(c: T[]): Polynomial<T>;
export function Polynomial(c: readonly any[]) {
	for (const i in c) {
		switch (typeof c[i]) {
			case 'number':
				return new polynomial((c as number[])) as Polynomial<number>;
			case 'bigint':
				return new polynomialB(c as bigint[]) as Polynomial<bigint>;
			default:
				return new polynomialT(c as any) as unknown as Polynomial<coeffOps<any>>;
		}
	}
}

function integerPolynomialN(p : Polynomial<number>) : Polynomial<number> {
	const den	= real.commonDenominator(p.c.filter(()=>true));
	return p.map(v => Math.round(v * den));
}

export function integerPolynomial<T extends PolyTypes>(p0 : Polynomial<T>|PolynomialN<T>) : Polynomial<PolyIType<T>> {
	const p = p0.unmonic();
	if (p.is(big.is)) {
		return p as any;

	} else if (p.is(real.is)) {
		return integerPolynomialN(p) as any;

	} else if (p.is(isInstanceMaker(rationalB))) {
		const p2: Polynomial<rationalB> = p;
		const m = big.lcm(...p2.c.map(v => v.den));
		return p2.map(v => v.num * (m / v.den)) as any;

	} else if (p.is(isInstanceMaker(rational))) {
		const p2: Polynomial<rational> = p;
		const m = integer.lcm(...p2.c.map(v => v.den as integer));
		return p2.map(v => v.num * (m / v.den)) as any;

	} else if (p.is(isScalar)) {
		const p2: Polynomial<scalar<any, any> & hasop<'recip'|'divmod'>> = p as any;
		const den = gen.commonDenominator(p2.c.filter(()=>true));
		const one = p2.leadCoeff().from(1);
		return p2.map(v => {
			const t = v.scale(den);
			t.divmod(one);
			return t;
		});
	} else {
		return undefined as any;
	}
}

export function polyEvaluate<T>(c: T[], x: T, monic?: boolean): T;
export function polyEvaluate<T, U extends evaluatable<T, U>>(c: T[], x: U, monic?: boolean): U;
export function polyEvaluate(c: any[], x: any, monic = false) {
	const xtype = typeof x;
	if (xtype === typeof c[0]) {
		switch (xtype) {
			case 'object':
				if (x.prototype === c[0].prototype)
					return sparseEvaluateT(c, x, monic);
				break;
			case 'number':
				return sparseEvaluate(c, x, monic);
			case 'bigint':
				return sparseEvaluateB(c, x);
		}
	}
	return sparseEvaluateTU(c, x, monic);
}

//-----------------------------------------------------------------------------
//	helpers
//-----------------------------------------------------------------------------

function insertSorted<T>(arr: T[], value: T, less: (a: T, b: T) => boolean = (a, b) => a < b): T[] {
	const i = arr.findIndex(x => less(value, x));
	if (i === -1)
		arr.push(value);
	else
   		arr.splice(i, 0, value);
	return arr;
}

function debugString(t: any, debug = false): string {
	if (!t)
		return '0';
	const s = debug && typeof t === 'object' && t && Symbol.for("debug.description") in t ? t[Symbol.for("debug.description")]() : String(t);
	return s.includes(' ') ? `(${s})` : s;
}

function coefficientString<T>(coef: T, i: number, x: string, debug: boolean): string {
	return (i ? `${coef === 1 ? '' : debugString(coef, debug)}${x}${i > 1 ? toSuperscript(i.toString()) : ''}` : `${debugString(coef, debug)}`);
}

function polynomialString<T>(coefficients: T[], x: string, debug: boolean): string {
	return  coefficients.map((coef: T, i) => coefficientString(coef, i, x, debug)).reverse().join(' + ');
}

function scalarPolynomialString<T extends hasop<'sign'|'abs'>>(coefficients: T[], x: string, debug: boolean): string {
	return coefficients.map((coef, i) =>
		coef.sign() === 0 ? '' : (coef.sign() < 0 ? ' - ' : ' + ') + coefficientString(coef.abs(), i, x, debug)
	).reverse().join('');
}

function lessThan<T extends canop<'valueOf'>>(a: T, b: number): boolean {
	return	typeof a === 'number' ? a < b
		:	typeof a === 'bigint' ? a < BigInt(b)
		:	a.valueOf() < b;
}

//-----------------------------------------------------------------------------
//	sparse or dense arrays
//-----------------------------------------------------------------------------

function arrayCompare<T extends builtinNumber>(a: T[], b: T[]) {
	if (a.length < b.length)
		return -sign(b[b.length - 1]);
	if (a.length > b.length)
		return sign(a[a.length - 1]);
	for (let i = a.length; i--;) {
		const c = compare(a[i], b[i]);
		if (c)
			return c;
	}
	return 0;
}

function arrayCompareT<T extends hasop<'lt'|'sign'>>(a: T[], b: T[]) {
	if (a.length < b.length)
		return -b[b.length - 1].sign();
	if (a.length > b.length)
		return a[b.length - 1].sign();
	for (let i = a.length; i--;) {
		if (a[i] !== b[i]) {
			const c = a[i] && b[i] ? gen.compare(a[i], b[i]) : a[i] ? a[i].sign() : -b[i].sign();
			if (c)
				return c;
		}
	}
	return 0;
}

function arrayEq<T extends builtinNumber>(a: T[], b: T[]) {
	if (a.length !== b.length)
		return false;
	if (a.length === 0)
		return true;
	for (const i in a) {
		if (a[i] !== (b[i] ?? 0))
			return false;
	}
	for (const i in b) {
		if (b[i] && !a[i])
			return false;
	}
	return true;
}
function arrayEqT<T extends coeffOps<T>>(a: T[], b: T[]) {
	if (a.length !== b.length)
		return false;
	if (a.length === 0)
		return true;
	let haseq = false, hassign = false;
	for (const i in a) {
		const v = a[i];
		haseq	||= hasop('eq')(v);
		hassign ||= hasop('sign')(v);
		if (!b[i]) {
			if (!hassign || (v as any).sign())
				return false;
		} else if (!haseq || !(a[i] as any).eq(b[i])) {
			return false;
		}
	}
	for (const i in b) {
		hassign ||= hasop('sign')(b[i]);
		if (!a[i] && (!hassign || (b[i] as any).sign()))
			return false;
	}
	return true;
}
// scale & rscale

function arrayScale(a: number[], b: number): number[] {
	for (const i in a)
		a[i] *= b;
	return a;
}
function arrayScaleB(a: bigint[], b: bigint): bigint[] {
	for (const i in a)
		a[i] *= b;
	return a;
}
function arrayScaleT<T extends coeffOps<T>>(a: T[], b: T|number): T[] {
	if (typeof b === 'number') {
		for (const i in a)
			a[i] = a[i].scale(b);
	} else {
		for (const i in a)
			a[i] = a[i].mul(b);
	}
	return a;
}

function arrayRscaleB(a: bigint[], b: bigint): bigint[] {
	for (const i in a)
		a[i] /= b;
	return a;
}
function arrayRscaleT<T extends coeffOps<T>>(a: T[], b: T|number): T[] {
	if (typeof b === 'number')
		return arrayScaleT(a, 1 / b);
	for (const i in a)
		a[i] = a[i].div(b);
	return a;
}

//-----------------------------------------------------------------------------
//	sparse arrays
//-----------------------------------------------------------------------------
function sparseShift<T>(a: T[], n: number): T[] {
	return n === 0 ? a : n > 0 ? Array(n).concat(a) as T[] : a.slice(-n);
}

// clean
/*
function sparseClean(a: number[]) {
	for (const i in a)
		if (a[i] === 0)
			delete a[i];
}
function sparseCleanB(a: bigint[]) {
	for (const i in a)
		if (a[i] === 0n)
			delete a[i];
}
function sparseCleanT<T extends coeffOps<T>>(a: T[]) {
	if (arrayOf(a, hasop('sign'))) {
		for (const i in a)
			if (a[i].sign() === 0)
				delete a[i];
	} else if (arrayOf(a, hasop('mag'))) {
		for (const i in a)
			if (lessThan(a[i].mag(), defaultEpsilon))
				delete a[i];
	}
}
*/
// trim

function sparseTrim(a: number[]) {
	const keys = Object.keys(a);
	let k = keys.length;
	while (k-- && a[+keys[k]] === 0)
		;
	a.length = k < 0 ? 0 : +keys[k] + 1;
}
function sparseTrimB(a: bigint[]) {
	const keys = Object.keys(a);
	let k = keys.length;
	while (k-- && a[+keys[k]] === 0n)
		;
	a.length = k < 0 ? 0 : +keys[k] + 1;
}

function sparseTrimT<T extends coeffOps<T>>(a: T[]) {
	const keys = Object.keys(a);
	let k = keys.length;
	if (k) {
		if (arrayOf(a, hasop('sign'))) {
			while (k-- && a[+keys[k]].sign() === 0)
				;
		} else if (arrayOf(a, hasop('mag'))) {
			while (k-- && lessThan(a[+keys[k]].mag(), defaultEpsilon))
				;
		}
		a.length = k < 0 ? 0 : +keys[k] + 1;
	}
}

// add

function sparseAdd(a: number[], b: number[]): number[] {
	for (const i in b)
		a[i] = (a[i] ?? 0) + b[i];
	return a;
}
function sparseAddB(a: bigint[], b: bigint[]): bigint[] {
	for (const i in b)
		a[i] = (a[i] ?? 0n) + b[i];
	return a;
}
function sparseAddT<T extends coeffOps<T>>(a: T[], b: T[]): T[] {
	for (const i in b)
		a[i] = a[i] ? a[i].add(b[i]) : b[i];
	return a;
}

// sub

function sparseSub(a: number[], b: number[]): number[] {
	for (const i in b)
		a[i] = (a[i] ?? 0) - b[i];
	return a;
}
function sparseSubB(a: bigint[], b: bigint[]): bigint[] {
	for (const i in b)
		a[i] = (a[i] ?? 0n) - b[i];
	return a;
}

function sparseSubT<T extends coeffOps<T>>(a: T[], b: T[]): T[] {
	for (const i in b)
		a[i] = a[i] ? a[i].sub(b[i]) : b[i].neg();
	return a;
}

// evaluate

function sparseEvaluate(c: number[], x: number, monic: boolean): number {
    let result	= 0;
    let xpow	= 1;
    let exp 	= 0;
    
    for (const i in c) {
		for (const exp1 = +i; exp < exp1; ++exp)
            xpow *= x;
        result += c[i] * xpow;
    }
	if (monic) {
		for (const exp1 = c.length; exp < exp1; ++exp)
            xpow *= x;
        result += xpow;
	}
    return result;
}
function sparseEvaluateB(c: bigint[], x: bigint): bigint {
    let result	= 0n;
    let xpow	= 1n;
    let exp 	= 0;
    
    for (const i in c) {
		for (const exp1 = +i; exp < exp1; ++exp)
            xpow *= x;
        result += c[i] * xpow;
    }
    return result;
}
function sparseEvaluateT<T extends coeffOps<T>>(c: T[], x: T, monic: boolean): T {
    let result: T|undefined;
    let exp 	= 1;
	let xpow	= x;
	
	for (const i in c) {
		if (+i === 0) {
			result = c[0];
			continue;
		}
		for (const exp1 = +i; exp < exp1; ++exp)
			xpow = xpow.mul(x);
		result = result ? result.add(c[i].mul(xpow)) : c[i].mul(xpow);
	}
	if (monic) {
		for (const exp1 = c.length; exp < exp1; ++exp)
			xpow = xpow.mul(x);
		result = result ? result.add(xpow) : xpow;
	}
    return result!;
}
function sparseEvaluateTU<T, U extends evaluatable<T, U>>(c: T[], x: U, monic: boolean): U {
    let result: U|undefined;
	let xpow	= x;
    let exp 	= 1;
	
	for (const i in c) {
		const exp1 = +i;
		if (+i === 0) {
			result = x.from(c[exp1]);
			continue;
		}
		for (const exp1 = +i; exp < exp1; ++exp)
			xpow = xpow.mul(x);
		result = result ? result.add(xpow.scale(c[i])) : xpow.scale(c[i]);
	}
	if (monic) {
		for (const exp1 = c.length; exp < exp1; ++exp)
			xpow = xpow.mul(x);
		result = result ? result.add(xpow) : xpow;
	}
    return result!;
}

// multiply

function sparseMultiply(a: number[], b: number[], monic: boolean) {
	const r: number[] = [];
	for (const i in a) {
		const ai = a[i];
		for (const j in b) {
			const k = +i + +j;
			r[k] = (r[k] ?? 0) + ai * b[j];
		}
	}
	if (monic) {
		for (const i in a) {
			const k = +i + b.length;
			r[k] = (r[k] ?? 0) + a[i];
		}
		for (const i in b) {
			const k = +i + a.length;
			r[k] = (r[k] ?? 0) + b[i];
		}
	}
	return r;
}
function sparseMultiplyB(a: bigint[], b: bigint[]) {
	const r: bigint[] = [];
	for (const i in a) {
		const ai = a[i];
		for (const j in b) {
			const k = +i + +j;
			r[k] = (r[k] ?? 0n) + ai * b[j];
		}
	}
	return r;
}
function sparseMultiplyT<T extends coeffOps<T>>(a: T[], b: T[], monic: boolean) {
	const r: T[] = [];
	for (const i in a) {
		const ai = a[i];
		for (const j in b) {
			const k = +i + +j;
			r[k] = r[k] ? r[k].add(ai.mul(b[j])) : ai.mul(b[j]);
		}
	}
	if (monic) {
		for (const i in a) {
			const k = +i + b.length;
			r[k] = r[k] ? r[k].add(a[i]) : a[i];
		}
		for (const i in b) {
			const k = +i + a.length;
			r[k] = r[k] ? r[k].add(b[i]) : b[i];
		}
	}
	return r;
}

// divmod

function sparseDivMonic(a: number[], b: number[]) {
	const blen	= b.length;
	if (a.length >= blen) {
		for (let i = a.length - blen; i >= 0; --i) {
			const at = a[i + blen];
			if (at) {
				for (const j in b)
					a[i + +j] = (a[i + +j] ?? 0) - b[j] * at;
			}
		}
		return a.splice(blen);
	}
	return [];
}
function sparseDivMonicT<T extends coeffOps<T>>(a: T[], b: T[]) {
	const blen	= b.length;
	if (a.length >= blen) {
		for (let i = a.length - blen; i >= 0; --i) {
			const at = a[i + blen];
			if (at) {
				for (const j in b)
					a[i + +j] = a[i + +j] ? a[i + +j].sub(at.mul(b[j])) : at.mul(b[j]).neg();
			}
		}
		return a.splice(blen);
	}
	return [];
}
function sparseDivMod(a: number[], b: number[]) {
	const blen	= b.length - 1;
	if (a.length > blen) {
		const bt	= b[blen];
		for (let i = a.length - blen; i--;) {
			let at = a[i + blen];
			if (at) {
				at /= bt;
				for (const j in b)
					a[i + +j] = (a[i + +j] ?? 0) - b[j] * at;
				a[i + blen] = at;
			}
		}
		const q = a.splice(blen);
		sparseTrim(a);
		return q;
	}
	return [];
}
function sparseDivModB(a: bigint[], b: bigint[]) {
	const blen 	= b.length - 1;
	if (a.length > blen) {
		for (let i = a.length - blen; i--;) {
			const at = a[i + blen];
			if (at) {
				for (const j in b)
					a[i + +j] = (a[i + +j] ?? 0n) - b[j] * at;
				a[i + blen] = at;
			}
		}
		const q = a.splice(blen);
		sparseTrimB(a);
		return q;
	}
	return [];
}
function sparseDivModT<T extends coeffOps<T>>(a: T[], b: T[]) {
	const blen	= b.length - 1;
	if (a.length > blen) {
		const bt	= b[blen];
		for (let i = a.length - blen; i--;) {
			let at = a[i + blen];
			if (at) {
				at = at.div(bt);
				for (const j in b)
					a[i + +j] = a[i + +j] ? a[i + +j].sub(at.mul(b[j])) : at.mul(b[j]).neg();
				a[i + blen] = at;
			}
		}
		const q = a.splice(blen);
		sparseTrimT(a);
		return q;
	}
	return [];
}

// pseudo-division

function sparsePseudoDivmod(a: number[], b: number[]) {
	const blen	= b.length - 1;
	if (a.length > blen) {
		const bt = b[blen];
		for (let i = a.length - blen; i--;) {
			const at = a[i + blen];
			if (at) {
				arrayScale(a, bt);
				for (const j in b)
					a[i + +j] = (a[i + +j] ?? 0) - b[j] * at;
				a[i + blen] = at;
			}
		}
		const q = a.splice(blen);
		sparseTrim(a);
		return q;
	}
	return [];
}
function sparsePseudoDivmodB(a: bigint[], b: bigint[]) {
	const blen	= b.length - 1;
	if (a.length > blen) {
		const bt = b[blen];
		for (let i = a.length - blen; i--;) {
			const at = a[i + blen];
			if (at) {
				arrayScaleB(a, bt);
				for (const j in b)
					a[i + +j] = (a[i + +j] ?? 0n) - b[j] * at;
				a[i + blen] = at;
			}
		}
		const q = a.splice(blen);
		sparseTrimB(a);
		return q;
	}
	return [];
}
function sparsePseudoDivmodT<T extends coeffOps<T>>(a: T[], b: T[]) {
	const blen	= b.length - 1;
	if (a.length > blen) {
		const bt = b[blen];
		for (let i = a.length - blen; i--;) {
			const at = a[i + blen];
			if (at) {
				arrayScaleT(a, bt);
				for (const j in b)
					a[i + +j] = a[i + +j] ? a[i + +j].sub(at.mul(b[j])) : at.mul(b[j]).neg();
				a[i + blen] = at;
			}
		}
		const q = a.splice(blen);
		sparseTrimT(a);
		return q;
	}
	return [];
}

function sparseSyntheticDiv(a: number[], r: number): number {
	for (let i = a.length - 1; i--;)
		a[i] = (a[i] ?? 0) + a[i + 1] * r;
	return a.shift() ?? 0;
}
function sparseSyntheticDivR(a: number[], r: rational): number {
	for (let i = a.length - 1; i--;)
		a[i] = (a[i] ?? 0) * r.den + a[i + 1] * r.num;
	return a.shift() ?? 0;
}

function sparseSyntheticDivB(a: bigint[], r: bigint): bigint {
	for (let i = a.length - 1; i--;)
		a[i] = (a[i] ?? 0n) + a[i + 1] * r;
	return a.shift() ?? 0n;
}
function sparseSyntheticDivBR(a: bigint[], r: rationalB): bigint {
	for (let i = a.length - 1; i--;)
		a[i] = (a[i] ?? 0n) * r.den + a[i + 1] * r.num;
	return a.shift() ?? 0n;
}

function sparseSyntheticDivT<T extends coeffOps<T>>(a: T[], r: T): T {
	for (let i = a.length - 1; i--;)
		a[i] = a[i] ? a[i].add(a[i + 1].mul(r)) : a[i + 1].mul(r);
	return a.shift()!;
}
function sparseSyntheticDivTR<T extends coeffOps<T>>(a: T[], r: rationalRoot<T>): T {
	for (let i = a.length - 1; i--;)
		a[i] = a[i] ? a[i].scale(r.den).add(a[i + 1].scale(r.num)) : a[i + 1].scale(r.num);
	return a.shift()!;
}


//-----------------------------------------------------------------------------
//	dense arrays
//-----------------------------------------------------------------------------
/*
// trim

function denseTrim(a: (number[])|(bigint[])) {
	while (a.length && !a[a.length - 1])
		a.pop();
}
function denseTrimT<T extends ops<T>>(a: T[]) {
	if (arrayOf(a, hasop('sign'))) {
		while (a.length && a[a.length - 1].sign() === 0)
			a.pop();
	} else {
		while (a.length && lessThan(a[a.length - 1].mag(), defaultEpsilon))
			a.pop();
	}
}

// add

function denseAdd(a: number[], b: number[]): number[] {
	for (const i in b)
		a[i] += b[i];
	return a;
}
function denseAddB(a: bigint[], b: bigint[]): bigint[] {
	for (const i in b)
		a[i] += b[i];
	return a;
}
function denseAddT<T extends ops<T>>(a: T[], b: T[]): T[] {
	for (const i in b)
		a[i] = a[i].add(b[i]);
	return a;
}

// sub

function denseSub(a: number[], b: number[]): number[] {
	for (const i in b)
		a[i] -= b[i];
	return a;
}
function denseSubB(a: bigint[], b: bigint[]): bigint[] {
	for (const i in b)
		a[i] -= b[i];
	return a;
}
function denseSubT<T extends ops<T>>(a: T[], b: T[]): T[] {
	for (const i in b)
		a[i] = a[i].sub(b[i]);
	return a;
}

// evaluate

function denseEvaluate(c: number[], t: number, monic: boolean): number {
	let i = c.length;
	let r = c[--i];
	if (monic)
		r += t;
	while (i--)
		r = r * t + c[i];
	return r;
}
function denseEvaluateB(c: bigint[], x: bigint): bigint {
	let i = c.length;
	let r = c[--i];
	while (i--)
		r = r * x + c[i];
	return r;
}
function denseEvaluateT<T extends ops<T>>(c: T[], t: T, monic: boolean): T {
	let i = c.length;
	let r = c[--i];
	if (monic)
		r = r.add(t);
	while (i--)
		r = r.mul(t).add(c[i]);
	return r;
}

function denseEvaluateNT<T extends ops<T> & hasop<'from'>>(c: number[], t: T, monic: boolean): T {
	let i = c.length;
	let r = t.from(c[--i]);
	if (monic)
		r = r.add(t);
	while (i--)
		r = r.mul(t).add(t.from(c[i]));
	return r;
}
function denseEvaluateBR(c: bigint[], x: rationalB): rationalB {
	let i = c.length;
	let acc		= c[--i];
	let denPow = 1n;
	while (i--) {
		denPow *= x.den;
		acc = acc * x.num + c[i] * denPow;
	}
	return new rationalB(acc, denPow);
}

// multiply

function denseMultiply(a: number[], b: number[], monic: boolean) {
	const r = new Array<number>(a.length + b.length).fill(0);
	for (let i = 0; i < a.length; i++)
		for (let j = 0; j < b.length; j++)
			r[i + j] += a[i] * b[j];

	if (monic) {
		for (let i = 0; i < a.length; i++)
			r[i + b.length] += a[i];

		for (let i = 0; i < b.length; i++)
			r[i + a.length] += b[i];
	}
}
function denseMultiplyB(a: bigint[], b: bigint[]) {
	const r = new Array<bigint>(a.length + b.length - 1).fill(0n);
	for (let i = 0; i < a.length; i++)
		for (let j = 0; j < b.length; j++)
			r[i + j] += a[i] * b[j];
	return r;
}
function denseMultiplyT<T extends ops<T>>(a: T[], b: T[], monic: boolean) {
	const zero = a[0].scale(0);
	const blen = monic ? b.length : b.length - 1;
	return Array.from({ length: a.length + blen }, (_, k) => {
		let sum = zero;
		for (let i = Math.max(0, k - blen); i <= Math.min(a.length - 1, k); i++)
			sum = sum.add(a[i].mul(b[k - i]));
		return sum;
	});
}

// divmod

function denseDivMod(a: number[], b: number[], monic: boolean) {
	const blen	= b.length - (monic ? 0 : 1);
	const qlen	= Math.max(a.length - b.length + 1, 0);
	const bt	= b[blen];
	const q: number[] = [];

	for (let i = qlen; i--;) {
		let at = a[i + blen];
		if (at) {
			if (!monic)
				at /= bt;
			q[i] = at;
			for (const j in b)
				a[i + +j] -= b[j] * at;
		} else {
			q[i] = 0;
		}
	}
	a.length = blen;
	denseTrim(a);
	return q;
}
function denseDivModB(a: bigint[], b: bigint[], monic: boolean) {
	const blen	= b.length - (monic ? 0 : 1);
	const qlen	= Math.max(a.length - b.length + 1, 0);
	const bt	= b[blen];
	const q: bigint[] = [];

	for (let i = qlen; i--;) {
		let at = a[i + blen];
		if (at) {
			if (!monic)
				at /= bt;
			q[i] = at;
			for (const j in b)
				a[i + +j] -= b[j] * at;
		} else {
			q[i] = 0n;
		}
	}
	a.length = blen;
	denseTrim(a);
	return q;
}
function denseDivModT<T extends ops<T>>(a: T[], b: T[], monic: boolean) {
	const blen	= b.length - (monic ? 0 : 1);
	const qlen	= Math.max(a.length - b.length + 1, 0);
	const bt	= b[blen];
	const q: T[] = [];

	for (let i = qlen; i--;) {
		let at = a[i + blen];
		if (at) {
			if (!monic)
				at = at.div(bt);
			q[i] = at;
			for (const j in b)
				a[i + +j] = a[i + +j].sub(b[j].mul(at));
		} else {
			q[i] = at;
		}
	}
	a.length = blen;
	denseTrimT(a);
	return q;
}


// pseudo-division

function densePseudoRem(a: number[], b: number[]) {
	const blen	= b.length - 1;
	const qlen	= Math.max(a.length - blen, 0);
	const bt	= b[blen];

	for (let i = qlen; i--;) {
		const at = a[i + blen];
		if (at) {
			arrayScale(a, bt);
			for (const j in b)
				a[i + +j] -= b[j] * at;
		}
	}
	a.length = blen;
	sparseTrim(a);
}
function densePseudoRemB(a: bigint[], b: bigint[]) {
	const blen	= b.length - 1;
	const qlen	= Math.max(a.length - blen, 0);
	const bt	= b[blen];

	for (let i = qlen; i--;) {
		const at = a[i + blen];
		if (at) {
			arrayScaleB(a, bt);
			for (const j in b)
				a[i + +j] -= b[j] * at;
		}
	}
	a.length = blen;
	sparseTrimB(a);
}
function densePseudoRemT<T extends ops<T>>(a: T[], b: T[]) {
	const blen	= b.length - 1;
	const qlen	= Math.max(a.length - blen, 0);
	const bt	= b[blen];

	for (let i = qlen; i--;) {
		const at = a[i + blen];
		if (at) {
			arrayScaleT(a, bt);
			for (const j in b)
				a[i + +j] = a[i + +j].sub(b[j].mul(at));
		}
	}
	a.length = blen;
	sparseTrimT(a);
}
*/
//-----------------------------------------------------------------------------
//	Polynomial with real coefficients
//-----------------------------------------------------------------------------

class polynomial implements Polynomial<number> {
	constructor(public c: number[]) {
		sparseTrim(c);
	}
	is<U extends number>(g: (x: number) => x is U):	this is Polynomial<U>;
	is<U extends PolyTypes>(g: (x: any) => x is U):	this is Polynomial<U>;
	is(g: (x: any) => boolean):						this is Polynomial<any> { return arrayOf(this.c, g as any); }

	leadCoeff() 			{ return this.c[this.c.length - 1] ?? 0; }
	unmonic()				{ return this; }
	degree()				{ return this.c.length - 1; }
	content() 				{ return real.gcd(...this.c); }
	shift(n: number)		{ return new polynomial(sparseShift(this.c, n)); }

	dup()					{ return new polynomial(this.c.slice()); }
	abs()					{ return this.leadCoeff() < 0 ? new polynomial(this.c.map(v => -v)) : this; }
	neg()					{ return new polynomial(this.c.map(v => -v)); }
	sign()					{ return Math.sign(this.leadCoeff()); }
	from(x: number) 		{ return new polynomial([x]); }
	scale(b: number)		{ return new polynomial(this.c.map(a => a * b)); }
	rscale(b: number)		{ return this.scale(1 / b); }
	ipow(b: number)			{ return gen.ipow(this, b); }

	compare(b: polynomial)	{ return arrayCompare(this.c, b.c); }
	eq(b: polynomial)		{ return arrayEq(this.c, b.c); }
	lt(b: polynomial)		{ return this.compare(b) < 0; }

	evaluate<U extends evaluatable<number>|number>(t: U): U {
		return typeof t === 'number'
			? sparseEvaluate(this.c, t, false) as U
			: sparseEvaluateTU(this.c, t, false) as U;
	}
	deriv() {
		return new polynomial(this.c.slice(1).map((v, i) => v * (i + 1)));
	}
	add(b: number|polynomial) {
		return this.dup().selfAdd(b);
	}
	sub(b: number|polynomial) {
		return this.dup().selfSub(b);
	}
	mul(b: polynomial) {
		return new polynomial(sparseMultiply(this.c, b.c, false));
	}
	div(b: polynomial) {
		return new polynomial(sparseDivMod(this.c.slice(), b.c));
	}

	selfAdd(b: number|polynomial) {
		if (typeof b === 'number') {
			this.c[0] = (this.c[0] ?? 0) + b;
		} else {
			sparseAdd(this.c, b.c);
			sparseTrim(this.c);
		}
		return this;
	}
	selfSub(b: number|polynomial) {
		if (typeof b === 'number') {
			this.c[0] = (this.c[0] ?? 0) - b;
		} else {
			sparseSub(this.c, b.c);
			sparseTrim(this.c);
		}
		return this;
	}
	selfScale(b: number) {
		arrayScale(this.c, b);
	}
	selfRscale(b: number) {
		this.selfScale(1 / b);
	}

	divmod(b: polynomial) {
		return new polynomial(sparseDivMod(this.c, b.c));
	}
	pseudoDivmod(b: polynomial) {
		return new polynomial(sparsePseudoDivmod(this.c, b.c));
	}
	syntheticDiv(b: number|rational): number {
		return typeof b === 'number'
			? sparseSyntheticDiv(this.c, b)
			: sparseSyntheticDivR(this.c, b);
	}

	normalise(epsilon = defaultEpsilon) {
		let i = this.c.length - 1;
		while (i && Math.abs(this.c[i]) < epsilon)
			i--;
		const f = 1 / this.c[i];
		return new polynomialN(this.c.slice(0, i).map(v => v * f));
	}
	rationalRoots(): rational[] {
		return rationalRoots(this);
	}
	realRoots(epsilon = defaultEpsilon): number[] {
		return this.normalise().realRoots(epsilon);
	}
	allRoots(epsilon = defaultEpsilon): complex[] {
		return this.normalise(epsilon).allRoots(epsilon);
	}
	map<U extends PolyTypes>(func: (c: number, i: number) => U): Polynomial<U> {
		return Polynomial(sparseClean(this.c.map((c, i) => func(c, i))));
	}

	toString(x = 'x', debug = false) {
		if (this.c.length < 2)
			return this.c.length ? String(this.c[0]) : '0';
		return coefficientString(this.leadCoeff(), this.degree(), x, debug) + this.c.slice(0, -1).map((coef, i) =>
			coef === 0 ? '' : (coef < 0 ? ' - ' : ' + ') + coefficientString(Math.abs(coef), i, x, debug)
		).reverse().join('');
	}
	[Symbol.for("debug.description")]() { return this.toString('x', true); }
}

//-----------------------------------------------------------------------------
//	Normalised (monic) Polynomial with implicit leading coefficient of 1
//-----------------------------------------------------------------------------

class polynomialN implements PolynomialN<number> {
	constructor(public c: number[]) {}
	is<U extends number>(g: (x: number) => x is U):	this is PolynomialN<U>;
	is<U extends PolyTypes>(g: (x: any) => x is U):	this is PolynomialN<U>;
	is(g: (x: any) => boolean):						this is PolynomialN<any> { return arrayOf(this.c, g as any); }

	unmonic()	{ return new polynomial([...this.c, 1]); }
	degree()	{ return this.c.length; }
	dup()		{ return new polynomialN(this.c.slice()); }
	abs()		{ return this; }
	sign()		{ return this.c.length ? 1 : 0; }

	evaluate<U extends number|evaluatable<number>>(t: U): U {
		return typeof t === 'number'
			? sparseEvaluate(this.c, t, true) as U
			: sparseEvaluateTU(this.c, t, true) as U;
	}
	deriv() {
		return new polynomial([...this.c.slice(1).map((v, i) => v * (i + 1)), this.c.length]);
	}
	mul(b: polynomialN) {
		return new polynomialN(sparseMultiply(this.c, b.c, true));
	}
	div(b: polynomialN) {
		return new polynomialN(sparseDivMonic(this.c.slice(), b.c));
	}
	rationalRoots(): rational[] {
		return rationalRoots(this);
	}
	realRoots(epsilon = defaultEpsilon): number[] {
		return normPolyRealRootsN(this.c, epsilon);
	}
	allRoots(epsilon = defaultEpsilon): complex[] {
		return normPolyComplexRootsN(this.c, epsilon);
	}
	toString(x = 'x', debug = false) {
		return	this.degree() === 0 ? '1'
			:	coefficientString(1, this.degree(), x, debug) + this.c.map((coef, i) =>
					coef === 0 ? '' : (coef < 0 ? ' - ' : ' + ') + coefficientString(Math.abs(coef), i, x,debug)
			).reverse().join('');
	}
	[Symbol.for("debug.description")]() { return this.toString('x', true); }
}

//-----------------------------------------------------------------------------
//	Polynomial with bigint coefficients
//-----------------------------------------------------------------------------

class polynomialB implements Polynomial<bigint> {
	constructor(public c: bigint[]) { sparseTrimB(this.c); }
	is<U extends bigint>(g: (x: bigint) => x is U):	this is Polynomial<U>;
	is<U extends PolyTypes>(g: (x: any) => x is U):	this is Polynomial<U>;
	is(g: (x: any) => boolean):						this is Polynomial<any> { return arrayOf(this.c, g as any); }

	leadCoeff() 			{ return this.c[this.c.length - 1] ?? 0n; }
	unmonic()				{ return this; }
	degree()				{ return this.c.length - 1; }
	content()				{ return big.gcd(...this.c); }
	shift(n: number)		{ return new polynomialB(sparseShift(this.c, n)); }

	dup()					{ return new polynomialB(this.c.slice()); }
	abs()					{ return this.leadCoeff() < 0n ? new polynomialB(this.c.map(v => -v)) : this; }
	neg()					{ return new polynomialB(this.c.map(v => -v)); }
	sign()					{ return big.sign(this.leadCoeff()); }
	from(x: bigint|number)	{ return new polynomialB([BigInt(x)]); }
	scale(b: bigint)		{ return new polynomialB(this.c.map(a => a * b)); }
	rscale(b: bigint)		{ return new polynomialB(this.c.map(a => a / b)); }
	ipow(b: number)			{ return gen.ipow(this, b); }

	compare(b: polynomialB)	{ return arrayCompare(this.c, b.c); }
	eq(b: polynomialB)		{ return arrayEq(this.c, b.c); }
	lt(b: polynomialB)		{ return this.compare(b) < 0; }

	evaluate<U extends bigint|rationalB>(t: U): U {
		return isInstance(t, rationalB)
			? rationalB(sparseEvaluateB(this.c, t.num), sparseEvaluateB(this.c, t.den)) as U
			: sparseEvaluateB(this.c, t) as U;
	}
	deriv() {
		return new polynomialB(this.c.slice(1).map((v, i) => v * BigInt(i + 1)));
	}
	add(b: bigint|polynomialB) {
		return this.dup().selfAdd(b);
	}
	sub(b: bigint|polynomialB) {
		return this.dup().selfSub(b);
	}
	mul(b: polynomialB) {
		return new polynomialB(sparseMultiplyB(this.c, b.c));
	}
	div(b: polynomialB) {
		return new polynomialB(sparseDivModB(this.c.slice(), b.c));
	}

	selfAdd(b: bigint|polynomialB) {
		if (typeof b === 'bigint') {
			this.c[0] = (this.c[0] ?? 0n) + b;
		} else {
			sparseAddB(this.c, b.c);
			sparseTrimB(this.c);
		}
		return this;
	}
	selfSub(b: bigint|polynomialB) {
		if (typeof b === 'bigint') {
			this.c[0] = (this.c[0] ?? 0n) - b;
		} else {
			sparseSubB(this.c, b.c);
			sparseTrimB(this.c);
		}
		return this;
	}
	selfScale(b: bigint) {
		arrayScaleB(this.c, b);
	}
	selfRscale(b: bigint) {
		arrayRscaleB(this.c, b);
	}

	divmod(b: polynomialB) {
		return new polynomialB(sparseDivModB(this.c, b.c));
	}
	pseudoDivmod(b: polynomialB) {
		return new polynomialB(sparsePseudoDivmodB(this.c, b.c));
	}
	syntheticDiv(b: bigint|rationalB): bigint {
		return typeof b === 'bigint'
			? sparseSyntheticDivB(this.c, b)
			: sparseSyntheticDivBR(this.c, b);
	}

	normalise(_epsilon = defaultEpsilon) {
		const d = this.leadCoeff();
		return new polynomialNT<rationalB>(this.c.slice(0, -1).map(v => rationalB(v, d)));
	}

	rationalRoots(): rationalB[] {
		return rationalRoots(this);
	}
	realRoots(): rationalB[] {
		const sf = squareFreeFactorization(this);
		const roots: rationalB[] = [];

		for (const {factor, multiplicity} of sf) {
			roots.push(...Array(multiplicity).fill(squareFreeRationalRootsB(factor as Polynomial<bigint>)).flat());
			if (factor.degree() > 0)
				roots.push(...factor.normalise().realRoots());
		}
		return roots;
	}
	allRoots(): never {
		return undefined as never;
	}
	map<U extends PolyTypes>(func: (c: bigint, i: number) => U): Polynomial<U> {
		return Polynomial(sparseClean(this.c.map((c, i) => func(c, i))));
	}

	toString(x = 'x', debug = false) {
		return this.c.map((coef, i) => {
			return i === this.degree() ? coefficientString(coef, i, x, debug)
				:  (coef < 0n ? ' - ' : ' + ') + coefficientString(big.abs(coef), i, x, debug);
		}).reverse().join('');
	}
	[Symbol.for("debug.description")]() { return this.toString('x', true); }
}

//-----------------------------------------------------------------------------
//	Polynomial with generic coefficients supporting ops interface
//-----------------------------------------------------------------------------

class polynomialT<T extends coeffOps<T>> implements Polynomial<T> {
	constructor(public c: T[]) {
		sparseTrimT(c);
	}
	is<U extends T>(g: (x: T) => x is U):			this is Polynomial<U>;
	is<U extends PolyTypes>(g: (x: any) => x is U):	this is Polynomial<U>;
	is(g: (x: any) => boolean):						this is Polynomial<any> { return g(this.leadCoeff()); }

	isCoeff(b: any): b is T	{ return b.constructor === this.leadCoeff()?.constructor; }

	unmonic()			{ return this; }
	leadCoeff() 		{ return this.c[this.c.length - 1]; }
	degree()			{ return this.c.length - 1; }
	shift(n: number)	{ return new polynomialT(sparseShift(this.c, n)); }
	dup() 				{ return new polynomialT<T>(this.c.slice()); }
	abs()				{ return this.is(hasop('sign')) && this.c[this.c.length-1].sign() < 0 ? new polynomialT(this.c.map(v => v.neg())) : this; }
	neg()				{ return new polynomialT(this.c.map(v => v.neg())); }
	sign()				{ return this.c.length < 1 ? 0 : this.is(hasop('sign')) ? this.c[this.c.length-1].sign() : NaN; }
	scale(b: number|T)	{ return new polynomialT(typeof b === 'number' ? this.c.map(a => a.scale(b)) : this.c.map(a => a.mul(b))); }
	rscale(b: number|T)	{ return typeof b === 'number' ? this.scale(1 / b) : new polynomialT(this.c.map(a => a.div(b))); }
	ipow(b: number)		{ return gen.ipow(this, b); }

	compare(b: polynomialT<T>)	{ return this.is(hasop('lt')) ? arrayCompareT(this.c as any, b.c as any) : NaN; }
	eq(b: polynomialT<T>) 		{ return arrayEqT(this.c, b.c); }
	lt(b: polynomialT<T>)		{ return this.compare(b) < 0; }

	from(x: number|T) {
		if (!real.is(x))
			return new polynomialT([x]);
		const lead = this.leadCoeff();
		if (lead && hasop('from')(lead))
			return new polynomialT([lead.from(x)]);
		throw new Error("Cannot create polynomial from type");
	}

	content(): T extends scalar<any> ? T : never {
		return (this.is(isScalarExt) ? gen.gcd(...this.c) : undefined) as any;
	}

	evaluate(t: any) {
		return this.isCoeff(t)
			? sparseEvaluateT(this.c, t, false)
			: sparseEvaluateTU(this.c, t, false);
	}
	deriv() {
		return new polynomialT(this.c.slice(1).map((v, i) => v.scale(i + 1)));
	}
	add(b: T|polynomialT<T>) {
		return this.dup().selfAdd(b);
	}
	sub(b: T|polynomialT<T>) {
		return this.dup().selfSub(b);
	}
	mul(b: polynomialT<T>) {
		return new polynomialT(sparseMultiplyT(this.c, b.c, false));
	}
	div(b: polynomialT<T>) {
		return this.dup().divmod(b);
	}
	selfAdd(b: T|polynomialT<T>) {
		if (b) {
			if (b instanceof polynomialT) {
				sparseAddT(this.c, b.c);
				sparseTrimT(this.c);
			} else {
				this.c[0] = this.c[0] ? this.c[0].add(b) : b;
			}
		}
		return this;
	}
	selfSub(b: T|polynomialT<T>) {
		if (b) {
			if (b instanceof polynomialT) {
				sparseSubT(this.c, b.c);
				sparseTrimT(this.c);
			} else {
				this.c[0] = this.c[0] ? this.c[0].sub(b) : b.neg();
			}
		}
		return this;
	}
	selfScale(b: number|T) {
		arrayScaleT(this.c, b);
	}
	selfRscale(b: number|T) {
		arrayRscaleT(this.c, b);
	}
	
	divmod(b: polynomialT<T>) {
		return new polynomialT(sparseDivModT(this.c, b.c));
	}
	pseudoDivmod(b: polynomialT<T>) {
		return new polynomialT(sparsePseudoDivmodT(this.c, b.c));
	}
	syntheticDiv(b: T|rationalRoot<T>): T {
		return this.isCoeff(b)
			? sparseSyntheticDivT(this.c, b)
			: sparseSyntheticDivTR(this.c, b);
	}

	normalise(epsilon = defaultEpsilon): PolynomialN<normalized<T>> {
		let i = this.c.length - 1;
		if (this.is(hasop('mag'))) {
			while (i && lessThan(this.c[i].mag(), epsilon))
				i--;
		} else if (this.is(hasop('sign'))) {
			while (i && this.c[i].sign() === 0)
				i--;
		}

		const f = this.c[i];
		return new polynomialNT<T>(this.c.slice(0, i).map(v => v.div(f))) as unknown as PolynomialN<normalized<T>>;
	}

	rationalRoots(): rationalRoot<T>[] {
		return rationalRoots(this);
	}

	realRoots(epsilon = defaultEpsilon): realRoot<T>[] {
		return this.normalise().realRoots(epsilon) as any;
	}
	allRoots(epsilon = defaultEpsilon): complexFor<T>[] {
		return this.normalise().allRoots!(epsilon) as any;
	}
	map<U extends PolyTypes>(func: (c: T, i: number) => U): Polynomial<U> {
		return Polynomial(sparseClean(this.c.map((c, i) => func(c, i))));
	}

	toString(x = 'x', debug = false) {
		if (this.c.length < 2)
			return this.c.length ? String(this.c[0]) : '0';
		const c = this.c.slice(0, -1);
		const s = coefficientString(this.leadCoeff(), c.length, x, debug);
		if (arrayOf(c, hasop('abs')))
			return s + scalarPolynomialString(c as any, x, debug);
		const s2 = polynomialString(c, x, debug);
		return s2 ? s + ' + ' + s2 : s;
	}
//	[Symbol.for("debug.description")]() { return this.toString('x', true); }
}

//-----------------------------------------------------------------------------
//	Normalised (monic) General Polynomial with implicit leading coefficient of 1
//-----------------------------------------------------------------------------

class polynomialNT<T extends coeffOps<T>> implements PolynomialN<T> {
	constructor(public c: T[]) {}
	is<U extends T>(g: (x: T) => x is U):			this is PolynomialN<U>;
	is<U extends PolyTypes>(g: (x: any) => x is U):	this is PolynomialN<U>;
	is(g: (x: any) => boolean):						this is PolynomialN<any> { return arrayOf(this.c, g as any); }

	isCoeff(b: any): b is T	{ return arrayOf(this.c, (x: any): x is T => x.constructor === b.constructor); }

	unmonic()	{
		const x = arrayEntry(this.c);
		if (!x || !hasop('from')(x))
			throw new Error("can't unmonic");
		return new polynomialT([...this.c, x.from(1)]);
	}
	degree()	{ return this.c.length; }
	dup()		{ return new polynomialNT<T>(this.c.slice()); }
	abs()		{ return this; }
	sign()		{ return this.c.length ? 1 : 0; }

	evaluate(t: any) {
		return this.isCoeff(t)
			? sparseEvaluateT(this.c, t, true)
			: sparseEvaluateTU(this.c, t, true);
	}

	deriv() {
		if (hasop('from')(this.c[0]))
			return new polynomialT<T>([...this.c.slice(1).map((v, i) => v.scale(i + 1)), this.c[0].from(this.c.length)]);
		return undefined as never;
	}
	mul(b: PolynomialN<T>) {
		return new polynomialNT<T>(sparseMultiplyT(this.c, b.c, true));
	}
	div(b: polynomialNT<T>) {
		return new polynomialNT<T>(sparseDivMonicT(this.c.slice(), b.c));
	}

	rationalRoots(): rationalRoot<T>[] {
		return rationalRoots(this);
	}
	realRoots(epsilon = defaultEpsilon): realRoot<T>[] {
		if (this.is(isScalar)) {
			const eps = this.c[0].from(epsilon);
			return (this.is(hasop('rpow'))
				? normPolyRealRootsT(this.c, eps)
				: refine_roots(this, sturmIsolateIntervalsT(this, eps).map(i => bisectRootT(this, i.min, i.max, eps)))
		 	) as any;
		}
		return undefined as never;
	}
	allRoots(epsilon = defaultEpsilon): complexFor<T>[] {
		if (this.is(isScalarExt))
			return normPolyComplexRootsT(this.c, this.c[0].from(epsilon)) as complexFor<T>[];
		return undefined as never;
	}
	toString(x = 'x', debug = false) {
		if (this.degree() === 0)
			return '1';
		const s = coefficientString(1, this.degree(), x, debug);
		if (arrayOf(this.c, hasop('abs')))
			return s + scalarPolynomialString(this.c as any, x, debug);
		const s2 = polynomialString(this.c, x, debug);
		return s2 ? s + ' + ' + s2 : s;
	}
	[Symbol.for("debug.description")]() { return this.toString('x', true); }
}


function gcdPowers(c: number[], monic: boolean) {
	return c[1] ? 1 : integer.gcd(...Object.keys(c).map(i => +i as integer).filter(i => c[i] !== 0), ...(monic ? [c.length as integer] : []) );
}
function gcdPowersT<T extends hasop<'sign'>>(c: T[], monic: boolean) {
	return c[1].sign() ? 1 : integer.gcd(...Object.keys(c).map(i => +i as integer).filter(i => c[i].sign() !== 0), ...(monic ? [c.length as integer] : []) );
}
function onlyPowers<T>(c: T[], gcd: number) {
	return Array.from({length: c.length / gcd}, (_, i) => c[i * gcd]);
}

//-----------------------------------------------------------------------------
//	lagrange root bounds
//-----------------------------------------------------------------------------

function sumTop2(a: number[]) {
	if (a.length < 2)
		return a.length === 1 ? a[0] : 0;
	let max1 = -Infinity, max2 = -Infinity;
	for (const v of a) {
		if (v > max1) {
			max2 = max1;
			max1 = v;
		} else if (v > max2) {
			max2 = v;
		}
	}
	return max1 + max2;
}

function sumTop2T<T extends hasop<'add'|'lt', T>>(a: T[]) : T | undefined {
	if (a.length < 2)
		return a[0];
	let max1 = a[0], max2 = a[0];
	for (const v of a) {
		if (max1.lt(v)) {
			max2 = max1;
			max1 = v;
		} else if (max2.lt(v)) {
			max2 = v;
		}
	}
	return max1.add(max2);
}

function lagrangeImproved(c: number[] | complex[]): number {
	const N = c.length;
	if (arrayOf(c, real.is))
		return sumTop2(c.map((c, i) => Math.pow(Math.abs(c), 1 / (N - i))));
	return sumTop2(c.map((c, i) => Math.pow(c.mag(), 1 / (N - i))));
}

function lagrangeImprovedT<T extends complexT<any> | hasop<'mag'|'rpow', T>>(c: T[]) {
	const N = c.length;
	return sumTop2T(c.map((c, i) => c.mag().rpow(1, (N - i))));
}

function realBound(c: number[]): real.extent {
	const N = c.length;
	const terms = c.map((c, i) => Math.pow(Math.abs(c), 1 / (N - i)));

	return new real.extent(
		-sumTop2(terms.filter((x, i) => (i % 2 == 0) === (c[i] > 0))),
		sumTop2(terms.filter((x, i) => c[i] < 0))
	);
}

function realBoundT<T extends scalar<T> & hasop<'rpow'>>(k: PolynomialN<T>) : gen.extent<T>;
function realBoundT<T extends scalar<T>>(k: PolynomialN<T>) : real.extent;
function realBoundT<T extends scalar<T>>(k: PolynomialN<T>) {
	const N = k.c.length;
	if (k.is(hasop('rpow')))
		return hasRpow(k);

	const terms = k.c.map((c, i) => Math.pow(Math.abs(Number(c)), 1 / (N - i)));
	return new real.extent(
		-sumTop2(terms.filter((x, i) => (i % 2 == 0) === (k.c[i].sign() > 0))),
		sumTop2(terms.filter((x, i) => k.c[i].sign() < 0))
	);

	function hasRpow(k: PolynomialN<T & hasop<'rpow'>>) {
		const terms = k.c.map((c, i) => (c.abs() as any).rpow(1, (N - i)));
		return new gen.extent(
			sumTop2T(terms.filter((x, i) => (i % 2 == 0) === (k.c[i].sign() > 0))).neg(),
			sumTop2T(terms.filter((x, i) => k.c[i].sign() < 0))
		);

	}
}

//-----------------------------------------------------------------------------
//	root refinement
//-----------------------------------------------------------------------------

function bisectRoot(p: Polynomial<number> | PolynomialN<number>, min: number, max: number, threshold: number, maxIter = 10) {
	let fmin = p.evaluate(min);

	for (let i = 0; i < maxIter && max - min > threshold; i++) {
		const mid	= 0.5 * (min + max);
		const fmid	= p.evaluate(mid);
		// choose side where sign changes
		if (fmin * fmid <= 0) {
			max		= mid;
		} else {
			min		= mid;
			fmin	= fmid;
		}
	}
	return 0.5 * (min + max);
}
function bisectRootT<T extends scalar<T>>(p: Polynomial<T> | PolynomialN<T>, min: T, max: T, threshold: T, maxIter = 10) {
	let fmin = p.evaluate(min);

	for (let i = 0; i < maxIter && threshold.lt(max.sub(min)); i++) {
		const mid	= min.add(max).scale(0.5);
		const fmid	= p.evaluate(mid);
		// choose side where sign changes
		if (fmin.sign() != fmid.sign()) {
			max		= mid;
		} else {
			min		= mid;
			fmin	= fmid;
		}
	}
	return min.add(max).scale(0.5);
}

function halley(p: Polynomial<number> | PolynomialN<number>, d1: Polynomial<number>, d2: Polynomial<number>, x: number, maxIter = 10, epsilon = 1e-14) {
	for (let i = 0; i < maxIter; i++) {
		const f		= p.evaluate(x);
		const f1	= d1.evaluate(x);
		const f2	= d2.evaluate(x);
		const denom = 2 * f1 * f1 - f * f2;
		if (denom === 0)
			break;
		const dx = (2 * f * f1) / denom;
		x -= dx;
		if (Math.abs(dx) < epsilon * Math.max(1, Math.abs(x)))
			break;
	}
	return x;
}
function halleyScalarT<T extends scalar<T>>(p: Polynomial<T> | PolynomialN<T>, d1: Polynomial<T>, d2: Polynomial<T>, x: T, maxIter = 10, threshold?: T) {
	for (let i = 0; i < maxIter; i++) {
		const f		= p.evaluate(x);
		const f1	= d1.evaluate(x);
		const f2	= d2.evaluate(x);
		const denom = f1.mul(f1).scale(2).sub(f.mul(f2));
		if (denom.sign() === 0)
			break;
		const dx = f.mul(f1).scale(2).div(denom);
		x = x.sub(dx);

		if (threshold && dx.abs().lt(threshold))
			break;
	}
	return x;
}

function halleyT<T extends coeffOps<T>>(p: Polynomial<T> | PolynomialN<T>, d1:Polynomial<T>, d2:Polynomial<T>, x: T, maxIter = 10, threshold?: T) {
	if (p.degree() < 2)
		return x;

	if (isScalar(x))
		return halleyScalarT(p as any, d1 as any, d2 as any, x, maxIter, threshold as any) as unknown as T;

	for (let i = 0; i < maxIter; i++) {
		const f		= p.evaluate(x);
		const f1	= d1.evaluate(x);
		const f2	= d2.evaluate(x);
		const denom = f1.mul(f1).scale(2).sub(f.mul(f2));
		const dx	= f.mul(f1).scale(2).div(denom);
		x = x.sub(dx);
	}
	return x;
}

export function refine_roots<T extends PolyTypes>(P: Polynomial<T> | PolynomialN<T>, x: realRoot<T>[], count = 1): realRoot<T>[] {
	const	d1	= P.deriv();
	const	d2	= d1.deriv();
	if (arrayOf(P.c, real.is))
		return x.map(x => halley(P as any, d1 as any, d2 as any, x as number, count)) as any;
	return x.map(x => halleyT(P as Polynomial<any>, d1, d2, x, count)) as any;
}

//-----------------------------------------------------------------------------
//	real roots
//-----------------------------------------------------------------------------

function normPolyRealRootsN(k: number[], epsilon: number): number[] {
	let zeros = 0;
	for (const i in k) {
		if (k[i] !== 0) {
			zeros = +i;
			break;
		}
	}

	return zeros > 0
		? insertSorted(checkEven(k.slice(zeros)), 0)
		: checkEven(k);

	function checkEven(k: number[]) {
		const gcd = gcdPowers(k, true);
		if (gcd > 1) {
			const r = normal(onlyPowers(k, gcd));
			return (gcd % 2) === 0
				? [...r.map(r => -r).reverse(), ...r]
				: r;
		}

		return normal(k);
	}

	function normal(k: number[]) {
		switch (k.length) {
			case 1:
				return [-k[0]];

			case 2: {
				const e = k[1] * 0.5;
				const d = e * e - k[0];
				if (d > 0) {
					const r = Math.sqrt(d);
					return [-r - e, r - e];
				} else if (d == 0) {
					return [-e];
				} else {
					return [];//[-e, Math.sqrt(-d)];
				}
			}
			case 3: {
				const	e	= k[2] / 3;
				const	f	= e * e - k[1] / 3;
				const	g	= (e * k[1] - k[0]) / 2 - e * e * e;
				const	h	= g * g - f * f * f;
			
				if (h < 0) {
					//3 real roots
					const	angle	= Math.atan2(real.copySign(Math.sqrt(-h), g), g) / 3;
					const	c		= Math.cos(angle), s = Math.sin(angle);
					const	rf		= Math.sqrt(f);
					return [
						-2 * rf * c - e,
						 rf * (c + sqrt3 * s) - e,
						 rf * (c - sqrt3 * s) - e,
					].sort((a, b) => a - b);
			
				} else if (h > 0) {
					//1 real root, 2 imaginary (y + iz) & (y - iz)
					const	rh = Math.sqrt(h);
					const	x = Math.pow(g + rh, 1 / 3);
					const	y = Math.pow(g - rh, 1 / 3);
					return [x + y - e, /*-0.5 * (x + y) - e,	sqrt3 / 2 * (x - y)*/];
				} else {
					//3 real and equal
					return [Math.pow(-k[0], 1 / 3)];
				}
			}
			case 4: {
				//  substitute x = y - A/4 to eliminate cubic term:
				//	x^4 + px^2 + qx + r = 0
				const a2	= k[3] * k[3];
				const p		= k[2] - 3 * a2 / 8;
				const q		= a2 * k[3] / 8 - k[3] * k[2] / 2 + k[1];
				const t		= -3 * a2 * a2 / 256 + a2 * k[2] / 16 - k[3] * k[1] / 4 + k[0];

				let roots3;
				if (Math.abs(t) < epsilon) {
					// no absolute term: y(y^3 + py + q) = 0
					roots3 = [0, ...normPolyRealRootsN([q, p, 0], epsilon)];
				} else {
					// solve the resolvent cubic ...
					const r 	= normPolyRealRootsN([-q * q / 8, p * p / 4 - t, p], epsilon);
					const m 	= Math.max(...r);
					const v 	= Math.sqrt(m * 2);
					const u 	= q / Math.sqrt(m * 8);
					roots3 = [...normPolyRealRootsN([m + p / 2 - u, v], epsilon), ...normPolyRealRootsN([m + p / 2 + u, -v], epsilon)];
				}
				return roots3.map(r => r - k[3] / 4);
			}

			default: {
				const pN = new polynomialN(k);
				const d1 = pN.deriv();
				const d2 = d1.deriv();
				return sturmIsolateIntervalsN(pN).map(i => halley(pN, d1, d2, bisectRoot(pN, i.min, i.max, epsilon)));
			}
		}
	}
}


function normPolyRealRootsT<T extends scalarExt<T>>(k: T[], epsilon: T): T[] {
	const zero = k[0].scale(0);

	let zeros = 0;
	for (const i in k) {
		if (k[i].sign() !== 0) {
			zeros = +i;
			break;
		}
	}

	return zeros > 0
		? insertSorted(checkEven(k.slice(zeros)), zero, (a: T, b: T) => a.lt(b))
		: checkEven(k);

	function checkEven(k: T[]): T[] {
		const gcd = gcdPowersT(k, true);
		if (gcd > 1) {
			const r = normal(onlyPowers(k, gcd));
			return (gcd % 2) === 0
				? [...r.map(r => r.neg()).reverse(), ...r]
				: r;
		}
		return normal(k);
	}

	function normal(k: T[]): T[] {
		switch (k.length) {
			case 1:
				return [k[0].neg()];

			case 2: {
				const e = k[1].scale(0.5);
				const d = e.mul(e).sub(k[0]);
				switch (d.sign()) {
					default:
					case 1: {
						const r = d.sqrt();
						return [r.neg().sub(e), r.sub(e)];
					}
					case 0:
						return [e.neg()];
					case -1:
						return [];//[-e, Math.sqrt(-d)];
				}
			}
			case 3: {
				const	e	= k[2].scale(1 / 3);
				const	f	= e.mul(e).sub(k[1].scale(1 / 3));
				const	g	= e.mul(k[1]).sub(k[0]).scale(0.5).sub(e.mul(e).mul(e));
				const	h	= g.mul(g).sub(f.mul(f).mul(f));
			
				switch (h.sign()) {
					case -1: {
						//3 real roots - use complex cube roots
						const	rh = h.abs().sqrt();
						const	x = complexT(g, rh).rpow(1, 3);
						const	y = complexT(g, rh.neg()).rpow(1, 3);
						const	half	= k[0].from(1).div(k[0].from(2));
						const	sqrt3	= k[0].from(3).rpow(1, 2);
						return [
							x.r.add(y.r).sub(e),
							x.r.add(y.r).neg().sub(x.i.sub(y.i).mul(sqrt3)).mul(half).sub(e),
							x.r.add(y.r).neg().add(x.i.sub(y.i).mul(sqrt3)).mul(half).sub(e),
						];
					}
					default:
					case 1: {
						//1 real root, 2 imaginary (y + iz) & (y - iz)
						const	rh = h.sqrt();
						const	x = g.add(rh).rpow(1, 3);
						const	y = g.sub(rh).rpow(1, 3);
						return [x.add(y).sub(e)];
					}
					case 0: {
						//3 real and equal
						return [k[0].neg().rpow(1, 3)];
					}
				}
			}
			case 4: {
				//  substitute x = y - A/4 to eliminate cubic term:
				//	x^4 + px^2 + qx + r = 0
				const a2	= k[3].mul(k[3]);
				const p		= k[2].sub(a2.scale(3 / 8));
				const q		= a2.mul(k[3]).scale(1 / 8).sub(k[3].mul(k[2]).scale(1 / 2)).add(k[1]);
				const t		= a2.mul(a2).scale(-3 / 256).add(a2.mul(k[2]).scale(1 / 16)).sub(k[3].mul(k[1]).scale(1 / 4)).add(k[0]);

				//const px8	= k[2].scale(8).sub(a2.scale(3));
				//const qx8	= a2.mul(k[3]).sub(k[3].mul(k[2]).scale(4)).add(k[1].scale(8));
				//const tx256	= a2.mul(a2).scale(-3).add(a2.mul(k[2]).scale(16)).sub(k[3].mul(k[1]).scale(64)).add(k[0].scale(256));

				let roots3;
				if (hasop('lt')(t) && t.abs().lt(epsilon)) {
					// no absolute term: y(y^3 + py + q) = 0
					roots3 = [zero, ...normPolyRealRootsT([q, p, zero], epsilon)];
				} else {
					// solve the resolvent cubic ...
					const r 	= normPolyRealRootsT([q.mul(q).scale(-1 / 8), p.mul(p).scale(1 / 4).sub(t), p], epsilon);
					const m 	= gen.max(...r);
					const v 	= m.scale(2).sqrt();
					const u 	= q.div(m.scale(8).sqrt());
					roots3 = [...normPolyRealRootsT([m.add(p.scale(0.5)).sub(u), v], epsilon), ...normPolyRealRootsT([m.add(p.scale(0.5)).add(u), v.neg()], epsilon)];
				}
				return roots3.map(r => r.sub(k[3].scale(1 / 4)));
			}

			default: {
				const pN = PolynomialN(k);
				const d1 = pN.deriv();
				const d2 = d1.deriv();
				return sturmIsolateIntervalsT(pN, epsilon).map(i => halleyT(pN, d1, d2, bisectRootT(pN, i.min, i.max, epsilon)));
			}
		}
	}
}

function sturmIsolateIntervalsN(pN: polynomialN) {
	const n = pN.degree();
	if (n === 0)
		return [] as real.extent[];

	const seq: polynomial[] = [];
	let p0 = new polynomial([...pN.c, 1]);
	let p1 = pN.deriv();
	seq.push(p0.dup(), p1.dup());

	while (p1.degree() >= 0) {
		p0.divmod(p1);
		if (p0.degree() < 0)
			break;

		// push -r
		for (let i = 0; i < p0.c.length; i++)
			p0.c[i] = -p0.c[i];

		seq.push(p0.dup());
		[p0, p1] = [p1, p0];
	}

	function signChangesAt(x: number) {
		let prev = 0;
		let changes = 0;
		for (const p of seq) {
			let s = Math.sign(p.evaluate(x));
			if (s === 0) {
				const eps = Math.max(1e-12, Math.abs(x) * 1e-12);
				s = Math.sign(p.evaluate(x + eps));
				if (s === 0)
					continue;
			}
			if (prev !== 0 && s !== prev)
				changes++;
			prev = s;
		}
		return changes;
	}

	function countRootsInInterval(a: number, b: number) {
		return Math.abs(signChangesAt(a) - signChangesAt(b));
	}
	
	const bounds = realBound(pN.c);
	let lower = bounds.min;
	let upper = bounds.max;
	let count = countRootsInInterval(lower, upper);

	const stack: [number, number, number][] = [];
	const intervals: real.extent[] = [];

	for (;;) {
		while (count > 1) {
			const mid = 0.5 * (lower + upper);
			const lowerCount = countRootsInInterval(lower, mid);
			if (lowerCount < count)
				stack.push([mid, upper, count - lowerCount]);
			upper = mid;
			count = lowerCount;
		}

		if (count === 1)
			intervals.push(new real.extent(lower, upper));

		if (stack.length == 0)
			break;
		[lower, upper, count] = stack.pop()!;
	}

	return intervals;
}

function sturmIsolateIntervalsT<T extends scalar<T>>(pN: PolynomialN<T>, epsilon: T): gen.extent<T>[] {
	const n = pN.degree();
	if (n === 0)
		return [];

	if (n === 1) {
		const root = pN.c[0].neg();
		return [new gen.extent(root, root)];
	}

	// Build Sturm sequence
	const seq: Polynomial<T>[] = [];
	let p0 = Polynomial<T>([...pN.c, pN.c[0].from(1)]);
	let p1 = pN.deriv();
	seq.push(p0.dup(), p1.dup());

	while (p1.degree() >= 0) {
		p0.divmod(p1);
		if (p0.degree() < 0)
			break;

		// push -r
		for (let i = 0; i < p0.c.length; i++)
			p0.c[i] = p0.c[i].neg();

		seq.push(p0.dup());
		[p0, p1] = [p1, p0];
	}

	function SignChanges(x: T) {
		let prev = 0;
		let changes = 0;
		for (const p of seq) {
			let s = p.evaluate(x).sign();
			if (s === 0) {
				// if polynomial evaluates to zero, use sign of nearby point
				const eps = gen.max(x.from(1e-12), x.abs().scale(1e-12));//Math.max(1e-12, Math.abs(x) * 1e-12);
				s = p.evaluate(x.add(eps)).sign();
				if (s === 0)
					continue;
			}
			if (prev !== 0 && s !== prev)
				changes++;
			prev = s;
		}
		return changes;
	}

	function countRootsInInterval(a: T, b: T) {
		const va = SignChanges(a);
		const vb = SignChanges(b);
		return Math.abs(va - vb);
	}

	const bounds = realBoundT(pN);
	let lower = asScalarT(epsilon, bounds.min).sub(epsilon);
	let upper = asScalarT(epsilon, bounds.max).add(epsilon);
	let count = countRootsInInterval(lower, upper);

	const stack: [T, T, number][] = [];
	const intervals: gen.extent<T>[] = [];

	for (;;) {
		while (count > 1) {
			// split interval
			const mid = lower.add(upper).scale(0.5);
			const lowerCount = countRootsInInterval(lower, mid);
			if (lowerCount < count)
				stack.push([mid, upper, count - lowerCount]);
			upper = mid;
			count = lowerCount;
		}

		if (count === 1)
			intervals.push(new gen.extent(lower, upper));
				
		if (stack.length == 0)
			break;
		[lower, upper, count] = stack.pop()!;
	}

	return intervals;
}

//-----------------------------------------------------------------------------
//	complex roots
//-----------------------------------------------------------------------------

function normPolyComplexRootsN(k: number[], epsilon: number): complex[] {
	let zeros = 0;
	for (const i in k) {
		if (k[i] !== 0) {
			zeros = +i;
			break;
		}
	}

	return zeros > 0
		? [...checkEven(k.slice(zeros)), complex.zero()]
		: checkEven(k);

	function checkEven(k: number[]): complex[] {
		if (k[1] === 0) {
			const gcd = gcdPowers(k, true);
			if (gcd > 1) {
				const r = normal(onlyPowers(k, gcd));
				return (gcd % 2) === 0
					? [...r.map(r => r.neg()).reverse(), ...r]
					: r;
			}
		}
		return normal(k);
	}

	function normal(k: number[]): complex[] {
		switch (k.length) {
			case 1:
				return [complex(-k[0])];

			case 2: {
				const e = k[1] * 0.5;
				const d = e * e - k[0];
				if (d > 0) {
					const r = Math.sqrt(d);
					return [complex(-r - e), complex(r - e)];
				}
				return d == 0 ? [complex(-e)] : complex.conjugatePair(complex(-e, Math.sqrt(-d)));
			}
			case 3: {
				const	e	= k[2] / 3;
				const	f	= e * e - k[1] / 3;
				const	g	= (e * k[1] - k[0]) / 2 - e * e * e;
				const	h	= g * g - f * f * f;
			
				if (h < 0) {
					//3 real roots
					const	angle	= Math.atan2(real.copySign(Math.sqrt(-h), g), g) / 3;
					const	c		= Math.cos(angle), s = Math.sin(angle);
					const	rf		= Math.sqrt(f);
					return	[
						complex(2 * c * rf - e),
						complex((-c - sqrt3 * s) * rf - e),
						complex((-c + sqrt3 * s) * rf - e),
					];
			
				} else if (h > 0) {
					//1 real root, 2 imaginary (y + iz) & (y - iz)
					const	rh = Math.sqrt(h);
					const	x = Math.pow(g + rh, 1 / 3);
					const	y = Math.pow(g - rh, 1 / 3);
					return [complex(x + y - e), ...complex.conjugatePair(complex(-0.5 * (x + y) - e, sqrt3 / 2 * (x - y)))];
				} else {
					//3 real and equal
					return [complex(Math.pow(-k[0], 1 / 3))];
				}
			}
			case 4: {
				//  substitute x = y - A/4 to eliminate cubic term:
				//	x^4 + px^2 + qx + r = 0
				const a2	= k[3] * k[3];
				const p		= k[2] - 3 * a2 / 8;
				const q		= a2 * k[3] / 8 - k[3] * k[2] / 2 + k[1];
				const t		= -3 * a2 * a2 / 256 + a2 * k[2] / 16 - k[3] * k[1] / 4 + k[0];

				let roots3: complex[];
				if (Math.abs(t) < epsilon) {
					// no absolute term: y(y^3 + py + q) = 0
					roots3	= [complex.zero(), ...normPolyComplexRootsN([q, p, 0], epsilon)];
				} else {
					// solve the resolvent cubic ...
					const r	= normPolyRealRootsN([-q * q / 8, p * p / 4 - t, p], epsilon);
					const m = Math.max(...r);
					const v = Math.sqrt(m * 2);
					const u = q / Math.sqrt(m * 8);
					roots3 = [...normPolyComplexRootsN([m + p / 2 - u, v], epsilon), ...normPolyComplexRootsN([m + p / 2 + u, -v], epsilon)];
				}
				return roots3.map(r => complex(r.r - k[3] / 4, r.i));
			}
			
			default:
				return aberthN(new polynomialN(k));
		}
	}	
}


function normPolyComplexRootsT<T extends scalarExt<T> & hasop<'mag'>>(k: T[], epsilon: T): complexT<T>[] {
	const zero = k[0].scale(0);

	let zeros = 0;
	for (const i in k) {
		if (k[i].sign() !== 0) {
			zeros = +i;
			break;
		}
	}

	return zeros > 0
		? [...checkEven(k.slice(zeros)), complexT(zero, zero)]
		: checkEven(k);

	function checkEven(k: T[]): complexT<T>[] {
		if (k[1]?.sign() === 0) {
			const gcd = gcdPowersT(k, true);
			if (gcd > 1) {
				const r = normal(onlyPowers(k, gcd));
				return (gcd % 2) === 0
					? [...r.map(r => r.neg()).reverse(), ...r]
					: r;
			}
		}
		return normal(k);
	}

	function normal(k: T[]): complexT<T>[] {
		switch (k.length) {
			case 1:
				return [complexT(k[0].neg(), zero)];

			case 2: {
				const e = k[1].scale(0.5);
				const d = e.mul(e).sub(k[0]);
				if (!(d.sign() <= 0)) {
					const r = d.sqrt();
					return [complexT(r.neg().sub(e), zero), complexT(r.sub(e), zero)];
				} else if (d.sign() === 0) {
					return [complexT(e.neg(), zero)];
				} else {
					return complexT.conjugatePair(complexT(e.neg(), d.neg().sqrt()));
				}
			}
			case 3: {
				const	e	= k[2].scale(1 / 3);
				const	f	= e.mul(e).sub(k[1].scale(1 / 3));
				const	g	= e.mul(k[1]).sub(k[0]).scale(0.5).sub(e.mul(e).mul(e));
				const	h	= g.mul(g).sub(f.mul(f).mul(f));
			
				switch (h.sign()) {
					case -1: {
						//3 real roots
						const	s0		= gen.copySign(h.neg().sqrt(), g);
						const	angle	= hasStatic(s0, 'atan2')?.(s0, g).div(3) ?? Math.atan2(Number(s0), Number(g)) / 3;
						const	c		= hasStatic(s0, 'cos')?.(angle) ?? k[0].from(Math.cos(angle));
						const	s		= hasStatic(s0, 'sin')?.(angle) ?? k[0].from(Math.sin(angle));
						const	rf		= f.sqrt();
						return [
							complexT(c.scale(2).mul(rf).sub(e), zero),
							complexT((c.neg().sub(s.scale(sqrt3))).mul(rf).sub(e), zero),
							complexT((c.neg().add(s.scale(sqrt3))).mul(rf).sub(e), zero),
						];
					}
					default:
					case 1: {
						//1 real root, 2 imaginary (y + iz) & (y - iz)
						const	rh = h.sqrt();
						const	x = g.add(rh).rpow(1, 3);
						const	y = g.sub(rh).rpow(1, 3);
						return [complexT(x.add(y).sub(e), zero), ...complexT.conjugatePair(complexT(x.add(y).scale(-0.5).sub(e), y.sub(x).scale(sqrt3 / 2)))];
					}
					case 0: {
						//3 real and equal
						return [complexT(k[0].neg().rpow(1, 3), zero)];
					}
				}
			}
			case 4: {
				//  substitute x = y - A/4 to eliminate cubic term:
				//	x^4 + px^2 + qx + r = 0
				const a2	= k[3].mul(k[3]);
				const p		= k[2].sub(a2.scale(3 / 8));
				const q		= a2.mul(k[3]).scale(1 / 8).sub(k[3].mul(k[2]).scale(1 / 2)).add(k[1]);
				const t		= a2.mul(a2).scale(-3 / 256).add(a2.mul(k[2]).scale(1 / 16)).sub(k[3].mul(k[1]).scale(1 / 4)).add(k[0]);

				let roots3: complexT<T>[];
				if (hasop('lt')(t) && t.abs().lt(epsilon)) {
					// no absolute term: y(y^3 + py + q) = 0
					roots3	= [complexT(zero, zero), ...normPolyComplexRootsT([q, p, zero], epsilon)];
				} else {
					// solve the resolvent cubic ...
					const r = normPolyRealRootsT([q.neg().mul(q).scale(1 / 8), p.mul(p).scale(1 / 4).sub(t), p], epsilon);
					const m = gen.max(...r);
					const v = m.scale(2).sqrt();
					const u = q.div(m.scale(8).sqrt());
					roots3 = [...normPolyComplexRootsT([m.add(p.scale(0.5)).sub(u), v], epsilon), ...normPolyComplexRootsT([m.add(p.scale(0.5)).add(u), v.neg()], epsilon)];
				}
				return roots3.map(r => complexT(r.r.sub(k[3].scale(1 / 4)), r.i));
			}

			default:
				return aberthT(new polynomialNT<T>(k));
		}
	}
}

function aberthN(poly: PolynomialN<number> | PolynomialN<complex>, tolerance = 1e-6, maxIterations = 100) {
	const n			= poly.degree();
	const dpoly		= poly.deriv();
	const radius	= lagrangeImproved(poly.c);
	const roots		= Array.from({length: n}, (_, i) => complex.fromPolar(radius, 2 * Math.PI * i / n));

	for (let iter = 0; iter < maxIterations; iter++) {
		let maxCorrection = 0;
		for (let i = 0; i < roots.length; i++) {
			const root		= roots[i];
			const p_root	= poly.evaluate(root as any) as complex;
			const dp_root	= dpoly.evaluate(root as any) as complex;
			
			let sum		= complex.zero();
			for (let j = 0; j < roots.length; j++) {
				if (i !== j)
					sum = sum.add(root.sub(roots[j]).recip());
			}
			const correction	= p_root.div((dp_root.sub(p_root.mul(sum))));
			roots[i]			= roots[i].sub(correction);
			maxCorrection		= Math.max(maxCorrection, Math.abs(correction.r) + Math.abs(correction.i));
		}
		if (maxCorrection < tolerance)
			break;
	}
	return roots;
}

function aberthT<S extends complexOps<S> & hasop<'lt'|'from'|'mag'>, T extends S & coeffOps<T> & (complexT<S> | hasop<'rpow'>)>(poly: polynomialNT<T>, tolerance?: S, maxIterations = 100) {

	let from: (v: number) => S;
	let evaluate: (p: any, t: complexT<S>) => complexT<S>;

	if (poly.is(isInstanceMaker(complexT))) {
		from		= (poly.c[0].r as S).from;
		evaluate	= (p, t) => p.evaluate(t);
	} else {
		from		= poly.c[0].from;
		evaluate	= (p, t) => p.evaluate(t);
	}

	const n			= poly.degree();
	const zero		= from(0);
	const czero		= complexT<S>(zero, zero);
	const dpoly 	= poly.deriv();

	const radius	= lagrangeImprovedT(poly.c);
	const roots		= Array.from({length: n}, (_, i) => complexT.fromPolar(radius, 2 * Math.PI * i / n));

	tolerance ??= from(1e-6);

	for (let iter = 0; iter < maxIterations; iter++) {
		let maxCorrection = zero;
		for (let i = 0; i < n; i++) {
			const root		= roots[i];
			const p_root	= evaluate(poly, root);
			const dp_root	= evaluate(dpoly, root);

			let sum		= czero;
			for (let j = 0; j < roots.length; j++) {
				if (i !== j)
					sum = sum.add(root.sub(roots[j]).recip());
			}
			const correction	= p_root.div((dp_root.sub(p_root.mul(sum))));
			roots[i]			= roots[i].sub(correction);
			maxCorrection		= gen.max(maxCorrection, correction.abs());
		}
		if (maxCorrection.lt(tolerance))
			break;
	}
	return roots;
}

//-----------------------------------------------------------------------------
// Integer/Rational Root Theorem extractor
//-----------------------------------------------------------------------------

function* rationalDivisorsI(numFactors: number[], denFactors: number[], limit?: rational): Generator<rational> {
	const numEntries = Array.from(Object.entries(numFactors).map(([i, v]) => [+i, v]));
	const denEntries = Array.from(Object.entries(denFactors).map(([i, v]) => [+i, v]));
	const primes = new Set<number>();

	function* backtrackNum(i: number, num: number, den: number): Generator<rational> {
		if (limit && num * limit.den > den * limit.num)
			return;
		if (i === numEntries.length) {
			yield rational(num, den);
			return;
		}
		const [p, exp] = numEntries[i++];
		if (primes.has(p)) {
			yield* backtrackNum(i, num, den);
		} else {
			for (let e = 0; e <= exp; e++, num *= p)
				yield* backtrackNum(i, num, den);
		}
	}

	function* backtrackDen(i: number, den: number): Generator<rational> {
		if (i === denEntries.length) {
			if (numEntries.length === 0)
				yield rational(1, den);
			else
				yield *backtrackNum(0, 1, den);
			return;
		}
		const [p, exp] = denEntries[i++];
		yield* backtrackDen(i, den);
		primes.add(p);
		for (let e = 1; e <= exp; e++)
			yield* backtrackDen(i, den *= p);
	}

	yield *backtrackDen(0, 1);
}
function* rationalDivisorsB(numFactors: Map<bigint, number>, denFactors: Map<bigint, number>, limit?: rationalB): Generator<rationalB> {
	const numEntries = Array.from(numFactors.entries());
	const denEntries = Array.from(denFactors.entries());
	const primes = new Set<bigint>();

	function* backtrackNum(i: number, num: bigint, den: bigint): Generator<rationalB> {
		if (limit && num * limit.den > den * limit.num)
			return;
		if (i === numEntries.length) {
			yield rationalB(num, den);
			return;
		}
		const [p, exp] = numEntries[i++];
		if (primes.has(p)) {
			yield* backtrackNum(i, num, den);
		} else {
			for (let e = 0; e <= exp; e++, num *= p)
				yield* backtrackNum(i, num, den);
		}
	}

	function* backtrackDen(i: number, den: bigint): Generator<rationalB> {
		if (i === denEntries.length) {
			if (numEntries.length === 0)
				yield rationalB(1n, den);
			else
				yield *backtrackNum(0, 1n, den);
			return;
		}
		const [p, exp] = denEntries[i++];
		yield* backtrackDen(i, den);
		primes.add(p);
		for (let e = 1; e <= exp; e++)
			yield* backtrackDen(i, den *= p);
	}

	yield *backtrackDen(0, 1n);
}

function rationalRoots<T extends PolyTypes>(p: Polynomial<T> | PolynomialN<T>) {
	return squareFreeFactorization(p).map(({factor,  multiplicity}) =>
		Array(multiplicity).fill(squareFreeRationalRoots(integerPolynomial(factor))).flat()
	).flat().sort((a, b) => a.compare(b));
}

export function squareFreeRationalRoots<T extends builtinNumber>(pI: Polynomial<T>): rationalRoot<T>[] {
	return pI && pI.degree() > 0
		? (pI.is(big.is) ? squareFreeRationalRootsB(pI) : squareFreeRationalRootsI(pI as Polynomial<number>)) as any
		: [];
}

function squareFreeRationalRootsI(p: Polynomial<number>): rational[] {
	const roots: rational[] = [];

	if (p.c[0] === 0) {
		roots.push(rational(0));
		p.syntheticDiv(0);
	}

	if (p.degree() > 1) {
		const pFactors	= new factorisationI(Math.abs(p.c[0]) as integer);
		const qFactors	= new factorisationI(Math.abs(p.leadCoeff()) as integer);
		const bound 	= rational.from(lagrangeImproved(p.normalise().c));

		for (const r of rationalDivisorsI(pFactors, qFactors, bound)) {
			let	i		= p.degree();
			if (i < 2)
				break;

			let acc1	= p.c[i];
			let acc2	= p.c[i];
			for (let denPow = r.den; i--; denPow *= r.den) {
				const c = p.c[i] * denPow;
				acc1 = acc1 *  r.num + c;
				acc2 = acc2 * -r.num + c;
			}

			if (acc1 === 0) {
				roots.push(r);
				p.syntheticDiv(r);
			}
			if (acc2 === 0) {
				const r2 = r.neg();
				roots.push(r2);
				p.syntheticDiv(r2);
			}
		}
	}
	if (p.degree() === 1) {
		roots.push(rational(-p.c[0], p.c[1]));
		p.c = [0];
	}
	return roots;
}

function squareFreeRationalRootsB(p: Polynomial<bigint>): rationalB[] {
	const roots: rationalB[] = [];

	if (p.c[0] === 0n) {
		roots.push(rationalB(0n));
		p.syntheticDiv(0n);
	}

	if (p.degree() > 1) {
		const pFactors	= new factorisationB(big.abs(p.c[0]));
		const qFactors	= new factorisationB(big.abs(p.leadCoeff()));
		const bound 	= realBoundT(p.normalise());
		const bound2	= rationalB.from(+gen.genmax(gen.neg(bound.min), bound.max));

		for (const r of rationalDivisorsB(pFactors, qFactors, bound2)) {
			let	i		= p.degree();
			if (i < 2)
				break;

			let acc1	= p.c[i];
			let acc2	= p.c[i];
			for (let denPow = r.den; i--; denPow *= r.den) {
				const c = p.c[i] * denPow;
				acc1 = acc1 *  r.num + c;
				acc2 = acc2 * -r.num + c;
			}

			if (acc1 === 0n) {
				roots.push(r);
				p.syntheticDiv(r);
			}
			if (acc2 === 0n) {
				const r2 = r.neg();
				roots.push(r2);
				p.syntheticDiv(r2);
			}
		}
	}
	if (p.degree() === 1) {
		roots.push(rationalB(-p.c[0], p.c[1]));
		p.c = [0n];
	}
	return roots;
}

//-----------------------------------------------------------------------------
//	Legendre polynomial and roots
//-----------------------------------------------------------------------------

export function legendrePolynomial(n: number): Polynomial<number> {
	let P0 = Polynomial([1]);
	if (n === 0)
		return P0;

	let P1 = Polynomial([0, 1]);
	for (let k = 2; k <= n; ++k) {
		P0 = P1;
		P1 = Polynomial([0, 1]).mul(P1).scale(2 * k - 1).sub(P0.scale(k - 1)).scale(1 / k);
	}

	return P1;
}

export function legendreTable(n: number): [number, number][] {
	const P		= legendrePolynomial(n);
	const dP	= P.deriv();
	// roots returns x_i in [-1, 1]
	return P.realRoots().map(x => [x, 2 / ((1 - x * x) * (dP.evaluate(x) ** 2))]);
}
