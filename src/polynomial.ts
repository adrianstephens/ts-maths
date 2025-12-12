/* eslint-disable no-restricted-syntax */
import {
	ops,		scalar,	scalarRational, scalarExt,
	isInstance,	has,	hasStatic,	arrayOf,
	isScalar,	isScalarRational, isScalarExt,	asScalarT
} from './core';

import Num, {extent, asScalarExt, isNumber} from './num';
import Big from './big';
import Gen, {extentT} from './gen';

import { toSuperscript } from './string';
import { factorisation, factorisationB } from './prime';
import complex, { complexT } from './complex';
import rational, { rationalB } from './rational';
import { LUDecomposeBareiss, LUDecomposeBareissT } from './vector2';

const sqrt3	= Math.sqrt(3);
const defaultEpsilon = 1e-9;

//-----------------------------------------------------------------------------
//	Interfaces
//-----------------------------------------------------------------------------

type normalized<T>		= T extends bigint ? rationalB : T;
type realRoots<T>		= T extends number ? number[] : T extends bigint ? rationalB[] : T extends scalar<any> ? T[] : never;
type complexRoots<T>	= T extends number ? complex[] : T extends scalarExt<infer R> ? complexT<R>[] : never;
type rationalRoots<T>	= T extends number ? rational[] : T extends bigint ? rationalB[] : T extends rational ? rational[] : T extends rationalB ? rationalB[] : T extends scalarRational<any> ? rationalB[] : never;

type PolyNTypes = number | ops<any, any>;
type PolyTypes = PolyNTypes | bigint;

// Interface for normalized polynomials with implicit leading coefficient of 1
export interface PolynomialN<C> {
	c:		C[];
	degree():						number;
	dup(): 							PolynomialN<C>;
	evaluate(t: C):					C;
	evaluate(t: C[]):				C[];
	deriv():						Polynomial<C>;
	mul(b: PolynomialN<C>):			PolynomialN<C>;
	divmod(b: PolynomialN<C>):		Polynomial<C>;
	rationalRoots():				rationalRoots<C>;
	realRoots(epsilon?: number):	realRoots<C>;
	allRoots(epsilon?: number):		complexRoots<C>;
	refine_roots(x: realRoots<C>, count?: number): realRoots<C>;
}

// Interface for regular polynomials
export interface Polynomial<C> extends PolynomialN<C> {
	leadCoeff():					C;
	dup():							Polynomial<C>;
	add(b: C | Polynomial<C>):		Polynomial<C>;
	sub(b: C | Polynomial<C>):		Polynomial<C>;
	scale(b: C):					Polynomial<C>;
	rscale(b: C):					Polynomial<C>;
	mul(b: Polynomial<C>):			Polynomial<C>;
	divmod(b: Polynomial<C>):		Polynomial<C>;
	div(b: Polynomial<C>):			Polynomial<C>;
	selfAdd(b: C | Polynomial<C>):	void;
	selfSub(b: C | Polynomial<C>):	void;
	selfScale(b: C):				void;
	selfRscale(b: C):				void;
	pseudoRemainder(b: Polynomial<C>): void;
	content():						C;
	abs():							Polynomial<C>;
	sign():							number;
	normalise(epsilon?: number):	PolynomialN<normalized<C>>;
	map<U extends PolyTypes>(func: (c: C, i: number) => U): Polynomial<U>;
}

// Overloads to ensure numeric literal arrays like `[-1, 1]` resolve to `Polynomial<number>`
export function PolynomialN(c: readonly number[]): PolynomialN<number>;
export function PolynomialN<T extends ops<T>>(c: T[]): PolynomialN<T>;
export function PolynomialN(c: readonly any[]) {
	switch (typeof c[0]) {
		case 'number':
			return new polynomialN((c as number[])) as PolynomialN<number>;
		default:
			return new polynomialNT(c as any) as PolynomialN<ops<any, any>>;
	}
}

// Overloads to ensure numeric literal arrays like `[-1, 1]` resolve to `Polynomial<number>`
export function Polynomial(c: readonly number[]): Polynomial<number>;
export function Polynomial(c: readonly bigint[]): Polynomial<bigint>;
export function Polynomial<T extends ops<T>>(c: T[]): Polynomial<T>;
export function Polynomial<T extends PolyTypes>(c: T[]): Polynomial<T>;
export function Polynomial(c: readonly any[]) {
	switch (typeof c[0]) {
		case 'number':
			return new polynomial((c as number[])) as Polynomial<number>;
		case 'bigint':
			return new polynomialB(c as bigint[]) as Polynomial<bigint>;
		default:
			return new polynomialT(c as any) as Polynomial<ops<any, any>>;
	}
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

function polyNOf<U>(poly: PolynomialN<any>, g: (x: any) => x is U): poly is PolynomialN<U> {
	return poly.c.length ? g(poly.c[0]) : false;
}

function polyOf<U>(poly: Polynomial<any>, g: (x: any) => x is U): poly is Polynomial<U> {
	return poly.c.length ? g(poly.c[0]) : false;
}

function toString(t: any, debug = false): string {
	const s = debug && typeof t === 'object' && t !== null && Symbol.for("debug.description") in t ? t[Symbol.for("debug.description")]() : String(t);
	return s.includes(' ') ? `(${s})` : s;
}

function coefficientString<T>(coef: T, i: number, debug: boolean): string {
	return (i ? `${coef === 1 ? '' : toString(coef, debug)}x${i > 1 ? toSuperscript(i.toString()) : ''}` : `${toString(coef, debug)}`);
}

function polynomialString<T extends ops<any>>(coefficients: T[], debug: boolean): string {
	return  coefficients.map((coef: T, i) => coefficientString(coef, i, debug)).reverse().join(' + ');
}

function scalarPolynomialString<T extends scalar<any>>(coefficients: T[], debug: boolean): string {
	return coefficients.map((coef, i) =>
		(coef.sign() < 0 ? ' - ' : ' + ') + coefficientString(coef.abs(), i, debug)
	).reverse().join('');
}

function lessThan<T extends scalar<T>>(a: number | bigint | T, b: number): boolean {
	return	typeof a === 'number' ? a < b
		:	typeof a === 'bigint' ? a < BigInt(b)
		:	a.lt(a.from(b));
}

//-----------------------------------------------------------------------------
//	sparse or dense arrays
//-----------------------------------------------------------------------------

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
function arrayScaleT<T extends ops<T>>(a: T[], b: T|number): T[] {
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
function arrayRscaleT<T extends ops<T>>(a: T[], b: T|number): T[] {
	if (typeof b === 'number')
		return arrayScaleT(a, 1 / b);
	for (const i in a)
		a[i] = a[i].div(b);
	return a;
}

//-----------------------------------------------------------------------------
//	sparse arrays
//-----------------------------------------------------------------------------

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
function sparseTrimT<T extends ops<T>>(a: T[]) {
	const keys = Object.keys(a);
	let k = keys.length;
	if (arrayOf(a, has('sign'))) {
		while (k-- && a[+keys[k]].sign() === 0)
			;
	} else {
		while (k-- && lessThan(a[+keys[k]].mag(), defaultEpsilon))
			;
	}
	a.length = k < 0 ? 0 : +keys[k] + 1;
}

// add

function sparseAdd(a: number[], b: number[]): number[] {
	for (const i in b)
		a[i] = (a[i] || 0) + b[i];
	return a;
}
function sparseAddB(a: bigint[], b: bigint[]): bigint[] {
	for (const i in b)
		a[i] = (a[i] || 0n) + b[i];
	return a;
}
function sparseAddT<T extends ops<T>>(a: T[], b: T[]): T[] {
	for (const i in b)
		a[i] = a[i] ? a[i].add(b[i]) : b[i];
	return a;
}

// sub

function sparseSub(a: number[], b: number[]): number[] {
	for (const i in b)
		a[i] = (a[i] || 0) - b[i];
	return a;
}
function sparseSubB(a: bigint[], b: bigint[]): bigint[] {
	for (const i in b)
		a[i] = (a[i] || 0n) - b[i];
	return a;
}

function sparseSubT<T extends ops<T>>(a: T[], b: T[]): T[] {
	for (const i in b)
		a[i] = a[i] ? a[i].sub(b[i]) : b[i];
	return a;
}

// evaluate

function sparseEvaluate(c: number[], x: number, monomial: boolean): number {
    let result	= 0;
    let xpow	= 1;
    let exp 	= 0;
    
    for (const i in c) {
		const exp1 = +i;
        while (exp < exp1) {
            xpow *= x;
			++exp;
		}
        result += c[i] * xpow;
    }
	if (monomial) {
		const exp1 = c.length - 1;
        while (exp < exp1) {
            xpow *= x;
			++exp;
		}
        result += xpow;
	}
    return result;
}
function sparseEvaluateB(c: bigint[], x: bigint): bigint {
    let result	= 0n;
    let xpow	= 1n;
    let exp 	= 0;
    
    for (const i in c) {
		const exp1 = +i;
        while (exp < exp1) {
            xpow *= x;
			++exp;
		}
        result += c[i] * xpow;
    }
    return result;
}
function sparseEvaluateT<T extends ops<T>>(c: T[], x: T, monomial: boolean): T {
    let result	= c[0];
    let exp 	= 1;
	let xpow	= x.dup();
	
	for (const i in c) {
		const exp1 = +i;
		if (exp1 === 0)
			continue;
		while (exp < exp1) {
			xpow = xpow.mul(x);
			++exp;
		}
		result = result.add(c[exp1].mul(xpow));
	}
	if (monomial) {
		const exp1 = c.length - 1;
		while (exp < exp1) {
			xpow = xpow.mul(x);
			++exp;
		}
		result = result.add(xpow);
	}
    return result;
}

function sparseEvaluateNT<T extends ops<T> & has<'from'>>(c: number[], x: T, monomial: boolean): T {
    let result	= x.from(0);
    let xpow	= x.from(1);
    let exp 	= 0;
    
    for (const i in c) {
		const exp1 = +i;
        while (exp < exp1) {
			xpow = xpow.mul(x);
			++exp;
		}
        result = result.add(xpow.scale(c[i]));
    }
	if (monomial) {
		const exp1 = c.length - 1;
        while (exp < exp1) {
			xpow = xpow.mul(x);
			++exp;
		}
        result = result.add(xpow);
	}
    return result;
}

// multiply

function sparseMultiply(a: number[], b: number[], monomial: boolean) {
	const r: number[] = [];
	for (const i in a) {
		const ai = a[i];
		for (const j in b) {
			const k = +i + +j;
			r[k] = (r[k] ?? 0) + ai * b[j];
		}
	}
	if (monomial) {
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
function sparseMultiplyT<T extends ops<T>>(a: T[], b: T[], monomial: boolean) {
	const zero = a[0].scale(0);
	const r: T[] = [];
	for (const i in a) {
		const ai = a[i];
		for (const j in b) {
			const k = +i + +j;
			r[k] = (r[k] ?? zero).add(ai.mul(b[j]));
		}
	}
	if (monomial) {
		for (const i in a) {
			const k = +i + b.length;
			r[k] = (r[k] ?? zero).add(a[i]);
		}
		for (const i in b) {
			const k = +i + a.length;
			r[k] = (r[k] ?? zero).add(b[i]);
		}
	}
	return r;
}

// divmod

function sparseDivMod(a: number[], b: number[], monomial: boolean) {
	const blen	= b.length - (monomial ? 0 : 1);
	const qlen	= Math.max(a.length - b.length + 1, 0);
	const bt	= b[blen];
	const q: number[] = [];

	for (let i = qlen; i--;) {
		let at = a[i + blen];
		if (at) {
			if (!monomial)
				at /= bt;
			q[i] = at;
			for (const j in b)
				a[i + +j] = (a[i + +j] || 0) - b[j] * at;
		}
	}
	a.length = blen;
	sparseTrim(a);
	return q;
}
function sparseDivModB(a: bigint[], b: bigint[]) {
	const blen 	= b.length - 1;
	const qlen	= Math.max(a.length - blen, 0);
	const q: bigint[] = [];

	for (let i = qlen; i--;) {
		const at = a[i + blen];
		if (at) {
			q[i] = at;
			for (const j in b)
				a[i + +j] = (a[i + +j] || 0n) - b[j] * at;
		}
	}
	a.length = blen;
	sparseTrimB(a);
	return q;
}
function sparseDivModT<T extends ops<T>>(a: T[], b: T[], monomial: boolean) {
	const blen	= monomial ? b.length : b.length - 1;
	const qlen	= Math.max(a.length - b.length + 1, 0);
	const bt	= b[blen];
	const q: T[] = [];

	for (let i = qlen; i--;) {
		let at = a[i + blen];
		if (at) {
			if (!monomial)
				at = at.div(bt);
			q[i] = at;
			for (const j in b)
				a[i + +j] = a[i + +j] ? a[i + +j].sub(at.mul(b[j])) : at.mul(b[j]).neg();
		}
	}
	a.length = blen;
	sparseTrimT(a);
	return q;
}

// pseudo-division

function sparsePseudoRem(a: number[], b: number[]) {
	const blen	= b.length - 1;
	const qlen	= Math.max(a.length - blen, 0);
	const bt	= b[blen];

	for (let i = qlen; i--;) {
		const at = a[i + blen];
		if (at) {
			arrayScale(a, bt);
			for (const j in b)
				a[i + +j] = (a[i + +j] || 0) - b[j] * at;
		}
	}
	a.length = blen;
	sparseTrim(a);
}
function sparsePseudoRemB(a: bigint[], b: bigint[]) {
	const blen	= b.length - 1;
	const qlen	= Math.max(a.length - blen, 0);
	const bt	= b[blen];

	for (let i = qlen; i--;) {
		const at = a[i + blen];
		if (at) {
			arrayScaleB(a, bt);
			for (const j in b)
				a[i + +j] = (a[i + +j] || 0n) - b[j] * at;
		}
	}
	let rlen = blen;
	while (rlen > 0 && !a[rlen - 1])
		rlen--;
	a.length = rlen;
}
function sparsePseudoRemT<T extends ops<T>>(a: T[], b: T[]) {
	const blen	= b.length - 1;
	const qlen	= Math.max(a.length - blen, 0);
	const bt	= b[blen];

	for (let i = qlen; i--;) {
		const at = a[i + blen];
		if (at) {
			arrayScaleT(a, bt);
			for (const j in b)
				a[i + +j] = a[i + +j] ? a[i + +j].sub(at.mul(b[j])) : at.mul(b[j]).neg();
		}
	}
	a.length = blen;
	sparseTrimT(a);
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
	if (arrayOf(a, has('sign'))) {
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

function denseEvaluate(c: number[], t: number, monomial: boolean): number {
	let i = c.length;
	let r = c[--i];
	if (monomial)
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
function denseEvaluateT<T extends ops<T>>(c: T[], t: T, monomial: boolean): T {
	let i = c.length;
	let r = c[--i];
	if (monomial)
		r = r.add(t);
	while (i--)
		r = r.mul(t).add(c[i]);
	return r;
}

function denseEvaluateNT<T extends ops<T> & has<'from'>>(c: number[], t: T, monomial: boolean): T {
	let i = c.length;
	let r = t.from(c[--i]);
	if (monomial)
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

function denseMultiply(a: number[], b: number[], monomial: boolean) {
	const r = new Array<number>(a.length + b.length).fill(0);
	for (let i = 0; i < a.length; i++)
		for (let j = 0; j < b.length; j++)
			r[i + j] += a[i] * b[j];

	if (monomial) {
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
function denseMultiplyT<T extends ops<T>>(a: T[], b: T[], monomial: boolean) {
	const zero = a[0].scale(0);
	const blen = monomial ? b.length : b.length - 1;
	return Array.from({ length: a.length + blen }, (_, k) => {
		let sum = zero;
		for (let i = Math.max(0, k - blen); i <= Math.min(a.length - 1, k); i++)
			sum = sum.add(a[i].mul(b[k - i]));
		return sum;
	});
}

// divmod

function denseDivMod(a: number[], b: number[], monomial: boolean) {
	const blen	= b.length - (monomial ? 0 : 1);
	const qlen	= Math.max(a.length - b.length + 1, 0);
	const bt	= b[blen];
	const q: number[] = [];

	for (let i = qlen; i--;) {
		let at = a[i + blen];
		if (at) {
			if (!monomial)
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
function denseDivModB(a: bigint[], b: bigint[], monomial: boolean) {
	const blen	= b.length - (monomial ? 0 : 1);
	const qlen	= Math.max(a.length - b.length + 1, 0);
	const bt	= b[blen];
	const q: bigint[] = [];

	for (let i = qlen; i--;) {
		let at = a[i + blen];
		if (at) {
			if (!monomial)
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
function denseDivModT<T extends ops<T>>(a: T[], b: T[], monomial: boolean) {
	const blen	= b.length - (monomial ? 0 : 1);
	const qlen	= Math.max(a.length - b.length + 1, 0);
	const bt	= b[blen];
	const q: T[] = [];

	for (let i = qlen; i--;) {
		let at = a[i + blen];
		if (at) {
			if (!monomial)
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
	degree()	{ return this.c.length - 1; }
	leadCoeff() { return this.c[this.c.length - 1] ?? 0; }
	dup()		{ return new polynomial(this.c.slice()); }

	evaluate(t: complex): complex;
	evaluate(t: number): number;
	evaluate(t: number[]): number[];
	evaluate(t: number|number[]|complex) {
		if (typeof t === 'number')
			return sparseEvaluate(this.c, t, false);

		if (Array.isArray(t))
			return t.map(t => sparseEvaluate(this.c, t, false));

		return sparseEvaluateNT(this.c, t, false);
	}
	deriv() {
		return new polynomial(this.c.slice(1).map((v, i) => v * (i + 1)));
	}
	add(b: number|polynomial) {
		if (typeof b === 'number')
			return new polynomial([(this.c[0] ?? 0) + b, ...this.c.slice(1)]);
		return new polynomial(sparseAdd(this.c.slice(), b.c));
	}
	sub(b: number|polynomial) {
		if (typeof b === 'number')
			return new polynomial([(this.c[0] ?? 0) - b, ...this.c.slice(1)]);
		return new polynomial(sparseSub(this.c.slice(), b.c));
	}
	scale(b: number) {
		return new polynomial(this.c.map(a => a * b));
	}
	rscale(b: number) {
		return this.scale(1 / b);
	}
	mul(b: polynomial) {
		return new polynomial(sparseMultiply(this.c, b.c, false));//or denseMultiply(this.c, b.c, false);
	}
	div(b: polynomial) {
		return this.dup().divmod(b);
	}

	selfAdd(b: number|polynomial) {
		if (typeof b === 'number') {
			this.c[0] = (this.c[0] ?? 0) + b;
		} else {
			sparseAdd(this.c, b.c);
			sparseTrim(this.c);
		}
	}
	selfSub(b: number|polynomial) {
		if (typeof b === 'number') {
			this.c[0] = (this.c[0] ?? 0) - b;
		} else {
			sparseSub(this.c, b.c);
			sparseTrim(this.c);
		}
	}
	selfScale(b: number) {
		arrayScale(this.c, b);
	}
	selfRscale(b: number) {
		arrayScale(this.c, 1 / b);
	}

	divmod(b: polynomial): polynomial {
		return new polynomial(sparseDivMod(this.c, b.c, false));// or denseDivmod(a, b);
	}
	pseudoRemainder(b: polynomial) {
		sparsePseudoRem(this.c, b.c);
	}

	content() {
		return Num.gcd(...this.c);
	}
	abs() {
		return this.leadCoeff() < 0 ? new polynomial(this.c.map(v => -v)) : this;
	}
	sign() {
		return Math.sign(this.leadCoeff());
	}
	normalise(epsilon = defaultEpsilon) {
		let i = this.c.length - 1;
		while (i && Math.abs(this.c[i]) < epsilon)
			i--;
		const f = 1 / this.c[i];
		return new polynomialN(this.c.slice(0, i).map(v => v * f));
	}
	rationalRoots(): rational[] {
		return rationalRootsN(this.dup());
	}
	realRoots(epsilon = defaultEpsilon): number[] {
		return this.normalise().realRoots(epsilon);
	}
	allRoots(epsilon = defaultEpsilon): complex[] {
		return this.normalise(epsilon).allRoots(epsilon);
	}
	refine_roots(x: number[], count = 1) {
		const	d1	= this.deriv();
		const	d2	= d1.deriv();
		return x.map(x => halley(this, d1, d2, x, count));
	}
	map<U extends PolyTypes>(func: (c: number, i: number) => U): Polynomial<U> {
		return Polynomial(this.c.map((c, i) => func(c, i)));
	}

	toString(debug = false) {
		return this.c.map((coef, i) => {
			const s = i === this.degree() ? '' : coef < 0 ? ' - ' : ' + ';
			if (s)
				coef = Math.abs(coef);
			return s + coefficientString(coef, i, debug);
		}).reverse().join('');
	}
	[Symbol.for("debug.description")]() { return this.toString(true); }
}

//-----------------------------------------------------------------------------
//	Normalised Polynomial with implicit leading coefficient of 1
//-----------------------------------------------------------------------------

class polynomialN implements PolynomialN<number> {
	constructor(public c: number[]) {}
	degree()	{ return this.c.length; }
	dup()		{ return new polynomialN(this.c.slice()); }

	evaluate(t: complex): complex;
	evaluate(t: number): number;
	evaluate(t: number[]): number[];
	evaluate(t: number|number[]|complex) {
		if (typeof t === 'number')
			return sparseEvaluate(this.c, t, true);
		if (Array.isArray(t))
			return t.map(t => sparseEvaluate(this.c, t, true));
		return sparseEvaluateNT(this.c, t, true);
	}
	deriv() {
		return new polynomial([...this.c.slice(1).map((v, i) => v * (i + 1)), this.c.length]);
	}
	mul(b: polynomialN) {
		return new polynomialN(sparseMultiply(this.c, b.c, true));
	}

	divmod(b: polynomialN) {
		return new polynomial(sparseDivMod(this.c, b.c, true));
	}
	rationalRoots(): rational[] {
		return rationalRootsN(new polynomial([...this.c, 1]));
	}
	realRoots(epsilon = defaultEpsilon): number[] {
		return normPolyRealRoots(this.c, epsilon);
	}
	allRoots(epsilon = defaultEpsilon): complex[] {
		return normPolyComplexRoots(this.c, epsilon);
	}
	refine_roots(x: number[], count = 1) {
		const	d1	= this.deriv();
		const	d2	= d1.deriv();
		return x.map(x => halley(this, d1, d2, x, count));
	}
	toString(debug = false) {
		return this.c.map((coef, i) =>
			(coef < 0 ? ' - ' : ' + ') + coefficientString(Math.abs(coef), i, debug)
		).reverse().join('');
	}
	[Symbol.for("debug.description")]() { return this.toString(true); }
}

//-----------------------------------------------------------------------------
//	Polynomial with bigint coefficients
//-----------------------------------------------------------------------------

class polynomialB implements Polynomial<bigint> {
	constructor(public c: bigint[]) { sparseTrimB(this.c); }
	degree()	{ return this.c.length - 1; }
	leadCoeff() { return this.c[this.c.length - 1] ?? 0n; }
	dup()		{ return new polynomialB(this.c.slice()); }

	evaluate(t: bigint[]): bigint[];
	evaluate(t: bigint): bigint;
	evaluate(t: rationalB): rationalB;
	evaluate(t: bigint|rationalB|bigint[]) {
		if (Array.isArray(t))
			return t.map(t => sparseEvaluateB(this.c, t));
		if (isInstance(t, rationalB))
			return rationalB(sparseEvaluateB(this.c, t.num), sparseEvaluateB(this.c, t.den));
		return sparseEvaluateB(this.c, t);
	}
	deriv() {
		return new polynomialB(this.c.slice(1).map((v, i) => v * BigInt(i + 1)));
	}
	add(b: bigint|polynomialB) {
		if (typeof b === 'bigint')
			return new polynomialB([(this.c[0] ?? 0n) + b, ...this.c.slice(1)]);
		return new polynomialB(sparseAddB(this.c.slice(), b.c));
	}
	sub(b: bigint|polynomialB) {
		if (typeof b === 'bigint')
			return new polynomialB([(this.c[0] ?? 0n) - b, ...this.c.slice(1)]);
		return new polynomialB(sparseSubB(this.c.slice(), b.c));
	}
	scale(b: bigint) {
		return new polynomialB(this.c.map(a => a * b));
	}
	rscale(b: bigint) {
		return new polynomialB(this.c.map(a => a / b));
	}
	mul(b: polynomialB) {
		return new polynomialB(sparseMultiplyB(this.c, b.c));
	}
	div(b: polynomialB) {
		return this.dup().divmod(b);
	}

	selfAdd(b: bigint|polynomialB) {
		if (typeof b === 'bigint') {
			this.c[0] = (this.c[0] ?? 0n) + b;
		} else {
			sparseAddB(this.c, b.c);
			sparseTrimB(this.c);
		}
	}
	selfSub(b: bigint|polynomialB) {
		if (typeof b === 'bigint') {
			this.c[0] = (this.c[0] ?? 0n) - b;
		} else {
			sparseSubB(this.c, b.c);
			sparseTrimB(this.c);
		}
	}
	selfScale(b: bigint) {
		arrayScaleB(this.c, b);
	}
	selfRscale(b: bigint) {
		arrayRscaleB(this.c, b);
	}

	divmod(b: polynomialB) {
		return new polynomialB(sparseDivModB(this.c, b.c));// or denseDivmodB(a, b.c);
	}
	pseudoRemainder(b: polynomialB) {
		sparsePseudoRemB(this.c, b.c);
	}
	content() {
		return Big.gcd(...this.c);
	}
	abs() {
		return this.leadCoeff() < 0n ? new polynomialB(this.c.map(v => -v)) : this;
	}
	sign() {
		return Big.sign(this.leadCoeff());
	}

	normalise(epsilon = defaultEpsilon) {
		let i = this.c.length - 1;
		while (i && Big.abs(this.c[i]) < epsilon)
			i--;
		const d = this.c[i];
		return new polynomialNT<rationalB>(this.c.slice(0, i).map(v => rationalB(v, d)));
	}

	rationalRoots(): rationalB[] {
		return rationalRootsB(this.dup());
	}

	realRoots(): rationalB[] {
		const p = this.dup();
		const roots: rationalB[] = rationalRootsB(p);
		if (p.degree() > 0)
			roots.push(...p.normalise().realRoots());
		return roots;
	}
	allRoots(): never {
		return undefined as never;
	}
	refine_roots(x: rationalB[], count = 1) : rationalB[] {
		return this.normalise().refine_roots(x, count);
	}
	map<U extends PolyTypes>(func: (c: bigint, i: number) => U): Polynomial<U> {
		return Polynomial(this.c.map((c, i) => func(c, i)));
	}

	toString(debug = false) {
		return this.c.map((coef, i) => {
			const s = i === this.degree() ? '' : coef < 0n ? ' - ' : ' + ';
			if (s)
				coef = Big.abs(coef);
			return s + coefficientString(coef, i, debug);
		}).reverse().join('');
	}
	[Symbol.for("debug.description")]() { return this.toString(true); }
}

//-----------------------------------------------------------------------------
//	Polynomial with generic coefficients supporting ops interface
//-----------------------------------------------------------------------------

class polynomialT<T extends ops<T>> implements Polynomial<T> {
	constructor(public c: T[]) {
		sparseTrimT(c);
	}
	degree()	{ return this.c.length - 1; }
	leadCoeff() { return this.c[this.c.length - 1]; }
	dup() 		{ return new polynomialT<T>(this.c.slice()); }

	evaluate(t: T): T;
	evaluate(t: T[]): T[];
	evaluate(t: T|T[]) {
		if (Array.isArray(t))
			return t.map(t => sparseEvaluateT(this.c, t, false));
		return sparseEvaluateT(this.c, t, false);
	}
	deriv() {
		return new polynomialT(this.c.slice(1).map((v, i) => v.scale(i + 1)));
	}
	add(b: T|polynomialT<T>): polynomialT<T> {
		if (b instanceof polynomialT)
			return new polynomialT(sparseAddT(this.c.slice(), b.c));
		return new polynomialT([this.c[0].add(b), ...this.c.slice(1)]);
	}
	sub(b: T|polynomialT<T>): polynomialT<T> {
		if (b instanceof polynomialT)
			return new polynomialT(sparseSubT(this.c.slice(), b.c));
		return new polynomialT([this.c[0].sub(b), ...this.c.slice(1)]);
	}
	scale(b: number|T): polynomialT<T> {
		return new polynomialT(typeof b === 'number' ? this.c.map(a => a.scale(b)) : this.c.map(a => a.mul(b)));
	}
	rscale(b: number|T): polynomialT<T> {
		return typeof b === 'number' ? this.scale(1 / b) : new polynomialT(this.c.map(a => a.div(b)));
	}
	mul(b: polynomialT<T>) {
		return new polynomialT(sparseMultiplyT(this.c, b.c, false));
	}
	div(b: polynomialT<T>) {
		return this.dup().divmod(b);
	}
	selfAdd(b: T|polynomialT<T>) {
		if (b instanceof polynomialT) {
			sparseAddT(this.c, b.c);
		} else {
			this.c[0] = this.c[0].add(b);
			sparseTrimT(this.c);
		}
	}
	selfSub(b: T|polynomialT<T>) {
		if (b instanceof polynomialT) {
			sparseSubT(this.c, b.c);
		} else {
			this.c[0] = this.c[0].sub(b);
			sparseTrimT(this.c);
		}
	}
	selfScale(b: number|T) {
		arrayScaleT(this.c, b);
	}
	selfRscale(b: number|T) {
		arrayRscaleT(this.c, b);
	}
	
	divmod(b: polynomialT<T>) {
		return new polynomialT(sparseDivModT(this.c, b.c, false));
	}

	pseudoRemainder(b: polynomialT<T>) {
		sparsePseudoRemT(this.c, b.c);
	}

	content(): T extends scalar<any> ? T : never {
		if (arrayOf(this.c, isScalarExt))
			return Gen.gcd(...this.c) as any;
		return undefined as never;
	}
	abs() : Polynomial<T> {
		return arrayOf(this.c, has('sign')) && this.c[this.c.length-1].sign() < 0 ? new polynomialT(this.c.map(v => v.neg())) : this;
	}
	sign() {
		return this.c.length === 0 ? 0 : arrayOf(this.c, has('sign')) ? this.c[this.c.length-1].sign() : NaN;
	}

	normalise(epsilon = defaultEpsilon): PolynomialN<normalized<T>> {
		let i = this.c.length - 1;
		while (i && lessThan(this.c[i].mag(), epsilon))
			i--;
		const f = this.c[i];
		return new polynomialNT<T>(this.c.slice(0, i).map(v => v.div(f))) as unknown as PolynomialN<normalized<T>>;
	}

	rationalRoots(): rationalRoots<T> {
		if (arrayOf(this.c, isScalar)) {
			if (arrayOf(this.c, v => isInstance(v, rationalB))) {
				const m = Big.lcm(...this.c.map((v: rationalB) => v.den));
				const p2 = new polynomialB(this.c.map((v: rationalB) => v.num * (m / v.den)));
				return rationalRootsB(p2) as rationalRoots<T>;

			} else if (arrayOf(this.c, v => isInstance(v, rational))) {
				const m = Num.lcm(...this.c.map((v: rational) => v.den));
				const p2 = new polynomial(this.c.map((v: rational) => v.num * (m / v.den)));
				return rationalRootsN(p2) as rationalRoots<T>;

			} else if (arrayOf(this.c, isScalarRational)) {
				const p2 = this.c.map((c: scalarRational<any>) => rationalB.from(c, 1n << 32n));
				const m = Big.lcm(...p2.map(v => v.den));
				const p4 = new polynomialB(p2.map(v => v.num * (m / v.den)));
				return rationalRootsB(p4) as rationalRoots<T>; 
			}
		}
		return undefined as never;
	}

	realRoots(epsilon = defaultEpsilon): realRoots<T> {
		return this.normalise().realRoots(epsilon) as realRoots<T>;
	}
	allRoots(epsilon = defaultEpsilon): complexRoots<T> {
		return this.normalise().allRoots!(epsilon) as complexRoots<T>;
	}
	map<U extends PolyTypes>(func: (c: T, i: number) => U): Polynomial<U> {
		return Polynomial(this.c.map((c, i) => func(c, i)));
	}

	refine_roots(x: realRoots<T>, count = 1): realRoots<T> {
		const	d1	= this.deriv();
		const	d2	= d1.deriv();
		return x.map(x => halleyT(this, d1, d2, x as any as T, count)) as realRoots<T>;
	}
	toString(debug = false) {
		if (this.c.length < 2)
			return this.c.length ? String(this.c[0]) : '0';
		const c = this.c.slice(0, -1);
		const n = c.length;
		return coefficientString(this.c[n], n, debug) + (arrayOf(c, isScalarExt) ? scalarPolynomialString(c, debug) : ' + ' + polynomialString(c, debug));
	}
//	[Symbol.for("debug.description")]() { return this.toString(true); }
}

//-----------------------------------------------------------------------------
//	Normalised General Polynomial with implicit leading coefficient of 1
//-----------------------------------------------------------------------------

class polynomialNT<T extends ops<T>> implements PolynomialN<T> {
	constructor(public c: T[]) {}
	degree()	{ return this.c.length; }
	dup()		{ return new polynomialNT<T>(this.c.slice()); }

	evaluate(t: T): T;
	evaluate(t: T[]): T[];
	evaluate(t: T|T[]) {
		if (Array.isArray(t))
			return t.map(t => sparseEvaluateT(this.c, t, true));
		return sparseEvaluateT(this.c, t, true);
	}

	deriv() {
		if (isScalar(this.c[0]))
			return new polynomialT<T>([...this.c.slice(1).map((v, i) => v.scale(i + 1)), this.c[0].from(this.c.length)]);
		return undefined as never;
	}
	mul(b: PolynomialN<T>) {
		return new polynomialNT<T>(sparseMultiplyT(this.c, b.c, true));
	}
	divmod(b: PolynomialN<T>) {
		return new polynomialT<T>(sparseDivModT(this.c, b.c, true));
	}

	rationalRoots(): rationalRoots<T> {
		if (arrayOf(this.c, isScalar)) {
			if (arrayOf(this.c, v => isInstance(v, rationalB))) {
				const m = Big.lcm(...this.c.map((v: rationalB) => v.den));
				const p2 = new polynomialB([...this.c.map((v: rationalB) => v.num * (m / v.den)), m]);
				return rationalRootsB(p2) as rationalRoots<T>;

			} else if (arrayOf(this.c, v => isInstance(v, rational))) {
				const m = Num.lcm(...this.c.map((v: rational) => v.den));
				const p2 = new polynomial([...this.c.map((v: rational) => v.num * (m / v.den)), m]);
				return rationalRootsN(p2) as rationalRoots<T>;

			} else {
				const p2 = this.c.map(c => rationalB.from(c as any, 1n << 32n));
				const m = Big.lcm(...p2.map(v => v.den));
				const p4 = new polynomialB([...p2.map(v => v.num * (m / v.den)), m]);
				return rationalRootsB(p4) as rationalRoots<T>; 
			}
		}
		return undefined as never;
	}

	realRoots(epsilon = defaultEpsilon): realRoots<T> {
		if (polyNOf(this, isScalar)) {
			const eps = this.c[0].from(epsilon);
			if (polyNOf(this, has('rpow')))
				return normPolyRealRootsT(this.c, eps) as realRoots<T>;
			else
				return this.refine_roots(sturmIsolateIntervalsT(this, eps).map(i => bisectRootT(this, i.min, i.max, eps))) as realRoots<T>;
		}
		return undefined as never;
	}

	allRoots(epsilon = defaultEpsilon): complexRoots<T> {
		if (arrayOf(this.c, isScalarExt))
			return normPolyComplexRootsT(this.c, this.c[0].from(epsilon)) as complexRoots<T>;
		return undefined as never;
	}
	refine_roots(x: realRoots<T>, count = 1): realRoots<T> {
		const	d1	= this.deriv();
		const	d2	= d1.deriv();
		return x.map(x => halleyT(this as polynomialNT<T>, d1, d2, x as any as T, count)) as realRoots<T>;
	}
	toString(debug = false) {
		return	this.degree() === 0 ? '1'
			:	coefficientString(1, this.degree(), debug) + (arrayOf(this.c, isScalarExt) ? scalarPolynomialString(this.c, debug) : ' + ' + polynomialString(this.c, debug));
	}
	[Symbol.for("debug.description")]() { return this.toString(true); }
}

//-----------------------------------------------------------------------------
//	PRS: Polynomial Remainder Sequence and GCD
//-----------------------------------------------------------------------------

export function polyGCD<T extends PolyNTypes>(A: Polynomial<T>, B: Polynomial<T>): Polynomial<T> {
	return polyOf(A, isNumber)
		? Gen.gcd(A, B)
		: polyGCDT(A as Polynomial<any>, B as Polynomial<any>) as Polynomial<T>;
}

function polyGCDT<T extends ops<T>>(A: Polynomial<T>, B: Polynomial<T>) {
	const seq = subresultantPRST(A, B);
	for (let i = seq.length; i--;) {
		if (seq[i].degree() > 0)
			return seq[i].rscale(seq[i].leadCoeff());//normalise();
	}
	return new polynomialT<T>([]);
}

// Build a simple subresultant PRS sequence using pseudoRemainder.
function subresultantPRST<T extends ops<T>>(A: Polynomial<T>, B: Polynomial<T>) {
	A = A.dup();
	B = B.dup();
	const seq = [A.dup(), B.dup()];

	while (B.degree() >= 0 && seq.length < 128) {	
		// perform pseudo-remainder in-place on A (pseudoRemainder mutates its receiver)
		A.pseudoRemainder(B);

		const cont = A.content();
		if (cont !== undefined)
			A.selfRscale(cont);

		seq.push(A.dup());
		[A, B] = [B, A];
	}
	return seq;
}

export function squareFreeFactorization<T extends PolyNTypes>(f: Polynomial<T>): { factor: Polynomial<T>, multiplicity: number }[] {
	return polyOf(f, isNumber)
		? squareFreeFactorizationN(f) as { factor: Polynomial<T>, multiplicity: number }[]
		: squareFreeFactorizationT(f as Polynomial<any>);
}


// Return array of { factor, multiplicity } for a polynomial f using primitive PRS gcd
function squareFreeFactorizationN(f: Polynomial<number>) {
	const res: { factor: Polynomial<number>, multiplicity: number }[] = [];
	if (f.degree() <= 0)
		return res;

	let g = polyGCD(f, f.deriv());
	let w = f.dup().divmod(g);

	for (let i = 1; i < 256 && w.degree() > 0; i++) {
		const y		= polyGCD(w, g);
		const fi	= w.divmod(y);

		// only record non-constant factors
		if (fi.degree() > 0)
			res.push({ factor: fi, multiplicity: i });

		// w <- y, g <- g / y
		w = y;
		g = g.divmod(y);
	}
	return res;
}

// Return array of { factor, multiplicity } for a polynomial f using primitive PRS gcd
function squareFreeFactorizationT<T extends ops<T>>(f: Polynomial<T>) {
	const res: { factor: Polynomial<T>, multiplicity: number }[] = [];
	if (f.degree() <= 0)
		return res;

	let g = polyGCDT(f, f.deriv());
	let w = f.dup().divmod(g);

	for (let i = 1; i < 256 && w.degree() > 0; i++) {
		const y = polyGCDT(w, g);
		const fi = w.divmod(y);
		
		// only record non-constant factors
		if (fi.degree() > 0) {
			const cont = fi.content();
			if (cont)
				fi.selfRscale(cont);
			res.push({ factor: fi, multiplicity: i });
		}

		// w <- y, g <- g / y
		w = y;
		g = g.divmod(y);
	}
	return res;
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
/*
function sumTop2B(a: bigint[]) {
	if (a.length < 2)
		return a.length === 1 ? a[0] : 0n;
	let max1 = a[0], max2 = a[0];
	for (const v of a) {
		if (max1 < v) {
			max2 = max1;
			max1 = v;
		} else if (max2 < v) {
			max2 = v;
		}
	}
	return max1 + max2;
}
*/
function sumTop2T<T extends scalar<T>>(zero: T, a: T[]) {
	if (a.length < 2)
		return a.length === 1 ? a[0] : zero;
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


function lagrangeImproved(c: number[]|complex[]): number {
	const N = c.length;
	if (arrayOf(c, isNumber))
		return sumTop2(c.map((c, i) => Math.pow(Math.abs(c), 1 / (N - i))));
	return sumTop2(c.map((c, i) => Math.pow(c.abs(), 1 / (N - i))));
}
/*
function lagrangeImprovedB(c: bigint[]): bigint {
	const N = c.length;
	return sumTop2B(c.map((c, i) => rootB(Big.abs(c), N - i)));
}
*/
function lagrangeImprovedT<T extends scalarExt<T>>(c: T[]|complexT<T>[]) {
	const N = c.length;
	const zero = isInstance(c[0], complexT) ? c[0].r.from(0) : c[0].from(0);
	return sumTop2T(zero, c.map((c, i) => c.abs().rpow(1, (N - i))));
}

function realBound(c: number[]): extent {
	const N = c.length;
	const terms = c.map((c, i) => Math.pow(Math.abs(c), 1 / (N - i)));

	return new extent(
		-sumTop2(terms.filter((x, i) => (i % 2 == 0) === (c[i] > 0))),
		sumTop2(terms.filter((x, i) => c[i] < 0))
	);
}

function realBoundT<T extends scalar<T>>(k: polynomialNT<T>) {
	const N = k.c.length;
	const terms = k.c.map((c, i) => asScalarExt(c.mag()).rpow(1, (N - i)));

	const zero = terms[0].from(0);

	return new extentT(
		sumTop2T(zero, terms.filter((x, i) => (i % 2 == 0) === (k.c[i].sign() > 0))).neg(),
		sumTop2T(zero, terms.filter((x, i) => k.c[i].sign() < 0))
	);
/*
	if (arrayOf(terms, isNumber)) {
		return new extent(
			-sumTop2(terms.filter((x, i) => (i % 2 == 0) === (k.c[i].sign() > 0))),
			sumTop2(terms.filter((x, i) => k.c[i].sign() < 0))
		);
	} else if (arrayO<scalar<any>>(terms)) {
		const zero = terms[0].from(0);

		return new extentT(
			sumTop2T(zero, terms.filter((x, i) => (i % 2 == 0) === (k.c[i].sign() > 0))).neg(),
			sumTop2T(zero, terms.filter((x, i) => k.c[i].sign() < 0))
		);
	} else {
		throw new Error("realBoundT: unsupported coefficient type");
	}
		*/
}

//-----------------------------------------------------------------------------
//	root refinement
//-----------------------------------------------------------------------------

function bisectRoot(p: polynomial|polynomialN, min: number, max: number, tol: number, maxIter = 10) {
	let fmin = p.evaluate(min);

	for (let i = 0; i < maxIter && max - min > tol; i++) {
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
function bisectRootT<T extends scalar<T>>(p: polynomialT<T>|polynomialNT<T>, min: T, max: T, threshold: T, maxIter = 10) {
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

function halley(p: polynomial|polynomialN, d1: polynomial, d2: polynomial,x: number, maxIter = 10, epsilon = 1e-14) {
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
function halleyScalarT<T extends scalar<T>>(p: polynomialT<T>|polynomialNT<T>, d1:polynomialT<T>, d2:polynomialT<T>, x: T, maxIter = 10, threshold?: T) {
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

function halleyT<T extends ops<T>>(p: polynomialT<T>|polynomialNT<T>, d1:polynomialT<T>, d2:polynomialT<T>, x: T, maxIter = 10, threshold?: T) {
	if (p.degree() < 2)
		return x;

	if (isScalar(x))
		return halleyScalarT(p as any, d1 as any, d2 as any, x, maxIter, threshold as any) as unknown as T;

	for (let i = 0; i < maxIter; i++) {
		const f		= p.evaluate(x);
		const f1	= d1.evaluate(x);
		const f2	= d2.evaluate(x);
		const denom = f1.mul(f1).scale(2).sub(f.mul(f2));
		const dx = f.mul(f1).scale(2).div(denom);
		x = x.sub(dx);
	}
	return x;
}

//	adjust intervals to guarantee convergence of halley's method by using roots of further derivatives
function adjust_roots(dpoly: polynomial|polynomialN, extents: extent[]) {
	const	rootsd	= dpoly.realRoots();
	for (const ext of extents) {
		for (const r of rootsd) {
			if (ext.contains(r)) {
				if (dpoly.evaluate(r) > 0)
					ext.max = r;
				else
					ext.min = r;
			}
		}
	}
	return rootsd.length;
}

// determine multiplicity of root at numeric x by checking successive derivatives
export function multiplicityAt<T extends number | scalar<any>>(poly: Polynomial<T> | PolynomialN<T>, x: T, epsilon = Math.max(defaultEpsilon, 1e-12)) {
	if (poly.degree() < 2)
		return 1;

	let multiplicity = 0;
	if (isNumber(x)) {
		const threshold = epsilon * Math.max(1, Math.abs(x));
		while (poly.degree() >= 0 && Math.abs((poly as Polynomial<number>).evaluate(x)) < threshold) {
			poly = poly.deriv();
			multiplicity++;
		}
	} else {
		const threshold = x.abs().scale(epsilon);
		while (poly.degree() >= 0 && (poly.evaluate(x) as scalar<any>).abs().lt(threshold)) {
			poly = poly.deriv();
			multiplicity++;
		}
	}

	return multiplicity;
}

//-----------------------------------------------------------------------------
//	real roots
//-----------------------------------------------------------------------------

function normPolyRealRoots(k: number[], epsilon: number): number[] {
	let zeros = 0;
	while (k[zeros] === 0)
		++zeros;

	return zeros > 0
		? insertSorted(checkEven(k.slice(zeros)), 0)
		: checkEven(k);

	function checkEven(k: number[]) {
		if (k.length % 2 == 0 && k[1] === 0) {
			let odds = false;
			for (let j = 3; j < k.length; j += 2) {
				odds = k[j] !== 0;
				if (odds)
					break;
			}
			if (!odds) {
				const r = normal(k.filter((v, i) => i % 2 == 0)).filter(r => r > 0).map(r => Math.sqrt(r));
				return [...r.map(r => -r).reverse(), ...r];
			}
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
					const	angle	= Math.atan2(Num.copySign(Math.sqrt(-h), g), g) / 3;
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
					roots3 = [0, ...normPolyRealRoots([q, p, 0], epsilon)];
				} else {
					// solve the resolvent cubic ...
					const r 	= normPolyRealRoots([-q * q / 8, p * p / 4 - t, p], epsilon);
					const m 	= Math.max(...r);
					const v 	= Math.sqrt(m * 2);
					const u 	= q / Math.sqrt(m * 8);
					roots3 = [...normPolyRealRoots([m + p / 2 - u, v], epsilon), ...normPolyRealRoots([m + p / 2 + u, -v], epsilon)];
				}
				return roots3.map(r => r - k[3] / 4);
			}

			case 5: {
				const poly		= new polynomialN(k);
				const dpoly		= poly.deriv();
				const d2poly	= dpoly.deriv();
				const r1		= dpoly.realRoots();
				const bounds	= realBound(k);

				let r: number[];

				if (r1.length === 4) {
					r = [];
					const r2	= [bounds.min, ...r1, bounds.max];
					const vals 	= poly.evaluate(r2);
					for (let i = 1; i < 6; i++) {
						if (vals[i] * vals[i - 1] <= 0)
							r.push((r2[i] + r2[i - 1]) / 2);
					}


				} else {
					const ranges = [
						bounds,
					];
					const vals 	= poly.evaluate(r1);
					if (r1.length === 2) {
						//roots1.xy	= sort(get(roots1.xy));
						if (vals[0] < 0) {
							ranges[0].min = r1[1];
						} else if (vals[1] > 0) {
							ranges[0].max = r1[0];
						} else {
							ranges[0].max = r1[0];
							ranges.push(new extent(r1[1], bounds.max));
						}
					}

					//further subdivide intervals to guarantee convergence of halley's method by using roots of further derivatives
					const num_roots = adjust_roots(d2poly, ranges);
					if (num_roots != 3)
						adjust_roots(d2poly.deriv(), ranges);

					r = ranges.map(i => i.centre());
				}

				//8 halley iterations
				return r.map(i => halley(poly, dpoly, d2poly, i, 8));
			}

			default: {
				const pN = new polynomialN(k);
				const d1 = pN.deriv();
				const d2 = d1.deriv();
				return sturmIsolateIntervals(pN).map(i => halley(pN, d1, d2, bisectRoot(pN, i.min, i.max, epsilon)));
			}
		}
	}
}


function normPolyRealRootsT<T extends scalarExt<T>>(k: T[], epsilon: T): T[] {
	const zero = k[0].scale(0);

	let zeros = 0;
	while (k[zeros].sign() === 0)
		++zeros;

	return zeros > 0
		? insertSorted(checkEven(k.slice(zeros)), zero, (a: T, b: T) => a.lt(b))
		: checkEven(k);

	function checkEven(k: T[]): T[] {
		if (k.length % 2 == 0 && k[1].sign() === 0) {
			let odds = false;
			for (let j = 3; j < k.length; j += 2) {
				odds = k[j].sign() !== 0;
				if (odds)
					break;
			}
			if (!odds) {
				const r = normal(k.filter((v, i) => i % 2 == 0)).filter(r => r.sign() > 0).map(r => r.sqrt());
				return [...r.map(r => r.neg()).reverse(), ...r];
			}
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

				let roots3;
				if (has('lt')(t) && t.abs().lt(epsilon)) {
					// no absolute term: y(y^3 + py + q) = 0
					roots3 = [zero, ...normPolyRealRootsT([q, p, zero], epsilon)];
				} else {
					// solve the resolvent cubic ...
					const r 	= normPolyRealRootsT([q.neg().mul(q).scale(1 / 8), p.mul(p).scale(1 / 4).sub(t), p], epsilon);
					const m 	= Gen.max(...r);
					const v 	= m.scale(2).sqrt();
					const u 	= q.div(m.scale(8).sqrt());
					roots3 = [...normPolyRealRootsT([m.add(p.scale(0.5)).sub(u), v], epsilon), ...normPolyRealRootsT([m.add(p.scale(0.5)).add(u), v.neg()], epsilon)];
				}
				return roots3.map(r => r.sub(k[3].scale(1 / 4)));
			}

			default: {
				const pN = new polynomialNT(k);
				const d1 = pN.deriv();
				const d2 = d1.deriv();
				return sturmIsolateIntervalsT(pN, epsilon).map(i => halleyT(pN, d1, d2, bisectRootT(pN, i.min, i.max, epsilon)));
			}
		}
	}
}

function sturmIsolateIntervals(pN: polynomialN) {
	const n = pN.degree();
	if (n === 0)
		return [] as extent[];

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
	const intervals: extent[] = [];

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
			intervals.push(new extent(lower, upper));

		if (stack.length == 0)
			break;
		[lower, upper, count] = stack.pop()!;
	}

	return intervals;
}

function sturmIsolateIntervalsT<T extends scalar<T>>(pN: polynomialNT<T>, epsilon: T): extentT<T>[] {
	const n = pN.degree();
	if (n === 0)
		return [];

	if (n === 1) {
		const root = pN.c[0].neg();
		return [new extentT(root, root)];
	}

	// Build Sturm sequence
	const seq: polynomialT<T>[] = [];
	let p0 = new polynomialT<T>([...pN.c, pN.c[0].from(1)]);
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
				const eps = Gen.max(x.from(1e-12), x.abs().scale(1e-12));//Math.max(1e-12, Math.abs(x) * 1e-12);
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
	const intervals: extentT<T>[] = [];

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
			intervals.push(new extentT(lower, upper));
				
		if (stack.length == 0)
			break;
		[lower, upper, count] = stack.pop()!;
	}

	return intervals;
}

//-----------------------------------------------------------------------------
//	complex roots
//-----------------------------------------------------------------------------

function normPolyComplexRoots(k: number[], epsilon: number): complex[] {
	let zeros = 0;
	while (k[zeros] === 0)
		++zeros;

	return zeros > 0
		? [...checkEven(k.slice(zeros)), complex.zero()]
		: checkEven(k);

	function checkEven(k: number[]): complex[] {
		if (k.length % 2 == 0 && k[1] === 0) {
			let odds = false;
			for (let j = 3; j < k.length; j += 2) {
				odds = k[j] !== 0;
				if (odds)
					break;
			}
			if (!odds) {
				const r = normal(k.filter((_, i) => i % 2 == 0)).map(r => r.sqrt());
				return [...r.map(r => r.neg()).reverse(), ...r];
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
					const	angle	= Math.atan2(Num.copySign(Math.sqrt(-h), g), g) / 3;
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
					roots3	= [complex.zero(), ...normPolyComplexRoots([q, p, 0], epsilon)];
				} else {
					// solve the resolvent cubic ...
					const r	= normPolyRealRoots([-q * q / 8, p * p / 4 - t, p], epsilon);
					const m = Math.max(...r);
					const v = Math.sqrt(m * 2);
					const u = q / Math.sqrt(m * 8);
					roots3 = [...normPolyComplexRoots([m + p / 2 - u, v], epsilon), ...normPolyComplexRoots([m + p / 2 + u, -v], epsilon)];
				}
				return roots3.map(r => complex(r.r - k[3] / 4, r.i));
			}
			
			default:
				return aberth(new polynomialN(k));
		}
	}	
}


function normPolyComplexRootsT<T extends scalarExt<T>>(k: T[], epsilon: T): complexT<T>[] {
	const zero = k[0].scale(0);

	let zeros = 0;
	while (k[zeros].sign() === 0)
		++zeros;

	return zeros > 0
		? [...checkEven(k.slice(zeros)), complexT(zero, zero)]
		: checkEven(k);

	function checkEven(k: T[]): complexT<T>[] {
		if (k.length % 2 == 0 && k[1].sign() === 0) {
			let odds = false;
			for (let j = 3; j < k.length; j += 2) {
				odds = k[j].sign() !== 0;
				if (odds)
					break;
			}
			if (!odds) {
				const r = normal(k.filter((v, i) => i % 2 == 0)).map(r => r.sqrt());
				return [...r.map(r => r.neg()).reverse(), ...r];
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
					return complexT.conjugatePair(complexT(e.neg(), d.sqrt()));
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
						const	s0		= Gen.copySign(h.neg().sqrt(), g);
						const	angle	= hasStatic(s0, 'atan2')?.(s0, g).div(3) ?? Math.atan2(Number(s0), Number(g)) / 3;
						const	c		= hasStatic(s0, 'cos')?.(angle) ?? k[0].from(Math.cos(angle));
						const	s		= hasStatic(s0, 'sin')?.(angle) ?? k[0].from(Math.sin(angle));
						//const	c		= k[0].from(Math.cos(angle)), s = k[0].from(Math.sin(angle));
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
				if (has('lt')(t) && t.abs().lt(epsilon)) {
					// no absolute term: y(y^3 + py + q) = 0
					roots3	= [complexT(zero, zero), ...normPolyComplexRootsT([q, p, zero], epsilon)];
				} else {
					// solve the resolvent cubic ...
					const r = normPolyRealRootsT([q.neg().mul(q).scale(1 / 8), p.mul(p).scale(1 / 4).sub(t), p], epsilon);
					const m = Gen.max(...r);
					const v = m.scale(2).sqrt();
					const u = q.div(m.scale(8).sqrt());
					roots3 = [...normPolyComplexRootsT([m.add(p.scale(0.5)).sub(u), v], epsilon), ...normPolyComplexRootsT([m.add(p.scale(0.5)).add(u), v.neg()], epsilon)];
				}
				return roots3.map(r => complexT(r.r.sub(k[3].scale(1 / 4)), r.i));
			}

			default:
				return aberthT<T>(new polynomialNT<T>(k));
		}
	}
}

function aberth(poly: polynomialN|polynomialNT<complex>, tolerance = 1e-6, maxIterations = 100) {
	const radius = lagrangeImproved(poly.c);
	const n		= poly.degree();
	const roots	= Array.from({length: n}, (_, i) => complex.fromPolar(radius, 2 * Math.PI * i / n));
	const dpoly = poly.deriv();

	for (let iter = 0; iter < maxIterations; iter++) {
		let maxCorrection = 0;
		for (let i = 0; i < roots.length; i++) {
			const zi	= roots[i];
			const p_zi	= poly.evaluate(zi);
			const dp_zi	= dpoly.evaluate(zi);
			
			let sum		= complex.zero();
			for (let j = 0; j < roots.length; j++) {
				if (i !== j)
					sum = sum.add(complex(1).div(zi.sub(roots[j])));
			}
			const correction = p_zi.div((dp_zi.sub(p_zi.mul(sum))));
			roots[i] = roots[i].sub(correction);
			maxCorrection = Math.max(maxCorrection, Math.abs(correction.r) + Math.abs(correction.i));
		}
		if (maxCorrection < tolerance)
			break;
	}
	return roots;
}

function isComplexPolyN<T extends scalarExt<T>>(poly: polynomialNT<T>|polynomialNT<complexT<T>>): poly is polynomialNT<complexT<T>> {
	return poly.c[0] instanceof complexT;
}

function aberthT<T extends scalarExt<T>>(poly: polynomialNT<T>|polynomialNT<complexT<T>>, tolerance?: T, maxIterations = 100) {
	let from: (v: number) => T;
	let evaluate: (p: any, t: complexT<T>) => complexT<T>;

	if (isComplexPolyN(poly)) {
		from = poly.c[0].r.from;
		evaluate = (p, t) => p.evaluate(t);
	} else {
		from = poly.c[0].from;
		evaluate = (p, t) => {
			let i = p.c.length - 1;
			let r = t.add(complexT<T>(p.c[i], p.c[i].scale(0)));
			while (i--)
				r = r.mul(t).add(complexT<T>(p.c[i], p.c[i].scale(0)));
			return r;
		};
	}

	const n			= poly.degree();
	const zero		= from(0), one = from(1);
	const czero		= complexT<T>(zero, zero), cone = complexT<T>(one, zero);
	const dpoly 	= poly.deriv();

	const radius	= lagrangeImprovedT<T>(poly.c);
	const roots		= Array.from({length: n}, (_, i) => complexT.fromPolar(radius, 2 * Math.PI * i / n));

	tolerance ??= from(1e-6);

	for (let iter = 0; iter < maxIterations; iter++) {
		let maxCorrection = zero;
		for (let i = 0; i < n; i++) {
			const zi	= roots[i];
			const p_zi	= evaluate(poly, zi);
			const dp_zi	= evaluate(dpoly, zi);

			let sum		= czero;
			for (let j = 0; j < roots.length; j++) {
				if (i !== j)
					sum = sum.add(cone.div(zi.sub(roots[j])));
			}
			const correction = p_zi.div((dp_zi.sub(p_zi.mul(sum))));
			roots[i] = roots[i].sub(correction);
			maxCorrection = Gen.max(maxCorrection, correction.abs());
		}
		if (maxCorrection.lt(tolerance))
			break;
	}
	return roots;
}

//-----------------------------------------------------------------------------
// Integer/Rational Root Theorem extractor
//-----------------------------------------------------------------------------

function divRoot(c: number[], root: number | rational): [number[], number] {
	let i = c.length - 1;
	if (i < 1)
		return [[], i ? c[0] : 0];

	const b = new Array<number>(i);
	if (typeof root === 'number') {
		b[i - 1] = c[i];
		while (--i)
			b[i - 1] = c[i] + root * b[i];
		return [b, c[0] + root * b[0]];

	} else {
		b[i - 1] = c[i];
		while (--i)
			b[i - 1] = c[i] * root.den + root.num * b[i];
		return [b, c[0] * root.den + root.num * b[0]];
	}
}
function divRootB(c: bigint[], root: bigint|rationalB): [bigint[], bigint] {
	let i = c.length - 1;
	if (i < 1)
		return [[], i ? c[0] : 0n];

	const b = new Array<bigint>(i);
	if (typeof root === 'bigint') {
		b[i - 1] = c[i];
		while (--i)
			b[i - 1] = c[i] + root * b[i];
		return [b, c[0] + root * b[0]];

	} else {
		b[i - 1] = c[i];
		while (--i)
			b[i - 1] = c[i] * root.den + root.num * b[i];
		return [b, c[0] * root.den + root.num * b[0]];
	}
}

function removeMultiplicity(p: polynomial, root: number|rational): boolean {
	while (p.degree() > 0) {
		const [ quotient, remainder ] = divRoot(p.c, root);
		if (remainder !== 0)
			return false;
		p.c = quotient;
	}
	return true;
}
function removeMultiplicityB(p: polynomialB, root: bigint|rationalB): boolean {
	while (p.degree() > 0) {
		const [ quotient, remainder ] = divRootB(p.c, root);
		if (remainder !== 0n)
			return false;
		p.c = quotient;
	}
	return true;
}

function* rationalDivisors(numFactors: number[], denFactors: number[], limit?: rational): Generator<rational> {
	const numEntries = Array.from(numFactors.entries());
	const denEntries = Array.from(denFactors.entries());
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

// modifies p to remove found roots
function rationalRootsN(p: polynomial): rational[] {
	const roots0: number[] = [];
	if (p.c[0] === 0)
		roots0.push(0);

	while (p.c[0] === 0)
		p.c = divRoot(p.c, 0)[0];

	const r = p.c.map(c => rational.from(c));
	const m = Num.lcm(...r.map(v => v.den));
	const p2 = new polynomial(r.map(v => v.num * (m / v.den)));

	p = p2;

	if (p.leadCoeff() === 1) {
		const a0		= p.c[0];
		const factors	= new factorisation(a0);
		const bound		= lagrangeImproved(p.c.slice(0, -1));

		for (const x of factors.divisors(bound)) {
			if (p.evaluate(x) === 0) {
				roots0.push(x);
				if (removeMultiplicity(p, x))
					break;
			}
			if (p.evaluate(-x) === 0) {
				roots0.push(-x);
				if (removeMultiplicity(p, -x))
					break;
			}
		}
		return roots0.map(r => rational.from(r));
	}

	const roots: rational[] = roots0.map(r => rational(r));

	const a0 = Math.abs(p.c[0]);
	const an = Math.abs(p.leadCoeff());

	const pFactors	= new factorisation(a0);
	const qFactors	= new factorisation(an);
	const bound 	= rational.from(lagrangeImproved(p.normalise().c));

	for (const r of rationalDivisors(pFactors, qFactors, bound)) {
		let	i		= p.degree();
		let acc1	= p.c[i];
		let acc2	= p.c[i];
		for (let denPow = r.den; i--; denPow *= r.den) {
			const c = p.c[i] * denPow;
			acc1 = acc1 *  r.num + c;
			acc2 = acc2 * -r.num + c;
		}

		if (acc1 === 0) {
			roots.push(r);
			if (removeMultiplicity(p, r))
				break;
		}
		if (acc2 === 0) {
			const r2 = r.neg();
			roots.push(r2);
			if (removeMultiplicity(p, r2))
				break;
		}
	}
	return roots.sort((a, b) => a.compare(b));
}

// modifies p to remove found roots
/*
function rationalRootsNB(p: polynomialB): bigint[] {
	const roots: bigint[] = [];
	if (p.c[0] === 0n)
		roots.push(0n);

	while (p.c[0] === 0n)
		p.c = divRootB(p.c, 0n)[0];

	const a0		= p.c[0];
	const factors	= new factorisationB(a0);
	const bound		= lagrangeImprovedB(p.c.slice(0, -1));

	for (const x of factors.divisors(bound)) {
		if (p.evaluate(x) === 0n) {
			roots.push(x);
			if (removeMultiplicityB(p, x))
				break;
		}
		if (p.evaluate(-x) === 0n) {
			roots.push(-x);
			if (removeMultiplicityB(p, -x))
				break;
		}
	}
	return roots.sort(compare);
}
*/
// modifies p to remove found roots
function rationalRootsB(p: polynomialB): rationalB[] {
	const roots: rationalB[] = [];
	if (p.c[0] === 0n)
		roots.push(rationalB.from(0n))                                  ;

	while (p.c[0] === 0n)
		p.c = divRootB(p.c, 0n)[0];

	const a0 = Big.abs(p.c[0]);
	const an = Big.abs(p.leadCoeff());

	const pFactors	= new factorisationB(a0);
	const qFactors	= new factorisationB(an);
	const bound 	= realBoundT(p.normalise());
	const bound2	= rationalB.from(+Gen.max(bound.min.neg(), bound.max));

	for (const r of rationalDivisorsB(pFactors, qFactors, bound2)) {
		let	i		= p.degree();
		let acc1	= p.c[i];
		let acc2	= p.c[i];
		for (let denPow = r.den; i--; denPow *= r.den) {
			const c = p.c[i] * denPow;
			acc1 = acc1 *  r.num + c;
			acc2 = acc2 * -r.num + c;
		}

		if (acc1 === 0n) {
			roots.push(r);
			if (removeMultiplicityB(p, r))
				break;
		}
		if (acc2 === 0n) {
			const r2 = r.neg();
			roots.push(r2);
			if (removeMultiplicityB(p, r2))
				break;
		}
	}
	return roots.sort((a, b) => a.compare(b));
}

//-----------------------------------------------------------------------------
//	Interpolation polynomials
//-----------------------------------------------------------------------------

export function interpolate<T extends PolyNTypes>(points: [T, T][], one?: T): Polynomial<T> {
	if (points.length === 0)
		return Polynomial<T>([]);
	if (typeof points[0][0] === 'number')
		return interpolateN(points as [number, number][]) as Polynomial<T>;
	
	if (!one && isScalar(points[0][0]))
		one = points[0][0].from(1);

	return interpolateT(points as [any, any][], one);
}

// interpolate polynomial (monomial basis) from points (x,y)
function interpolateN(points: [number, number][]): Polynomial<number> {
	const res = new polynomial([]);
	for (const [xi, yi] of points) {
		// basis polynomial Li with value 1 at xi and 0 at others
		let L		= new polynomial([1]);
		let denom	= 1;
		for (const [xj, _yj] of points) {
			if (xi !== xj) {
				L		= L.mul(new polynomial([-xj, 1]));
				denom	*= xi - xj;
			}
		}
		if (Math.abs(denom) > 1e-18)
			res.selfAdd(L.scale(yi / denom));
	}
	return res;
}

function interpolateT<T extends ops<T>>(points: [T, T][], one: T): Polynomial<T> {
	const res = new polynomialT<T>([]);
	for (const [xi, yi] of points) {
		// basis polynomial Li with value 1 at xi and 0 at others
		let L		= new polynomialT<T>([one]);
		let denom	= one;
		for (const [xj, _yj] of points) {
			if (xi !== xj) {
				L		= L.mul(new polynomialT<T>([xj.neg(), one]));
				denom	= denom.mul(xi.sub(xj));
			}
		}
		res.selfAdd(L.scale(yi.div(denom)));
	}
	return res;
}

//-----------------------------------------------------------------------------
//	Resultant and Discriminants
//-----------------------------------------------------------------------------

// Build Sylvester matrix for two polynomials p and q
export function sylvesterMatrix<T>(p: T[], q: T[], zero: T) {
	const m = p.length - 1;
	const n = q.length - 1;
	const size = m + n;
	const rows = Array.from({ length: size }, () => Array.from({ length: size }, () => zero));

	// p rows: n rows
	for (let r = 0; r < n; r++) {
		for (let i = 0; i <= m; i++)
			rows[r][r + i] = p[i] ?? zero;
	}

	// q rows: m rows
	for (let r = 0; r < m; r++) {
		for (let i = 0; i <= n; i++)
			rows[n + r][r + i] = q[i] ?? zero;
	}

	return rows;
}

export function resultant<T extends number | scalar<any>>(p: Polynomial<T>, q: Polynomial<T>): T {
	return polyOf(p, isNumber)
		? resultantN(p as Polynomial<number>, q as Polynomial<number>) as T
		: resultantT(p as Polynomial<scalar<any>>, q as Polynomial<scalar<any>>) as T;
}
export function discriminant<T extends number | scalar<any>>(p: Polynomial<T>) {
	return polyOf(p, isNumber)
		? discriminantN(p as Polynomial<number>) as T
		: discriminantT(p as Polynomial<scalar<any>>) as T;
}

function resultantN(p: Polynomial<number>, q: Polynomial<number>): number {
	const pd = p.degree();
	const qd = q.degree();

	if (pd < 0 || qd < 0)
		return 0;

	if (pd + qd === 0)
		return 1;

	const M = sylvesterMatrix(p.c, q.c, 0);
	const n = M.length;
	const { swaps } = LUDecomposeBareiss(M, true);
	return swaps & 1 ? -M[n - 1][n - 1] : M[n - 1][n - 1];
}

// discriminant = (-1)^{n(n-1)/2} * (1/leadCoeff) * resultant(p, p')
function discriminantN(p: Polynomial<number>) {
	const	n	= p.degree();
	if (n < 1)
		return 0;
	const	R	= resultantN(p, p.deriv());
	return (((n * (n - 1))) & 2 ? -R : R) / p.leadCoeff();
}

function resultantT<T extends scalar<T>>(p: Polynomial<T>, q: Polynomial<T>): T {
	const pd = p.degree();
	const qd = q.degree();
	const zero = p.c[0].from(0);

	if (pd < 0 || qd < 0)
		return zero;

	if (pd + qd === 0)
		return p.c[0].from(1);

	const M = sylvesterMatrix(p.c, q.c, zero);
	const n = M.length;
	const { swaps } = LUDecomposeBareissT(M, true);
	return swaps & 1 ? M[n - 1][n - 1].neg() : M[n - 1][n - 1];
}

function discriminantT<T extends scalar<T>>(p: Polynomial<T>) {
	const	n	= p.degree();
	if (n < 1)
		return p.c[0].from(0);
	const	R	= resultantT(p, p.deriv());
	return (((n * (n - 1))) & 2 ? R.neg() : R).div(p.leadCoeff());
}

//-----------------------------------------------------------------------------
// polynomial factorization via vieta's formulas
//-----------------------------------------------------------------------------

/**
 * Generate Vieta's formulas for a polynomial of given degree
 * Returns equations relating roots to coefficients
 * 
 * For degree n with roots r₁, r₂, ..., rₙ and coefficients c₁, c₂, ..., cₙ:
 * - σ₁ = r₁ + r₂ + ... + rₙ = -c₁
 * - σ₂ = r₁r₂ + r₁r₃ + ... = c₂
 * - σ₃ = r₁r₂r₃ + ... = -c₃
 * - ...
 * - σₙ = r₁r₂...rₙ = (-1)ⁿcₙ
 */
export function vietasFormulas<T extends scalar<T>>(poly: polynomialT<T>, roots: T[]): T[] {
	const n = roots.length;
	if (n !== poly.c.length)
		throw new Error('Number of roots must match number of coefficients');
	
	const equations: T[] = [];
	
	// Generate elementary symmetric polynomials σₖ
	for (let k = 1; k <= n; k++) {
		let sigma = poly.c[0].from(0);
		
		// Sum over all k-element subsets of roots
		const indices = Array.from({ length: k }, (_, i) => i);
		
		function generateSubsets(start: number, depth: number) {
			if (depth === k) {
				// Multiply the k roots at these indices
				let product = roots[indices[0]];
				for (let i = 1; i < k; i++)
					product = product.mul(roots[indices[i]]);
				sigma = sigma.add(product);
				return;
			}
			
			for (let i = start; i <= n - (k - depth); i++) {
				indices[depth] = i;
				generateSubsets(i + 1, depth + 1);
			}
		}
		
		generateSubsets(0, 0);
		
		// σₖ = (-1)ᵏ⁺¹ × cₖ  (alternating signs)
		const sign = (k % 2 === 0) ? 1 : -1;
		equations.push(sigma.sub(poly.c[k - 1].scale(sign)));
	}
	
	return equations;
}

//-----------------------------------------------------------------------------
//	Legendre polynomial and roots
//-----------------------------------------------------------------------------

export function legendrePolynomial(n: number): Polynomial<number> {
	let P0 = new polynomial([1]);
	if (n === 0)
		return P0;

	let P1 = new polynomial([0, 1]);

	for (let k = 2; k <= n; ++k) {
		const x = new polynomial([0, 1]); // x
		const term1 = x.mul(P1).scale(2 * k - 1);
		const term2 = P0.scale(k - 1);
		const Pk = term1.sub(term2).scale(1 / k);

		P0 = P1;
		P1 = Pk;
	}

	return P1;
}

export function legendreTable(n: number): [number, number][] {
	const P		= legendrePolynomial(n);
	const dP	= P.deriv();
	const roots = P.realRoots(); // returns x_i in [-1, 1]

	return roots.map(x => {
		const dp = dP.evaluate(x);
		return [x, 2 / ((1 - x * x) * dp * dp)];
	});

}
