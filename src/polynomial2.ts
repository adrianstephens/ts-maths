import { Polynomial, PolyTypes, PolyNTypes } from './polynomial';
import { ops, scalar, has, isScalar } from './core';
import real from './real';
import integer from './integer';
import Gen from './gen';

import { LUDecomposeBareiss, LUDecomposeBareissT, LUSolveBareissTransposeMultiT, solveRectangularBareissT } from './bareiss';

//-----------------------------------------------------------------------------
//	PRS: Polynomial Remainder Sequence and GCD
//-----------------------------------------------------------------------------

export function polyGCD<T extends PolyNTypes>(A: Polynomial<T>, B: Polynomial<T>): Polynomial<T> {
	return A.is(real.is)
		? Gen.gcd(A, B)
		: polyGCDT(A as Polynomial<any>, B as Polynomial<any>) as Polynomial<T>;
}

function polyGCDT<T extends ops<T>>(A: Polynomial<T>, B: Polynomial<T>) {
	const seq = subresultantPRST(A, B);
	for (let i = seq.length; i--;) {
		if (seq[i].degree() >= 0)
			return seq[i].rscale(seq[i].leadCoeff());//normalise();
	}
	// No non-constant gcd found -> return constant 1 of the appropriate coefficient type
	// Prefer using the constant-term coefficient as a sample for constructing '1'
	const sample = (A.c.length ? A.c[0] : (B.c.length ? B.c[0] : undefined)) as any;
	if (sample !== undefined) {
		if (typeof sample === 'number')
			return Polynomial<T>([1 as any]);
		if (typeof sample === 'bigint')
			return Polynomial<T>([1n as any]);
		if (sample && typeof sample.from === 'function')
			return Polynomial<T>([(sample as any).from(1)]);
	}
	// Last resort: return numeric 1 which callers may coerce as needed
	return Polynomial<T>([1 as any]);
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
		if (cont)
			A.selfRscale(cont);

		seq.push(A.dup());
		[A, B] = [B, A];
	}
	return seq;
}

type Multiplicity<T extends PolyNTypes> = {
	factor:			Polynomial<T>;
	multiplicity:	number;
}
export function squareFreeFactorization<T extends PolyNTypes>(f: Polynomial<T>): Multiplicity<T>[] {
	return f.is(real.is)
		? squareFreeFactorizationN(f) as { factor: Polynomial<T>, multiplicity: number }[]
		: squareFreeFactorizationT(f as Polynomial<any>);
}


// Return array of { factor, multiplicity } for a polynomial f using primitive PRS gcd
function squareFreeFactorizationN(f: Polynomial<number>) {
	const res: Multiplicity<number>[] = [];
	if (f.degree() <= 0)
		return res;

	let g = polyGCD(f, f.deriv());
	// ensure g is primitive (remove content) so subsequent divmod/pseudo-division behaves
	const gcont = g.content();
	if (gcont)
		g.selfRscale(gcont);
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
	// If no factors were recorded but f is non-constant, then f is square-free
	// and irreducible by the PRS method used here; return f as a single factor.
	if (res.length === 0 && f.degree() > 0)
		res.push({ factor: f.dup(), multiplicity: 1 });
	return res;
}

// Return array of { factor, multiplicity } for a polynomial f using primitive PRS gcd
function squareFreeFactorizationT<T extends ops<T>>(f: Polynomial<T>) {
	const res: Multiplicity<T>[] = [];
	if (f.degree() <= 0)
		return res;

	let g = polyGCDT(f, f.deriv());
	if (g.degree() === 0) {
		res.push({ factor: f.dup(), multiplicity: 1 });
		return res;
	}

	let w = f.dup().divmod(g);

	for (let i = 1; i < 256 && w.degree() > 0; i++) {
		const y = polyGCDT(w, g);
		const fi = w.divmod(y);
		// diagnostic logging removed
		
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
	const res = Polynomial([]);
	for (const [xi, yi] of points) {
		// basis polynomial Li with value 1 at xi and 0 at others
		let L		= Polynomial([1]);
		let denom	= 1;
		for (const [xj, _yj] of points) {
			if (xi !== xj) {
				L		= L.mul(Polynomial([-xj, 1]));
				denom	*= xi - xj;
			}
		}
		if (Math.abs(denom) > 1e-18)
			res.selfAdd(L.scale(yi / denom));
	}
	return res;
}

function interpolateT<T extends ops<T>>(points: [T, T][], one: T): Polynomial<T> {
	const res = Polynomial<T>([]);
	for (const [xi, yi] of points) {
		// basis polynomial Li with value 1 at xi and 0 at others
		let L		= Polynomial([one]);
		let denom	= one;
		for (const [xj, _yj] of points) {
			if (xi !== xj) {
				L		= L.mul(Polynomial([xj.neg(), one]));
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
	return p.is(real.is)
		? resultantN(p as Polynomial<number>, q as Polynomial<number>) as T
		: resultantT(p as Polynomial<scalar<any>>, q as Polynomial<scalar<any>>) as T;
}
export function discriminant<T extends number | scalar<any>>(p: Polynomial<T>) {
	return p.is(real.is)
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
export function vietasFormulas<T extends scalar<T>>(poly: Polynomial<T>, roots: T[]): T[] {
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
		equations.push((k % 2 === 0) ? sigma.sub(poly.c[k - 1]) : sigma.add(poly.c[k - 1]));
	}
	
	return equations;
}

//-----------------------------------------------------------------------------
// Partial-fraction helper (numeric, simple case)
//-----------------------------------------------------------------------------
/**
 * Partial-fraction decomposition for one-variable rational functions with numeric coefficients when the denominator has only distinct real linear factors.
 * Returns the polynomial part and an array of terms { root, residue } such that
 *
 *   P(x)/Q(x) = polyPart(x) + sum_r residue_r / (x - root_r)
 *
 * Returns `undefined` when the denominator is not separable into distinct real linear factors or when numeric roots cannot be obtained.
 */

export type RationalPolynomial<T extends PolyTypes> = {
	num: Polynomial<T>;
	den: Polynomial<T>;
}

type Term<T extends PolyTypes> = {
	root:		T;
	residue:	T;
}

export function partialFractions(r: RationalPolynomial<number>): { polyPart: Polynomial<number>; terms: Term<number>[] } | undefined {
	if (r.den.degree() <= 0)
		return undefined;

	const rem		= r.num.dup();
	const polyPart	= rem.divmod(r.den);
	const roots		= r.den.realRoots();
	if (!roots || roots.length !== r.den.degree())
		return undefined;

	const terms: { root: number; residue: number }[] = [];

	const dden		= r.den.deriv();
	for (const r of roots) {
		const denomDer = dden.evaluate(r);
		if (Math.abs(denomDer) < 1e-12)
			return undefined;
		terms.push({ root: r, residue: rem.evaluate(r) / denomDer });
	}
	return { polyPart, terms };
}

type TermT<T extends scalar<T>> = {
	factor: Polynomial<T>;
	order: number;
	numer: Polynomial<T>
};


// Generic exact partial-fraction decomposition using maths primitives.
// Decomposes numer/denom over scalar type T into polynomial part and partial-fraction terms with polynomial numerators for each factor power.
export function partialFractionsT<T extends scalar<T> & has<'recip'>>(r: RationalPolynomial<T>): { polyPart: Polynomial<T>; terms: TermT<T>[] } | undefined {
	const rows = r.den.degree();
	if (rows === 0)
		return undefined;

	const rem		= r.num.dup();
	const polyPart	= rem.divmod(r.den);
	const _rden		= r.den.dup();
	const factors	= squareFreeFactorization(_rden);

	// build unknowns
	const unknowns: { factorIndex: number; power: number; coeffIndex: number }[] = [];
	for (let i = 0; i < factors.length; i++) {
		const d = factors[i].factor.degree();
		for (let k = 1; k <= factors[i].multiplicity; k++) {
			for (let c = 0; c < d; c++)
				unknowns.push({ factorIndex: i, power: k, coeffIndex: c });
		}
	}

	const cols = unknowns.length;
	const zero = r.den.c[0].from(0);
	
	// build A matrix (rows x cols) and b vector
	const A = Array.from({ length: rows }, () => Array.from({ length: cols }, () => zero));
	const b = Array.from({ length: rows }, (_, i) => rem.c[i] ?? zero);
	for (let col = 0; col < cols; col++) {
		const u = unknowns[col];
		let denomWithout = r.den.dup();
		for (let t = 0; t < u.power; t++)
			denomWithout = denomWithout.divmod(factors[u.factorIndex].factor);
		// multiply by x^coeffIndex: shift coefficients
		const contribCoeffs = Array.from({ length: u.coeffIndex }, () => zero).concat(denomWithout.c.map(c => c));
		for (let row = 0; row < rows; row++)
			A[row][col] = contribCoeffs[row] ?? zero;
	}

	// solve A * x = b over T using Bareiss routines
	let solVec: T[];
	if (rows === cols) {
		const Amat = A.map(r => r.slice());
		const { perm } = LUDecomposeBareissT(Amat, true);

		for (let i = 0; i < cols; i++)
			if (Amat[i][i].sign() === 0)
				return undefined;

		const res	= LUSolveBareissTransposeMultiT(Amat, [b.map(x => x)], perm) as T[][];
		if (res.length !== 1 || res[0].length !== cols)
			return undefined;
		solVec = res[0];
	} else {
		const sol = solveRectangularBareissT(A, [b]);
		if (!sol)
			return undefined;
		solVec = sol[0];
	}

	// map back to polynomial numerators
	const terms: TermT<T>[] = [];
	let idx = 0;
	for (const fac of factors) {
		const d = fac.factor.degree();
		for (let k = 1; k <= fac.multiplicity; k++)
			terms.push({ factor: fac.factor, order: k, numer: Polynomial(Array.from({ length: d }, () => solVec[idx++] ?? zero)) });
	}
	return { polyPart, terms };
}

// modifies r
export function partialPower<T extends scalar<T> & has<'recip'>>(r: RationalPolynomial<T>): number {
	// try substitution when denominator is a polynomial in x^d and numerator matches du/dx pattern
	const degs = Object.keys(r.den.c).map(i => +i as integer).filter(i => r.den.c[i].sign() !== 0);
		
	if (degs.length > 0) {
		const g = integer.gcd(...degs);
		if (g >= 2) {
			// try smallest divisor >1
			for (let d = 2; d <= g; d++) {
				if (g % d)
					continue;
				// check numerator is single-term at power d-1
				if (r.num.c.every((c, i) => (i === d - 1) === (c.sign() !== 0))) {
					const k = r.num.c[d - 1];
					const reduceDen: T[] = [];
					degs.filter(i => (i % d) === 0).forEach(i => reduceDen[i / d] = r.den.c[i]);
					r.num = Polynomial([k.div(k.from(d))]);
					r.den = Polynomial(reduceDen);
					return d;
/*
					const res = partialFractionsT({
						num: Polynomial([k.div(k.from(d))]),
						den: Polynomial(reduceDen)
					});
					if (res)
						return { ...res, power: d };
*/
				}
			}
		}
	}
	return 1;
}

export function hermiteReduce<T extends scalar<T> & has<'recip'>>(r: RationalPolynomial<T>, one: T): { polyPart: Polynomial<T>; derivative: RationalPolynomial<T>[]; remainder: RationalPolynomial<T> } | undefined {
	const pf = partialFractionsT(r);
	if (!pf)
		return undefined;

	const { polyPart, terms } = pf;
	const _rden		= r.den.dup();
	const factors = squareFreeFactorization(_rden);

	let termIdx = 0;
	const zero	= one.scale(0);
	const derivativeTerms:		RationalPolynomial<T>[] = [];
	const remainderNumerators:	Polynomial<T>[] = [];

	for (const facInfo of factors) {
		const f = facInfo.factor;
		const m = facInfo.multiplicity;
		const M: Polynomial<T>[] = [];
		for (let j = 1; j <= m; j++) {
			const t = terms[termIdx++];
			M.push(t ? t.numer : Polynomial(Array.from({ length: f.degree() }, () => zero)));
		}
		if (m === 1) {
			remainderNumerators.push(M[0]);
			continue;
		}

		const d		= f.degree();
		const rows	= d * m;
		const cols	= m * d;

		const bCoeffs: T[] = Array.from({ length: rows }, () => zero);
		for (let j = 1; j <= m; j++) {
			const prod = m - j ? M[j - 1].mul(Gen.ipow(f, m - j)) : M[j - 1];
			for (let r0 = 0; r0 < prod.c.length; r0++)
				bCoeffs[r0] = (bCoeffs[r0] ?? zero).add(prod.c[r0]);
		}

		const A		= Array.from({ length: rows }, () => Array.from({ length: cols }, () => zero));
		const fDer	= f.deriv();
		const fPows: Polynomial<T>[] = [Polynomial([one]), f];
		for (let j = 2; j <= m; j++)
			fPows[j] = fPows[j - 1].mul(f);

		for (let k = 1; k <= m - 1; k++) {
			for (let t = 0; t < d; t++) {
				const basis		= Polynomial(Array.from({ length: t }, () => zero).concat([one]));
				const contrib	= basis.deriv().mul(fPows[m - k]).sub(basis.mul(fDer).mul(fPows[m - k - 1]).scale(one.scale(k)));
				const colIdx	= (k - 1) * d + t;
				for (let row = 0; row < rows; row++)
					A[row][colIdx] = contrib.c[row] ?? zero;
			}
		}

		for (let t = 0; t < d; t++) {
			const basis		= Polynomial(Array.from({ length: t }, () => zero).concat([one]));
			const contrib	= basis.mul(fPows[m - 1]);
			const colIdx	= (m - 1) * d + t;
			for (let row = 0; row < rows; row++)
				A[row][colIdx] = contrib.c[row] ?? zero;
		}

		const sol = solveRectangularBareissT(A, [bCoeffs]);
		if (!sol) {
			remainderNumerators.push(M[0]);
			continue;
		}

		const xvec = sol[0];
		const Nk: Polynomial<T>[] = [];
		for (let k = 1; k <= m - 1; k++)
			Nk.push(Polynomial(Array.from({ length: d }, (_, t) => xvec[(k - 1) * d + t]) ?? zero));

		const D = Polynomial(Array.from({ length: d }, (_, t) => xvec[(m - 1) * d + t]) ?? zero);

		for (let k = 1; k <= m - 1; k++)
			derivativeTerms.push({ num: Nk[k - 1], den: Gen.ipow(f, k) });

		remainderNumerators.push(D);
	}

	// assemble S and R
	let S = Polynomial([one]);
	for (const facInfo of factors)
		S = S.mul(facInfo.factor);

	let R = Polynomial([zero]);
	for (let i = 0; i < factors.length; i++) {
		const _other = S.divmod(factors[i].factor);
		if (remainderNumerators[i])
			R = R.add(remainderNumerators[i].mul(_other));
	}

	return { polyPart: polyPart, derivative: derivativeTerms, remainder: { num: R, den: S } };
}
