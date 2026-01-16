import { scalar, hasop, canop } from './core';
import { Polynomial, PolynomialN, PolyTypes, PolyNTypes, coeffOps, sparseClean, integerPolynomial, squareFreeRationalRoots } from './polynomial';
import { Matrix, makeVec, makeMat, LUDecomposeBareiss, LUDecomposeBareissT, LUSolveBareiss, LUSolveBareissT, bareissOps, computeMinimalPolynomial, permute, copyFillHolesVec, similarityTransform0 } from './linear';
import real from './real';
import integer from './integer';
import rational, { rationalT, canMakeRationalOps } from './rational';
import gen, { Mod, ModFactory } from './gen';

export type _Factor<T> = {
	factor:			T;
	multiplicity:	number;
}

export class Extension<T> {
	constructor(public alpha: Polynomial<Polynomial<T>>, public poly: Polynomial<T>) {}
	toString() {
		return `alpha: ${this.alpha}, poly: ${this.poly}`;
	}
}

export type MaybeExtension<T>	= Extension<T> | Polynomial<Polynomial<T>>;

export type Factor<T> 		= _Factor<Polynomial<T>>;
export type FactorExt<T> 	= _Factor<MaybeExtension<T>>;

export type factorOps<T>	= coeffOps<T> & hasop<'recip'|'sign'|'from'>;
export type factorType		= number | factorOps<any>;

export type PolyMod<T>			= Mod<Polynomial<T>> & { shift(n: number): PolyMod<T>; };
export type PolyModFactory<T>	= (new (v: Polynomial<T>) => PolyMod<T>) & { wrap(p: Polynomial<T>): PolyMod<T> };

export function PolyModFactory<T>(r: Polynomial<T>): PolyModFactory<T> {
	class M extends ModFactory(r) {
		shift(n: number) { return this.fix(this.v.shift(n)); }
	};
	return M as any;
}


/*
export function polyGCD<T extends PolyTypes>(A: Polynomial<T>, B: Polynomial<T>): Polynomial<T> {
	return gen.gcd(A, B);
}

function polyGCDT<T extends coeffOps<T>>(A: Polynomial<T>, B: Polynomial<T>) {
	const seq = subresultantPRST(A, B);
	for (let i = seq.length; i--;) {
		if (seq[i].degree() >= 0)
			return seq[i].rscale(seq[i].leadCoeff());//normalise();
	}
	return A.from(1);
}

// Build a simple subresultant PRS sequence using pseudoRemainder.
function subresultantPRST<T extends coeffOps<T>>(A: Polynomial<T>, B: Polynomial<T>) {
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
*/

//-----------------------------------------------------------------------------
//	squareFreeFactorization
//-----------------------------------------------------------------------------

export function squareFreeFactorization<T>(f0: Polynomial<T>|PolynomialN<T>): Factor<T>[] {
	const res: Factor<T>[] = [];
	if (f0.degree() <= 0)
		return res;

	let w = f0.unmonic().dup();
	let g = w.deriv();

	g = gen.gcd(w, g);
	if (g.degree() === 0) {
		res.push({ factor: w, multiplicity: 1 });
		return res;
	}
	w = w.pseudoDivmod(g);

	for (let multiplicity = 1; multiplicity < 256 && w.degree() > 0; multiplicity++) {
		const y			= gen.gcd(w, g);
		const factor	= w.pseudoDivmod(y);
		
		if (factor.degree() > 0) {
			const cont = factor.content();
			if (cont)
				factor.selfRscale(cont);
			res.push({ factor, multiplicity });
		}

		w = y;
		g = g.pseudoDivmod(y);
	}
	return res;
}

export function factorOverQ<T extends factorType>(f: Polynomial<T>|PolynomialN<T>) {
	function split({factor, multiplicity}: Factor<T>) {
		const factors: Factor<number>[] = [];
		const ipoly = integerPolynomial(factor);
		const roots = squareFreeRationalRoots(ipoly);

		for (const r of roots)
			factors.push({factor: Polynomial([-Number(r.num), Number(r.den)]), multiplicity});

		if (ipoly.degree() > 0)
			factors.push({factor: ipoly.map(c => Number(c)), multiplicity});

		return factors;
	}

	return squareFreeFactorization(f).map(p => split(p)).flat();
}

//-----------------------------------------------------------------------------
//  Factorization over extension field K
//-----------------------------------------------------------------------------

export function matrixMinimalPolynomial<T extends factorType>(M: Matrix<T>): PolynomialN<T> | undefined {
	const zero = gen.from(M.c[0][0], 0);
	const alpha = computeMinimalPolynomial(function*(): Generator<T[]> {
		for (let p = M.from(1).fillHoles(zero); ; p = p.mul(M))
			yield p.c.flat();
	});
	if (alpha)
		return PolynomialN(alpha as any) as PolynomialN<T>;
}

function minimalPolynomial<T extends factorType>(M: Polynomial<T>, X: Polynomial<T>): PolynomialN<T> | undefined {
	const N		= M.degree();
	const e		= PolyModFactory(M).wrap(X);
	const zero	= gen.zero(M.leadCoeff());
	const alpha = computeMinimalPolynomial(function*(): Generator<T[]> {
		yield [gen.from(M.leadCoeff(), 1)];
		for (let m = 1, p = e; m <= N; m++, p = p.mul(e))
			yield copyFillHolesVec(p.v.c, p.v.c.length, zero);//.slice();
	});

	if (alpha)
		return PolynomialN(alpha.map(a => gen.neg(a) as any));
}

// Factor a square-free polynomial S over K into irreducible factors over K
function splitSquareFreeOverK<T extends factorType>(S: Polynomial<T>, coeffsToTry: T[] = []): Polynomial<T> | undefined {

	const N = S.degree();
	if (N <= 1)
		return;

	const one = gen.from(S.leadCoeff(), 1);

	// compute minimal polynomial of multiplication operator (deterministic)
	const lcm = minimalPolynomial(S, Polynomial(makeVec(N, one)));
	if (!lcm)
		return;

	// gcd between S and lcm may reveal a nontrivial factor
	const g = gen.gcd(S, lcm.unmonic());
	if (g.degree() > 0 && g.degree() < S.degree())
		return g;

	// If operator minimal polynomial did not split S, try per-basis minimal polynomials (annihilators of basis elements)
	// These can reveal nontrivial factors that the operator polynomial misses
	for (let idx = 0; idx < N; idx++) {
		const m = minimalPolynomial(S, Polynomial([one]).shift(idx));
		if (m) {
			const g = gen.gcd(S, m.unmonic());
			if (g.degree() > 0 && g.degree() < S.degree())
				return g;
		}
	}

	// Try small deterministic linear combinations of basis elements as multiplication elements in A = K[x]/(S)

	// generate combinations with 1 non-zero coefficients
	for (let i = 0; i < N; i++) {
		for (const a of coeffsToTry) {
			const m = minimalPolynomial(S, Polynomial([a]).shift(i));
			if (m) {
				const g = gen.gcd(S, m.unmonic());
				if (g.degree() > 0 && g.degree() < S.degree())
					return g;
			}
		}
	}

	// generate combinations 2 non-zero coefficients
	for (let i = 0; i < N; i++) {
		for (let j = i + 1; j < N; j++) {
			for (const a of coeffsToTry) {
				for (const b of coeffsToTry) {
					const vec: T[] = [];
					vec[i] = a;
					vec[j] = b;
					const m = minimalPolynomial(S, Polynomial(vec));
					if (m) {
						const g = gen.gcd(S, m.unmonic());
						if (g.degree() > 0 && g.degree() < S.degree())
							return g;
					}
				}
			}
		}
	}
}

export function factorOverK<T extends factorType>(f: Polynomial<T>, coeffsToTry: T[] = []): Factor<T>[] {
	function recurse(S: Polynomial<T>, coeffsToTry: T[] = []): Polynomial<T>[] {
		const split = splitSquareFreeOverK(S, coeffsToTry);
		return split
			? recurse(split).concat(recurse(S.dup().divmod(split)))
			: [S];
	}

	const res: Factor<T>[] = [];
	for (const i of squareFreeFactorization(f)) {
		const pieces = recurse(i.factor, coeffsToTry);
		for (const p of pieces)
			res.push({ factor: p, multiplicity: i.multiplicity });
	}
	return res;
}

//-----------------------------------------------------------------------------
//	Factor Over K_Q
//-----------------------------------------------------------------------------

// Compute E_i matrices for multiplication-by-t^i projected into subspace spanned by ns.
// build E_i = T^{-1} * (multiplication-by-t^i) * T for i=0..r-1

function computeEis(R: Polynomial<number>, NS: Matrix<number>, NSinv: Matrix<number>, perm: number[], rank: number): Matrix<number>[] {
	const r		= R.degree();

	return Array.from({length: r}, (_, t) =>
		Matrix(NS.c.map(ns => {
			const B = makeVec(NS.rows, 0);
			for (const i in ns) {
				const bv	= ns[i];
				const row	= Math.floor(+i / r) * r;
				const rem	= Polynomial([1]).shift(+i % r + t);
				rem.divmod(R);

				for (const j in rem.c)
					B[row + +j] = rem.c[j] * bv;
			}
		
			return LUSolveBareiss(NSinv.c, permute(B, perm), rank) ?? makeVec(NS.cols, 0);
		}))
	);
}

export function factorSquareFreeOverK_I(S: Polynomial<Polynomial<number>>, R: Polynomial<number>): MaybeExtension<number>[] {
	S = S.scale(R.leadCoeff());

	let s = S.degree();
	const r = R.degree();
	if (s <= 1 || r <= 0)
		return [S];

	const ModR = PolyModFactory(R);
	const SmodR = S.map(i => ModR.wrap(i));
	s = SmodR.degree();
	if (s < 0)
		return [S];

	const leadMod = SmodR.leadCoeff();
	const n = s * r;
	const Mc = makeMat(n, n, 0);
	const M = Matrix(Mc);

	// For each basis element t^i x^j (col = j*r + i) compute x * (t^i x^j)
	for (let j = 0; j < s - 1; j++) {
		const base = j * r;
		for (let i = 0; i < r; i++) {
			const col = Mc[base + i];
			const row = base + r;
			const rem = leadMod.shift(i);
			for (const ti in rem.v.c)
				col[row + +ti] = rem.v.c[ti];
		}
	}

	const ScoeffsMod = SmodR.c.slice(0, -1);
	for (let i = 0; i < r; i++) {
		const col = Mc[(s - 1) * r + i];
		for (const k in ScoeffsMod) {
			const row = +k * r;
			const rem = ScoeffsMod[k].neg().shift(i);
			for (const ti in rem.v.c)
				col[row + +ti] = rem.v.c[ti];
		}
	}

	const char = PolynomialN(M.characteristic());
	const factors = factorOverQ(char).map(f => f.factor);

	for (const factor of factors) {
		const d = factor.degree();

		if (d === 1) {
			const root = Polynomial([factor.c[0]]);
			const S2 = S.dup();
			if (S2.syntheticDiv(root).degree() < 0)
				return [Polynomial([root.neg(), Polynomial([1])])];
			continue;
		}

		const P = factor.evaluate(M);
		const NS = P.nullspace();
		if (NS.cols === 0)
			break;

		const NSinv = NS.dup();
		const { perm: NSperm, rank: NSrank } = LUDecomposeBareiss(NSinv.c);

		const Ei = computeEis(R, NS, NSinv, NSperm, NSrank);
		const M_sub = similarityTransform0(M, NS, NSinv, NSperm, NSrank);

		const Fmats = Array<Matrix<number>>(d * r);
		for (let u = 0, t = M_sub.from(1); u < d; u++, t = t.mul(M_sub)) {
			for (let i = 0; i < r; i++)
				Fmats[u * r + i] = t.mul(Ei[i]);
		}

		const m = NS.cols;
		const nvars = d * r;
		const eqRows: number[][] = [];
		const eqB: number[] = [];

		for (let p = 0; eqB.length < nvars && p < m; p++) {
			for (let q = 0; eqB.length < nvars && q < m; q++) {
				eqRows.push(Array.from({ length: nvars }, (_, j) => Fmats[j].c[p][q]));
				eqB.push(M_sub.c[p][q]);
			}
		}

		const { perm, rank } = LUDecomposeBareiss(eqRows);
		const sol = LUSolveBareiss(eqRows, permute(eqB, perm), rank);
		if (!sol)
			break;

		const alpha = Polynomial(sparseClean(Array.from({ length: d }, (_, u) => ModR.wrap(Polynomial(sparseClean(sol.slice(u * r, (u + 1) * r))))));
		const ModZ = ModFactory(factor.map(c => ModR.wrap(Polynomial([c]))));
		const quot = SmodR.map(c => ModZ.wrap(Polynomial([c])));
		const rem = quot.syntheticDiv(ModZ.wrap(alpha));

		if (rem.v.degree() < 0 && quot.degree() >= S.degree() - 1) {
			if (R.eq(factor) || (alpha.degree() === 0 && quot.c.every(i => i.v.degree() <= 0))) {
				const x = Polynomial([0, 1]);
				const λ = alpha.map(i => i.v).evaluate(x);
				const S1 = Polynomial([λ.neg(), Polynomial([1])]);
				return [
					...factorSquareFreeOverK_I(S1, R),
					...factorSquareFreeOverK_I(S.div(S1), R)
				];
			}
			// Otherwise, this is an extension factor: return as Extension
			return [new Extension(alpha.map(c => c.v), factor)];
		}
	}

	return [S];
}

// Public API: factor a polynomial S over K = Q[t]/(R).
export function factorOverK_I(S: Polynomial<Polynomial<number>>, R: Polynomial<number>): FactorExt<number>[] {
	const sfR	= squareFreeFactorization(R);
	const Rrad	= sfR.length === 0 ? R.dup() : sfR.reduce((acc, f) => acc.mul(f.factor), Polynomial([1]));
	const sfS	= squareFreeFactorization(S);
	const result = [];

	for (const {factor, multiplicity} of sfS) {
		const pieces = factorSquareFreeOverK_I(factor, Rrad);
		for (const p of pieces)
			result.push({factor: p, multiplicity});
	}
	return result;
}

function maybeExtensionToRational(f: MaybeExtension<number>, den: number) : MaybeExtension<rational> {
	return 'alpha' in f
		? {alpha: f.alpha.map(c => c.map(c => rational(c, den))), poly: f.poly.map(c => rational(c, den))}
		: f.map(c => c.map(c => rational(c, den)));
}

export function factorSquareFreeOverK_Q(S: Polynomial<Polynomial<rational>>, R: Polynomial<rational>): MaybeExtension<rational>[] {
//	const den = real.lcm(...S.c.map(c => c.c.map(c => c.den)).flat(), ...R.c.map(c => c.den));
	return factorSquareFreeOverK_I(
		S.map(c => integerPolynomial(c)),
		integerPolynomial(R)
	).map(f => maybeExtensionToRational(f, 1));
}

// Public API: factor a polynomial S over K = Q[t]/(R).
// Wrapper over factorOverK_I: converts to integers, factors, then converts back.
export function factorOverK_Q(S: Polynomial<Polynomial<rational>>, R: Polynomial<rational>): FactorExt<rational>[] {
//	const den = real.lcm(...S.c.map(c => c.c.map(c => c.den)).flat(), ...R.c.map(c => c.den));
	return factorOverK_I(
		S.map(c => integerPolynomial(c)),
		integerPolynomial(R)
	).map(f => ({ factor: maybeExtensionToRational(f.factor, 1), multiplicity: f.multiplicity}));
}

//-----------------------------------------------------------------------------
//	Interpolation polynomials
//-----------------------------------------------------------------------------

// interpolate polynomial (monomial basis) from points (x,y)
function interpolateN(points: [number, number][]): Polynomial<number> {
	const res = Polynomial([0]);
	for (const i of points) {
		// basis polynomial Li with value 1 at xi and 0 at others
		let L		= Polynomial([1]);
		let denom	= 1;
		for (const j of points) {
			if (j === i)
				break;
			if (i[0] !== j[0]) {
				L		= L.mul(Polynomial([-j[0], 1]));
				denom	*= i[0] - j[0];
			}
		}
		if (Math.abs(denom) > 1e-18)
			res.selfAdd(L.scale(i[1] / denom));
	}
	return res;
}

function interpolateT<T extends coeffOps<T> & hasop<'sign'>>(points: [T, T][], one: T): Polynomial<T> {
	const res = Polynomial<T>([one.scale(0)]);
	for (const i of points) {
		// basis polynomial Li with value 1 at xi and 0 at others
		let L		= Polynomial([one]);
		let denom	= one;
		for (const j of points) {
			if (j === i)
				break;
			const d = i[0].sub(j[0]);
			if (d.sign()) {
				L		= L.mul(Polynomial([j[0].neg(), one]));
				denom	= denom.mul(d);
			}
		}
		res.selfAdd(L.scale(i[1].div(denom)));
	}
	return res;
}

export function interpolate<T extends PolyNTypes>(points: [T, T][], one?: T): Polynomial<T> {
	if (points.length === 0)
		return Polynomial<T>([]);
	if (typeof points[0][0] === 'number')
		return interpolateN(points as [number, number][]) as any;
	
	if (!one && hasop('from')(points[0][0]))
		one = points[0][0].from(1);

	return interpolateT(points as [any, any][], one);
}

//-----------------------------------------------------------------------------
//	Resultant and Discriminants
//-----------------------------------------------------------------------------

export function sylvesterMatrix<T>(polys: Polynomial<T>[], zero: T) {
	const size = polys.reduce((a, b) => a + b.degree(), 0);
	if (size === 0 || polys.length < 2)
		return [] as T[][];

	const M = makeMat(size, (polys.length - 1) * size, zero);

	let rowOffset = 0;
	for (const p of polys) {
		const blockRows = size - p.degree();
		for (let r = 0; r < blockRows; r++) {
			const row = M[rowOffset + r];
			for (const k in p.c)
				row[r + +k] = p.c[k];
		}
		rowOffset += blockRows;
	}
	return M;
}


// Generic wrapper for resultant: dispatch to numeric or generic variant
export function resultant<T extends PolyNTypes & canop<'from'>>(p: Polynomial<T>, q: Polynomial<T>): T|undefined {
	const pd = p.degree();
	const qd = q.degree();

	if (pd >= 0 || qd >= 0) {
		const lead = p.leadCoeff();
		if (pd + qd === 0)
			return gen.from(lead, 1);
		const M = sylvesterMatrix([p, q], gen.from(lead, 0));
		const n = M.length;
		const { swaps } = real.is(lead) ? LUDecomposeBareiss(M as number[][]) : LUDecomposeBareissT(M as any);
		return gen.negate(M[n - 1][n - 1], !!(swaps & 1));
	}
}

// discriminant = (-1)^{n(n-1)/2} * (1/leadCoeff) * resultant(p, p')
export function discriminant<T extends number | scalar<any>>(p: Polynomial<T>): T|undefined {
	const	n	= p.degree();
	if (n > 0) {
		const	R	= resultant(p, p.deriv());
		if (R)
			return gen.div(gen.negate(R, !!(((n * (n - 1))) & 2)), p.leadCoeff());
	}
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
		const indices = Array.from({ length: k }, (_, i) => i);

		// Build sigma by accumulating products over all k-element subsets of roots
		let sigma = poly.c[0].from(0);
		function generateAndAccumulate(start: number, depth: number) {
			if (depth === k) {
				let product = roots[indices[0]];
				for (let i = 1; i < k; i++)
					product = product.mul(roots[indices[i]]);
				sigma = sigma.add(product);
				return;
			}
			for (let i = start; i <= n - (k - depth); i++) {
				indices[depth] = i;
				generateAndAccumulate(i + 1, depth + 1);
			}
		}

		generateAndAccumulate(0, 0);

		// σₖ = (-1)^{k+1} × c_k  (alternating signs)
		const ck = poly.c[k - 1];
		equations.push((k % 2 === 0) ? sigma.sub(ck) : sigma.add(ck));
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

type Term<T extends PolyTypes> = {
	root:		T;
	residue:	T;
}

export function partialFractions(r: rationalT<Polynomial<number>>): { polyPart: Polynomial<number>; terms: Term<number>[] } | undefined {
	if (r.den.degree() <= 0)
		return undefined;

	const rem		= r.num.dup();
	const _polyPart	= rem.divmod(r.den);
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
	return { polyPart: _polyPart, terms };
}

type TermT<T extends scalar<T>> = {
	factor: Polynomial<T>;
	order: number;
	numer: Polynomial<T>
};


// Generic exact partial-fraction decomposition using maths primitives.
// Decomposes numer/denom over scalar type T into polynomial part and partial-fraction terms with polynomial numerators for each factor power.
export function partialFractionsT<T extends scalar<T> & hasop<'recip'>>(r: rationalT<Polynomial<T>>): { polyPart: Polynomial<T>; terms: TermT<T>[] } | undefined {
	if (r.den.degree() <= 0)
		return undefined;

	const rem		= r.num.dup();
	const _polyPart	= rem.divmod(r.den);
	const factors	= squareFreeFactorization(r.den);
	const zero		= r.den.c[0].from(0);

	const A: T[][] = [];

	for (const {factor, multiplicity} of factors) {
		const d = factor.degree();
		for (let k = 1; k <= multiplicity; k++) {
			const fPower		= gen.ipow(factor, k);
			const denomWithout	= r.den.scale(fPower.leadCoeff()).divmod(fPower);
			for (let c = 0; c < d; c++) {
				// multiply by x^c and apply scaling: shift coefficients
				A.push(denomWithout.shift(c).c);
			}
		}
	}

	// solve A * x = b over T using Bareiss routines
	const { perm, rank } = LUDecomposeBareissT(A);
	const solVec = LUSolveBareissT(A, permute(rem.c, perm), rank);
	if (!solVec)
		return undefined;

	// map back to polynomial numerators, using original non-monic factors
	const terms: TermT<T>[] = [];
	let idx = 0;
	for (const {factor, multiplicity} of factors) {
		const d = factor.degree();
		for (let k = 1; k <= multiplicity; k++) {
			const numer = Polynomial(Array.from({ length: d }, () => solVec[idx++] ?? zero));
			if (numer.c.some(c => c.sign() !== 0))
				terms.push({ factor, order: k, numer });
		}
	}
	return { polyPart: _polyPart, terms };
}

// modifies r
export function partialPower<T extends scalar<T> & hasop<'recip'>>(r: rationalT<Polynomial<T>>): number {
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


//-----------------------------------------------------------------------------
//  eigenvalues
//-----------------------------------------------------------------------------

// Exact eigensolver using characteristic polynomial factorization.
// Returns eigenspaces with their minimal polynomials and exact rational eigenvectors.
// For an eigenvalue λ that is a root of factor f(x), all eigenvectors span the nullspace of f(M).

export function eigenVectorForPolynomial<T extends bareissOps<T> & hasop<'from'>>(A: T[][], eigenvalue: Polynomial<T>) {
	return eigenvalue.evaluate(Matrix(A)).nullspace();
}

export function eigenvaluesExact(A: number[][]): Factor<number>[] {
	if (A.length === 0)
		return [];

	const charPoly	= Matrix(A).characteristic();
	const den		= Number(real.commonDenominator(charPoly));
	return factorOverQ(Polynomial([...charPoly.map(ak => ak * den), den]));
}


export function eigenvaluesExactT<T extends canMakeRationalOps<T> & hasop<'sign'>>(A: T[][]): Factor<number>[] {
	if (A.length === 0)
		return [];
	
	const charPoly	= Matrix(A).characteristic();
	const den		= Number(gen.commonDenominator(charPoly));
	const denT		= A[0][0].from(den);
	return factorOverQ(Polynomial([...charPoly.map(ak => +ak.mul(denT)), den]));
}
