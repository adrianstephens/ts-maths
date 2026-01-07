import { scalar, hasop, isScalar, arithmeticOps } from './core';
import { Polynomial, PolynomialN, PolyTypes, PolyNTypes, coeffOps, sparseClean } from './polynomial';
import { Matrix, makeVec, makeMat, LUDecomposeBareiss, LUDecomposeBareissT, LUSolveBareiss, LUSolveBareissT, nullspaceN, nullspaceT, bareissOps, projectSubspaceN, projectSubspaceT, characteristicT, characteristicN } from './linear';
import real from './real';
import integer from './integer';
import rational, { rationalT, canMakeRationalOps } from './rational';
import gen, { Mod, ModFactory } from './gen';

export type Factor<T> = {
	factor:			Polynomial<T>;
	multiplicity:	number;
}

export type Extension<T>	= { alpha: Polynomial<PolyMod<T>>; poly: Polynomial<number> };

export type factorOps<T>	= coeffOps<T> & hasop<'recip'|'sign'|'from'>;
export type factorType		= number | factorOps<any>;

export type PolyMod<T> = Mod<Polynomial<T>> & { shift(n: number): PolyMod<T>; };
export type PolyModFactory<T> = (new (v: Polynomial<T>) => PolyMod<T>) & { wrap(p: Polynomial<T>): PolyMod<T> };
export function PolyModFactory<T>(r: Polynomial<T>): PolyModFactory<T> {
	class M extends ModFactory(r) {
		shift(n: number) { return this.fix(this.v.shift(n)); }
	};
	return M as any;
}


//-----------------------------------------------------------------------------
//	PRS: Polynomial Remainder Sequence and GCD
//-----------------------------------------------------------------------------

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

export function squareFreeFactorization<T extends PolyTypes>(f: Polynomial<T>): Factor<T>[] {
	const res: Factor<T>[] = [];
	if (f.degree() <= 0)
		return res;

	let g = gen.gcd(f, f.deriv());
	if (g.degree() === 0) {
		res.push({ factor: f.dup(), multiplicity: 1 });
		return res;
	}

	let w = f.dup().pseudoDivmod(g);

	for (let i = 1; i < 256 && w.degree() > 0; i++) {
		const y		= gen.gcd(w, g);
		const fi	= w.pseudoDivmod(y);
        
		// only record non-constant factors
		if (fi.degree() > 0) {
			const cont = fi.content();
			if (cont)
				fi.selfRscale(cont);
			res.push({ factor: fi, multiplicity: i });
		}

		// w <- y, g <- g / y
		w = y;
		g = g.pseudoDivmod(y);
	}
	return res;
}

//-----------------------------------------------------------------------------
//  eigenvalues
//-----------------------------------------------------------------------------

export interface eigenPairExact<T> {
	factor:			Polynomial<number>;  // Minimal polynomial defining eigenvalue
	multiplicity:	number;
	vectors:		T[][];  // Exact eigenvectors
}

// Exact eigensolver using characteristic polynomial factorization.
// Returns eigenspaces with their minimal polynomials and exact rational eigenvectors.
// For an eigenvalue λ that is a root of factor f(x), all eigenvectors span the nullspace of f(M).

export function eigenVectorForPolynomial<T extends bareissOps<T> & hasop<'from'>>(A: T[][], eigenvalue: Polynomial<T>) {
	const M = Matrix(A);
	const P = eigenvalue.evaluate(M);
	return nullspaceT(P.c);
}

export function eigenPairsExact(A: number[][]): eigenPairExact<number>[] {
	if (A.length === 0)
		return [];
	
	const charPoly	= characteristicN(A);
	const den		= Number(real.commonDenominator(charPoly));
	const sf		= squareFreeFactorization(Polynomial([...charPoly.map(ak => ak * den), den]));
	const factors: Factor<number>[] = [];

	for (const f of sf) {
		if (f.factor.degree() > 0) {
			const roots = f.factor.rationalRoots();
			if (roots.length)
				factors.push(...roots.map(r => ({factor: Polynomial([-r.num, r.den]), multiplicity: f.multiplicity})));
			else
				factors.push(f);
		}
	}

	const result: eigenPairExact<number>[] = [];
	const M = Matrix(A);
	
	for (const fac of factors) {
		const P = fac.factor.evaluate(M);
		const vectors = nullspaceN(P.c);
		if (vectors.length > 0)
			result.push({ factor: fac.factor, multiplicity: fac.multiplicity, vectors });
	}
	
	return result;
}


export function eigenPairsExactT<T extends canMakeRationalOps<T> & hasop<'sign'>>(A: T[][]): eigenPairExact<T>[] {
	if (A.length === 0)
		return [];
	
	const charPoly	= characteristicT(A);
	const den		= Number(gen.commonDenominator(charPoly));
	const denT		= A[0][0].from(den);
	const sf		= squareFreeFactorization(Polynomial([...charPoly.map(ak => +ak.mul(denT)), den]));
	const factors: Factor<number>[] = [];

	for (const f of sf) {
		if (f.factor.degree() > 0) {
			const roots = f.factor.rationalRoots();
			if (roots.length)
				factors.push(...roots.map(r => ({factor: Polynomial([ -Number(r), 1 ]), multiplicity: f.multiplicity})));
			else
				factors.push(f);
		}
	}
	const result: eigenPairExact<T>[] = [];
	const M = Matrix(A);
	
	for (const fac of factors) {
		const P = fac.factor.evaluate(M);
		const vectors = nullspaceT(P.c);
		if (vectors.length > 0)
			result.push({ factor: fac.factor, multiplicity: fac.multiplicity, vectors });
	}
	
	return result;
}

//-----------------------------------------------------------------------------
//  Factorization over extension field K
//-----------------------------------------------------------------------------

function minimalPolynomial<T extends factorType>(M: Polynomial<T>, X: Polynomial<T>): PolynomialN<T>|undefined {

	const x		= M.leadCoeff();
	const zero	= gen.from(x, 0);

	// produce sequence v_k = X^k mod S
	const mod	= PolyModFactory(M);
	const N		= M.degree();

	const cols: T[][] = [
		[]
	];
	cols[0][0] = gen.from(x, 1);

	const e = mod.wrap(X);

	// find smallest m such that cols[0..m-1] are linearly dependent with cols[m]
	for (let m = 1, p = e; m <= N; m++, p = p.mul(e)) {
		cols.push(p.v.c.slice());

		const A = Array.from({ length: N }, (_, row) => Array.from({ length: m }, (_, col) => cols[col][row] ?? zero));
		const b = Array.from({ length: N }, (_, row) => cols[m][row] ?? zero);

		if (real.is(x)) {
			const { perm } = LUDecomposeBareiss(A as number[][]);
			const alpha = LUSolveBareiss(A as number[][], b as number[], perm);
			if (alpha)
				return PolynomialN(alpha.map(a => -a)) as any;
		} else {
			const { perm } = LUDecomposeBareissT(A as factorOps<any>[][]);
			const alpha = LUSolveBareissT(A as factorOps<any>[][], b as factorOps<any>[], perm);
			if (alpha)
				return PolynomialN(alpha.map(a => a.neg())) as any;
		}
		
	}
}

function fromPolynomialN<T extends PolyTypes>(n: PolynomialN<T>, one: T) {
	return Polynomial([...n.c, one]);
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
	const g = gen.gcd(S, fromPolynomialN(lcm, one));
	if (g.degree() > 0 && g.degree() < S.degree())
		return g;

	// If operator minimal polynomial did not split S, try per-basis minimal polynomials (annihilators of basis elements)
	// These can reveal nontrivial factors that the operator polynomial misses
	for (let idx = 0; idx < N; idx++) {
		const m = minimalPolynomial(S, Polynomial([one]).shift(idx));
		if (m) {
			const g = gen.gcd(S, fromPolynomialN(m, one));
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
				const g = gen.gcd(S, fromPolynomialN(m, one));
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
						const g = gen.gcd(S, fromPolynomialN(m, one));
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
// build E_i = T^{-1} * (multiplication-by-t^i) * T for i=0..dr-1

function computeEisT<T extends arithmeticOps<T> & hasop<'sign'|'from'|'mul'|'add'|'recip'>>(ns: T[][], R: Polynomial<T>, zero: T, one: T): Matrix<T>[] {
	const r		= R.degree();
	const n		= Math.max(...ns.map(i => i.length));
	const m		= ns.length;
	const E		= Array.from({ length: r }, () => Matrix(makeMat(m, m, zero)));
	const T		= Array.from({ length: n }, (_, row) => Array.from({ length: m }, (_, col) => ns[col][row] ?? zero));
	const { perm } = LUDecomposeBareissT(T);

	for (let t = 0; t < r; t++) {
		const BT = makeMat(m, n, zero);
		for (let col = 0; col < m; col++) {
			for (const c2 in ns[col]) {
				const rem = Polynomial([one]).shift(+c2 % r + t);
				rem.divmod(R);

				const bv		= ns[col][c2];
				const baseRow	= Math.floor(+c2 / r) * r;
				for (const ti in rem.c) {
					const row	= baseRow + +ti;
					BT[row][col] = BT[row][col].add(rem.c[ti].mul(bv));
				}
			}
		}
		for (let k = 0; k < m; k++) {
			const xcol = LUSolveBareissT(T, BT.map(rw => rw[k]), perm);
			if (xcol) {
				for (let i = 0; i < m; i++)
					E[t].c[i][k] = xcol[i];
			}
		}
	}
	return E;
}


function computeEisN(nsCols: number[][], r: Polynomial<number>): Matrix<number>[] {
	const dr	= r.degree();
	const N		= Math.max(...nsCols.map(i => i.length));
	const m		= nsCols.length;
	const E		= Array.from({ length: dr }, () => Matrix(makeMat(m, m, 0)));
	const T		= Array.from({ length: N }, (_, row) => Array.from({ length: m }, (_, col) => nsCols[col][row] ?? 0));
	const { perm } = LUDecomposeBareiss(T);

	for (let t = 0; t < dr; t++) {
		const BT = makeMat(m, N, 0);
		for (let col = 0; col < m; col++) {
			for (const c2 in nsCols[col]) {
				const rem = Polynomial([1]).shift(+c2 % dr + t);
				rem.divmod(r);

				const bv		= nsCols[col][c2];
				const baseRow	= Math.floor(+c2 / dr) * dr;
				for (const ti in rem.c) {
					const row	= baseRow + +ti;
					BT[row][col] = BT[row][col] + rem.c[ti] * bv;
				}
			}
		}
		for (let k = 0; k < m; k++) {
			const xcol = LUSolveBareiss(T, BT.map(rw => rw[k]), perm);
			if (xcol) {
				for (let i = 0; i < m; i++)
					E[t].c[i][k] = xcol[i];
			}
		}
	}
	return E;
}


// Q-linear deterministic factorisation:
// treat A = K[x]/(S) as a Q-vector space with basis { t^i x^j } and find a factor of multiplication-by-x over Q using exact rational arithmetic

function findFactorOverK_Q(S: Polynomial<Polynomial<rational>>, R: Polynomial<rational>): Polynomial<Polynomial<rational>> | Extension<rational> | undefined {
	let		s	= S.degree();
	const	r	= R.degree();
	if (s <= 1 || r <= 0)
		return;

	const ModR	= PolyModFactory(R);
	const SmodR = S.map(i => ModR.wrap(i));
	s	= SmodR.degree();
	if (s < 0)
		return;

	const Sconst	= S.c.every(c => c.degree() <= 0) && S.map(c => c.c[0]);

	const invLeadMod = SmodR.leadCoeff().recip();
	if (!invLeadMod)
		throw new Error('Non-invertible leading coefficient in S when constructing multiplication matrix');

	const q0	= rational.from(0);
	const q1	= rational.from(1);
	const n		= s * r;
	const M		= makeMat(n, n, q0);

	// For each basis element t^i x^j (col = j*r + i) compute x * (t^i x^j)
	for (let j = 0; j < s - 1; j++) {
		const base = j * r;
		for (let i = 0; i < r; i++)
			M[base + i][base + r + i] = q1;  // x * basis[colIdx] = basis[rowIdx]
	}

	const ScoeffsMod = SmodR.c.slice(0, -1);
	// Build multiplication-by-x matrix M for the K-vector space K[x]/(S)
	// where K = Q[t]/(R). The basis is {t^i x^j} for i=0..r-1, j=0..s-1.
	for (let i = 0; i < r; i++) {
		const col = (s - 1) * r + i;	// input: basis element t^i x^{s-1}
		// x^{n} reduction: x^n = - Slead^{-1} * sum_{k=0..n-1} Scoeffs[k] x^k
		for (const k in ScoeffsMod) {
			const rem = ScoeffsMod[k].mul(invLeadMod).neg().shift(i);
			const rowbase = +k * r;
			for (const ti in rem.v.c) {
				const row = rowbase + +ti;
				M[col][row] = M[col][row].add(rem.v.c[ti]);
			}
		}
	}

	const achar		= characteristicT(M);
	const lcmDen	= real.lcm(...achar.map(x => x.den));
	const char		= Polynomial([...achar.map(ak => ak.num * (lcmDen / ak.den)), lcmDen]);
	const factors	= squareFreeFactorization(char);

	// Try adjoin-root extraction: for each integer factor p(x) of the characteristic polynomial, adjoin a root ζ of p and attempt to find α ∈ K[ζ] with x - α | S in K[ζ][x].
	for (const f of factors) {
		const factor	= f.factor;
		const d			= factor.degree();
		if (d <= 0)
			continue;

		const roots	= factor.rationalRoots();
		if (roots.length) {
			const factor = Polynomial([Polynomial([rational(-roots[0].num)]), Polynomial([rational(roots[0].den)])]);
			S.c = S.div(factor).c;
			return factor;
		}

		const rfactor	= factor.map(c => rational(c));
		const matM	= Matrix(M);
		const P		= rfactor.evaluate(matM);
		const ns	= nullspaceT(P.c);
		if (!ns || ns.length === 0)
			continue;

		// build T_rows from basis ns
		// compute M_sub = T^{-1} M T (use shared helper which handles column-major M)
		const M_sub = Matrix(projectSubspaceT(M, ns, q0));

		// build E_i = T^{-1} * (multiplication-by-t^i) * T for i=0..dr-1 (centralized helper)
		const Ei = computeEisT(ns, R, q0, q1);

		// build F_{u,i} = M_sub^u * E[i]  where F_{u,i} represents ζ^u t^i
		const Fmats: Matrix<rational>[][] = [];
		for (let u = 0, t = M_sub.from(rational(1)); u < d; u++, t = t.mul(M_sub))
			Fmats[u] = Array.from({ length: r }, (_, i) => t.mul(Ei[i]));

		// M_sub represents x in the nullspace. We want α = Σ α_{u,i} ζ^u t^i ∈ K[ζ] such that M_sub = Σ α_{u,i} F_{u,i}.
		// This is a linear system. We need d·r equations for d·r unknowns. Use the first d·r entries of M_sub (flattened) to form a square system.
		const m			= ns.length;
		const nvars 	= d * r;
		const eqRows:	rational[][] = [];
		const eqB:		rational[] = [];
		
		// Take first nvars matrix entries to form square system
		let count = 0;
		outer: for (let p = 0; p < m; p++) {
			for (let q = 0; q < m; q++) {
				const row = Array<rational>(nvars);
				for (let u = 0; u < d; u++) {
					for (let i = 0; i < r; i++)
						row[u * r + i] = Fmats[u][i].c[p][q];
				}
				eqRows.push(row);
				eqB.push(M_sub.c[p][q]);
				if (++count >= nvars)
					break outer;
			}
		}

		const { perm }		= LUDecomposeBareissT(eqRows);
		const solVec		= LUSolveBareissT(eqRows, eqB, perm);
		if (!solVec)
			continue;

		const alpha			= Polynomial(sparseClean(Array.from({length: d}, (_, u) => ModR.wrap(Polynomial(sparseClean(Array.from({length: r}, (_, i) => solVec[u * r + i])))))));
		const ModZ			= ModFactory(rfactor.map(ci => ModR.wrap(Polynomial([ci]))));
		const quot			= SmodR.map(c => ModZ.wrap(Polynomial([c])));
		const rem			= quot.syntheticDiv(ModZ.wrap(alpha));

		// check remainder zero: need to verify all coefficients are zero
		if (rem.v.degree() < 0) {
			// Dividing degree-s polynomial by linear factor should yield degree s-1 quotient; if quotient degree is too small, this is a degenerate/trivial factorization
			if (quot.degree() < s - 1)
				continue;

			// Check if quotient and alpha both live in K (degree 0 in ζ), or if p(ζ) matches R(t)
			if (R.eq(rfactor) || (alpha.degree() === 0 && quot.c.every(i => i.v.degree() <= 0))) {
				const t	= ModR.wrap(Polynomial([q0, q1]));
				S.c		= quot.map(pz => pz.v.evaluate(t).v).c;
				return Polynomial([alpha.evaluate(t).v.neg(), Polynomial([q1])]);
			}
			
			// Check if this is a degenerate extension: rfactor is a scalar multiple of S (as polynomials in x) (we're just adjoining a root of S, which is uninformative for factorization)
			if (Sconst && rfactor.scale(Sconst.leadCoeff()).eq(Sconst.scale(rfactor.leadCoeff())))
				continue;

			// Genuinely in extension: return Extension with alpha and modulus
			return { alpha, poly: factor };
		}
	}
}

export function factorSquareFreeOverK_Q(S: Polynomial<Polynomial<rational>>, R: Polynomial<rational>): (Polynomial<Polynomial<rational>>|Extension<rational>)[] {
	const result = [];
	S = S.dup();
/*
	// First, extract any rational roots if all coefficients are constant
	if (S.degree() > 1 && S.c.every(c => c.degree() <= 0)) {
		const Sconst	= S.map(c => c.c[0]);
		const roots		= Sconst.rationalRoots();
		if (roots.length) {
			for (const r of roots) {
				result.push(Polynomial([Polynomial([rational(-r.num)]), Polynomial([rational(r.den)])]));
				Sconst.syntheticDiv(r);
			}
			S = Sconst.map(c => Polynomial([c]));
		}
	}
*/
	// Then, try to factor any remaining higher-degree polynomial over the extension
	for (let factor; (factor = findFactorOverK_Q(S, R)); ) {
		if ('alpha' in factor) {
			result.push(factor);
			break;
		} else {
			result.push(factor);
			if (S.degree() <= 0)
				break;
		}
	}
	if (S.degree() > 0)
		result.push(S);
	return result;
}

// Public API: factor a polynomial S over K = Q[t]/(R).
export function factorOverK_Q(S: Polynomial<Polynomial<rational>>, R: Polynomial<rational>): (Polynomial<Polynomial<rational>>|Extension<rational>)[] {
	const sfR	= squareFreeFactorization(R);
	const Rrad	= sfR.length === 0 ? R.dup() : sfR.reduce((acc, f) => acc.mul(f.factor), Polynomial([rational.from(1)]));
	const sfS	= squareFreeFactorization(S);
	const result = [];

	for (const item of sfS) {
		const pieces = factorSquareFreeOverK_Q(item.factor, Rrad);
		for (const p of pieces) {
			for (let k = 0; k < item.multiplicity; k++)
				result.push(p);
		}
	}
	return result;
}


//-----------------------------------------------------------------------------
//	Factor Over K_I
//-----------------------------------------------------------------------------

function findFactorOverK_I(S: Polynomial<Polynomial<number>>, R: Polynomial<number>): Polynomial<Polynomial<number>> | Extension<number> | undefined {
	let		s	= S.degree();
	const	r	= R.degree();
	if (s <= 1 || r <= 0)
		return;

	const ModR	= PolyModFactory(R);
	const SmodR = S.map(i => ModR.wrap(i));
	s	= SmodR.degree();
	if (s < 0)
		return;

	const Sconst	= S.c.every(c => c.degree() <= 0) && S.map(c => c.c[0]);
	const leadMod	= SmodR.leadCoeff();
	const n		= s * r;
	const M		= makeMat(n, n, 0);

	// For each basis element t^i x^j (col = j*r + i) compute x * (t^i x^j)
	for (let j = 0; j < s - 1; j++) {
		const base = j * r;
		for (let i = 0; i < r; i++) {
			// scale-by-lead: we represent the operator c_s·x to avoid division by c_s.
			// multiplication by the lead coefficient (a polynomial in t) acts on t^i via modulo R.
			const remLead = leadMod.v.shift(i);
			remLead.divmod(R);
			for (const ti in remLead.c) {
				const row = base + r + +ti;
				M[base + i][row] += remLead.c[ti];
			}
		}
	}

	const ScoeffsMod = SmodR.c.slice(0, -1);
	for (let i = 0; i < r; i++) {
		const col = (s - 1) * r + i;	// input: basis element t^i x^{s-1}
		// x^{n} reduction: x^n = - Slead^{-1} * sum_{k=0..n-1} Scoeffs[k] x^k
		for (const k in ScoeffsMod) {
			const rem = ScoeffsMod[k].neg().shift(i);
			const rowbase = +k * r;
			for (const ti in rem.v.c) {
				const row = rowbase + +ti;
				M[col][row] += rem.v.c[ti];
			}
		}
	}

	const achar		= characteristicN(M);
	// Clear denominators to obtain integer polynomial for factoring
	const char		= Polynomial([...achar, 1]);
	const factors	= squareFreeFactorization(char);

	// Try adjoin-root extraction: for each integer factor p(x) of the characteristic polynomial, adjoin a root ζ of p and attempt to find α ∈ K[ζ] with x - α | S in K[ζ][x].
	for (const f of factors) {
		const factor	= f.factor;
		const d			= factor.degree();
		if (d <= 0)
			continue;

		const roots	= factor.rationalRoots();
		if (roots.length) {
			const factor = Polynomial([Polynomial([-roots[0]]), Polynomial([1])]);
			S.c = S.div(factor).c;
			return factor;
		}

		const P		= factor.evaluate(Matrix(M));
		const ns	= nullspaceN(P.c);
		if (!ns || ns.length === 0)
			continue;

		// build T_rows from basis ns
		// compute M_sub = T^{-1} M T (use shared helper which handles column-major M)
		const M_sub = Matrix(projectSubspaceN(M, ns));

		// build E_i = T^{-1} * (multiplication-by-t^i) * T for i=0..dr-1 (centralized helper)
		const Ei = computeEisN(ns, R);

		// build F_{u,i} = M_sub^u * E[i]  where F_{u,i} represents ζ^u t^i
		const Fmats: Matrix<number>[][] = [];
		for (let u = 0, t = M_sub.from(1); u < d; u++, t = t.mul(M_sub))
			Fmats[u] = Array.from({ length: r }, (_, i) => t.mul(Ei[i]));

		// M_sub represents x in the nullspace. We want α = Σ α_{u,i} ζ^u t^i ∈ K[ζ] such that M_sub = Σ α_{u,i} F_{u,i}.
		// This is a linear system. We need d·r equations for d·r unknowns. Use the first d·r entries of M_sub (flattened) to form a square system.
		const m			= ns.length;
		const nvars 	= d * r;
		const eqRows:	number[][] = [];
		const eqB:		number[] = [];
		
		// Take first nvars matrix entries to form square system
		let count = 0;
		outer: for (let p = 0; p < m; p++) {
			for (let q = 0; q < m; q++) {
				const row = Array<number>(nvars);
				for (let u = 0; u < d; u++) {
					for (let i = 0; i < r; i++)
						row[u * r + i] = Fmats[u][i].c[p][q];
				}
				eqRows.push(row);
				eqB.push(M_sub.c[p][q]);
				if (++count >= nvars)
					break outer;
			}
		}

		const { perm }		= LUDecomposeBareiss(eqRows);
		const solVec		= LUSolveBareiss(eqRows, eqB, perm);
		if (!solVec)
			continue;

		const alpha			= Polynomial(sparseClean(Array.from({length: d}, (_, u) => ModR.wrap(Polynomial(sparseClean(Array.from({length: r}, (_, i) => solVec[u * r + i])))))));
		const ModZ			= ModFactory(factor.map(ci => ModR.wrap(Polynomial([ci]))));
		const quot			= SmodR.map(c => ModZ.wrap(Polynomial([c])));
		const rem			= quot.syntheticDiv(ModZ.wrap(alpha));

		// check remainder zero: need to verify all coefficients are zero
		if (rem.v.degree() < 0) {
			// Dividing degree-s polynomial by linear factor should yield degree s-1 quotient; if quotient degree is too small, this is a degenerate/trivial factorization
			if (quot.degree() < s - 1)
				continue;

			// Check if quotient and alpha both live in K (degree 0 in ζ), or if p(ζ) matches R(t)
			if (R.eq(factor) || (alpha.degree() === 0 && quot.c.every(i => i.v.degree() <= 0))) {
				const t	= ModR.wrap(Polynomial([0, 1]));
				S.c		= quot.map(pz => pz.v.evaluate(t).v).c;
				return Polynomial([alpha.evaluate(t).v.neg(), Polynomial([1])]);
			}
			
			// Check if this is a degenerate extension: rfactor is a scalar multiple of S (as polynomials in x) (we're just adjoining a root of S, which is uninformative for factorization)
			if (Sconst && factor.scale(Sconst.leadCoeff()).eq(Sconst.scale(factor.leadCoeff())))
				continue;

			// Genuinely in extension: return Extension with alpha and modulus
			return { alpha, poly: factor };
		}
	}
}


export function factorSquareFreeOverK_I(S: Polynomial<Polynomial<number>>, R: Polynomial<number>): (Polynomial<Polynomial<number>>|Extension<number>)[] {
	const result = [];
	S = S.dup();
	for (let factor; (factor = findFactorOverK_I(S, R)); ) {
		if ('alpha' in factor) {
			result.push(factor);
			break;
		} else {
			result.push(factor);
			if (S.degree() <= 0)
				break;
		}
	}
	if (S.degree() > 0)
		result.push(S);
	return result;
}

// Public API: factor a polynomial S over K = Q[t]/(R).
export function factorOverK_I(S: Polynomial<Polynomial<number>>, R: Polynomial<number>): (Polynomial<Polynomial<number>>|Extension<number>)[] {
	const sfR	= squareFreeFactorization(R);
	const Rrad	= sfR.length === 0 ? R.dup() : sfR.reduce((acc, f) => acc.mul(f.factor), Polynomial([1]));
	const sfS	= squareFreeFactorization(S);
	const result = [];

	for (const item of sfS) {
		const pieces = factorSquareFreeOverK_I(item.factor, Rrad);
		for (const p of pieces) {
			for (let k = 0; k < item.multiplicity; k++)
				result.push(p);
		}
	}
	return result;
}

//-----------------------------------------------------------------------------
//	Interpolation polynomials
//-----------------------------------------------------------------------------

export function interpolate<T extends PolyNTypes>(points: [T, T][], one?: T): Polynomial<T> {
	if (points.length === 0)
		return Polynomial<T>([]);
	if (typeof points[0][0] === 'number')
		return interpolateN(points as [number, number][]) as any;
	
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

function interpolateT<T extends coeffOps<T>>(points: [T, T][], one: T): Polynomial<T> {
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
export function resultant<T extends PolyNTypes>(p: Polynomial<T>, q: Polynomial<T>): T|undefined {
	const pd = p.degree();
	const qd = q.degree();

	if (pd >= 0 || qd >= 0) {
		const lead = p.leadCoeff();
		if (pd + qd === 0)
			return gen.from(lead as any, 1);
		const M = sylvesterMatrix([p, q], gen.from(lead as any, 0));
		const n = M.length;
		const { swaps } = real.is(lead) ? LUDecomposeBareiss(M as any) : LUDecomposeBareissT(M as any);;
		return swaps & 1 ? gen.neg(M[n - 1][n - 1]) : M[n - 1][n - 1];
	}
}

// discriminant = (-1)^{n(n-1)/2} * (1/leadCoeff) * resultant(p, p')
export function discriminant<T extends number | scalar<any>>(p: Polynomial<T>): T|undefined {
	const	n	= p.degree();
	if (n > 0) {
		const	R	= resultant(p, p.deriv());
		if (R)
			return gen.div((((n * (n - 1))) & 2 ? gen.neg(R) : R), p.leadCoeff());
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
	const rows = r.den.degree();
	if (rows <= 0)
		return undefined;

	const rem		= r.num.dup();
	const _polyPart	= rem.divmod(r.den);
	const factors	= squareFreeFactorization(r.den);
	const zero		= r.den.c[0].from(0);

	const A: T[][] = [];

	for (const f of factors) {
		const d = f.factor.degree();
		for (let k = 1; k <= f.multiplicity; k++) {
			for (let c = 0; c < d; c++) {
				//unknowns.push({ factorIndex: i, power: k, coeffIndex: c });
				const fPower		= gen.ipow(f.factor, k);
				const denomWithout	= r.den.scale(fPower.leadCoeff()).divmod(fPower);
				
				// multiply by x^coeffIndex and apply scaling: shift coefficients
				const col = makeVec(rows, zero);
				for (const i in denomWithout.c)
					col[c + +i] = denomWithout.c[i];
				A.push(col);
			}
		}
	}

	// solve A * x = b over T using Bareiss routines
	const { perm } = LUDecomposeBareissT(A);
	const solVec = LUSolveBareissT(A, Array.from({ length: rows }, (_, i) => rem.c[i] ?? zero), perm);
	if (!solVec)
		return undefined;

	// map back to polynomial numerators, using original non-monic factors
	const terms: TermT<T>[] = [];
	let idx = 0;
	for (const fac of factors) {
		const d = fac.factor.degree();
		for (let k = 1; k <= fac.multiplicity; k++) {
			const numer = Polynomial(Array.from({ length: d }, () => solVec[idx++] ?? zero));
			if (numer.c.some(c => c.sign() !== 0))
				terms.push({ factor: fac.factor, order: k, numer });
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

export function hermiteReduce<T extends scalar<T> & hasop<'recip'>>(r: rationalT<Polynomial<T>>, one: T): { polyPart: Polynomial<T>; derivative: rationalT<Polynomial<T>>[]; remainder: rationalT<Polynomial<T>> } | undefined {
	const pf = partialFractionsT(r);
	if (!pf)
		return undefined;

	const { polyPart, terms } = pf;
	const factors	= squareFreeFactorization(r.den.dup());

	let termIdx = 0;
	const zero	= one.scale(0);
	const derivativeTerms:		rationalT<Polynomial<T>>[] = [];
	const remainderNumerators:	Polynomial<T>[] = [];

	for (const facInfo of factors) {
		const f = facInfo.factor;
		const m = facInfo.multiplicity;

		const M: Polynomial<T>[] = [];
		for (let j = 1; j <= m; j++) {
			const t = terms[termIdx++];
			M.push(t ? t.numer : Polynomial(makeVec(f.degree(), zero)));
		}
		if (m === 1) {
			remainderNumerators.push(M[0]);
			continue;
		}

		const d		= f.degree();
		const rows	= d * m;

		const bCoeffs: T[] = [];
		for (let j = 1; j <= m; j++) {
			const prod = m === j ? M[j - 1] : M[j - 1].mul(gen.ipow(f, m - j));
			for (let r0 = 0; r0 < prod.c.length; r0++)
				bCoeffs[r0] = bCoeffs[r0] ? bCoeffs[r0].add(prod.c[r0]) : prod.c[r0];
		}

		const A		= makeMat(rows, rows, zero);
		const fDer	= f.deriv();
		const fPows: Polynomial<T>[] = [Polynomial([one]), f];
		for (let j = 2; j <= m; j++)
			fPows[j] = fPows[j - 1].mul(f);

		for (let k = 1; k <= m - 1; k++) {
			for (let t = 0; t < d; t++) {
				const basis		= Polynomial([one]).shift(t);
				const contrib	= basis.deriv().mul(fPows[m - k]).sub(basis.mul(fDer).mul(fPows[m - k - 1]).scale(one.scale(k)));
				const colIdx	= (k - 1) * d + t;
				for (let row = 0; row < rows; row++)
					A[row][colIdx] = contrib.c[row] ?? zero;
			}
		}

		for (let t = 0; t < d; t++) {
			const contrib	= fPows[m - 1].shift(t);
			const colIdx	= (m - 1) * d + t;
			for (let row = 0; row < rows; row++)
				A[row][colIdx] = contrib.c[row] ?? zero;
		}

		const { perm } = LUDecomposeBareissT(A);
		const xvec = LUSolveBareissT(A, bCoeffs, perm);
		if (!xvec) {
			remainderNumerators.push(M[0]);
			continue;
		}

		const Nk: Polynomial<T>[] = [];
		for (let k = 1; k <= m - 1; k++)
			Nk.push(Polynomial(Array.from({ length: d }, (_, t) => xvec[(k - 1) * d + t]) ?? zero));

		const D = Polynomial(Array.from({ length: d }, (_, t) => xvec[(m - 1) * d + t]) ?? zero);

		for (let k = 1; k <= m - 1; k++)
			derivativeTerms.push(rationalT(Nk[k - 1], gen.ipow(f, k)));

		remainderNumerators.push(D);
	}

	// assemble S and R
	const	S = factors.reduce((S, b) => S.mul(b.factor), Polynomial([one]));
	const	R = factors.reduce((R, b, i) => {
		const _other = S.divmod(b.factor);
		return remainderNumerators[i] ? R.add(remainderNumerators[i].mul(_other)) : R;
	}, Polynomial([zero]));

	return { polyPart: polyPart, derivative: derivativeTerms, remainder: rationalT(R, S) };
}
