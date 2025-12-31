/* eslint-disable custom/custom-control-block-style */
import { scalar, hasop, isScalar, ops1 } from './core';
import { Polynomial, PolyTypes, PolyNTypes, coeffOps } from './polynomial';
import { LUDecomposeBareiss, LUDecomposeBareissT, LUSolveBareissTransposeMultiT, solveRectangularBareiss, solveRectangularBareissT, matMulT, matPow, nullspaceColT, transpose, evaluateIntegerPolyAtMatrix, projectSubspace, characteristicT } from './linear';
import real from './real';
import integer from './integer';
import rational, { rationalT, canMakeRational } from './rational';
import gen, { Mod, ModFactory } from './gen';

export type Factor<T> = {
	factor:			Polynomial<T>;
	multiplicity:	number;
}

type factorOps<T>	= coeffOps<T> & hasop<'recip'|'sign'|'from'>;
type factorOps1<T>	= factorOps<T> & hasop<'lt'|'divmod'>;
type factorType		= number | factorOps<any>;

// linear algebra helpers are provided by ./linear


export type PolyMod<T> = Mod<Polynomial<T>> & { degree(): number; shift1(): PolyMod<T>; };
type PolyModFactory<T> = (new (v: Polynomial<T>) => PolyMod<T>) & { wrap(p: Polynomial<T>): PolyMod<T> };

function PolyModFactory<T>(r: Polynomial<T>): PolyModFactory<T> {
	return (r.is(real.is)
		? realMod(r)
		: TMod(r as any)
	) as any;

	function TMod<U extends factorOps<U>>(r: Polynomial<U>) {
		class M extends ModFactory(r) {
			degree() { return this.v.degree(); }
			//evaluate(t: U) {
			//	const r = this.v.evaluate(t);
			//}
			shift1() {
				const c = this.v.c;
				const raw: U[] = [];
				for (const i in c)
					raw[+i + 1] = c[i];

				const factor = c[r.degree() - 1];// ? zero : a.c[n - 1].div(m.c[n]);
				if (factor) {
					for (const i in c) {
						const mi = r.c[i];
						if (mi)
							raw[i] = raw[i] ? raw[i].sub(factor.mul(mi)) : factor.mul(mi).neg();
					}
				}
				return this._create(Polynomial(raw));
			}

		};
		return M;
	}
	function realMod(r: Polynomial<number>) {
		class M extends ModFactory(r) {
			degree() { return this.v.degree(); }
			shift1() {
				const c = this.v.c;
				const raw: number[] = [];
				for (const i in c)
					raw[+i + 1] = c[i];

				const factor = c[r.degree() - 1];
				if (factor) {
					for (const i in c)
						raw[i] = (raw[i] ?? 0) - factor * r.c[i];
				}
				return this._create(Polynomial(raw));
			}
		};
		return M;
	}
}


//-----------------------------------------------------------------------------
//	PRS: Polynomial Remainder Sequence and GCD
//-----------------------------------------------------------------------------

export function polyGCD<T extends PolyNTypes>(A: Polynomial<T>, B: Polynomial<T>): Polynomial<T> {
	return A.is(real.is)
		? gen.gcd(A, B)
		: polyGCDT(A as Polynomial<any>, B as Polynomial<any>) as Polynomial<T>;
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

export function squareFreeFactorization<T extends PolyTypes>(f: Polynomial<T>): Factor<T>[] {
	return f.is(real.is)
		? squareFreeFactorizationN(f) as any
		: squareFreeFactorizationT(f as Polynomial<any>);
}


// Return array of { factor, multiplicity } for a polynomial f using primitive PRS gcd
function squareFreeFactorizationN(f: Polynomial<number>): Factor<number>[] {
	const res: Factor<number>[] = [];
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
	// If no factors were recorded but f is non-constant, then f is square-free and irreducible by the PRS method used here; return f as a single factor
	if (res.length === 0 && f.degree() > 0)
		res.push({ factor: f.dup(), multiplicity: 1 });
	return res;
}

// Return array of { factor, multiplicity } for a polynomial f using primitive PRS gcd
function squareFreeFactorizationT<T extends coeffOps<T>>(f: Polynomial<T>) {
	const res: Factor<T>[] = [];
	if (f.degree() <= 0)
		return res;

	//let g = polyGCDT(f, f.deriv());
	let g = gen.gcd(f, f.deriv());
	if (g.degree() === 0) {
		res.push({ factor: f.dup(), multiplicity: 1 });
		return res;
	}

	let w = f.dup().divmod(g);

	for (let i = 1; i < 256 && w.degree() > 0; i++) {
		//const y = polyGCDT(w, g);
		const y = gen.gcd(w, g);
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
//  Factorization over extension field K = base T (e.g. Polynomial<number>/ (r) )
//-----------------------------------------------------------------------------

// Helper: build matrix from column vectors `cols` and solve A * x = b for x.
// Returns the solution vector `alpha` or null when no solution.
function solveRelationFromCols<T extends factorType>(cols: T[][], m: number, n: number, zero: T): T[] | null {
	const A = Array.from({ length: n }, (_, row) => Array.from({ length: m }, (_, col) => cols[col][row] ?? zero));
	const B = [Array.from({ length: n }, (_, row) => cols[m][row] ?? zero)];

	const sol = real.is(cols[0][0])
		? solveRectangularBareiss(A as number[][], B as number[][])
		: solveRectangularBareissT(A as any, B as any);
	return sol ? sol[0] as T[] : null;
}

// Factor a square-free polynomial S over K into irreducible factors over K
function factorSquareFreeOverK<T extends factorType>(S: Polynomial<T>, coeffsToTry = [-2, -1, 1, 2]): Polynomial<T>[] {
	const N = S.degree();
	if (N <= 1)
		return [S.dup()];

	const nums	= S.is(real.is);
	const one	= nums ? 1 : (S.c[0] as factorOps<T>).from(1);
	const zero	= nums ? 0 : one.scale(0);
	const neg	= nums ? (x: T) => -(x ?? 0) : (x: T) => x ? (x as factorOps<T>).neg() as T : zero;
	const from	= nums ? (x: number) => x : (x: number) => one.from(x);
	const mod	= PolyModFactory(S);

	function solveRelation(cols: T[][]) {
		// find smallest m such that cols[0..m-1] are linearly dependent with cols[m]
		for (let m = 1; m <= N; m++) {
			const alpha = solveRelationFromCols(cols, m, N, zero);
			if (alpha)
				return Polynomial(Array.from({ length: m + 1 }, (_, i) => i == m ? one : neg(alpha[i])));
		}
		// no relation found => minimal polynomial is x^{n} (deg n)
		return Polynomial([one]).shift(N);
	}

	// Compute minimal annihilating polynomial for the basis element x^basisIdx in A = K[x]/(S).
	function minimalPolynomialForBasis(basisIdx: number) {
		// produce sequence v_k = x^k * basis / S reduced, represented as coefficient vectors length n
		const cols: T[][] = [];
		let p = mod.wrap(Polynomial([one]).shift(basisIdx));
		for (let k = 0; k <= N; k++) {
			cols.push(p.v.c.slice());
			p = p.shift1();
		}
		return cols;
	}

	// Compute minimal polynomial of multiplication-by-`elem` operator on A = K[x]/(S)
	function minimalPolynomialForElement(elem: Polynomial<T>) {
		// produce sequence v_k = elem^k * 1 mod S
		const cols: T[][] = [];
		const e = mod.wrap(elem);
		let p = mod.wrap(Polynomial([one]));
		for (let k = 0; k <= N; k++) {
			cols.push(p.v.c.slice());
			p = p.mul(e);
		}
		return cols;
	}

	// Compute minimal polynomial of multiplication-by-x operator on A = K[x]/(S)
	function minimalPolynomialOperator() {
		// choose deterministic initial vector v = 1 + x + x^2 + ...
		const cols: T[][] = [];
		let p = mod.wrap(Polynomial(Array.from({ length: N }, () => one)));
		for (let k = 0; k <= N; k++) {
			cols.push(p.v.c.slice());
			p = p.shift1();
		}
		return cols;
	}


	// compute minimal polynomial of multiplication operator (deterministic)
	const lcmPoly = solveRelation(minimalPolynomialOperator());
	if (!lcmPoly)
		return [S.dup()];

	// gcd between S and lcmPoly may reveal a nontrivial factor
	const g = gen.gcd(S, lcmPoly);
	if (g.degree() > 0 && g.degree() < S.degree())
		return factorSquareFreeOverK(g).concat(factorSquareFreeOverK(S.divmod(g)));

	// If operator minimal polynomial did not split S, try per-basis minimal polynomials (annihilators of basis elements)
	// These can reveal nontrivial factors that the operator polynomial misses
	for (let idx = 0; idx < N; idx++) {
		const m = solveRelation(minimalPolynomialForBasis(idx));
		if (m) {
			const gg = gen.gcd(S, m);
			if (gg.degree() > 0 && gg.degree() < S.degree())
				return factorSquareFreeOverK(gg).concat(factorSquareFreeOverK(S.divmod(gg)));
		}
	}

	// Try small deterministic linear combinations of basis elements as multiplication elements in A = K[x]/(S)
	// For small degrees this often finds a splitting element when per-basis tests fail.
	// Coefficients tried are from the small set [-2,-1,1,2]. We limit to combinations with at most two non-zero entries to keep search bounded.

	// generate combinations with 1 or 2 non-zero coefficients
	for (let i = 0; i < N; i++) {
		for (const a of coeffsToTry) {
			const gg = gen.gcd(S, solveRelation(minimalPolynomialForElement(Polynomial([from(a)]).shift(i))));
			if (gg.degree() > 0 && gg.degree() < S.degree())
				return factorSquareFreeOverK(gg).concat(factorSquareFreeOverK(S.divmod(gg)));
		}
	}

	for (let i = 0; i < N; i++) {
		for (let j = i + 1; j < N; j++) {
			for (const a of coeffsToTry) {
				for (const b of coeffsToTry) {
					const vec: T[] = [];
					vec[i] = from(a);
					vec[j] = from(b);
					const gg = gen.gcd(S, solveRelation(minimalPolynomialForElement(Polynomial(vec))));
					if (gg.degree() > 0 && gg.degree() < S.degree())
						return factorSquareFreeOverK(gg).concat(factorSquareFreeOverK(S.divmod(gg)));
				}
			}
		}
	}

	// as a last deterministic attempt, try the element equal to x (the residue class)
	const ggX = gen.gcd(S, solveRelation(minimalPolynomialForElement(Polynomial([zero, one]))));
	if (ggX.degree() > 0 && ggX.degree() < S.degree())
		return factorSquareFreeOverK(ggX).concat(factorSquareFreeOverK(S.divmod(ggX)));

	// cannot split further, assume irreducible
	return [S.dup()];
}

export function factorOverK<T extends factorType>(f: Polynomial<T>): Factor<T>[] {
	const sf = squareFreeFactorization(f);
	const res: Factor<T>[] = [];
	for (const item of sf) {
		const pieces = factorSquareFreeOverK(item.factor);
		for (const p of pieces)
			res.push({ factor: p as Polynomial<T>, multiplicity: item.multiplicity });
	}
	return res;
}

// K[ζ] helpers: types and small utilities to operate on polynomials in ζ whose coefficients are `Mod`-wrapped polynomials in t
type PolyPolyR = Polynomial<Polynomial<rational>>;
type KzMod = Polynomial<PolyMod<rational>>;

// Evaluate a polynomial p(x) whose coefficients are in K (Polynomial<rational>)
// at x = alphaKz (an element of K[ζ]), returning a K[ζ] element. Uses Horner.
function evaluateAtKz(p: PolyPolyR, alphaKz: KzMod, ModZ: ModFactory<Polynomial<PolyMod<rational>>>, ModLocal: PolyModFactory<rational>): KzMod {
	return p.map(ci => ModZ.wrap(Polynomial([ModLocal.wrap(ci.dup())]))).evaluate(ModZ.wrap(alphaKz)).v;
}

// Perform synthetic division of S by (x - alpha) in K[ζ]. Returns remainder and quotient array (indices 1..an)
function syntheticDivideInKz(S: PolyPolyR, alphaKz: KzMod, ModZ: ModFactory<Polynomial<PolyMod<rational>>>, ModLocal: PolyModFactory<rational>) {
	const an = S.degree();
	const aKzMod = S.c.map(c => ModZ.wrap(Polynomial([ModLocal.wrap(c)])));
	const alphaKzMod = ModZ.wrap(alphaKz);
	const b: Mod<Polynomial<PolyMod<rational>>>[] = [];

	b[an] = aKzMod[an];
	for (let k = an - 1; k >= 0; k--)
		b[k] = aKzMod[k] ? aKzMod[k].add(b[k + 1].mul(alphaKzMod)) : b[k + 1].mul(alphaKzMod);

	return { rem: b[0].v, quotArr: b.slice(1, an + 1).map(m => m.v) };
}

// Invert an element of K[ζ] (Polynomial<Mod>) modulo p(ζ) by solving a linear system for inverse coefficients in K. Returns null when not invertible
function invertKz(elem: KzMod, ModZ: ModFactory<Polynomial<PolyMod<rational>>>, d: number, ModLocal: PolyModFactory<rational>): KzMod | null {
	// Build matrix A where column j is remainder of elem * ζ^j mod pPoly,
	const zero = ModLocal.wrap(Polynomial([rational.from(0)]));
	const A: PolyMod<rational>[][] = Array.from({ length: d }, () => Array.from({ length: d }, () => zero));
	for (let j = 0; j < d; j++) {
		const rem = ModZ.wrap(elem.shift(j)).v;
		for (const idx in rem.c) {
			const ii = +idx;
			if (ii < d && rem.c[ii])
				A[ii][j] = rem.c[ii];
		}
	}
	const B		= Array.from({ length: d }, (_, i) => ModLocal.wrap(Polynomial([rational.from(i === 0 ? 1 : 0)])));
	const sol	= solveRectangularBareissT(A, [B]);
	return sol ? Polynomial(sol[0]) : null;
}

// Compute E_i matrices for multiplication-by-t^i projected into subspace spanned by nsCols.
function computeEis<T extends ops1<T> & hasop<'sign'|'from'|'mul'|'add'|'recip'>>(nsCols: T[][], dr: number, r: Polynomial<T>, zero: T, one: T) {
	const N	= Math.max(...nsCols.map(i => i.length));
	const m	= nsCols.length;
	const E	= Array.from({ length: dr }, () => Array.from({ length: m }, () => Array.from({ length: m }, () => zero)));

	const T_rows = Array.from({ length: N }, (_, row) => Array.from({ length: m }, (_, col) => nsCols[col][row] ?? zero));

	for (let t = 0; t < dr; t++) {
		const BT = Array.from({ length: N }, () => Array.from({ length: m }, () => zero));
		for (let col = 0; col < m; col++) {
			for (const c2 in nsCols[col]) {
				const rem = Polynomial([one]).shift(+c2 % dr + t);
				rem.divmod(r);

				const bv		= nsCols[col][c2];
				const baseRow	= Math.floor(+c2 / dr) * dr;
				for (const ti in rem.c) {
					const row	= baseRow + +ti;
					BT[row][col] = BT[row][col].add(rem.c[ti].mul(bv));
				}
			}
		}
		for (let k = 0; k < m; k++) {
			const sol2 = solveRectangularBareissT(T_rows, [BT.map(rw => rw[k])]);
			if (sol2) {
				const xcol = sol2[0];
				for (let i = 0; i < m; i++)
					E[t][i][k] = xcol[i];
			}
		}
	}
	return E;
}

// Q-linear deterministic factorisation: treat A = K[x]/(S) as a Q-vector space with basis { t^i x^j } and compute the minimal polynomial of multiplication-by-x over Q using exact rational arithmetic
// Returns array of irreducible factors.
// Return type: either a polynomial factor over K or an extension description
type FactorExt	= { alpha: KzMod; pCoeffs: number[] };
type FactorQOut	= PolyPolyR | FactorExt;

function factorSquareFreeOverK_Q(S: PolyPolyR, R: Polynomial<rational>): FactorQOut[] {
	// Deterministic Q-linear splitter: build exact multiplication-by-x matrix over Q on basis { t^i x^j } and compute its characteristic polynomial exactly
	// Use that polynomial's factors to derive gcds with S and split S over K

	let		s	= S.degree();
	const	r	= R.degree();
	if (s <= 1 || r <= 0)
		return [S.dup()];

	const Mod = PolyModFactory(R);

	// Basis size
	const N = s * r;

	const q0 = rational.from(0);
	const q1 = rational.from(1);

	const ScoeffsMod = S.c.map(i => Mod.wrap(i));
	while (s >= 0 && ScoeffsMod[s].degree() < 0)
		s--;
	if (s < 0)
		return [S.dup()];

	const invLeadMod = ScoeffsMod[s].recip();
	if (!invLeadMod)
		throw new Error('Non-invertible leading coefficient in S when constructing multiplication matrix');

	ScoeffsMod.length = s;

	// Build multiplication matrix M (rows x cols) initialized to zero
	const M = Array.from({ length: N }, () => Array.from({ length: N }, () => q0));

	// For each basis element t^i x^j (col = j*dr + i) compute x * (t^i x^j)
	for (let j = 0; j < s - 1; j++) {
		const base = j * r;
		for (let i = 0; i < r; i++)
			M[base + i][base + r + i] = q1;	// x * t^i x^j = t^i x^{j+1}
	}

	for (let i = 0; i < r; i++) {
		const col = (s - 1) * r + i;
		// x^{n} reduction: x^n = - Slead^{-1} * sum_{k=0..n-1} Scoeffs[k] x^k
		for (const k in ScoeffsMod) {
			const rem = ScoeffsMod[k].mul(invLeadMod).neg().v.shift(i);
			rem.divmod(R);

			const rowbase = +k * r;
			for (const ti in rem.c) {
				const row = rowbase + +ti;;
				M[col][row] = M[col][row].add(rem.c[ti]);
			}
		}
	}

	const achar		= characteristicT(M);
	// Clear denominators to obtain integer polynomial for factoring
	const lcmDen	= real.lcm(...achar.map(x => x.den));
	const factors	= squareFreeFactorization(Polynomial([...achar.map(ak => ak.num * (lcmDen / ak.den)), lcmDen]));

	// Try adjoin-root extraction: for each integer factor p(x) of the characteristic polynomial, adjoin a root ζ of p and attempt to find α ∈ K[ζ] with x - α | S in K[ζ][x].

	// Try each integer factor
	for (const fac of factors) {
		const d = fac.factor.degree();
		if (d <= 0)
			continue;

		const P = evaluateIntegerPolyAtMatrix(fac.factor.c, M, rational.from);
		// compute nullspace on row-major view
		const ns = nullspaceColT(P);
		if (!ns || ns.length === 0)
			continue;

		// build T_rows from basis ns
		// compute M_sub = T^{-1} M T (use shared helper which handles column-major M)
		const M_sub = projectSubspace(M, ns, q0);

		// build E_i = T^{-1} * (multiplication-by-t^i) * T for i=0..dr-1 (centralized helper)
		const E = computeEis(ns, r, R, q0, q1);

		// compute M_sub powers
		const Mpow = Array.from({ length: d }, (_, u) => matPow(M_sub, u));

		// build F_{u,i} = Mpow[u] * E[i]
		const Fmats = Array.from({ length: d }, (_, u) =>
			Array.from({ length: r }, (_, i_t) => matMulT(Mpow[u], E[i_t]))
		);

		// Build linear system: unknowns count = d * dr
		const eqRows:	rational[][] = [];
		const eqB:		rational[] = [];
		const m		= ns.length;
		for (let p = 0; p < m; p++) {
			for (let q = 0; q < m; q++) {
				const row: rational[] = Array.from({ length: d * r }, () => q0);
				for (let u = 0; u < d; u++) {
					for (let i_t = 0; i_t < r; i_t++)
						row[u * r + i_t] = Fmats[u][i_t][p][q];
				}
				eqRows.push(row);
				eqB.push(M_sub[p][q]);
			}
		}
		const solA = solveRectangularBareissT<rational>(eqRows, [eqB]);
		if (!solA)
			continue;

		const solVec = solA[0];
		// reconstruct alpha as array of length d of Polynomial<rational> (coeffs in t)
		const alphaKz		= Polynomial(Array.from({length: d}, (_, u) => Mod.wrap(Polynomial(Array.from({length: r}, (_, i) => solVec[u * r + i])))));
		const pCoeffsInt	= fac.factor.c.map(v => Math.round(v));
		const ModZ			= ModFactory(Polynomial(pCoeffsInt.map(ci => Mod.wrap(Polynomial([rational.from(ci)])))));
		const { rem, quotArr } = syntheticDivideInKz(S, alphaKz, ModZ, Mod);

		// check remainder zero
		let isZero = true;
		for (const idx in rem.c) {
			const u = +idx;
			if (u < d && rem.c[u].v.degree() >= 0) {
				isZero = false;
				break;
			}
		}
		if (!isZero)
			continue;

		// verify that all ζ-components beyond 0 are zero
		let allInK = true;
		outer: for (const i in quotArr) {
			const pz = quotArr[i];
			for (const j in pz.c) {
				const u = +j;
				if (u > 0 && u < d && pz.c[u] && pz.c[u].v.degree() >= 0) {
					allInK = false;
					break outer;
				}
			}
		}
		if (!allInK)
			return [{ alpha: alphaKz, pCoeffs: pCoeffsInt }];

		let alphaInK = true;
		for (let u = 1; alphaInK && u < d; u++)
			alphaInK = alphaKz.c[u].v.degree() < 0;

		if (alphaInK) {
			return factorSquareFreeOverK_Q(Polynomial(quotArr.map(pz => pz.c[0].v)), R)
				.concat(factorSquareFreeOverK_Q(Polynomial([alphaKz.c[0].v.neg(), Polynomial([rational.from(1)])]), R));
		}
	}

	// no split found
	return [S.dup()];
}


//-----------------------------------------------------------------------------
//	Rothstein
//-----------------------------------------------------------------------------

// Partial Rothstein–Trager pipeline
// Returns R(t), its square-free factors, and for each factor the gcd D vs (N - t D') with coefficients reduced modulo the factor so they live in Q[t]/(r(t))

export interface FactorGCD<T> extends Factor<T> {
	gcd: Polynomial<Polynomial<rational>> | FactorExt;
}

export interface PartialResult<T> {
	R?:			Polynomial<T>;
	factors:	Factor<T>[];
	gcds:		FactorGCD<T>[];
}

export function rothsteinPartial<T extends number|factorOps1<any>>(N: Polynomial<Polynomial<T>>, D: Polynomial<Polynomial<T>>): PartialResult<T> {
	const zero		= N.leadCoeff().scale(0);
	const results: FactorGCD<T>[] = [];

	const Dd		= D.deriv();
	// Q(x,t) = N(x) - t * D'(x)  => coefficient at x^i is Ni(t) - t * Dd_i(t)
	const Q			= Polynomial(Array.from({ length: Math.max(N.c.length, Dd.c.length) }, (_, i) => (N.c[i] ?? zero).sub((Dd.c[i] ?? zero).shift(1))));
	const R			= resultant(D, Q);

	// Special-case when resultant is identically zero: resultant 0 => R has no square-free factors but D and Q may still have a non-trivial gcd for all t
	// Detect that case and record the gcd so downstream residue extraction can handle it
	if (!R) {
		const G = gen.gcd(D, Q);
		if (G.degree() >= 0)
			results.push({
				factor:			Polynomial<T>([]),
				multiplicity:	1,
				gcd:			G.map(c => c.map(t => rational.from(t)))
			});
		return { R, factors: [], gcds: results };
	}

	const factors	= squareFreeFactorization(R);

	// build polynomials in x with inner coeffs polynomials in t
	const D1 = D.map(c => c.map(t => rational.from(t)));

	for (const fac of factors) {
		const r		= fac.factor;
		const mod	= ModFactory(r);

		// Wrap each inner Polynomial<T> into the Mod wrapper for K = Q[t]/(r)
		// construct inner polynomial representing N_i + t * D'_i as Polynomial([Ni, Di])
		const G = gen.gcd(
			D.map(c => mod.wrap(c)),
			Q.map(c => mod.wrap(c))
		);

		const G1 = G.map(c => c.v.map(rational.from));
		const r1 = r.map(rational.from);
		const sf = squareFreeFactorization(G1);
		type GFactorAny = { factor: FactorQOut; multiplicity: number };
		const GfactorsAny: GFactorAny[] = [];

		for (const item of sf) {
			const pieces = factorSquareFreeOverK_Q(item.factor, r1);
			for (const p of pieces)
				GfactorsAny.push({ factor: p, multiplicity: item.multiplicity });
		}

		if (GfactorsAny.length === 0) {
			// Try adjoin-root (primitive-element) approach: factor full D over K = Q[t]/(r).
			// This attempts to find x-factors of D that only appear after adjoining a root of r(t).
			// If that yields nontrivial factors, use them; otherwise fall back to recording the raw gcd we computed.
			const Dfactors = factorSquareFreeOverK_Q(D1, r1);
			if (Dfactors.length > 0) {
				for (const df of Dfactors)
					results.push({ factor: r.dup(), multiplicity: fac.multiplicity, gcd: df });
			} else {
				results.push({ factor: r.dup(), multiplicity: fac.multiplicity, gcd: G1 });
			}
		} else {
			for (const gf of GfactorsAny)
				results.push({ factor: r.dup(), multiplicity: fac.multiplicity * gf.multiplicity, gcd: gf.factor });
		}
	}

	return { R, factors, gcds: results };
}

// Attempt to factor polynomial G(x) with coefficients in K = Q[t]/(r) by evaluation/interpolation
// This is a heuristic: evaluate at sample t-values u where r(u) != 0, factor numeric polynomials,and attempt to interpolate factor coefficients back to polynomials in t of degree < deg(r)

// Extract residues for linear gcd factors only.
export interface Residue<T> extends Factor<T> {
	residue?:	Polynomial<rational>;
	residueK?:	Polynomial<PolyMod<rational>>;
}

export function rothsteinResidues<T extends (canMakeRational & PolyTypes)>(N: Polynomial<Polynomial<T>>, D: Polynomial<Polynomial<T>>, gcds: FactorGCD<T>[]): Residue<T>[] {
	// Inputs are polynomials in x whose coefficients are polynomials in t (Polynomial<Polynomial<T>>).
	const logs: Residue<T>[] = [];

	const N1		= N.map(c => c.map(rational.from));
	const Dd		= D.map(c => c.map(rational.from)).deriv();

	for (const ginfo of gcds) {
		const G = ginfo.gcd; // Polynomial whose coefficients are inner polynomials in t
		if ('alpha' in G)
			continue;

		// inner coefficient structure for G collected when needed

		// prepare linear coeffs if present
		const aInner = G.c[1];
		const bInner = G.c[0];

		// Global gcd case (resultant zero placeholder): handle without Mod wrapper
		if (!ginfo.factor || ginfo.factor.degree() < 0) {
			if (G.degree() === 1 && aInner?.degree() === 0 && aInner.c[0]) {
				const x0 = bInner && bInner.neg().rscale(aInner.c[0]);
				const den = x0 ? Dd.c[0] : Dd.evaluate(x0);
				if (den.degree() === 0 && den.c[0])
					logs.push({ factor: Polynomial<T>([]), residue: (x0 ? N1.evaluate(x0) : N1.c[0]).rscale(den.c[0]), multiplicity: ginfo.multiplicity });
			}
			continue;
		}

		const r		= ginfo.factor;
		const r1	= r.map(rational.from);
		const ModLocal = PolyModFactory(r1);
		const DdMod	= Dd.map(c => ModLocal.wrap(c));
		const Nmod	= N1.map(c => ModLocal.wrap(c));

		function putLog(x?: PolyMod<rational>) {
			const invDen	= (x ? DdMod.evaluate(x) : DdMod.c[0]).recip();
			if (invDen)
				logs.push({ factor: r.dup(), residue: (x ? Nmod.evaluate(x) : Nmod.c[0]).mul(invDen).v, multiplicity: ginfo.multiplicity });
		}

		function putLogK(x: KzMod, coeffs: number[]) {
			const ModZ		= ModFactory(Polynomial(coeffs.map(ci => ModLocal.wrap(Polynomial([rational.from(ci)])))));
			const invDen	= invertKz(evaluateAtKz(Dd, x, ModZ, ModLocal), ModZ, coeffs.length - 1, ModLocal);
			if (invDen)
				logs.push({ factor: r.dup(), residueK: evaluateAtKz(N1, x, ModZ, ModLocal).mul(invDen), multiplicity: ginfo.multiplicity });
		}

		// If gcd is linear we can directly compute residue in K; otherwise
		// attempt to factor G further using the Q-linear splitter to find extension roots.
		if (G.degree() === 1) {
			const invA = aInner && ModLocal.wrap(aInner).recip();
			if (invA)
				putLog(bInner && ModLocal.wrap(bInner).neg().mul(invA));

		} else {
			// Attempt adjoin-root splitting of G over K to obtain residues in extensions
			const parts = factorSquareFreeOverK_Q(G, r1);

			// If splitter returned the same irreducible integer polynomial, treat as no split and use const-coeff fallback
			if (parts && parts.length && !(parts.length === 1 && !('alpha' in parts[0]) && parts[0].eq(G))) {
				for (const part of parts) {
					if ('alpha' in part) {
						putLogK(part.alpha, part.pCoeffs);
					} else if (part.degree() === 1) {
						const invA = part.c[1] && ModLocal.wrap(part.c[1]).recip();
						if (invA)
							putLog(ModLocal.wrap(part.c[0]).neg().mul(invA));
					}
				}

			} else {
				// If splitter returned nothing interesting, but G has constant (t-free) coefficients, we can adjoin a root ζ of the integer polynomial G and evaluate residues in Q(ζ)
				let allConst = true;
				const coeffNums: number[] = [];
				for (const i in G.c) {
					const ci = G.c[i];
					if (ci.degree() > 0) {
						allConst = false;
						break;
					}
					if (ci.degree() === 0)
						coeffNums[i] = +ci.c[0];
				}
				if (allConst)
					putLogK(Polynomial([ModLocal.wrap(Polynomial([rational.from(1)]))]).shift(1), coeffNums);
			}
		}
	}

	return logs;
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
function discriminantN(p: Polynomial<number>): number {
	const	n	= p.degree();
	if (n < 1)
		return 0;
	const	R	= resultantN(p, p.deriv());
	return (((n * (n - 1))) & 2 ? -R : R) / p.leadCoeff();
}

function resultantT<T extends coeffOps<T> & hasop<'sign'|'from'>>(p: Polynomial<T>, q: Polynomial<T>): T|undefined {
	const pd = p.degree();
	const qd = q.degree();

	if (pd >= 0 && qd >= 0) {
		const one = p.leadCoeff().from(1);
		if (pd + qd === 0)
			return one;

		const M = sylvesterMatrix(p.c, q.c, one.scale(0));
		const n = M.length;
		const { swaps } = LUDecomposeBareissT(M, true);
		return swaps & 1 ? M[n - 1][n - 1].neg() : M[n - 1][n - 1];
	}
}

function discriminantT<T extends coeffOps<T> & hasop<'sign'|'from'>>(p: Polynomial<T>): T|undefined {
	const	n	= p.degree();
	if (n > 0) {
		const	R	= resultantT(p, p.deriv());
		if (R)
			return (((n * (n - 1))) & 2 ? R.neg() : R).div(p.leadCoeff());
	}
}

// Generic wrapper for resultant: dispatch to numeric or generic variant
export function resultant<T extends PolyNTypes>(p: Polynomial<T>, q: Polynomial<T>): T|undefined {
	return p.is(real.is)
		? resultantN(p as any, q as any) as T
		: resultantT(p as any, q as any) as T|undefined
	;
}

export function discriminant<T extends number | scalar<any>>(p: Polynomial<T>): T|undefined {
	return p.is(real.is)
		? discriminantN(p as Polynomial<number>) as T
		: discriminantT(p as any) as T;
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
		// sigma starts at zero of the appropriate type
		const sigma = poly.c[0].from(0);
		const indices = Array.from({ length: k }, (_, i) => i);

		// Build sigma by accumulating products over all k-element subsets of roots
		let accum = poly.c[0].from(0);
		function generateAndAccumulate(start: number, depth: number) {
			if (depth === k) {
				let product = roots[indices[0]];
				for (let i = 1; i < k; i++)
					product = product.mul(roots[indices[i]]);
				accum = accum.add(product);
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
		equations.push((k % 2 === 0) ? accum.sub(ck) : accum.add(ck));
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
			M.push(t ? t.numer : Polynomial(Array.from({ length: f.degree() }, () => zero)));
		}
		if (m === 1) {
			remainderNumerators.push(M[0]);
			continue;
		}

		const d		= f.degree();
		const rows	= d * m;
		const cols	= m * d;

		const bCoeffs: T[] = [];
		for (let j = 1; j <= m; j++) {
			const prod = m === j ? M[j - 1] : M[j - 1].mul(gen.ipow(f, m - j));
			for (let r0 = 0; r0 < prod.c.length; r0++)
				bCoeffs[r0] = bCoeffs[r0] ? bCoeffs[r0].add(prod.c[r0]) : prod.c[r0];
		}

		const A		= Array.from({ length: rows }, () => Array.from({ length: cols }, () => zero));
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
