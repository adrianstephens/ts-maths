/* eslint-disable custom/custom-control-block-style */
import { scalar, hasop, isScalar } from './core';
import { Polynomial, PolyTypes, PolyNTypes, coeffOps } from './polynomial';
import { LUDecomposeBareiss, LUDecomposeBareissT, LUSolveBareissTransposeMultiT, solveRectangularBareiss, solveRectangularBareissT } from './bareiss';
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

// Matrix multiply and trace helpers (rational arithmetic)
function matMul<T extends factorOps<T>>(A: T[][], B: T[][]): T[][] {
	const zero = A[0][0].scale(0);
	const N = A.length;
	const R = Array.from({ length: N }, () => Array.from({ length: N }, () => zero));
	for (let i = 0; i < N; i++) {
		for (let k = 0; k < N; k++) {
			const aik = A[i][k];
			if (aik) {
				for (let j = 0; j < N; j++) {
					const bkj = B[k][j];
					if (bkj)
						R[i][j] = R[i][j].add(aik.mul(bkj));
				}
			}
		}
	}
	return R;
}

function traceMat<T extends factorOps<T>>(A: T[][]): T {
	const N = A.length;
	let s = A[0][0];
	for (let i = 1; i < N; i++)
		s = s.add(A[i][i]);
	return s;
}


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
	if (real.is(cols[0][0])) {
		const A = Array.from({ length: n }, (_, row) => Array.from({ length: m }, (_, col) => cols[col][row] ?? zero)) as unknown as number[][];
		const B = [Array.from({ length: n }, (_, row) => cols[m][row] ?? zero)] as unknown as number[][];
		const sol = solveRectangularBareiss(A, B);
		return sol ? (sol[0] as unknown as T[]) : null;
	} else {
		const A = Array.from({ length: n }, (_, row) => Array.from({ length: m }, (_, col) => cols[col][row] ?? zero)) as unknown as any[][];
		const B = [Array.from({ length: n }, (_, row) => cols[m][row] ?? zero)] as unknown as any[][];
		const sol = solveRectangularBareissT(A, B);
		return sol ? (sol[0] as unknown as T[]) : null;
	}
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

//-----------------------------------------------------------------------------
//  Factorization over extension field K = base T (e.g. Polynomial<number>/ (r) )
//-----------------------------------------------------------------------------

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
		b[k] = aKzMod[k].add(b[k + 1].mul(alphaKzMod));

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

// Q-linear deterministic factorisation: treat A = K[x]/(S) as a Q-vector space with basis { t^i x^j } and compute the minimal polynomial of multiplication-by-x over Q using exact rational arithmetic
// Returns array of irreducible factors.
// Return type: either a polynomial factor over K or an extension description
type FactorQOut = PolyPolyR | { ext: { alpha: KzMod; pCoeffs: number[]; quotient: KzMod[] } };

function factorSquareFreeOverK_Q(S: PolyPolyR, r: Polynomial<rational>): FactorQOut[] {
	// Deterministic Q-linear splitter: build exact multiplication-by-x matrix over Q on basis { t^i x^j } and compute its characteristic polynomial exactly
	// Use that polynomial's factors to derive gcds with S and split S over K

	let		n	= S.degree();
	const	dr	= r.degree();
	if (n <= 1 || dr <= 0)
		return [S.dup()];

	const Mod = PolyModFactory(r);

	// Basis size
	const N = n * dr;

	const q0 = rational.from(0);
	const q1 = rational.from(1);

	const ScoeffsMod = S.c.map(i => Mod.wrap(i));
	while (n >= 0 && (ScoeffsMod[n].degree() < 0))
		n--;
	if (n < 0)
		return [S.dup()];

	ScoeffsMod.length = n + 1;
	const Slead = ScoeffsMod[n];
	if (!Slead || Slead.degree() < 0)
		return [S.dup()];

	// Build multiplication matrix M (rows x cols) initialized to zero
	const M = Array.from({ length: N }, () => Array.from({ length: N }, () => q0));

	// diagnostic helper removed

	// For each basis element t^i x^j (col = j*dr + i) compute x * (t^i x^j)
	const invLeadMod = Slead.recip();
	if (!invLeadMod)
		throw new Error('Non-invertible leading coefficient in S when constructing multiplication matrix');

	for (let j = 0; j < n; j++) {
		for (let i = 0; i < dr; i++) {
			const col = j * dr + i;
			const nextJ = j + 1;
			if (nextJ < n) {
				// x * t^i x^j = t^i x^{j+1}
				M[nextJ * dr + i][col] = q1;
				continue;
			}
			// x^{n} reduction: x^n = - Slead^{-1} * sum_{k=0..n-1} Scoeffs[k] x^k
			for (let k = 0; k < n; k++) {
				const ck = ScoeffsMod[k];
				if (ck) {
					// multiply by t^i and reduce mod r
					const shifted = ck.mul(invLeadMod).neg().v.shift(i);
					shifted.divmod(r); // mutates shifted to be the remainder
					const rem = shifted;
					for (let ti = 0; ti < rem.c.length; ti++) {
						const rawCoeff = rem.c[ti];
						if (rawCoeff) {
							const row = k * dr + ti;
							M[row][col] = M[row][col].add(rawCoeff);
						}
					}
				}
			}
		}
	}

	// Compute traces of powers: trace(M^k) for k=1..N
	const traces: rational[] = [];
	let P = M.map(rw => rw.slice());
	for (let k = 1; k <= N; k++) {
		traces[k] = traceMat(P);
		if (k < N)
			P = matMul(P, M);
	}

	// Faddeev–LeVerrier: compute characteristic polynomial coefficients a1..aN
	const a = Array.from({ length: N + 1 }, () => rational.from(0));
	for (let k = 1; k <= N; k++) {
		let sum = traces[k];
		for (let i = 1; i <= k - 1; i++)
			sum = sum.add(a[i].mul(traces[k - i]));
		a[k] = sum.neg().div(rational.from(k));
	}

	// Clear denominators to obtain integer polynomial for factoring
	const dens		= a.slice(1).map(x => x.den);
	const lcmDen	= dens.length ? real.lcm(...dens) : 1;
	// Build integer coefficient array for characteristic polynomial
	// Faddeev–LeVerrier gives a[1..N] for polynomial x^N + a1*x^{N-1} + ... + aN
	const intCoeffs = Array.from({ length: N + 1 }, () => 0);
	intCoeffs[N] = 1; // leading coefficient for x^N
	for (let k = 1; k <= N; k++)
		intCoeffs[N - k] = a[k].num * (lcmDen / a[k].den);		// place a[k] at power x^{N-k}

	const factors = squareFreeFactorization(Polynomial(intCoeffs));
	// integer characteristic polynomial computed

	// Try adjoin-root extraction: for each integer factor p(x) of the characteristic polynomial, adjoin a root ζ of p and attempt to find α ∈ K[ζ] with x - α | S in K[ζ][x].

	// Helpers: evaluate integer polynomial at rational matrix, nullspace over Q, matrix power
	function evaluatePolyAtMatrix(poly: number[], Mmat: rational[][]): rational[][] {
		const size = Mmat.length;
		const zero = rational.from(0);
		const Pmat = Array.from({ length: size }, (_, i) => Array.from({ length: size }, (_, j) => i === j ? rational.from(poly[0] ?? 0) : zero));
		if (poly.length > 1) {
			let Mpow = Mmat.map(rw => rw.slice());
			for (const k in poly) {
				if (+k === 0)
					continue;
				const ck = rational.from(poly[k]);
				for (let i = 0; i < size; i++)
					for (let j = 0; j < size; j++)
						Pmat[i][j] = Pmat[i][j].add(Mpow[i][j].mul(ck));
				if (+k + 1 < poly.length)
					Mpow = matMul(Mpow, Mmat);
			}
		}
		return Pmat;
	}

	function nullspaceRational(Aorig: rational[][]): rational[][] {
		const A = Aorig.map(rw => rw.slice());
		const rows = A.length; const cols = A[0].length;
		const pivots: number[] = [];
		let r0 = 0;
		for (let c = 0; c < cols && r0 < rows; c++) {
			let piv = r0;
			while (piv < rows && A[piv][c].num === 0)
				piv++;
			if (piv === rows)
				continue;
			if (piv !== r0)
				[A[r0], A[piv]] = [A[piv], A[r0]];
			pivots.push(c);
			for (let i = r0 + 1; i < rows; i++) {
				if (A[i][c].num) {
					const factor = A[i][c].div(A[r0][c]);
					for (let j = c; j < cols; j++)
						A[i][j] = A[i][j].sub(factor.mul(A[r0][j]));
				}
			}
			r0++;
		}

		// pivot info collected
		const pivotSet = new Set(pivots);
		const freeCols: number[] = [];
		for (let j = 0; j < cols; j++)
			if (!pivotSet.has(j))
				freeCols.push(j);

		const basis: rational[][] = [];
		for (const fc of freeCols) {
			const x: rational[] = Array.from({ length: cols }, () => rational.from(0));
			x[fc] = rational.from(1);
			for (let pi = pivots.length - 1; pi >= 0; pi--) {
				const pc = pivots[pi];
				const rowIdx = pi;
				let sum = rational.from(0);
				for (let j = pc + 1; j < cols; j++)
					if (A[rowIdx][j].num !== 0 && x[j].num !== 0)
						sum = sum.add(A[rowIdx][j].mul(x[j]));

				if (A[rowIdx][pc].num === 0)
					throw new Error('unexpected zero pivot in nullspace');
				x[pc] = sum.neg().div(A[rowIdx][pc]);
			}
			basis.push(x);
		}
		return basis;
	}

	function matPow(A: rational[][], e: number): rational[][] {
		const N = A.length;
		// identity
		let R: rational[][] = Array.from({ length: N }, (_, i) => Array.from({ length: N }, (_, j) => i === j ? rational.from(1) : rational.from(0)));
		if (e === 0)
			return R;
		let B = A.map(rw => rw.slice());
		while (e > 0) {
			if (e & 1)
				R = matMul(R, B);
			e >>= 1;
			if (e)
				B = matMul(B, B);
		}
		return R;
	}

	// Try each integer factor
	for (const fac of factors) {
		const d = fac.factor.degree();
		if (d <= 0) continue;
		const P = evaluatePolyAtMatrix(fac.factor.c, M);
		// P evaluated for factor
		const ns = nullspaceRational(P);
		if (!ns || ns.length === 0)
			continue;

		// build T_rows from basis ns
		const m = ns.length;
		const Nsize = M.length;
		const q0 = rational.from(0);
		const T_rows = Array.from({ length: Nsize }, (_, row) => Array.from({ length: m }, (_, col) => ns[col][row] ?? q0));

		// compute M_sub = T^{-1} M T
		const M_sub = Array.from({ length: m }, () => Array.from({ length: m }, () => q0));
		for (let j = 0; j < m; j++) {
			const w = Array.from({ length: Nsize }, () => q0);
			for (let row = 0; row < Nsize; row++) {
				let sum = q0;
				for (let col = 0; col < Nsize; col++) {
					const mv = M[row][col];
					if (mv.num === 0) continue;
					const bv = ns[j][col] ?? q0;
					if (bv.num === 0) continue;
					sum = sum.add(mv.mul(bv));
				}
				w[row] = sum;
			}
			const sol = solveRectangularBareissT<rational>(T_rows, [w]);
			if (!sol)
				continue;
			const ccol = sol[0];
			for (let i = 0; i < m; i++) M_sub[i][j] = ccol[i];
		}

		// build E_i = T^{-1} * (multiplication-by-t^i) * T for i=0..dr-1
		const E: rational[][][] = Array.from({ length: dr }, () => Array.from({ length: m }, () => Array.from({ length: m }, () => q0)));
		for (let i_t = 0; i_t < dr; i_t++) {
			// BT = B_{t^i} * T
			const BT: rational[][] = Array.from({ length: Nsize }, () => Array.from({ length: m }, () => q0));
			for (let colj = 0; colj < m; colj++) {
				for (let c2 = 0; c2 < Nsize; c2++) {
					const j2 = Math.floor(c2 / dr);
					const i2 = c2 % dr;
					const arrPoly = Array.from({ length: Math.max(i2 + i_t + 1, dr) }, () => rational.from(0));
					arrPoly[i2 + i_t] = rational.from(1);
					const rem = (() => { const _p = Polynomial(arrPoly); _p.divmod(r); return _p; })();
					const bv = ns[colj][c2] ?? q0;
					if (bv.num === 0) continue;
					const baseRow = j2 * dr;
					for (let ti = 0; ti < dr; ti++) {
						const rowIdx = baseRow + ti;
						const coeff = rem.c[ti] ?? q0;
						if (coeff.num === 0) continue;
						BT[rowIdx][colj] = BT[rowIdx][colj].add(coeff.mul(bv));
					}
				}
			}
			// solve T * x = BT_col
			for (let k = 0; k < m; k++) {
				const sol2 = solveRectangularBareissT<rational>(T_rows, [BT.map(rw => rw[k])]);
				if (sol2) {
					const xcol = sol2[0];
					for (let i = 0; i < m; i++)
						E[i_t][i][k] = xcol[i];
				}
			}
		}

		// compute M_sub powers
		const Mpow = Array.from({ length: d }, (_, u) => matPow(M_sub, u));

		// build F_{u,i} = Mpow[u] * E[i]
		const Fmats = Array.from({ length: d }, (_, u) =>
			Array.from({ length: dr }, (_, i_t) => matMul(Mpow[u], E[i_t]))
		);

		// Build linear system: unknowns count = d * dr
		const eqRows: rational[][] = [];
		const eqB: rational[] = [];
		for (let p = 0; p < m; p++) {
			for (let q = 0; q < m; q++) {
				const row: rational[] = Array.from({ length: d * dr }, () => q0);
				for (let u = 0; u < d; u++) {
					for (let i_t = 0; i_t < dr; i_t++)
						row[u * dr + i_t] = Fmats[u][i_t][p][q];
				}
				eqRows.push(row);
				eqB.push(M_sub[p][q]);
			}
		}
		const solA = solveRectangularBareissT<rational>(eqRows, [eqB]);
		if (!solA) continue;
		const solVec = solA[0];
		// reconstruct alpha as array of length d of Polynomial<rational> (coeffs in t)
		const alphaArr = Array.from({ length: d }, () => Polynomial<rational>([]));
		for (let u = 0; u < d; u++) {
			const coeffs = Array.from({ length: dr }, () => rational.from(0));
			for (let i_t = 0; i_t < dr; i_t++) coeffs[i_t] = solVec[u * dr + i_t] ?? rational.from(0);
			const Pcoeff = Polynomial(coeffs);
			Pcoeff.divmod(r);
			alphaArr[u] = Pcoeff;
		}
		const pCoeffsInt	= fac.factor.c.map(v => Math.round(v));
		const ModZ			= ModFactory(Polynomial(pCoeffsInt.map(ci => Mod.wrap(Polynomial([rational.from(ci)])))));
		const alphaKz		= Polynomial(alphaArr.map(p => Mod.wrap(p)));
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
			return [{ ext: { alpha: alphaKz, pCoeffs: pCoeffsInt, quotient: quotArr } }];

		let alphaInK = true;
		for (let u = 1; alphaInK && u < d; u++)
			alphaInK = alphaArr[u].degree() < 0;

		if (alphaInK) {
			return factorSquareFreeOverK_Q(Polynomial(quotArr.map(pz => pz.c[0].v)), r)
				.concat(factorSquareFreeOverK_Q(Polynomial([alphaArr[0].neg(), Polynomial([rational.from(1)])]), r));
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

interface FactorGCD<T> extends Factor<T> {
	gcd: Polynomial<Polynomial<rational>>;
	// when gcd only exists after adjoining a root, gcdExt describes the extension root
	gcdExt?: { alpha: KzMod; pCoeffs: number[]; quotient: KzMod[] };
}


export function rothsteinPartial<T extends number|factorOps1<any>>(N: Polynomial<Polynomial<T>>, D: Polynomial<Polynomial<T>>) {
	const zero		= N.leadCoeff().scale(0);
	const results: FactorGCD<T>[] = [];

	const Dd		= D.deriv();
	// Q(x,t) = N(x) - t * D'(x)  => coefficient at x^i is Ni(t) - t * Dd_i(t)
	const Q			= Polynomial(Array.from({ length: Math.max(N.c.length, Dd.c.length) }, (_, i) => (N.c[i] ?? zero).sub((Dd.c[i] ?? zero).shift(1))));
	const R			= resultant(D, Q);
	// Q and resultant R computed

	// Special-case when resultant is identically zero: resultant 0 => R has no square-free factors but D and Q may still have a non-trivial gcd for all t
	// Detect that case and record the gcd so downstream residue extraction can handle it
	if (!R) {
		const G = gen.gcd(D, Q);
		if (G.degree() >= 0)
			results.push({
				factor:			Polynomial<T>([]),
				multiplicity:	1,
				gcd:			G.map(c => Polynomial(c.c.map(t => rational.from(t))))
			});
		return { R, factors: [], gcds: results };
	}

	const factors	= squareFreeFactorization(R);

	// build polynomials in x with inner coeffs polynomials in t
	const D1 = Polynomial(D.c.map(c => Polynomial(c.c.map(t => rational.from(t)))));

	for (const fac of factors) {
		const r		= fac.factor;
		const mod	= ModFactory(r);

		// Wrap each inner Polynomial<T> into the Mod wrapper for K = Q[t]/(r)
		// construct inner polynomial representing N_i + t * D'_i as Polynomial([Ni, Di])
		const G = gen.gcd(
			D.map(c => mod.wrap(c)),
			Q.map(c => mod.wrap(c))
		);

		const G1 = G.map(c => Polynomial(c.v.c.map(t => rational.from(t))));
		const r1 = r.map(c => rational.from(c));
		const sf = squareFreeFactorization(G1);
		type GFactorAny = { factor?: PolyPolyR; ext?: { alpha: KzMod; pCoeffs: number[]; quotient: KzMod[] }; multiplicity: number };
		const GfactorsAny: GFactorAny[] = [];
		for (const item of sf) {
			const pieces = factorSquareFreeOverK_Q(item.factor, r1);
			for (const p of pieces) {
				if ((p as any).ext)
					GfactorsAny.push({ ext: (p as any).ext, multiplicity: item.multiplicity });
				else
					GfactorsAny.push({ factor: p as PolyPolyR, multiplicity: item.multiplicity });
			}
		}

		if (GfactorsAny.length === 0) {
			// Try adjoin-root (primitive-element) approach: factor full D over K = Q[t]/(r).
			// This attempts to find x-factors of D that only appear after adjoining a root of r(t).
			// If that yields nontrivial factors, use them; otherwise fall back to recording the raw gcd we computed.
			const Dfactors = factorSquareFreeOverK_Q(D1, r1);
			if (Dfactors.length > 0) {
				for (const df of Dfactors) {
					if ((df as any).ext)
						results.push({ factor: r.dup(), multiplicity: fac.multiplicity, gcd: Polynomial<Polynomial<rational>>([]), gcdExt: (df as any).ext });
					else
						results.push({ factor: r.dup(), multiplicity: fac.multiplicity, gcd: df as PolyPolyR });
				}
			} else {
				results.push({ factor: r.dup(), multiplicity: fac.multiplicity, gcd: G1 });
			}
		} else {
			for (const gf of GfactorsAny) {
				if (gf.ext)
					results.push({ factor: r.dup(), multiplicity: fac.multiplicity * gf.multiplicity, gcd: Polynomial<Polynomial<rational>>([]), gcdExt: gf.ext });
				else if (gf.factor)
					results.push({ factor: r.dup(), multiplicity: fac.multiplicity * gf.multiplicity, gcd: gf.factor });
			}
		}
	}

	return { R, factors, gcds: results };
}

// Attempt to factor polynomial G(x) with coefficients in K = Q[t]/(r) by evaluation/interpolation
// This is a heuristic: evaluate at sample t-values u where r(u) != 0, factor numeric polynomials,and attempt to interpolate factor coefficients back to polynomials in t of degree < deg(r)

// Extract residues for linear gcd factors only.
interface Residue<T> extends Factor<T> {
	residue?:	Polynomial<rational>;
	residueK?:	Polynomial<PolyMod<rational>>;
}

export function rothsteinResidues<T extends canMakeRational>(N: Polynomial<Polynomial<T>>, D: Polynomial<Polynomial<T>>, gcds: FactorGCD<T>[]): Residue<T>[] {
	// Inputs are polynomials in x whose coefficients are polynomials in t (Polynomial<Polynomial<T>>).
	const logs: Residue<T>[] = [];

	const N1		= Polynomial(N.c.map(c => Polynomial(c.c.map(rational.from))));
	const Dd		= Polynomial(D.c.map(c => Polynomial(c.c.map(rational.from)))).deriv();

	for (const ginfo of gcds) {
		const G = ginfo.gcd; // Polynomial whose coefficients are inner polynomials in t
		if (!G || G.degree() < 1)
			continue;

		// inner coefficient structure for G collected when needed

		// prepare linear coeffs if present
		const aInner = G.c[1];
		const bInner = G.c[0];

		// Global gcd case (resultant zero placeholder): handle without Mod wrapper
		if (ginfo.factor.degree() < 0) {
			if (G.degree() === 1 && aInner && aInner.degree() === 0 && aInner.c[0]) {
				const x0 = bInner.scale(-1).rscale(aInner.c[0]);
				const e = Dd.evaluate(x0);
				if (e.degree() === 0 && e.c[0])
					logs.push({ factor: ginfo.factor.dup(), residue: N1.evaluate(x0).rscale(e.c[0]), multiplicity: ginfo.multiplicity });
			}
			continue;
		}

		const r		= ginfo.factor;
		const r1	= Polynomial(r.c.map(rational.from));
		const ModLocal = PolyModFactory(r1);
		const DdMod	= Dd.map(c => ModLocal.wrap(c));
		const Nmod	= N1.map(c => ModLocal.wrap(c));

		// If gcd is linear we can directly compute residue in K; otherwise
		// attempt to factor G further using the Q-linear splitter to find extension roots.
		if (G.degree() === 1) {
			const invA = aInner && ModLocal.wrap(aInner).recip();
			if (invA) {
				const x0		= ModLocal.wrap(bInner).neg().mul(invA);
				const invDen	= DdMod.evaluate(x0);
				if (invDen)
					logs.push({ factor: r.dup(), residue: Nmod.evaluate(x0).mul(invDen).v, multiplicity: ginfo.multiplicity });
			}

		} else {
			// Attempt adjoin-root splitting of G over K to obtain residues in extensions
			const parts = factorSquareFreeOverK_Q(G, r1);

			// If splitter returned the same irreducible integer polynomial, treat as no split and use const-coeff fallback
			const partsLooksSame = parts && parts.length === 1 && !(parts[0] as any).ext && (parts[0] as PolyPolyR).toString() === G.toString();
			if (parts && parts.length && !partsLooksSame) {
				for (const part of parts) {
					if ((part as any).ext) {
						const ext		= (part as any).ext as { alpha: KzMod; pCoeffs: number[]; quotient: KzMod[] };
						const ModZ		= ModFactory(Polynomial(ext.pCoeffs.map(ci => ModLocal.wrap(Polynomial([rational.from(ci)])))));
						const invDen	= invertKz(evaluateAtKz(Dd, ext.alpha, ModZ, ModLocal), ModZ, ext.pCoeffs.length - 1, ModLocal);
						if (invDen)
							logs.push({ factor: r.dup(), residueK: evaluateAtKz(N1, ext.alpha, ModZ, ModLocal).mul(invDen), multiplicity: ginfo.multiplicity });
					} else {
						const pf = part as PolyPolyR;
						if (pf.degree() === 1) {
							const invA = pf.c[1] && ModLocal.wrap(pf.c[1]).recip();
							if (invA) {
								const x0		= ModLocal.wrap(pf.c[0]).neg().mul(invA);
								const invDen	= DdMod.evaluate(x0).recip();
								if (invDen)
									logs.push({ factor: r.dup(), residue: Nmod.evaluate(x0).mul(invDen).v, multiplicity: ginfo.multiplicity });
							}
						}
					}
				}
			} else {
				// If splitter returned nothing interesting, but G has constant (t-free) coefficients, we can adjoin a root ζ of the integer polynomial G and evaluate residues in Q(ζ)
				let allConst = true;
				const coeffNums: number[] = [];
				for (const i in G.c) {
					const ci = G.c[i];
					if (ci.degree() >= 0) {
						if (ci.degree() > 0 || ci.c[0].den !== 1) {
							allConst = false;
							break;
						}
						coeffNums[i] = ci.c[0].num;
					}
				}
				if (allConst) {
					const alpha		= Polynomial(Array.from({ length: coeffNums.length }, (_, i) => ModLocal.wrap(Polynomial([rational.from(i === 1 ? 1 : 0)]))));
					const ModZ		= ModFactory(Polynomial(coeffNums.map(ci => ModLocal.wrap(Polynomial([rational.from(ci)])))));
					const invDen	= invertKz(evaluateAtKz(Dd, alpha, ModZ, ModLocal), ModZ, coeffNums.length - 1, ModLocal);
					if (invDen)
						logs.push({ factor: r.dup(), residueK: evaluateAtKz(N1, alpha, ModZ, ModLocal).mul(invDen), multiplicity: ginfo.multiplicity });
				}
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
