"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.evaluateMod = evaluateMod;
exports.polyGCD = polyGCD;
exports.squareFreeFactorization = squareFreeFactorization;
exports.factorOverK = factorOverK;
exports.rothsteinPartial = rothsteinPartial;
exports.rothsteinResidues = rothsteinResidues;
exports.interpolate = interpolate;
exports.sylvesterMatrix = sylvesterMatrix;
exports.resultant = resultant;
exports.discriminant = discriminant;
exports.vietasFormulas = vietasFormulas;
exports.partialFractions = partialFractions;
exports.partialFractionsT = partialFractionsT;
exports.partialPower = partialPower;
exports.hermiteReduce = hermiteReduce;
const core_1 = require("./core");
const polynomial_1 = require("./polynomial");
const bareiss_1 = require("./bareiss");
const real_1 = __importDefault(require("./real"));
const integer_1 = __importDefault(require("./integer"));
const rational_1 = __importStar(require("./rational"));
const gen_1 = __importStar(require("./gen"));
const debug = process && process.env && process.env.DEBUG_RATIONAL;
// Matrix multiply and trace helpers (rational arithmetic)
function matMul(A, B) {
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
function traceMat(A) {
    const N = A.length;
    let s = A[0][0];
    for (let i = 1; i < N; i++)
        s = s.add(A[i][i]);
    return s;
}
function PolyMod(r) {
    return (r.is(real_1.default.is)
        ? realMod(r)
        : TMod(r));
    function TMod(r) {
        class M extends (0, gen_1.ModFactory)(r) {
            //static wrap(p: Polynomial<U>) {
            //	p = p.dup();
            //	p.divmod(r);
            //	return new M(p);
            //}
            degree() { return this.v.degree(); }
            mul(B) {
                const raw = [];
                const a = this.v.c;
                const b = B.v.c;
                for (const i in a) {
                    const ai = a[i];
                    for (const j in b) {
                        const k = +i + +j;
                        raw[k] = raw[k] ? raw[k].add(ai.mul(b[j])) : ai.mul(b[j]);
                    }
                }
                const s = r.c;
                const n = r.degree();
                const lead = s[n];
                for (let k = raw.length - 1; k >= n; k--) {
                    const coeff = raw[k];
                    if (coeff) {
                        const factor = coeff.div(lead);
                        for (let i = 0; i < n; i++) {
                            if (s[i]) {
                                const idx = k - n + i;
                                raw[idx] = raw[idx] ? raw[idx].sub(factor.mul(s[i])) : factor.mul(s[i]).neg();
                            }
                        }
                    }
                }
                return this._create((0, polynomial_1.Polynomial)(raw));
            }
        }
        ;
        return M;
    }
    function realMod(r) {
        class M extends (0, gen_1.ModFactory)(r) {
            degree() { return this.v.degree(); }
            mul(B) {
                const a = this.v.c;
                const b = B.v.c;
                const raw = [];
                for (const i in a) {
                    const ai = a[i];
                    for (const j in b) {
                        const k = +i + +j;
                        raw[k] = (raw[k] ?? 0) + ai * b[j];
                    }
                }
                const s = r.c;
                const n = r.degree();
                const lead = s[n];
                for (let k = raw.length - 1; k >= n; k--) {
                    const coeff = raw[k];
                    if (coeff) {
                        const factor = coeff / lead;
                        for (let i = 0; i < n; i++) {
                            if (s[i]) {
                                const idx = k - n + i;
                                raw[idx] = (raw[idx] ?? 0) - factor * s[i];
                            }
                        }
                    }
                }
                return this._create((0, polynomial_1.Polynomial)(raw));
            }
        }
        ;
        return M;
    }
}
// companion-style multiply-by-x reduction: compute x * a mod m
function mulXModT(a, m) {
    const raw = [];
    for (const i in a.c)
        raw[+i + 1] = a.c[i];
    const factor = a.c[m.degree() - 1]; // ? zero : a.c[n - 1].div(m.c[n]);
    if (factor) {
        for (const i in a.c) {
            const mi = m.c[i];
            if (!mi)
                continue;
            raw[i] = raw[i] ? raw[i].sub(factor.mul(mi)) : factor.mul(mi).neg();
        }
    }
    return (0, polynomial_1.Polynomial)(raw);
}
function mulXModN(a, m) {
    const raw = [];
    for (const i in a.c)
        raw[+i + 1] = a.c[i];
    const factor = a.c[m.degree() - 1];
    if (factor) {
        for (const i in a.c)
            raw[i] = (raw[i] ?? 0) - factor * m.c[i];
    }
    return (0, polynomial_1.Polynomial)(raw);
}
function mulXMod(a, m) {
    return a.is(real_1.default.is)
        ? mulXModN(a, m)
        : mulXModT(a, m);
}
// Evaluate polynomial p(x) where p is polynomial in x with coefficients in t-polynomials
// x0 is an element of K represented as Polynomial<number> (polynomial in t). Reduce intermediate results mod r.
function evaluateMod(p, x, m) {
    if (p.c.length === 0)
        return (0, polynomial_1.Polynomial)([]);
    let i = p.c.length - 1;
    let res = p.c[i];
    while (i--) {
        res = res.mul(x).add(p.c[i]);
        res.divmod(m);
    }
    return res;
}
//-----------------------------------------------------------------------------
//	PRS: Polynomial Remainder Sequence and GCD
//-----------------------------------------------------------------------------
function polyGCD(A, B) {
    return A.is(real_1.default.is)
        ? gen_1.default.gcd(A, B)
        : polyGCDT(A, B);
}
function polyGCDT(A, B) {
    const seq = subresultantPRST(A, B);
    for (let i = seq.length; i--;) {
        if (seq[i].degree() >= 0)
            return seq[i].rscale(seq[i].leadCoeff()); //normalise();
    }
    return A.from(1);
}
// Build a simple subresultant PRS sequence using pseudoRemainder.
function subresultantPRST(A, B) {
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
function squareFreeFactorization(f) {
    return f.is(real_1.default.is)
        ? squareFreeFactorizationN(f)
        : squareFreeFactorizationT(f);
}
// Return array of { factor, multiplicity } for a polynomial f using primitive PRS gcd
function squareFreeFactorizationN(f) {
    const res = [];
    if (f.degree() <= 0)
        return res;
    let g = polyGCD(f, f.deriv());
    // ensure g is primitive (remove content) so subsequent divmod/pseudo-division behaves
    const gcont = g.content();
    if (gcont)
        g.selfRscale(gcont);
    let w = f.dup().divmod(g);
    for (let i = 1; i < 256 && w.degree() > 0; i++) {
        const y = polyGCD(w, g);
        const fi = w.divmod(y);
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
function squareFreeFactorizationT(f) {
    const res = [];
    if (f.degree() <= 0)
        return res;
    //let g = polyGCDT(f, f.deriv());
    let g = gen_1.default.gcd(f, f.deriv());
    if (g.degree() === 0) {
        res.push({ factor: f.dup(), multiplicity: 1 });
        return res;
    }
    let w = f.dup().divmod(g);
    for (let i = 1; i < 256 && w.degree() > 0; i++) {
        //const y = polyGCDT(w, g);
        const y = gen_1.default.gcd(w, g);
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
function solveRelationFromCols(cols, m, n, zero) {
    if (real_1.default.is(cols[0][0])) {
        const sol = (0, bareiss_1.solveRectangularBareiss)(Array.from({ length: n }, (_, row) => Array.from({ length: m }, (_, col) => cols[col][row] ?? zero)), [Array.from({ length: n }, (_, row) => cols[m][row] ?? zero)]);
        return sol ? sol[0] : null;
    }
    else {
        const sol = (0, bareiss_1.solveRectangularBareissT)(Array.from({ length: n }, (_, row) => Array.from({ length: m }, (_, col) => cols[col][row] ?? zero)), [Array.from({ length: n }, (_, row) => cols[m][row] ?? zero)]);
        return sol ? sol[0] : null;
    }
}
// Factor a square-free polynomial S over K into irreducible factors over K
function factorSquareFreeOverK(S, coeffsToTry = [-2, -1, 1, 2]) {
    const N = S.degree();
    if (N <= 1)
        return [S.dup()];
    const nums = S.is(real_1.default.is);
    const one = nums ? 1 : S.c[0].from(1);
    const zero = nums ? 0 : one.scale(0);
    const neg = nums ? (x) => -(x ?? 0) : (x) => x ? x.neg() : zero;
    const from = nums ? (x) => x : (x) => one.from(x);
    const mod = PolyMod(S);
    function solveRelation(cols) {
        // find smallest m such that cols[0..m-1] are linearly dependent with cols[m]
        for (let m = 1; m <= N; m++) {
            const alpha = solveRelationFromCols(cols, m, N, zero);
            if (alpha)
                return (0, polynomial_1.Polynomial)(Array.from({ length: m + 1 }, (_, i) => i == m ? one : neg(alpha[i])));
        }
        // no relation found => minimal polynomial is x^{n} (deg n)
        return (0, polynomial_1.Polynomial)([one]).shift(N);
    }
    // Compute minimal annihilating polynomial for the basis element x^basisIdx in A = K[x]/(S).
    function minimalPolynomialForBasis(basisIdx) {
        // produce sequence v_k = x^k * basis / S reduced, represented as coefficient vectors length n
        const cols = [];
        let p = (0, polynomial_1.Polynomial)([one]).shift(basisIdx);
        for (let k = 0; k <= N; k++) {
            cols.push(p.c.slice(0, N));
            p = mulXMod(p, S);
        }
        return cols;
    }
    // Compute minimal polynomial of multiplication-by-`elem` operator on A = K[x]/(S)
    function minimalPolynomialForElement(elem) {
        // produce sequence v_k = elem^k * 1 mod S
        const cols = [];
        let p = mod.wrap((0, polynomial_1.Polynomial)([one]));
        for (let k = 0; k <= N; k++) {
            cols.push(p.v.c.slice());
            p = p.mul(elem);
        }
        return cols;
    }
    // Compute minimal polynomial of multiplication-by-x operator on A = K[x]/(S)
    function minimalPolynomialOperator() {
        // choose deterministic initial vector v = 1 + x + x^2 + ...
        const cols = [];
        let p = (0, polynomial_1.Polynomial)(Array.from({ length: N }, () => one));
        for (let k = 0; k <= N; k++) {
            cols.push(p.c.slice(0, N));
            p = mulXMod(p, S);
        }
        return cols;
    }
    // compute minimal polynomial of multiplication operator (deterministic)
    const lcmPoly = solveRelation(minimalPolynomialOperator());
    if (!lcmPoly)
        return [S.dup()];
    // gcd between S and lcmPoly may reveal a nontrivial factor
    const g = gen_1.default.gcd(S, lcmPoly);
    if (g.degree() > 0 && g.degree() < S.degree())
        return factorSquareFreeOverK(g).concat(factorSquareFreeOverK(S.divmod(g)));
    // If operator minimal polynomial did not split S, try per-basis minimal polynomials (annihilators of basis elements)
    // These can reveal nontrivial factors that the operator polynomial misses
    for (let idx = 0; idx < N; idx++) {
        const m = solveRelation(minimalPolynomialForBasis(idx));
        if (m) {
            const gg = gen_1.default.gcd(S, m);
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
            const gg = gen_1.default.gcd(S, solveRelation(minimalPolynomialForElement((0, polynomial_1.Polynomial)([from(a)]).shift(i))));
            if (gg.degree() > 0 && gg.degree() < S.degree())
                return factorSquareFreeOverK(gg).concat(factorSquareFreeOverK(S.divmod(gg)));
        }
    }
    for (let i = 0; i < N; i++) {
        for (let j = i + 1; j < N; j++) {
            for (const a of coeffsToTry) {
                for (const b of coeffsToTry) {
                    const vec = [];
                    vec[i] = from(a);
                    vec[j] = from(b);
                    const gg = gen_1.default.gcd(S, solveRelation(minimalPolynomialForElement((0, polynomial_1.Polynomial)(vec))));
                    if (gg.degree() > 0 && gg.degree() < S.degree())
                        return factorSquareFreeOverK(gg).concat(factorSquareFreeOverK(S.divmod(gg)));
                }
            }
        }
    }
    // as a last deterministic attempt, try the element equal to x (the residue class)
    const ggX = gen_1.default.gcd(S, solveRelation(minimalPolynomialForElement((0, polynomial_1.Polynomial)([zero, one]))));
    if (ggX.degree() > 0 && ggX.degree() < S.degree())
        return factorSquareFreeOverK(ggX).concat(factorSquareFreeOverK(S.divmod(ggX)));
    // cannot split further, assume irreducible
    return [S.dup()];
}
function factorOverK(f) {
    const sf = squareFreeFactorization(f);
    const res = [];
    for (const item of sf) {
        const pieces = factorSquareFreeOverK(item.factor);
        for (const p of pieces)
            res.push({ factor: p, multiplicity: item.multiplicity });
    }
    return res;
}
function factorSquareFreeOverK_Q(S, r) {
    // Deterministic Q-linear splitter: build exact multiplication-by-x matrix over Q
    // on basis { t^i x^j } and compute its characteristic polynomial exactly. Use
    // that polynomial's factors to derive gcds with S and split S over K.
    let n = S.degree();
    const dr = r.degree();
    if (n <= 1 || dr <= 0)
        return [S.dup()];
    const Mod = PolyMod(r);
    // Basis size
    const N = n * dr;
    const q0 = rational_1.default.from(0);
    const q1 = rational_1.default.from(1);
    // Copy Scoeffs (coefficients of S in x as polynomials in t)
    //	let Scoeffs = Array.from({ length: n + 1 }, (_, i) => S.c[i] ? S.c[i].dup() : Polynomial<rational>([]));
    // Reduce effective degree in K = Q[t]/(r) by checking coefficients modulo r
    let ScoeffsMod = S.c.map(ci => Mod.wrap(ci));
    let effN = ScoeffsMod.length - 1;
    while (effN >= 0 && (ScoeffsMod[effN].degree() < 0))
        effN--;
    if (effN < 0)
        return [S.dup()];
    if (effN < n) {
        ScoeffsMod = ScoeffsMod.slice(0, effN + 1);
        n = effN;
    }
    const Slead = ScoeffsMod[n];
    if (!Slead || Slead.degree() < 0)
        return [S.dup()];
    // Build multiplication matrix M (rows x cols) initialized to zero
    const M = Array.from({ length: N }, () => Array.from({ length: N }, () => q0));
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
                if (!ck)
                    console.log('DBG factorSquareFreeOverK_Q: ck missing at k=', k, 'n=', n, 'Scoeffs.length=', ScoeffsMod.length);
                const ckMod = ck.mul(invLeadMod).neg(); // element of K
                // multiply by t^i and reduce mod r
                const rem = ckMod.v.shift(i).divmod(r);
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
    // Compute traces of powers: trace(M^k) for k=1..N
    const traces = [];
    let P = M.map(rw => rw.slice());
    for (let k = 1; k <= N; k++) {
        traces[k] = traceMat(P);
        if (k < N)
            P = matMul(P, M);
    }
    // Faddeev–LeVerrier: compute characteristic polynomial coefficients a1..aN
    const a = Array.from({ length: N + 1 }, () => rational_1.default.from(0));
    for (let k = 1; k <= N; k++) {
        let sum = traces[k];
        for (let i = 1; i <= k - 1; i++)
            sum = sum.add(a[i].mul(traces[k - i]));
        a[k] = sum.neg().div(rational_1.default.from(k));
    }
    // Clear denominators to obtain integer polynomial for factoring
    const dens = a.slice(1).map(x => x.den);
    const lcmDen = dens.length ? real_1.default.lcm(...dens) : 1;
    const intCoeffs = [1];
    for (let k = 1; k <= N; k++) {
        const coeff = a[k];
        intCoeffs.push(coeff.num * (lcmDen / coeff.den));
    }
    const factors = squareFreeFactorization((0, polynomial_1.Polynomial)(intCoeffs));
    // Try splitting S using each charpoly factor by forming a Mod-wrapped polynomial and gcd
    const SmodWrapped = (0, polynomial_1.Polynomial)(S.c.map(ci => Mod.wrap(ci)));
    for (const fac of factors) {
        const pf = fac.factor;
        const wrapped = (0, polynomial_1.Polynomial)(pf.c.map(ci => Mod.wrap((0, polynomial_1.Polynomial)([rational_1.default.from(ci)]))));
        const gg = gen_1.default.gcd(SmodWrapped, wrapped);
        if (gg.degree() > 0 && gg.degree() < S.degree()) {
            // recurse on the found factor and complementary factor
            const left = (0, polynomial_1.Polynomial)(gg.c.map((c) => c ? c.v : (0, polynomial_1.Polynomial)([])));
            const rightMod = SmodWrapped.divmod(gg);
            const right = (0, polynomial_1.Polynomial)(rightMod.c.map((c) => c ? c.v : (0, polynomial_1.Polynomial)([])));
            return factorSquareFreeOverK_Q(left, r).concat(factorSquareFreeOverK_Q(right, r));
        }
    }
    return [S.dup()];
}
function rothsteinPartial(N, D) {
    const zero = N.leadCoeff().scale(0);
    const results = [];
    const Dd = D.deriv();
    // Q(x,t) = N(x) - t * D'(x)  => coefficient at x^i is Ni(t) - t * Dd_i(t)
    const Q = (0, polynomial_1.Polynomial)(Array.from({ length: Math.max(N.c.length, Dd.c.length) }, (_, i) => (N.c[i] ?? zero).sub((Dd.c[i] ?? zero).shift(1))));
    const R = resultant(D, Q);
    // Special-case when resultant is identically zero: resultant 0 => R has no square-free factors
    // but D and Q may still have a non-trivial gcd for all t. Detect that case and record the
    // gcd so downstream residue extraction can handle it.
    if (!R) {
        const G = gen_1.default.gcd(D, Q);
        if (G.degree() >= 0)
            results.push({
                factor: (0, polynomial_1.Polynomial)([]),
                multiplicity: 1,
                gcd: (0, polynomial_1.Polynomial)(G.c.map(c => (0, polynomial_1.Polynomial)(c.c.map(t => rational_1.default.from(t)))))
            });
        return { R, factors: [], gcds: results };
    }
    const factors = squareFreeFactorization(R);
    // build polynomials in x with inner coeffs polynomials in t
    const D1 = (0, polynomial_1.Polynomial)(D.c.map(c => (0, polynomial_1.Polynomial)(c.c.map(t => rational_1.default.from(t)))));
    for (const fac of factors) {
        const r = fac.factor;
        const mod = (0, gen_1.ModFactory)(r);
        // Wrap each inner Polynomial<T> into the Mod wrapper for K = Q[t]/(r)
        // construct inner polynomial representing N_i + t * D'_i as Polynomial([Ni, Di])
        const G = gen_1.default.gcd((0, polynomial_1.Polynomial)(D.c.map(c => mod.wrap(c))), (0, polynomial_1.Polynomial)(Q.c.map(c => mod.wrap(c))));
        const G1 = (0, polynomial_1.Polynomial)(G.c.map(c => (0, polynomial_1.Polynomial)(c.v.c.map(t => rational_1.default.from(t)))));
        const r1 = (0, polynomial_1.Polynomial)(r.c.map(c => rational_1.default.from(c)));
        const sf = squareFreeFactorization(G1);
        const Gfactors = [];
        for (const item of sf) {
            const pieces = factorSquareFreeOverK_Q(item.factor, r1);
            for (const p of pieces)
                Gfactors.push({ factor: p, multiplicity: item.multiplicity });
        }
        if (Gfactors.length === 0) {
            // Try adjoin-root (primitive-element) approach: factor full D over K = Q[t]/(r).
            // This attempts to find x-factors of D that only appear after adjoining a root of r(t).
            // If that yields nontrivial factors, use them; otherwise fall back to recording the raw gcd we computed.
            const Dfactors = factorSquareFreeOverK_Q(D1, r1);
            if (Dfactors.length > 0) {
                for (const df of Dfactors)
                    results.push({ factor: r.dup(), multiplicity: fac.multiplicity, gcd: df });
            }
            else {
                results.push({ factor: r.dup(), multiplicity: fac.multiplicity, gcd: G1 });
            }
        }
        else {
            for (const gf of Gfactors)
                results.push({ factor: r.dup(), multiplicity: fac.multiplicity * gf.multiplicity, gcd: gf.factor });
        }
    }
    return { R, factors, gcds: results };
}
function rothsteinResidues(N, D, gcds) {
    // Inputs are polynomials in x whose coefficients are polynomials in t (Polynomial<Polynomial<T>>).
    const logs = [];
    const N1 = (0, polynomial_1.Polynomial)(N.c.map(c => (0, polynomial_1.Polynomial)(c.c.map(rational_1.default.from))));
    const Dd = (0, polynomial_1.Polynomial)(D.c.map(c => (0, polynomial_1.Polynomial)(c.c.map(rational_1.default.from)))).deriv();
    for (const ginfo of gcds) {
        const G = ginfo.gcd; // Polynomial whose coefficients are inner polynomials in t
        if (!G || G.degree() < 1)
            continue;
        const aInner = G.c[1];
        const bInner = G.c[0];
        if (!aInner)
            continue;
        // Global gcd case (resultant zero placeholder): handle without Mod wrapper
        if (ginfo.factor.degree() < 0) {
            if (G.degree() === 1 && aInner.degree() === 0 && aInner.c[0]) {
                const x0 = bInner.scale(-1).rscale(aInner.c[0]);
                const e = Dd.evaluate(x0);
                if (e.degree() === 0 && e.c[0])
                    logs.push({ factor: ginfo.factor.dup(), residue: N1.evaluate(x0).rscale(e.c[0]), multiplicity: ginfo.multiplicity });
            }
            continue;
        }
        const r = ginfo.factor;
        const r1 = (0, polynomial_1.Polynomial)(r.c.map(rational_1.default.from));
        const mod = (0, gen_1.ModFactory)(r1);
        // handle only linear gcds (a(t) x + b(t)) — other splitting should be done in rothsteinPartial
        if (G.degree() !== 1)
            continue;
        const invA = mod.wrap(aInner).recip();
        if (!invA)
            continue;
        const x0 = mod.wrap(bInner).neg().mul(invA).v;
        const invDenMod = mod.wrap(evaluateMod(Dd, x0, r1)).recip();
        if (invDenMod) {
            const residue = evaluateMod(N1, x0, r1).mul(invDenMod.v);
            residue.divmod(r1);
            logs.push({ factor: r.dup(), residue, multiplicity: ginfo.multiplicity });
        }
    }
    return logs;
}
//-----------------------------------------------------------------------------
//	Interpolation polynomials
//-----------------------------------------------------------------------------
function interpolate(points, one) {
    if (points.length === 0)
        return (0, polynomial_1.Polynomial)([]);
    if (typeof points[0][0] === 'number')
        return interpolateN(points);
    if (!one && (0, core_1.isScalar)(points[0][0]))
        one = points[0][0].from(1);
    return interpolateT(points, one);
}
// interpolate polynomial (monomial basis) from points (x,y)
function interpolateN(points) {
    const res = (0, polynomial_1.Polynomial)([]);
    for (const [xi, yi] of points) {
        // basis polynomial Li with value 1 at xi and 0 at others
        let L = (0, polynomial_1.Polynomial)([1]);
        let denom = 1;
        for (const [xj, _yj] of points) {
            if (xi !== xj) {
                L = L.mul((0, polynomial_1.Polynomial)([-xj, 1]));
                denom *= xi - xj;
            }
        }
        if (Math.abs(denom) > 1e-18)
            res.selfAdd(L.scale(yi / denom));
    }
    return res;
}
function interpolateT(points, one) {
    const res = (0, polynomial_1.Polynomial)([]);
    for (const [xi, yi] of points) {
        // basis polynomial Li with value 1 at xi and 0 at others
        let L = (0, polynomial_1.Polynomial)([one]);
        let denom = one;
        for (const [xj, _yj] of points) {
            if (xi !== xj) {
                L = L.mul((0, polynomial_1.Polynomial)([xj.neg(), one]));
                denom = denom.mul(xi.sub(xj));
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
function sylvesterMatrix(p, q, zero) {
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
function resultantN(p, q) {
    const pd = p.degree();
    const qd = q.degree();
    if (pd < 0 || qd < 0)
        return 0;
    if (pd + qd === 0)
        return 1;
    const M = sylvesterMatrix(p.c, q.c, 0);
    const n = M.length;
    const { swaps } = (0, bareiss_1.LUDecomposeBareiss)(M, true);
    return swaps & 1 ? -M[n - 1][n - 1] : M[n - 1][n - 1];
}
// discriminant = (-1)^{n(n-1)/2} * (1/leadCoeff) * resultant(p, p')
function discriminantN(p) {
    const n = p.degree();
    if (n < 1)
        return 0;
    const R = resultantN(p, p.deriv());
    return (((n * (n - 1))) & 2 ? -R : R) / p.leadCoeff();
}
function resultantT(p, q) {
    const pd = p.degree();
    const qd = q.degree();
    if (pd >= 0 && qd >= 0) {
        const one = p.leadCoeff().from(1);
        if (pd + qd === 0)
            return one;
        const M = sylvesterMatrix(p.c, q.c, one.scale(0));
        const n = M.length;
        const { swaps } = (0, bareiss_1.LUDecomposeBareissT)(M, true);
        return swaps & 1 ? M[n - 1][n - 1].neg() : M[n - 1][n - 1];
    }
}
function discriminantT(p) {
    const n = p.degree();
    if (n > 0) {
        const R = resultantT(p, p.deriv());
        if (R)
            return (((n * (n - 1))) & 2 ? R.neg() : R).div(p.leadCoeff());
    }
}
// Generic wrapper for resultant: dispatch to numeric or generic variant
function resultant(p, q) {
    return p.is(real_1.default.is)
        ? resultantN(p, q)
        : resultantT(p, q);
}
function discriminant(p) {
    return p.is(real_1.default.is)
        ? discriminantN(p)
        : discriminantT(p);
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
function vietasFormulas(poly, roots) {
    const n = roots.length;
    if (n !== poly.c.length)
        throw new Error('Number of roots must match number of coefficients');
    const equations = [];
    // Generate elementary symmetric polynomials σₖ
    for (let k = 1; k <= n; k++) {
        // sigma starts at zero of the appropriate type
        const sigma = poly.c[0].from(0);
        const indices = Array.from({ length: k }, (_, i) => i);
        // Build sigma by accumulating products over all k-element subsets of roots
        let accum = poly.c[0].from(0);
        function generateAndAccumulate(start, depth) {
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
function partialFractions(r) {
    if (r.den.degree() <= 0)
        return undefined;
    const rem = r.num.dup();
    const _polyPart = rem.divmod(r.den);
    const roots = r.den.realRoots();
    if (!roots || roots.length !== r.den.degree())
        return undefined;
    const terms = [];
    const dden = r.den.deriv();
    for (const r of roots) {
        const denomDer = dden.evaluate(r);
        if (Math.abs(denomDer) < 1e-12)
            return undefined;
        terms.push({ root: r, residue: rem.evaluate(r) / denomDer });
    }
    return { polyPart: _polyPart, terms };
}
// Generic exact partial-fraction decomposition using maths primitives.
// Decomposes numer/denom over scalar type T into polynomial part and partial-fraction terms with polynomial numerators for each factor power.
function partialFractionsT(r) {
    const rows = r.den.degree();
    if (rows === 0)
        return undefined;
    const rem = r.num.dup();
    const _polyPart = rem.divmod(r.den);
    const _rden = r.den.dup();
    const factors = squareFreeFactorization(_rden);
    // build unknowns
    const unknowns = [];
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
    let solVec;
    if (rows === cols) {
        const Amat = A.map(r => r.slice());
        const { perm } = (0, bareiss_1.LUDecomposeBareissT)(Amat, true);
        // optional debug output when running with DEBUG_RATIONAL=1
        if (debug) {
            try {
                console.log('DIAG partialFractionsT: Amat rows:');
                for (const row of Amat)
                    console.log(row.map(x => x && x.toString ? x.toString() : String(x)).join(', '));
                console.log('DIAG partialFractionsT: perm =', perm);
            }
            catch (e) {
                // ignore diagnostic failures
            }
        }
        for (let i = 0; i < cols; i++)
            if (Amat[i][i].sign() === 0)
                return undefined;
        const res = (0, bareiss_1.LUSolveBareissTransposeMultiT)(Amat, [b.map(x => x)], perm);
        if (res.length !== 1 || res[0].length !== cols)
            return undefined;
        solVec = res[0];
        if (debug) {
            try {
                console.log('DIAG partialFractionsT: solVec =', solVec.map(x => x && x.toString ? x.toString() : String(x)).join(', '));
            }
            catch (e) { }
        }
    }
    else {
        const sol = (0, bareiss_1.solveRectangularBareissT)(A, [b]);
        if (!sol)
            return undefined;
        solVec = sol[0];
    }
    // map back to polynomial numerators
    const terms = [];
    let idx = 0;
    for (const fac of factors) {
        const d = fac.factor.degree();
        for (let k = 1; k <= fac.multiplicity; k++)
            terms.push({ factor: fac.factor, order: k, numer: (0, polynomial_1.Polynomial)(Array.from({ length: d }, () => solVec[idx++] ?? zero)) });
    }
    return { polyPart: _polyPart, terms };
}
// modifies r
function partialPower(r) {
    // try substitution when denominator is a polynomial in x^d and numerator matches du/dx pattern
    const degs = Object.keys(r.den.c).map(i => +i).filter(i => r.den.c[i].sign() !== 0);
    if (degs.length > 0) {
        const g = integer_1.default.gcd(...degs);
        if (g >= 2) {
            // try smallest divisor >1
            for (let d = 2; d <= g; d++) {
                if (g % d)
                    continue;
                // check numerator is single-term at power d-1
                if (r.num.c.every((c, i) => (i === d - 1) === (c.sign() !== 0))) {
                    const k = r.num.c[d - 1];
                    const reduceDen = [];
                    degs.filter(i => (i % d) === 0).forEach(i => reduceDen[i / d] = r.den.c[i]);
                    r.num = (0, polynomial_1.Polynomial)([k.div(k.from(d))]);
                    r.den = (0, polynomial_1.Polynomial)(reduceDen);
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
function hermiteReduce(r, one) {
    const pf = partialFractionsT(r);
    if (!pf)
        return undefined;
    const { polyPart, terms } = pf;
    const factors = squareFreeFactorization(r.den.dup());
    let termIdx = 0;
    const zero = one.scale(0);
    const derivativeTerms = [];
    const remainderNumerators = [];
    for (const facInfo of factors) {
        const f = facInfo.factor;
        const m = facInfo.multiplicity;
        const M = [];
        for (let j = 1; j <= m; j++) {
            const t = terms[termIdx++];
            M.push(t ? t.numer : (0, polynomial_1.Polynomial)(Array.from({ length: f.degree() }, () => zero)));
        }
        if (m === 1) {
            remainderNumerators.push(M[0]);
            continue;
        }
        const d = f.degree();
        const rows = d * m;
        const cols = m * d;
        const bCoeffs = [];
        for (let j = 1; j <= m; j++) {
            const prod = m === j ? M[j - 1] : M[j - 1].mul(gen_1.default.ipow(f, m - j));
            for (let r0 = 0; r0 < prod.c.length; r0++)
                bCoeffs[r0] = bCoeffs[r0] ? bCoeffs[r0].add(prod.c[r0]) : prod.c[r0];
        }
        const A = Array.from({ length: rows }, () => Array.from({ length: cols }, () => zero));
        const fDer = f.deriv();
        const fPows = [(0, polynomial_1.Polynomial)([one]), f];
        for (let j = 2; j <= m; j++)
            fPows[j] = fPows[j - 1].mul(f);
        for (let k = 1; k <= m - 1; k++) {
            for (let t = 0; t < d; t++) {
                const basis = (0, polynomial_1.Polynomial)([one]).shift(t);
                const contrib = basis.deriv().mul(fPows[m - k]).sub(basis.mul(fDer).mul(fPows[m - k - 1]).scale(one.scale(k)));
                const colIdx = (k - 1) * d + t;
                for (let row = 0; row < rows; row++)
                    A[row][colIdx] = contrib.c[row] ?? zero;
            }
        }
        for (let t = 0; t < d; t++) {
            const contrib = fPows[m - 1].shift(t);
            const colIdx = (m - 1) * d + t;
            for (let row = 0; row < rows; row++)
                A[row][colIdx] = contrib.c[row] ?? zero;
        }
        const sol = (0, bareiss_1.solveRectangularBareissT)(A, [bCoeffs]);
        if (!sol) {
            remainderNumerators.push(M[0]);
            continue;
        }
        const xvec = sol[0];
        const Nk = [];
        for (let k = 1; k <= m - 1; k++)
            Nk.push((0, polynomial_1.Polynomial)(Array.from({ length: d }, (_, t) => xvec[(k - 1) * d + t]) ?? zero));
        const D = (0, polynomial_1.Polynomial)(Array.from({ length: d }, (_, t) => xvec[(m - 1) * d + t]) ?? zero);
        for (let k = 1; k <= m - 1; k++)
            derivativeTerms.push((0, rational_1.rationalT)(Nk[k - 1], gen_1.default.ipow(f, k)));
        remainderNumerators.push(D);
    }
    // assemble S and R
    const S = factors.reduce((S, b) => S.mul(b.factor), (0, polynomial_1.Polynomial)([one]));
    const R = factors.reduce((R, b, i) => {
        const _other = S.divmod(b.factor);
        return remainderNumerators[i] ? R.add(remainderNumerators[i].mul(_other)) : R;
    }, (0, polynomial_1.Polynomial)([zero]));
    return { polyPart: polyPart, derivative: derivativeTerms, remainder: (0, rational_1.rationalT)(R, S) };
}
//# sourceMappingURL=factors.js.map