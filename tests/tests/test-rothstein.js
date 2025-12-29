"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const polynomial_1 = require("../src/polynomial");
const factors_1 = require("../src/factors");
const gen_1 = require("../src/gen");
function fail(msg, ...rest) {
    console.error(msg, rest);
    process.exit(2);
}
function checkResiduesInvariant(residues) {
    for (const r of residues) {
        if (r.factor.degree() < 0)
            continue; // global-resultant placeholder: no modulus to reduce by
        if (r.residue.degree() >= r.factor.degree())
            fail('residue degree not reduced modulo factor', r.factor.toString(), r.residue.toString());
    }
}
function verifyResiduesIdentity(N, D, partial, residues) {
    for (const res of residues) {
        // find corresponding gcd info from partial
        let ginfo = partial.gcds.find((g) => g.factor.toString() === res.factor.toString() && g.multiplicity === res.multiplicity);
        if (!ginfo) {
            // fallback: try matching just by factor string
            ginfo = partial.gcds.find((g) => g.factor.toString() === res.factor.toString());
        }
        if (!ginfo) {
            if (res.factor.degree() < 0) {
                // global-resultant case: try to find any gcd entry with degree>0
                ginfo = partial.gcds.find((g) => g.gcd && g.gcd.degree() >= 0);
            }
        }
        if (!ginfo)
            fail('verifyResiduesIdentity: cannot locate gcd info for residue', res.factor.toString());
        const G = ginfo.gcd; // Polynomial in x with inner polynomials in t
        if (G.degree() !== 1) {
            // only verify linear gcd residues
            continue;
        }
        const aInner = G.c[1];
        const bInner = G.c[0];
        if (!aInner)
            continue;
        if (res.factor.degree() < 0) {
            // global-case: compute x0 = -b/a (polynomial division by scalar a.c[0]) and verify directly
            if (aInner.degree() !== 0)
                continue;
            const a0 = aInner.c[0];
            if (a0 === 0)
                continue;
            const x0 = bInner.scale(-1).rscale(a0);
            const numer = N.evaluate(x0);
            const denom = D.deriv().evaluate(x0);
            const rhs = res.residue.mul(denom);
            if (!numer.eq(rhs))
                fail('residue identity failed (global)', res.factor.toString(), numer.toString(), rhs.toString());
            continue;
        }
        // modular case: use ModFactory and evaluate reduced remainders
        const mod = (0, gen_1.ModFactory)(res.factor);
        const aMod = mod.wrap(aInner);
        const bMod = mod.wrap(bInner);
        const invA = aMod.recip();
        if (!invA)
            continue;
        const x0mod = bMod.neg().mul(invA);
        const x0 = x0mod.v; // polynomial in t
        // compute remainders of N(x0) and D'(x0) modulo factor
        const numerRem = N.evaluate(x0).dup();
        numerRem.divmod(res.factor);
        const denomRem = D.deriv().evaluate(x0).dup();
        denomRem.divmod(res.factor);
        const rhs = res.residue.mul(denomRem).dup();
        rhs.divmod(res.factor);
        if (!numerRem.eq(rhs))
            fail('residue identity failed (mod)', res.factor.toString(), numerRem.toString(), rhs.toString());
    }
}
const zero = (0, polynomial_1.Polynomial)([0]);
const one = (0, polynomial_1.Polynomial)([1]);
const t = (0, polynomial_1.Polynomial)([0, 1]);
// Case 1: simple separable denominator D = x^2 - 1, N = 1
(function case1() {
    // represent coefficients as inner polynomials (Polynomial<Polynomial<number>>)
    const D = (0, polynomial_1.Polynomial)([(0, polynomial_1.Polynomial)([-1]), zero, one]);
    const N = (0, polynomial_1.Polynomial)([one]);
    const partial = (0, factors_1.rothsteinPartial)(N, D);
    const residues = (0, factors_1.rothsteinResidues)(N, D, partial.gcds);
    verifyResiduesIdentity(N, D, partial, residues);
    // `rothsteinResidues` always returns an array; no type check needed.
    checkResiduesInvariant(residues);
    console.log('case1 ok');
})();
// Case 2: global-resultant case D = x, N = t  (Q == 0 => gcd = D)
(function case2() {
    const D = (0, polynomial_1.Polynomial)([zero, one]); // x
    const N = (0, polynomial_1.Polynomial)([t]); // constant t
    const partial = (0, factors_1.rothsteinPartial)(N, D);
    const residues = (0, factors_1.rothsteinResidues)(N, D, partial.gcds);
    verifyResiduesIdentity(N, D, partial, residues);
    // Find global-resultant entry (factor degree < 0) and assert residue equals tInner
    const global = residues.find(r => r.factor.degree() < 0);
    if (!global)
        fail('case2: no global-resultant residue found');
    const g = global;
    if (g.residue.toString() !== t.toString())
        fail('case2: unexpected residue value', g.residue.toString(), t.toString());
    checkResiduesInvariant(residues);
    console.log('case2 ok');
})();
// Case 3: irreducible denominator D = x^2 + 1, N = 1 (sanity: no crash)
(function case3() {
    const D = (0, polynomial_1.Polynomial)([one, zero, one]); // x^2 + 1
    const N = (0, polynomial_1.Polynomial)([one]);
    const partial = (0, factors_1.rothsteinPartial)(N, D);
    const residues = (0, factors_1.rothsteinResidues)(N, D, partial.gcds);
    verifyResiduesIdentity(N, D, partial, residues);
    checkResiduesInvariant(residues);
    console.log('case3 ok');
})();
// Case 4: random small example D = x^2 - 2, N = 1
(function case4() {
    const D = (0, polynomial_1.Polynomial)([(0, polynomial_1.Polynomial)([-2]), zero, one]);
    const N = (0, polynomial_1.Polynomial)([one]);
    const partial = (0, factors_1.rothsteinPartial)(N, D);
    const residues = (0, factors_1.rothsteinResidues)(N, D, partial.gcds);
    verifyResiduesIdentity(N, D, partial, residues);
    checkResiduesInvariant(residues);
    console.log('case4 ok');
})();
// Case 5: D = x^2 - t  (inner linear coefficient), N = 1
(function case5() {
    const D = (0, polynomial_1.Polynomial)([t.neg(), zero, one]); // -t + 0*x + 1*x^2
    const N = (0, polynomial_1.Polynomial)([one]);
    const partial = (0, factors_1.rothsteinPartial)(N, D);
    const residues = (0, factors_1.rothsteinResidues)(N, D, partial.gcds);
    verifyResiduesIdentity(N, D, partial, residues);
    checkResiduesInvariant(residues);
    console.log('case5 ok');
})();
// Case 6: D = x - t^2, N = t
(function case6() {
    const D = (0, polynomial_1.Polynomial)([t.mul(t).neg(), one]); // -t^2 + x
    const N = (0, polynomial_1.Polynomial)([t]);
    const partial = (0, factors_1.rothsteinPartial)(N, D);
    const residues = (0, factors_1.rothsteinResidues)(N, D, partial.gcds);
    verifyResiduesIdentity(N, D, partial, residues);
    checkResiduesInvariant(residues);
    console.log('case6 ok');
})();
// Case 7: D = x^2 + (1 + t), N = 1  (inner constant has degree 1)
(function case7() {
    const D = (0, polynomial_1.Polynomial)([t.add(one), zero, one]); // (1+t) + 0*x + 1*x^2
    const N = (0, polynomial_1.Polynomial)([one]);
    const partial = (0, factors_1.rothsteinPartial)(N, D);
    const residues = (0, factors_1.rothsteinResidues)(N, D, partial.gcds);
    verifyResiduesIdentity(N, D, partial, residues);
    checkResiduesInvariant(residues);
    console.log('case7 ok');
})();
console.log('rothstein-all tests passed');
process.exit(0);
//# sourceMappingURL=test-rothstein.js.map