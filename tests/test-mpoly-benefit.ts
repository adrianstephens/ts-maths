import * as assert from 'assert';
import { symbolic } from '../dist/symbolic';
import { Polynomial as polynomial, polyGCDT, resultantT } from '../dist/polynomial';

// Test 1: resultant should be identically zero when the two polynomials share a common factor (x - t)
(() => {
    const x = symbolic.variable('x');
    const t = symbolic.variable('t');

    const pExpr = x.sub(t).mul(x.sub(1));
    const qExpr = x.sub(t).mul(x.sub(2));

    const pGroups = pExpr.expand().collect('x');
    const qGroups = qExpr.expand().collect('x');

    const P = polynomial<symbolic>(pGroups.map(g => g ?? symbolic.from(0)));
    const Q = polynomial<symbolic>(qGroups.map(g => g ?? symbolic.from(0)));

    const R = resultantT(P, Q);

    // numeric sanity checks at sample t values; if the symbolic representation
    // is not structurally zero we still require numeric evaluation to vanish
    console.log('DEBUG: P =', P.toString());
    console.log('DEBUG: Q =', Q.toString());
    console.log('DEBUG: resultant (symbolic) =', String(R));
    for (const v of [0, 1, 2, 3, -1.5]) {
        const rv = R.evaluate({ t: v });
        console.log(`DEBUG: resultant at t=${v} ->`, rv);
        assert(Math.abs(rv) < 1e-12, `resultant(t=${v}) expected 0, got ${rv}`);
    }

    if (!R.eq(symbolic.zero) && String(R) !== String(symbolic.zero))
        console.log('note: resultant string:', String(R));

    console.log('mpoly-benefit: resultant-zero test passed');
})();

// Test 2: polyGCDT should recover the common factor (x - t)
(() => {
    const x = symbolic.variable('x');
    const t = symbolic.variable('t');

    const pExpr = x.sub(t).mul(x.sub(1));
    const qExpr = x.sub(t).mul(x.sub(2));

    const P = polynomial<symbolic>(pExpr.expand().collect('x').map(g => g ?? symbolic.from(0)));
    const Q = polynomial<symbolic>(qExpr.expand().collect('x').map(g => g ?? symbolic.from(0)));

    const G = polyGCDT(P, Q);

    // expected gcd is (x - t) -> build expected polynomial
    const expectedExpr = x.sub(t);
    const Expected = polynomial<symbolic>(expectedExpr.expand().collect('x').map(g => g ?? symbolic.from(0)));

    // structural check via string representation (safe fallback if no deep eq)
    const gs = G.toString();
    const es = Expected.toString();
    assert(gs === es, `gcd mismatch: got ${gs}, expected ${es}`);

    // numeric sanity: gcd should evaluate to ~0 when x == t
    const gAt = G.evaluate(t); // evaluate polynomial in x at x = t gives symbolic expression in t
    const val = gAt.evaluate({ t: 2 });
    // when substituting x=t the gcd polynomial should be zero for any t; we check for t=2
    assert(Math.abs(val) < 1e-12, `gcd evaluated at x=t expected 0, got ${val}`);

    console.log('mpoly-benefit: polyGCDT common-factor test passed');
})();

console.log('ok test-mpoly-benefit.ts');
