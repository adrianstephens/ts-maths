import { polynomial, polynomialT, polyGCDT } from '../src/polynomial';
import { rational } from '../src/rational';

console.log('running test-gcd-primitive.ts (diagnostics enabled)');

// gcd((x-1)^3*(x-2), (x-1)^2*(x-3)) -> (x-1)^2  (with scaled integer coefficients)
(function() {
    const base1 = new polynomial([ -1, 1 ]); // x-1
    const base2 = new polynomial([ -2, 1 ]); // x-2
    const base3 = new polynomial([ -3, 1 ]); // x-3

    const p1 = base1.mul(base1).mul(base1).mul(base2); // (x-1)^3 * (x-2)
    const p2 = base1.mul(base1).mul(base3); // (x-1)^2 * (x-3)

    // scale coefficients by 2 to create content > 1 and verify primitive PRS removes it
    const p1s = new polynomial(p1.c.map(v => v * 2));
    const p2s = new polynomial(p2.c.map(v => v * 2));

    console.log('constructed base polynomials; degrees:', p1s.c.length - 1, p2s.c.length - 1);

    const a = new polynomialT(p1s.c.map(c => rational.from(c)));
    const b = new polynomialT(p2s.c.map(c => rational.from(c)));

    // NOTE: calling primitiveSubresultantPRS directly causes a TypeScript
    // generic constraint mismatch for some scalar wrappers (rational). To
    // reproduce the previous hang we call the public polyGCDT entrypoint
    // which selects the appropriate PRS internally.
    console.log('skipping direct primitiveSubresultantPRS() call; will use polyGCDT() instead');

    try {
        console.log('calling polyGCDT() at', new Date().toISOString());
        const g = polyGCDT(a, b);
        console.log('polyGCDT returned; degree =', g.degree());
        const coeffs = g.c.map(v => Number(v));
        console.log('gcd coeffs (raw):', coeffs);
        const expected = [1, -2, 1];
        if (coeffs.length !== expected.length)
            throw new Error('gcd has unexpected degree: ' + coeffs.length);
        for (let i = 0; i < expected.length; i++)
            if (Math.abs(coeffs[i] - expected[i]) > 1e-12)
                throw new Error('gcd coefficients mismatch: ' + JSON.stringify(coeffs));
    } catch (e) {
        console.error('polyGCDT threw:', e && (e as Error).message || e);
        throw e;
    }
})();

console.log('ok test-gcd-primitive.ts');
