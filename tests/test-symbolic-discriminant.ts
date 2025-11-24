import * as assert from 'assert';
import { symbolic } from '../src/symbolic';
import { polynomialT, discriminantT } from '../src/polynomial';

// Quadratic polynomial: a*x^2 + b*x + c
const a = symbolic.variable('a');
const b = symbolic.variable('b');
const c = symbolic.variable('c');

const p = new polynomialT<symbolic>([c, b, a]);
const D = discriminantT(p);

// expected: b^2 - 4 a c
const expected = b.pow(2).sub(a.mul(c).scale(4));

// Test by numeric sampling at several random points (avoid degenerate choices)
const samples = [
    { a: 1, b: 2, c: 1 },
    { a: 2, b: 5, c: 3 },
    { a: 3, b: -1, c: 4 },
    { a: 5, b: 0, c: -2 },
    { a: -2, b: 7, c: 1 }
];

for (const env of samples) {
    const dv = D.evaluate(env as Record<string, number>);
    const ev = expected.evaluate(env as Record<string, number>);
    assert(Math.abs(dv - ev) < 1e-12, `discriminant mismatch for ${JSON.stringify(env)}: got ${dv}, expected ${ev}`);
}

console.log('symbolic discriminant test passed');

// --- Additional parametric discriminant checks ---

// Show pretty and alternate toString options for visual confirmation
console.log('Quadratic discriminant (pretty):', D.toString());
console.log('Quadratic discriminant (with superscript & radicals):', D.toString({ superPower: true, radicalPower: true }));

// Cubic: construct (x - r)^2 * (x - s) and check discriminant vanishes
const x = symbolic.variable('x');
const r = symbolic.variable('r');
const s = symbolic.variable('s');

// build polynomial expression and collect coefficients in x
const cubicExpr = (x.sub(r).pow(2)).mul(x.sub(s)).expand({ depth: 2 });
const groups = cubicExpr.collect('x');
const cubicCoeffs = [] as symbolic[];
for (const g of groups)
    cubicCoeffs.push(g ?? symbolic.from(0));

const cubicPoly = new polynomialT<symbolic>(cubicCoeffs);
const Dc = discriminantT(cubicPoly);

console.log('Cubic discriminant (from (x-r)^2*(x-s)) pretty:', Dc.toString());
console.log('Cubic discriminant (compact):', Dc.toString({ superPower: false, radicalPower: false }));

// numeric sanity: discriminant should be zero when r != s (we constructed a repeated root at r)
const sampleC = [{ r: 2, s: 3 }, { r: 5, s: 5 }];
for (const env of sampleC) {
    const val = Dc.evaluate(env as Record<string, number>);
    // when r != s the polynomial still has multiplicity 2 at r, discriminant 0; when r==s multiplicity 3 still discriminant 0
    assert(Math.abs(val) < 1e-12, `cubic discriminant expected 0 for ${JSON.stringify(env)}, got ${val}`);
}

// Parametric family: x^2 + 2*t*x + t^2 = (x + t)^2 -> discriminant identically zero
const t = symbolic.variable('t');
const quadFamily = new polynomialT<symbolic>([t.pow(2), t.scale(2), symbolic.from(1)]);
const Df = discriminantT(quadFamily);
console.log('Parametric family discriminant:', Df.toString());
// numeric check
for (const tv of [0, 1, -3, 2.5]) {
    const val = Df.evaluate({ t: tv } as Record<string, number>);
    assert(Math.abs(val) < 1e-12, `parametric discriminant expected 0 for t=${tv}, got ${val}`);
}

console.log('additional parametric discriminant tests passed');
