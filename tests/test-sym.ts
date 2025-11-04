/* eslint-disable @typescript-eslint/no-unused-vars */
import { test, expect } from './test';
import { symbolic } from '../dist/symbolic';

test('symbolic instance adapter basic', () => {
    const x = symbolic.variable("x");
    const y = symbolic.variable("y");
    const z = symbolic.variable("z");
    const two = symbolic.from(2);

    // build expression (x + y) * 2 using the instance-style ops
    const sum = x.add(y);
    const sum2 = sum.add(z);
    const expr = sum.mul(two);

	const x5 = x.pow(2).mul(x.pow(3)).mul(x.pow(-5));
	console.log(x5);

    // string form (non-guaranteed formatting, but check tokens present)
    const s = expr.toString();
    expect(s, 'string contains x,y,2').check(str => typeof str === 'string' && str.includes('x') && str.includes('y') && str.includes('2'));

    // evaluate at x=3,y=4 -> (3+4)*2 = 14
    const val = expr.evaluate({ x: 3, y: 4 });
    expect(val, 'evaluate (3+4)*2').toEqual(14);

    // derivative w.r.t x should be 2
    const d = expr.derivative("x");
    const dv = d.evaluate({ x: 3, y: 4 });
    expect(dv, 'derivative d/dx').toEqual(2);

	const e2 = sum.div(sum2);
	const d2 = e2.derivative("x");
	console.log('d2=', d2.toString());

	const e3 = x.pow(2).add(y.pow(2)).add(z.pow(2));

	const sin = symbolic.sin(e3);
	const dsin = sin.derivative("x");
	console.log('sin=', sin.toString());
	console.log('dsin/dx=', dsin.toString());

	const pow = symbolic.pow(e3, x);
	const dpow = pow.derivative("x");
	console.log('p=', pow.toString());
	console.log('dp/dx=', dpow.toString());
    // additional function checks
    const a_asin = symbolic.asin(x);
    expect(a_asin.evaluate({ x: 0.5 }), 'asin eval').check(v => Math.abs(v - Math.asin(0.5)) < 1e-12);
    const da_asin = a_asin.derivative('x');
    expect(da_asin.evaluate({ x: 0.5 }), 'asin deriv').check(v => Math.abs(v - 1 / Math.sqrt(1 - 0.5 * 0.5)) < 1e-12);

    const a_acos = symbolic.acos(x);
    expect(a_acos.evaluate({ x: 0.5 }), 'acos eval').check(v => Math.abs(v - Math.acos(0.5)) < 1e-12);
    const da_acos = a_acos.derivative('x');
    expect(da_acos.evaluate({ x: 0.5 }), 'acos deriv').check(v => Math.abs(v - (-1 / Math.sqrt(1 - 0.5 * 0.5))) < 1e-12);

    const a_atan = symbolic.atan(x);
    expect(a_atan.evaluate({ x: 1 }), 'atan eval').check(v => Math.abs(v - Math.atan(1)) < 1e-12);
    const da_atan = a_atan.derivative('x');
    expect(da_atan.evaluate({ x: 1 }), 'atan deriv').check(v => Math.abs(v - (1 / (1 + 1 * 1))) < 1e-12);

    const a_atan2 = symbolic.atan2(y, x);
    expect(a_atan2.evaluate({ x: 2, y: 1 }), 'atan2 eval').check(v => Math.abs(v - Math.atan2(1, 2)) < 1e-12);
    const da_atan2_dx = a_atan2.derivative('x');
    // derivative w.r.t x: -y / (x^2 + y^2)
    expect(da_atan2_dx.evaluate({ x: 2, y: 1 }), 'atan2 dx').check(v => Math.abs(v - (-1 / (4 + 1))) < 1e-12);

    const a_sinh = symbolic.sinh(x);
    expect(a_sinh.evaluate({ x: 0.5 }), 'sinh eval').check(v => Math.abs(v - Math.sinh(0.5)) < 1e-12);
    const da_sinh = a_sinh.derivative('x');
    expect(da_sinh.evaluate({ x: 0.5 }), 'sinh deriv').check(v => Math.abs(v - Math.cosh(0.5)) < 1e-12);

    const a_cosh = symbolic.cosh(x);
    expect(a_cosh.evaluate({ x: 0.5 }), 'cosh eval').check(v => Math.abs(v - Math.cosh(0.5)) < 1e-12);
    const da_cosh = a_cosh.derivative('x');
    expect(da_cosh.evaluate({ x: 0.5 }), 'cosh deriv').check(v => Math.abs(v - Math.sinh(0.5)) < 1e-12);

    const a_tanh = symbolic.tanh(x);
    expect(a_tanh.evaluate({ x: 0.5 }), 'tanh eval').check(v => Math.abs(v - Math.tanh(0.5)) < 1e-12);
    const da_tanh = a_tanh.derivative('x');
    expect(da_tanh.evaluate({ x: 0.5 }), 'tanh deriv').check(v => Math.abs(v - (1 - Math.tanh(0.5) ** 2)) < 1e-12);

});

test('canonicalization merges powers and derivatives simplify', () => {
    const x = symbolic.variable("x");
    const two = symbolic.from(2);

    const c = x.mul(symbolic.from(2)).add(x.mul(symbolic.from(3)));
    const d = x.mul(symbolic.from(5));
    expect(c.eq(d), 'c equals d').toEqual(true);


    const a = x.pow(2).mul(x.pow(3));
    const b = x.pow(5);
    expect(a.eq(b), 'a equals b').toEqual(true);
    expect(a.id, 'same interned id').toEqual(b.id);

    const da = a.derivative("x");
    const db = b.derivative("x");
    expect(da.eq(db), 'derivatives equal').toEqual(true);
    expect(da.evaluate({ x: 2 }), 'derivative value').toEqual(5 * Math.pow(2, 4));

    // more complex: x^2 * (x+1) * x^3 -> x^5*(x+1)
    const expr = x.pow(2).mul(x.add(symbolic.from(1))).mul(x.pow(3));
    const expected = x.pow(5).mul(x.add(symbolic.from(1)));
    expect(expr.eq(expected), 'expr canonicalizes to expected').toEqual(true);
    expect(expr.id, 'expr interned id equals expected').toEqual(expected.id);

    const dexpr = expr.derivative("x");
    // numeric derivative check via finite differences
    const f = (xx: number) => expr.evaluate({ x: xx });
    const h = 1e-6;
    const numeric = (f(3 + h) - f(3 - h)) / (2 * h);
    expect(dexpr.evaluate({ x: 3 }), 'derivative numeric approx').check(v => Math.abs(v - numeric) < 1e-6);
});
