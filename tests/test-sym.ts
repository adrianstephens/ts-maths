import { test, expect } from './test';
import { symbolic } from '../dist/symbolic';

test('symbolic instance adapter basic', () => {
    const x = symbolic.variable("x");
    const y = symbolic.variable("y");
    const two = symbolic.from(2);

    // build expression (x + y) * 2 using the instance-style ops
    const sum = x.add(y);
    const expr = sum.mul(two);

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
});
